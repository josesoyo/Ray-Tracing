﻿module Random 

open System
// random numbers 
let PI = double(3.1415926535897932384626433832795028841971693993751058209749445923078164062)
let rnd = Random()
// two main types:
// Random().Next() -> default: a positive integer, or (min,max)
// Random().NextDouble() -> a random between [0,1)

// Generate sequences of random numbers (integers of float between 0,1)
let SeqOfRndInt n mi ma = 
    // The initInfinit
    Seq.initInfinite (fun _ -> rnd.Next(mi, ma))
    |> Seq.take n //(fun _ -> rnd.Next(mi,ma))
let SeqOfRndFloat n = 
    Seq.initInfinite (fun _ -> rnd.NextDouble())
    |> Seq.take n 

let ListOfRndFloat n = SeqOfRndFloat n |> Seq.toList
let ListOfRndInt n mi ma = SeqOfRndInt n mi ma |> Seq.toList

//let rndf = SeqOfRndFloat 2
//let rndfl = ListOfRndFloat 2

let SampDisk (radius:float) =
    let samples = ListOfRndFloat 2
    let r = radius* sqrt (samples.[0])
    let theta = 2.*PI*samples.[1]
    (r,theta)

let SampUnitDisk () =
    // unit dist
    let samples = ListOfRndFloat 2
    let ru = sqrt samples.[0]
    let theta = 2.*PI*samples.[1]
    (ru,theta)

let SampUnitHemisphereToCart () =
    // Generate a PRandom unit vector  in 3D in +Z
    // Not efficient?
    // For Lambertian dispersion
    // equal probability per unit of solid angle
    let (phi, ctheta)  =
        let samples =ListOfRndFloat 2
        let sol = (2.0*PI*samples.[0],samples.[1])
        //printfn "%+A" samples
        sol
    let stheta = sqrt(1.-(pown ctheta 2))
    let x = (sin phi )*stheta 
    let y = (cos phi) *stheta 
    let z = ctheta
    //let modul = (x*x)+(y*y)+(z*z)
    (x,y,z)

let SampUnitHemiCosToCart () =
    // Hemisphere cosine weighted
    let (r,theta) = SampUnitDisk ()
    let (x,y) = (r*cos theta , r*sin theta)
    let z = sqrt(max 0.0 (1.-x*x-y*y))
    //let modul = x*x+y*y+z*z
    (x,y,z)//,modul)

let SampTriangle () =
    // u v to random sample a triangle
    let samples = ListOfRndFloat 2
    let squ = sqrt samples.[0]
    (1.-squ, squ*samples.[1])    


let Samp2DGauss ( sigma:float, mu:float) =
    // Generates a gaussian PRN with 
    //sigma variance and mu decenter 
    let samples = ListOfRndFloat 2
    //log(x) = natural logarithm
    let R = sqrt(-2.*(log(samples.[0])))
    let phi = 2.*PI*samples.[1]
    let z0 = R * cos(phi)    
    let z1 = R * sin(phi)
    [z0*sigma+mu;z1*sigma+mu]


let samp2NGauss (sigma:float, mu:float, N: int) =
    // Gives a list of 2N Gaussian PRN
    [1..N]|>List.collect(fun _ -> Samp2DGauss(sigma,mu))

//
//  not all are random methods
//
let NewtonRapson (x0:float) (f:float->float) (f1:float->float) (tol:float) (maxIter:int) =   
    // find the zeros of a function with Newton's method
    let mutable haveWefoundASolution = false
    let epsilon = 1e-14
    let mutable x = x0
    let mutable xprim = 0.
    
    let numitrs =
        [|1..maxIter|] 
        |> Array.tryPick( fun _ -> 
                               let y = f x
                               let yp = f1 x
                               match abs(yp) with
                               | d when d <= epsilon ->
                                   //x <- -infinity
                                   Some(true)
                               | _ ->
                                   let x1 = x - y/yp
                                   if abs(x1-x) <= tol*abs(x1) then haveWefoundASolution <- true
                                   if abs(y) < epsilon then haveWefoundASolution <- true
                                   x <- x1     // update the variable
                                   xprim <- y
                                   match haveWefoundASolution with true -> Some(true) | _ -> None
                      ) 
    (x), (haveWefoundASolution, numitrs)  // return the value + extra information

let inv_sqr() =
    // Generate a random number based on the 
    // pdf = k/theta^2 with theta € [th_min=55e-6rad, pi/2] 
    let xrand = rnd.NextDouble()
    let k =  0.09932750   //0.01006406986 // based on the current thmin = 10^-2
    let tho = 55e-6
    let tol, maxIter = 1e-7, 1000

    //
    //  About the function:
    //  - I have used the Taylor expansion of the funtion to integrate since it sin(th)/th**2 cannot be integrated directly
    let f0  (th:float) (th0:float) (x:float) = // function 
        k*(log(th/th0)-((th**2.)-(th0**2.))/(12.)+((th**5.)-(th0**5.))/(480.)-((th**7.)-(th0**7.))/(30240.)) - x
    let f1 th =    // derivate
        k*((1./th)-th/6.+(th**3.)/120.-(th**5.)/5040.) 
    let f th = f0 th tho xrand  
    // there's a problem with the x0 point, if it's now the th_min it raises an error
    fst (NewtonRapson (0.000055) f f1 tol maxIter)  


let hist (data:float[]) (nbins:int)=
    // histogram to test the functions created
    let ma, mi = (data |> Array.max)+1e-10 , data |> Array.min
    let wide = (ma-mi)/float(nbins)
    let xaxis = [|1..nbins|] |> Array.map(fun x -> float(x)*wide/2.+mi)
    let out = Array.init nbins (fun _ -> 0.) // initialize the output 
    data 
    |> Seq.iter( fun x -> 
                        let whichbin = int((x-mi)/wide)
                        printfn "%d" whichbin
                        (out.[whichbin] <- out.[whichbin]+1.)
                    )
    let norm = out |> Array.sum  // normalization factor
    (out, xaxis) ||> Array.map2(fun x y -> ( y, x/norm))
