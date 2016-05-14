module Random 

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
    // Not efficient
    // For Lambertian dispersion
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
