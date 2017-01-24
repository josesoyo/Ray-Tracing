
#r @"../Types/bin/Debug/Types.dll"
#r @"../RayTracing/bin/Debug/RayTracing.dll"
#r @"../PostProcess/bin/Debug/PostProcess.dll"
//Math.Net
#r @"../packages/MathNet.Numerics.3.11.1/lib/net40/MathNet.Numerics.dll"
#r @"../packages/MathNet.Numerics.FSharp.3.11.1/lib/net40/MathNet.Numerics.FSharp.dll"

#r @"../packages/FSCL.Compiler.2.0.1/lib/net45/FSCL.Compiler.Core.dll"
#r @"../packages/FSCL.Compiler.2.0.1/lib/net45/FSCL.Compiler.dll"
#r @"../packages/FSCL.Compiler.2.0.1/lib/net45/FSCL.Compiler.Language.dll"
#r @"../packages/FSCL.Compiler.2.0.1/lib/net45/FSCL.Compiler.NativeComponents.dll"
#r @"../packages/FSCL.Compiler.2.0.1/lib/net45/FSCL.Compiler.Util.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/FSCL.Runtime.CompilerSteps.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/FSCL.Runtime.Core.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/FSCL.Runtime.Execution.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/FSCL.Runtime.Language.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/FSCL.Runtime.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/FSCL.Runtime.Scheduling.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/OpenCLManagedWrapper.dll"
OpenCL.OpenCLPlatform.Platforms.Count // Item(0)

#load "mat_lib.fsx"
open materials
// libraries referenced
open Types.Algebra
open Types.ObjectTypes
open Types.types
open RayTracing.intersections
open SaveSensorInfo  // @RayTracing solution
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open ForwardRayTracing
open SaveSensorInfo
open Random
open System.IO
open MathNet.Numerics.IntegralTransforms
open System.Numerics
open PostProcess.Noise
open periodogram

let Rtube, Ltube = 0.6<m>, 3e3<m>
let Rmirror = 0.175<m>
let Rbaffle = 0.176 //<m>
let wavelength = WaveLength(1.064e-6<m>)
let BeamWaist = 0.028<m>
let nRepeat =  1
let Nrays = 1000
let particlesPerRay = 5

// Define the cylinder
let t , Amplitude,Amplitude2 = [|(0.)..(1./10000.)..(5.0-1./10000.)|], Vector(0.,0.,0.075e-6) ,Vector(0.,0.075e-6,0.)  // Temporal series for the phase scan 20000
let Amplitude_big,Amplitude_big2 =  Vector(0.,0.,100.e-6), Vector(0.,100.e-6,0.)
let noise = ([|(10.,Amplitude,0.);(10.,Amplitude2,(PI/7.))|] , t)    // (f,) 
let noise_upconversion = ([|(5.,Amplitude_big,0.);(5.,Amplitude_big2,(PI/6.))|], t)
let amplitude_z = Vector(1.0e-6,0.,0.)
let znoise = ([|(100.,amplitude_z,0.)|], t)
//
// select if I want to use the upconverted or the not upconverted version
let noise_select = noise_upconversion  
//
//

let NewRay (pos:Point) (normal:UnitVector) (sigma:float) (rMax:float) (nOfParticles:int)=
    let rotpoint = Matrix.RotateVector(UnitVector(0.,0.,1.),normal)
    let rec rfPos() =  // position function
        let orig = Samp2DGauss(sigma,0.) |> fun x -> Point(x.[0],x.[1],0.)
        if orig.ToVector().Module() > rMax then rfPos()
        else
            let rp = rotpoint.RotatePoint(orig)
            rp |> fun px -> px.MoveAndCreateNew(pos) 
    let rPos = rfPos()
    let rvect = // direction
        let theta = inv_sqr()
        let phi = 2.*PI*rnd.NextDouble()
        let stheta = sin(theta)
        UnitVector(cos(phi)*stheta, sin(phi)*stheta, cos(theta))
        |> fun x -> rotpoint.RotateVector(x)
    {
         Wavelenght = wavelength;
         from = rPos; uvec = rvect;
         MaxLength = infi;
         OpticalPathTravelled = 0.<m>;
         NumBounces = 0.; bounces = [];
         MaxDispersions = 3.;
         NumOfParticlesCreated = nOfParticles;
         FracOfRay = 1.
         IndexOfRefraction = 1.
         PhaseModulation = [||]
    }

let ps0 = Point(0.,0.,0.)
let dir0 = UnitVector(1.,0.,0.)
// Function to generate the rays
let ray() = NewRay ps0 dir0 (float(BeamWaist)) (float(Rbaffle)) particlesPerRay




#time

let all (i:int) (path_init:string)=
    let tube = cylinder(Rtube,Ltube,Point(0.,0.,0.),UnitVector(1.,0.,0.),"ANG_Steel", Sensor(), noise_select)

    // define the mirrors
    let m1 = disc(Point(0.,0.,0.), float Rmirror, UnitVector(1.,0.,0.),"Mirror", true ) 
    let m2 = disc(Point(float Ltube,0.,0.), float Rmirror, UnitVector(-1.,0.,0.),"Mirror",true)
    //let objs = [|Cylinder(tube); Disc(m1);Disc(m2)|] m2.Centre baffle_end.Disc.Centre
    // Case I want to control all the rays:
    //let m11 = disc(Point(-1e-10,0.,0.), (float Rtube)+0.001, UnitVector(1.,0.,0.),"Mirror", true) 
    //let m22 = disc(Point((float Ltube)+1e-10,0.,0.), (float Rtube)+0.001, UnitVector(-1.,0.,0.),"Mirror", true)
    //let baffle_end = annular_disc(Point((float Ltube)-0.01,0.,0.),Rbaffle,0.4,UnitVector(1.,0.,0.),"Baffle_SiC",Sensor(),znoise)  // I choose the material is Silicon Carbide, not perfectly defined
    //let baffle_start = annular_disc(Point(0.01,0.,0.),Rbaffle,0.4,UnitVector(1.,0.,0.),"Baffle_SiC",Sensor(),znoise)  // Baffle_SiCI choose the material is Silicon Carbide, not perfectly defined
    // i add a small imperfection on the baffle consisting on a mirror surface with a radius of 4cm (0.3 - 0.34 m)
    //let baffle_imperfection = annular_disc(Point(0.01,0.,0.),0.35,0.4,UnitVector(1.,0.,0.),"Mirror_Half",Sensor(),znoise)  // I choose the material is Silicon Carbide, not perfectly defined
    //let baffle_imperfection2 = annular_disc(Point((float Ltube)-0.0101,0.,0.),0.3,0.34,UnitVector(1.,0.,0.),"Mirror_Half",Sensor(),znoise)  // I choose the material is Silicon Carbide, not perfectly defined

    let objs = [|Cylinder(tube); Disc(m1);Disc(m2);//Disc(m11);Disc(m22);
                //Annular_Disc(baffle_end)//;Annular_Disc(baffle_start);
                //Annular_Disc(baffle_imperfection);
                //Annular_Disc(baffle_imperfection2)
                |]

    [|1..Nrays|] |> Array.iter(fun x -> ForwardRay(ray(),objs,mat,0) ) // ok, look like works -   Parallel
    //let i = 0
    //printfn "the iteration %d has ratios of:\nm2:%f\tm1:%f" i (float m2.Sensor.SavedData.Length /float m22.Sensor.SavedData.Length) (float  m1.Sensor.SavedData.Length/float m11.Sensor.SavedData.Length)
    printfn "the iteration %d has produced:\nm2:%d\tm1:%d\tBaffle:0X0" i (m2.Sensor.SavedData.Count) (m1.Sensor.SavedData.Count) //baffle_end.Disc.Sensor.SavedData.Count
    printfn "the iteration %d has produced:\nm2:%+A\t\n\n\nm1:%+A" i (m2.Sensor.SavedData.[0].Noise|> Array.filter(fun x -> x<> 0.)) (m1.Sensor.SavedData.[0].Noise |> Array.filter(fun x -> x<> 0.) ) 
    (*
    m2.Sensor.SavedData//.Length
    |> Array.iteri( fun i x ->
                     if Array.isEmpty x.Noise then printfn "not empty num%d" i
                     ignore i
                  )
    *)
    let filtered_m2 = 
        let ndisc = disc(Point(float Ltube,0.,0.), float Rmirror, UnitVector(-1.,0.,0.),"Mirror", true)
        // filter the data that doesn't contains noise
        m2.Sensor.SavedData.ToArray() 
        |> Array.filter(fun x -> (Array.isEmpty x.Noise) |> not)   // filter rays that arrived directly
        |> Array.iter(fun x -> ndisc.Sensor.AddData(x))             // add the new data 
        // return the new disc/mirror
        ndisc
    printfn "nparti"
    let nparti = (Nrays*particlesPerRay-m2.Sensor.SavedData.Count + filtered_m2.Sensor.SavedData.Count)  // number of real particles that interacted with the system
    //let phnoise = NoiseInterferometerArm_WELCH m1 tube mat (ray()) 8e5 nparti 750     // old
    let periodogramLength = 4000
    let phnoise_m1 = 
        match m1 with
        | x when  x.Sensor.SavedData.Count<>0 -> 
            SumPhaseNoise_WELCH m1 tube mat (ray()) periodogramLength                    //  returns (1/n)Sum(b(theta)*n(f))
        | _ -> [||],[|1..periodogramLength/2|] |> Array.map(fun _ -> 0.)
    let phnoise_m2 = 
        match filtered_m2 with
        | x when  x.Sensor.SavedData.Count<>0 -> 
             //NoiseInterferometerArm_WELCH filtered_m2 tube mat (ray()) 8e5 nparti 400 
             SumPhaseNoise_WELCH filtered_m2 tube mat (ray()) periodogramLength                   //  returns (1/n)Sum(b(theta)*n(f))
        | _ -> [||],[|1..periodogramLength/2|] |> Array.map(fun _ -> 0.)
    printfn "period"
     
    //  
    (*
    // Baffle
    let Baffle_Render = 
        match baffle_end.Disc.Sensor.SavedData.Length with
        | 0 -> '1' |> ignore
        | _ ->
            let ra = baffle_end.Disc.Radius
            let vox = 
                {Pmin = Point(-ra,-ra,0.); Pmax = Point(ra,ra,0.)}
            let matrix_out = CreateImage_Points_from_disk(baffle_end.Disc.Sensor.SavedData,vox,baffle_end.Disc.Normal,baffle_end.Disc.Centre,ra,200,200)
            let baffle_path = Path.Combine(path_init,string(i)+"_Baffle.png")
            SensorToImage(matrix_out,baffle_path,200,200)
    *)
    //
    //  inv_sqrt() test
    //  
    (*
    let samples=
        [|0..10000000|]
        |> Array.map( fun _ -> string(inv_sqr()) )
    *)
    //let i = 2
    //let path_init = Path.Combine( __SOURCE_DIRECTORY__, "notebooks/data/")
    let Print_ScatteringASD_Plus_b_theta_Mirror1 = 
        let path_save = Path.Combine( path_init,string(i)+"_mod_m1_nup.txt")
        let path_savef = Path.Combine( path_init,string(i)+"_modf_m1_nup.txt")
        File.WriteAllLines( path_savef, fst phnoise_m1 |> Array.map(fun x -> string(x)) )
        File.WriteAllLines( path_save, snd phnoise_m1 |> Array.map(fun x -> string(x)) )

    let Print_ScatteringASD_Plus_b_theta_Mirror2 = 
        let path_save_2 = Path.Combine( path_init,string(i)+"_mod_m2.txt")
        let path_savef_2 = Path.Combine( path_init,string(i)+"_modf_m2.txt")
        File.WriteAllLines( path_savef_2, fst phnoise_m2 |> Array.map(fun x -> string(x)) )
        File.WriteAllLines( path_save_2, snd phnoise_m2 |> Array.map(fun x -> string(x)) )

        let path_npart = Path.Combine( path_init,string(i)+"_particles.txt")
        File.WriteAllLines( path_npart,  [|string(nparti)|] )
    printfn "Iteration number %d has been finished" i



let path_base = Path.Combine( __SOURCE_DIRECTORY__, "out/")
//let path_init, i = path_base, 1
//
//      Run the many ray simulations
//
[1..nRepeat] |> List.iter(fun x -> all x path_base)
//

//

(*
// How to read
let i = 1
let path_init = Path.Combine( __SOURCE_DIRECTORY__, "notebooks/data/")
let path_save = Path.Combine( path_init,string(i)+"_mod_m1_nup.txt")

*)
let readLines filePath = System.IO.File.ReadLines(filePath)
//let iters = 1
let readPSDs (iters:int) (path_init:string) =
    // Read the files 
    let readOne (i:int):(float[]*float[])*(float[]*float[])*float =
        let path_save = Path.Combine( path_init,string(i)+"_mod_m1_nup.txt")
        let path_savef = Path.Combine( path_init,string(i)+"_modf_m1_nup.txt")
        let path_save_2 = Path.Combine( path_init,string(i)+"_mod_m2.txt")
        let path_savef_2 = Path.Combine( path_init,string(i)+"_modf_m2.txt")
        
        // read frequencies
        let freq1 = readLines path_savef |> Seq.toArray |> Array.map(fun x -> float x)
        let freq2 = readLines path_savef_2 |> Seq.toArray |> Array.map(fun x -> float x)

        // read the ASD
        let asd1 = readLines path_save |> Seq.toArray |> Array.map(fun x -> float x)
        let asd2 = readLines path_save_2 |> Seq.toArray |> Array.map(fun x -> float x)
        let npath = Path.Combine( path_init,string(i)+"_particles.txt")
        let npart = readLines npath |> Seq.toArray |> Array.map(fun x -> float x)           // always will be length1
        (freq1,asd1),(freq2,asd2), npart.[0]

    match iters with
    | x when x > 1 ->
        let mutable ( f1,  a1), ( f2,  a2), npart0  = readOne 1
        printfn "the number of particles are: %f" npart0
        let freqs = 
            match f1.Length, f2.Length with
            | x , _ when x <> 0 -> f1
            | _ , y when y <> 0 -> f2
            | _ -> f1 // very bad if this happens
        [|2..iters|] 
        |> Array.iter(fun x -> 
                                let (_,aa1),(_,aa2),npart = readOne x
                                printfn "the iteration name is: %d" x
                                match f1.Length with
                                | 0 -> ignore 0
                                | _ ->     
                                    [|0..freqs.Length-1|] 
                                    |> Array.iter(fun ynd -> a1.[ynd] <- (a1.[ynd]+aa1.[ynd])  )
                                match f1.Length with
                                | 0 -> ignore 0
                                | _ ->
                                    [|0..freqs.Length-1|] 
                                    |> Array.iter(fun y -> a2.[y] <- (a2.[y]+aa2.[y])  )
                                npart0 <- npart0+npart         // sum the particles to obtain the total number of particles
                      )
        
        let wavelen =  (match ray().Wavelenght with WaveLength x -> float x)
        let OutTheSquareRoot = (wavelen*wavelen*mat.["Mirror"].LambPPM)/(sqrt(2.**5.)*PI*PI*(float Ltube)*(float Rmirror))

        printfn "the FINAL number of particles are: %f" npart0
        // ( f1,  a1), ( f2,  a2)
        ( freqs,  a1|> Array.map(fun x -> OutTheSquareRoot*sqrt(x/npart0)) ),       ( freqs,  a2|> Array.map(fun x -> OutTheSquareRoot*sqrt(x/npart0)) )
                                        
    | _ -> 
       printfn "It was already done since it's a single run!\nBut only a part!"
       let ( f1,  a1), ( f2,  a2), npart0  = readOne 1

       let wavelen =  (match ray().Wavelenght with WaveLength x -> float x)
       let OutTheSquareRoot = (wavelen*wavelen*mat.["Mirror"].LambPPM)/(sqrt(2.**5.)*PI*PI*(float Ltube)*(float Rmirror))
       // mult = 1e-6*1.064e-6*1.064e-6/((2**2.5)*np.pi*np.pi*3e3*0.175)
       
       ( f1,  a1|> Array.map(fun x -> OutTheSquareRoot*sqrt(x/npart0)) ),       ( f2,  a2|> Array.map(fun x -> OutTheSquareRoot*sqrt(x/npart0)) )


let m1, m2 = readPSDs nRepeat path_base

let save_freq =
    // save the frequency of the mirror oscillations -> are the same, but just in case
    match (fst m1) = (fst m2) with
    | true -> 
        let path_savef = Path.Combine(path_base, "frequencies.txt")
        printfn "this"
        File.WriteAllLines( path_savef, fst m1|> Array.map(fun x -> string(x)) )
        
    | false ->
        let path_savef1 = Path.Combine(path_base, "frequencies_m1.txt")
        printfn "this not"
        if fst m1 |> Array.isEmpty |> not then
            File.WriteAllLines( path_savef1, fst m1|> Array.map(fun x -> string(x)) )
        let path_savef2 = Path.Combine(path_base, "frequencies_m2.txt")
        if fst m2 |> Array.isEmpty |> not then
            File.WriteAllLines( path_savef2, fst m2|> Array.map(fun x -> string(x)) )

if snd m1 |> Array.isEmpty |> not then
    let path_save1 = Path.Combine(path_base, "ASD_withBaffle_m1.txt")
    File.WriteAllLines(path_save1, snd m1 |> Array.map(fun x -> string(x)))

if snd m2 |> Array.isEmpty |> not then
    let path_save2 = Path.Combine(path_base, "ASD_withBaffle_m2.txt")
    File.WriteAllLines(path_save2, snd m2 |> Array.map(fun x -> string(x)))
