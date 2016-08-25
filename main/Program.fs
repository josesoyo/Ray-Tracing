

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
//open MathNet.Numerics.IntegralTransforms
open System.Numerics
open PostProcess.Noise
open periodogram

let NewRay (pos:Point) (normal:UnitVector) (sigma:float) (rMax:float) (nOfParticles:int) wavelength=
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
         NumBounces = 0uy; bounces = [];
         MaxDispersions = 3uy;
         NumOfParticles = nOfParticles;
         IndexOfRefraction = 1.
         PhaseModulation = [||]
    }

let ps0 = Point(0.,0.,0.)
let dir0 = UnitVector(1.,0.,0.)
// Function to generate the rays




//#time

let all (i:int) (path_init:string) rayfunc nRays rTube lTube rMirror rBaffle noise_tube noise_baffle=

    printfn "Starting iteration numer %i" i
    let tube = cylinder(rTube,lTube,Point(0.,0.,0.),UnitVector(1.,0.,0.),"ANG_Steel", Sensor(), noise_tube)

    // define the mirrors
    let m1 = disc(Point(0.,0.,0.), float rMirror, UnitVector(1.,0.,0.),"Mirror", true ) 
    let m2 = disc(Point(float lTube,0.,0.), float rMirror, UnitVector(-1.,0.,0.),"Mirror",true)
    //let objs = [|Cylinder(tube); Disc(m1);Disc(m2)|] m2.Centre baffle_end.Disc.Centre
    // Case I want to control all the rays:
    //let m11 = disc(Point(-1e-10,0.,0.), (float Rtube)+0.001, UnitVector(1.,0.,0.),"Mirror", true) 
    //let m22 = disc(Point((float Ltube)+1e-10,0.,0.), (float Rtube)+0.001, UnitVector(-1.,0.,0.),"Mirror", true)
    let baffle_end = annular_disc(Point((float lTube)-0.01,0.,0.),rBaffle,0.4,UnitVector(1.,0.,0.),"Baffle_SiC",Sensor(),noise_baffle)  // I choose the material is Silicon Carbide, not perfectly defined
    let baffle_start = annular_disc(Point(0.01,0.,0.),rBaffle,0.4,UnitVector(1.,0.,0.),"Baffle_SiC",Sensor(),noise_baffle)  // Baffle_SiCI choose the material is Silicon Carbide, not perfectly defined
    // i add a small imperfection on the baffle consisting on a mirror surface with a radius of 4cm (0.3 - 0.34 m)
    //let baffle_imperfection = annular_disc(Point(0.01,0.,0.),0.35,0.4,UnitVector(1.,0.,0.),"Mirror_Half",Sensor(),znoise)  // I choose the material is Silicon Carbide, not perfectly defined
    //let baffle_imperfection2 = annular_disc(Point((float Ltube)-0.0101,0.,0.),0.3,0.34,UnitVector(1.,0.,0.),"Mirror_Half",Sensor(),znoise)  // I choose the material is Silicon Carbide, not perfectly defined

    let objs = [|Cylinder(tube); Disc(m1);Disc(m2);//Disc(m11);Disc(m22);
                Annular_Disc(baffle_end);Annular_Disc(baffle_start);
                //Annular_Disc(baffle_imperfection);
                //Annular_Disc(baffle_imperfection2)
                |]

    [|1..nRays|] |> Array.Parallel.iter(fun x -> ForwardRay(rayfunc(),objs,mat) ) // ok, look like works
    //let i = 0
    //printfn "the iteration %d has ratios of:\nm2:%f\tm1:%f" i (float m2.Sensor.SavedData.Length /float m22.Sensor.SavedData.Length) (float  m1.Sensor.SavedData.Length/float m11.Sensor.SavedData.Length)
    printfn "the iteration %d has produced:\nm2:%d\tm1:%d\tBaffle:%d" i (m2.Sensor.SavedData.Count) (m1.Sensor.SavedData.Count) baffle_end.Disc.Sensor.SavedData.Count
    (*
    m2.Sensor.SavedData//.Length
    |> Array.iteri( fun i x ->
                     if Array.isEmpty x.Noise then printfn "not empty num%d" i
                     ignore i
                  )
    *)
    let filtered_m2 = 
        let ndisc = disc(Point(float lTube,0.,0.), float rMirror, UnitVector(-1.,0.,0.),"Mirror", true)
        // filter the data that doesn't contains noise
        m2.Sensor.SavedData.ToArray() 
        |> Array.filter(fun x -> (Array.isEmpty x.Noise) |> not)   // filter rays that arrived directly
        |> Array.iter(fun x -> ndisc.Sensor.AddData(x))             // add the new data 
        // return the new disc/mirror
        ndisc
    let particlesPerRay = rayfunc().NumOfParticles
    let nparti = (nRays*particlesPerRay-m2.Sensor.SavedData.Count + filtered_m2.Sensor.SavedData.Count)  // number of real particles that interacted with the system

    //let phnoise = NoiseInterferometerArm_WELCH m1 tube mat (ray()) 8e5 nparti 750     // old
    let periodogramLength = 4000
    let phnoise_m1 = 
        match m1 with
        | x when  x.Sensor.SavedData.Count<>0 -> 
            SumPhaseNoise_WELCH m1 tube mat (rayfunc()) periodogramLength                    //  returns (1/n)Sum(b(theta)*n(f))
        | _ -> [||],[|1..periodogramLength/2|] |> Array.map(fun _ -> 0.)
    let phnoise_m2 = 
        match filtered_m2 with
        | x when  x.Sensor.SavedData.Count<>0 -> 
             //NoiseInterferometerArm_WELCH filtered_m2 tube mat (ray()) 8e5 nparti 400 
             SumPhaseNoise_WELCH filtered_m2 tube mat (rayfunc()) periodogramLength                   //  returns (1/n)Sum(b(theta)*n(f))
        | _ -> [||],[|1..periodogramLength/2|] |> Array.map(fun _ -> 0.)
     
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
    //let path_init = Path.Combine( __SOURCE_DIRECTORY__, "data/")
    let Print_ScatteringASD_Plus_b_theta_Mirror1 = 
        let path_save = Path.Combine( path_init,string(i)+"_mod_m1_nup.txt")
        let path_savef = Path.Combine( path_init,string(i)+"_modf_m1_nup.txt")
        printfn "the first path is %+A" path_save
        printfn "the second path is %+A" path_savef
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

//let iters = 1
let readPSDs (iters:int) (path_init:string) wavelength fRmirror fLtube =
    // Read the files 
    let readLines filePath = System.IO.File.ReadLines(filePath)

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
        
        //let wavelen =  (match ray().Wavelenght with WaveLength x -> float x)
        let OutTheSquareRoot = (wavelength*wavelength*mat.["Mirror"].LambPPM)/(sqrt(2.**5.)*PI*PI*(fLtube)*(fRmirror))

        printfn "the FINAL number of particles are: %f" npart0
        // ( f1,  a1), ( f2,  a2)
        ( freqs,  a1|> Array.map(fun x -> OutTheSquareRoot*sqrt(x/npart0)) ),       ( freqs,  a2|> Array.map(fun x -> OutTheSquareRoot*sqrt(x/npart0)) )
                                        
    | _ -> 
       printfn "It was already done since it's a single run!\nBut only a part!"
       let ( f1,  a1), ( f2,  a2), npart0  = readOne 1

       //let wavelen =  (match ray().Wavelenght with WaveLength x -> float x)
       let OutTheSquareRoot = (wavelength*wavelength*mat.["Mirror"].LambPPM)/(sqrt(2.**5.)*PI*PI*(fLtube)*(fRmirror))
       // mult = 1e-6*1.064e-6*1.064e-6/((2**2.5)*np.pi*np.pi*3e3*0.175)
       
       ( f1,  a1|> Array.map(fun x -> OutTheSquareRoot*sqrt(x/npart0)) ),       ( f2,  a2|> Array.map(fun x -> OutTheSquareRoot*sqrt(x/npart0)) )

[<EntryPoint>]
let main args =
    // args should be: [|Nrays; particlesPerRay; nRepeat |]
    let Rtube, Ltube = 0.6<m>, 3e3<m>
    let Rmirror = 0.175<m>
    let Rbaffle = 0.176 //<m>
    let wavelength = WaveLength(1.064e-6<m>)
    let BeamWaist = 0.028<m>
    let nRepeat =  int args.[2] //30
    let Nrays = int args.[0] //2000
    let particlesPerRay = int args.[1] //25

    // Define the cylinder
    let t , Amplitude,Amplitude2 = [|(0.)..(1./20000.)..3.0|], Vector(0.,0.,0.075e-6) ,Vector(0.,0.075e-6,0.)  // Temporal series for the phase scan
    let Amplitude_big,Amplitude_big2 =  Vector(0.,0.,8.e-6), Vector(0.,8.e-6,0.)
    let noise = ([|(10.,Amplitude,0.);(10.,Amplitude2,(PI/7.))|] , t)    // (f,) 
    let noise_upconversion = ([|(5.,Amplitude_big,0.);(5.,Amplitude_big2,(PI/6.))|], t)
    let amplitude_z = Vector(1.0e-6,0.,0.)
    let znoise = ([|(100.,amplitude_z,0.)|], t)
    //
    // select if I want to use the upconverted or the not upconverted version
    let noise_select = noise_upconversion  
    //
    //
    printfn "The number of particles is %i" Nrays
    printfn "the rays generated per each particle can be up to: %i" Nrays
    printfn "ane the bucle will be repeated up to %i times" nRepeat
    printfn "If it hasn't been created, please create a directory called:\n\tdata"
    let path_base = Path.Combine( __SOURCE_DIRECTORY__, "data/")
    printfn "the source directory is %+A" __SOURCE_DIRECTORY__
    let ray () = NewRay ps0 dir0 (float(BeamWaist)) (float(Rbaffle)) particlesPerRay wavelength
    
    //let path_init, i = path_base, 1
    //
    //      Run the many ray simulations
    //
    printfn "Starting the Ray Tracing"
    //let all (i:int) (path_init:string) rayfunc nRays Rtube Ltube Rmirror Rbaffle noise_tube noise_baffle=
    let simplifiedTracing numIter = all numIter path_base (ray) Nrays (Rtube) (Ltube) (Rmirror) (float Rbaffle) noise_select znoise
    let UseAsinc = false
    match UseAsinc with
    | false ->
        [1..nRepeat] |> List.iter(fun x -> simplifiedTracing x)
    | true ->
        let asyncasting itrs = 
            async {return itrs |> List.iter(fun it -> simplifiedTracing it)}
        let numAsync = 10
        let slist = [1..numAsync] |> List.map(fun x -> [x..numAsync..(nRepeat-numAsync+x)])
        
        slist
        |> List.collect(fun x -> [asyncasting x])
        |> Async.Parallel |> Async.RunSynchronously
        |> ignore

    printfn "It finished!"
    //

    //

    (*
    // How to read
    let i = 1
    let path_init = Path.Combine( __SOURCE_DIRECTORY__, "data/")
    let path_save = Path.Combine( path_init,string(i)+"_mod_m1_nup.txt")

    *)

    
    let m1, m2 = readPSDs nRepeat path_base (match wavelength with WaveLength x -> float x) (float Rmirror) (float Ltube)

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
    
    // end the EntryPoint
    0
