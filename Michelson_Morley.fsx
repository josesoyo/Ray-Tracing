// Example to simulate a michelson Morley

// First read the libraries

// test on forward ray tacing
#r @"Types\bin\Debug\Types.dll"
#r @"RayTracing\bin\Debug\RayTracing.dll"
#r @"PostProcess\bin\Debug\PostProcess.dll"

// libraries referenced
open Types.Algebra
open Types.ObjectTypes
open Types.types
open RayTracing.intersections
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open ForwardRayTracing
open SaveSensorInfo
open Random
open System.IO

#load "mat_lib.fsx"
open materials

// In order to create the MM I need:
//  2 end mirrors that tahre SphSurfaceLens type
//  1 Beam splitter -> disc
//  1 Source
//  1 Detector = disc with Sensor

//
//  End Mirrors
let ux = UnitVector(-1.,0.,0.)
let dx = 0.07150 // 0.00575
let radx = 2.2<m> //2.2<m>
let cx = Point(dx-float radx,0.,0.)



let uy = UnitVector(0.,-1.,0.)
let dy = dx
let rady = 9.2<m>
let cy = Point(0.,0.25e-6+dy-float rady,0.) 

let EMX = SphSurfaceLens(cx,radx,0.0575<m>,ux,false,"Mirror")
//let EMXs = disc(Point(float dx,0.,0.),0.05,ux,"Mirror")
let EMY = SphSurfaceLens(cy,rady,0.0575<m>,uy,false,"Mirror")

// 
// Beam splitter
let cbs = Point(0.,0.,0.)
let ubs = UnitVector(-1.,1.,0.)
let radbs = 0.1
let BS = disc(cbs,radbs,ubs,"BeamSplitter")

// 
//  Sensors
let cs = Point(0.,-0.2,0.)
let us = UnitVector(0.,1.,0.)
let radd = 0.006
let Detector =  disc(cs,radd,us,true)        // end phototodetector at the output of the interferometer

let cs2 =  Point(0.03,0.,0.)
let us2 = UnitVector(-1.,0.,0.)
let radd2 = 0.005
let Detector2 = disc(cs2,radd2,us2,true)   // second detector to check the beam on other places od the interferometer

Detector.Sensor
// Set up the interferometer
let Interferometer = [|Disc(Detector);Disc(BS);SurfaceLens(EMX) ;SurfaceLens(EMY)|] //


// Create Source
let wavelength =  WaveLength(0.6e-6<m>) // red light

let NewRay (pos:Point) (normal:UnitVector) (sigma:float) (rMax:float) (nOfParticles:int)=
    let rotpoint = Matrix.RotateVector(UnitVector(0.,0.,1.),normal)
    let rfPos() =  // position function

            SampDisk (rMax)
            |> fun x -> 
                let r, ph = fst x, snd x
                r*sin(ph), r*cos(ph)
            |> fun x -> Point(fst x,snd x,0.)
            |> fun x -> rotpoint.RotatePoint(x)
            |>  fun px -> px.MoveAndCreateNew(pos) 

    let rPos = rfPos()
    let rvect = // direction
        // collimated ray
        normal
  
    {
         Wavelenght = wavelength;
         from = rPos; uvec = rvect;
         MaxLength = infi;
         OpticalPathTravelled = (sqrt(rPos.Z*rPos.Z+rPos.Y*rPos.Y)/17005.) |> LanguagePrimitives.FloatWithMeasure<m>;
         NumBounces = 0uy; bounces = [];
         MaxDispersions = 3uy;
         NumOfParticles = 1;
         IndexOfRefraction = 1.
         PhaseModulation = [||]
    }

let ps0 = Point(-0.5,0.,0.)
let dir0 = UnitVector(1.,0.,0.)
let normal = UnitVector(1.,0.,0.)
let sigma = 0.1
// Function to generate the rays
let ray() = NewRay ps0 dir0 (0.00575) (0.00575) 1 

//  Perform the ray tracing
//let
let nrays = 500000 // 250660
#time

[|1..nrays|] |> Array.Parallel.iter(fun x -> lock Detector (fun () -> ForwardRay(ray(),Interferometer,mat)))
//  Sensor must be lock, if not the ResizeArray() doesn't work well with Array.Parallel and not all the photons are stored
//

printfn "num of particles on sensor are %d" (Detector.Sensor.SavedData.ToArray().Length) // .[0].Position


let snrs_mat = CreateImage_Points_from_disk_phase(Detector.Sensor.SavedData,wavelength,{Pmin=Point(-radd,-radd,0.);Pmax=Point(radd,radd,0.)},
                                                  Detector.Normal,Detector.Centre,Detector.Radius,200,200)

let snrs_mat2 = CreateImage_Points_from_disk_amplitude(Detector.Sensor.SavedData,wavelength,{Pmin=Point(-radd,-radd,0.);Pmax=Point(radd,radd,0.)},
                                                       Detector.Normal,Detector.Centre,Detector.Radius,200,200)

//Detector.Sensor.SavedData |> Seq.iter(fun x ->printfn "%f" x.Phase)
let lpath = Path.Combine(__SOURCE_DIRECTORY__,"int_phase2.jpg")
SensorToImage(snrs_mat,lpath,200,200)
let lpath2 = Path.Combine(__SOURCE_DIRECTORY__,"int_amplitude2.jpg")
SensorToImage(snrs_mat2,lpath2,200,200)