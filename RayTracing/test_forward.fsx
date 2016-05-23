// test on forward ray tacing
#r @"C:\Users\Jose M. Gonzalez\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\Types\bin\Debug\Types.dll"
#r @"C:\Users\Jose M. Gonzalez\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\RayTracing\bin\Debug\RayTracing.dll"
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

let cy = cylinder(0.5<m>,10.5<m>,Point(0.,0.0,0.),UnitVector(1.,0.,0.),"Mirror")
let p = Point(10.,0.,0.)
let rad = 1.
let nrm = UnitVector(-1.,0.,0.)
let d = disc(p,rad,nrm,true)
d.Sensor
let objs = [|Cylinder(cy) ;Disc(d)|]


let r = {
         Wavelenght = WaveLength(5e-7<m>);
         from = Point(0.,00.00,0.0); uvec = UnitVector(1.,1.,0.);
         MaxLength = infi;
         OpticalPathTravelled = 0.<m>;
         NumBounces = 0uy; bounces = [];
         MaxDispersions = 1uy;
         NumOfParticles = 0;
         IndexOfRefraction = 1.
         NoiseAdd= [||]
        }

let mout = [|{MatName= "Mirror"; R=1.; T=0.; n=(2.,WaveLength(5e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.};
             {MatName= "Glass"; R=0.; T=1.; n=(1.3,WaveLength(5e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.};
             {MatName= "air"; R=0.; T=1.; n=(1.,WaveLength(5e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.}
             |]

let mat = dict (mout|> Array.map(fun x -> (x.MatName,x)))

let manyRays = [|0..1000000|]   // generate some rays on different directions from random points on the initial surface
               |> Array.map(fun x -> let pos = Samp2DGauss(0.15,0.)
                                     ({r with uvec=UnitVector(1., rnd.NextDouble(), rnd.NextDouble()); from = Point(0.,pos.[0],pos.[1])}))

#time
//[|0..10000|]   // generate some rays on different directions from random points on the initial surface
//|> Array.Parallel.map(fun x -> let pos = Samp2DGauss(0.15,0.)
//                               ({r with uvec=UnitVector(1., rnd.NextDouble(), rnd.NextDouble()); from = Point(0.,pos.[0],pos.[1])})
//                               |> ( fun x -> ForwardRay(x,objs,mat))
//                      )
manyRays|> Array.map( fun x -> ForwardRay(x,objs,mat))
//manyRays |> Array.iter( fun x -> ForwardRay(x,objs,mat)) // 
#time
let data =
    match (objs.[1]) with
    | Disc x -> let vox = {Pmin = Point(-x.Radius,-x.Radius,0.); Pmax = Point(x.Radius,x.Radius,0.)}
                x.Sensor.SavedData,vox,x.Normal, x.Centre, x.Radius , 450, 450
let _, _, uv,pt,rd,_ ,_= data

manyRays.Length
printfn "ciao"

// How to do the rotation matrix
// let mm = Matrix.RotateVector(uv, UnitVector(0.,0.,1.))

let snsrs = CreateImage_Points_from_disk(data)
let pa = Path.Combine( __SOURCE_DIRECTORY__,"test.png")
SensorToImage(snsrs,pa, 450,450)

//  Precious test was one cylinder with a disc inside it,
//  Now I add a sphere

let c = Point(2.,0.,0.)
let sph = sphere(c,0.25<m>,"Glass")


let p2 = Point(2.541666666666666,0.,0.) // in a sphere of n=1.3 and ops 0.
let p3 = Point(2.85,0.,0.)
let p4 = Point(2.35,0.,0.)
let rad2 = 0.5
let d2 = disc(p2,rad2,nrm,"air",false)
let d3 = disc(p3,rad2,nrm,"air",true)
let d4 = disc(p4,rad2,nrm,"air",false)
// ;Disc(d2);Disc(d3); ; Disc(d4)
let objs2 = [|Cylinder(cy); Disc(d2); Disc(d4);Disc(d3)|] //;Sphere(sph)
d3.Sensor
match objs2.[1] with 
|Disc x -> x.Centre
| _ -> Point(0.,0.,0.)

let manyRay2 = [|0..10000|]   // generate some rays on different directions from random points on the initial surface
               |> Array.map(fun x -> let pos = Samp2DGauss(1.1195,0.)
                                     ({r with uvec=UnitVector(1., 0., 0.); from = Point(0.,pos.[0],pos.[1])}))

#time
//[|0..10000|]   // generate some rays on different directions from random points on the initial surface
//|> Array.Parallel.map(fun x -> let pos = Samp2DGauss(0.15,0.)
//                               ({r with uvec=UnitVector(1., rnd.NextDouble(), rnd.NextDouble()); from = Point(0.,pos.[0],pos.[1])})
//                               |> ( fun x -> ForwardRay(x,objs,mat))
//                      )
manyRay2.Length
manyRay2|> Array.iter( fun x -> ForwardRay(x,objs2,mat))
let nr = [|({r with uvec=UnitVector(1., 0., 0.); from = Point(0.,0.,0.)})|]
nr|> Array.iter( fun x -> ForwardRay(x,objs2,mat))
let data2 =
    match (objs2.[3]) with
    | Disc x -> let vox = {Pmin = Point(-x.Radius,-x.Radius,0.); Pmax = Point(x.Radius,x.Radius,0.)}
                x.Sensor.SavedData,vox,x.Normal, x.Centre, x.Radius , 450, 450
let dat, _, uv2,pt2,rd2,_ ,_= data2




let snsrs2 = CreateImage_Points_from_disk(data2)
//let pa = Path.Combine( __SOURCE_DIRECTORY__,"test.png")
SensorToImage(snsrs2,pa, 450,450)


//
//
// Next test the rays are reflected on a flat surface
//
//

let nNRM = UnitVector(0.,0.,1.)
let Pground = Point(0.,0.,0.)
let ground = disc(Pground,100.,nNRM,"Mirror",Sensor(),[|(0uy, 0.0)|])  // Disc that represents the ground

// Source of Light
//  &
// sensor

let spoint = Point(3.,0.,3.)        // Sensor point centre
let lpoint = Point(-3.,0.,3.)       // Light source centre
let snrm = UnitVector(-1.,0.,-1.)   // Sensor Direction
let lnrm = UnitVector(1.,0.,-1.532)    // Light Direction
let sdisc = disc(spoint,3.5,snrm,true)      // Sensor
lnrm
let objs3 = [|Disc(ground);Disc(sdisc)|]

let rotLight = Matrix.RotateVector(UnitVector(0.,0.,1.),lnrm)
let manyRay3 = [|0..100000|]   // generate some rays on different directions from random points on the initial surface
               |> Array.map(fun x -> let pos = 
                                            let ipos = Samp2DGauss(0.1,0.)
                                            let ip= Point(ipos.[0],ipos.[1],0.)
                                            //let ip= Point(0.2,0.2,0.)
                                            rotLight.RotatePoint(ip).MoveAndCreateNew(lpoint)
                                     ({r with uvec=lnrm; from = pos}))

manyRay3.[0].uvec
manyRay3|> Array.iter( fun x -> ForwardRay(x,objs3,mat))
 