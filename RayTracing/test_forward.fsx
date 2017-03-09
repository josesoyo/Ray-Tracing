// test on forward ray tacing
#r @"..\Types\bin\Debug\Types.dll"
#r @"..\RayTracing\bin\Debug\RayTracing.dll"
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
//d.Sensor.SavedData.[0]
let objs = [|Cylinder(cy) ;Disc(d)|]


let r = {
         Wavelenght = WaveLength(5e-7<m>);
         from = Point(0.,00.00,0.0); uvec = UnitVector(1.,0.,0.);
         MaxLength = infi;
         OpticalPathTravelled = 0.<m>;
         NumBounces = 0.; bounces = [];
         MaxDispersions = 1.;
         NumOfParticlesCreated = 1;
         FracOfRay = 1.;
         IndexOfRefraction = 1.
         PhaseModulation = [||]
        }

let mout = [|{MatName= "Mirror"; R=1.; T=0.; n=(2.,WaveLength(5e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.};
             {MatName= "Glass"; R=0.; T=1.; n=(1.3,WaveLength(5e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.};
             {MatName= "air"; R=0.; T=1.; n=(1.,WaveLength(5e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.};
             {MatName= "Dispersive"; R=0.1; T=0.; n=(1.,WaveLength(5e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.9};
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
manyRays|> Array.map( fun x -> ForwardRay(x,objs,mat, 0))
//manyRays |> Array.iter( fun x -> ForwardRay(x,objs,mat)) // 
#time
let data =
    match (objs.[1]) with
    | Disc x -> let vox = {Pmin = Point(-x.Radius,-x.Radius,0.); Pmax = Point(x.Radius,x.Radius,0.)}
                x.Sensor.SavedData,x.Normal, x.Centre, x.Radius , 450, 450
let _,  uv,pt,rd,_ ,_= data

manyRays.Length
printfn "ciao"

// How to do the rotation matrix
// let mm = Matrix.RotateVector(uv, UnitVector(0.,0.,1.))

let snsrs = CreateImage_Points_from_disk(data)
let pa = Path.Combine( __SOURCE_DIRECTORY__,"test.png")
SensorToImage(snsrs,pa, 450,450)

//-------------------------------------------------------------//
//                                                             //
//               Focal lenght of a ball lens                   //
//                      sphere                                 //
//                                                             //
//-------------------------------------------------------------//

let c = Point(0.,0.,0.)
let sph = sphere(c,0.25<m>,"Glass")


let p2 = Point(0.541666666666666,0.,0.) // in a sphere of n=1.3 and ops 0.  0.541666666666666
let p3 = Point(0.79,0.,0.) // 0.79
let p4 = Point(0.26,0.,0.)
let rad2 = 0.5
let d2 = disc(p2,rad2,nrm,"air",false)
let d3 = disc(p3,rad2,nrm,"air",true)
let d4 = disc(p4,rad2,nrm,"air",false)
// ;Disc(d2);Disc(d3); ; Disc(d4)
let objs2 = [|Sphere(sph); Disc(d2); Disc(d4);Disc(d3)|] //; Cylinder(cy)
d2.Centre
match objs2.[1] with 
|Disc x -> x.Centre
| _ -> failwith "NOOO" //Point(0.,0.,0.)

let manyRay2 = [|0..10000|]   // generate some rays on different directions from random points on the initial surface
               |> Array.map(fun x -> let rec pos0() = 
                                            let p = Samp2DGauss(1.1195,0.)
                                            if sqrt(p.[0]*p.[0]+p.[1]*p.[1]) >0.3 then
                                                pos0()
                                            else p
                                     let pos = pos0()
                                     ({r with uvec=UnitVector(1., 0., 0.); from = Point(-1.,pos.[0],pos.[1])}))

#time
//[|0..10000|]   // generate some rays on different directions from random points on the initial surface
//|> Array.Parallel.map(fun x -> let pos = Samp2DGauss(0.15,0.)
//                               ({r with uvec=UnitVector(1., rnd.NextDouble(), rnd.NextDouble()); from = Point(0.,pos.[0],pos.[1])})
//                               |> ( fun x -> ForwardRay(x,objs,mat))
//                      )
manyRay2.Length
manyRay2|> Array.iter( fun x -> ForwardRay(x,objs2,mat, 0))
//let nr = [|({r with uvec=UnitVector(1., 0., 0.); from = Point(0.,0.,0.)})|]
//nr|> Array.iter( fun x -> ForwardRay(x,objs2,mat))
let data2 =
    match (objs2.[3]) with
    | Disc x -> let vox = {Pmin = Point(-x.Radius,-x.Radius,0.); Pmax = Point(x.Radius,x.Radius,0.)}
                x.Sensor.SavedData,x.Normal, x.Centre, x.Radius , 450, 450
let dat,  uv2,pt2,rd2,_ ,_= data2


dat.Count

let snsrs2 = CreateImage_Points_from_disk(data2)
let pa = Path.Combine( __SOURCE_DIRECTORY__,"test.png")
SensorToImage(snsrs2,pa, 450,450)


//
//
// Next test the rays are reflected on a flat surface
//
//

let nNRM = UnitVector(0.,0.,1.)
let Pground = Point(0.,0.,0.)
let ground = disc(Pground,100.,nNRM,"Dispersive")  // Disc that represents the ground Mirror or Dispersive

// Source of Light
//  &
// sensor

let spoint = Point(3.,0.,3.)        // Sensor point centre
let lpoint = Point(-3.,0.,3.)       // Light source centre
let snrm = UnitVector(-1.,0.,-1.)   // Sensor Direction
let lnrm = UnitVector(1.,0.,-1.)    // Light Direction
let sdisc = disc(spoint,3.5,snrm,true)      // Sensor
lnrm
let objs3 = [|Disc(ground);Disc(sdisc)|]

let rotLight = Matrix.RotateVector(UnitVector(0.,0.,1.),lnrm)
let manyRay3 = [|0..2000|]   // generate some rays on different directions from random points on the initial surface
               |> Array.map(fun x -> let pos = 
                                            let ipos = Samp2DGauss(0.1,0.)
                                            let ip= Point(ipos.[0],ipos.[1],0.)
                                            //let ip= Point(0.2,0.2,0.)
                                            rotLight.RotatePoint(ip).MoveAndCreateNew(lpoint)
                                     ({r with uvec=lnrm; from = pos}))

manyRay3.[2].from
#time
manyRay3|> Array.iter( fun x -> ForwardRay(x,objs3,mat,0 ))
//sdisc.Sensor.SavedData.Length

// plot it
let data3 =
    match (objs3.[1]) with
    | Disc x -> let vox = {Pmin = Point(-x.Radius,-x.Radius,0.); Pmax = Point(x.Radius,x.Radius,0.)}
                x.Sensor.SavedData,x.Normal, x.Centre, x.Radius , 450, 450
let dat3, uv3,pt3,rd3,_ ,_= data3

let snsrs3 = CreateImage_Points_from_disk(data3)
//let pa = Path.Combine( __SOURCE_DIRECTORY__,"test.png")
SensorToImage(snsrs3,pa, 450,450)

