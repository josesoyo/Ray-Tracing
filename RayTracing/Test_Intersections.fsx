// Just test intersection for SphSurfaceLens and Cylinder
#r @"..\Types\bin\Debug\Types.dll"
#load "RayCore.fs"

open Types.Algebra
open Types.ObjectTypes
open Types.types
open RayTracing.intersections
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// define one SphSurfaceLens
let lens = SphSurfaceLens(Point(10.1,0.,0.),0.1<m>,0.01<m>,UnitVector(-1.,0.0,0.),true,"someName")
lens.CosMin
lens.ClearAperture
lens.RadiusOfCurvature
lens.Axis
lens.SphCentre

let cy = cylinder(0.1<m>,1.5<m>,Point(10.1,-0.5,0.),UnitVector(0.,1.,0.),"someName")
       
(*    type Ray = {
                Wavelenght:Wavelength;
                from: Point; uvec: UnitVector;                 // From and direction
                MaxLength:float<m>                              // Max distance can travell (should be infinite by defect)
                OpticalPathTravelled: float<m>;                   // Optical Path Length Modified after every step with the IOR
                NumBounces: int; mutable bounces: float list;     // Num of bounces + the positions (Just in case for the future)
                NumOfParticles: int;                               // Num of photos -> To split in a Lambertian surface, etc...
                IndexOfRefraction:float
                }
*)
let r = {
         Wavelenght = WaveLength(5e-7<m>);
         from = Point(-1.25,-1.0,1.0); uvec = UnitVector(1.,1.,0.);
         MaxLength = infi;
         OpticalPathTravelled = 0.<m>;
         NumBounces = 0uy; bounces = [];
         MaxDispersions = 1uy;
         NumOfParticles = 0;
         IndexOfRefraction = 1.
         PhaseModulation = [||]
        }

//intersection between the ray and the lens
intersect_SphSurfaceLens(r,lens)

intersect_cyl(r,cy)

cy.Normal
cy.Origin
cy.Radius
cy.Zmax




[|Some 1;None ;Some 3|] |> Array.filter(fun x -> let nar = UnSomeNone(x)
                                                 not(Array.isEmpty(nar))) 
                        |> Array.map(fun x -> match x with Some x -> x)


//  disk
let p = Point(10.,0.5,0.)
let rad = 1.
let nrm = UnitVector(1.,-0.5,0.)
let d = disc(p,rad,nrm,true)
d.Centre
// Intersection intersection_Disk
let i = intersect_Disk(r,d).[0]
i.point
d.Sensor.AddData(SensorContent(i.point,i.ray.uvec,1,
                                (i.ray.OpticalPathTravelled/(match i.ray.Wavelenght with WaveLength x -> x))%(float(match i.ray.Wavelenght with WaveLength x -> x)),
                                [||]))
d.Sensor.SavedData
(1000.*3.+1e-7)%3.


// intersection with a box
let bb = box(Point(-0.5,-0.5,-0.5),Point(0.5,0.5,0.5),UnitVector(0.,0.,1.),Point(0.,0.,1.),"")
bb.LBBox
let io = intersect_Box(r,bb)
io.[0]
