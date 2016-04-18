// Just test intersection for SphSurfaceLens and Cylinder
#r @"C:\Users\Jose M. Gonzalez\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\Types\bin\Debug\Types.dll"
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

let cy = Cylinder(0.1<m>,0.5<m>,Point(10.1,-0.5,0.),UnitVector(0.,1.,0.),"someName")
       
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
         from = Point(0.,0.005,0.); uvec = UnitVector(1.,0.,0.);
         MaxLength = infi;
         OpticalPathTravelled = 0.<m>;
         NumBounces = 0; bounces = [];
         NumOfParticles = 1;
         IndexOfRefraction = 1.
        }

//intersection between the ray and the lens
intersect_SphSurfaceLens(r,lens)
let ray = r
let sLens = lens
isphere.[0].t
isphere.[1].t
match isphere with
|[||] -> printfn "no"

let intersec_costh = isphere            // Cosinus between the normal of the lens(side of the lens) and the side in which the intersection happened
                     |> Array.map(fun x -> sLens.Axis*x.normal)

 