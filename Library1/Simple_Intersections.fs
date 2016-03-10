//namespace Preprocessor

module Octree_Intersections// =
//module OctreeCullingTypes// =
open Types.ObjectTypes
open Types.Algebra
//open MathNet.Spatial.Euclidean
type RayFrom = {uvec:UnitVector; length: float; from:Point; travelled:float} 
type OLDmesh = {Vertices:Point [] ; Triangles: int list [];  Bbox:BBox}
type Intersection = { point:Point; ray:RayFrom;t:float} //Nsamples = 0 means intersection with Sensor

open Types.ObjectTypes
//open OctreeCullingTypes
//open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
//open MathNet.Spatial.Euclidean


    
let intersec_mesh (ray:RayFrom,  vertices:Point [],triangle:int list, shape:char)= // normal is only passing
    // Method 2 from PBRT book eq 3.5
    let nodes = vertices // *** Is wasting memory --> It copyes the direction, no waste memory *** I can: mesh.Vertices.[n0]

    let (n0,n1,n2) = (triangle.[0]-1, triangle.[1]-1, triangle.[2]-1)
    let (u0u1,u0u2) = (nodes.[n1]-nodes.[n0],nodes.[n2]-nodes.[n0]) 

    let raydir = ray.uvec
    // A couple of def:
    let s = ray.from - nodes.[n0]//ray.from
    let s1 = raydir >< (u0u2) //dxe2 = ray.ray X u0u2
    let s2 = (s) >< (u0u1)// sxe1 = V3D(ray.from - u0) X u0u1
    // Test to check interception
    let s1Dote1 = s1*(u0u1)
    let u = (s1*(s))/ s1Dote1
    let v = (s2*(raydir))/s1Dote1
    let logic =
        if shape = 't' then // intersection with a triangle condition
            if (u>0. && v>0. && (u+v)<=1.) then true
            else false
        elif shape = 's' then // intersection with a square condition - PArallelograms (Not trapezoids)
            if (u>0. && v>0. && u<=1. && v <= 1.) then true
            else false
        else 
            printfn "ERROR in the definition of the shape"
            false
    if logic then
        let t1 = (s2*(u0u2)) / (s1Dote1)
        if t1 >= ray.length then [||] 
        // The collision cannot be further than the light when we do a shadow
        // The equal is because in the case s1Dote1 = 0 degenerate and there's no collision (t= infinity)
        // generates a problem in multiple transmision/reflection if ray.lenght is not inf intersecting not for shadow
        // Problem solved?
        else
            // t1 < dist (light- point) case shadow
            //let newRay = {uvec=ray.uvec; from = ray.from;length = ray.length; travelled = (t1+ ray.travelled)}
            let VIntersect = (u*u0u1) + (v*u0u2) 
            let PIntersect = Point( VIntersect.X + nodes.[n0].X,VIntersect.Y + nodes.[n0].Y,VIntersect.Z + nodes.[n0].Z )
            [|{ point=PIntersect; ray=ray; t=t1}|]
    else
        [||]
        //type Intersection_mesh = { normal:Vector3D; point:Point3D; ray:RayFrom; mesh:mesh;t:float}

// Prepared to accept triangles and squares as a primitive for intersection. 
//Squares should reduce the computation time
let intersec_tri (ray, vertices,triangle)= intersec_mesh (ray, vertices,triangle,'t')
let intersec_square (ray, vertices,triangle)= intersec_mesh (ray, vertices,triangle,'s') // Parallelograms
//
   

