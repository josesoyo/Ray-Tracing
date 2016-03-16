// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#load @"C:\Users\JoseM\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\Types\Algebra.fs"
open Types.Algebra
#load "MainTypes.fs"
open Types

// Define your library scripting code here

// I want to define the vector, unitvector and the two operations
(*
type Vector = 
|Vector of float list
|UnitVector of float list

let VectorToUnit(v:Vector) =
    // Transforms a vector into UnitVectpr
    match v with
    | UnitVector v -> UnitVector(v)
    | Vector v ->
            let norm = (0., v) ||> List.fold(fun acc x -> acc + x*x) |> sqrt
            v |> List.map(fun x ->  x/norm) |> UnitVector

let DotProduct(v1:Vector,v2:Vector) =
    let v1a = 
        match v1 with 
        |Vector v1-> v1 
        |UnitVector v1-> v1      

    let v2a = 
        match v2 with 
        |Vector v2-> v2
        |UnitVector v2-> v2   
    (0.,v1a,v2a) |||> List.fold2(fun acc x y -> acc + x * y )
    
let CrossProduct(v1:Vector, v2:Vector) =
    let u = 
        match v1 with 
        |Vector v1-> v1 
        |UnitVector v1-> v1      

    let v = 
        match v2 with 
        |Vector v2-> v2
        |UnitVector v2-> v2   
    
    let c1 = u.[1]*v.[2] - u.[2]*v.[1]
    let c2 = u.[2]*v.[0] - u.[0]*v.[2]
    let c3 = u.[0]*v.[1] - u.[1]*v.[0]
    match v1, v2 with
    | UnitVector v1 , UnitVector v2 ->  UnitVector([c1;c2;c3])
    | _ , _ -> Vector([c1;c2;c3])

let IsParallelTo(v1, v2, tolerance) =
    let u = 
        match v1 with 
        |Vector v1-> v1 
        |UnitVector v1-> v1      

    let v = 
        match v2 with 
        |Vector v2-> v2
        |UnitVector v2-> v2   
    let cp = 
        let cpp = CrossProduct(v2,v1)
        match cpp with
        |Vector cpp-> cpp
        |UnitVector cpp-> cpp  
    if abs(cp.[0]) < tolerance && abs(cp.[1]) < tolerance && abs(cp.[2]) < tolerance then true
    else false

let v1 = Vector([2.;1.;0.])
let v2 = UnitVector([1.;0.5;0.])
DotProduct(v2,v1)
VectorToUnit v1
CrossProduct(v2,v1) <> Vector([0.;0.;0.])
*)

/////////////////
//
////////////////

type MyClass(x0, y0, z0) =
    let mutable x = x0
    let mutable y = y0
    let mutable z = z0
    do
        printfn "Initialized object that has coordinates (%d, %d, %d)" x y z
    member this.X with get() = x and set(value) = x <- value
    member this.Y with get() = y and set(value) = y <- value
    member this.Z with get() = z and set(value) = z <- value
    new() = MyClass(0, 0, 0)
type MyStruct =
    struct
       val X : int
       val Y : int
       val Z : int
       new(x, y, z) = { X = x; Y = y; Z = z }
    end

let myStructure1 = new MyStruct(1, 2, 3) 
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
(*
type SphSurfaceLens2 =
    struct
        val SphereCentre:Point
        val Radius:float<m>
        val CosMin: float
        val Axis: UnitVector
        val Convex: bool
    new(p,r,c,a,cb) = {SphereCentre = }
*)
    
type SphSurfaceLens() =
    let mutable SphereCentre= Point(0.,0.,0.)
    let mutable radius = 0.<m>
    let mutable cosMin = 0.
    let mutable axis = UnitVector(0.,0.,0.)
    let mutable convex = true

    member this.SphCentre with get() = SphereCentre and set(sc) = SphereCentre <- sc
    member this.Radius  with get() = radius and set(r) = radius <- r
    member this.CosMin  with get() = cosMin and set(cm) =  cosMin <- cm 
    member this.Axis  with get() = axis and set(uv) = axis <- uv
    //member this.Axis  with get() = axis and set(uv) = axis <-UnitVector uv
    member this.Convex with get() = convex and set(cb) = convex <- cb
    static member CreateLensSurface(centreofSPH, roc:float<m>, diameter:float<m>,axis:UnitVector, convex:bool) =
        let sinth = 0.5*diameter/roc
        let costh =
            if sinth < 1. then sqrt(1.-(sinth*sinth)) 
            else 0.
        let SL = SphSurfaceLens()
        SL.Radius<- roc
        SL.Axis <- axis
        SL.CosMin <- costh
        SL.Convex <- convex
        SL.SphCentre <- centreofSPH
        SL


let ne = SphSurfaceLens.CreateLensSurface(Point(1.,0.,0.), 2.<m>,0.1<m>,UnitVector(1.,0.,0.),false)
ne.Radius