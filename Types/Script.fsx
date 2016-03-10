// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "MainTypes.fs"
open Types

// Define your library scripting code here

// I want to define the vector, unitvector and the two operations

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
