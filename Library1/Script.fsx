// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#r @"c:\Users\JoseM\documents\visual studio 2015\Projects\Library1\packages\MathNet.Numerics.3.0.2\lib\net40\MathNet.Numerics.dll"
#r @"c:\Users\JoseM\documents\visual studio 2015\Projects\Library1\packages\MathNet.Spatial.0.2.0-alpha\lib\net40\MathNet.Spatial.dll"
#load "../Types/MainTypes.fs"
#load "../Types/ObjectTypes.fs"

open System.IO
//open Preprocessor.reading

let thingPath = Path.Combine(__SOURCE_DIRECTORY__,"wavefront\\thorlabsIris2.obj")
//let scene = ReadMeshWavefront2(thingPath)
//func1()
let a = 3
type tro =  {Vertices: int [] array ; triangles: int list list}
// Define your library scripting code here
(*
let a = "2//3 4//5 6//7"
let a2 = a.Split(' ')
let a3 = a2.[0].Split('/') // vertice texture normal
a3.[2]

let a = [|[2;3];[5;7]|]
let b = [|[9;9];[10;10]|]
a.append b
a.[1]
*)
//      //      //      //
//
// Tests with List.find and Array.find
type testFind = {a:int; b:string}
let FindStr searched elem = searched = elem.b
#time
//let c = 
let succes = Array.find (FindStr "99990") ([|0..10..1000000|] |> Array.map(fun asdf -> {a= 5*asdf; b= string(asdf)}))//c
#time

//      //      //      //
// Test with a modificable type after each iteration
// Test iter and mutable part
type update = {a: int; mutable upd: float list}
let nu = {a=10; upd = []}
[1..nu.a] |> List.iter(fun te -> ( nu.upd <- nu.upd@[float(te)]))
// Test SumBy 
type upda = {a: int;  upd: float}
let te = [|0..10|]|> Array.map(fun m -> {a=2*m;upd=float m})
te |> Array.sumBy(fun m -> m.a)

// Triple lambda function
let madett (ab,bb,cc) = 
    (2*ab)+bb+cc
let partition = [|0..1|] |> Array.collect(fun ab -> 
                         [|5..6|] |> Array.collect(fun bb ->  
                                [|10..3..13|] |> Array.collect(fun cc -> [|( madett(ab, bb,cc ))|]  )))//> 10 then
                                                                            // false
                                                                            //else
                                                                            // true)
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
                                                                         
let par2:float<metre> [] = partition |> Array.map(fun elem -> (float elem|> LanguagePrimitives.FloatWithMeasure))
par2|> Array.map(fun elam -> float elam )
not true

let IDlist( i, b:bool) =
    if b then [|i|]
    else [||]

let ar= [|2.;3.;4.|]
open Array.Parallel
#time
let all =
    let ar2 = [|1..2000000|] |> Array.Parallel.map(fun p -> ([|float(p)*ar.[0];5.*float(p)*ar.[1];5.*float(p)*ar.[2]|], p%2=0))
    //let bar = [|1..2000000|] |> Array.Parallel.map(fun x -> x%2=0)
    let bar2 = ar2 |> Array.Parallel.map(fun arr  -> IDlist(arr))
    bar2 |> Array.collect(fun x -> x)
    |> ignore
#time
