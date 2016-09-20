// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#r @"..\Types\bin\Debug\Types.dll"
#r @"..\RayTracing\bin\Debug\RayTracing.dll"
#r @"..\Preprocessor\bin\Debug\Library1.dll"

#load "BackTypes.fs"
#load "SimpleShading.fs"
#load @"..\Preprocessor\ReadMatLib.fs"

open Preprocessor.ReadMatLib

// I must read a mtlib file and see how it works.
let mpath = @"..\main\wavefront\thorlabsIris33.mtl"

let nmat = ReadMatLib_debug(mpath)
//let mdic = dict (nmat|> Array.map(fun x -> (x.MatName,x)))
nmat.["Material__33"]






printfn "ciao"