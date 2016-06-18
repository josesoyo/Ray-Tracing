// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#r @"C:\Users\Jose M. Gonzalez\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\Types\bin\Debug\Types.dll"
#r @"C:\Users\Jose M. Gonzalez\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\RayTracing\bin\Debug\RayTracing.dll"
#r @"C:\Users\Jose M. Gonzalez\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\Library1\bin\Debug\Library1.dll"

#load "BackTypes.fs"
#load "SimpleShading.fs"
#load @"C:\Users\Jose M. Gonzalez\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\Library1\ReadMatLib.fs"

open Preprocessor.ReadMatLib

// I must read a mtlib file and see how it works.
let mpath = @"C:\Users\Jose M. Gonzalez\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\main\wavefront\thorlabsIris33.mtl"

let nmat = ReadMatLib_debug(mpath)
//let mdic = dict (nmat|> Array.map(fun x -> (x.MatName,x)))
nmat.["Material__33"]






printfn "ciao"