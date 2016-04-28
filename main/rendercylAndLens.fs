

open System.IO


// I must read a mtlib file and see how it works.
open Preprocessor.ReadMatLib
let mpath = @"C:\Users\Jose M. Gonzalez\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\main\wavefront\thorlabsIris33.mtl"
let nmat = ReadMatLib_debug(mpath)

// create a cylinder and a sLens 
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Types.Algebra
open Types.ObjectTypes
open Types.types

let cy = cylinder(0.1<m>,1.5<m>,Point(12.1,-0.5,0.),UnitVector(0.,1.,0.),"Material__27")
let lens = SphSurfaceLens(Point(10.951,-0.2,0.05),0.1<m>,0.05<m>,UnitVector(-1.,0.0,0.),false,"Material__27")


// Create a Camera and the scene - Camera on 0,0,0 pointing to 1,0,0

open BackwardRender.Camera
open BackwardRender.BackTypes

let Camera = {EyePoint = Point(0.,-0.0,-0.0); LookAt= Vector(1.,0.,0.); Up=UnitVector(0.,0.,1.); // iris
               PixNumH=300;PixNumW=300;PixSize= 5e-4}
let light0 = {origin= Point(0.,-0.20,-0.5);intensity = 1.} // for gourd
let scene = {Camera=Camera;  Elements= [|Cylinder(cy);SurfaceLens(lens)|]; Materials=nmat ; Plights=[|light0|]}

let render = Do_Casting (scene,1,true)
let spath = "cylAndlens2.bmp"

// save in a file just in case
let separator = ";"
let str = render.RotMat
let ar = Array.init (str.GetLength(0)) (fun i -> 
  seq { for j in 0 .. str.GetLength(1) - 1 -> str.[i, j].ToString() }
  |> String.concat separator )

let whereSave = Path.Combine(__SOURCE_DIRECTORY__,"test.txt")
System.IO.File.WriteAllLines(whereSave, ar ); 

createBMP(render,spath)

