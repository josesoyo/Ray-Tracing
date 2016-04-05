// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System.IO
open Types
open Types.Algebra
open Types.types
open Types.ObjectTypes
open Preprocessor.reading
open Preprocessor.ReadMatLib
open Preprocessor.Octree

//let thingPath = Path.Combine(__SOURCE_DIRECTORY__,"wavefront\\thorlabsIris33.obj") //_simp
//let thingPath = Path.Combine(__SOURCE_DIRECTORY__,"wavefront\\cube.obj")
let thingPath = Path.Combine(__SOURCE_DIRECTORY__,"wavefront\\gurdd.obj")
printfn "%s" thingPath
let readAll = ReadMeshWavefront2 >> ReadMatLib
let (mesh,materials) = readAll(thingPath)
// the number of elements are:
//      groups = 40 ; Vn = 84590 ; v= 105917 ; usemtlib = 75
//let vn =scene.VNormals
//let vertices = scene.Vertices
//let gr = scene.groups
//let box = scene.Bbox
// Construct the octree
printfn "finished to read"
//Octree (group:group [] , vert:float<metre> [] [], space:BBox, maxEle:int ,depth:int ,maxDepth:int)
// async version creates 8 parts (no recursive nodes)
let octree = OctreeAsync(mesh.groups , mesh.Vertices, mesh.Bbox , 1000 ,0 ,20)
//let octree = CreateOctree(mesh.groups , mesh.Vertices, mesh.Bbox , 70 ,0 ,20)

printfn "Finished"
printfn "start the rendering"

open BackwardRender.Camera
open BackwardRender.SimpleShading
open BackwardRender.BackTypes

//let Camera = {EyePoint = Point(0.,-1.0,-1.0); LookAt= Vector(0.,1.,1.); Up=UnitVector(0.,0.,1.); // iris
//               PixNumH=300;PixNumW=300;PixSize= 5e-4}

//let Camera = {EyePoint = Point(-0.79105,-0.791050,0.20); LookAt= Vector(1.,1.,-0.21); Up=UnitVector(0.,0.,1.); // cube
//               PixNumH=200;PixNumW=200;PixSize= 0.8e-2}
let Camera = {EyePoint = Point(4.50105,-0.000001,-4.7501050); LookAt= Vector(-1.,-0.0000001,1.); Up=UnitVector(0.0000001,0.00000001,1.); // gourd
               PixNumH=400;PixNumW=400;PixSize= 0.22e-2}

let element = {Mesh = mesh; Octree= octree }
//let light1 = {origin= Point( -1.,-1.,-2.6);intensity = 1.} / originals
//let light2 = {origin= Point( -1.,0.,0.);intensity = 1.}
//let light2 = {origin= Point( -0.2,0.1,-1.);intensity = 1.}
let light2 = {origin= Point( 1.2,0.1,-0.1);intensity = 1.}
let light1 = {origin= Point(4.79105,0.20,-4.791050);intensity = 1.} // for gourd
let scene = {Camera=Camera;  Elements= [|element|]; Materials=materials ; Plights=[|light1;light2|]}

let render = Do_Casting (scene,4,true)
let spath = "test.bmp"

// save in a file just in case
let separator = ";"
let str = render.RotMat
let ar = Array.init (str.GetLength(0)) (fun i -> 
  seq { for j in 0 .. str.GetLength(1) - 1 -> str.[i, j].ToString() }
  |> String.concat separator )

let whereSave = Path.Combine(__SOURCE_DIRECTORY__,"test.txt")
System.IO.File.WriteAllLines(whereSave, ar ); 

createBMP(render,spath)

printfn "ciao ho finito"
System.Console.ReadKey(intercept=false) |> ignore
