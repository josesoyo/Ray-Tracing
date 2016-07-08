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

let thingPath = Path.Combine(__SOURCE_DIRECTORY__,"wavefront/thorlabsIris33.obj") //_simp
//let thingPath = Path.Combine(__SOURCE_DIRECTORY__,"wavefront\\cube.obj")
//let thingPath = Path.Combine(__SOURCE_DIRECTORY__,"wavefront\\gurdd.obj")
printfn "%s" thingPath
let readAll = ReadMeshWavefront >> ReadMatLib
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
//let octree = OctreeAsync(mesh.groups , mesh.Vertices, mesh.Bbox , 1000 ,0 ,20)
let octree = CreateOctree(mesh.groups , mesh.Vertices, mesh.Bbox , 1000 ,0 ,2)


printfn "Finished"
//System.Console.ReadKey(intercept=false) |> ignore
printfn "start the rendering"

open BackwardRender.Camera
open BackwardRender.SimpleShading
open BackwardRender.BackTypes

let Camera = {EyePoint = Point(0.,-3.0,-3.0); LookAt= Vector(0.,1.,1.); Up=UnitVector(0.,0.,1.); // iris
               PixNumH=300;PixNumW=300;PixSize= 5e-4}

//let Camera = {EyePoint = Point(-1.2579105,-1.25791050,1.50); LookAt= Vector(1.,1.00001,-1.65); Up=UnitVector(1e-15,1e-15,1.); // cube
//               PixNumH=200;PixNumW=200;PixSize= 0.8e-2}

//let Camera = {EyePoint = Point(-0.2050105,1.35000001,-0.020501050); LookAt= Vector(0.0001,-1.50000001,0.0001); Up=UnitVector(0.0000001,1.,0.00000001); // gourd
//let Camera = {EyePoint = Point(-4.50105,0.000001,-4.7501050); LookAt= Vector(1.,-0.0000001,1.); Up=UnitVector(0.0000001,0.00000001,1.); // gourd
//               PixNumH=400;PixNumW=400;PixSize= 0.322e-2}

let element = {Mesh = mesh; Octree= octree }
// per iris
//let light1 = {origin= Point( -1.,1.,-2.6);intensity = 1.}
//let light2 = {origin= Point( -1.,-1.5,-1.);intensity = 1.}
//let light3 = {origin = Point( 0.,-3.,-3.); intensity = 0.5}
// other
//let light2 = {origin= Point( 1.2,0.1,1.1);intensity = 1.}
let light1 = {origin= Point(-4.79105,-0.20,-4.791050);intensity = 1.} // for gourd
let scene = {Camera=Camera;  Elements= [|Mesh(element)|]; Materials=materials ; Plights=[|light1|]}

let render = Do_Casting (scene,6,true)
let spath = "test4_2kmax_3levelsdepth.bmp"

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

System.Console.ReadKey(intercept=false) |> ignore
System.Console.ReadKey(intercept=false) |> ignore
System.Console.ReadKey(intercept=false) |> ignore
System.Console.ReadKey(intercept=false) |> ignore

System.Console.ReadKey(intercept=false) |> ignore




//      //      //
//      second image
//      //      //
//System.Console.ReadKey(intercept=false) |> ignore


let Camera2 = {EyePoint = Point(0.,-2.0,-2.0); LookAt= Vector(0.,1.,1.); Up=UnitVector(0.,0.,1.); // iris
               PixNumH=300;PixNumW=300;PixSize= 5e-4}
let light12 = {origin= Point(-4.79105,-0.20,-4.791050);intensity = 1.} // for gourd
let scene2 = {Camera=Camera2;  Elements= [|Mesh(element)|]; Materials=materials ; Plights=[|light12|]}

let render2 = Do_Casting (scene2,6,true)
let spath2 = "test2closer.bmp"
let str2 = render.RotMat
let ar2 = Array.init (str2.GetLength(0)) (fun i -> 
  seq { for j in 0 .. str2.GetLength(1) - 1 -> str2.[i, j].ToString() }
  |> String.concat separator )

let whereSave2 = Path.Combine(__SOURCE_DIRECTORY__,"testcloser.txt")
System.IO.File.WriteAllLines(whereSave2, ar2 ); 
createBMP(render2,spath2)


//      //      //
//      third image
//      //      //
//System.Console.ReadKey(intercept=false) |> ignore


let Camera3 = {EyePoint = Point(0.,-1.50,-1.50); LookAt= Vector(0.,1.,1.); Up=UnitVector(0.,0.,1.); // iris
               PixNumH=300;PixNumW=300;PixSize= 5e-4}
let light13 = {origin= Point(-4.79105,-0.20,-4.791050);intensity = 1.} // for gourd
let scene3 = {Camera=Camera3;  Elements= [|Mesh(element)|]; Materials=materials ; Plights=[|light13|]}

let render3 = Do_Casting (scene3,6,true)
let spath3 = "test2closer2.bmp"
let str3 = render.RotMat
let ar3 = Array.init (str3.GetLength(0)) (fun i -> 
  seq { for j in 0 .. str3.GetLength(1) - 1 -> str3.[i, j].ToString() }
  |> String.concat separator )

let whereSave3 = Path.Combine(__SOURCE_DIRECTORY__,"testcloser2.txt")
System.IO.File.WriteAllLines(whereSave3, ar3 ); 
createBMP(render3,spath3)



//      //      //
//      fourth image
//      //      //
//System.Console.ReadKey(intercept=false) |> ignore


let Camera4 = {EyePoint = Point(0.,-2.0,-0.0); LookAt= Vector(0.,1.,0.); Up=UnitVector(0.,0.,1.); // iris
               PixNumH=300;PixNumW=300;PixSize= 5e-4}
let light14 = {origin= Point(-4.79105,-0.20,-4.791050);intensity = 1.} // for gourd
let scene4 = {Camera=Camera4;  Elements= [|Mesh(element)|]; Materials=materials ; Plights=[|light14|]}

let render4 = Do_Casting (scene4,6,true)
let spath4 = "test2girao1.bmp"
let str4 = render.RotMat
let ar4 = Array.init (str4.GetLength(0)) (fun i -> 
  seq { for j in 0 .. str4.GetLength(1) - 1 -> str4.[i, j].ToString() }
  |> String.concat separator )

let whereSave4 = Path.Combine(__SOURCE_DIRECTORY__,"testgirao1.txt")
System.IO.File.WriteAllLines(whereSave4, ar4 ); 
createBMP(render4,spath4)

System.Console.ReadKey(intercept=false) |> ignore




//      //      /
//      fifht image
//      //      //
//System.Console.ReadKey(intercept=false) |> ignore


let Camera5 = {EyePoint = Point(0.,-0.0,-1.0); LookAt= Vector(0.,0.,1.); Up=UnitVector(0.,1.,1.); // iris
               PixNumH=300;PixNumW=300;PixSize= 5e-4}
let light15 = {origin= Point(-4.79105,-0.20,-4.791050);intensity = 1.} // for gourd
let scene5 = {Camera=Camera5;  Elements= [|Mesh(element)|]; Materials=materials ; Plights=[|light15|]}

let render5 = Do_Casting (scene5,6,true)
let spath5 = "test2girao2.bmp"
let str5 = render.RotMat
let ar5 = Array.init (str5.GetLength(0)) (fun i -> 
  seq { for j in 0 .. str5.GetLength(1) - 1 -> str5.[i, j].ToString() }
  |> String.concat separator )

let whereSave5 = Path.Combine(__SOURCE_DIRECTORY__,"testgirao2.txt")
System.IO.File.WriteAllLines(whereSave5, ar5 ); 
createBMP(render5,spath5)




//      //      //
//      sixth image
//      //      //
//System.Console.ReadKey(intercept=false) |> ignore


let Camera6 = {EyePoint = Point(0.,-2.0,-0.0); LookAt= Vector(0.,1.,0.); Up=UnitVector(0.,0.,1.); // iris
               PixNumH=300;PixNumW=300;PixSize= 5e-4}
let light16 = {origin= Point(-4.79105,-0.20,-4.791050);intensity = 1.} // for gourd
let scene6 = {Camera=Camera6;  Elements= [|Mesh(element)|]; Materials=materials ; Plights=[|light16|]}

let render6 = Do_Casting (scene6,6,true)
let spath6 = "test2girao3.bmp"
let str6 = render.RotMat
let ar6 = Array.init (str6.GetLength(0)) (fun i -> 
  seq { for j in 0 .. str6.GetLength(1) - 1 -> str6.[i, j].ToString() }
  |> String.concat separator )

let whereSave6 = Path.Combine(__SOURCE_DIRECTORY__,"testgirao3.txt")
System.IO.File.WriteAllLines(whereSave6, ar6 ); 
createBMP(render6,spath6)

System.Console.ReadKey(intercept=false) |> ignore
