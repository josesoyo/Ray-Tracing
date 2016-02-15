// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System.IO
open Preprocessor.reading
open Preprocessor.Octree
open Types.ObjectTypes
open Types.types

let thingPath = Path.Combine(__SOURCE_DIRECTORY__,"wavefront\\thorlabsIris33.obj")
let (scene,matPath) = ReadMeshWavefront2(thingPath)
let vn =scene.VNormals
let vertices = scene.Vertices
let gr = scene.groups
let box = scene.Bbox
// Construct the octree
//Octree (group:group [] , vert:float<metre> [] [], space:BBox, maxEle:int ,depth:int ,maxDepth:int)
let octree = CreateOctree(gr , vertices, box , 1000 ,0 ,20)

printfn "Finished"
printfn "Finished"