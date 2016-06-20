
#r @"..\Types\bin\Debug\Types.dll"
#r @"..\Library1\bin\Debug\Library1.dll"
open System.IO
open Types
open Types.Algebra
open Types.types
open Types.ObjectTypes
open Preprocessor.reading
open Preprocessor.ReadMatLib
open Preprocessor.Octree

let thingPath = Path.Combine(__SOURCE_DIRECTORY__,"../main/wavefront/thorlabsIris33.obj") //_simp
//let thingPath = Path.Combine(__SOURCE_DIRECTORY__,"wavefront\\cube.obj")
//let thingPath = Path.Combine(__SOURCE_DIRECTORY__,"wavefront\\gurdd.obj")
printfn "%s" thingPath
let readAll = ReadMeshWavefront >> ReadMatLib
let (mesh,materials) = readAll(thingPath)

// Second method to do it
let readAll2 = ReadMeshWavefront2 >> ReadMatLib
let (mesh2,materials2) = readAll2(thingPath)


//Chech that they are the same things read
// compare the bbox
let comparePoints (p1:Point) (p2:Point) =
    let cx = (p1.X = p2.X)
    let cy = (p1.Y = p2.Y)
    let cz = (p1.Z = p2.Z)
    (cx,cy,cz)
let compareUnitVect (p1:UnitVector) (p2:UnitVector) =
    let cx = (p1.X = p2.X)
    let cy = (p1.Y = p2.Y)
    let cz = (p1.Z = p2.Z)
    (cx,cy,cz)

let inline thrd (_,_,a) =
    a
let inline seco (_,a,_) =
    a
let inline frst (a,_,_) =
    a
// compare the bboxes
comparePoints mesh.Bbox.Pmax  mesh2.Bbox.Pmax
comparePoints mesh.Bbox.Pmin  mesh2.Bbox.Pmin
//compare the vertives frst(x) &&seco(x) && thrd(x)   - not((frst(x)&&seco(x))&&thrd(x))
let vtest = (mesh.Vertices, mesh2.Vertices) ||> Array.map2(fun x y -> comparePoints x y ) |> Array.tryFind(fun x -> not(thrd(x)&&frst(x)&&seco(x)) )

let ntest = (mesh.VNormals, mesh2.VNormals) ||> Array.map2(fun x y -> compareUnitVect x y ) |> Array.tryFind(fun x -> not(thrd(x)&&frst(x)&&seco(x)))
mesh.groups.[0].Bbox
(mesh.groups,  mesh2.groups) ||> Array.map2(fun x y -> 
                                             ( (x.TrianglesNormals,y.TrianglesNormals) ||> Array.map2(fun xi yi -> compareUnitVect (snd xi) (snd yi)) )  
                                             |> Array.tryFind(fun x -> not(thrd(x)&&frst(x)&&seco(x)))
                                            )  |> Array.tryPick(fun x -> x)
(mesh.groups,  mesh2.groups) ||> Array.map2(fun x y -> 
                                             ( (x.TrianglesNormals,y.TrianglesNormals) ||> Array.map2(fun xi yi ->  (fst xi).[5]=(fst yi).[5]) )  
                                             |> Array.tryFind(fun x ->x = false))  |> Array.tryPick(fun x -> x)


(mesh.groups,  mesh2.groups) ||> Array.map2(fun x y ->  ( comparePoints x.Bbox.Pmax y.Bbox.Pmax )) |> Array.tryFind(fun x -> not(thrd(x)&&frst(x)&&seco(x)) )

// material name
// not the same because in case many materials one choses the first, other the last (no problem here)
(mesh.groups,  mesh2.groups) ||> Array.map2(fun x y ->  (x.MatName = y.MatName)) |> Array.tryFindIndex(fun x -> not( x) ) 
mesh.groups.[2].MatName
mesh2.groups.[2].MatName
// check the names
(mesh.groups,  mesh2.groups) ||> Array.map2(fun x y -> (x.Name = y.Name)) |> Array.tryFind(fun x -> x = false)

printfn "ok"

// create both octrees with the two meshes
