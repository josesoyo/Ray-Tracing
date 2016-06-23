// Render some of the objects that have been created
// reference external modules
#r @"..\Types\bin\Debug\Types.dll"
#r @"..\RayTracing\bin\Debug\RayTracing.dll"
#r @"..\Library1\bin\Debug\Library1.dll"
// load f# files 
#load "BackTypes.fs"
#load "SimpleShading.fs"
#load "Camera.fs"
open System.IO
#load @"..\Library1\ReadMatLib.fs"

let pi = 3.1415

// I must read a mtlib file and see how it works.
open Preprocessor.ReadMatLib

let mpath = Path.Combine(__SOURCE_DIRECTORY__, @"..\main\wavefront\thorlabsIris33.mtl")
let nmat = ReadMatLib_debug(mpath)

// create a cylinder and a sLens 
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Types.Algebra
open Types.ObjectTypes
open Types.types
open TypesStruct

let lens = SphSurfaceLens(Point(5.1,0.,0.1),0.51<m>,0.1<m>,UnitVector(1.,-0.0,0.),true,"Material__27")
let con = cone(0.1<m>,0.25<m>,Point(8.,0.,-0.150),UnitVector(0.,0.,1.),"Material__27")
let pcon = truncatedCone(1.<m>,1.<m>,0.5<m>,Point(-0.5,-0.,-0.0),UnitVector(1.,0.,0.),"Material__27")
let cy = cylinder(0.5<m>,1.<m>,Point(0.,-0.,0.),UnitVector(1.,0.,0.),"Material__27")

let baffle = Create_Baffles(1.<m>,0.5<m>,Point(0.,0.,0.), pi/4.,
                            109e-3<m>, UnitVector(1.,0.,0.), "Material__27",
                            Sensor(), ([||],[||]) ) 

let baffle_Length = (1.-0.5)/tan(pi/4.) |> abs
  
con.Origin
con.Radius
con.wBbox
con.lBbox
(*
// couple of things to see if the sensor works
cy.Sensor.SavedData.[2].Position
let nc = SensorContent(Point(1.,2.,3.),UnitVector(1.,0.,0.),10,10.)
#time
for i in [|0..10000|] do
    cy.UpdateSensor(nc)
#time
[|0..10000|] |> Array.iter(fun x -> cy.UpdateSensor(nc) )
#time

*)
// Create a Camera and the scene - Camera on 0,0,0 pointing to 1,0,0

open BackwardRender.Camera
open BackwardRender.BackTypes

let Camera = {EyePoint = Point(-2.0,-0.0,-0.0); LookAt= Vector(1.,0.,0.); Up=UnitVector(0.,0.,1.); // iris
               PixNumH=300;PixNumW=300;PixSize= 5e-4}
let light0 = {origin= Point(-2.,-0.0,0.5);intensity = 1.} // for gourd
let scene = {Camera=Camera;  Elements=[|Cylinder(cy);Cone(con);TruncatedCone(pcon)|] ; Materials=nmat ; Plights=[|light0|]} //[|SurfaceLens(lens)|]
//match scene.Elements.[0] with Cone x -> x.Origin
let render = Do_Casting (scene,4,true)
let spath = "cylAndlens.bmp"

// save in a file just in case
let separator = ";"
createBMP(render,spath)



//      ///     ///
//
//          Render a Baffle
//
//      ///     ///

open BackwardRender.Camera
open BackwardRender.BackTypes

let Camer2 = {EyePoint = Point(2.,0.0,-0.0); LookAt= Vector(-0.12,-0.,0.); Up=UnitVector(0.,0.,1.); // iris
               PixNumH=800;PixNumW=800;PixSize= 5e-4}
let light1 = {origin= Point(0.,-0.0,0.35);intensity = 1.} // for gourd
let light2 = {origin= Point(2.,-2.0,-1.35);intensity = 1.} // for gourd

let scene1 = {Camera=Camer2;  Elements=baffle ; Materials=nmat ; Plights=[|light1;light2|]} //[|SurfaceLens(lens)|]
//let scene1 = {Camera=Camer2;  Elements=[|TruncatedCone(pcon); Cylinder(cy)|] ; Materials=nmat ; Plights=[|light1;light2|]} //[|SurfaceLens(lens)|]
//match scene.Elements.[0]  with Cone x -> x.Origin
let render1 = Do_Casting (scene1,4,true)

let npath = "Baffle.bmp"
createBMP(render1,npath)

