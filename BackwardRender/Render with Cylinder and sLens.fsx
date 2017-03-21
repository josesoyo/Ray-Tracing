//  Render some of the objects that have been created
//  This helps me to check if the structures I have created are working well or not
//  
//  The first part there are some lenses as biconvex, biconcave and meniscus to see ig the lenses are well defined
//  -The cylinders are done to see if the distences on the centre and edge are the ones that should be
//
// reference external modules
#r @"..\Types\bin\Debug\Types.dll"
#r @"..\RayTracing\bin\Debug\RayTracing.dll"
#r @"..\Preprocessor\bin\Debug\Library1.dll"
// load f# files 
#load "BackTypes.fs"
#load "SimpleShading.fs"
#load "Camera.fs"
open System.IO
#load @"..\Preprocessor\ReadMatLib.fs"

let pi = 3.1415

// I must read a mtlib file and see how it works.
open Preprocessor.ReadMatLib

let mpath = Path.Combine(__SOURCE_DIRECTORY__, @"..\main\wavefront\thorlabsIris33.mtl")
let nmat = ReadMatLib_debug(mpath)

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Types.Algebra
open Types.ObjectTypes
open Types.types
open TypesStruct

// more functions to test:

let lens = SphSurfaceLens(Point(0.51,0.,0.1),0.51<m>,0.1<m>,UnitVector(1.,-0.5,0.),true,"Material__27")
let lens2 = SphSurfaceLens(Point(0.51,0.,-0.1),0.51<m>,0.1<m>,UnitVector(-1.,-0.5,0.),false,"Material__27")
let con = cone(0.1<m>,0.25<m>,Point(8.,0.,-0.150),UnitVector(0.,0.,1.),"Material__27")
let pcon = truncatedCone(1.<m>,1.<m>,0.5<m>,Point(-0.5,-0.,-0.0),UnitVector(1.,0.,0.),"Material__27")



//      //      //      //      //      //      //      //
//
//      Render a Biconcave, BiConvex or Meniscus lens and see if the edges are well the same size as must be comparing with a cylinder
// - Meniscus
// - biConcave
// - biConvex
//
//      //      //      //      //      //      //      //
let cy = cylinder(0.01<m>,0.05<m>,Point(0.,-0.015 ,0.),UnitVector(1.,0.,0.),"Material__27")                                   // for bimenuscus comparative
let cy2 = cylinder(0.01<m>,0.05<m>,Point(0.0106971445,0.05 ,0.),UnitVector(1.,0.,0.),"Material__27")                         // for bimenuscus comparative
let cy3 = cylinder(0.01<m>,(0.05+2.*0.0106971445)*1.<m>,Point(-0.0106971445,0.05 ,0.),UnitVector(1.,0.,0.),"Material__27")   // for biconcave comparative
let cy4 = cylinder(0.01<m>,(0.05-2.*0.0106971445)*1.<m>,Point(0.0106971445,0.05 ,0.),UnitVector(1.,0.,0.),"Material__27")    // for biconvex comparative
let axis = UnitVector(1.,0.,0.) 
let biCv =  biConvex(0.1<m>,0.1<m>,axis,0.05<m>,0.09<m>,Point(0.,0.0,0.),"Material__27",Sensor(),([||],[||]))                // biconvexlenx
let biCc =  biConcave(0.1<m>,0.1<m>,axis,0.05<m>,0.09<m>,Point(0.,-0.0,0.),"Material__27",Sensor(),([||],[||]))              // biconcave
let bim =   Meniscus(0.1<m>,0.1<m>,axis,0.05<m>,0.09<m>,Point(0.0,0.0,0.),"Material__27",Sensor(),([||],[||]))               // meniscus  sag1 =  0.0106971445
let cyl_hole = cylinder_with_hole(cy, 0.004,Point(0.01,-0.0051 ,-0.5),UnitVector(0.,0.,1.))                                   //  Cylinder with a hole 


let bx = box(Point(-0.01,-0.005,-0.02),Point(0.01,0.005,0.02),axis,UnitVector(1.,1.,1.),Point(0.,0.,0.),"Material__27",Sensor(),([||],[||]))
// Create a Camera and the scene - Camera on 0,0,0 pointing to 1,0,0
open BackwardRender.Camera
open BackwardRender.BackTypes

let Camera = {EyePoint = Point(-0.0,-0.0,-1.50); LookAt= Vector(0.,0.,1.50); Up=UnitVector(0.,1.,0.); // iris
               PixNumH=300;PixNumW=300;PixSize= 5e-4}

let light0 = {origin= Point(0.,1.50,-1.5);intensity = 1.}    // light the system 
let light1 = {origin= Point(-0.05,0.00,0.0);intensity =0.75}    // light the system

let scene = {Camera=Camera;  Elements       =[|Cylinder_With_Hole(cyl_hole)|];Materials=nmat ; Plights=[|light0;light1|]}
//(Array.concat [[|Cylinder(cy2);Cylinder(cy)|];[|bim.[0];bim.[2]|] ]); Materials=nmat ; Plights=[|light0;light1|]} //[|Cylinder(cy);Cone(con);TruncatedCone(pcon)|]
//match scene.Elements.[0] with Cone x -> x.Origin
let render = Do_Casting (scene,1,true)
let spath = "Hole_.bmp" // save on the folder of BackWardRender

// save in a file just in case
let separator = ";"
createBMP(render,spath)



//      ///     ///
//
//          Render a Baffle
//
//      ///     ///

let baffle = Create_Baffles(1.<m>,0.5<m>,Point(0.,0.,0.), pi/4.,
                            109e-3<m>, UnitVector(1.,0.,0.), "Material__27",
                            Sensor(), ([||],[||]) ) 

let baffle_Length = (1.-0.5)/tan(pi/4.) |> abs

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

//
//      Render a U200 mount
//

#load @"../VIRGO/Mounts.fsx"
open mounts
let l_U200 = U200(Point(0.,0.,0.),UnitVector(0.,1.,0.),UnitVector(0.,0.,1.),"Material__27",'L',([||],[||]))
let r_U200 = U200(Point(0.,0.,0.),UnitVector(0.,1.,0.),UnitVector(0.,0.,1.),"Material__27",'R',([||],[||]))


let l_M8822 = M8822(Point(0.,0.,0.),UnitVector(0.,1.,0.),UnitVector(0.,0.,1.),"Material__27",'L',([||],[||]))
let r_M8822 = M8822(Point(0.,0.,0.),UnitVector(0.,1.,0.),UnitVector(0.,0.,1.),"Material__27",'R',([||],[||]))

// Create a Camera and the scene - Camera on 0,0,0 pointing to 1,0,0

open BackwardRender.Camera
open BackwardRender.BackTypes
(*
// front side
let Camera = {EyePoint = Point(-0.50,-1.50,-0.0); LookAt= Vector(0.34,1.,0.0); Up=UnitVector(0.,0.,1.); // iris
               PixNumH=300;PixNumW=300;PixSize= 4e-4}

let light0 = {origin= Point(2.,-1.50,-10.5);intensity = 1.}    // light the system 
let light1 = {origin= Point(-0.05,-10.00,0.0);intensity =0.75}    // light the system

*)

// up side
let Camera = {EyePoint = Point(-0.00,-0.00,1.50); LookAt= Vector(0.,0.0,-1.0); Up=UnitVector(0.,1.,0.); // iris
               PixNumH=300;PixNumW=300;PixSize= 5e-4}

let light0 = {origin= Point(-2.,1.50,0.5);intensity = 1.}    // light the system 
let light1 = {origin= Point(-0.05,0.00,0.0);intensity =0.75}    // light the system

let scene = {Camera=Camera;  Elements=r_M8822;Materials=nmat ; Plights=[|light0|]}
//(Array.concat [[|Cylinder(cy2);Cylinder(cy)|];[|bim.[0];bim.[2]|] ]); Materials=nmat ; Plights=[|light0;light1|]} //[|Cylinder(cy);Cone(con);TruncatedCone(pcon)|]
//match scene.Elements.[0] with Cone x -> x.Origin
let render = Do_Casting (scene,2,true)
let spath = "M8822R_up.bmp" // save on the folder of BackWardRender

// save in a file just in case
let separator = ";"
createBMP(render,spath)
