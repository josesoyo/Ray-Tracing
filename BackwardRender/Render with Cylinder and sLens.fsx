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
let biCv =  biConvex(0.1<m>,0.1<m>,axis,0.05<m>,0.09<m>,Point(0.,0.0,0.),"Material__27",Sensor(),([||],[||]))     // biconvexlenx
let biCc =  biConcave(0.1<m>,0.1<m>,axis,0.05<m>,0.09<m>,Point(0.,-0.0,0.),"Material__27",Sensor(),([||],[||]))     // biconcave
let bim =   Meniscus(0.1<m>,0.1<m>,axis,0.05<m>,0.09<m>,Point(0.0,0.0,0.),"Material__27",Sensor(),([||],[||]))     // meniscus  sag1 =  0.0106971445
let bx = box(Point(-0.01,-0.005,-0.02),Point(0.01,0.005,0.02),axis,UnitVector(1.,1.,1.),Point(0.,0.,0.),"Material__27",Sensor(),([||],[||]))
// Create a Camera and the scene - Camera on 0,0,0 pointing to 1,0,0

open BackwardRender.Camera
open BackwardRender.BackTypes

let Camera = {EyePoint = Point(-0.0,-0.0,-1.50); LookAt= Vector(0.,0.,1.0); Up=UnitVector(0.,1.,0.); // iris
               PixNumH=300;PixNumW=300;PixSize= 5e-4}

let light0 = {origin= Point(0.,1.50,-1.5);intensity = 1.}    // light the system 
let light1 = {origin= Point(-0.05,0.00,0.0);intensity =0.75}    // light the system

let scene = {Camera=Camera;  Elements=[|Box(bx)|];Materials=nmat ; Plights=[|light0;light1|]}
//(Array.concat [[|Cylinder(cy2);Cylinder(cy)|];[|bim.[0];bim.[2]|] ]); Materials=nmat ; Plights=[|light0;light1|]} //[|Cylinder(cy);Cone(con);TruncatedCone(pcon)|]
//match scene.Elements.[0] with Cone x -> x.Origin
let render = Do_Casting (scene,1,true)
let spath = "Box_.bmp" // save on the folder of BackWardRender

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


let U200(orig:Point,axis:UnitVector,up:UnitVector,mat:string, side:char,nois:noise ) =
    // as always +Z is up
    // define the cylinder that holds the lens plus the two boxes depending on the left or right configuration
    // 
    // important about sensor:  There is a single sensor, so all of them update the same
    let th_holder = 0.001525<m>     // thickness of the holder
    let th_G =    0.0099<m>         // Thickness G basado sul plot
    let cyl_len = th_holder+th_G    // thickness of the holder

    let cy_int = cylinder(25.4e-3<m>,cyl_len,orig,axis,mat,Sensor(),nois)    
    let cy_out = cylinder(35.75e-3<m>,cyl_len,orig,axis,mat,Sensor(),nois)
    // discs of faces
    let orig2 = orig+float(cyl_len)*axis
    let fst_disc = annular_disc(orig,25.4e-3,35.7e-3,axis.Negate(),mat,Sensor(),nois)
    let snd_disc = annular_disc(orig2,25.4e-3,35.7e-3,axis,mat,Sensor(),nois)
    let bx1,bx2 =
        let A = 73.2e-3   // all side
        let B = 38.1e-3   // from centre to down side
        let E = 38.4e-3   // length of the holder withouth the screw
        match side with
        | 'L' ->
            // the box  is on the left side (-X)
            let b1 = box( {Pmin=Point(B-A, float(-1.*th_holder), -B); Pmax=Point(B , E, -25.4e-3)},
                           axis, up,orig,mat
                         )
            let b2 = box( {Pmin=Point( -B, float(-1.*th_holder),-B); Pmax=Point(-25.4e-3, E, A-B)},
                           axis, up,orig,mat
                         )
            (b1,b2)
        | 'R' ->
            // the box is on the right side (+X)
            let b1 = box( {Pmin=Point(-B, float(-1.*th_holder), -B); Pmax=Point(A-B , E, -25.4e-3)},
                           axis, up,orig,mat
                         )
            let b2 = box( {Pmin=Point( 25.4e-3, float(-1.*th_holder),-B); Pmax=Point(B, E, A-B)},
                           axis, up,orig,mat
                         )
            (b1,b2)
        | _   -> 
           failwith "Error defining the boxes of U200" //(for i in 0..10 do printfn "Error defining the side of a mount U200 from Newport!\n")
           // the box is on the right side (+X)
           //let b1 = box( {Pmin=Point(-B, float(-1.*th_holder), -B); Pmax=Point(A-B , E, -25.4e-3)},
           //               axis, up,orig,mat
           //             )
           //let b2 = box( {Pmin=Point( 25.4e-3, float(-1.*th_holder),-B); Pmax=Point(B, E, A-B)},
           //               axis, up,orig,mat
           //             )
           //(b1,b2)

    [|Cylinder(cy_int); Cylinder(cy_out); Annular_Disc(fst_disc); Annular_Disc(snd_disc); Box(bx1); Box(bx2) |]
    //[|Cylinder(cy_int); Cylinder(cy_out); Box(bx1); Box(bx2) |]

let l_U200 = U200(Point(0.,0.,0.),UnitVector(0.,1.,0.),UnitVector(0.,0.,1.),"Material__27",'L',([||],[||]))
let r_U200 = U200(Point(0.,0.,0.),UnitVector(0.,1.,0.),UnitVector(0.,0.,1.),"Material__27",'R',([||],[||]))


// Create a Camera and the scene - Camera on 0,0,0 pointing to 1,0,0

open BackwardRender.Camera
open BackwardRender.BackTypes

// front side
let Camera = {EyePoint = Point(-0.50,-1.50,-0.0); LookAt= Vector(0.34,1.,0.0); Up=UnitVector(0.,0.,1.); // iris
               PixNumH=300;PixNumW=300;PixSize= 4e-4}

let light0 = {origin= Point(2.,-1.50,-10.5);intensity = 1.}    // light the system 
let light1 = {origin= Point(-0.05,-10.00,0.0);intensity =0.75}    // light the system

(*
// up side
let Camera = {EyePoint = Point(-0.00,-0.00,1.50); LookAt= Vector(0.,0.0,-1.0); Up=UnitVector(0.,1.,0.); // iris
               PixNumH=300;PixNumW=300;PixSize= 5e-4}

let light0 = {origin= Point(-2.,1.50,0.5);intensity = 1.}    // light the system 
let light1 = {origin= Point(-0.05,0.00,0.0);intensity =0.75}    // light the system
*)

let scene = {Camera=Camera;  Elements=r_U200;Materials=nmat ; Plights=[|light0|]}
//(Array.concat [[|Cylinder(cy2);Cylinder(cy)|];[|bim.[0];bim.[2]|] ]); Materials=nmat ; Plights=[|light0;light1|]} //[|Cylinder(cy);Cone(con);TruncatedCone(pcon)|]
//match scene.Elements.[0] with Cone x -> x.Origin
let render = Do_Casting (scene,2,true)
let spath = "U200R_front.bmp" // save on the folder of BackWardRender

// save in a file just in case
let separator = ";"
createBMP(render,spath)
