module TypesStruct

open Types.ObjectTypes
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Types.Algebra
open Types.types
open System

// Create some functions that will create complex structures - My idea is:
//    - Baffles
//    - lens
//    - Mirror (discs with the cylinder)

let Create_Baffles(extRad:float<m>,intRad:float<m>,orig:Point, baffle_Angle, //,baffle_Length:float<m> - not necessary, stupid
                    thickness:float<m>, nrm:UnitVector, matname:string,
                    snrs:Sensor, noise:noise) =
    // need:
    //type truncatedCone(radius:float<m>,height:float<m>,maxHeight:float<m> ,origin:Point, nrm:UnitVector, matname:string, sensor:Sensor, noise:noise) =
    // 
    let Cone_height = float(extRad)/tan(baffle_Angle)
                      |> LanguagePrimitives.FloatWithMeasure<m>  // height of the non truncated cone
    let baffle_Length = (extRad-intRad)/tan(baffle_Angle) |> abs

    let d1 = truncatedCone(extRad,Cone_height,baffle_Length ,orig, nrm, matname, snrs, noise)

    let Origin2 = 
        let intermediate = d1.Cone.Obj2World.RotatePoint(Point(0.,0.,float thickness))
        intermediate.MoveAndCreateNew(orig) // orign that counts the direction
    
    let d2 = truncatedCone(extRad,Cone_height,baffle_Length ,Origin2, nrm, matname, snrs, noise)

    // create the internal cylinder
    let cyl_Orig = 
       let intermediate=  d1.Cone.Obj2World.RotatePoint(Point(0.,0.,float baffle_Length))
       intermediate.MoveAndCreateNew(orig) // orign that counts the direction
    

    let cyli = cylinder(intRad,thickness , cyl_Orig,nrm ,matname,snrs, noise)

    [|TruncatedCone(d1);Cylinder(cyli);TruncatedCone(d2)|]

let biConvex(roc1:float<m>,roc2:float<m>, axis:UnitVector, th:float<m>, dia:float<m>, startingPoint:Point, matname:string,snrs:Sensor, noise:noise)=
    // biconvex lens
    //  s1 - cylinder - s2
    //         __
    //        (__)
    //       
    // clarification:
    //      Axis always goes from s1 to s2
    //    
    let naxis = axis.Negate()
    let bol = true
    let r1, r2 = float roc1, float roc2
    
    let centre1 = startingPoint+r1*axis // centre of surface1
    let centre2 =  startingPoint+float(th-roc2)*axis// centre of the surface2 
    
    let snrs1 = new Sensor(snrs)
    let snrs2 = new Sensor(snrs)

    // Surfaces of both lenses
    let s1 = SphSurfaceLens(centre1, roc1, dia, naxis,  true, matname, snrs1, noise)
    let s2 = SphSurfaceLens(centre2, roc2, dia, axis, true, matname, snrs2, noise)

    let S1Th = r1*(1.-s1.CosMin)
    let S2Th = r2*(1.-s2.CosMin)
    let cylStart = startingPoint+(S1Th)*axis
    let cylTh = float th-S1Th-S2Th 
                |> LanguagePrimitives.FloatWithMeasure<m>

    let edge = cylinder(dia/2.,cylTh,cylStart,axis,matname)
    [|SurfaceLens(s1);Cylinder(edge);SurfaceLens(s2)|]

let biConcave(roc1:float<m>,roc2:float<m>, axis:UnitVector, th:float<m>, dia:float<m>, startingPoint:Point, matname:string,snrs:Sensor, noise:noise) =
    // Biconcave lens
    //  s1 - cylinder - s2
    //         __
    //        )__(
    //       
    // clarification:
    //      Axis always goes from s1 to s2
    //

    let naxis = axis.Negate()
    //let bol = true
    let r1, r2 = float roc1, float roc2
    
    let centre1 = startingPoint+r1*naxis //axis.Negate() // centre of surface1
    let centre2 =  startingPoint+float(th+roc2)*axis    // centre of the surface2 
    // Surfaces of both lenses
    let snrs1 = new Sensor(snrs)
    let snrs2 = new Sensor(snrs)

    let s1 = SphSurfaceLens(centre1, roc1, dia, axis,  true, matname, snrs1, noise)
    let s2 = SphSurfaceLens(centre2, roc2, dia, naxis, true, matname, snrs2, noise)

    let S1Th = r1*(1.-s1.CosMin)
    let S2Th = r2*(1.-s2.CosMin)
    let cylStart = startingPoint+(S1Th)*naxis
    let cylTh = float th+S1Th+S2Th 
                |> LanguagePrimitives.FloatWithMeasure<m>

    let edge = cylinder(dia/2.,cylTh,cylStart,axis,matname)
    [|SurfaceLens(s1);Cylinder(edge);SurfaceLens(s2)|]


let Meniscus(roc1:float<m>,roc2:float<m>, axis:UnitVector, th:float<m>, dia:float<m>, startingPoint:Point, matname:string,snrs:Sensor, noise:noise) =
    // Meniscus lens
    //  s1 - cylinder - s2
    //         __
    //        (__(
    //       
    // clarification:
    //      Axis always goes from s1 to s2
    //
    let naxis = axis.Negate()
    
    let bol = true
    let r1, r2 = float roc1, float roc2
    
    let centre1 = startingPoint+r1*axis        // centre of surface1
    let centre2 =  startingPoint+float(th+roc2)*axis// centre of the surface2 
    
    let snrs1 = new Sensor(snrs)
    let snrs2 = new Sensor(snrs)


    // Surfaces of both lenses
    let s1 = SphSurfaceLens(centre1, roc1, dia, naxis,  true, matname, snrs1, noise)
    let s2 = SphSurfaceLens(centre2, roc2, dia, naxis, false, matname, snrs2, noise)

    let S1Th = r1*(1.-s1.CosMin) 
    let S2Th = r2*(1.-s2.CosMin)
    let cylStart = startingPoint+(S1Th)*axis
    let cylTh = (float th)-S1Th+S2Th 
                |> LanguagePrimitives.FloatWithMeasure<m>

    let edge = cylinder(dia/2.,cylTh,cylStart,axis,matname)
    [|SurfaceLens(s1);Cylinder(edge);SurfaceLens(s2)|]

let Create_Lens(centreofS1:Point, roc1:float<m>,roc2:float<m>, diam:float<m>,axis:UnitVector, thickness:float<m>,
                matname:string, snrs:Sensor, noise:noise,lensType:string) =

    //type SphSurfaceLens(centreofSPH, roc:float<m>, diam:float<m>,axs:UnitVector, conv:bool,matname:string, snrs:Sensor, noise:noise) =
    //type cylinder(rad:float<m>,zmax:float<m>,orig:Point,nrm:UnitVector,matname:string, snrs:Sensor, noise:noise) = 
    let nrm = axis.Negate()
    match lensType with 
    | "BiConvex"  -> biConvex(roc1,roc2, axis, thickness, diam, centreofS1, matname,snrs, noise)
    | "BiConcave" -> biConcave(roc1,roc2, axis, thickness, diam, centreofS1, matname,snrs, noise)
    | "Meniscus" ->  Meniscus(roc1,roc2, axis, thickness, diam, centreofS1, matname,snrs, noise)
    | _ -> 
        printfn "This is not a valid input for Create_Lens\nThe valid options are:\n\tBiConvex\n\tBiConcave\n\tMeniscus"
        [||]
(* 
let CreateLensBiConvex(r1:float, r2:float, axis:UnitVector3D, th:float, dia:float, startingPoint:Point3D, mat:material)=
    // Function to create a biconvex lens with two lens surface and one cylinder.
    // All defined with respect to s1 -> this means that for cyl and s2 the axis is the opposite
    let naxis = axis.Negate()
    let S1 = GenerateLensSurface(startingPoint+naxis.ScaleBy(r1), r1, mat, dia,axis,true)
    let S1Th = r1*(1.-S1.CosMin)
    let S2 = GenerateLensSurface(startingPoint+naxis.ScaleBy(th-r2), r2, mat, dia,naxis,true)
    let S2Th = r2*(1.-S2.CosMin)
    let cylth = th-S1Th-S2Th
    let cylSide = GenerateCylinder(dia/2., 0.,cylth,startingPoint+naxis.ScaleBy(S1Th),naxis,mat)
    (S1,cylSide,S2)

let CreateLensBiConcave(r1:float, r2:float, axis:UnitVector3D, th:float, dia:float, startingPoint:Point3D, mat:material)=
    // Function to create a biconvex lens with two lens surface and one cylinder.
    // All defined with respect to s1 -> this means that for cyl and s2 the axis is the opposite
    let naxis = axis.Negate()
    let S1 = GenerateLensSurface(startingPoint+axis.ScaleBy(r1), r1, mat, dia,naxis,false)
    let S1Th = r1*(1.-S1.CosMin)
    let S2 = GenerateLensSurface(startingPoint+naxis.ScaleBy(th+r2), r2, mat, dia,axis,false)
    let S2Th = r2*(1.-S2.CosMin)
    let cylth = th+S1Th+S2Th
    let cylSide = GenerateCylinder(dia/2., 0.,cylth,startingPoint+axis.ScaleBy(S1Th),naxis,mat)
(S1,cylSide,S2)   
*)



let mirror_flat(axis:UnitVector, th:float<m>, dia:float<m>, startingPoint:Point, matnameS1:string, matnameS2:string, matnameExt:string,snrs:Sensor, noise:noise) =
    // create a plane mirror
    //  _____
    //  |   |
    //  |   |-> (axis)
    //  |___|
    //  
    //  s1  s2
    // stargting point at s1

    let naxis = axis.Negate()
    
    let s1 = disc(startingPoint,float(dia)/2.,naxis,matnameS1,(new Sensor(snrs)),noise)
    let s2 = disc((startingPoint+float(th)*axis),
                   float(dia)/2.,axis,matnameS2,(new Sensor(snrs)),noise) 

    let edge = cylinder(dia/2.,th,startingPoint,axis,matnameExt)

    [|Disc(s1);Cylinder(edge);Disc(s2)|]
