//namespace Preprocessor

module SimpleRead//=
open System.IO
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames 
open MathNet.Numerics.LinearAlgebra
open MathNet.Spatial.Euclidean // requieres System.Xml
open Types.ObjectTypes

let VerfromFile (seq:string)=
    if seq.StartsWith("vn") then
        // reserved because vn means vertice normal and must be ignored 
        [||]
    elif seq.StartsWith("v") then
        //printfn "tenemos un vector!: %s" seq
        let a= seq.Substring(2) // it's 3 from files comimg from 3DSmax....
        //printfn "tenemos los valores : %s" a
        let b = a.Split(' ')
        // Convert the array into a list and string to float
        //printfn "input: %+A" a
        //printfn "input: %+A" b
        let (x,y,z) =  (  float(b.[0]),float(b.[1]) ,float(b.[2]))//(float(b.[1]),float(b.[2]),float(b.[0]))
        let x2:float<metre> = (x |> LanguagePrimitives.FloatWithMeasure)// |> <metre/'u>
        let y2:float<metre> = (y |> LanguagePrimitives.FloatWithMeasure)// |> <metre/'u>
        let z2:float<metre> = (z |> LanguagePrimitives.FloatWithMeasure)// |> <metre/'u>
        let pos = [|x2; y2; z2|]
        [|pos|]
    else
        printfn "Error on VertfromFile (read vertices)"
        [||]
        //printfn "%A" pos
            
let VerNfromFile (seq:string)=
    if seq.StartsWith("vn") then
        // reserved because vn means vertice normal and must be ignored 
        //printfn "tenemos un vector!: %s" seq
        let a= seq.Substring(3) // it's 3 from files comimg from 3DSmax....
        //printfn "tenemos los valores : %s" a
        let b = a.Split(' ')
        // Convert the array into a list and string to float
        //printfn "input: %+A" a
        //printfn "input: %+A" b
        let (x,y,z) = (float(b.[0]),float(b.[1]),float(b.[2]))//(float(b.[1]),float(b.[2]),float(b.[0]))
        let pos = [x; y; z]
        [|pos|]

    else
        printfn "There's an error on VertNfromFile"
        [||]
        //printfn "%A" pos


let TrifromFile (seq:string) =
    if seq.StartsWith("f") then
        //printfn "tenemos un vector!: %s" seq
        let a= seq.Substring(2)
        //printfn "tenemos los valores : %s" a
        let b = a.Split(' ')
        // Convert the array into a list and string to float
        let (x,y,z) =(int(b.[0]),int(b.[1]),int(b.[2])) //(int(b.[1]),int(b.[2]),int(b.[0]))
        let pos = [x; y; z]
        pos
    else
        printfn "There is an error on TrifromFile function"
        []    //    printfn "tenemos un triangulo: %s" seq 
let TriVertNfromFile (seq:string) =
    if seq.StartsWith("f") then
        //printfn "tenemos un vector!: %s" seq
        let a= seq.Substring(2)
        //printfn "tenemos los valores : %s" a
        let b = a.Split(' ')
        let primi = b.[0].Split('/')
        let second = b.[1].Split('/')
        let terzo = b.[2].Split('/')
        // Convert the array into a list and string to float
        let (x,y,z) =(int(primi.[0]),int(second.[0]),int(terzo.[0])) 
        let (xn,yn,zn) = (int(primi.[2]),int(second.[2]),int(terzo.[2]))
        let pos = [x; y; z;xn ;yn ;zn]  //[triangle;normal]
        pos
    else
        printfn "There is an error on TrifromFile function"
        []    //    printfn "tenemos un triangulo: %s" seq 


(*
let MeshNormals22(nodes:float [] list, triangle:int list) =
    let mutable normals =[UnitVector3D(0.0,0.0,1.0)]
    //printfn "triangles %d" triangle.Length
    printfn "triangulo: %+A"triangle
    printfn "%+A %+A %+A"  nodes.[7213] nodes.[7214] nodes.[7212] 
    printfn "%+A"  ((triangle.[0])-1)
    printfn "%+A"  ((triangle.[1])-1)
    printfn "%+A"  ((triangle.[2])-1)
    let n0 = ((triangle.[0])-1)
    let n1 = ((triangle.[1])-1)
    let n2 = ((triangle.[2])-1)
    //printfn "%d %d %d" n0 n1 n2
    let node0 = nodes.[n0]
    let node1 = nodes.[n1]
    let node2 = nodes.[n2]
    let iu1x,iu1y,iu1z =nodes.[n1].[0]-nodes.[n0].[0],nodes.[n1].[1]-nodes.[n0].[1],nodes.[n1].[2]-nodes.[n0].[2]
    printfn "%+A %+A %+A" iu1x iu1y iu1z
    let u0u1 = Vector3D(iu1x,iu1y,iu1z)
    let iu2x,iu2y,iu2z = nodes.[n2].[0]-nodes.[n0].[0],nodes.[n2].[1]-nodes.[n0].[1],nodes.[n2].[2]-nodes.[n0].[2]
    let u0u2 =  Vector3D(iu2x,iu2y,iu2z) //must be cyclic
    printfn "ciao ciao"

    // normal of the triangle and associate Plane 
    let normal = (u0u1.CrossProduct(u0u2)).Normalize()
    normal

    *)
let MeshNormals(nodes:float<metre> [] [], triangle:int list) =
    //let mutable normals =[UnitVector3D(0.0,0.0,1.0)]
    //printfn "triangles %d" triangle.Length
    let removeUnit (x:float<_>) =     float x  //remove the units

    let (n0,n1,n2) = ((triangle.[0])-1, (triangle.[1])-1, (triangle.[2])-1)
    //printfn "%d %d %d" n0 n1 n2
    let (u0u1,u0u2) = (Vector3D(removeUnit(nodes.[n1].[0]-nodes.[n0].[0]),
                                removeUnit(nodes.[n1].[1]-nodes.[n0].[1]),
                                removeUnit(nodes.[n1].[2]-nodes.[n0].[2])),
                        Vector3D(removeUnit(nodes.[n2].[0]-nodes.[n0].[0]),
                                removeUnit(nodes.[n2].[1]-nodes.[n0].[1]),
                                removeUnit(nodes.[n2].[2]-nodes.[n0].[2]))) //must be cyclic
    // normal of the triangle and associate Plane 
    let bpar=  u0u1.IsParallelTo(u0u2,1e-10)
    let normal =
        if bpar then UnitVector3D(0.,0.,0.)
        else  (u0u1.CrossProduct(u0u2)).Normalize()
    normal
    
let TestMeshParallel(nodes:float<metre> [] [], triangle:int list) =
    // Return true if the vectors of the triangle are parallel, it means zero area

    let removeUnit (x:float<_>) =     float x  //remove the units

    let (n0,n1,n2) = ((triangle.[0])-1, (triangle.[1])-1, (triangle.[2])-1)
    //printfn "%d %d %d" n0 n1 n2
    let (u0u1,u0u2) = (Vector3D(removeUnit(nodes.[n1].[0]-nodes.[n0].[0]),
                                removeUnit(nodes.[n1].[1]-nodes.[n0].[1]),
                                removeUnit(nodes.[n1].[2]-nodes.[n0].[2])),
                        Vector3D(removeUnit(nodes.[n2].[0]-nodes.[n0].[0]),
                                removeUnit(nodes.[n2].[1]-nodes.[n0].[1]),
                                removeUnit(nodes.[n2].[2]-nodes.[n0].[2]))) //must be cyclic
    // normal of the triangle and associate Plane 
    u0u1.IsParallelTo(u0u2,1e-10) 

let OpenMatLib(seq:string) =
    // Just read the string of the direction of the library
    let shortPath = "wavefront\\" + seq.Substring(7)
    Path.Combine(__SOURCE_DIRECTORY__,shortPath)
let ReadMatLib(seq:string) =
    seq.Substring(7)    

        