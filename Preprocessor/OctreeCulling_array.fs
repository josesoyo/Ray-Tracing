//namespace Preprocessor


module OctreeCulling// =
open Types.ObjectTypes
open Types.Algebra
//open OctreeCullingTypes
//open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
//open MathNet.Spatial.Euclidean
open Octree_Intersections
open BBoxMethods
let test(groups:group[], a:int) :group [] =
    if a > 1 then 
        groups
    else 
        [||]
let IDlist( i, b:bool) =
    if b then [|i|]
    else [||]
let vinside(vertice:float<m> [], box:BBox) =
    // Check if one vertex is inside the BBox - Not enough to say if 1Triangle is inside
    //               CAREFULL!!!
    //               One side should be < and the other >=, but I create a problem on the edges...
    //              To solve: if npart=nmax then pmax=pmax+0.01
    if vertice.[0] < box.Pmax.[0] && vertice.[1] < box.Pmax.[1] && vertice.[2] < box.Pmax.[2] then 
        if vertice.[0] >= box.Pmin.[0] && vertice.[1] >= box.Pmin.[1] && vertice.[2] >= box.Pmin.[2] then true
        else false     
    else false
let VertInside (box, vertices:float<m> [] [])= 
    //List of vertices inside a partition
    vertices|> Array.Parallel.map(fun x -> vinside(x,box))

let Edges2RayFrom (box:BBox) =
    // Find the RayFrom object of each edge of the partition box
    // 1st - Find the points of the cube in order of vector calculation
    let modpointmin (pmin:float<m> [],pmax:float<m> [], i: int) =
        if i = 0 then [|pmax.[0];pmin.[1];pmin.[2]|]
        else if i = 1 then [|pmin.[0];pmax.[1];pmin.[2]|]
        else [|pmin.[0];pmin.[1];pmax.[2]|]
    let modpointmins2 (pmin2:float<m> [],pmax:float<m> [], i: int) =
        //2nd generation of points to compute the vectors
        if i = 0 then [|[|pmin2.[0];pmax.[1];pmin2.[2]|];[|pmin2.[0];pmin2.[1];pmax.[2]|]|]
        else if i = 1 then [| [|pmax.[0];pmin2.[1];pmin2.[2]|] ; [|pmin2.[0];pmin2.[1];pmax.[2]|] |]
        else [|[|pmax.[0];pmin2.[1];pmin2.[2]|]; [|pmin2.[0];pmax.[1];pmin2.[2]|]|]
    let modpointmax (pmin:float<m> [],pmax:float<m> [], i: int) =
        //last  points to compute vectors (pmax-this)
        if i = 0 then [|pmin.[0];pmax.[1];pmax.[2]|]
        else if i = 1 then [|pmax.[0];pmin.[1];pmax.[2]|]
        else [|pmax.[0];pmax.[1];pmin.[2]|]
    //Points
    let pmins = [|0..2|] |> Array.map(fun x ->  modpointmin(box.Pmin,box.Pmax,x))
    let pintermediate = [|0..2|] |> Array.map (fun x ->  modpointmins2(pmins.[x],box.Pmax,x))
    let pmaxs = [|0..2|] |> Array.map(fun x->  modpointmax(box.Pmin,box.Pmax,x))
    //Vertexs
    let subs2RayFrom(toP:float<m> [], fromP: float<m> []) =
        // create the RayFrom object from two points
        // toPoint - fromPoint  = Vector3D
        let p1 = Point3D( float toP.[0], float toP.[1],float toP.[2])
        let p2 = Point3D(float fromP.[0],float fromP.[1],float fromP.[2])
        let ve= p1-p2
        {uvec=ve.Normalize();length=ve.Length;from= p2; travelled = 0.} //type RayFrom 
    let vfirst = pmins |> Array.map(fun x-> subs2RayFrom(x,box.Pmin))
    let vsecond =  
        [|0..2|]
        |>Array.collect(fun x -> [|subs2RayFrom(pintermediate.[x].[0],pmins.[x]);subs2RayFrom(pintermediate.[x].[1],pmins.[x])|])

    let vlast = pmaxs |> Array.collect(fun x -> [|subs2RayFrom(box.Pmax, x)|])
    //Do a mesh for the cube:
    let CubeVertices = Array.append ( Array.append [|box.Pmin|]  pmins) ( Array.append pmaxs [|box.Pmax|] ) 
    let CubeTriangles = [|[1;2;4];[1;4;3];[1;2;3]; [8;5;6];[8;7;6];[8;7;5]|]
    //let CubeDummyNormals = [|UnitVector3D(0.,0.,-1.)|] 

    let CubeMesh = {Vertices = CubeVertices |>Array.map(fun x -> Point3D(float x.[0],float x.[1], float x.[2]));
                    Triangles=CubeTriangles;
                    Bbox=box}
    (Array.append (Array.append vfirst vsecond) vlast, CubeMesh)
    // RayFrom Types of the edges of the cube

let RealBoolean(vertBool: bool [], tri:int list,vertices: float<m> [] [], edgeRays:RayFrom [], cubeMesh:OLDmesh) =
    if vertBool.[tri.[0]-1] &&  vertBool.[tri.[1]-1] &&  vertBool.[tri.[2]-1] then (true,[||])
    else
        let intersecTRI = edgeRays
                            |> Array.collect(fun x -> intersec_tri(x,
                                                                    vertices|> Array.map(fun elam -> Point3D(elam |> Array.map(fun x -> float x))),
                                                                    tri) )
                            |> Array.filter (fun x -> x.t > 0.000)
        let triangleRays = //01 12 20
            let r1 = Vector3D(float (vertices.[tri.[0]-1].[0] - vertices.[tri.[1]-1].[0]),
                                float (vertices.[tri.[0]-1].[1] - vertices.[tri.[1]-1].[1]), 
                                float (vertices.[tri.[0]-1].[2] - vertices.[tri.[1]-1].[2]))

            let r2 = Vector3D(float (vertices.[tri.[1]-1].[0] - vertices.[tri.[2]-1].[0]),
                                float (vertices.[tri.[1]-1].[1] - vertices.[tri.[2]-1].[1]), 
                                float (vertices.[tri.[1]-1].[2] - vertices.[tri.[2]-1].[2]))
                                  
            let r3 = Vector3D(float (vertices.[tri.[2]-1].[0] - vertices.[tri.[0]-1].[0]),
                                float (vertices.[tri.[2]-1].[1] - vertices.[tri.[0]-1].[1]), 
                                float (vertices.[tri.[2]-1].[2] - vertices.[tri.[0]-1].[2]))


            [|
            {uvec= r1.Normalize();length = r1.Length;
                from = Point3D(vertices.[tri.[1]-1] |> Array.map(fun elam -> float elam ));
                travelled = 0.};
            {uvec= r2.Normalize();length = r2.Length;
                from =Point3D(vertices.[tri.[1]-1]|> Array.map(fun elam -> float elam ));
                travelled = 0.}
            {uvec= r3.Normalize();length = r3.Length;
                from = Point3D(vertices.[tri.[2]-1]|> Array.map(fun elam -> float elam ))
                ;travelled = 0.}
                |]
        let cubeIntersected (triangleRay:RayFrom, cubeMesh:OLDmesh): Intersection [] =
            [|0..(Array.length(cubeMesh.Triangles)-1)|]
            |> Array.collect(fun x -> intersec_square(triangleRay,cubeMesh.Vertices,cubeMesh.Triangles.[x]))
            |> Array.filter(fun x -> x.t> 0.00)
        let intersecCube =triangleRays |> Array.collect(fun x -> cubeIntersected(x,cubeMesh)) 
        let allPos = Array.append intersecTRI intersecCube|> Array.map(fun x -> x.point)
        match allPos with 
        | [||] -> (false, [||])
        | _  -> (true, allPos)
        
let tinside(vertBool:bool [], vertices: float<m> [] [],triangles:int list [] ,box:BBox, gbox:BBox) =
    // Returns an Array with a tuple: (Triangle [], Point3D []) 
    //the bool says if there's intersection with the partition and [||] if there's on the edges
    // fun Edges2RayFrom(box) -> (Ray*cubemesh)
    let interbool = BoxBoxIntersection(box,gbox)
    if interbool then
        let (edgeRays, cubeMesh) =Edges2RayFrom(box)
        let BoolTandPoints = triangles |> Array.map(fun x -> RealBoolean(vertBool, x, vertices, edgeRays, cubeMesh) )
        let Triangles  = BoolTandPoints|>  Array.map(fun x -> fst x)  |> fun x ->  ( x ,triangles)
                                        ||> Array.map2(fun x y -> IDlist(y,x)) |> Array.collect(fun x -> x)
        //let Triangles =  [|0..(BoolT.Length-1)|] |> Array.collect(fun x -> IDlist(x,BoolT.[x])) 
        let Points = BoolTandPoints |> Array.collect(fun x -> snd x)
        (Triangles,Points)
    else ([||],[||])
let SubMeshBox(onLimits: Point3D [] ,boolv:bool [], vertices: float<m> [] [] ) =
    // Check the limits of the box inside the octree
    let vertlim= boolv |> Array.exists(fun elem -> elem = true)     // If there are points inside
    let limlim = onLimits |> Array.isEmpty                          // If there are points on the boundaries

    match (vertlim, limlim) with
    |(false,false) -> // Error, empty empty shouldn't exit
        {Pmin = [||]; Pmax = [||]}
    |(false, true) ->   // Only points on the limit
        let minX:float<m> = onLimits |> Array.minBy(fun elem -> elem.X) |> (fun elem -> elem.X) |> LanguagePrimitives.FloatWithMeasure
        let maxX:float<m> = onLimits |> Array.maxBy(fun elem -> elem.X)  |> (fun elem -> elem.X) |> LanguagePrimitives.FloatWithMeasure 
        let minY:float<m> = onLimits |> Array.minBy(fun elem -> elem.Y)  |> (fun elem -> elem.Y) |> LanguagePrimitives.FloatWithMeasure 
        let maxY:float<m> = onLimits |> Array.maxBy(fun elem -> elem.Y)  |> (fun elem -> elem.Y) |> LanguagePrimitives.FloatWithMeasure 
        let minZ:float<m> = onLimits |> Array.minBy(fun elem -> elem.Z) |> (fun elem -> elem.Z) |> LanguagePrimitives.FloatWithMeasure 
        let maxZ:float<m> = onLimits |> Array.maxBy(fun elem -> elem.Z) |> (fun elem -> elem.Z) |> LanguagePrimitives.FloatWithMeasure 
        {Pmin = [|minX;minY;minZ|]; Pmax = [|maxX;maxY;maxZ|]}
    |(true, false) ->   // Points inside
        let Pinside = (vertices, boolv)||> Array.map2(fun v b -> IDlist(v,b)) |> Array.collect(fun x -> x)// List of vertices inside
        let minX2:float<m> = Pinside |> Array.minBy(fun elem -> elem.[0]) |> (fun elem -> elem.[0]) 
        let maxX2:float<m> = Pinside |> Array.maxBy(fun elem -> elem.[0])  |> (fun elem -> elem.[0])  
        let minY2:float<m> = Pinside |> Array.minBy(fun elem -> elem.[1])  |> (fun elem -> elem.[1])  
        let maxY2:float<m> = Pinside |> Array.maxBy(fun elem -> elem.[1])  |> (fun elem -> elem.[1])  
        let minZ2:float<m> = Pinside |> Array.minBy(fun elem -> elem.[2]) |> (fun elem -> elem.[2])  
        let maxZ2:float<m> = Pinside |> Array.maxBy(fun elem -> elem.[2]) |> (fun elem -> elem.[2]) 
        {Pmin = [|minX2;minY2;minZ2|]; Pmax = [|maxX2;maxY2;maxZ2|]}




    |(true, true ) ->   // Points on the limit and inside
        // Points on the limits
        let minX:float<m> = onLimits |> Array.minBy(fun elem -> elem.X) |> (fun elem -> elem.X) |> LanguagePrimitives.FloatWithMeasure
        let maxX:float<m> = onLimits |> Array.maxBy(fun elem -> elem.X)  |> (fun elem -> elem.X) |> LanguagePrimitives.FloatWithMeasure 
        let minY:float<m> = onLimits |> Array.minBy(fun elem -> elem.Y)  |> (fun elem -> elem.Y) |> LanguagePrimitives.FloatWithMeasure 
        let maxY:float<m> = onLimits |> Array.maxBy(fun elem -> elem.Y)  |> (fun elem -> elem.Y) |> LanguagePrimitives.FloatWithMeasure 
        let minZ:float<m> = onLimits |> Array.minBy(fun elem -> elem.Z) |> (fun elem -> elem.Z) |> LanguagePrimitives.FloatWithMeasure 
        let maxZ:float<m> = onLimits |> Array.maxBy(fun elem -> elem.Z) |> (fun elem -> elem.Z) |> LanguagePrimitives.FloatWithMeasure 
        // Points inside
        let Pinside = (vertices, boolv)||> Array.map2(fun v b -> IDlist(v,b))|> Array.collect(fun x -> x) // List of vertices inside
        let minX2:float<m> = Pinside |> Array.minBy(fun elem -> elem.[0]) |> (fun elem -> elem.[0]) 
        let maxX2:float<m> = Pinside |> Array.maxBy(fun elem -> elem.[0])  |> (fun elem -> elem.[0])  
        let minY2:float<m> = Pinside |> Array.minBy(fun elem -> elem.[1])  |> (fun elem -> elem.[1])  
        let maxY2:float<m> = Pinside |> Array.maxBy(fun elem -> elem.[1])  |> (fun elem -> elem.[1])  
        let minZ2:float<m> = Pinside |> Array.minBy(fun elem -> elem.[2]) |> (fun elem -> elem.[2])  
        let maxZ2:float<m> = Pinside |> Array.maxBy(fun elem -> elem.[2]) |> (fun elem -> elem.[2]) 
        {Pmin = [|(min minX2 minX); (min minY2 minY); (min minZ minZ2)|]; 
            Pmax = [|(max maxX maxX2); (max maxY maxY2); (max maxZ maxZ2)|]}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Functions on all the groups in a mesh
    





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let  PartitionateGroup(space:BBox,groups:group [],vertices:float<m> [][]):(int*group) [] =
    //All about the partition of the groups/mesh 
    // Returns a Tuple with the ID of the group + the group itself

    // Compute vertices inside the partition
    let VertInBool = VertInside(space,vertices)

    // Iter on groups
    printfn "entering in tinside"
    let iter = groups |> Array.Parallel.mapi(fun i x -> (i,(tinside(VertInBool,vertices,x.Triangles,space,x.Bbox))))
                        // true if the triangles are not empty
                        |> Array.filter(fun x -> 
                                        (not  (Array.isEmpty (fst (snd x))))
                                        )   
                        // MeshID*(triangles*PointsOnBoundary)

    printfn "leaving in tinside"
    let subBox = iter |>  Array.Parallel.map(fun  x  -> SubMeshBox(snd (snd x ), VertInBool,vertices))
    printfn "Submeshbox done"
    (iter , subBox) ||> Array.map2(fun it sbox ->
                                                (fst it ,
                                                 {Name =(groups.[(fst it)].Name) ; Triangles = fst(snd(it));Normals = [||]; 
                                                  Bbox = sbox; MatName=(groups.[(fst it)].MatName)})
                                            )


   

