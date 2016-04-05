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
let vinside(vertice:Point , box:BBox) =
    // Check if one vertex is inside the BBox - Not enough to say if 1Triangle is inside
    //               CAREFULL!!!
    //               One side should be < and the other >=, but I create a problem on the edges...
    //              To solve: if npart=nmax then pmax=pmax+0.01
    if vertice.X <= box.Pmax.X && vertice.Y <= box.Pmax.Y && vertice.Z <= box.Pmax.Z then 
        if vertice.X >= box.Pmin.X && vertice.Y >= box.Pmin.Y && vertice.Z >= box.Pmin.Z then true
        else false     
    else false
let VertInside (box, vertices:Point [])= 
    //List of vertices inside a partition
    vertices|> Array.Parallel.map(fun x -> vinside(x,box))

let Edges2RayFrom (box:BBox) =
    // Find the RayFrom object of each edge of the partition box
    // 1st - Find the points of the cube in order of vector calculation
    let modpointmin (pmin:Point,pmax:Point, i: int) =
        if i = 0 then Point(pmax.X,pmin.Y,pmin.Z)
        else if i = 1 then Point(pmin.X,pmax.Y,pmin.Z)
        else Point(pmin.X,pmin.Y,pmax.Z)
    let modpointmins2 (pmin2:Point,pmax:Point, i: int) =
        //2nd generation of points to compute the vectors
        if i = 0 then [|Point(pmin2.X,pmax.Y,pmin2.Z);Point(pmin2.X,pmin2.Y,pmax.Z)|]
        else if i = 1 then [| Point(pmax.X,pmin2.Y,pmin2.Z) ; Point(pmin2.X,pmin2.Y,pmax.Z) |]
        else [|Point(pmax.X,pmin2.Y,pmin2.Z); Point(pmin2.X,pmax.Y,pmin2.Z)|]
    let modpointmax (pmin:Point,pmax:Point, i: int) =
        //last  points to compute vectors (pmax-this)
        if i = 0 then Point(pmin.X,pmax.Y,pmax.Z)
        else if i = 1 then Point(pmax.X,pmin.Y,pmax.Z)
        else Point(pmax.X,pmax.Y,pmin.Z)
    //Points
    let pmins = [|0..2|] |> Array.map(fun x ->  modpointmin(box.Pmin,box.Pmax,x))
    let pintermediate = [|0..2|] |> Array.map (fun x ->  modpointmins2(pmins.[x],box.Pmax,x))
    let pmaxs = [|0..2|] |> Array.map(fun x->  modpointmax(box.Pmin,box.Pmax,x))
    //Vertexs
    let subs2RayFrom(toP:Point, fromP: Point) =
        // create the RayFrom object from two points
        // toPoint - fromPoint  = Vector3D
        //let p1 = Point3D( float toP.[0], float toP.[1],float toP.[2])
        //let p2 = Point3D(float fromP.[0],float fromP.[1],float fromP.[2])
        let ve= toP-fromP
        {uvec=ve.ToUnitVector();length=ve.Module();from= fromP; travelled = 0.} //type RayFrom 
    let vfirst = pmins |> Array.map(fun x-> subs2RayFrom(x,box.Pmin))
    let vsecond =  
        [|0..2|]
        |>Array.collect(fun x -> [|subs2RayFrom(pintermediate.[x].[0],pmins.[x]);subs2RayFrom(pintermediate.[x].[1],pmins.[x])|])

    let vlast = pmaxs |> Array.collect(fun x -> [|subs2RayFrom(box.Pmax, x)|])
    //Do a mesh for the cube:
    let CubeVertices = Array.append ( Array.append [|box.Pmin|]  pmins) ( Array.append pmaxs [|box.Pmax|] ) 
    let CubeTriangles = [|[1;2;4];[1;4;3];[1;2;3]; [8;5;6];[8;7;6];[8;7;5]|]
    //let CubeDummyNormals = [|UnitVector3D(0.,0.,-1.)|] 

    let CubeMesh = {Vertices = CubeVertices //|>Array.map(fun x -> Point(x.X, x.Y, x.Z));
                    Triangles=CubeTriangles;
                    Bbox=box}
    (Array.append (Array.append vfirst vsecond) vlast, CubeMesh)
    // RayFrom Types of the edges of the cube

let RealBoolean(vertBool: bool [], tri:int list,vertices: Point [], edgeRays:RayFrom [], cubeMesh:OLDmesh) =
    if vertBool.[tri.[0]-1] &&  vertBool.[tri.[1]-1] &&  vertBool.[tri.[2]-1] then (true,[||])
    else
        let intersecTRI = edgeRays
                            |> Array.collect(fun x -> intersec_tri(x,
                                                                    vertices,
                                                                    tri) )
                            |> Array.filter (fun x -> x.t > 0.000)
        let triangleRays = //01 12 20
            let r1 = Vector(float (vertices.[tri.[0]-1].X - vertices.[tri.[1]-1].X),
                                float (vertices.[tri.[0]-1].Y - vertices.[tri.[1]-1].Y), 
                                float (vertices.[tri.[0]-1].Z - vertices.[tri.[1]-1].Z))

            let r2 = Vector(float (vertices.[tri.[1]-1].X - vertices.[tri.[2]-1].X),
                                float (vertices.[tri.[1]-1].Y - vertices.[tri.[2]-1].Y), 
                                float (vertices.[tri.[1]-1].Z - vertices.[tri.[2]-1].Z))
                                  
            let r3 = Vector(float (vertices.[tri.[2]-1].X - vertices.[tri.[0]-1].X),
                                float (vertices.[tri.[2]-1].Y - vertices.[tri.[0]-1].Y), 
                                float (vertices.[tri.[2]-1].Z - vertices.[tri.[0]-1].Z))


            [|
            {uvec= r1.ToUnitVector();length = r1.Module();
                from = vertices.[tri.[1]-1];
                travelled = 0.};
            {uvec= r2.ToUnitVector();length = r2.Module();
                from =vertices.[tri.[2]-1];
                travelled = 0.}
            {uvec= r3.ToUnitVector();length = r3.Module();
                from = vertices.[tri.[0]-1]
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
        
let tinside(vertBool:bool [], vertices: Point [],trianglesnormals:(int list*UnitVector) [] ,box:BBox, gbox:BBox) =
    // Returns an Array with a tuple: (Triangle [], Point3D []) 
    //the bool says if there's intersection with the partition and [||] if there's on the edges
    // fun Edges2RayFrom(box) -> (Ray*cubemesh)
    let interbool = BoxBoxIntersection(box,gbox)
    if interbool then
        let (edgeRays, cubeMesh) =Edges2RayFrom(box)
        let BoolTandPoints = trianglesnormals |> Array.map(fun x -> RealBoolean(vertBool,fst x, vertices, edgeRays, cubeMesh) )
        let TrianglesNormals  = BoolTandPoints|>  Array.map(fun x -> fst x)  |> fun x ->  ( x ,trianglesnormals)
                                        ||> Array.map2(fun x y -> IDlist(y,x)) |> Array.collect(fun x -> x)
        //let Triangles =  [|0..(BoolT.Length-1)|] |> Array.collect(fun x -> IDlist(x,BoolT.[x])) 
        let Points = BoolTandPoints |> Array.collect(fun x -> snd x)
        (TrianglesNormals,Points)
    else ([||],[||])
let SubMeshBox(onLimits: Point [] ,boolv:bool [], vertices: Point [] ) =
    // Check the limits of the box inside the octree.Z
    let vertlim= boolv |> Array.exists(fun elem -> elem = true)     // If there are points inside
    let limlim = onLimits |> Array.isEmpty |> not                   // If there are points on the boundaries

    match (vertlim, limlim) with
    |(false,false) -> // Error, empty empty shouldn't exit
        {Pmin = Point(infinity,infinity,infinity); Pmax = Point(-infinity,-infinity,-infinity)}
    |(false, true) ->   // Only points on the limit
        let minX = onLimits |> Array.minBy(fun elem -> elem.X) |> (fun elem -> elem.X) 
        let maxX = onLimits |> Array.maxBy(fun elem -> elem.X)  |> (fun elem -> elem.X)  
        let minY = onLimits |> Array.minBy(fun elem -> elem.Y)  |> (fun elem -> elem.Y)  
        let maxY = onLimits |> Array.maxBy(fun elem -> elem.Y)  |> (fun elem -> elem.Y)  
        let minZ = onLimits |> Array.minBy(fun elem -> elem.Z) |> (fun elem -> elem.Z)  
        let maxZ = onLimits |> Array.maxBy(fun elem -> elem.Z) |> (fun elem -> elem.Z)  
        {Pmin = Point(minX,minY,minZ); Pmax = Point(maxX,maxY,maxZ)}
    |(true, false) ->   // Points inside
        let Pinside = (vertices, boolv)||> Array.map2(fun v b -> IDlist(v,b)) |> Array.collect(fun x -> x)// List of vertices inside
        let minX2 = Pinside |> Array.minBy(fun elem -> elem.X) |> (fun elem -> elem.X) 
        let maxX2 = Pinside |> Array.maxBy(fun elem -> elem.X)  |> (fun elem -> elem.X)  
        let minY2 = Pinside |> Array.minBy(fun elem -> elem.Y)  |> (fun elem -> elem.Y)  
        let maxY2 = Pinside |> Array.maxBy(fun elem -> elem.Y)  |> (fun elem -> elem.Y)  
        let minZ2 = Pinside |> Array.minBy(fun elem -> elem.Z) |> (fun elem -> elem.Z)  
        let maxZ2 = Pinside |> Array.maxBy(fun elem -> elem.Z) |> (fun elem -> elem.Z) 
        {Pmin = Point(minX2,minY2,minZ2); Pmax = Point(maxX2,maxY2,maxZ2)}




    |(true, true ) ->   // Points on the limit and inside
        // Points on the limits
        let minX = onLimits |> Array.minBy(fun elem -> elem.X) |> (fun elem -> elem.X) 
        let maxX = onLimits |> Array.maxBy(fun elem -> elem.X)  |> (fun elem -> elem.X)  
        let minY = onLimits |> Array.minBy(fun elem -> elem.Y)  |> (fun elem -> elem.Y)  
        let maxY = onLimits |> Array.maxBy(fun elem -> elem.Y)  |> (fun elem -> elem.Y)  
        let minZ = onLimits |> Array.minBy(fun elem -> elem.Z) |> (fun elem -> elem.Z)  
        let maxZ = onLimits |> Array.maxBy(fun elem -> elem.Z) |> (fun elem -> elem.Z)  
        // Points inside
        let Pinside = (vertices, boolv)||> Array.map2(fun v b -> IDlist(v,b))|> Array.collect(fun x -> x) // List of vertices inside
        let minX2 = Pinside |> Array.minBy(fun elem -> elem.X) |> (fun elem -> elem.X) 
        let maxX2 = Pinside |> Array.maxBy(fun elem -> elem.X)  |> (fun elem -> elem.X)  
        let minY2 = Pinside |> Array.minBy(fun elem -> elem.Y)  |> (fun elem -> elem.Y)  
        let maxY2 = Pinside |> Array.maxBy(fun elem -> elem.Y)  |> (fun elem -> elem.Y)  
        let minZ2 = Pinside |> Array.minBy(fun elem -> elem.Z) |> (fun elem -> elem.Z)  
        let maxZ2 = Pinside |> Array.maxBy(fun elem -> elem.Z) |> (fun elem -> elem.Z) 
        {Pmin = Point((min minX2 minX), (min minY2 minY), (min minZ minZ2)); 
            Pmax = Point((max maxX maxX2), (max maxY maxY2), (max maxZ maxZ2))}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Functions on all the groups in a mesh
    





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let  PartitionateGroup(space:BBox,groups:group [],vertices:Point[]):(int*group) [] =
    //All about the partition of the groups/mesh 
    // Returns a Tuple with the ID of the group + the group itself

    // Compute vertices inside the partition
    let VertInBool = VertInside(space,vertices)

    // Iter on groups
    //printfn "entering in tinside"
    let iter = groups |> Array.Parallel.mapi(fun i x -> (i,(tinside(VertInBool,vertices,x.TrianglesNormals,space,x.Bbox))))
                        // true if the triangles are not empty
                        |> Array.filter(fun x -> 
                                        (not  (Array.isEmpty (fst (snd x))))
                                        )   
                        // MeshID*(triangles*PointsOnBoundary)

    //printfn "leaving in tinside"
    let subBox = iter |>  Array.Parallel.map(fun  x  -> SubMeshBox(snd (snd x ), VertInBool,vertices))
    //printfn "Submeshbox done"
    (iter , subBox) ||> Array.map2(fun it sbox ->
                                                (fst it ,
                                                 {Name =(groups.[(fst it)].Name) ; TrianglesNormals = fst(snd(it));
                                                  Bbox = sbox; MatName=(groups.[(fst it)].MatName)})
                                            )


   

