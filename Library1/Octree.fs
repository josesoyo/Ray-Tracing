namespace Preprocessor

module Octree =
    open Types.ObjectTypes
    open BBoxMethods
    open OctreeCulling
    open Types.Algebra
    //open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

    let OctBucle(aa,bb,cc,group: group [] , vertices:Point [], space:BBox, maxEle:int ,depth:int ,maxDepth:int) =
        let stepx = (space.Pmax.X- space.Pmin.X)/2.
        let stepy = (space.Pmax.Y- space.Pmin.Y)/2.
        let stepz = (space.Pmax.Z- space.Pmin.Z)/2.
        // Boundaries of the partition
        let xmin = stepx*float(aa-1)+ space.Pmin.X
        let xmax = stepx*float(aa)+ space.Pmin.X
        let ymin = stepy*float(bb-1)+ space.Pmin.Y
        let ymax = stepy*float(bb)+ space.Pmin.Y
        let zmin = stepz*float(cc-1)+ space.Pmin.Z
        let zmax = stepz*float(cc)+ space.Pmin.Z
        let thisbox = {Pmin=Point(xmin,ymin,zmin); Pmax = Point(xmax,ymax,zmax)}

        let subgroups = PartitionateGroup(thisbox,
                                           group,
                                           vertices )
        let numEle =        // Include the max depth as a terminator on Octree
            if depth < maxDepth then 
                subgroups |> Array.sumBy(fun x -> (snd x).TrianglesNormals.Length)
            else
                maxEle+100
        (subgroups,numEle)




    // Objectives of this module: Partitionate the meshes with an Octree algorithm
    let rec CreateOctree (group:group [] , vert:Point [], space:BBox, maxEle:int ,depth:int ,maxDepth:int):OctreeSystem []=
        // Function to create the octree based on the current situation of the types
        // The output of the octree will be a group array
        
        let partition = [|1..2|] |> Array.collect(fun aa -> 
                          [|1..2|] |> Array.collect(fun bb ->
                            [|1..2|] |> Array.collect(fun cc -> 
                              [|(let tupleResult = OctBucle(aa,bb,cc,group,vert,space,maxEle,depth,maxDepth ) 
                                 printfn "depth: %d \t aa bb cc are %d %d %d" depth aa bb cc
                                 let out = (fst tupleResult)

                                  // Compute bbox of elements
                                 let mutable reducedBox = {Pmin = Point(infinity, infinity,infinity);
                                                           Pmax = Point(-infinity,-infinity,-infinity)}
                                 // Compute union of two bboxes                           
                                 out |> Array.iter(fun n -> reducedBox <- BoxofUnion((snd n).Bbox, reducedBox))

                                 if snd tupleResult < maxEle then // the maxEle should count the maxDepth counter  
                                     if  (out |> Array.isEmpty) then [||] // I was creating a Partition being empty...
                                     else [|Partition({Bbox= reducedBox; Partition = out})|]
                                 else  
                                    
                                     [|Octree({Bbox =reducedBox ; Octree =CreateOctree (group, vert, reducedBox, maxEle,depth+1,maxDepth) })|]
                                ) 
                                
                              |] |> Array.collect(fun z -> z)
                             )
                           )
                         ) //|> Array.collect(fun z -> z)
        
        
       
        partition



    //
    //      Async part
    //

    let rec OctreeAsync(group:group [] , vert:Point [], space:BBox, maxEle:int ,depth:int ,maxDepth:int):OctreeSystem []=

        // Prepare the List to asyncronize
        let order = [1..2] |> List.collect( fun x -> 
                     [1..2]  |> List.collect(fun y ->
                         [1..2] |> List.map(fun z -> (x,y,z))
                         ))


                        
        

        let asyncOct(orderi,group,vert,space,maxEle,depth,maxDepth ) = async { let (aa,bb,cc)= orderi
                                                                              return OctreeCore(aa,bb,cc,group,vert,space,maxEle,depth,maxDepth ) }

        let partition = order 
                          |> List.collect(fun x -> [asyncOct(x,group,vert,space,maxEle,depth,maxDepth )])
                          |> Async.Parallel |> Async.RunSynchronously
                          |> Array.collect(fun x -> x) 

                          

        partition
    //
    and OctreeCore(aa,bb,cc,group,vert,space,maxEle,depth,maxDepth )=
        // to be executed inside OctreeAsync, it was a lambda
        let tupleResult =OctBucle(aa,bb,cc,group,vert,space,maxEle,depth,maxDepth )
        printfn "aa bb cc and detph are %d %d %d %d" aa bb cc depth
        let out = (fst tupleResult)
        // Compute bbox of elements
        let mutable reducedBox = {Pmin = Point(infinity, infinity,infinity);
                                  Pmax = Point(-infinity,-infinity,-infinity)}
        // Compute union of two bboxes                           
        out |> Array.iter(fun n -> reducedBox <- BoxofUnion((snd n).Bbox, reducedBox))
        if snd tupleResult < maxEle then // the maxEle should count the maxDepth counter  
            [|Partition({Bbox= reducedBox; Partition = out})|]
        else  
            //  Not recursive the creation of async because I use the non parallel version for recursivity.
            [|Octree({Bbox =reducedBox ; Octree =CreateOctree (group, vert, reducedBox, maxEle,depth+1,maxDepth) })|]



    //