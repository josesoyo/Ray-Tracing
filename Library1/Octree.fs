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
                subgroups |> Array.sumBy(fun x -> (snd x).Triangles.Length)
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
                                 printfn "aa bb cc are %d %d %d" aa bb cc
                                 let out = (fst tupleResult)
                                 if snd tupleResult < maxEle then // the maxEle should count the maxDepth counter  
                                     [|Partition(out)|]
                                 else  
                                     // Compute bbox of elements
                                     let mutable reducedBox = {Pmin = Point(infinity, infinity,infinity);
                                                               Pmax = Point(-infinity,-infinity,-infinity)}
                                     // Compute union of two bboxes                           
                                     out |> Array.iter(fun n -> reducedBox <- BoxofUnion((snd n).Bbox, reducedBox))

                                     [|Octree({Bbox =reducedBox ; Octree =CreateOctree (group, vert, reducedBox, maxEle,depth+1,maxDepth) })|]
                                )
                                
                              |]
                             )
                           )
                         ) |> Array.collect(fun z -> z)
        
        
       
        partition