namespace Preprocessor

module Octree =
    open Types.ObjectTypes
    open BBoxMethods
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
    open OctreeCulling

    let OctBucle(aa,bb,cc,group: group [] , vertices:float<metre> [] [], space:BBox, maxEle:int ,depth:int ,maxDepth:int) =
        let stepx = (space.Pmax.[0]- space.Pmin.[0])/2.
        let stepy = (space.Pmax.[1]- space.Pmin.[1])/2.
        let stepz = (space.Pmax.[2]- space.Pmin.[2])/2.
        // Boundaries of the partition
        let xmin = stepx*float(aa-1)+ space.Pmin.[0]
        let xmax = stepx*float(aa)+ space.Pmin.[0]
        let ymin = stepy*float(bb-1)+ space.Pmin.[1]
        let ymax = stepy*float(bb)+ space.Pmin.[1]
        let zmin = stepz*float(cc-1)+ space.Pmin.[2]
        let zmax = stepz*float(cc)+ space.Pmin.[2]
        let thisbox = {Pmin=[|xmin;ymin;zmin|]; Pmax = [|xmax;ymax;zmax|]}

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
    let rec CreateOctree (group:group [] , vert:float<metre> [] [], space:BBox, maxEle:int ,depth:int ,maxDepth:int):OctreeSystem []=
        // Function to create the octree based on the current situation of the types
        // The output of the octree will be a group array
        let infi:float<metre> = (infinity |> LanguagePrimitives.FloatWithMeasure)     //inifinity with unit of measure
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
                                     let mutable reducedBox = {Pmin = [|infi; infi;infi|];
                                                               Pmax = [|-infi;-infi;-infi|]}
                                     // Compute union of two bboxes                           
                                     out |> Array.iter(fun n -> reducedBox <- BoxofUnion((snd n).Bbox, reducedBox))

                                     [|Octree({Bbox =reducedBox ; Octree =CreateOctree (group, vert, space, maxEle,depth+1,maxDepth) })|]
                                )
                                
                              |]
                             )
                           )
                         ) |> Array.collect(fun z -> z)
        
        
       
        partition