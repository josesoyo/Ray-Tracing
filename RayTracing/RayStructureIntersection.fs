namespace RayTracing

module RayStructureIntersection =
    // two main functions must be created, one for the intersection of the octree and other for the partition
    open BBoxMethods
    open Types.types
    open Types.ObjectTypes
    open Types.Algebra
    open intersections
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

    let IntersectionGroup(ray:Ray,gr:group,msh:mesh):Intersection Option =
        let pp = (gr.TrianglesNormals) 
                 |> Array.collect(fun tri -> intersec_tri(ray,msh,fst tri,snd tri,gr.MatName))
                 |> Array.filter(fun x -> x.t > 1e-9<m>)            // Positive values
                 //|> Array.minBy(fun x -> x.t) 

        match pp with
        | [||] -> None
        | _ -> Some (pp|> Array.minBy(fun x -> x.t) )

    let IntersectionPartition(ray:Ray,part:Partition,msh:mesh):Intersection Option =
        // find the intersections with the groups inside the partition
        //let mutable initInteresection =  { normal=UnitVector(1.,1.,1.); point=Point(0.,0.,0.); ray=ray;MatName=""; t=0.<m>}
        
        let p = part.Partition |> Array.map(fun parti -> (BBox_intersec(ray, (snd parti).Bbox), (snd parti)) )       // Obtain only the partitions withouth the which one is info
                               |> Array.sortBy(fun x -> fst x )                 // order groups by distance
                               |> Array.filter(fun oc -> (fst oc) >0.)   // delete groups that don't intersect // originally: (fst oc) <> infinity
                    
        p |> Array.tryPick(fun x -> IntersectionGroup(ray, snd x, msh) ) 


    let intOctree(ray:Ray, octreeSystem) =
        // returns the closest distance to intercept the bounding box 
        match octreeSystem with
        | Octree x -> (BBox_intersec( ray, x.Bbox), Octree(x))
        | Partition x -> (BBox_intersec( ray, x.Bbox),Partition(x))



    //
    //  Main function
    //
    let rec  IntersectionOctree( ray:Ray, oct:OctreeSystem[], msh:mesh) =
        // Intersection of the octree
        let nodes1st = oct |> Array.map( fun octree -> intOctree(ray, octree))  // (dist, octree/partition)
                           |> Array.sortBy (fun oct -> fst oct)                 // Array ordered
                           |> Array.filter(fun oct -> (fst oct) > 0.)     // returns those that are not infinity, and negative = infinity
                           |> Array.map(fun oct -> snd oct)
        //let mutable initInteresection =  { normal=UnitVector(1.,1.,1.); point=Point(0.,0.,0.); ray=ray;MatName=""; t=0.<m>}
        // Iter the nodes until the first no empty is obtained.
        nodes1st 
        |> Array.tryPick (fun x ->  insideOct(ray, x,msh) )
        // I think I can work with Some/None -> None = No intersection
       
    and insideOct (ray:Ray ,oct: OctreeSystem,msh:mesh ) =
        match oct with
        | Octree x -> IntersectionOctree(ray, x.Octree,msh)
        | Partition x -> IntersectionPartition(ray, x,msh)
                              




    // intersection without the octree structure
    let IntersectionSimple (ray:Ray,msh: mesh)= //:Intersection Option =
        // Performs a simple intersection in order to render withouth the octree
        msh.groups 
        |> Array.map(fun parti -> (BBox_intersec(ray, parti.Bbox),parti) )       // Obtain only the partitions withouth the which one is info
        |> Array.sortBy(fun x -> fst x )                 // order groups by distance
        |> Array.filter(fun oc -> (fst oc) > 0.  ) // delete groups that don't intersect      
        |> Array.tryPick(fun x -> IntersectionGroup(ray,(snd x),msh))

       
    //use: IntersectionGroup(ray:Ray,gr:group,msh:mesh):Intersection Option