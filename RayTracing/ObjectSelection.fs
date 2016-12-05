namespace RayTracing

module ObjectSelection =
    // Module to select the object that it's going to be intersected
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    open Types.Algebra
    open Types
    open Types.types
    open Types.ObjectTypes
    open RayTracing.RayStructureIntersection
    open RayTracing.intersections

    let match_element(ray:Ray,obj:Object) =
        match obj with
        | Mesh x ->        IntersectionOctree (ray,x.Octree,x.Mesh)

        | Cylinder x ->    let inter = intersect_cyl(ray,x) |> Array.filter(fun x -> x.t > 1e-10<m>) 
                           match inter with 
                           |[||] -> None 
                           |_ -> Some(inter|> Array.minBy(fun x -> x.t))

        | SurfaceLens x -> let inter = intersect_SphSurfaceLens(ray,x) |> Array.filter(fun x -> x.t > 1e-10<m>)
                           match inter with 
                           |[||] -> None 
                           |_ -> Some(inter|> Array.minBy(fun x -> x.t))

        | Disc x ->        let inter = intersect_Disk(ray,x) |> Array.filter(fun x -> x.t > 1e-10<m>)
                           match inter with
                           | [||] -> None
                           | _  -> Some(inter |> Array.minBy(fun x -> x.t))
        | Sphere x ->     let inter = intersect_Sphere(ray,x) |> Array.filter(fun x -> x.t > 1e-10<m>) 
                          match inter with
                           | [||] -> None
                           | _  -> Some(inter |> Array.minBy(fun x -> x.t))
        | Box x ->        let inter = intersect_Box(ray, x) |> Array.filter(fun x -> x.t > 1e-10<m>)
                          match inter with 
                          | [||] -> None
                          | _ -> Some(inter  |> Array.minBy(fun x -> x.t))
        | Cone x ->       let inter = intersect_Cone(ray, x) |> Array.filter(fun x -> x.t > 1e-10<m>)
                          match inter with 
                          | [||] -> None
                          | _ -> Some(inter  |> Array.minBy(fun x -> x.t))
        | TruncatedCone x ->       let inter = intersect_TruncatedCone(ray, x) |> Array.filter(fun x -> x.t > 1e-10<m>)
                                   match inter with 
                                   | [||] -> None
                                   | _ -> Some(inter  |> Array.minBy(fun x -> x.t))
        | Annular_Disc x ->   let inter = intersect_annular_disc(ray, x) |> Array.filter(fun x -> x.t > 1e-10<m>)
                              match inter with 
                              | [||] -> None
                              | _ -> Some(inter  |> Array.minBy(fun x -> x.t))
  
    let UnSomeNone(x) =
        match x with
        | Some x -> [|x|]
        | None   -> [||]

    let intersection_all(ray:Ray,objs:Object[]) =
        // do the intersection with all the objects of the scene
        objs 
        |> Array.map(fun x -> match_element(ray, x))                // already must return intersections which contains t > 0.
        //|> Array.map(fun x ->UnSomeNone(x) ) 
        |> Array.filter(fun x -> x <> None)
                                 // old method
                                 //let nar = UnSomeNone(x)
                                 //not(Array.isEmpty(nar))) 
        |> Array.map(fun x-> match x with Some x -> x )  // Equivalent to an Array.collect - SHOULD NEVER GIVE None BECAUSE I HAVE PREVIOUsLy FILTERED
        //|> Array.collect(fun x -> match x with Some x -> [|x|] | None -> [||])
    let intersection_all_forward(ray:Ray,objs:Object[]) =
        // do the intersection with all the objects of the scene
        // Function intended to use with forward ray tracing
        // The Array.mapi allows me to track the objects in order to later test if they are a sensor or not
        objs 
        |> Array.mapi(fun i x -> (match_element(ray, x),i))                // already must return intersections which contains t > 0.
        //|> Array.map(fun x ->UnSomeNone(x) ) 
        |> Array.filter(fun x ->fst x <> None)
                                 // old method
                                 //let nar = UnSomeNone(x)
                                 //not(Array.isEmpty(nar))) 
        |> Array.map(fun x-> match fst x with Some y -> (y,snd x) )  // Equivalent to an Array.collect - SHOULD NEVER GIVE None BECAUSE I HAVE PREVIOULT FILTERED
        //|> Array.minBy(fun x -> (fst x).t)  // I cannot do it here, if it receives an empty array, returns an error


