namespace BackwardRender


module SimpleShading =

    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    open Types.Algebra
    open Types
    open Types.types
    open Types.ObjectTypes
    open RayTracing.RayStructureIntersection
    open BackTypes
    open RayTracing.ObjectSelection

    let colorAtOctree (intersection:Intersection,scn:scene)=
        // Ia: Ambient Light
        // Id: Diffuse Light
        // Is: Specular Light

        // Ambient
        let Ia = 0.05
        let realMatName = RealMatName intersection.MatName (abs(intersection.ray.uvec*intersection.normal))
        let AmbLight = Ia*scn.Materials.[realMatName].T
        
        // Diffuse
        // Diffuse
        let DiffLight (intersection:Intersection, light:Plight ,fatt:float, normLightDir:UnitVector,scn:scene) =
            let KdOd = scn.Materials.[realMatName].T
            //let LightDir = light.origin - intersection.point
            //let NormLightDir = LightDir.Normalize() //dir from point to light
            let Id = light.intensity * (max (intersection.normal*(normLightDir)) 0.0)
            //let fatt= Fatt intersection light
            //printfn "Distance travelled is: %f" fatt
            KdOd*Id//if IsShadow intersection light then KdOd*Id*fatt*light.intensity //Id*Kd*Od
            //else Color(0.0,0.0,0.0)

        // Specular
        let SpecLight (intersection:Intersection, light:Plight ,fatt:float, normLightDir:UnitVector,scn:scene)=
            let Ks =  scn.Materials.[realMatName].R
            //let LightDir = light.origin - intersection.point
            //let NormLightDir = LightDir.Normalize() //dir from point to light
            let Rvect = (2.0*(intersection.normal)*(normLightDir))*intersection.normal + (-1.*normLightDir)
            let DotProds = max (Rvect*(-1.*intersection.ray.uvec)) 0.0
            let Is = light.intensity *pown DotProds 10
            //if DotProds = 0. then
            //    printfn "find an error"
            //else 2. |> ignore
            //let fatt= Fatt intersection light
            Ks*Is//if IsShadow intersection light then Ks*Is*fatt*light.intensity
        
        let IsShadow scn intersection light =
            // If LightDir.Length = 1 the function will FAIL
            let LightDir = light.origin - intersection.point
            let NormLightDir = LightDir.ToUnitVector()
            
            let RayLight = {intersection.ray with
                                uvec=NormLightDir; from=intersection.point; 
                                MaxLength = (LightDir.Module() |> LanguagePrimitives.FloatWithMeasure<m>); 
                             }
             
            let intersects = intersection_all(RayLight,scn.Elements)
                             //scn.Elements 
                             //|> Array.collect(fun x ->  [|IntersectionOctree( RayLight, x.Octree, x.Mesh)|]) // [|IntersectionSimple(RayLight,x.Mesh)|]) //
                             //|> fun x -> x.[0] // I only need the first one to say that the ray doesnt arrives to the light
                                //Cast_Octree(scn, RayLight,octree)//CastRay_nest//CastRay_nest (scn, RayLight)
            //printfn "there's an intersection at: %f" intersects.Head.ray.travelled
            match intersects with
                | [||] -> let fatt= 1. //Fatt intersection light //let unit = paralel intersection
                          DiffLight (intersection, light, fatt, NormLightDir,scn) + SpecLight (intersection, light, fatt, NormLightDir,scn)
                | _ ->  0. // Some x

        let OneShadow light = IsShadow scn intersection light
        AmbLight + Array.sumBy(fun x -> OneShadow x) scn.Plights

