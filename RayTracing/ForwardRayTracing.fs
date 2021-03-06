﻿module ForwardRayTracing
//#r @"C:\Users\Jose M. Gonzalez\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\Types\bin\Debug\Types.dll"
//#load "RayCore.fs"
//#load "RayStructureIntersection.fs"
//#load "ObjectSelection.fs"
//#load "ShadingForward.fs"
// open namespaces
open Types.Algebra
open Types.ObjectTypes
open Types.types
open RayTracing.ObjectSelection     // All about RayTracing
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open ShadingForward
open ShadingNoise
(*

                Case the ray carries just noise is not considered now

let UpdateSensor(ray:Ray,intersection:Intersection,obj:Object):Unit =
    let sc = SensorContent(intersection.point,ray.uvec,
                           ray.NumOfParticles, 
                           (ray.OpticalPathTravelled/(match (ray.Wavelenght) with WaveLength x ->x))%(float(match ray.Wavelenght with WaveLength x -> x)), 
                           ray.NoiseAdd ) // I need the intersection
    match obj with
    | Cylinder x ->   x.Sensor.AddData(sc)
    | SurfaceLens x-> x.Sensor.AddData(sc)
    | Disc x-> x.Sensor.AddData(sc)
    | Sphere x-> x.Sensor.AddData(sc)

    
// No sensor options
// Sensor options
let UpdateSensorSelection(ray:Ray, intersection:Intersection, objs:Object[],objID:int) =
    UpdateSensor(ray,intersection,objs.[objID])
*)
let UpdateSensorPhase(intersection:Intersection,obj:Object):Unit =
    let sc = SensorContent(intersection.point,intersection.ray.uvec,
                           intersection.ray.FracOfRay, 
                           ((intersection.ray.OpticalPathTravelled/(match intersection.ray.Wavelenght with WaveLength x -> x))*6.28318530718)%6.28318530718,  // 2*PI
                           intersection.ray.PhaseModulation, //|> Array.map(fun x -> float x),
                           intersection.ray.Memory) // I need the intersection
    match obj with
    | Cylinder x ->     x.Sensor.AddData(sc) //lock x.Sensor (fun () -> x.Sensor.AddData(sc))
    | Cylinder_With_Hole x -> x.Sensor.AddData(sc) 
    | SurfaceLens x->   x.Sensor.AddData(sc) //lock x.Sensor (fun () -> x.Sensor.AddData(sc))
    | Disc x->          x.Sensor.AddData(sc) //lock x.Sensor (fun () -> x.Sensor.AddData(sc))
    | Annular_Disc x -> x.Disc.Sensor.AddData(sc) //lock x.Disc.Sensor (fun () -> x.Disc.Sensor.AddData(sc))
    | Sphere x->        x.Sensor.AddData(sc) //lock x.Sensor (fun () -> x.Sensor.AddData(sc))
    | Cone x ->         x.Sensor.AddData(sc) //lock x.Sensor (fun () -> x.Sensor.AddData(sc))
    | TruncatedCone x ->x.Cone.Sensor.AddData(sc)
    | Box x ->          x.Sensor.AddData(sc)

let UpdateSensorSelectionPhase(intersection:Intersection, objs:Object[],objID:int) =
    UpdateSensorPhase(intersection,objs.[objID])
  
// Algorithm for the Ray tracing


// match sensor or not
let IsItSensor(objs:Object[], id:int):bool*bool =
    match objs.[id] with
    | Cylinder x ->     (x.Sensor.Exists, x.Sensor.Terminate) // says if it is a sensor or not and if it terminates
    | Cylinder_With_Hole x ->(x.Sensor.Exists, x.Sensor.Terminate) // says if it is a sensor or not and if it terminates
    | SurfaceLens x ->  (x.Sensor.Exists, x.Sensor.Terminate) // says if it is a sensor or not and if it terminates
    | Disc x ->         (x.Sensor.Exists, x.Sensor.Terminate)
    | Annular_Disc x -> (x.Disc.Sensor.Exists, x.Disc.Sensor.Terminate)
    | Sphere x ->       (x.Sensor.Exists, x.Sensor.Terminate)
    | Cone x ->         (x.Sensor.Exists, x.Sensor.Terminate)
    | TruncatedCone x ->(x.Cone.Sensor.Exists, x.Cone.Sensor.Terminate)
    | Box x ->          (x.Sensor.Exists, x.Sensor.Terminate)
    | _     ->          failwith "Object type not accepted for Sensor"

let getNoise(objs:Object[], id:int) =
    match objs.[id] with
    | Cylinder x ->     x.Noise
    | Cylinder_With_Hole x -> x.Noise
    | SurfaceLens x ->  x.Noise
    | Disc x ->         x.Noise
    | Annular_Disc x -> x.Disc.Noise
    | Sphere x ->       x.Noise
    | Cone x ->         x.Noise
    | TruncatedCone x ->x.Cone.Noise
    | Box x   ->        x.Noise
    | _     ->          failwith "Object type not accepted for Noise"


// Main Function - initial ray must be computed before
// This function only traces one ray, so it reques a superfunction to trace all the rays and probably do the parallel stuff
let min_FracOfRay = 1e-8
printfn "The minimum fraction of a ray traced is: %f" min_FracOfRay

let rec ForwardRay (ray:Ray,objs:Object[],material:System.Collections.Generic.IDictionary<string,Material>,level:int) = // MaxRays:int
    if level >= 1000 then
        // This will prevent stackOverFlow error in case there's a strange loop on reflection.
        failwith "Stack level deeper than 10000"
 
    // match intersection with Sensor or not Sensor and return Rays or end
    // intersecta
    let intersect:(Intersection*int) =  
        // find the closest intersection and if it doesn't exists, then return a non existent value

        let rei = intersection_all_forward(ray,objs)
        //printfn "the level is %d" level
        if  Array.isEmpty rei then ({ normal=UnitVector(1.,0.,0.); point=Point(0.,0.,0.); ray=ray; MatName="none" ; t= 1.<m>},-1)  
        else 
            rei 
            |> Array.minBy(fun x -> (fst x).t) 
            |> fun x ->   // new part for noise efficient computation
                
                let inter_sect = fst x 
                                 |> fun hit -> hit.ray  // define the ray
                                 |> fun hit_ray ->  let new_memory = {Direction=hit_ray.uvec ; GoingToID= snd x ;  // memory to add
                                                                      Origin_From=hit_ray.from ; Destination= (fst x).point }
                                                    {hit_ray with Memory = Array.append hit_ray.Memory [| new_memory |] }  // update the ray
                                 |> fun new_ray -> {(fst x) with ray = new_ray  }   // Update the new ray on the intersection
                (inter_sect, snd x)



          
    match snd intersect with
    | x when x >= 0 ->
        let sensorOrNot = IsItSensor(objs,snd intersect) // Is this intersected ibject a sensor?  - (snd intersect) is the object's number in the Array
        let noise = getNoise(objs,snd intersect)
        // do sensor or not
        match sensorOrNot with
        |(true, true) -> 
            // it's sensor and end
            // function with unit return
         
            UpdateSensorSelectionPhase(fst intersect,objs, snd intersect)
            
        |(true, false) ->   
            // sensor, but not end
            // 1st - Update Sensor

            UpdateSensorSelectionPhase(fst intersect,objs, snd intersect)

            // 2nd - Shading
            // filter the rays that have been dispersed more times that the maximum allowable
            let rays:Ray[] = ShadingForward( fst intersect, material,noise) |> Array.filter(fun x -> (x.NumBounces <= x.MaxDispersions && x.FracOfRay > min_FracOfRay))
            // check that the number of dispersive reflections is not more than the expected
            match Array.isEmpty rays with
            | false ->
               // match ray.NumBounces with
               //| n when n <= ray.MaxDispersions -> 
               //     // 3nd - Continue the ray tracing
               rays |> Array.iter(fun r -> ForwardRay (r,objs,material,level+1))
               // | _ -> () // end
            | true -> () // end it's absorbed
            //
            //      //              //
            //
            //
        |(_, _ ) ->  // (false , _ )
            //not end => just continue
            // 1st - Shading

            // filter the rays that have been dispersed more times that the maximum allowable
            let rays:Ray[] = ShadingForward( fst intersect, material,noise) |> Array.filter(fun x -> (x.NumBounces <= x.MaxDispersions && x.FracOfRay > min_FracOfRay))

            match Array.isEmpty rays with
            | false ->
                // check that the number of dispersive reflections is not more than the expected
                //match ray.NumBounces with
                //| n when n <= ray.MaxDispersions -> 
                //    // 2nd - continue ray tracing 
                rays |> Array.iter(fun r -> ForwardRay (r,objs,material,level+1))
                //| _ -> 
                //    () // end withouth producing anything
            | true ->  () // end it's absorbed

    | _ -> ()  // no intersection, nothing happens

let default_ray = { Wavelenght = WaveLength(1.064e-6<m>)
                    from= Point(infinity,infinity,infinity);
                    uvec = UnitVector(1.,0.,0.);
                    MaxLength =infi;
                    OpticalPathTravelled = 0.<m>;
                    NumBounces= -1.;
                    MaxDispersions = -1.;
                    NumOfParticlesCreated = 0;
                    FracOfRay = 1.;
                    IndexOfRefraction = 1.;
                    Memory = [||];
                    PhaseModulation = [||]
                    }
// type Intersection = { normal:UnitVector; point:Point; ray:Ray;MatName:string;  t:float<m>}
let default_intersection = {normal=UnitVector(1.,0.,0.); 
                            point= Point(infinity, infinity,infinity); 
                            ray=default_ray;
                            t=0.<m>; MatName=""}
let rec ForwardRay_noise (objs:Object[],material:System.Collections.Generic.IDictionary<string,Material>, route:route_ray[], old_modulation:float[]) = // MaxRays:int
    // this function will compute the path of the ray from the source to the Sensor and add the noise
    match route.Length with 
    | x when x >= 2 -> 
        //  not yet at sensor
        let origin = route.[0]
        let destination = route.[1]
        let ray_from = {default_ray with from = origin.Origin_From; uvec = origin.Direction; PhaseModulation= old_modulation}
        let ray_to ={default_ray with from = destination.Origin_From; uvec = destination.Direction; PhaseModulation= old_modulation}
        let normal_stocazzo = GetNormalAtPoint(objs.[origin.GoingToID],destination.Origin_From)
        let dotproduct1 = (normal_stocazzo*ray_from.uvec)//
        let dotproduct = dotproduct1*(normal_stocazzo*ray_to.uvec)
        match dotproduct with
        | y when y > 0. ->
            // if the product is bigger than 0, then the ray was transmitted -> No noise
            ForwardRay_noise(objs,material,route.[1..route.Length-1], old_modulation)
        
        | y when y < 0. ->
            // if the product is smaller than zero, the ray was reflected or dispersed
            let noise = getNoise(objs,origin.GoingToID)
            let new_modulation = PhaseModulation(ray_to,{default_intersection with ray=ray_from},noise) 
            ForwardRay_noise(objs,material,route.[1..route.Length-1], new_modulation)

        | y when y = 0. -> 
            printfn "WTH!? cosinus equal to zero??"
            ForwardRay_noise(objs,material,route.[1..route.Length-1], old_modulation)
        | _ -> failwith "Really happened? It shouldn't happen!"
            
    | x when x = 1 ->
        // then there is only the sensor missing
        old_modulation
        

    | _ ->   failwith "the number of 'route' cannot be lower of 1"
   


let RayTraceAll(rays:Ray[],objs,material) =
    // do the bucle of all the rays
    // This function requires that all the rays are previously computed

    rays |> Array.iter( fun x -> ForwardRay(x,objs,material,0))       