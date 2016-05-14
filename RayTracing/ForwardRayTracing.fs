module ForwardRayTracing
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

let UpdateSensor(ray:Ray,intersection:Intersection,obj:Object):Unit =
    let sc = SensorContent(intersection.point,ray.uvec,
                           ray.NumOfParticles, 
                           (ray.OpticalPathTravelled/(match (ray.Wavelenght) with WaveLength x ->x))%(float(match ray.Wavelenght with WaveLength x -> x)), 
                           ray.NoiseAdd ) // I need the intersection
    match obj with
    | Cylinder x ->   x.Sensor.AddData(sc)
    | SurfaceLens x-> x.Sensor.AddData(sc)
    | Disc x-> x.Sensor.AddData(sc)

    
// No sensor options
// Sensor options
let UpdateSensorSelection(ray:Ray, intersection:Intersection, objs:Object[],objID:int) =
    UpdateSensor(ray,intersection,objs.[objID])
  
// Algorithm for the Ray tracing


// match sensor or not
let IsItSensor(objs:Object[], id:int):bool*bool =
    match objs.[id] with
    | Cylinder x -> (x.Sensor.Exists, x.Sensor.Terminate) // says if it is a sensor or not and if it terminates
    | SurfaceLens x -> (x.Sensor.Exists, x.Sensor.Terminate) // says if it is a sensor or not and if it terminates
    | Disc x -> (x.Sensor.Exists, x.Sensor.Terminate)

let getNoise(objs:Object[], id:int) =
    match objs.[id] with
    | Cylinder x -> x.Noise
    | SurfaceLens x -> x.Noise
    | Disc x -> x.Noise

// Main Function - initial ray must be computed before
// This function only traces one ray, so it reques a superfunction to trace all the rays and probably do the parallel stuff
let rec ForwardRay (ray:Ray,objs:Object[],material:System.Collections.Generic.IDictionary<string,Material>) = // MaxRays:int
   
    // match intersection with Sensor or not Sensor and return Rays or end
    // intersecta
    let intersect:(Intersection*int) =  
        // find the closest intersection and if it doesn't exists, then return an non existent value
        let rei = intersection_all_forward(ray,objs)
        if  Array.isEmpty rei then ({ normal=UnitVector(1.,0.,0.); point=Point(0.,0.,0.); ray=ray;MatName="none" ; t= 1.<m>},-1)  
        else rei |> Array.minBy(fun x -> (fst x).t)
    match snd intersect with
    | x when x >= 0 ->
        let sensorOrNot = IsItSensor(objs,snd intersect) // Is this intersected ibject a sensor?  - (snd intersect) is the number of object that is a sensor
        let noise = getNoise(objs,snd intersect)
        // do sensor or not
        match sensorOrNot with
        |(true, true) -> 
            // it's sensor and end
            // function with unit return
            UpdateSensorSelection(ray,fst intersect,objs, snd intersect)
            
        |(true, false) ->   
            // sensor, but not end
            // 1st - Update Sensor
            UpdateSensorSelection(ray,fst intersect,objs, snd intersect)

            // 2nd - Shading
            let rays:Ray[] = ShadingForward(ray, fst intersect, material,noise)
            // check that the number of dispersive reflections is not more than the expected
            match ray.NumBounces with
            | n when n <= ray.MaxDispersions -> 
                // 3nd - Continue the ray tracing
                rays |> Array.iter(fun r -> ForwardRay (r,objs,material))
            | _ -> 'e' |> ignore// end
            
        |(_, _ ) ->  // (false , _ )
            //not end - just continue
            // 1st - Shading
            let rays:Ray[] = ShadingForward(ray, fst intersect, material,noise)
        
            // check that the number of dispersive reflections is not more than the expected
            match ray.NumBounces with
            | n when n <= ray.MaxDispersions -> 
                // 2nd - continue ray tracing 
                rays |> Array.iter(fun r -> ForwardRay (r,objs,material))
            | _ -> 'e' |> ignore// end withouth producing anything

    | _ -> 'e' |> ignore // no intersection, nothing happens

(*
let RayTraceAll(nRays:int,objs,material) =
    // do the bucle of all the rays
    let mutable n = 0
    // In order to define the ray, I must know the source that generates the rays

    while n<nRays do 
        let ray:Ray = ()
        ForwardRay(ray,objs,material)
        n <- n+1
        *)