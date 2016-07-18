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
let UpdateSensorPhase(ray:Ray,intersection:Intersection,obj:Object):Unit =
    let sc = SensorContent(intersection.point,ray.uvec,
                           ray.NumOfParticles, 
                           (ray.OpticalPathTravelled/(match (ray.Wavelenght) with WaveLength x ->x))%(float(match ray.Wavelenght with WaveLength x -> x)), 
                           ray.PhaseModulation ) // I need the intersection
    match obj with
    | Cylinder x ->     x.Sensor.AddData(sc)
    | SurfaceLens x->   x.Sensor.AddData(sc)
    | Disc x->          x.Sensor.AddData(sc)
    | Cone x ->         x.Sensor.AddData(sc)
    | Sphere x->        x.Sensor.AddData(sc)
    | Annular_Disc x -> x.Disc.Sensor.AddData(sc)

let UpdateSensorSelectionPhase(ray:Ray, intersection:Intersection, objs:Object[],objID:int) =
    UpdateSensorPhase(ray,intersection,objs.[objID])
  
// Algorithm for the Ray tracing


// match sensor or not
let IsItSensor(objs:Object[], id:int):bool*bool =
    match objs.[id] with
    | Cylinder x ->     (x.Sensor.Exists, x.Sensor.Terminate) // says if it is a sensor or not and if it terminates
    | SurfaceLens x ->  (x.Sensor.Exists, x.Sensor.Terminate) // says if it is a sensor or not and if it terminates
    | Disc x ->         (x.Sensor.Exists, x.Sensor.Terminate)
    | Annular_Disc x -> (x.Disc.Sensor.Exists, x.Disc.Sensor.Terminate)
    | Sphere x ->       (x.Sensor.Exists, x.Sensor.Terminate)
    | Cone x ->         (x.Sensor.Exists, x.Sensor.Terminate)
    | TruncatedCone x ->(x.Cone.Sensor.Exists, x.Cone.Sensor.Terminate)

let getNoise(objs:Object[], id:int) =
    match objs.[id] with
    | Cylinder x ->     x.Noise
    | SurfaceLens x ->  x.Noise
    | Disc x ->         x.Noise
    | Annular_Disc x -> x.Disc.Noise
    | Sphere x ->       x.Noise
    | Cone x ->         x.Noise
    | TruncatedCone x ->x.Cone.Noise

// Main Function - initial ray must be computed before
// This function only traces one ray, so it reques a superfunction to trace all the rays and probably do the parallel stuff
let rec ForwardRay (ray:Ray,objs:Object[],material:System.Collections.Generic.IDictionary<string,Material>) = // MaxRays:int
   
    // match intersection with Sensor or not Sensor and return Rays or end
    // intersecta
    let intersect:(Intersection*int) =  
        // find the closest intersection and if it doesn't exists, then return a non existent value
        let rei = intersection_all_forward(ray,objs)
        if  Array.isEmpty rei then ({ normal=UnitVector(1.,0.,0.); point=Point(0.,0.,0.); ray=ray;MatName="none" ; t= 1.<m>},-1)  
        else 
            rei 
            |> Array.minBy(fun x -> (fst x).t) 
          
    match snd intersect with
    | x when x >= 0 ->
        let sensorOrNot = IsItSensor(objs,snd intersect) // Is this intersected ibject a sensor?  - (snd intersect) is the number of object that is a sensor
        let noise = getNoise(objs,snd intersect)
        // do sensor or not
        match sensorOrNot with
        |(true, true) -> 
            // it's sensor and end
            // function with unit return
            //match ray.NumOfParticles with
            //| 1 ->
            UpdateSensorSelectionPhase(ray,fst intersect,objs, snd intersect)
            //printfn "%+A" ray.PhaseModulation
            //| _ ->
            //   UpdateSensorSelection(ray,fst intersect,objs, snd intersect)
            
        |(true, false) ->   
            // sensor, but not end
            // 1st - Update Sensor
            //match ray.NumOfParticles with
            //| 1 ->
            UpdateSensorSelectionPhase(ray,fst intersect,objs, snd intersect)
            //    printfn "%+A" ray.PhaseModulation
            //| _ ->
            //    UpdateSensorSelection(ray,fst intersect,objs, snd intersect)
            
            //
            //          //  //  only for the case of the arm cavity //  //
            //          This should avoid a stackoverflow problem
            let xavant = (fst intersect).point.X - (fst intersect).ray.from.X  // quant ha avancat
            let boolsArm = (3.< (fst intersect).point.X && (fst intersect).point.X < 1900.) && (xavant < 1.)
            //
            //
            match boolsArm with 
            | true ->  // if it doesn't advance, then it shouldn't arrive
                'e' |> ignore
            | false ->
                // 2nd - Shading
                // filter the rays that have been dispersed more times that the maximum allowable
                let rays:Ray[] = ShadingForward( fst intersect, material,noise) |> Array.filter(fun x -> x.NumBounces <= x.MaxDispersions)
                // check that the number of dispersive reflections is not more than the expected
                match Array.isEmpty rays with
                | false ->
                    match ray.NumBounces with
                    | n when n <= ray.MaxDispersions -> 
                        // 3nd - Continue the ray tracing
                        rays |> Array.iter(fun r -> ForwardRay (r,objs,material))
                    | _ -> 'e' |> ignore// end
                | true -> 'e' |> ignore// end it's absorbed
            //
            //      //              //
            //
            //
        |(_, _ ) ->  // (false , _ )
            //not end => just continue
            // 1st - Shading
            // filter the rays that have been dispersed more times that the maximum allowable
            //
            //          //  //  only for the case of the arm cavity //  //
            //          This should avoid a stackoverflow problem
            let xavant = (fst intersect).point.X - (fst intersect).ray.from.X  // quant ha avancat

            let boolsArm = (3.< (fst intersect).point.X && (fst intersect).point.X < 1900.) && (xavant < 1.)
            //
  
            match boolsArm with 
            | true ->  // if it doesn't advance, then it shouldn't arrive
                'e' |> ignore
            | false ->

                let rays:Ray[] = ShadingForward( fst intersect, material,noise) |> Array.filter(fun x -> x.NumBounces <= x.MaxDispersions)
                match Array.isEmpty rays with
                | false ->
                    // check that the number of dispersive reflections is not more than the expected
                    match ray.NumBounces with
                    | n when n <= ray.MaxDispersions -> 
                        // 2nd - continue ray tracing 
                        rays |> Array.iter(fun r -> ForwardRay (r,objs,material))
                    | _ -> 
                        'e' |> ignore// end withouth producing anything
                | true ->  'e' |> ignore// end it's absorbed

    | _ -> 'e' |> ignore // no intersection, nothing happens


let RayTraceAll(rays:Ray[],objs,material) =
    // do the bucle of all the rays
    // This function requires that the rays are introduced

    rays |> Array.iter( fun x -> ForwardRay(x,objs,material))       