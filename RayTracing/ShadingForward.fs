module ShadingForward

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Types.Algebra
open Types
open Types.types
open Types.ObjectTypes
open RayTracing.RayStructureIntersection
open Random
open ShadingNoise

// define some types of functions before using the functions
type ShadingForward = (Ray*Intersection*System.Collections.Generic.IDictionary<string,Material>) -> Ray[]   // the summ of all the next options
type reflection = (Ray*Intersection*int) -> Ray         // Rays reflected
type transmision = (Ray*Intersection*float*int) -> Ray        // Rays transmitted
type dispersion = (Ray*Intersection*System.Collections.Generic.IDictionary<string,Material>) -> Ray[]       // ppm/2pi
type absortion = Unit -> Unit                                                                               // Shouldn't exist - Just to remember it's an effect

let dispersion(intersection:Intersection,fracOfRay:float, cos_inc:float) =
    // Disperse light: 
    // fracOfRay is the fraction of the original ray that will be dispersed. Each dispersed ray contains the respective fraction
    //compute the dispersed ray: random direction on an hemisphere
    let ray = {intersection.ray with  
                from= intersection.point; NumBounces = intersection.ray.NumBounces + 1.;//1uy;
                 OpticalPathTravelled = intersection.ray.OpticalPathTravelled - (match intersection.ray.Wavelenght with WaveLength x -> x)/2. 
                 }

    let rotmat = 
        if cos_inc < 0. then    // intersection.normal*intersection.ray.uvec
            Matrix.RotateVector(UnitVector(0.,0.,1.), intersection.normal)          // from to
        else Matrix.RotateVector(UnitVector(0.,0.,1.), intersection.normal.Negate()) 

    let nuvec = [|1..intersection.ray.NumOfParticlesCreated|]       // creates one new vector for each particle created on dispersion
                |> Array.map(fun x -> UnitVector(SampUnitHemisphereToCart() ))//SampUnitHemiCosToCart()) ) 
                |> Array.map (fun uv -> rotmat.RotateVector(uv))    // New direction for the ray
    nuvec
    |> Array.map(fun uv -> {ray with uvec = uv; FracOfRay=fracOfRay/float(intersection.ray.NumOfParticlesCreated)})

        

let reflection(intersection:Intersection,fracOfRay:float, cos_inc:float) = 
    // perform the reflection
    //It returns the reflected ray
    //
    let ray = intersection.ray
    //let pnormal = intersection.normal
    let lightDir = ray.uvec // Direction of the ray
    let normal = intersection.normal
    //    if cos_inc < 0. then  intersection.normal//pnormal // cos_inc = lightDir*(pnormal)
    //    else  intersection.normal.Negate() //pnormal.Negate()
    
    let Reflvect = 
            (-2.0*cos_inc)*normal+lightDir //Inverted - Reflected ray  normal*(lightDir)
   
    let newvect = Reflvect.ToUnitVector()
    
    // return the new ray - the one from the intersection modified with the extra pathLengt
    {ray with 
        uvec = newvect; from = intersection.point;  NumBounces = intersection.ray.NumBounces + 0.0025;          // update the point in which the ray comes from + 400 reflections = 1 dispersion
        OpticalPathTravelled = ray.OpticalPathTravelled  + (match ray.Wavelenght with WaveLength x -> x)/2.; // only the half wavelength because is the ray from intersection
        FracOfRay=fracOfRay}
    //  end of Reflection
 
let transmission(intersection:Intersection, ior:float, fracOfRay:float, cos_inc0:float) =
    //
    let ray = intersection.ray
    let RayDir = ray.uvec
    (*
    //let LightDir = RayDir.Negate() //Ray that incides on the surface * -1
    let SideRay (rayIndex,index, ci, normal:UnitVector) =   
        // Changes the situation checking from air or to  
       //if rayIndex = index then 
       if ci < 0. then      // normals opposites: INSIDE
        // the ray is inside the material
        (-ci, 1./index, normal.Negate()) 
       else                 // OUTSIDE
        (ci, index, normal)
    //let n = ior // With AIR
    *)
    let cos_inc,vnormal = 
        if cos_inc0 < 0. then
            -cos_inc0  ,intersection.normal//  Normals always same side of Ray that comes, so no need to match ci
        else 
           cos_inc0  ,intersection.normal.Negate()
    let nu = //SideRay(ray.IndexOfRefraction,ior ,ci, intersection.normal)
        if (ray.IndexOfRefraction) = ior then
            // inside
             1./ior 
        else
            // outside
             ior 
    let AngCritic n_transm =
        // Obtain Critical angle for TIR
        if n_transm > 1. then
            1.571
        else
            let tir = asin(n_transm) // Pi/2  
            tir
    let ang_critic = AngCritic nu
    let ang_inc = acos(cos_inc)
    //
    if ang_inc < ang_critic then // TIR
        let inv_n = 1./nu // It is used the inverse = (n_from/n_to)
        let cos_trans = sqrt(1.-(inv_n*inv_n )*(1.-cos_inc*cos_inc)) // Cosinus transmited
        let vtrans = (inv_n)*RayDir + (-1.)*(cos_trans - inv_n*cos_inc)*vnormal
        let newvect = vtrans.ToUnitVector()

        // I must consider that if the ray enters inside the material, the IOR of the ray must be changed following the next rule:
        //      If ray.IOR <> material.IOR  => change the ray.IOR to the one of the material
        //      if ray.IOR = material.IOR => IT's leaving the material, and thus going to air
        match ray.IndexOfRefraction with
        | x when x = ior ->    // leaving the material 
            {ray with               // ray from intersection there's no opticalpathlength extra, already included
               uvec = newvect; from = intersection.point; IndexOfRefraction = 1.
               FracOfRay=fracOfRay}
        | _ ->
            {ray with               // ray from intersection there's no opticalpathlength extra, already included
               uvec = newvect; from = intersection.point; IndexOfRefraction = ior
               FracOfRay=fracOfRay}
    else  reflection ({intersection with 
                            ray = {ray with 
                                     OpticalPathTravelled = ray.OpticalPathTravelled - (match ray.Wavelenght with WaveLength x -> x)/2.;
                                     // the +0.1 on NumBounces is because some TIR can lead to a bucle
                                     NumBounces = intersection.ray.NumBounces + 0.1 }
                        } ,fracOfRay, cos_inc
                       ) // Else reflected  = TIR -> reduced the OPtravelled by wavelength/2   
                       


let RayProbabilityes(material:System.Collections.Generic.IDictionary<string,Material>,intersection:Intersection, cos_inc:float) =
    // Select from the material which one is the reflectivity, transmitivity and dispersion
    // It is considered that the software works with FIELD, so we require the square root of the probability

    let nmatname = RealMatName intersection.MatName cos_inc                     // case we are working with a material whose properties depends on the AOI
    

    //printfn "the material is: %s" nmatname 
    // Decision to work with Field amplitude, not power
    sqrt(material.[nmatname].T), sqrt(material.[nmatname].R), sqrt(material.[nmatname].LambPPM)
    

// Functions
let ShadingForward(intersection:Intersection,material:System.Collections.Generic.IDictionary<string,Material>, noise:noise):(Ray[]) =
    // Create the shading of the ray tracing. This Function must be modified many times
    let cos_inc_direct = intersection.normal*intersection.ray.uvec   // I set the cosinus considering that the ray direction is on the direction of the intersection
    match intersection.ray.FracOfRay with
    // just in case!
    | n when n <= 0. -> 
        //[||]  // not possible - Delete or failwith
        failwithf "Not possible to have a fraction of a ray lower than zero\nThe current ray is:\n%+A" intersection.ray
    (*
    | n when n = 1 ->
       // Case when raytracing of a single ray
        
        let pt, pr , pd = RayProbabilityes(material, intersection, cos_inc_direct)
        let coin = rnd.NextDouble()     // coin because it will be used for the selection
        // options: transmission, reflection or dispersion. Absortion is done implicitly
        match coin with 
        | c when c <= pt -> 
            let index_of_refraction = fst material.[intersection.MatName].n
            let out = transmission(intersection, index_of_refraction, 1, cos_inc_direct) 
            [|{out with PhaseModulation = PhaseModulation(out, intersection,noise) }|]
            //[|out|]
        | c when pt < c && c <= (pt+pr) -> 
 
            let out =  reflection(intersection,1, cos_inc_direct)
            [|{out with PhaseModulation = PhaseModulation(out, intersection,noise) }|]
            //[| out |]
         | c when pt+pr < c && c <= pt+pr+pd ->
            let out =dispersion(intersection,1,cos_inc_direct) // to modify, dispersion
            //[|{out with PhaseModulation = PhaseModulation(out, intersection,noise) }|]
            out |> Array.map(fun ra -> {ra with PhaseModulation = PhaseModulation(ra, intersection,noise) })
            //[| out |]
        | _ ->
            [||]
    *)
    | _ ->
        // Many particles case. The option that is going to be chosed must be explained
        let pt, pr , pd = RayProbabilityes(material, intersection, cos_inc_direct)
        let nt, nr , nd =
            let fpart = (intersection.ray.FracOfRay)  // float of the fraction of original ray
            (*
            if (1.-pt - pr - pd) < 1e-10 then // No absortion: I don't trust they match perfectly *)
                // No absortion
            ((fpart*pt),(fpart*pr), pd*fpart )
            (*
            else
                // absortion of the material
                let r, t = (fpart*pr) , (fpart*pt)
                let d = (fpart*pd) 
                let a = (fpart*(1.-pt-pr-pd))
                let missing = (fpart-r-d-a-t) // in case the sum of the parts is not giving the total
                if  missing = 0. then
                    // no missing "particles"
                    (r, t, d)
                else if missing > 0 then
                    // missing particles
                    let probMax = [|('T',pt);('R',pr);('D',pd);('A', 1.-pt-pr-pd)|] |> Array.maxBy(fun x -> snd x) // case the sum of all parts < total
                    match fst probMax with
                    // Add the missing particle to the most probable choice Transmitted, Reflected, Dispersed, Absorved
                    | 'T' -> (r,t+missing,d)
                    | 'R' -> (r+missing,t,d)
                    | 'D' -> (r,t,d+missing)
                    | 'A' -> (r,t,d)
                    | _ ->   failwith "Impossible error on ShadingForward??"
                            
                else // ERROR control
                    printfn "Theres an error on ShadingForward"
                    System.Console.ReadKey() |> ignore // stop the computation
                    (r,t, d )

                *)

        let tout = match nt with
                   | nt when nt > 0. -> 

                        let index_of_refraction = fst material.[intersection.MatName].n
                        transmission(intersection, index_of_refraction, nt,cos_inc_direct) 
                                       |> fun x ->  [|{x with PhaseModulation = PhaseModulation(x, intersection,noise) }|]
                   | _ -> [||]

        let rout = match nr with
                   | nr when nr > 0. ->
                                reflection(intersection,nr, cos_inc_direct) 
                                |> fun x -> [|{x with PhaseModulation = PhaseModulation(x, intersection,noise) }|]
                   | _ -> [||]

        let dout = match nd with
                   | nd when nd > 0. -> dispersion(intersection,nd, cos_inc_direct) 
                                       |> Array.map( fun x -> {x with PhaseModulation =PhaseModulation(x, intersection,noise) })// to modify, dispersion
                   | _ -> [||]
          
        Array.concat [|tout; rout ;dout|] // concatenate the options
   
