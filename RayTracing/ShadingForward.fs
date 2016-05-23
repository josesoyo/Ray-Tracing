module ShadingForward

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Types.Algebra
open Types
open Types.types
open Types.ObjectTypes
open RayTracing.RayStructureIntersection
open Random

// define some types of functions before using the functions
type ShadingForward = (Ray*Intersection*System.Collections.Generic.IDictionary<string,Material>) -> Ray[]   // the summ of all the next options
type reflection = (Ray*Intersection*int) -> Ray         // Rays reflected
type transmision = (Ray*Intersection*float*int) -> Ray        // Rays transmitted
type dispersion = (Ray*Intersection*System.Collections.Generic.IDictionary<string,Material>) -> Ray[]       // ppm/2pi
type absortion = Unit -> Unit                                                                               // Shouldn't exist - Just to remember it's an effect

let dispersion(intersection:Intersection,numOfParticles:int) =
    //compute the dispersed ray: random direction on an hemisphere
    let ray = intersection.ray
    let normal = intersection.normal
    let rotmat = Matrix.RotateVector(UnitVector(0.,0.,1.), normal)          // from to
    let nuvec = rotmat.RotateVector(Vector(SampUnitHemisphereToCart()))    // New direction for the ray
    {ray with 
        uvec = nuvec.ToUnitVector(); from= intersection.point;
        NumBounces = ray.NumBounces + 1uy;
        OpticalPathTravelled = ray.OpticalPathTravelled - (match ray.Wavelenght with WaveLength x -> x)/2. 
        NumOfParticles=numOfParticles}

let reflection(intersection:Intersection,numOfParticles:int) = 
    // perform the reflection
    //It returns the reflected ray
    //
    let ray = intersection.ray
    let pnormal = intersection.normal
    let lightDir = ray.uvec // Direction of the ray
    let normal = 
        if lightDir*(pnormal)< 0. then  pnormal
        else  pnormal.Negate()
    
    let Reflvect = 
            (-2.0*normal*(lightDir))*normal+lightDir //Inverted - Reflected ray
   
    let newvect = Reflvect.ToUnitVector()
    
    // return the new ray - the one from the intersection modified with the extra pathLengt
    {ray with 
        uvec = newvect; from = intersection.point;              // update the point in which the ray comes from
        OpticalPathTravelled = ray.OpticalPathTravelled  + (match ray.Wavelenght with WaveLength x -> x)/2.; // only the half wavelength because is the ray from intersection
        NumOfParticles=numOfParticles}
    //  end of Reflection
 
let transmission(intersection:Intersection, ior:float, numOfParticles:int) =
    //
    let ray = intersection.ray
    let RayDir = ray.uvec
    let LightDir = RayDir.Negate() //Ray that incides on the surface * -1
    let SideRay (rayIndex,index, ci, normal:UnitVector) =   
        // Changes the situation checking from air or to  
       //if rayIndex = index then 
       if ci < 0. then 
        // the ray is inside the material
        (-ci, 1./index, normal.Negate()) 
       else
        (ci, index, normal)
    //let n = ior // With AIR
    let ci = intersection.normal*(LightDir) //Cosinus incident angle
 
    let (cos_inc,nu,vnormal) = SideRay(ray.IndexOfRefraction,ior ,ci, intersection.normal)
    let inv_n = 1./nu // It is used the inverse = (n_from/n_to)
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
               NumOfParticles=numOfParticles}
        | _ ->
            {ray with               // ray from intersection there's no opticalpathlength extra, already included
               uvec = newvect; from = intersection.point; IndexOfRefraction = ior
               NumOfParticles=numOfParticles}
    else  reflection ({intersection with 
                            ray = {ray with 
                                     OpticalPathTravelled = ray.OpticalPathTravelled - (match ray.Wavelenght with WaveLength x -> x)/2. }
                        } ,numOfParticles
                       
                       ) // Else reflected  = TIR -> reduced the OPtravelled by wavelength/2   


let RayProbabilityes(material:System.Collections.Generic.IDictionary<string,Material>,intersection:Intersection) =
    // function created because it must be more complicated in the future
    material.[intersection.MatName].T, material.[intersection.MatName].R, material.[intersection.MatName].LambPPM

let SingleFreqNoiseAdd(ray:Ray,inter:Intersection,ns:noise[]) =
    // Spectral density  -> Noise
    // adds the noise considering the direction of the ray and the normal
    // but the mean value
    // it's a kind of isotropic
    let diff = (inter.normal-ray.uvec)
    let norm = sqrt(diff*diff)
    let freq, nois= ns.[0]
    freq, (2.*PI/sqrt(3.)/(match ray.Wavelenght with WaveLength x ->float x))*norm*nois

// Functions
let ShadingForward(intersection:Intersection,material:System.Collections.Generic.IDictionary<string,Material>, noise:noise[]):(Ray[]) =
    // Create the shading of the ray tracing. This Function must be modified many times
    match intersection.ray.NumOfParticles with
    | n when n = 0 ->
        // Case when I'm doing single raytracing
        
        let pt, pr , pd = RayProbabilityes(material, intersection)
        let coin = rnd.NextDouble()     // coin because it will be used for the selection
        // options: transmission, reflection or dispersion. Absortion is done implicitly
        match coin with 
        | c when c <= pt -> 
            let out = {transmission(intersection,fst material.[intersection.MatName].n,0) with NoiseAdd = (Array.append intersection.ray.NoiseAdd [|SingleFreqNoiseAdd(intersection.ray,intersection,noise)|] )}
            [|out|]
        | c when pt < c && c <= (pt+pr) -> 
            let out =  {reflection(intersection,0) with NoiseAdd = (Array.append intersection.ray.NoiseAdd [|SingleFreqNoiseAdd(intersection.ray,intersection,noise)|])}
            [| out |]
        | c when pt+pr < c && c <= pt+pr+pd ->
            let out ={dispersion(intersection,0) with NoiseAdd = (Array.append intersection.ray.NoiseAdd [|SingleFreqNoiseAdd(intersection.ray,intersection,noise)|])}  // to modify, dispersion
            [| out |]
        | _ ->
            [||]

    | _ -> [|intersection.ray|]  //| n when n > 0 ->    // case there are many particles, it 
(*//| _ ->      // No possible = Absortion
  // if there's a lambertian material should be considered the n+1 on reflections
*)            


