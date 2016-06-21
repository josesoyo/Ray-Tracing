module ShadingNoise

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Types.Algebra
open Types
open Types.types
open Types.ObjectTypes
open RayTracing.RayStructureIntersection
open Random // Here I defined PI

let SingleFreqNoiseAdd(ray:Ray,inter:Intersection,ns:noise) =
    // Spectral density  -> Noise
    // adds the noise considering the direction of the ray and the normal
    // but the mean value
    // it's a kind of isotropic
    let diff = (inter.normal-ray.uvec)
    let norm = sqrt(diff*diff)
    let freq, nois, _ = (fst ns).[0]
    printfn "SingleFreqNoiseAdd Badly defined"
    (2.*PI/sqrt(3.)/(match ray.Wavelenght with WaveLength x ->float x))*norm//*nois // also was the freq
      
let sinMod t (difRay:Vector) (fA:(byte*Vector*float)[]) =
    // function to compute the globlal modulation (sum of all)
    let duepi = 2.*PI
        
    Array.fold(fun acc x -> let freq, amplitude, phase = fA.[x]
                            acc+sin(duepi*float(freq)*t+phase)*(amplitude*difRay)) 0. [|0..fA.Length-1|] 
        
let PhaseModulation(shadedRay:Ray,inter:Intersection,ns:noise) =
    // add phase modulation to a ray
    let ray = inter.ray
    let sraydir = shadedRay.uvec
    let difRay = sraydir - (ray.uvec)
    //let freq, amplitude = (fst ns).[0]
    let freqAndAmplitude = (fst ns)  // [|f,A,phase|]
    let timeStamps = snd ns
    let duepi = 2.*PI

    let SINMOD t = sinMod t difRay freqAndAmplitude
    timeStamps 
    |> Array.map(fun t -> ((duepi/(match ray.Wavelenght with WaveLength x -> float x))*SINMOD t)
                         //((duepi/(match ray.Wavelenght with WaveLength x -> float x))*sin(duepi*float(freq)*t)*(amplitude*difRay)) //%PI  //freq
                 ) // Phase modulation
    


