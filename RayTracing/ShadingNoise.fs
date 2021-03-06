﻿module ShadingNoise

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Types.Algebra
open Types
open Types.types
open Types.ObjectTypes
open RayTracing.RayStructureIntersection
open Random // Here I defined PI
open System
open System.Threading.Tasks
//
// for OpenCL case
open FSCL.Compiler
open FSCL.Language
open FSCL.Runtime
//
//
let SingleFreqNoiseAdd(ray:Ray,inter:Intersection, ns:((float*Vector*float)[]*float[])) =  // 
    // Spectral density  -> Noise
    // adds the noise considering the direction of the ray and the normal
    // but the mean value
    // it's a kind of isotropic
    let diff = (inter.ray.uvec-ray.uvec)
    let norm = sqrt(diff*diff)
    let freq, nois, _ = (fst ns).[0]
    printfn "SingleFreqNoiseAdd Badly defined"
    (2.*PI/sqrt(3.)/(match ray.Wavelenght with WaveLength x ->float x))*norm//*nois // also was the freq
      
let duepi = 2.*PI
let sinMod t (difRay:Vector) (fA:(float*Vector*float)[]) =
    // function to compute the globlal modulation (sum of all)
    //let duepi = 2.*PI
        
    Array.fold(fun acc x -> let freq, amplitude, phase = fA.[x]
                            acc+sin(duepi*freq*t+phase)*(amplitude*difRay)) 0. [|0..fA.Length-1|] 
        

let PhaseModulation(shadedRay:Ray,inter:Intersection,nsa:noise) =
    match nsa with 
    | Quiet -> shadedRay.PhaseModulation
    | Simulated ns ->  // here it is the old code for simulated data
        let freqAndAmplitude = (fst ns)  // [|freq, A, phase|]
        (*match (Array.isEmpty freqAndAmplitude) with
        // it the object is not oscillating, there's nothing to compute  -> Added in None option
        | true -> shadedRay.PhaseModulation
        | false ->    *)
        // add phase modulation to a ray
        let ray = inter.ray
        let sraydir = shadedRay.uvec
        let difRay = sraydir - (ray.uvec)
        //let freq, amplitude = (fst ns).[0]
        let timeStamps = snd ns 
 
        let SINMOD t = sinMod t difRay freqAndAmplitude
        let ww = (duepi/(match ray.Wavelenght with WaveLength x -> float x))
        match (Array.isEmpty shadedRay.PhaseModulation) with
        | true ->
            // if it's empty, there isn't modulation   -> first interaction with an oscillating element
            timeStamps 
            |> Array.Parallel.map(fun t -> (ww*SINMOD t)
                            ) // Phase modulation
                                
        | false ->
            // if it's not empty, then the modulation must be summed to the one that the ray already contains
            let prevMod =  shadedRay.PhaseModulation
            //(prevMod,timeStamps)
            //||> Array.map2(fun old t -> old+(ww*SINMOD t)
            //               ) // Phase modulation
            // Parallel.For method, actyally is like using [|0..timeStamps.Len-1|] |> Array.Parallel.map(fun  -> (prevMod.[i]+(ww*SINMOD timeStamps.[i])))
            //Parallel.For(0, (prevMod.Length), fun i -> (prevMod.[i] <- prevMod.[i]+(ww*SINMOD timeStamps.[i])) ) |> ignore
            [|0..timeStamps.Length-1|] |> Array.Parallel.map(fun i -> (prevMod.[i]+(ww*SINMOD timeStamps.[i])))
            // return the output updated on the same array. 
            //prevMod
            // Might create problems if it wasn't because I am redefining the modulation on ShadingForward

    | RealData rdsp ->   // new code with the real data
        //   If there is real data it will never be empty, by default empty noise is Simulated
        let dir, _ ,disp = rdsp
        let ray = inter.ray
        let sraydir = shadedRay.uvec
        let difRay = sraydir - (ray.uvec)
        
        //let freq, amplitude = (fst ns).[0]
        //let timeStamps = snd ns 
        //let duepi = 2.*PI
        let ww = (duepi/(match ray.Wavelenght with WaveLength x -> float x))//*(dir*difRay)

        match (Array.isEmpty shadedRay.PhaseModulation) with
        | true ->
            
            [|0..disp.[0].Length-1|]         // number of measures
            |> Array.Parallel.map(fun i -> 
                ww*(Array.fold(fun acc j ->  let multipli = ((dir.[j])*difRay)
                                             disp.[j].[i]*multipli) 0. [|0..disp.Length-1|] )
                )
        | false ->
            let prevMod =  shadedRay.PhaseModulation

            [|0..disp.[0].Length-1|]         // number of measures
            |> Array.Parallel.map(fun i -> 
                prevMod.[i]+ww*(Array.fold(fun acc j ->  //let nn = ww*disp.[j].[i]
                                                         disp.[j].[i]*((dir.[j])*difRay)) 0. [|0..disp.Length-1|] )
                )

(**

OpenCL case with FSCL


[<ConstantDefine>] 
let PII0 = 
    float32(PI)

[<ConstantDefine>] 
let pii = 
    PII0

[<ConstantDefine>]
let pi2 =
    2.f*pii

[<ConstantDefine>]
let pi_half =
    pii/2.f


[<ReflectedDefinition;Kernel>]
let modulate ((oldMod:float32[]), (timeStamp:float32[]), (diffRayXAmp:float32[]), (freqs:float32[]), (phases:float32[]), (numwaves_inv:float32), (wi:WorkItemInfo)) =
    // I just want to sum the elements of sarr on each larr cell

    let gid = wi.GlobalID(0)    // based on the length of lArr
    let outArr = Array.zeroCreate<float32> oldMod.Length
    let mutable acc = oldMod.[gid]

    for i in 0..freqs.Length-1 do
        acc <- numwaves_inv*diffRayXAmp.[i]*cos(pi2*freqs.[i]*timeStamp.[gid]+phases.[i]-pi_half)+acc

    outArr.[gid] <- acc
    outArr



    
let PhaseModulation_(shadedRay:Ray,inter:Intersection,ns:noise) =
    let freqAndAmplitude = (fst ns)  // [|freq, A, phase|]
    match (Array.isEmpty freqAndAmplitude) with
    // it the object is not oscillating, there's nothing to compute
    | true -> shadedRay.PhaseModulation
    | false ->
        // add phase modulation to a ray
        let ray = inter.ray
        let sraydir = shadedRay.uvec
        let difRay = sraydir - (ray.uvec)
        //let freq, amplitude = (fst ns).[0]
        let timeStamps = snd ns
        let duepi = 2.*PI

        let diffRayXAmp = freqAndAmplitude 
                          |> Array.map(fun x -> 
                             let _,am,_ = x 
                             float32 (difRay*am)  
                             )
        let nfa =  freqAndAmplitude   
                   |> Array.map(fun x -> 
                        let f,_,p = x
                        (f,p)
                        )         
        let numWaves_inv = float32(duepi/(match ray.Wavelenght with WaveLength x -> float x))
 
        let prevMod =
            if (Array.isEmpty shadedRay.PhaseModulation) then
                Array.zeroCreate<float32> timeStamps.Length
 
            else
                shadedRay.PhaseModulation
 
        let freqs = nfa |> Array.map(fun x -> fst x)
        let phases = nfa |> Array.map(fun x -> snd x)

        let ws = new WorkSize(int64(timeStamps.Length))  // (GlobalSize, [LocalSize, globalOffset]) -> can be int64 or int64[]
        let outOCL = <@modulate(prevMod,timeStamps,diffRayXAmp,freqs,phases,numWaves_inv,ws)@>.Run()
            
        outOCL
*)       


