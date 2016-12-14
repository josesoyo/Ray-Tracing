
#r @"../packages/FSCL.Compiler.2.0.1/lib/net45/FSCL.Compiler.Core.dll"
#r @"../packages/FSCL.Compiler.2.0.1/lib/net45/FSCL.Compiler.dll"
#r @"../packages/FSCL.Compiler.2.0.1/lib/net45/FSCL.Compiler.Language.dll"
#r @"../packages/FSCL.Compiler.2.0.1/lib/net45/FSCL.Compiler.NativeComponents.dll"
#r @"../packages/FSCL.Compiler.2.0.1/lib/net45/FSCL.Compiler.Util.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/FSCL.Runtime.CompilerSteps.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/FSCL.Runtime.Core.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/FSCL.Runtime.Execution.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/FSCL.Runtime.Language.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/FSCL.Runtime.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/FSCL.Runtime.Scheduling.dll"
#r @"../packages/FSCL.Runtime.2.0.1/lib/net451/OpenCLManagedWrapper.dll"

#r @"../Types/bin/Debug/Types.dll"
//#r @" /bin/Debug/RayTracing.dll"

OpenCL.OpenCLPlatform.Platforms.Count // Item(0)
OpenCL.OpenCLPlatform.Platforms.[0].Devices

//
// things for FSCL
open FSCL.Compiler
open FSCL.Language
open FSCL.Runtime 

//open ShadingNoise
open Types.Algebra
open System
//open Types.ObjectTypes
//open Types.types
// Test if the openCL function works

let timeStamps , Amplitude,Amplitude2 = [|(0.f)..(1.f/32768.f)..(6.0f-1.f/32768.f)|], Vector(0.,0.,0.075e-6) ,Vector(0.,0.075e-6,0.)  // Temporal series for the phase scan 20000
let ww = float32(3.1416*2./1.064e-6)
let diffRayXAmp = [|1e-6f;1e-6f|]
let nfa = [|(5.f,3.14f/5.f);(5.f,0.f)|]
let ws = new WorkSize(1024L)  // (GlobalSize, [LocalSize, globalOffset]) -> can be int64 or int64[]
timeStamps.Length


[<ReflectedDefinition>]       
let sinModOpenCL(tim:float32, dd:float32[], nfA:(float32*float32)[]) =
    // function to compute the globlal modulation (sum of all)
    let duepi = 2.f*3.14f
    //let dd = dotrayamp
    //    let ooo = 
    //            ([|0..nfA.Length-1|])
    //             |> Array.fold(fun acc x-> let freq, phase = nfA.[x]
    //                                       acc+sin(duepi*freq*tim+phase)*(dd.[x])) 0. 
    let mutable acc = 0.f
    for i in [|0..nfA.Length-1|] do
            let freq, phase = nfA.[i]
            acc <- acc+sin(duepi*freq*tim+phase)*(dd.[i])
    acc

[<ReflectedDefinition;Kernel>]  
let trueOpenCL(timeStamps:float32[],ww:float32,diffRayXAmpl:float32[],nfa:(float32*float32)[],wi:WorkItemInfo) =
    //<@
    let narr = Array.zeroCreate<float32> timeStamps.Length 
    let gid = wi.GlobalID(0)

    //[|0..timeStamps.Length-1|] 
    //|> Array.iter(fun t -> narr.[t] <- (ww*(sinModOpenCL((timeStamps.[t]),diffRayXAmp, nfa)))//SINMOD t)
    //                )//@>.Run() // Phase modulation
    narr.[gid] <- ww*sinModOpenCL((timeStamps.[gid]),diffRayXAmpl, nfa)
    narr



[<ReflectedDefinition;Kernel>]  
let falseOpenCL(prevMod:float32[],timeStamps:float32[],ww:float32,diffRayXAmpl:float32[],nfa:(float32*float32)[],wi:WorkItemInfo) =  
    let narr = Array.zeroCreate<float32> timeStamps.Length 
    let gid = wi.GlobalID(0)
    //<@
    //(prevMod,timeStamps)
    //[|0..timeStamps.Length-1|] 
    //|> Array.iter(fun t -> narr.[t] <- prevMod.[t]+(ww*( sinModOpenCL(timeStamps.[t],diffRayXAmp, nfa)))//SINMOD t)
    //                )//@>.Run() // Phase modulation
    narr.[gid] <- prevMod.[gid]+(ww*( sinModOpenCL(timeStamps.[gid], diffRayXAmpl, nfa)))
    narr
    

<@trueOpenCL(timeStamps,ww,diffRayXAmp,nfa,ws)@>.Run()


[<ReflectedDefinition; Kernel>]
let SimpleAdd(a: float[], b: float[], wi: WorkItemInfo) =
    // functions that actually returns the value of the sum
    let i = wi.GlobalID(0)
    let c = Array.zeroCreate a.Length
    c.[i] <- a.[i] + b.[i]
    c
let aa = Array.zeroCreate<float> 2048
let bb = Array.zeroCreate<float> 2048
[|0..aa.Length-1|] |> Array.iter(fun x -> aa.[x] <- float x)
[|0..aa.Length-1|] |> Array.iter(fun x -> bb.[x] <- 2.-float x)
let nws = new WorkSize(int64 aa.Length)  // (GlobalSize, [LocalSize, globalOffset]) -> can be int64 or int64[]

let oo = <@SimpleAdd(aa,bb,nws)@>.Run()
<@aa |> Array.map(fun x -> cos (x+3.14/2.))@>.Run()
oo |> Array.filter(fun x -> x<>0.) |> fun x -> x.Length
