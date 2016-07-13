module periodogram

open System.Numerics
open MathNet.Numerics
open MathNet.Numerics.IntegralTransforms // Fourier.(the function to do the dft)
let welch_method(data:float[],window:float[],overlap:int,fs:float) =
    // data -> the data to compute the PSD
    // window -> Window for the method
    // windowl -< length of each window (not needed)
    // overlap -> overlap between 2 subdivisions
    // fs -> max frequency to compute??
    let winlen = window.Length              // size of each periodogram
    let step = winlen-overlap               // step between 2 periodograms
    let k = 1+(data.Length-winlen)/step     // number of sections to compute the periodogram
    let kLf = float (k*winlen)
    let U = (window |> Array.map(fun x -> x*x) |> Array.sum)/float(winlen)  // Square sum of the elements of the window
    let mutable istart, iend = 0 , winlen-1 
    let mutable ftrans = [|[||]|]
    //for i in [|1..k|] do
    [|1..k|] 
    |> Array.iter( //.Parallel
        fun i ->     
        //if window.Length <> data.[istart..iend].Length then
        //    printfn "here there's a breakpoint"
        let f = (window,data.[istart..iend]) ||> Array.map2(fun x y -> Complex(x*y,0.))
        Fourier.Forward(f)  // apply the fft
        istart <- istart+step
        iend <- iend+step
        ftrans <- Array.append ftrans [|f|]
        )
    
    let absComplexArr (x:Complex[]) = x |> Array.map(fun x -> 2.*x.Magnitude*x.Magnitude*(float winlen)/U) // 2* for because I only use the positive part
    //let modulFFT = 
    
    let intermediate =
        ftrans 
        //|> fun psd -> psd.[0..psd.[0].Length-1] // We are only interested on the possitive part, i.e. the first half
        |> Array.map(fun x ->  (absComplexArr x) )
    //printfn "ciao"
    //printfn "trans:\n%+A" ftrans
    //printfn "intermediate:\n%+A" intermediate


    intermediate |> Array.filter(fun x -> not(Array.isEmpty x))
    |> fun xt -> 
        [|0..xt.[0].Length-1|] 
        |> Array.map( fun index -> 
                           //if index = 0 then 
                           //printfn "index is: %d" index
                           //printfn "\ttheintermediateis:\n%+A" xt.Length
                           (xt |> Array.sumBy(fun eachPos -> eachPos.[index]))/kLf  
                     )


let PSD_WELCH(timeStamp:float[],data:float[],windowName:string,windowLengtht:int,overlap:int,fs:float)   =   
    // compute the spectrogram with the welch method and return it with its frequency equivalent 
    let freqMin,freqMax = 1./(timeStamp.[windowLengtht-1]-timeStamp.[0]), 0.5/(timeStamp.[1]-timeStamp.[0])
    //  windows available can be seen at:
    //  http://numerics.mathdotnet.com/api/MathNet.Numerics/Window.htm
    let winds =
        // compute the specified window
        match windowName with
        | "Haming"     -> Window.Hamming(windowLengtht)
        | "Hann"       -> Window.Hann(windowLengtht)
        | "FlatTop"    -> Window.FlatTop(windowLengtht) 
        | "Triangular" -> Window.Triangular(windowLengtht)
        // not valid, requires more info | "Gauss"      -> Window.Gauss(windowLengtht)
        | _            -> 
            printfn "the window defined is not implemented, the ones already implemented are:\n   Haming\n   Hannn\n   FlatTop\n   Triangular\n   Gauss\nDefault window chosen = Hann"  
            Window.Hann(windowLengtht)    
    let frequencies = [|(0.)..(freqMin)..freqMax|]
    let PSD = welch_method(data,winds,overlap,fs)
    //printfn "are freq and PSD the same length?: %+A" (PSD.Length = frequencies.Length)
    (frequencies, PSD.[0..frequencies.Length-1])
