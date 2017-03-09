namespace PostProcess

open MathNet.Numerics.IntegralTransforms // Fourier.(the function to do the dft)
open Types.Algebra
open Types.ObjectTypes
open Types.types
open System.Numerics
open Random
open System.IO
open waveproduct

module Noise =
    open periodogram

    let FourierTransformPhase(sns:SensorContent, timestamp:float[]) = 
        // Enter the sensor and produce as output the fourier transform of each phase
        // I cannot do the summ of all the phases, because each photon gives a contribution depending on the arrival angle
        // process:
        // 

        let phase_fre_dom =  sns.Noise |> Array.map(fun x -> Complex(sin x,0.))
        //sns.SavedData  |> Array.map(fun x -> x.Noise // Array for each time for each photon
        //                                                        |> Array.map(fun y -> Complex(y,0.))
        //                                                    )
        //|> Array.iter(fun x -> Fourier.Forward(x.Phase)) // there are different FFT algorithms to choose
        phase_fre_dom |> (fun x -> Fourier.Forward(x) )

        // define the frequencies of analized
        let freqmin, freqmax = 0.5/abs(timestamp.[timestamp.Length-1]-timestamp.[0]) ,1./abs(timestamp.[1]-timestamp.[0])
        let freq = [|-freqmax..freqmin..freqmax|]
        if freq.Length <> phase_fre_dom.Length then printfn "The length of the frequencies and ph(f) are not the same"
        freq, phase_fre_dom

    let ASDofPhase(sns:SensorContent, timestamp:float[]) = 
        let freqs, phase_fre_dom = FourierTransformPhase(sns, timestamp)
        
        // normalization factor
        let absPhase = phase_fre_dom |> Array.map(fun z -> Complex.Abs(z))
        let normFact = absPhase |>  Array.sum
        freqs,(absPhase)|> Array.map(fun z -> z/normFact)
    
    let b_theta ang:float =
        // from paper of Vinet, eq (2)
        let thmin = 1e-2
        let  k =  0.01006406986 // based on the current thmin
        if ang > thmin then
            k/ang/ang
        else k/thmin/thmin
    let bXn (an:float) (npart:float) (n:float[]) =
        // Returns the value of the product of the probability of the direction of this ray on the mirror per the spectral density of the phase change
        // considers that the input n is ASD
        // sns.SavedData.[0].Direction
        let fpart =  npart 
        let n2 = n |> Array.map(fun x -> fpart*x*x)         // if there are 2 particles, it is considered twice, three three times, etc..
        let prb:float = b_theta(an) // b(theta)
        n2 |> Array.map(fun x -> prb*x)

    let bXn2 (an:float) (npart:float) (n2:float[]) =
        // Returns the value of the product of the probability of the direction of this ray on the mirror per the spectral density of the phase change
        // Already considers that the input is PSD: n2
        // sns.SavedData.[0].Direction
        let fpart =  npart 
        let prb:float = b_theta(an) // b(theta)
        n2 |> Array.map(fun x -> fpart*prb*x)              // if there are 2 particles, it is considered twice, three three times, etc..

    let NoiseInterferometerArm (mirror:disc) (tube:cylinder) (mat:System.Collections.Generic.IDictionary<string,Material>) (ray:Ray) (powerStored:float) (nRays:int)=
        // this is to find h(f) on one of the arms of the interferometer
        // inside mirror there's the sensor and the noise
        let planckConstant, vLight = 6.626070e-34, 299792458. // Plank constant and c all in SI [h] = Js, [c] = m/s
        let wavelen = (match ray.Wavelenght with WaveLength x -> float x)
        let PhotonEnergy = planckConstant*vLight/wavelen
        //let ns = mat.[mirror.MatName].LambPPM*powerStored/PhotonEnergy  // case I compute, but I shouldn't
        let ns = float nRays
        // do the sum of the b*n2
        
        let sumProd = 
            let tStamp = snd tube.Noise  |> Array.map(fun x -> float x)
            mirror.Sensor.SavedData.ToArray()//.[0].Direction 
            |> Array.map(fun x -> 
                                    // the b(theta) and n(f)
                                    let ang = acos(x.Direction*mirror.Normal) |> abs  // be sure that it's always positive
                                    let n = snd(ASDofPhase(x, tStamp)) // freq, ASD(normalized)
                                    let npart = x.FracOfRay
                                    bXn ang npart n // do the product 
                          )
            |> fun allNoise -> // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                [|0..allNoise.[0].Length-1|] |> Array.map(fun ind -> (allNoise |> Array.sumBy(fun eachPhotonNoise -> eachPhotonNoise.[ind]) ))
        
        let OutTheSquareRoot = (wavelen*wavelen*mat.[mirror.MatName].LambPPM)/(sqrt(2.**5.)*PI*PI*(float tube.Zmax)*(float mirror.Radius))

        sumProd |> Array.map(fun nf -> OutTheSquareRoot*sqrt(nf/ns))

    let SumPhaseNoise_WELCH (mirror:disc) (tube:cylinder) (mat:System.Collections.Generic.IDictionary<string,Material>) (ray:Ray) (windowLengtht:int) =
        // this is to find h(f) on one of the arms of the interferometer
        // inside mirror there's the sensor and the noise
        // It uses the welch method to obtain the PSD of the phases of each photon.

        //let planckConstant, vLight = 6.626070e-34, 299792458. // Plank constant and c all in SI [h] = Js, [c] = m/s
        //let wavelen = (match ray.Wavelenght with WaveLength x -> float x)
        //let PhotonEnergy = planckConstant*vLight/wavelen
        //let ns = mat.[mirror.MatName].LambPPM*powerStored/PhotonEnergy  // case I compute, but I shouldn't
        //let ns = float nRays
        // do the sum of the b*n2
        
        let sumProd = // frequencies*noise_PSD
            printfn "time is: %+A" tube.Noise
            let tStamp = (snd tube.Noise) 
            
            mirror.Sensor.SavedData.ToArray() |> Array.filter(fun x -> x.Noise.Length <> 0) // filter the photons that arrive directly and don't contribute to the noise 
            |> Array.map(fun x -> 
                                    // the b(theta) and n(f)
                                    let ang = acos(x.Direction*mirror.Normal) |> abs  // be sure that it's always positive
                                    //let n = snd(ASDofPhase(x, tStamp)) // freq, ASD(normalized)
                                    let n2 = PSD_WELCH(tStamp|> Array.map(fun x -> float x),x.Noise|> Array.map(fun x -> sin(x)), // requires the sin(phase)
                                                       "Hann",windowLengtht,windowLengtht/2,0.) 
                                    // n2 because it considers that the 
                                    let npart = x.FracOfRay               // I must consider that a ray may contain more than 1 particle
                                    fst n2 ,bXn2 ang npart (snd n2) // do the product 
                          )
            |> fun allNoise -> // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                //let allNoise = snd allNoise2
                let noiseSummed =
                    [|0..(snd allNoise.[0]).Length-1|] 
                    |> Array.map(fun ind -> (allNoise |> Array.sumBy(fun eachPhotonNoise -> (snd eachPhotonNoise).[ind]) ))
                (fst allNoise.[0]),noiseSummed          // All the photons have the same timestamp and the same frequency domain
        
        //let OutTheSquareRoot = (wavelen*wavelen*mat.[mirror.MatName].LambPPM)/(sqrt(2.**5.)*PI*PI*(float tube.Zmax)*(float mirror.Radius))

        sumProd //fst sumProd,snd sumProd |> Array.map(fun nf -> (nf/ns))



    let NoiseInterferometerArm_WELCH (mirror:disc) (tube:cylinder) (mat:System.Collections.Generic.IDictionary<string,Material>) (ray:Ray) (powerStored:float) (nRays:int) (windowLengtht:int) =
        // this is to find h(f) on one of the arms of the interferometer
        // inside mirror there's the sensor and the noise
        // It uses the welch method to obtain the PSD of the phases of each photon.

        let planckConstant, vLight = 6.626070e-34, 299792458. // Plank constant and c all in SI [h] = Js, [c] = m/s
        let wavelen = (match ray.Wavelenght with WaveLength x -> float x)
        let PhotonEnergy = planckConstant*vLight/wavelen
        //let ns = mat.[mirror.MatName].LambPPM*powerStored/PhotonEnergy  // case I compute, but I shouldn't
        let ns = float nRays
        // do the sum of the b*n2
        
        let sumProd = SumPhaseNoise_WELCH (mirror) (tube) (mat) (ray) (windowLengtht)// frequencies*noise_PSD
        (*
            mirror.Sensor.SavedData |> Array.filter(fun x -> x.Noise.Length <> 0) // filter the photons that arrive directly and don't contribute to the noise 
            |> Array.map(fun x -> 
                                    // the b(theta) and n(f)
                                    let ang = acos(x.Direction*mirror.Normal) |> abs  // be sure that it's always positive
                                    let tStamp = snd tube.Noise
                                    //let n = snd(ASDofPhase(x, tStamp)) // freq, ASD(normalized)
                                    let n2 = PSD_WELCH(tStamp,x.Noise|> Array.map(fun x -> sin(x)), // requires the sin(phase)
                                                       "Hann",windowLengtht,windowLengtht/2,0.) 
                                    // n2 because it considers that the 
                            
                                    fst n2 ,bXn2 ang (snd n2) // do the product 
                          )
            |> fun allNoise -> // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                //let allNoise = snd allNoise2
                let noiseSummed =
                    [|0..(snd allNoise.[0]).Length-1|] 
                    |> Array.map(fun ind -> (allNoise |> Array.sumBy(fun eachPhotonNoise -> (snd eachPhotonNoise).[ind]) ))
                (fst allNoise.[0]),noiseSummed          // All the photons have the same timestamp and the same frequency domain
        *)
        let OutTheSquareRoot = (wavelen*wavelen*mat.[mirror.MatName].LambPPM)/(sqrt(2.**5.)*PI*PI*(float tube.Zmax)*(float mirror.Radius))

        fst sumProd,snd sumProd |> Array.map(fun nf -> OutTheSquareRoot*sqrt(nf/ns))


        
    let SumAndSavePhases (snrs:Sensor) (mrNormal:UnitVector) (path_save:string)=
        // sum the phases with the factor of the b(theta)
        // Just done as exercise because it cannot be used to obtain proper results.
        let ph_b_theta = snrs.SavedData.ToArray() 
                         |> Array.map(fun y -> let Sqrt_b_ang = sqrt( b_theta( acos(y.Direction*mrNormal) |> abs ))
                                               (y.Noise |> Array.map(fun z -> Sqrt_b_ang*sin(z)))      )
        
        let phs = ph_b_theta 
                  |> fun allNoise -> // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                        [|0..allNoise.[0].Length-1|]
                        |> Array.map(fun ind -> 
                                        (allNoise |> Array.sumBy(fun eachPhotonNoise -> eachPhotonNoise.[ind]) )
                                     )
        //let path_save = @"C:\Users\Jose M. Gonzalez\Desktop\Vibration1_sin_upconversion.txt"
        File.WriteAllLines( path_save, phs |> Array.map(fun x -> string(x)) )


    let Phase_NoiseAlone (dsc:disc,  windowLengtht:int, path_save:string) = //(roc:float) (waist:float) (diameter:float) (pw:float) =
        // compute the total phase change produced by the photons impinging on the detector
        //let dsc = match obj with Disc x -> x | _ -> failwith "the sensor should be a disc sensor"
        let snrs = dsc.Sensor
        let timeStamps = snd dsc.Noise                                                                // the Noise works a: ((frequency, Amplitude,phase)[], t_sampling)
        let frequencies = (1./(timeStamps.[windowLengtht-1]-timeStamps.[0]), 0.5/(timeStamps.[1]-timeStamps.[0])) 
                          ||> fun freqMin freqMax -> [|(0.)..(freqMin)..freqMax|] //       define the frequencies 

        // I need to iter the values of the sensor, multiply dotprod*noise(freq) and sum them
        let suum =
            match (snrs.SavedData.Count) with
            | y when y>0 ->    
                snrs.SavedData                                                                            // read the sensor info
                |> Seq.map( fun x ->
                               let _ , asd = 
                                    match x.Noise.Length with
                                    | z when z > 0 ->
                                        ASD_WELCH(timeStamps,x.Noise|> Array.map(fun y -> sin(y+x.Phase)),    // consider the dinamic and the static phase
                                                  "Hann",windowLengtht, windowLengtht/2, 0.)
                                    | _ -> (Array.zeroCreate<float> frequencies.Length , Array.zeroCreate<float> frequencies.Length )
                               asd                                                                        // return the total phase change at the defined frequency
                            )
                          
                |> fun allNoise ->       // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                    let noiseSummed =
                        [|0..((allNoise) |> Seq.head).Length-1|] 
                        |> Array.map(fun ind -> (allNoise |> Seq.sumBy(fun eachPhotonNoise -> (eachPhotonNoise).[ind]) ))
                    noiseSummed          //return the sum of all the phase noise (no frequency defined here)
            | y when y<= 0 ->   Array.zeroCreate<float> frequencies.Length                                   

        File.WriteAllLines( path_save, (frequencies, suum) ||> Array.map2(fun y x -> string(y)+" "+ string(x)) ) 



    let PhaseChange (dsc:disc, src:Source, windowLengtht:int, path_save:string) = //(roc:float) (waist:float) (diameter:float) (pw:float) =
        // compute the total phase change produced by the photons impinging on the detector
        //let dsc = match obj with Disc x -> x | _ -> failwith "the sensor should be a disc sensor"
        let snrs = dsc.Sensor
        let timeStamps = snd dsc.Noise                                                                // the Noise works a: ((frequency, Amplitude,phase)[], t_sampling)
        //let windowLengtht = 
        // I need to iter the values of the sensor, multiply dotprod*noise(freq) and sum them
        let frequencies = (1./(timeStamps.[windowLengtht-1]-timeStamps.[0]), 0.5/(timeStamps.[1]-timeStamps.[0])) 
                          ||> fun freqMin freqMax -> [|(0.)..(freqMin)..freqMax|] //       define the frequencies 
        let suum =
            match (snrs.SavedData.Count) with
            | y when y>0 ->                                                                                     // read the sensor info 
                snrs.SavedData
                |> Seq.map( fun x ->
                                let dotprod,ph_gauss_met = Gauss_Plane_Wave_Product(x,dsc,src)                   // dot product, phase_gauss_meters
                                let _ , asd = 
                                    match x.Noise.Length with
                                    | z when z > 0 ->
                                        ASD_WELCH(timeStamps,x.Noise|> Array.map(fun y -> sin(y+x.Phase+ph_gauss_met)),    // consider the dinamic and the static phase
                                                  "Hann",windowLengtht, windowLengtht/2, 0.)
                                    | _ -> (Array.zeroCreate<float> frequencies.Length , Array.zeroCreate<float> frequencies.Length )
                                let abcd = 'c'
                                asd |> Array.map(fun x -> x*dotprod)                                            // return the total phase change at the defined frequency
                            )
                          
                |> fun allNoise ->       // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                    let noiseSummed =
                        [|0..((allNoise) |> Seq.head).Length-1|] 
                        |> Array.map(fun ind -> (allNoise |> Seq.sumBy(fun eachPhotonNoise -> (eachPhotonNoise).[ind]) ))
                    noiseSummed          //return the sum of all the phase noise (no frequency defined here)
            | y when y<= 0 ->   Array.zeroCreate<float> frequencies.Length                                   

        File.WriteAllLines( path_save, (frequencies, suum) ||> Array.map2(fun y x -> string(y)+" "+ string(x)) ) 



        

    let PhaseChange_QPD (dsc:disc , src:Source, windowLengtht:int , path_save:string) = //(roc:float) (waist:float) (diameter:float) (pw:float) =
        // compute the total phase change produced by the photons impinging on a quadrant photodetector
        // it divides the space on the centre on the 4 quadrants and returns the phase for each quadrant
        //let dsc = match obj with Disc x -> x | _ -> failwith "the sensor should be a disc sensor"
        let snrs = dsc.Sensor
        let timeStamps = snd dsc.Noise                                                                // the Noise works a: ((frequency, Amplitude,phase)[], t_sampling)
        //let windowLengtht = 
        // I need to iter the values of the sensor, multiply dotprod*noise(freq) and sum them
        // from global to local coordinates
        let s_dir =  dsc.Normal       // sensor direction
        //    match obj with 
        //    | Disc x -> x.Normal
        //    | _ -> failwith "The function is thought for disc sensors"
        let transmat = Matrix.RotateVector(s_dir,UnitVector(0.,0.,1.))   // rotate sensor direction to 0.,0.,1.
        let s_orig =  dsc.Centre           // sensor centre
        //     match obj with 
        //     | Disc x -> x.Centre
        //     | _ -> failwith "The function is thought for disc sensors"
        
                                         
        let frequencies = (1./(timeStamps.[windowLengtht-1]-timeStamps.[0]), 0.5/(timeStamps.[1]-timeStamps.[0])) 
                          ||> fun freqMin freqMax -> [|(0.)..(freqMin)..freqMax|] //       define the frequencies 

        //
        //  Upper side of the sensor
        //
        let suum_UR =
          match (snrs.SavedData.Count) with
            | y when y>0 ->                                                                               // read the sensor info 
                snrs.SavedData                                                                                        // read the sensor info
                |> Seq.filter(fun p -> //  filter to create the upper left
                                       let p_local = transmat.RotatePoint((p.Position-s_orig).ToPoint())              // hitting point in local coordinates
                                       if abs(p_local.Z) > 1e-10 then failwith "Zvalue is wrong on PhaseChange_QPD"
                                       (p_local.X > 0. && p_local.Y > 0.)
                                )                                             
                |> Seq.map( fun x ->
                                let dotprod,ph_gauss_met = Gauss_Plane_Wave_Product(x,dsc,src)                        // dot product, phase_gauss_meters
                                let _ , asd = 
                                    match x.Noise.Length with
                                    | z when z > 0 ->
                                        ASD_WELCH(timeStamps,x.Noise|> Array.map(fun y -> sin(y+x.Phase+ph_gauss_met)),    // consider the dinamic and the static phase
                                                  "Hann",windowLengtht, windowLengtht/2, 0.)
                                    | _ -> (Array.zeroCreate<float> frequencies.Length , Array.zeroCreate<float> frequencies.Length )
                                asd |> Array.map(fun x -> x*dotprod)                                                  // return the total phase change at the defined frequency
                            )
                          
                |> fun allNoise ->       // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                    let noiseSummed =
                        [|0..((allNoise) |> Seq.head).Length-1|] 
                        |> Array.map(fun ind -> (allNoise |> Seq.sumBy(fun eachPhotonNoise -> (eachPhotonNoise).[ind]) ))
                    noiseSummed          //return the sum of all the phase noise (no frequency defined here)
            | y when y<= 0 ->   Array.zeroCreate<float> frequencies.Length                                   
 
        let suum_UL =
          match (snrs.SavedData.Count) with
            | y when y>0 ->                                                                               // read the sensor info 
                snrs.SavedData                                                                                        // read the sensor info
                |> Seq.filter(fun p -> //  filter to create the upper left
                                       let p_local = transmat.RotatePoint((p.Position-s_orig).ToPoint())              // hitting point in local coordinates
                                       if abs(p_local.Z) > 1e-10 then failwith "Zvalue is wrong on PhaseChange_QPD"
                                       (p_local.X <= 0. && p_local.Y > 0.)
                                )                                             
                |> Seq.map( fun x ->
                                let dotprod,ph_gauss_met = Gauss_Plane_Wave_Product(x,dsc,src)                        // dot product, phase_gauss_meters
                                let _ , asd = 
                                    match x.Noise.Length with
                                    | z when z > 0 ->
                                        ASD_WELCH(timeStamps,x.Noise|> Array.map(fun y -> sin(y+x.Phase+ph_gauss_met)),    // consider the dinamic and the static phase
                                                  "Hann",windowLengtht, windowLengtht/2, 0.)
                                    | _ -> (Array.zeroCreate<float> frequencies.Length , Array.zeroCreate<float> frequencies.Length )
                                asd |> Array.map(fun x -> x*dotprod)                                                  // return the total phase change at the defined frequency
                            )
                          
                |> fun allNoise ->       // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                    let noiseSummed =
                        [|0..((allNoise) |> Seq.head).Length-1|] 
                        |> Array.map(fun ind -> (allNoise |> Seq.sumBy(fun eachPhotonNoise -> (eachPhotonNoise).[ind]) ))
                    noiseSummed          //return the sum of all the phase noise (no frequency defined here)
            | y when y<= 0 ->   Array.zeroCreate<float> frequencies.Length                                   
        //
        //  Lower side of the sensor
        //
        let suum_DR =
          match (snrs.SavedData.Count) with
            | y when y>0 ->                                                                               // read the sensor info 
                snrs.SavedData                                                                                        // read the sensor info
                |> Seq.filter(fun p -> //  filter to create the upper left
                                       let p_local = transmat.RotatePoint((p.Position-s_orig).ToPoint())              // hitting point in local coordinates
                                       if abs(p_local.Z) > 1e-10 then failwith "Zvalue is wrong on PhaseChange_QPD"
                                       (p_local.X > 0. && p_local.Y <= 0.)
                                )                                             
                |> Seq.map( fun x ->
                                let dotprod,ph_gauss_met = Gauss_Plane_Wave_Product(x,dsc,src)                        // dot product, phase_gauss_meters
                                let _ , asd = 
                                    match x.Noise.Length with
                                    | z when z > 0 ->
                                        ASD_WELCH(timeStamps,x.Noise|> Array.map(fun y -> sin(y+x.Phase+ph_gauss_met)),    // consider the dinamic and the static phase
                                                  "Hann",windowLengtht, windowLengtht/2, 0.)
                                    | _ -> (Array.zeroCreate<float> frequencies.Length , Array.zeroCreate<float> frequencies.Length )
                                asd |> Array.map(fun x -> x*dotprod)                                                  // return the total phase change at the defined frequency
                            )
                          
                |> fun allNoise ->       // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                    let noiseSummed =
                        [|0..((allNoise) |> Seq.head).Length-1|] 
                        |> Array.map(fun ind -> (allNoise |> Seq.sumBy(fun eachPhotonNoise -> (eachPhotonNoise).[ind]) ))
                    noiseSummed          //return the sum of all the phase noise (no frequency defined here)
            | y when y<= 0 ->   Array.zeroCreate<float> frequencies.Length                                   
        let suum_DL =
          match (snrs.SavedData.Count) with
            | y when y>0 ->                                                                               // read the sensor info 
                snrs.SavedData                                                                                        // read the sensor info
                |> Seq.filter(fun p -> //  filter to create the upper left
                                       let p_local = transmat.RotatePoint((p.Position-s_orig).ToPoint())              // hitting point in local coordinates
                                       if abs(p_local.Z) > 1e-10 then failwith "Zvalue is wrong on PhaseChange_QPD"
                                       (p_local.X <= 0. && p_local.Y <= 0.)
                                )                                             
                |> Seq.map( fun x ->
                                let dotprod,ph_gauss_met = Gauss_Plane_Wave_Product(x,dsc,src)                        // dot product, phase_gauss_meters
                                let _ , asd = 
                                    match x.Noise.Length with
                                    | z when z > 0 ->
                                        ASD_WELCH(timeStamps,x.Noise|> Array.map(fun y -> sin(y+x.Phase+ph_gauss_met)),    // consider the dinamic and the static phase
                                                  "Hann",windowLengtht, windowLengtht/2, 0.)
                                    | _ -> (Array.zeroCreate<float> frequencies.Length , Array.zeroCreate<float> frequencies.Length )

                                asd |> Array.map(fun x -> x*dotprod)                                                  // return the total phase change at the defined frequency
                            )
                          
                |> fun allNoise ->       // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                    let noiseSummed =
                        [|0..((allNoise) |> Seq.head).Length-1|] 
                        |> Array.map(fun ind -> (allNoise |> Seq.sumBy(fun eachPhotonNoise -> (eachPhotonNoise).[ind]) ))
                    noiseSummed          //return the sum of all the phase noise (no frequency defined here)
            | y when y<= 0 ->   Array.zeroCreate<float> frequencies.Length                                   
                                         


        let ur, ul, dr, dl = path_save.Replace(".dat", "UR."), path_save.Replace(".dat", "UL."), path_save.Replace(".dat", "DR."), path_save.Replace(".dat", "DL.")
        File.WriteAllLines( ur, (frequencies, suum_UL) ||> Array.map2(fun y x -> string(y)+" "+ string(x)) ) 
        File.WriteAllLines( ul, (frequencies, suum_UL) ||> Array.map2(fun y x -> string(y)+" "+ string(x)) ) 
        File.WriteAllLines( dr, (frequencies, suum_DR) ||> Array.map2(fun y x -> string(y)+" "+ string(x)) ) 
        File.WriteAllLines( dl, (frequencies, suum_DL) ||> Array.map2(fun y x -> string(y)+" "+ string(x)) ) 




        

    let PhaseChange_QPD_decentre (dsc:disc , src:Source ,windowLengtht:int , dx:float , dy:float , path_save:string) = //(roc:float) (waist:float) (diameter:float) (pw:float) =
        // compute the total phase change produced by the photons impinging on a quadrant photodetector
        // it divides the space on the centre on the 4 quadrants and returns the phase for each quadrant
        //let dsc = match obj with Disc x -> x | _ -> failwith "the sensor should be a disc sensor"
        let snrs = dsc.Sensor
        let timeStamps = snd dsc.Noise                                                                // the Noise works a: ((frequency, Amplitude,phase)[], t_sampling)
        //let windowLengtht = 
        // I need to iter the values of the sensor, multiply dotprod*noise(freq) and sum them
        // from global to local coordinates
        let s_dir =  dsc.Normal           // sensor direction
        //   match obj with 
        //   | Disc x -> x.Normal
        //   | _ -> failwith "The function is thought for disc sensors"
        let transmat = Matrix.RotateVector(s_dir,UnitVector(0.,0.,1.))   // rotate sensor direction to 0.,0.,1.
        let s_orig =  dsc.Centre           // sensor centre
        //     match obj with 
        //     | Disc x -> x.Centre
        //     | _ -> failwith "The function is thought for disc sensors"
                                         
        let frequencies = (1./(timeStamps.[windowLengtht-1]-timeStamps.[0]), 0.5/(timeStamps.[1]-timeStamps.[0])) 
                          ||> fun freqMin freqMax -> [|(0.)..(freqMin)..freqMax|] //       define the frequencies 
        

        //
        //  Upper side of the sensor
        //
        let suum_UR =
          match (snrs.SavedData.Count) with
            | y when y>0 ->                                                                               // read the sensor info 
                snrs.SavedData                                                                                        // read the sensor info
                |> Seq.filter(fun p -> //  filter to create the upper left
                                       let p_local = transmat.RotatePoint((p.Position-s_orig).ToPoint())              // hitting point in local coordinates
                                       if abs(p_local.Z) > 1e-10 then failwith "Zvalue is wrong on PhaseChange_QPD"
                                       (p_local.X > dx  && p_local.Y > dy )
                                )                                             
                |> Seq.map( fun x ->
                                let dotprod,ph_gauss_met = Gauss_Plane_Wave_Product(x,dsc,src)                        // dot product, phase_gauss_meters
                                let _ , asd = 
                                    match x.Noise.Length with
                                    | z when z > 0 ->
                                        ASD_WELCH(timeStamps,x.Noise|> Array.map(fun y -> sin(y+x.Phase+ph_gauss_met)),    // consider the dinamic and the static phase
                                                  "Hann",windowLengtht, windowLengtht/2, 0.)
                                    | _ -> (Array.zeroCreate<float> frequencies.Length , Array.zeroCreate<float> frequencies.Length )

                                asd |> Array.map(fun x -> x*dotprod)                                                  // return the total phase change at the defined frequency
                            )
                          
                |> fun allNoise ->       // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                    let noiseSummed =
                        [|0..((allNoise) |> Seq.head).Length-1|] 
                        |> Array.map(fun ind -> (allNoise |> Seq.sumBy(fun eachPhotonNoise -> (eachPhotonNoise).[ind]) ))
                    noiseSummed          //return the sum of all the phase noise (no frequency defined here)
            | y when y<= 0 ->   Array.zeroCreate<float> frequencies.Length                                   
        let suum_UL =
          match (snrs.SavedData.Count) with
            | y when y>0 ->                                                                               // read the sensor info 
                snrs.SavedData                                                                                        // read the sensor info
                |> Seq.filter(fun p -> //  filter to create the upper left
                                       let p_local = transmat.RotatePoint((p.Position-s_orig).ToPoint())              // hitting point in local coordinates
                                       if abs(p_local.Z) > 1e-10 then failwith "Zvalue is wrong on PhaseChange_QPD"
                                       (p_local.X <= dx && p_local.Y > dy)
                                )                                             
                |> Seq.map( fun x ->
                                let dotprod,ph_gauss_met = Gauss_Plane_Wave_Product(x,dsc,src)                        // dot product, phase_gauss_meters
                                let _ , asd = 
                                    match x.Noise.Length with
                                    | z when z > 0 ->
                                        ASD_WELCH(timeStamps,x.Noise|> Array.map(fun y -> sin(y+x.Phase+ph_gauss_met)),    // consider the dinamic and the static phase
                                                  "Hann",windowLengtht, windowLengtht/2, 0.)
                                    | _ -> (Array.zeroCreate<float> frequencies.Length , Array.zeroCreate<float> frequencies.Length )

                                asd |> Array.map(fun x -> x*dotprod)                                                  // return the total phase change at the defined frequency
                            )
                          
                |> fun allNoise ->       // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                    let noiseSummed =
                        [|0..((allNoise) |> Seq.head).Length-1|] 
                        |> Array.map(fun ind -> (allNoise |> Seq.sumBy(fun eachPhotonNoise -> (eachPhotonNoise).[ind]) ))
                    noiseSummed          //return the sum of all the phase noise (no frequency defined here)
            | y when y<= 0 ->   Array.zeroCreate<float> frequencies.Length                                   
        //
        //  Lower side of the sensor
        //
        let suum_DR =
          match (snrs.SavedData.Count) with
            | y when y>0 ->                                                                               // read the sensor info 
                snrs.SavedData                                                                                        // read the sensor info
                |> Seq.filter(fun p -> //  filter to create the upper left
                                       let p_local = transmat.RotatePoint((p.Position-s_orig).ToPoint())              // hitting point in local coordinates
                                       if abs(p_local.Z) > 1e-10 then failwith "Zvalue is wrong on PhaseChange_QPD"
                                       (p_local.X > dx && p_local.Y <= dy)
                                )                                             
                |> Seq.map( fun x ->
                                let dotprod,ph_gauss_met = Gauss_Plane_Wave_Product(x,dsc,src)                        // dot product, phase_gauss_meters
                                let _ , asd = 
                                    match x.Noise.Length with
                                    | z when z > 0 ->
                                        ASD_WELCH(timeStamps,x.Noise|> Array.map(fun y -> sin(y+x.Phase+ph_gauss_met)),    // consider the dinamic and the static phase
                                                  "Hann",windowLengtht, windowLengtht/2, 0.)
                                    | _ -> (Array.zeroCreate<float> frequencies.Length , Array.zeroCreate<float> frequencies.Length )

                                asd |> Array.map(fun x -> x*dotprod)                                                  // return the total phase change at the defined frequency
                            )
                          
                |> fun allNoise ->       // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                    let noiseSummed =
                        [|0..((allNoise) |> Seq.head).Length-1|] 
                        |> Array.map(fun ind -> (allNoise |> Seq.sumBy(fun eachPhotonNoise -> (eachPhotonNoise).[ind]) ))
                    noiseSummed          //return the sum of all the phase noise (no frequency defined here)
            | y when y<= 0 ->   Array.zeroCreate<float> frequencies.Length                                   
        let suum_DL =
          match (snrs.SavedData.Count) with
            | y when y>0 ->                                                                               // read the sensor info 
                snrs.SavedData                                                                                        // read the sensor info
                |> Seq.filter(fun p -> //  filter to create the upper left
                                       let p_local = transmat.RotatePoint((p.Position-s_orig).ToPoint())              // hitting point in local coordinates
                                       if abs(p_local.Z) > 1e-10 then failwith "Zvalue is wrong on PhaseChange_QPD"
                                       (p_local.X <= dx && p_local.Y <= dy)
                                )                                             
                |> Seq.map( fun x ->
                                let dotprod,ph_gauss_met = Gauss_Plane_Wave_Product(x,dsc,src)                        // dot product, phase_gauss_meters
                                let _ , asd = 
                                    match x.Noise.Length with
                                    | z when z > 0 ->
                                        ASD_WELCH(timeStamps,x.Noise|> Array.map(fun y -> sin(y+x.Phase+ph_gauss_met)),    // consider the dinamic and the static phase
                                                  "Hann",windowLengtht, windowLengtht/2, 0.)
                                    | _ -> (Array.zeroCreate<float> frequencies.Length , Array.zeroCreate<float> frequencies.Length )

                                asd |> Array.map(fun x -> x*dotprod)                                                  // return the total phase change at the defined frequency
                            )
                          
                |> fun allNoise ->       // Sum the components of each time [| [|1;2|] ; [|0;1|] |] -> [| 1; 3 |]
                    let noiseSummed =
                        [|0..((allNoise) |> Seq.head).Length-1|] 
                        |> Array.map(fun ind -> (allNoise |> Seq.sumBy(fun eachPhotonNoise -> (eachPhotonNoise).[ind]) ))
                    noiseSummed          //return the sum of all the phase noise (no frequency defined here)
            | y when y<= 0 ->   Array.zeroCreate<float> frequencies.Length                                   
                                         



        let ur, ul, dr, dl = path_save.Replace(".dat", "UR."), path_save.Replace(".dat", "UL."), path_save.Replace(".dat", "DR."), path_save.Replace(".dat", "DL.")
        File.WriteAllLines( ur, (frequencies, suum_UL) ||> Array.map2(fun y x -> string(y)+" "+ string(x)) ) 
        File.WriteAllLines( ul, (frequencies, suum_UL) ||> Array.map2(fun y x -> string(y)+" "+ string(x)) ) 
        File.WriteAllLines( dr, (frequencies, suum_DR) ||> Array.map2(fun y x -> string(y)+" "+ string(x)) ) 
        File.WriteAllLines( dl, (frequencies, suum_DL) ||> Array.map2(fun y x -> string(y)+" "+ string(x)) ) 

    let PhaseChange_QPD_decentreX (dsc:disc , src:Source , windowLengtht:int , dx:float , path_save:string) = 
        // same as before, but scan only on X axis
        PhaseChange_QPD_decentre (dsc , src , windowLengtht , dx , 0. , path_save)
        
        
    let PhaseChange_QPD_decentreY (dsc:disc , src:Source, windowLengtht:int , dy:float , path_save:string) = 
        // same as before, but scan only on Y axis
        PhaseChange_QPD_decentre (dsc, src ,windowLengtht , 0. , dy , path_save) 
