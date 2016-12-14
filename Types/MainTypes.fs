namespace Types


module types =
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    //open MathNet.Spatial.Euclidean // requieres System.Xml
    open Algebra


    type Wavelength = WaveLength of float<m>              //

    type Material = {T: float; R:float;                        // transmission an reflectivity (coating)
                      n:float*Wavelength;                      // Index of Refraction
                      LambPPM:float;                           // Lambertian BRDF in ppm, not considered the 1/2PI
                      MatName: string                             // Identifier of the material to find it
                     }
    
    // first method to noise
    // This was intended in the case the objects contains some noise that it's transmitted to the ray
    //type noise = (byte*float)                       // example: (8uy, 9.) -> frequency[hz]*noise[M/sqrt(hz)] 

    // Second method to noise
    // This is to translate the vibration of the object into phase change on the ray.
    type noise = ((float*Vector*float)[]*float[])                        // Here it refers to the fact that ((frequency, Amplitude,phase)[], t_sampling)
    // byte is a number in the range [0,255], I use this because I expect I won't work on higher frequencies
    type Ray = {
                Wavelenght:Wavelength;
                from: Point; uvec: UnitVector;                 // From and direction
                MaxLength:float<m>                              // Max distance can travell (should be infinite by defect)
                OpticalPathTravelled: float<m>;                   // Optical Path Length Modified after every step with the IOR
                NumBounces:float; //byte;
                MaxDispersions: float;//byte;                              // Maximum bounces on dispersive media that can be done
                mutable bounces: float list;     // Num of bounces + the positions (Just in case for the future)
                NumOfParticles: int;                               // Num of photos -> To split in a Lambertian surface, etc...
                // If NumOfParticles is equal to 0 means that I'm doing single ray tracing          (no splitting)
                // if NumOfParticles is bigger or equal to 1 means that it's a multipleparticle     (splitting)
                IndexOfRefraction:float

                // Careful, NoiseAdd it's for frequency domain and PhaseModulation for time domain
                // NoiseAdd:float[]     - Not required now
                PhaseModulation: float[]
                // for NoiseAdd, I will need some function in order to sum the noise on the right frequency, but up to now, as a first step I am planning to use a single frequency
                }

    
    // New intersection type created because I must know which one is the object when the intersected object is a sensor
    type Intersection = { normal:UnitVector; point:Point; ray:Ray;MatName:string;  t:float<m>}

    let RealMatName (raw_Material_Name:string) (cos_inc:float) = 
        // find the real name of a material
        match raw_Material_Name with
        | x when x.StartsWith("ANG_") ->
            let angle = (acos( abs(cos_inc)) ) 
                        |> fun x -> (x/3.14159265359)*180. 
                        |> round //|> int - non ce bisogno di utilizzare l'int, string gia e' in abastanza
                        |> string
            x.[4..x.Length-1]+"_"+angle

        | _ ->  
            raw_Material_Name
       

