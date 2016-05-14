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
    
    type noise = (byte*float)                       // example: (8uy, 9.) -> frequency[hz]*noise[M/sqrt(hz)] 
    // byte is a number in the range [0,255], I use this because I expect I won't work on higher frequencies
    type Ray = {
                Wavelenght:Wavelength;
                from: Point; uvec: UnitVector;                 // From and direction
                MaxLength:float<m>                              // Max distance can travell (should be infinite by defect)
                OpticalPathTravelled: float<m>;                   // Optical Path Length Modified after every step with the IOR
                NumBounces: byte;
                MaxDispersions: byte;                              // Maximum bounces on dispersive media that can be done
                mutable bounces: float list;     // Num of bounces + the positions (Just in case for the future)
                NumOfParticles: int;                               // Num of photos -> To split in a Lambertian surface, etc...
                // If NumOfParticles is equal to 0 means that I'm doing single ray tracing          (no splitting)
                // if NumOfParticles is bigger or equal to 1 means that it's a multipleparticle     (splitting)
                IndexOfRefraction:float

                NoiseAdd:noise[]
                // for NoiseAdd, I will need some function in order to sum the noise on the right frequency, but up to now, as a first step I am planning to use a single frequency
                }

    
    // New intersection type created because I must know which one is the object when the intersected object is a sensor
    type Intersection = { normal:UnitVector; point:Point; ray:Ray;MatName:string ; t:float<m>}//; ObjectSensor:Object Option}
