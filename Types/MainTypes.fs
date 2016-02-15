namespace Types

(*
module measures =
    // implemented in :
    //Microsoft.FSharp.Data.UnitSystems.SI.UnitNames 
    [<Measure>]  type m 
*)

module types =
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames 
    open MathNet.Spatial.Euclidean // requieres System.Xml


    type Wavelength = float<metre>              //

    type Material = {T: float; R:float;                        // transmission an reflectivity (coating)
                      n:float*Wavelength;                      // Index of Refraction
                      LambPPM:float;                           // Lambertian BRDF in ppm, not considered the 1/2PI
                      Name: string                             // Identifier of the material to find it
                     }

    type Ray = {
                Wavelenght:Wavelength;
                from: Point3D; uvec:UnitVector3D;                 // From and direction
                MaxLength:float<metre>                                // Max distance can travell (should be infinite by defect)
                OpticalPathTravelled: float<metre>;                   // Optical Path Length Modified after every step with the IOR
                NumBounces: int; mutable bounces: float list;     // Num of bounces + the positions (Just in case for the future)
                NumOfParticles: int                               // Num of photos -> To split in a Lambertian surface, etc...
                }


