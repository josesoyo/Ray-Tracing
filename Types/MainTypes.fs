﻿namespace Types


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

    type Ray = {
                Wavelenght:Wavelength;
                from: Point; uvec: UnitVector;                 // From and direction
                MaxLength:float<m>                              // Max distance can travell (should be infinite by defect)
                OpticalPathTravelled: float<m>;                   // Optical Path Length Modified after every step with the IOR
                NumBounces: int; mutable bounces: float list;     // Num of bounces + the positions (Just in case for the future)
                NumOfParticles: int;                               // Num of photos -> To split in a Lambertian surface, etc...
                IndexOfRefraction:float
                }

    
    // New intersection type created because I must know which one is the object when the intersected object is a sensor
    type Intersection = { normal:UnitVector; point:Point; ray:Ray;MatName:string ; t:float<m>}//; ObjectSensor:Object Option}
