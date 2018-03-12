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
    // If there is no noise it will be defined as 
    type noise = 
    | Simulated of ((float*Vector*float)[]*float[])                        // Here it refers to the fact that ((frequency, Amplitude,phase)[], t_sampling)
    | RealData of (UnitVector[]*float[]*float[][])                             // Direction X time X movement 
    | Quiet //now requires more implementation, but should be better...

    let extractTimeFromNoise (ns:noise) =
        match ns with
        | Simulated (_,y) -> y
        | RealData (_,y,_) -> y
    let extractDisplacementFromNoise (ns:noise) =
      match ns with
      | Simulated x -> failwith "There is no displacement in Simulated displacement"
      | RealData (_,_, x) -> x
    // byte is a number in the range [0,255], I use this because I expect I won't work on higher frequencies

    // Create the typres required to perform tracking of the 
    type route_ray = {Direction:UnitVector;       // Direction in which the ray is reflected/transmitted/dispersed
                      GoingToID:int;              // Id of the object that hits later. This serves to find where it hits and perform the noise analysis
                      Origin_From:Point;        // I need to know the first point
                      Destination:Point   // Destination, shouldn't be complettelly necessary, but will help tp check that all has been correctly
                      } 
    type Ray = {
                Wavelenght:Wavelength;
                from: Point; uvec: UnitVector;            // From and direction
                MaxLength:float<m>                        // Max distance can travell (should be infinite by defect)
                OpticalPathTravelled: float<m>;           // Optical Path Length Modified after every step with the IOR
                NumBounces:float; //byte;
                MaxDispersions: float;//byte;             // Maximum bounces on dispersive media that can be done
                //mutable bounces: float list;              // Num of bounces + the positions (Just in case for the future)
                NumOfParticlesCreated: int;               // Num of photos -> To split in a Lambertian surface, etc...
                // NumOfParticlesCreated are the number of particles that are created after each dispersion (splitting)
                FracOfRay:float;                          // Fraction of the initial beam that is being traced           
                IndexOfRefraction:float
                Memory: route_ray[]                       // data to compute the route that the photon follows.              
                // Careful, NoiseAdd it's for frequency domain and PhaseModulation for time domain
                // NoiseAdd:float[]     - Not required now
                PhaseModulation: float[]
                // for NoiseAdd, I will need some function in order to sum the noise on the right frequency, but up to now, as a first step I am planning to use a single frequency
                }

    // SourceType
    type IsSPhere =
          | IsSphere of (float)  // ROC positive is convex, negative is concave
          | Other of string //Other of string

    type Source = { Position:Point;
                    Direction:UnitVector;
                    Diameter:float;
                    Waist:float;             // [m]
                    Radius_beam:float;       // [m] radius of the beam
                    Power:float ;            // [W]
                    Dispersion:float;        // fraction of dispersed light
                    Label:string;
                    IsSphere:IsSPhere        // Union type that contains (rad*diameter) or a string to say nothing 
                    RadiusOfCurvature:float  // [m] RoC of the gaussian beam      -> to compute the phase out of the centre
                    Phase:float              // accumulated phase [accumulated + Guoy]        
                    }

    // New intersection type created because I must know which one is the object when the intersected object fsharp a sensor
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
       

