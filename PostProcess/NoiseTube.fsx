//                      //
//   old, not valid     //
//                      //



// Script to see if the simulation of a tube works
// This script should be done in UNIX style

// reference
// reference external modules
#r @"..\Types\bin\Debug\Types.dll"
#r @"..\RayTracing\bin\Debug\RayTracing.dll"
#r @"..\PostProcess\bin\Debug\PostProcess.dll"
//Math.Net
#r @"..\packages\MathNet.Numerics.3.11.1\lib\net40\MathNet.Numerics.dll"
#r @"..\packages\MathNet.Numerics.FSharp.3.11.1\lib\net40\MathNet.Numerics.FSharp.dll"

// Open referenced
open Types.Algebra
open Types.ObjectTypes
open Types.types
open RayTracing.intersections
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open ForwardRayTracing
open SaveSensorInfo
open Random
open System.IO
open MathNet.Numerics.IntegralTransforms
open System.Numerics
open PostProcess.Noise

let Rtube, Ltube = 0.6<m>, 3e3<m>
let Rmirror = 0.175<m>

let wavelength = WaveLength(1.064e-6<m>)
let BeamWaist = 0.028<m>
let Nrays = 100

// Define the cylinder
let t , Amplitude = [|(0.)..(1./400.)..4.|], Vector(0.,0.,0.15e-6)   // Temporal series for the phase scan
let Amplitude_big =  Vector(0.,0.,2e-6)
let noise = ([|(10.,Amplitude,0)|], t)    // (f,) 
let noise_upconversion = ([|(10.,Amplitude_big,0.)|], t)
let tube = cylinder(Rtube,Ltube,Point(0.,0.,0.),UnitVector(1.,0.,0.),"Tube", Sensor(), noise_upconversion)

// define the mirrors
let m1 = disc(Point(0.,0.,0.), float Rmirror, UnitVector(1.,0.,0.),"Mirror") 
let m2 = disc(Point(float Ltube,0.,0.), float Rmirror, UnitVector(-1.,0.,0.),"Mirror", true)
let objs = [|Cylinder(tube); Disc(m1);Disc(m2)|]

let mout = [|{MatName= "Mirror"; R=1.; T=0.;
                 n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 1e-6};
             {MatName= "Tube"; R=1.; T=0.0; 
                 n=(1.3,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.};
             {MatName= "air"; R=0.; T=1.; 
                 n=(1.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.}
             |]

let mat = dict (mout|> Array.map(fun x -> (x.MatName,x)))
let NewRay (pos:Point) (normal:UnitVector) (sigma:float) (rMax:float) =
    let rotpoint = Matrix.RotateVector(UnitVector(0.,0.,1.),normal)
    let rec rfPos() =  // position function
        let orig = Samp2DGauss(sigma,0.) |> fun x -> rotpoint.RotatePoint(Point(x.[0],x.[1],0.))
        if orig.ToVector().Module() > rMax then rfPos()
        else
            orig |> fun px -> px.MoveAndCreateNew(pos) 
    let rPos = rfPos()
    let rvect = // direction
        let theta = inv_sqr()
        let phi = 2.*PI*rnd.NextDouble()
        let stheta = sin(theta)
        UnitVector(cos(phi)*stheta, sin(phi)*stheta, cos(theta))
        |> fun x -> rotpoint.RotateVector(x)
    {
         Wavelenght = wavelength;
         from = rPos; uvec = rvect;
         MaxLength = infi;
         OpticalPathTravelled = 0.<m>;
         NumBounces = 0.; bounces = [];
         MaxDispersions = 3.;
         NumOfParticlesCreated= 1;
         FracOfRay = 1.;
         IndexOfRefraction = 1.
         PhaseModulation = [||]
    }
let ps0 = Point(0.,0.,0.)
let dir0 = UnitVector(1.,0.,0.)

// Function to generate the rays
let ray() = NewRay ps0 dir0 (float(BeamWaist)) (float(Rmirror))
let path_save = @"Vibration1_sin_upconversion.txt"
//File.WriteAllLines( path_save, [|"1.33";"3.522"|]) // check if the writting will be ok

[|1..Nrays|] |> Array.iter(fun x -> printfn "\n\nNray\n\n"
                                    ForwardRay(ray(),objs,mat,0) ) // ok, look like works
// Save the phase
let ph = m2.Sensor.SavedData.[0].Noise |> Array.map(fun c -> string(c))

// m2.Sensor.SavedData

let phnoise = NoiseInterferometerArm m2 tube mat (ray()) 8e5 // transform the phase into noise
File.WriteAllLines( path_save, phnoise |> Array.map(fun x -> string(x)) ) // Save the data to later plot it in my laptop

