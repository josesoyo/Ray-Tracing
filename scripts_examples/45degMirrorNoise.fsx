
#r @"../Types\bin\Debug\Types.dll"
#r @"../RayTracing\bin\Debug\RayTracing.dll"
#r @"../PostProcess\bin\Debug\PostProcess.dll"
//Matet
#r @"../packages\MathNet.Numerics.3.11.1\lib\net40\MathNet.Numerics.dll"
#r @"../packages\MathNet.Numerics.FSharp.3.11.1\lib\net40\MathNet.Numerics.FSharp.dll"
#r @"../packages\FSharp.Charting.0.90.14\lib\net40\FSharp.Charting.dll"
// libraries referenced

open Types.Algebra
open Types.ObjectTypes
open Types.types
open TypesStruct
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open CreateRay
open ForwardRayTracing
open SaveSensorInfo
open SaveSensorInfo
open System.IO
open PostProcess.Noise
open periodogram
open MathNet.Numerics.IntegralTransforms
open System.Numerics
open FSharp.Charting

//          //          //          //          //          //

// define materials
let mout = [|{MatName= "NBK7"; R=0.; T=1.;
                 n=(1.51680,WaveLength(0.5876e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.};
             {MatName= "Mirror"; R=1.; T=0.;
                 n=(1.51680,WaveLength(0.5876e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.};
             |]
let mat = dict(mout |> Array.map(fun x -> (x.MatName,x)))


// define the noise on the scene
let timestamp = [|(0.)..(1./256.)..(2.)|]
let fNoise = [|10.|]                            // Hz
let phNoise  = 
    //let rndom = System.Random()
    //[|rndom.NextDouble()*6.30|]      // random phase
    [|0.|]
let ampNoise = [|Vector(0.,10.5e-6,10.5e-6) |]    // amplitude

// this method works on , but not on IFsharp
let noise = (fNoise,ampNoise,phNoise) |||> Array.map3(fun x y z -> (x,y,z)) // (frequency, Amplitude,phase)[]
// alternative
//let noise =
//     (fNoise,ampNoise) ||> Array.map2(fun x y -> (x,y))
//     |> fun tup -> (tup,phNoise) ||> Array.map2(fun x y -> (fst x, snd x, y))

// Signals on the ray
let f1,f2,f3 = 40.,110.,120.         // frequency
let a1,a2,a3 = 0.051,0.1,0.01        // amplitudes
let ph1,ph2,ph3 = 3., 0.78,2.6       // phase



let initphmod = timestamp |> Array.map (fun t ->a2*sin(2.*3.1415*f2*t+ph2)+a1*sin(2.*3.1415*f1*t+ph1)+a3*sin(2.*3.1415*f3*t+ph3)) //  

(*
printfn "Printing the initial signal on the phase\n\tclose the chart in order to continue running the code"
Chart.Line(([|(0.)..(1./1024.)..(2.)|],initphmod) ||> Array.map2(fun x y -> (x,y)))
|> Chart.WithTitle("PhaseModulation", InsideArea=false)
|> Chart.WithXAxis(Min=0.,Max=0.5)
|> Chart.Show 
*)

//      //      //
// scene
// Define the system with the mirror that has noise
let bilens = biConvex(0.10293<m>,0.10293<m>,UnitVector(0.,0.,1.),2.5e-3<m>,10e-3<m>,Point(0.,0.,0.), "NBK7",Sensor(),([| |],[||]))
10e-3
let focusing_z = 0.09919+2.5e-3//+2.5e-3
let focusing_point =  Point(0.,0.,focusing_z)  // 0.09919+2.5e-3
let reflecting_point = focusing_point
let mirror = disc(focusing_point,0.1,UnitVector(0.,-1.,-1.),"Mirror") //(noise,timestamp))
let dispersion_mirror_point = reflecting_point.MoveAndCreateNew(Point(0.,0.,-1e-6))
let dispersion_mirror = disc(dispersion_mirror_point,0.0051,UnitVector(0.,-1.,-1.),"Mirror",Sensor(),(noise,timestamp)) //(noise,timestamp))

let sensor_point = Point(0.,-0.05,focusing_z)
let sensor = disc(sensor_point,0.5,UnitVector(0.,1.,0.),"",Sensor(true,true),([| |],[||]))
let sistema_2 = Array.append bilens [|Disc(sensor); Disc(mirror); Disc(dispersion_mirror)|]
//
//

//      //      //
// define ray
let ray() = NewRayCollimated (Point(0.,0.,-0.1)) (UnitVector(0.,0.,1.)) (0.01) (0.02) (1) (WaveLength(1.064e-6<m>)) // waveline = Nd
let CollimatedRayWithNoise phmod =
    // define a ray with phase modulation that it is alre    
    let raynew = ray()
    {raynew with PhaseModulation = phmod}

let rn() = CollimatedRayWithNoise initphmod


//////  //////  //////////////////////////////////////////////
let Nrays = 5//000
// Ray tracingfafa
#time
[|1..Nrays|] |> Array.Parallel.iter(fun _ -> lock sensor.Sensor.SavedData (fun () -> ForwardRay(rn (),sistema_2,mat,0) ))
//////  //////  /////////////////////////////////////////////

(*
// things that I can check
(sensor.Sensor.SavedData.[2].Noise, rn().PhaseModulation) ||> Array.map2(fun x y -> x-y)      // difference beween fina ray and original ray
sensor.Sensor.SavedData.Count                                                                 // number of rays hitting the sensor
sensor.Sensor.SavedData.ToArray() |> Array.filter(fun x -> not(Array.isEmpty(x.Noise))) |> fun x -> x.Length
*)

//-----------------------// 
//                       //
//  saving file tests    //
//   Write and Read      //
//-----------------------//
printfn " There are %d photons on the sensor" sensor.Sensor.SavedData.Count
__SOURCE_DIRECTORY__
let dir = Path.Combine(__SOURCE_DIRECTORY__, "test.dat")
sensor.Sensor.SavedData.[0].FracOfRay
// Write a file Line by line creating a format
let writeLines (filePath:string) (data:seq<SensorContent>) =
    //let dir = filePath
    let sw = new StreamWriter(filePath)
    sw.WriteLine("Fr Px Py Pz Ux Uy Yz Ph")
    data |> Seq.iter(fun i -> sw.WriteLine(string(i.FracOfRay)+" "+string(i.Position.X)+" "+ string(i.Position.Y)+ " "+string(i.Position.Z)+ " "+string(i.Direction.X)+" "+ string(i.Direction.Y)+ " "+string(i.Direction.Z)+" "+string(i.Phase)))
    sw.Close()

writeLines dir (sensor.Sensor.SavedData)
// Read a file line by line
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)       // open streamReader
    while not sr.EndOfStream do
        yield sr.ReadLine ()                   // Read a line
    sr.Close()                                 // close streamReadder
            }
let inp = readLines dir
let aaaa = (inp) |> Seq.map(fun x -> x.Split(' ') ) 
           |> Seq.skip 2//fun x -> x.[2..x.Length-1]                            // The two first lines are the headers  
           |> Seq.map(fun x -> x|> Seq.map(fun y -> float(y)))
aaaa.[6]
Seq.
//-------------------// 
//                   //
//  Fourier tests    //
//                   //
//-------------------//
let ASD_Photons (time:float[]) (snrs:Sensor) =
    // function to summ all the frequencies of all the photons
    let data = snrs.SavedData
    let windows_length = 512
    let sum_ASD = 
        data.ToArray() 
        |> Array.filter(fun x -> x.Noise.Length <> 0)
        |> Array.map(fun x ->snd(PSD_WELCH(time,x.Noise |> Array.map(fun x -> sin(x)),"Hann",windows_length,260,0.) ) )
        |> fun asds -> 
            [|0..asds.[0].Length-1|]
            |> Array.map(fun i ->
                             (asds |> Array.sumBy(fun eachone -> eachone.[i])) 
                         )        
    let freq ,_ = PSD_WELCH(time,(data.ToArray()|> Array.filter(fun x -> x.Noise.Length <> 0) |> fun x -> x.[0].Noise),"",windows_length,256,0.)
    (freq, sum_ASD)

let out_asd = ASD_Photons timestamp sensor.Sensor

(*
let minf,maxf = 1./(timestamp.[timestamp.Length-1]-timestamp.[0]), 0.5/(timestamp.[1]-timestamp.[0])    // delta frequency and maximum frequency
let freqs = [|(0.)..minf..maxf|]                                                                        // FFT frequencies
*)

Chart.Line(out_asd ||> Array.map2(fun x y  -> (x,y)) )  |> Chart.WithXAxis(Min=0.,Max=200.)
|> Chart.Show
