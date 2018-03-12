module rw

open Types
open Types.types
open Types.ObjectTypes
open System.IO

let writeSensorInfo (filePath:string) (data:seq<SensorContent>) (nrays:int)=
    // Write the info into a file - Original function was on '45degMirrorNoise.fsx'
    if Seq.isEmpty data then ()  // do not create the file if it's empty!
    else
        if File.Exists(filePath) then
            let sw = new StreamWriter(filePath,true)
            sw.WriteLine("0.0 1000.0")
            data |> Seq.iter(fun i -> sw.WriteLine(string(i.FracOfRay)+" "+string(i.Position.X)+" "+ string(i.Position.Y)+ " "+string(i.Position.Z)+ " "+string(i.Direction.X)+" "+ string(i.Direction.Y)+ " "+string(i.Direction.Z)+" "+string(i.Phase)))
            sw.Close()
        else
            let sw = new StreamWriter(filePath,true)
            sw.WriteLine("#Info about the file is on the first line, data after 3rd file, the number of rays of the simulation were: "+string(nrays))
            sw.WriteLine("#Fraction  PosX  PosY  PosZ  VecX  VecY  VecZ  Phase")
            data |> Seq.iter(fun i -> sw.WriteLine(string(i.FracOfRay)+" "+string(i.Position.X)+" "+ string(i.Position.Y)+ " "+string(i.Position.Z)+ " "+string(i.Direction.X)+" "+ string(i.Direction.Y)+ " "+string(i.Direction.Z)+" "+string(i.Phase)))
            sw.Close()

let writeSensorInfoModulation (filePath:string) (data0: seq<SensorContent>) =  //
    // Write the modulation of each ray into a file - Original function was on '45degMirrorNoise.fsx'
    if Seq.isEmpty data0 then ()  // do not create the file if it's empty!
    else
        if File.Exists(filePath) then
            let sw = new StreamWriter(filePath,true)
            sw.WriteLine("0.0 1000.0")
            printfn "I use a 1000 as number of rays used in the simulation, please, change it if it is necessary"
            data0 |>
            Seq.iter( fun no -> sw.WriteLine( "0"+(no.Noise  |> Array.fold (fun i d -> i+" "+string(d)) "0" )))
            sw.Close()
        else
            let sw = new StreamWriter(filePath,true)
            sw.WriteLine("Modulation for these rays: ")
            sw.WriteLine("Modulation")
            //sw.WriteLine("0.0")
            data0  |>
            Seq.iter( fun no -> sw.WriteLine( "0"+(no.Noise  |> Array.fold (fun i d -> i+" "+string(d)) "0" )))
            sw.Close()

 
let writeSensorCount (filePath:string) (data:(string*int)[]) (nrays:int) =
    // Write the info into a file - Original function was on '45degMirrorNoise.fsx'
    if Seq.isEmpty data then ()  // do not create the file if it's empty!
    else
        let sw = new StreamWriter(filePath)
        sw.WriteLine("#Info about the file is on the first line, data after 3rd file, the number of rays of the simulation were: "+string(nrays))
        sw.WriteLine("#SensorName counts")
        data |> Seq.iter(fun i -> sw.WriteLine((fst i)+" "+string(snd(i))))
        sw.Close()


let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
    sr.Close()
            }
let readSensorInfo (filePath:string) =
    // read the file and transform it into numerical    -  - Original function on '45degMirrorNoise.fsx'
    let infoRead = readLines filePath
    (infoRead) |> Seq.map(fun x -> x.Split(' ') ) 
    |> Seq.skip 2 //fun x -> x.[2..x.Length-1]                             // The two first lines are the headers  
    |> Seq.map(fun x -> x|> Seq.map(fun y -> float(y)))               // Transform the strings into floats [line].[value]

let readSensorMod (filePath:string) =
    // read the file and transform it into numerical    -  - Original function on '45degMirrorNoise.fsx'
    let infoRead = readLines filePath 
                   |> Seq.skip 2                            // The two first lines are the headers  
                   |> Seq.toArray
    (infoRead) |> Array.map(fun x -> x.Split(' ') ) 
    |> Array.map(fun x -> x|> Array.map(fun y -> float(y))  )               // Transform the strings into floats [line].[value]

