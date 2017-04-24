// do the ray tracing and etc
//  Comments:
//      - 

// just open the libraries to define the objects
//#r @"../Types/bin/Debug/Types.dll"
//#r @"../RayTracing/bin/Debug/RayTracing.dll"
//#r @"../BackwardRender/bin/Debug/BackwardRender.dll"
open Types.Algebra
open Types.ObjectTypes
open TypesStruct
open Types.types
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Random
open ForwardRayTracing
open PostProcess.Noise
open System.IO
open System

//#load "../scripts_examples/mat_lib.fsx"
open materials

 
//#load "Mounts.fsx"
open mounts
//#load "SNEB.fsx"
open SNEB

let ParticlesPerRay = 10


let PointW2L (p:Point) (origPosToVect:Vector) (rotMat:Matrix) =
    // transform a position from global coordinates to local coordinates
    let pn = p+(-1.)*origPosToVect
    rotMat.RotatePoint(pn)
  
let PointL2W (p:Point) (worldOrigigntoVect:Vector) (rotMat:Matrix) =
    // transform a position from global coordinates to local coordinates
    let pn = rotMat.RotatePoint(p)
    pn+(1.)*worldOrigigntoVect
    
   
let Point_remove_z_component (p:Point, origPosToVect:Vector, rotMat_To_Local:Matrix, rotMat_To_World:Matrix) = 
    // When I want to transform from a spherical surface to a disc I have the problem that the Z0 <> 0 and it produces problems on PostProcess
    // with that function I solve that problem
    let nwPoint = PointW2L p origPosToVect rotMat_To_Local
    let nwPint_z0 = Point(nwPoint.X,nwPoint.Y,0.)
    PointL2W nwPint_z0 origPosToVect rotMat_To_World
let VectorW2L (v:UnitVector) =
    // Transform a vector from local coordinates to global coordinates    
    Matrix.RotateVector(v,UnitVector(0.,0.,1.))      


// stray light from MMT2
//      point, direction = SNEB_MMT_M2p, SNEB_MMT_M2d
let RayFromSource (source:Source) (nOfParticlesDispersed:int) = //(maxInputPower:float)=    // SingleFreqNoiseAdd(ray:Ray,inter:Intersection,ns:noise)
    // generate random rays following:
    //      - gausian distribution for the profile 
    //      - Lambertian for the direction (Check it)
    //  waist: diameter
    //  if the surface is spherical, then modify the origin and normal based on the sph equation 
    //          if RoC > 0. the lens is convex, RoC < 0. is concave, otherwise (RoC=0) is not defined
    //  The normal is always the direction in which the rays are generated
    //
    //  maxInputPower = defines the normalization of the energy to say which value is equal to 1
    let pos = source.Position
    let normal = source.Direction
    let sigma = source.Radius_beam*2.
    let rMax = source.Diameter/2.
    let power = source.Power      // in Watts
    // See with the radius of curvature if it is an spherical lens or not
    let RoC = match source.IsSphere with
              | IsSphere x -> x
              | Other x -> infinity
    //printfn "The ROC is: %+A" RoC
    let rotpoint = Matrix.RotateVector(UnitVector(0.,0.,1.),normal)
    let rec rfPos() =  // position function on local coordinates
        let orig = match RoC with
                   | x when x = infinity -> Samp2DGauss(sigma,0.) |> fun x -> Point(x.[0],x.[1],0.)
                   | x when x <> infinity && x > 0. ->  
                       // Case the surface is spherical
                       let p0 = Samp2DGauss(sigma,0.) |> fun x -> Point(x.[0],x.[1],0.)
                       let z0 = -sqrt(RoC*RoC-p0.X*p0.X-p0.Y*p0.Y)
                       Point(p0.X,p0.Y,z0)
                   | x when x <> infinity && x <= 0. ->  
                       // Case the surface is spherical and concave
                       let p0 = Samp2DGauss(sigma,0.) |> fun x -> Point(x.[0],x.[1],0.)
                       let z0 = sqrt(RoC*RoC-p0.X*p0.X-p0.Y*p0.Y)
                       Point(p0.X,p0.Y,z0)
        if (orig.X*orig.X+orig.Y*orig.Y) > rMax*rMax then rfPos()
        else
            orig

    let rPos = rfPos()   // Careful, it's on LOCAL COORDINATES!
    let rvect = match rPos.Z with
                | 0. ->
                    // flat surface
                    UnitVector (SampUnitHemisphereToCart()) // direction on coordinates (0., 0., 1.)
                    |> fun x -> rotpoint.RotateVector(x)
                | _  ->
                    let mat2 =  Matrix.RotateVector(normal, (rPos-Point(0.,0.,-RoC)).ToUnitVector() )
                    UnitVector (SampUnitHemisphereToCart())// direction
                    |> fun x ->  mat2.RotateVector(x)      // rotate to the lens point normal
                    |> fun x -> rotpoint.RotateVector(x)   // rotate to flat surface

    let phase_meters =((source.Phase)/360.*1.064e-6+(rPos.X*rPos.X+rPos.Y*rPos.Y)/(2.*source.RadiusOfCurvature))*1.<m>       // phase of the initial ray in meters: centre + displacement from the centre
    {
         Wavelenght = WaveLength(1.064e-6<m>);
         from =  (rotpoint.RotatePoint(rPos)|> fun px -> px.MoveAndCreateNew(pos)) ;  uvec = rvect;
         MaxLength = infi;
         OpticalPathTravelled = phase_meters;
         NumBounces = 1.//0uy;   It starts at the first dispersion!
         Memory = [||];
         MaxDispersions = 3.//3uy;
         NumOfParticlesCreated = nOfParticlesDispersed;
         FracOfRay = sqrt(power) //maxInputPower;    // 1mW is the unity
         IndexOfRefraction = 1.
         PhaseModulation = [||]
    }
   
let NewRay (pos:Point) (normal:UnitVector) (sigma:float) (rMax:float) (nOfParticlesDispersed:int)=
    // generate random rays following:
    //      - gausian distribution for the profile 
    //      - Lambertian for the direction (Check it)
    //      - The phase of the initial beam is not included
    let rotpoint = Matrix.RotateVector(UnitVector(0.,0.,1.),normal)
    let rec rfPos() =  // position function
        let orig = Samp2DGauss(sigma,0.) |> fun x -> Point(x.[0],x.[1],0.)
        if orig.ToVector().Module() > rMax then rfPos()
        else
            let rp = rotpoint.RotatePoint(orig)
            rp |> fun px -> px.MoveAndCreateNew(pos) 
    let rPos = rfPos()
    let rvect = UnitVector (SampUnitHemisphereToCart())// direction
               |> fun x -> rotpoint.RotateVector(x)
    {
         Wavelenght = WaveLength(1.064e-6<m>);
         from = rPos; uvec = rvect;
         MaxLength = infi;
         OpticalPathTravelled = 0.<m>;
         NumBounces = 1.//0uy;      It starts at the first dispersion!
         Memory = [||];
         MaxDispersions = 3.//3uy;
         NumOfParticlesCreated = nOfParticlesDispersed;
         FracOfRay = 1.;
         IndexOfRefraction = 1.
         PhaseModulation = [||]
    }


[<EntryPoint>]
let main args = 
    // the source num will nbe an input of the programm
    let source_Num =  int(args.[0])  // 2
    //printfn "I have chosen that the ray is terminated at a fraction of 1E-09 because:\nI work with field and the first dispersion is not counted"
    //printfn "the number of surfaces that produce difuse light are %d" (sources.Length)
    //let source_Num = (Console.Read() )
    printfn "The dispersing surface is: %d, called %s" source_Num sources.[source_Num].Label
    let Nrays = 100000 //0
    printfn "The number of rays chosen are: %d" Nrays
    let ray() =RayFromSource sources.[source_Num] ParticlesPerRay //sources.[0].Power
    //#time
    
                        //----------------------------//
                        //                            //
                        //        Ray Trace           //    
                        //                            //
                        //----------------------------//
    // DC part
    [|1..Nrays|] |> Array.iter(fun x ->  if x%5000 = 0 then printfn "ray number %d" x   // check that the simulation is progressing  if x%20 = 0 then
                                         ForwardRay(ray(),elem,mat,0) )   // ok, look like works

    //                                                                       //
    //                                                                       //
    //                                                                       //
    // open the sensors to ray trace the data they have  and compute the noise
    //let nelem = ETM.Sensor.SavedData  // to have the values of the components of the bench

    let out_etm =
          ETM.Sensor.SavedData.ToArray() |>
          Array.map(fun content -> let ns = ForwardRay_noise(elem,mat,content.Route,[||])  
                                   SensorContent(content.Position,content.Direction,content.FracOfRay,content.Phase,ns,content.Route)
                                   // return the sensor content with the noise computed.
                      )
                      |> fun xx -> let new_res_arr = ResizeArray<SensorContent>(xx.Length) 
                                   xx |> Array.iter(fun yy -> new_res_arr.Add(yy))
                                   new_res_arr              
    // create an equivalent disc for the curved surface of the ETM
    
    let ETM_in_disc = disc(ETM_in.Origin,(float ETM_in.ClearAperture)/2.,ETM_in.Axis,ETM_in.MaterialName,ETM_in.Sensor,ETM_in.Noise)
   
    // to transform the z0 to 0 on local coordinates for ETM_in:
    let w2l = Matrix.RotateVector(ETM_in.Axis,UnitVector(0.,0.,1.))
    let l2w = Matrix.RotateVector(UnitVector(0.,0.,1.),ETM_in.Axis)
    let orig_vec = ETM_in.Origin.ToVector()
    let out_etm_in =
          ETM_in_disc.Sensor.SavedData.ToArray() |>
          Array.map(fun content -> let ns = ForwardRay_noise(elem,mat,content.Route,[||])  
                                   let new_point_noZ0 = Point_remove_z_component(content.Position,orig_vec,w2l,l2w)   // delete z0 component on local coordinatesx
                                   SensorContent(new_point_noZ0 ,content.Direction,content.FracOfRay,content.Phase,ns,content.Route)
                                   // return the sensor content with the noise computed.
                      )                               
                      |> fun xx -> let new_res_arr = ResizeArray<SensorContent>(xx.Length) 
                                   xx |> Array.iter(fun yy -> new_res_arr.Add(yy))
                                   new_res_arr              
    let out_pd1 =
          match SNEB_PD1.[4] with Disc x -> x.Sensor.SavedData.ToArray() |>
                                            Array.map(fun content -> let ns = ForwardRay_noise(elem,mat,content.Route,[||])  
                                                                     SensorContent(content.Position,content.Direction,content.FracOfRay,content.Phase,ns,content.Route)
                                                                     // return the sensor content with the noise computed.
                                                        )  
                                            |> fun xx -> let new_res_arr = ResizeArray<SensorContent>(xx.Length) 
                                                         xx |> Array.iter(fun yy -> new_res_arr.Add(yy))
                                                         new_res_arr              

    let out_pd2 =
          match SNEB_PD2.[4] with Disc x -> x.Sensor.SavedData.ToArray() |>
                                            Array.map(fun content -> let ns = ForwardRay_noise(elem,mat,content.Route,[||])  
                                                                     SensorContent(content.Position,content.Direction,content.FracOfRay,content.Phase,ns,content.Route)
                                                                    // return the sensor content with the noise computed.
                                                        )                               
                                            |> fun xx -> let new_res_arr = ResizeArray<SensorContent>(xx.Length) 
                                                         xx |> Array.iter(fun yy -> new_res_arr.Add(yy))
                                                         new_res_arr              

    let out_cam1 =
          match SNEB_CAM1.[4] with Disc x -> x.Sensor.SavedData.ToArray() |>
                                             Array.map(fun content -> let ns = ForwardRay_noise(elem,mat,content.Route,[||])  
                                                                      SensorContent(content.Position,content.Direction,content.FracOfRay,content.Phase,ns,content.Route)
                                                                     // return the sensor content with the noise computed.
                                                        )                               
                                            |> fun xx -> let new_res_arr = ResizeArray<SensorContent>(xx.Length) 
                                                         xx |> Array.iter(fun yy -> new_res_arr.Add(yy))
                                                         new_res_arr              
    let out_cam2 =
          match SNEB_CAM2.[4] with Disc x -> x.Sensor.SavedData.ToArray() |>
                                             Array.map(fun content -> let ns = ForwardRay_noise(elem,mat,content.Route,[||])  
                                                                      SensorContent(content.Position,content.Direction,content.FracOfRay,content.Phase,ns,content.Route)
                                                                    // return the sensor content with the noise computed.
                                                        )                               
                                            |> fun xx -> let new_res_arr = ResizeArray<SensorContent>(xx.Length) 
                                                         xx |> Array.iter(fun yy -> new_res_arr.Add(yy))
                                                         new_res_arr              
    let out_4d2 =
          match SNEB_4Q2.[4] with Disc x -> x.Sensor.SavedData.ToArray() |>
                                            Array.map(fun content -> let ns = ForwardRay_noise(elem,mat,content.Route,[||])  
                                                                     SensorContent(content.Position,content.Direction,content.FracOfRay,content.Phase,ns,content.Route)
                                                                    // return the sensor content with the noise computed.
                                                        )                               
                                            |> fun xx -> let new_res_arr = ResizeArray<SensorContent>(xx.Length) 
                                                         xx |> Array.iter(fun yy -> new_res_arr.Add(yy))
                                                         new_res_arr              
    let out_4d1 =
          match SNEB_4Q1.[4] with Disc x -> x.Sensor.SavedData.ToArray() |>
                                            Array.map(fun content -> let ns = ForwardRay_noise(elem,mat,content.Route,[||])  
                                                                     SensorContent(content.Position,content.Direction,content.FracOfRay,content.Phase,ns,content.Route)
                                                                    // return the sensor content with the noise computed.
                                                        )                               
                                            |> fun xx -> let new_res_arr = ResizeArray<SensorContent>(xx.Length) 
                                                         xx |> Array.iter(fun yy -> new_res_arr.Add(yy))
                                                         new_res_arr              
            
              
    //(sneb_etm_sensor , match SNEB_PD1.[4] with Disc x -> x.Sensor.SavedData)  ||> Seq.map2(fun a b -> a.Noise=b.Noise)                                             
    //                                                                       //
    //                                                                       //
    //                                                                       //

                        //----------------------------//
                        //                            //
                        //       See output           //    This is always true
                        //                            //
                        //----------------------------//


    //let out_pd1_ = match SNEB_PD1.[4] with Disc x -> x.Sensor.SavedData.ToArray() | _ -> failwith " Error on the type" 
    //let out_pd2 = match SNEB_PD2.[4] with Disc x -> x.Sensor.SavedData.ToArray() | _ -> failwith " Error on the type" 
    //(out_pd1_,out_pd1) ||> Array.map2(fun un tw -> un.Noise=tw.Noise) |> Array.filter(fun x -> x) 
    //|> fun x -> if x.Length=out_pd1.Length then printfn "yah" elif x.Length>1 then printfn "uh" else printfn "bad"


    //let out_cam1 = match SNEB_CAM1.[4] with Disc x -> x.Sensor.SavedData.ToArray() | _ -> failwith " Error on the type" 
    //let out_cam2 =match  SNEB_CAM2.[4] with Disc x -> x.Sensor.SavedData.ToArray()| _ -> failwith " Error on the type" 

    //let out_4d2 = match SNEB_4Q2.[4] with Disc x -> x.Sensor.SavedData.ToArray() | _ -> failwith " Error on the type"  //.[2].FracOfRay
    //let out_4d1 = match SNEB_4Q1.[4] with Disc x -> x.Sensor.SavedData.ToArray() | _ -> failwith " Error on the type" //.[0].FracOfRay

                        //----------------------------//
                        //                            //
                        //  Write and Read to a File  //    No modulation up to now
                        //                            //
                        //----------------------------//
    

    let writeSensorInfo (filePath:string) (data:seq<SensorContent>) =
        // Write the info into a file - Original function on '45degMirrorNoise.fsx'
        let sw = new StreamWriter(filePath)
        sw.WriteLine("Info about the file is on the first line, data after 3rd file, the number of rays of the simulation were: "+string(Nrays))
        sw.WriteLine("Fraction  PosX  PosY  PosZ  VecX  VecY  VecZ  Phase")
        data |> Seq.iter(fun i -> sw.WriteLine(string(i.FracOfRay)+" "+string(i.Position.X)+" "+ string(i.Position.Y)+ " "+string(i.Position.Z)+ " "+string(i.Direction.X)+" "+ string(i.Direction.Y)+ " "+string(i.Direction.Z)+" "+string(i.Phase)))
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
  

    let sensors = [|out_pd1; out_pd2; out_cam1; out_cam2; out_4d1 ; out_4d2|]   // info on each sensor
    let SensorNameList = [|"PD1";"PD2"; "cam1";"cam2"; "4Q1"; "4Q2"; "ETM" ;"ETM_in"|]          // names of the sensors                                            
    let SourceName = "main/outSensor/"+sources.[source_Num].Label                                     // this is the folder and the source, the SOURCE Will be added in the future automatically!!
    let dirs = SensorNameList                                        // Path for each sensor
               |> Array.map(fun x -> SourceName+  "__"+x+".dat" ) 
               //|> Array.map (fun x -> Path.Combine(__SOURCE_DIRECTORY__, x) )
    //dirs |> Array.iter(fun x -> (printfn "%s\n" x))
    // Save the data on files, 1 for each source
    //(sensors, dirs) ||> Seq.iter2(fun x y -> writeSensorInfo y x)

    // read test
    //readSensorInfo ((Seq.toArray  dirs).[0])


    //----------------------//
    //                      //
    //    Write Locally     //    add the modulation on the elements
    //                      //
    //-------- -------------//

    let pd1 = 
        let dd =
            match SNEB_PD1.[4] with Disc x -> x | _ -> failwith " Error on the type" 
        dd.Sensor.SavedData <- out_pd1
        dd
    let pd2 =
        let dd =
             match SNEB_PD2.[4] with Disc x -> x | _ -> failwith " Error on the type" 
        dd.Sensor.SavedData <- out_pd2
        dd

    let cam1 = 
        let dd =   
            match SNEB_CAM1.[4] with Disc x -> x  | _ -> failwith " Error on the type" 
        dd.Sensor.SavedData <- out_cam1
        dd
    let cam2 =
        let dd =      
            match  SNEB_CAM2.[4] with Disc x -> x  | _ -> failwith " Error on the type" 
        dd.Sensor.SavedData <- out_cam2
        dd
 
    let Q4d2 = 
        let dd =   
            match SNEB_4Q2.[4] with Disc x -> x | _ -> failwith " Error on the type"  
        dd.Sensor.SavedData <- out_4d2
        dd
    let Q4d1 = 
        let dd =   
            match SNEB_4Q1.[4] with Disc x -> x | _ -> failwith " Error on the type" 
        dd.Sensor.SavedData <- out_4d1
        dd
    ETM.Sensor.SavedData <- out_etm        // also add the noise for the mirror sensors
    ETM_in_disc.Sensor.SavedData <- out_etm_in

    
    //let s_centres = [| pd1.Centre; pd2.Centre; cam1.Centre; cam2.Centre; Q4d1.Centre; Q4d2.Centre|]                  // centre of each sensor
    let s_dirs= [| pd1.Normal; pd2.Normal; cam1.Normal; cam2.Normal; Q4d1.Normal; Q4d2.Normal; ETM.Normal;ETM_in_disc.Normal|]          // direction of each sensor
    let s_rot = s_dirs |> Array.map(fun x -> Matrix.RotateVector(x, UnitVector(0.,0.,1.)) )                          // rotation matrix for each sensor


    
    // Transform sensor data in world coordintates into local coordinates

    let out_pd1_l = out_pd1 |> Seq.map(fun x ->SensorContent( (PointW2L x.Position (pd1.Centre.ToVector()) s_rot.[0]),
                                                              VectorW2L(pd1.Normal).RotateVector(x.Direction),
                                                              x.FracOfRay,
                                                              x.Phase,x.Noise,x.Route )   )

    let out_pd2_l = out_pd2 |> Seq.map(fun x ->SensorContent( (PointW2L x.Position (pd2.Centre.ToVector()) s_rot.[1]),
                                                              VectorW2L(pd2.Normal).RotateVector(x.Direction),
                                                              x.FracOfRay,
                                                              x.Phase,x.Noise,x.Route )   )

    let out_cam1_l = out_cam1 |> Seq.map(fun x ->SensorContent( (PointW2L x.Position (cam1.Centre.ToVector()) s_rot.[2]),
                                                              VectorW2L(cam1.Normal).RotateVector(x.Direction),
                                                              x.FracOfRay,
                                                              x.Phase,x.Noise,x.Route )   )

    let out_cam2_l = out_cam2 |> Seq.map(fun x ->SensorContent( (PointW2L x.Position (cam2.Centre.ToVector()) s_rot.[3]),
                                                              VectorW2L(cam2.Normal).RotateVector(x.Direction),
                                                              x.FracOfRay,
                                                              x.Phase,x.Noise ,x.Route)   )

    let out_4d1_l = out_4d1 |> Seq.map(fun x ->SensorContent( (PointW2L x.Position (Q4d1.Centre.ToVector()) s_rot.[4]),
                                                              VectorW2L(Q4d1.Normal).RotateVector(x.Direction),
                                                              x.FracOfRay,
                                                              x.Phase,x.Noise, x.Route )   )
    let out_4d2_l = out_4d2 |> Seq.map(fun x ->SensorContent( (PointW2L x.Position (Q4d2.Centre.ToVector()) s_rot.[5]),
                                                              VectorW2L(Q4d2.Normal).RotateVector(x.Direction),
                                                              x.FracOfRay,
                                                              x.Phase,x.Noise, x.Route )   )


    let ETM_l = out_etm |> Seq.map(fun x ->SensorContent( (PointW2L x.Position (ETM.Centre.ToVector()) s_rot.[6]),
                                                                                    VectorW2L(ETM.Normal).RotateVector(x.Direction),
                                                                                    x.FracOfRay,
                                                                                    x.Phase,x.Noise, x.Route )   )
    let ETM_l_2 = out_etm_in |> Seq.map(fun x ->SensorContent( (PointW2L x.Position (ETM.Centre.ToVector()) s_rot.[7]),
                                                                                              VectorW2L(ETM.Normal).RotateVector(x.Direction),
                                                                                              x.FracOfRay,
                                                                                              x.Phase,x.Noise, x.Route )   )
    let sensors_local = [|out_pd1_l; out_pd2_l; out_cam1_l; out_cam2_l; out_4d1_l ; out_4d2_l; ETM_l;ETM_l_2|]   // info on each sensor
    // Save the data on files, 1 for each source - Now locally
    (sensors_local, dirs) ||> Seq.iter2(fun x y -> writeSensorInfo y x)
                                                     
    //
    //   Print the % of photons arriving
    //
    let field_ray_init = sqrt(sources.[source_Num].Power) 
    printfn "The total percent of received ligth from the dispersed is:"
    printfn "Fraction on pd1:            %f " ((out_pd1 |> Seq.sumBy(fun x -> x.FracOfRay))*100./(float Nrays*field_ray_init))
    printfn "Fraction on pd2:            %f " ((out_pd2 |> Seq.sumBy(fun x -> x.FracOfRay))*100./(field_ray_init*float Nrays))
    printfn "Fraction on cam1            %f" ((out_cam1 |> Seq.sumBy(fun x -> x.FracOfRay))*100./(field_ray_init*float Nrays))
    printfn "Fraction on cam2            %f" ((out_cam2 |> Seq.sumBy(fun x -> x.FracOfRay))*100./(field_ray_init*float Nrays))
    printfn "Fraction on quadrant1       %f" ((out_4d1 |> Seq.sumBy(fun x -> x.FracOfRay))*100./(field_ray_init*float Nrays))
    printfn "Fraction on quadrant2       %f" ((out_4d2 |> Seq.sumBy(fun x -> x.FracOfRay))*100./(field_ray_init*float Nrays))
    printfn "Fraction arriving to ETM is %f" ((ETM_l |> Seq.sumBy(fun x -> x.FracOfRay))*100./(field_ray_init*float Nrays))
    printfn "Fraction arriving to ETM_in is %f" ((ETM_l_2 |> Seq.sumBy(fun x -> x.FracOfRay))*100./(field_ray_init*float Nrays))


    
                        //----------------------------//
                        //                            //
                        //     Fourier analysis       //    This is always true
                        //                            //
                        //----------------------------//
     
    let SensorNameList2 = [|"asd_PD1";"asd_PD2"; "asd_cam1";"asd_cam2"; "asd_4Q1"; "asd_4Q2"; "asd_ETM"; "asd_ETM_in" |]          // names of the sensors                                            
    let SourceName_asd = "main/outSensor_asd/"+sources.[source_Num].Label                                     // this is the folder and the source, the SOURCE Will be added in the future automatically!!
    let dirs2 = SensorNameList2                                        // Path for each sensor
                |> Array.map(fun x -> SourceName_asd+  "__"+x+".dat" ) // |> Seq.toArray
                //|> Array.map (fun x -> Path.Combine(__SOURCE_DIRECTORY__, x) )
  
   
    //PhaseChange(pd1,)   sources.[source_Num]
    let windowsLength = 200
    printfn "do pd2"
    PhaseChange(pd2,SNEB_PD2_Source, sources.[source_Num].Dispersion,Nrays, windowsLength,    dirs2.[1])
    printfn "do pcam2"
    PhaseChange(cam2,SNEB_CAM2_Source, sources.[source_Num].Dispersion,Nrays, windowsLength,  dirs2.[3] )
    printfn "do Q4d1"
    PhaseChange_QPD(Q4d1,SNEB_4Q1_Source,sources.[source_Num].Dispersion,Nrays, windowsLength,dirs2.[4] )
    // End mirror
    printfn "do ETM"
    PhaseChange(ETM,ETM_Source, sources.[source_Num].Dispersion, Nrays, windowsLength,dirs2.[6] )
    printfn "do ETM_in"
    PhaseChange(ETM_in_disc,ETM_in_Source, sources.[source_Num].Dispersion, Nrays,windowsLength,dirs2.[7] )
    // cameras whose power is 0
    printfn "do pd1"
    Phase_NoiseAlone(pd1, sources.[source_Num].Dispersion, Nrays, windowsLength,dirs2.[0])
    printfn "do cam1"
    Phase_NoiseAlone(cam1,sources.[source_Num].Dispersion, Nrays, windowsLength,dirs2.[2])
    printfn "do Q4d2"
    PhaseChange_NoPow_QPD(Q4d2, sources.[source_Num].Dispersion,Nrays, windowsLength,dirs2.[5])
 

    // Decentering, I will choose on X 0.1, 0.2 and 0.3
    printfn "do start decentre Q4d1"
    PhaseChange_QPD_decentreX(Q4d1,SNEB_4Q1_Source,sources.[source_Num].Dispersion,Nrays, windowsLength,0.1, dirs2.[4] )
    PhaseChange_QPD_decentreX(Q4d1,SNEB_4Q1_Source,sources.[source_Num].Dispersion,Nrays, windowsLength,0.2, dirs2.[4] )
    PhaseChange_QPD_decentreX(Q4d1,SNEB_4Q1_Source,sources.[source_Num].Dispersion,Nrays, windowsLength,0.3, dirs2.[4] )

    printfn "do decentre Q4d2"
    PhaseChange_NoPow_QPD_decentreX(Q4d2, sources.[source_Num].Dispersion,Nrays, windowsLength, 0.1, dirs2.[5])
    PhaseChange_NoPow_QPD_decentreX(Q4d2, sources.[source_Num].Dispersion,Nrays, windowsLength, 0.2, dirs2.[5])
    PhaseChange_NoPow_QPD_decentreX(Q4d2, sources.[source_Num].Dispersion,Nrays, windowsLength, 0.3, dirs2.[5])


    printfn "The simulation has finished\n Remember that ETM_in should be multiplied by sqrt(epsilon_T_in) since the light is coupled inside the cavity" //, push a key to close the SW"
    //let ign0re = Console.Read() 
    0

    (*

    //
    // Tests with the output
    //
    open SaveSensorInfo
    let out_4d2_disc = match SNEB_4Q2.[4] with Disc x -> x
    let disc_rad= out_4d2_disc.Radius
    //out_4d2_disc.Normal
    let disc_box = {Pmin=Point(-disc_rad,-disc_rad, 0.); Pmax=Point(disc_rad+1e-15,disc_rad+1e-15,0.)}
    CreateImage_Points_from_disk_amplitude(out_4d2,ray_sneb_mmt_m2().Wavelenght,out_4d2_disc.Normal,out_4d2_disc.Centre,disc_rad,2,2)


    Types.ObjectTypes+SensorContent {Direction = Types.Algebra+UnitVector;
                                       Noise = [||];
                                       NumRays = 1;
                                       Phase = 3.098351236;
                                       Position = Types.Algebra+Point;}
    > 
    *)