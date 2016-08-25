module SaveSensorInfo

open Types.Algebra
open Types.ObjectTypes
open Types.types
open Random
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
//Here I want to process and save the information on the sensor once the raytracing has finished.
//By that I mean that this part is once all the rays have finished

// the process consist on read all thedata on the array and then find for each position the local coordinates on pixel
let pixNumber (size:BBox) (pixSizeX:float) (pixSizeY:float) (n:int) (m:int) (p:Point) = //(p2:Point)=
    // I'll use the idea of functional programming to subtitute the repeated variables
    if abs p.Z  > 1e-8 then printfn "There's an ERRROR on the sensor when it's translated to zero \nThe value of z is: %f" p.Z
    // Pixel number
    let xpix = int ((p.X - size.Pmin.X) / pixSizeX)
    let ypix = int ((p.Y - size.Pmin.Y) / pixSizeY)
    //printfn "Point: %f;%f;%f" p.X p.Y p.Z
    if xpix > n  then 
        printfn "There's an error on the definition of the pixels because it says it's on pixel %d and the maxPix is %d" xpix n
    if ypix > m  then 
        printfn "There's an error on the definition of the pixels because it says it's on pixel %d and the maxPix is %d" ypix m
    xpix, ypix

// Position sensor, the easy one
let CreateImage_Points_from_disk_phase(data0:seq<SensorContent>,wavelength:Wavelength,size:BBox, rot:UnitVector, center: Point, radius:float, n:int,m:int) =
    // translate and rotate the points to transformthem into a local coordinates
    let data = Seq.toArray data0
    let rotationMat = Matrix.RotateVector(rot,UnitVector(0.,0.,1.))                 //  Rotation
    let Translation = center.ToVector()     //  Translation is the position of the original center
    let points_Local = data |> Array.map(fun x -> let p = (x.Position)+(-1.)*Translation
                                                  let rr = rotationMat.RotatePoint(p) 
                                                  rr )  // array of the points in local cordinates
    let pixSizex = 2.*radius/float n
    let pixSizey = 2.*radius/float m
    // Create the matrix with n*m pixels
    let Sensor_re = Matrix(n,m) // my own matrix type
    let Sensor_im = Matrix(n,m) // my own matrix type

    let wavel = match wavelength with WaveLength x -> float x 
    let phase_Local = data |> Array.map(fun x -> (x.Phase) )

    let pixNumSimp poin = pixNumber size pixSizex pixSizey n m poin // I redefine the function
    (points_Local, phase_Local) ||> Array.iter2(fun a b -> let x, y = pixNumSimp a
                                                           Sensor_re.RotMat.[x,y] <- (Sensor_re.RotMat.[x,y] + 1.*cos(b))
                                                           Sensor_im.RotMat.[x,y] <- (Sensor_im.RotMat.[x,y] + 1.*sin(b))
                                                )
    [|0..Sensor_re.RotMat.GetLength(0)-1|] 
    |> Array.Parallel.iter(fun nx -> 
                    //[|0..Sensor_re.RotMat.GetLength(1)-1|] |> Array.iter(fun ny -> Sensor_re.RotMat.[nx,ny] <-  (Sensor_re.RotMat.[nx,ny]*Sensor_re.RotMat.[nx,ny]+Sensor_im.RotMat.[nx,ny]*Sensor_im.RotMat.[nx,ny]) )
                    [|0..Sensor_re.RotMat.GetLength(1)-1|] |> Array.iter(fun ny -> Sensor_re.RotMat.[nx,ny] <-  (atan2 Sensor_re.RotMat.[nx,ny] Sensor_im.RotMat.[nx,ny])+3.14159265358979323 )
                 )
    Sensor_re


let CreateImage_Points_from_disk(data:SensorContent[],size:BBox, rot:UnitVector, center: Point, radius:float, n:int,m:int) =
    // translate and rotate the points to transformthem into a local coordinates
    let rotationMat = Matrix.RotateVector(rot,UnitVector(0.,0.,1.))                 //  Rotation
    let Translation = center.ToVector()     //  Translation is the position of the original center
    let points_Local = data |> Array.map(fun x-> rotationMat.RotatePoint  ((x.Position)+(-1.)*Translation)  )  // array of the points in local cordinates

    let pixSizex = 2.*radius/float n
    let pixSizey = 2.*radius/float m
    // Create the matrix with n*m pixels
    let Sensor = Matrix(n,m) // my own matrix type

    let pixNumSimp poin = pixNumber size pixSizex pixSizey n m poin // I redefine the function
    points_Local |> Array.iter(fun x -> let x, y = pixNumSimp x
                                        Sensor.RotMat.[x,y] <- Sensor.RotMat.[x,y] + 1.
                               )
    Sensor
    // now I need to create the bmp and show it or save it

open System.IO
open System.Windows.Forms
open System.Drawing
  
let SensorToImage(sensorMat:Matrix,path:string, n:int,m:int) =
    // Transform the RGB of the sensor into a true RGB for a BMP
    let bmp = new Bitmap(n,m)
    
    let mutable maxVal = 0.
    [|0..n-1|] |>Array.iter (fun x ->           // Find the maximum value of the sensor
                                [|0..m-1|] |> Array.iter(fun y -> 
                                                            if sensorMat.RotMat.[x,y] > maxVal then maxVal <- sensorMat.RotMat.[x,y] 
                                                            else 'c' |> ignore
                                                            )
                             )
    let maxint = maxVal
    for i in 0..(n-1) do
        for j in 0..(m-1) do
            bmp.SetPixel(i,j,Color.FromArgb(min 255 (int(255.*sensorMat.RotMat.[i,j]/maxint)),  
                                            min 255 (int(255.*sensorMat.RotMat.[i,j]/maxint)),
                                            min 255 (int(255.*sensorMat.RotMat.[i,j]/maxint)) )
                                            )    
    // Save the image
    bmp.Save(path)
    // create the form to show the output
    let form = new Form(Text="Rendering test",TopMost=true)
    form.Show()
    let img = new PictureBox(Dock=DockStyle.Fill)
    form.Controls.Add(img)

    img.Image <- bmp
    printfn "end"
