namespace BackwardRender
// To test that the code works I create some basic stuff to perform forward ray tracing

module Camera =
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    open Types.Algebra
    open Types
    open Types.types
    open Types.ObjectTypes
    open RayTracing.RayStructureIntersection        // one of the two RayTRcing must be deleted
    open RayTracing.ObjectSelection
    open SimpleShading
    open BackTypes

    open System.IO
    open System.Windows.Forms
    open System.Drawing

    //let Nproc = 20
    //let PixNumH , PixNumW = 100,100
    let selectType(ray,elements: elementMesh,useOctree:bool) =
        // select if it will intersect with the mesh of the octree
        //currently the function is NOT USED
        match useOctree with
        | true -> IntersectionOctree (ray,elements.Octree,elements.Mesh)
        | false -> IntersectionSimple (ray,elements.Mesh)               // to do tests 
   //Backward ray tracing, from cam to light
    let casting (scene:scene, pixH:int list,useOctree:bool) =
        let camera = scene.Camera
        let mutable bwimage = Matrix(camera.PixNumW, camera.PixNumH )//empty matrix of pixxpix
        let w = camera.LookAt.ToUnitVector()
        let u = (camera.Up><w)// Automatically generates an unit vector.ToUnitVector()
        let v = w><u
        let PixWide ,PixHeigh = camera.PixSize , camera.PixSize
        for i in 0..(scene.Camera.PixNumW-1) do 
            for j in pixH do
                    // create a ray from eye to pixel
                    let direct = camera.LookAt+(PixWide*float(scene.Camera.PixNumW/2-i))*u+(PixHeigh*float(scene.Camera.PixNumH/2-j))*v // Real to where it points
                    let infi:float<m> = (infinity |> LanguagePrimitives.FloatWithMeasure) 
                    let Ray = {
                                Wavelenght=WaveLength(0.6e-6<m>);
                                from= camera.EyePoint; uvec= direct.ToUnitVector();                 // From and direction
                                MaxLength = infi                              // Max distance can travell (should be infinite by defect)
                                OpticalPathTravelled= 0.<m>;                   // Optical Path Length Modified after every step with the IOR
                                NumBounces = 0.; Memory= [||];     // Num of bounces + the positions (Just in case for the future)
                                MaxDispersions = 3.;
                                NumOfParticlesCreated = 0;                               // Num of photos -> To split in a Lambertian surface, etc...
                                FracOfRay = 1.;
                                IndexOfRefraction=1.;
                                PhaseModulation = [||]
                                }

                    //if i = 200 && j = 200 then
                    //    printfn "here doesn't work"
                    //let grid = Cast_3DGrid (Scene,Ray,pPartition)//CastRay_nest (Scene, Ray) 3DGrid
                    let intersects =  intersection_all(Ray,scene.Elements)
                                     //|> Array.map(fun obj -> selectType(Ray,obj,useOctree))//IntersectionOctree (Ray,obj.Octree,obj.Mesh))// Octree
                                     //|> Array.collect(fun x -> match x with Some x -> [|x|] | None -> [||])
                    //if i =20 && j = 40 then
                    //    printfn "Will be a problem..."
                    //if grid.Length <> intersects.Length then
                    //    printfn "There's a problem at i%d j:%d" i j
                
                    //match grid with
                    match intersects with
                    | [||] -> bwimage.RotMat.[i,j] <- 0.0
                                
                    //match intersects with
                    //| _ ->  let color = GlobalIllum(grid |>List.minBy(fun x -> x.t), Scene ,pPartition )//colorAt (intersects |>List.minBy(fun x -> x.t), Scene  )
                    | _ ->  let color = colorAtOctree(intersects |>Array.minBy(fun x -> x.t), scene )
                            
                            //printfn "Color is: %+A" color
                            bwimage.RotMat.[i,j] <- color
                            
            if i%100=0 then printfn "%i" i
        [bwimage]
        
    let asyncasting (scene:scene,pixW:int list,useOctree) = async {return casting (scene, pixW,useOctree)}    // Prepare the parallel


    let  Do_Casting (scene,nprocs,useOctree:bool) =
        let pi =[0..(nprocs-1)] |> List.collect(fun x -> [[x..nprocs..scene.Camera.PixNumH-1]])

        let mClr = pi//[p0;p1;p2;p3] 
                          |> List.collect(fun x -> [asyncasting (scene,x,useOctree)])
                          |> Async.Parallel |> Async.RunSynchronously
                          |> Array.toList |> List.collect(fun x -> x)
        //#time // In case of execute as a script
        let imagergb = 
            let mutable ebw = Matrix(scene.Camera.PixNumW,scene.Camera.PixNumH)
          
            for i in [0..mClr.Length-1] do
                ebw <- (mClr.[i]+ebw)//mClr.[i-3])
              
            ebw

        imagergb
      







    let createBMP(img:Matrix,pathSave:string) =
        let size1,size2 = Array2D.length1 img.RotMat,Array2D.length2 img.RotMat
        let bmp = new Bitmap(size1,size2)

        let MatMax = img.RotMat  // Find the maximum of a matrix
                     |>  Seq.cast<float> |> Seq.max |> fun x -> match x with 0. -> 1. | _ -> x

        for i in [0..(Array2D.length1 img.RotMat)-1] do
            for j in [0..(Array2D.length1 img.RotMat)-1] do
                bmp.SetPixel(i,j,Color.FromArgb(int(255.0*img.RotMat.[i,j]/MatMax),  int(255.0*img.RotMat.[i,j]/MatMax) , int(255.0*img.RotMat.[i,j]/MatMax)) )

        // Show the image
        let form = new Form(Text="Rendering test",TopMost=true)
        //form.Show()
        printfn "I have commented a line on CreateBMP"
        let img = new PictureBox(Dock=DockStyle.Fill)
        form.Controls.Add(img)
        img.Image <- bmp
        bmp.Save(Path.Combine( __SOURCE_DIRECTORY__,pathSave))
