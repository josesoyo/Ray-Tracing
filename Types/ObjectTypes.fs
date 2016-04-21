namespace Types

module ObjectTypes=
    //open MathNet.Spatial.Euclidean
    //open MathNet.Numerics.LinearAlgebra
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    open Algebra

    // Sensor type
    type SensorContent(pos,dir,nr,ph) =

        let position:Point = pos
        let direction:UnitVector = dir
        let numRays:int = nr
        let phase:float =ph
        member this.Position with get() = position
        member this.Direction with get() = direction
        member this.NumRays with get () = numRays
        member this.Phase with get() = phase

    type Sensor(exs:bool, term:bool) =
        // Sensor type, it will be add to all the objects.
        // If the sensor doesn't exit, then it will pass the sensor options
        // If exists, will check if it is a termination on the ray (End Sensor), or the ray continues, but saves the value
        let exists = exs     // says if it's a sensor or not
        let terminate = term
        let mutable data:(SensorContent[]) = [||]
        member this.Exists with get() = exists
        member this.Terminate with get() = terminate
        member this.SavedData with get() = data

        member this.AddData(sc) =
            data <- Array.append data [|sc|]

        new (exs) =
            Sensor(exs, false)
        new () =
            // default sensor is a no-sensor
            Sensor(false, false)




    // Types for space
    type BBox = {Pmin :Point ; Pmax:Point}

    type group = {Name:string; TrianglesNormals: (int list *UnitVector)[];Bbox:BBox; MatName:string}
    type mesh = {Vertices:Point [] ; VNormals:UnitVector []; groups: group [] ;Bbox:BBox}
    type sphere = {center:Point; radius:float<m>}//; material:Material }
    type partSphere = {Sphere:sphere; 
                       zmin:float<m>; zmax:float<m>;
                       //phimin:float<radian>;phimax:float<radian>;
                       ObjToWorld:Matrix//<float>;  // Transforms from (0.,0.,1.) -> Normal
                       WorldToObj:Matrix//<float>  // Transforms from Normal -> (0.,0.,1.)                   
                      }
    (*
    type SphSurfaceLens = { Sphere:sphere ;
                            CosMin: float;
                            Axis: UnitVector;
                            Convex:bool; // Convex = convergent (normal sphere) 
                          } // Name because will be the surface of a lens
    *)
    type SphSurfaceLens(centreofSPH, roc:float<m>, diam:float<m>,axs:UnitVector, conv:bool,matname:string, snrs:Sensor) =
        // Define default values
        let sensor = snrs
        let mutable SphereCentre= centreofSPH// Point(0.,0.,0.)
        let mutable radius = roc//0.<m>
        let mutable clearaperture = diam
        let mutable cosMin = //0.
                             let sinth = 0.5*clearaperture/roc
                             if sinth < 1. then sqrt(1.-(sinth*sinth)) 
                             else 0.
        let mutable axis = axs//UnitVector(0.,0.,0.)
        let mutable convex = conv
        let mutable Materialname = matname

        member this.SphCentre with get() = SphereCentre and set(sc) = SphereCentre <- sc
        member this.RadiusOfCurvature  
            with get() = radius 
            and set(r) =
                radius <- r
                cosMin <-    let sinth = 0.5*clearaperture/r
                             if sinth < 1. then sqrt(1.-(sinth*sinth)) 
                             else 0.
        member this.ClearAperture 
            with get() = clearaperture
            and set(ca) =
                clearaperture <- ca
                cosMin <-    let sinth = 0.5*ca/radius
                             if sinth < 1. then sqrt(1.-(sinth*sinth)) 
                             else 0.

        member this.CosMin  with get() = cosMin //and set(cm) =  cosMin <- cm 
        member this.Axis  with get() = axis and set(uv) = axis <- uv
        //member this.Axis  with get() = axis and set(uv) = axis <-UnitVector uv
        member this.Convex with get() = convex and set(cb) = convex <- cb
        member this.MaterialName with get() = Materialname and set(mn) = Materialname <- mn
        static member Zero = SphSurfaceLens(Point(0.,0.,0.), 0.<m>, 0.<m>,UnitVector(0.,0.,1.), true,"")
        new (centreofSPH, roc, diam,axs, conv,matname) =
            // Create the material NOT being sensor
            SphSurfaceLens(centreofSPH, roc, diam,axs, conv,matname, Sensor()) 

        (*
        static member CreateLensSurface(centreofSPH, roc:float<m>, diameter:float<m>,axis:UnitVector, convex:bool) =
            // Generate the structure
            let sinth = 0.5*diameter/roc
            let costh =
                if sinth < 1. then sqrt(1.-(sinth*sinth)) 
                else 0.
            let SL = SphSurfaceLens()
            SL.Radius<- roc
            SL.Axis <- axis
            SL.CosMin <- costh
            SL.Convex <- convex
            SL.SphCentre <- centreofSPH
            SL
        *)

    (* 
    type cylinder = {Radius: float;             // Radius of the cylinder
                     zmin:float; zmax:float;    // By definition in object space z aligned
                     LBbox:BBox; WBbox:BBox;    // Local and world Bounding box
                     Origin:Point3D             // Origin of the axis in real world (Translation)
                     Normal:UnitVector3D;       // Direction of the cylinder
                     material:material
                     ObjToWorld:Matrix<float>;  // Transforms from (0.,0.,1.) -> Normal
                     WorldToObj:Matrix<float>  // Transforms from Normal -> (0.,0.,1.)
                     }
    *)
    type cylinder(rad:float<m>,zmax:float<m>,orig:Point,nrm:UnitVector,matname:string, snrs:Sensor) = 
        // Type for a cylinder which in the local frame is oriented on +Z direction starting at z = 0 <m>
        // define the contentis
        let sensor = snrs
        let  mutable radius = rad //0<m>
        let mutable zmax = zmax// zmin, zmin0<m>, infi
        let mutable origin = orig                       // where the cylinder starts
        let mutable normal = nrm                        // where it points
        let mutable Materialname = matname
        let mutable LBbox = {Pmin = Point.FromMeasures(-radius,-radius,0.<m>); Pmax = Point.FromMeasures(radius,radius,zmax)}
        let mutable ObjToWorld = Matrix.RotateVector(UnitVector(0.,0.,1.), normal)
        let mutable WorldToObj = Matrix.RotateVector(normal,UnitVector(0.,0.,1.))
        let ComputeWbox (objtoWorld:Matrix) lbox=
                // private function to compute the world Bounding Box
                let NonTransformedEdges = [|LBbox.Pmin;
                                            Point(LBbox.Pmin.X,LBbox.Pmax.Y,LBbox.Pmin.Z);
                                            Point(LBbox.Pmax.X,LBbox.Pmin.Y,LBbox.Pmin.Z);
                                            Point(LBbox.Pmax.X,LBbox.Pmax.Y,LBbox.Pmin.Z);
                                
                                            Point(LBbox.Pmin.X,LBbox.Pmin.Y,LBbox.Pmax.Z);
                                            Point(LBbox.Pmax.X,LBbox.Pmin.Y,LBbox.Pmax.Z);
                                            Point(LBbox.Pmin.X,LBbox.Pmax.Y,LBbox.Pmax.Z);
                                            LBbox.Pmax|]

                let TransformedEdges = NonTransformedEdges                  // Rotate as should be by normal direction
                                       |> Array.map(fun x -> objtoWorld.RotatePoint(x))
                let minTrfEdgesX =  (TransformedEdges |> Array.minBy(fun x -> x.X)).X   
                let minTrfEdgesY =  (TransformedEdges |> Array.minBy(fun x -> x.Y)).Y
                let minTrfEdgesZ =  (TransformedEdges |> Array.minBy(fun x -> x.Z)).Z 
                let minTrfEdges = Point(minTrfEdgesX,minTrfEdgesY,minTrfEdgesZ)
                // Min of the Box in world
                let wPmin = minTrfEdges.MoveAndCreateNew(origin) 

                let maxTrfEdgesX =  (TransformedEdges |> Array.maxBy(fun x -> x.X)).X   
                let maxTrfEdgesY =  (TransformedEdges |> Array.maxBy(fun x -> x.Y)).Y 
                let maxTrfEdgesZ =  (TransformedEdges |> Array.maxBy(fun x -> x.Z)).Z
                let maxTrfEdges = Point(maxTrfEdgesX,maxTrfEdgesY,maxTrfEdgesZ)
                // Max of Box in World
                let wPmax = maxTrfEdges.MoveAndCreateNew(origin)
                {Pmin=wPmin;Pmax=wPmax}

        let mutable WBbox = ComputeWbox ObjToWorld LBbox       // Generate Bounding box on the world

        member this.Radius 
            with get() = radius 
            and set(r) = 
                // not only updates the radius, also updates the bounding box
                radius <- r
                LBbox <- {Pmin = Point.FromMeasures(-r,-r,0.<m>); Pmax = Point.FromMeasures(r,r,zmax)}
                WBbox <- ComputeWbox ObjToWorld LBbox
        //member this.Zmin with get() = zmin
        member this.Zmax 
            with get() = zmax 
            and set(zm) = 
                zmax <- zm
                let nlbox = {Pmin = Point.FromMeasures(-radius,-radius,0.<m>); Pmax = Point.FromMeasures(radius,radius,zm)}
                LBbox <-nlbox
                WBbox <- ComputeWbox ObjToWorld nlbox

        member this.Origin with get() = origin
        member this.Normal 
            with get() = normal
            and set(nrm) =
                // updates also the properties of that are dependent of the radius
                normal <- nrm
                let obj2world = Matrix.RotateVector(UnitVector(0.,0.,1.), normal)
                WorldToObj <- Matrix.RotateVector(normal,UnitVector(0.,0.,1.))
                WBbox <-ComputeWbox obj2world LBbox

        member this.MaterialName with get() = Materialname and set(ma) = Materialname <- ma
        member this.wBbox with get() = WBbox
        member this.lBbox with get() = LBbox
        member this.Obj2World with get() = ObjToWorld
        member this.World2Obj with get() = WorldToObj
        static member Zero =  cylinder(0.<m>,0.<m>,Point(0.,0.,0.),UnitVector(0.,0.,1.),"") 
        new  (rad,zmax,orig,nrm,matname) = 
            cylinder(rad,zmax,orig,nrm,matname, Sensor()) 








    type OctreeSystem =                         // I consider that now only the triangles will be in the octree
    |Octree of Octree
    |Partition of Partition                    // int is the identifier of the group: GroupID
    and Octree = {Bbox:BBox; Octree: OctreeSystem []}
    and Partition = {Bbox: BBox; Partition:(int*group) []}
    //and Grid3D = {Bbox:BBox; Triangles: int list []; Name:string; MatName:string}

    // add all the mesh+octree of the mesh into a single element
    type elementMesh = {Mesh:mesh; Octree: OctreeSystem[] }

    type Object =
    // Option type to add all the different structure types into a single object type
    | Mesh of elementMesh
    | Cylinder of cylinder
    | SurfaceLens of SphSurfaceLens
