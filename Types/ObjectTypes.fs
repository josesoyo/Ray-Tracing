namespace Types

module ObjectTypes=
    //open MathNet.Spatial.Euclidean
    //open MathNet.Numerics.LinearAlgebra
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    open Algebra

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
    type SphSurfaceLens() =
        // Define default values
        let mutable SphereCentre= Point(0.,0.,0.)
        let mutable radius = 0.<m>
        let mutable cosMin = 0.
        let mutable axis = UnitVector(0.,0.,0.)
        let mutable convex = true

        member this.SphCentre with get() = SphereCentre and set(sc) = SphereCentre <- sc
        member this.Radius  with get() = radius and set(r) = radius <- r
        member this.CosMin  with get() = cosMin and set(cm) =  cosMin <- cm 
        member this.Axis  with get() = axis and set(uv) = axis <- uv
        //member this.Axis  with get() = axis and set(uv) = axis <-UnitVector uv
        member this.Convex with get() = convex and set(cb) = convex <- cb

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







    type OctreeSystem =                         // I consider that now only the triangles will be in the octree
    |Octree of Octree
    |Partition of Partition                    // int is the identifier of the group: GroupID
    and Octree = {Bbox:BBox; Octree: OctreeSystem []}
    and Partition = {Bbox: BBox; Partition:(int*group) []}
    //and Grid3D = {Bbox:BBox; Triangles: int list []; Name:string; MatName:string}

    // add all the mesh+octree of the mesh into a single element
    type elementMesh = {Mesh:mesh; Octree: OctreeSystem[] }

