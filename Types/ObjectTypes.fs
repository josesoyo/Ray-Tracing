namespace Types

module ObjectTypes=
    //open MathNet.Spatial.Euclidean
    //open MathNet.Numerics.LinearAlgebra
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    open Algebra

    // Types for space

    type BBox = {Pmin :Point ; Pmax:Point}

    type group = {Name:string; Triangles: int list [];  Normals: UnitVector [];Bbox:BBox; MatName:string}
    type mesh = {Vertices:Point [] ; VNormals:UnitVector []; groups: group [] ;Bbox:BBox}

    type sphere = {center:Point; radius:float<m>}//; material:Material }
    type partSphere = {Sphere:sphere; 
                       zmin:float<m>; zmax:float<m>;
                       //phimin:float<radian>;phimax:float<radian>;
                       ObjToWorld:Matrix//<float>;  // Transforms from (0.,0.,1.) -> Normal
                       WorldToObj:Matrix//<float>  // Transforms from Normal -> (0.,0.,1.)                   
                      }
    type SphSurfaceLens = { Sphere:sphere ;
                            CosMin: float;
                            Axis: UnitVector;
                            Convex:bool; // Convex = convergent (normal sphere) 
                          } // Name because will be the surface of a lens






    type OctreeSystem =                         // I consider that now only the triangles will be in the octree
    |Octree of Octree
    |Partition of (int*group) []                // int is the identifier of the group: GroupID
    and Octree = {Bbox:BBox; Octree: OctreeSystem []}
    //and Grid3D = {Bbox:BBox; Triangles: int list []; Name:string; MatName:string}


