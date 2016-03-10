module bboxread 

//open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols 
//open MathNet.Spatial.Euclidean
open Types.Algebra
open Types.ObjectTypes
let BBtri (triangles: int list [], vert: Point []) =
        printfn "num of vert: %+A" vert.Length
        let points0 =triangles |> Array.map(fun x -> vert.[x.[0]-1])
        let points1 =triangles |> Array.map(fun x -> vert.[x.[1]-1])
        let points2 =triangles |> Array.map(fun x -> vert.[x.[2]-1])
        let points = Array.append (Array.append points0 points1) points2 
        printfn "points defined with a length %i" points.Length
        let xMax = points |> Array.maxBy(fun x -> x.X) 
        printfn "m1"
        let yMax = points |> Array.maxBy(fun x -> x.Y)
        printfn "m2"
        let zMax = points |> Array.maxBy(fun x -> x.Z)        
        printfn "m3"
        let xMin = points |> Array.minBy(fun x -> x.X)
        printfn "m4"
        let yMin = points |> Array.minBy(fun x -> x.Y)
        printfn "m5"
        let zMin = points |> Array.minBy(fun x -> x.Z)
        printfn "m6"
        {Pmin=Point(xMin.X,yMin.Y,zMin.Z);Pmax=Point(xMax.X,yMax.Y,zMax.Z)} // BBox of the piece
let BBgroups(gr: group []) =
        let xMax = gr |> Array.maxBy(fun x -> x.Bbox.Pmax.X) 
        let yMax = gr |> Array.maxBy(fun x -> x.Bbox.Pmax.Y)
        let zMax = gr |> Array.maxBy(fun x -> x.Bbox.Pmax.Z)        
        let xMin = gr |> Array.minBy(fun x -> x.Bbox.Pmin.X)
        let yMin = gr |> Array.minBy(fun x -> x.Bbox.Pmin.Y)
        let zMin = gr |> Array.minBy(fun x -> x.Bbox.Pmin.Z)
        {Pmin=Point(xMin.Bbox.Pmin.X,yMin.Bbox.Pmin.Y,zMin.Bbox.Pmin.Z);
            Pmax=Point(xMax.Bbox.Pmax.X,yMax.Bbox.Pmax.Y,zMax.Bbox.Pmax.Z)} // BBox of the piece