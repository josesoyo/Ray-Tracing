module bboxread 

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames 
open MathNet.Spatial.Euclidean
open Types.ObjectTypes
let BBtri (triangles: int list [], vert: float<metre> [] []) =
        printfn "num of vert: %+A" vert.Length
        let points0 =triangles |> Array.map(fun x -> vert.[x.[0]-1])
        let points1 =triangles |> Array.map(fun x -> vert.[x.[1]-1])
        let points2 =triangles |> Array.map(fun x -> vert.[x.[2]-1])
        let points = Array.append (Array.append points0 points1) points2 
        printfn "points defined with a length %i" points.Length
        let xMax = points |> Array.maxBy(fun x -> x.[0]) 
        printfn "m1"
        let yMax = points |> Array.maxBy(fun x -> x.[1])
        printfn "m2"
        let zMax = points |> Array.maxBy(fun x -> x.[2])        
        printfn "m3"
        let xMin = points |> Array.minBy(fun x -> x.[0])
        printfn "m4"
        let yMin = points |> Array.minBy(fun x -> x.[1])
        printfn "m5"
        let zMin = points |> Array.minBy(fun x -> x.[2])
        printfn "m6"
        {Pmin=[|xMin.[0];yMin.[1];zMin.[2]|];Pmax=[|xMax.[0];yMax.[1];zMax.[2]|]} // BBox of the piece
let BBgroups(gr: group []) =
        let xMax = gr |> Array.maxBy(fun x -> x.Bbox.Pmax.[0]) 
        let yMax = gr |> Array.maxBy(fun x -> x.Bbox.Pmax.[1])
        let zMax = gr |> Array.maxBy(fun x -> x.Bbox.Pmax.[2])        
        let xMin = gr |> Array.minBy(fun x -> x.Bbox.Pmin.[0])
        let yMin = gr |> Array.minBy(fun x -> x.Bbox.Pmin.[1])
        let zMin = gr |> Array.minBy(fun x -> x.Bbox.Pmin.[2])
        {Pmin=[|xMin.Bbox.Pmin.[0];yMin.Bbox.Pmin.[1];zMin.Bbox.Pmin.[2]|];
            Pmax=[|xMax.Bbox.Pmax.[0];yMax.Bbox.Pmax.[1];zMax.Bbox.Pmax.[2]|]} // BBox of the piece