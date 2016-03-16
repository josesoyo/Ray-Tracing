// To test if i can dothe octree as an async object

let order = [1..2] |> List.collect( fun x -> 
             [1..2]  |> List.collect(fun y ->
                 [1..2] |> List.map(fun z -> (x,y,z))
                 ))



let asyncOct(orderi,group,vert,space,maxEle,depth,maxDepth ) = async { let (aa,bb,cc)= orderi
                                                                      return OctBucle(aa,bb,cc,group,vert,space,maxEle,depth,maxDepth ) }

let mClr = order 
                  |> List.collect(fun x -> [asyncOct(orderi,group,vert,space,maxEle,depth,maxDepth )])
                  |> Async.Parallel |> Async.RunSynchronously
                  |> Array.toList |> List.collect(fun x -> x) |> List.toArray


let partition = [|1..2|] |> Array.collect(fun aa -> 
                          [|1..2|] |> Array.collect(fun bb ->
                            [|1..2|] |> Array.collect(fun cc -> 
                            if aa <> 1 && bb <> 1 then [||]
                            else [|(cc,bb,aa)|]
                            )))
           