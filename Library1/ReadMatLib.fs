namespace Preprocessor

module ReadMatLib =
    open Types
    open Types.Algebra
    open Types.ObjectTypes
    open Types.types
    open System.IO
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols 


    let rec ReadMaterialParameters (lines: string list,mate:Material[], name:string, t:float,r:float,ior:float,wave:float, ppm:float) =
    
        let line = 
            let hed = lines.Head
            if hed.Length < 3 then "EMPTYYY"
            else hed
        printfn "%+A" line
        match line.[0..2] with 
        | "new" ->  let nmat = {MatName= name; R=r; T=t; n=(ior,WaveLength(wave |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= ppm}
                    
                    lines, Array.append mate [|nmat|] // return with the nmap line
                    
        | "TR " -> let nl = line.Substring(3).Split(' ')
                   let tt , rr = float nl.[0], float nl.[1]
                   printfn "tra&r: %+A +%A" tt rr
                   if lines.Length >1 then
                       ReadMaterialParameters (lines.Tail,mate, name, tt,rr,ior,wave, ppm)
                   else                                 // Termination always in other function
                       ReadMaterialParameters(["new "],mate, name, tt,rr,ior,wave, ppm)
    
        | "IOR" -> let nl = line.Substring(4).Split(' ')
                   let nior , nwave = float nl.[0], float nl.[1] //|> LanguagePrimitives.FloatWithMeasure<m>
                   printfn "n&w: %+A %+A " nior nwave
                   if lines.Length >1 then
                        ReadMaterialParameters (lines.Tail,mate, name, t,r,nior,nwave, ppm)
                   else                                // Termination always in other function
                        ReadMaterialParameters (["new "],mate, name, t,r,nior,nwave, ppm)
        | "PPM" -> let nl = line.Substring(4)
                   let nppm = float nl
                   printfn "%+A" nppm
                   if lines.Length >1 then
                        ReadMaterialParameters (lines.Tail,mate, name, t,r,ior,wave,nppm)
                   else                                // Termination always in other function
                        ReadMaterialParameters (["new "],mate, name, t,r,ior,wave,nppm)
        
        | _  ->  if lines.Length >1 then
                    ReadMaterialParameters (lines.Tail,mate, name, t,r,ior,wave, ppm)
                 else
                    ReadMaterialParameters (["new "],mate, name, t,r,ior,wave, ppm)

    and ReadMaterial(lines: string list, mate: Material[]):(Material[]) =
        let line = 
            let hed = lines.Head
            if hed.Length < 3 then "EMPTYYY"
            else hed
        printfn "%+A" line
        let mutable mlines = if lines.Length > 2 then lines.Tail
                             else []
        let mutable mmate = mate
        match line.[0..2] with 
        | "new" ->  let name = line.Substring(7)
                    let nlines , nmat = ReadMaterialParameters (lines.Tail,mate,name , 0.,0.,0.,0., 0.) 
                    mlines <- nlines
                    mmate <- nmat
        | "TR " -> printfn "Error! Should be inside newmaterial"
        | "IOR" -> printfn "Error! Should be inside newmaterial"
        | "PPM" -> printfn "Error! Should be inside newmaterial"
        | _  ->  line |> ignore
        printfn "materials are: +%A" mmate
        if mlines.Length > 2 then ReadMaterial( mlines,mmate)
        else mmate




    let ReadMatLib(mesh:mesh,matName:string) =
        // Reads the materials and returns mesh*Dict(materials)
        let list_lines = Seq.toList(File.ReadLines(matName)) //lines

        mesh, dict(ReadMaterial(list_lines,[||])  |> Array.map(fun x -> (x.MatName,x)))  

    let ReadMatLib_debug(matName:string) =
        // Reads the materials and returns mesh*Dict(materials)
        let list_lines = Seq.toList(File.ReadLines(matName)) //lines

        let out = ReadMaterial(list_lines,[||])
        printfn "the result is\n%+A" out
        let mdic = dict (out|> Array.map(fun x -> (x.MatName,x)))

        mdic