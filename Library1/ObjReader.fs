namespace Preprocessor


//
//  Finnaly looks like it's working!
//      the read data contains the exact number of groups (40 for the thorlabs holder)
//
module foo =
    let func1() =
        printfn " it's 2"
    

module reading =
    // 
    //  Carefull, how the materials are read is not included yet
    //
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames 
    open MathNet.Spatial.Euclidean
    open System.IO
    open SimpleRead
    open Types
    open Types.ObjectTypes
    open bboxread
    // define the types
    (*type group = {Triangles: int list list;  Normals: UnitVector3D list;Bbox:BBox}
    type mesh = {Vertices:float [] list ; groups: group list ;Bbox:BBox} *)
    // let Vertice : int[] = Array.create 3 0   // create an array of 0 0 0
    
    /// /// ///
    ///
    /// /// ///
    
    let rec ReadGroup_Vn (lines:string list, tList: int list [], nList: UnitVector3D [] ,vertices: float<metre> [] [],
                          gname:string,matname:string) =
      // read all the faces in a group
      let first = lines.Head
      let mutable normals = nList
      let mutable triangles = tList  
      
      if first.StartsWith("f") then
        // read the triangles and add them from old routintes in ObjReader.fs
        // compute the new normal -> Carefull! Remember the point of the orientation. ¿May I use the normals of the vertives?
        let ntriangle = TriVertNfromFile (first)                 // read the triangle id
        if TestMeshParallel(vertices,ntriangle) then ReadGroup_Vn(lines.Tail,triangles, normals,vertices,gname,matname)
        else
            //let nnormal = MeshNormals(vertices,ntriangle)    // Compute the normal
            triangles <- Array.append triangles [|ntriangle|]
            //normals <- normals@[nnormal]
            if lines.Length > 1 then ReadGroup_Vn(lines.Tail,triangles, normals,vertices,gname,matname)
            else                // End Of File
              // To finish one Compute BBox 
          
              let bbox = BBtri(triangles,vertices)
              if triangles.Length <> 6 then
                printfn "There is an error on the reading of the faces and vn!!!!"
              {Name = gname; Triangles = triangles;Normals = normals;Bbox =bbox; MatName =matname}          
      //elif first.StartsWith("") then ReadGroup_Vn(lines.Tail,triangles, normals,vertices) // ALWAYS TRUE...
      elif first.StartsWith("usemtl") then 
        printfn "Carefull this is not well considered how the usemtlib works"
        ReadGroup_Vn(lines.Tail,triangles, normals,vertices,gname,matname)
      else 
        let bbox =BBtri(triangles,vertices)
        {Name= gname;Triangles = triangles;Normals = normals;Bbox =bbox; MatName =matname}
       
      
          
    let rec ReadGroup(lines:string list, tList: int list [], nList: UnitVector3D [],vertices: float<metre> [] [] ,
                       gname:string,matname:string) =
      
      // read all the faces in a group
      let first = lines.Head
      let mutable normals = nList
      let mutable triangles = tList

      if first.StartsWith("f") then
        // read the triangles and add them from old routintes in ObjReader.fs
        // compute the new normal -> Carefull! Remember the point of the orientation. ¿May I use the normals of the vertives?
        let ntriangle = TrifromFile (first)                 // read the triangle id
        
        let nnormal = MeshNormals(vertices,ntriangle)    // Compute the normal
        if nnormal = UnitVector3D(0.,0.,0.) then ReadGroup(lines.Tail,triangles, normals,vertices,gname,matname)
        else
            triangles <- Array.append triangles [|ntriangle|]
            normals <-  Array.append normals [|nnormal|]
            if lines.Length > 1 then ReadGroup(lines.Tail,triangles, normals,vertices,gname,matname)
            else                // End Of File
              // To finish one Compute BBox 
          
              let bbox = BBtri(triangles,vertices)
              {Name = gname;Triangles = triangles;Normals = normals;Bbox =bbox; MatName = matname}            
          //elif first.StartsWith("") then ReadGroup_Vn(lines.Tail,triangles, normals,vertices)      
      elif first.StartsWith("usemtl") then 
        printfn "Carefull this is not well considered how the usemtlib works"
        ReadGroup(lines.Tail,triangles, normals,vertices,gname,matname)
      else 
        let bbox =BBtri(triangles,vertices)
        {Name = gname; Triangles = triangles;Normals = normals;Bbox =bbox; MatName = matname}





    /// /// ///
    //              Read Lines
    /// /// ///
    let  rec find_Face_Vn (lines:string list, tList: int list [], nList: UnitVector3D [] ,vertices: float<metre> [] [],gname:string) =
         // Required because sometimes after a groups starts, there's an empty line
         //if lines.Head.StartsWith("f")  then
         //   ReadGroup_Vn(lines,tList, nList,vertices,gname,matname)
         if lines.Head.StartsWith("usemtl") then 
            //Should start always with a material
            let matname = lines.Head.Substring(7) // usemtl + 1char
            ReadGroup_Vn(lines.Tail,tList, nList,vertices,gname,matname)
         else//elif tail.Head.StartsWith("g") || first.StartsWith("s") then
            find_Face_Vn(lines.Tail, tList, nList ,vertices,gname)

    let  rec find_Face (lines:string list, tList: int list [], nList: UnitVector3D [] ,vertices: float<metre> [] [],
                        gname:string) =
         // Required because sometimes after a groups starts, there's an empty line

         if lines.Head.StartsWith("usemtl") then 
            //Should start always with a material
            let matname = lines.Head.Substring(7) // usemtl + 1char
            ReadGroup(lines.Tail,tList, nList,vertices,gname,matname)
         else//elif tail.Head.StartsWith("g") || first.StartsWith("s") then
            find_Face(lines.Tail, tList, nList ,vertices,gname)

    
    let rec ReadTheLines_Vn (lines:string list, oldvert, oldvertn ,oldgroups,oldbox:BBox,matLibPath:string) =
        // Read the lines of the mesh that contains vertice normal information
        let first = lines.Head
        let mutable (vert, vertn,groups, nmatLibPath) = (oldvert,oldvertn,oldgroups,matLibPath) 
        if first.StartsWith("vn") then
            // Read Vertice Normal
            vertn <-  Array.append vertn (VerNfromFile(first)) // Dummy update
            //printfn "" //"Vertices normals must be read"
        elif first.StartsWith("v") then
            //read the vertices
            let nvertice = VerfromFile(first)
            vert <-  Array.append vert nvertice            
        elif first.StartsWith("g") then // || first.StartsWith("s")
            // Read the new group type
            //let tail = lines.Tail
            let gname = first.Substring(2)
            let ngroups = find_Face_Vn(lines.Tail,[||],[||],vert, gname)
            //let ngroups = ReadGroup_Vn(lines.Tail,[],[],vert)        
            groups <-  Array.append groups [|ngroups|]
        else if first.StartsWith("mtllib") then
            printfn "Reading the material library Path"
            let mlib = first.Substring(7)
            nmatLibPath <- OpenMatLib(mlib)
        else if first.StartsWith("mtllib") then
            printfn "Reading the material library Path"
            let mlib = first.Substring(7)
            nmatLibPath <- OpenMatLib(mlib)
        else if first.StartsWith("usemtl") then
            printfn "There is a material library used and there's an error because must be read inside a group"
          
        // recursivity?
        if lines.Length > 1 then 
            ReadTheLines_Vn(lines.Tail,vert,vertn,groups,oldbox, nmatLibPath)  
        else
            printfn "Here must return the MESh (NOT DONE)"// Return the mesh
            let box = BBgroups(groups)  // compute new the globall Bbox
            ({Vertices = vert ; VNormals = vertn ; groups=groups ;Bbox = box}  , matLibPath)

    let rec ReadTheLines (lines:string list, oldvert, oldgroups,oldbox:BBox , matLibPath:string) =
        // Read the lines passed as a string list
      let first = lines.Head  //Check how is the head
      let mutable (vert,groups, nmatLibPath) = (oldvert,oldgroups,matLibPath)

      if first.StartsWith("vn") then
        // I cannot send here the function because then the output wouldn't be a UNIT
        printfn "Carefull! Should you have normals of the vertices?!?!"
        
      elif first.StartsWith("v") then
        //read the vertices
        let nvertice = VerfromFile(first)
        vert <-  Array.append vert nvertice
      elif first.StartsWith("g") then // || first.StartsWith("s") 
        //do groups 
        let gname = first.Substring(2)
        let ngroups = find_Face(lines.Tail,[||],[||],vert,gname)//ReadGroup(lines.Tail,[],[],vert,gname)
        printfn "help me!"
        groups <-  Array.append groups [|ngroups|]
        else if first.StartsWith("mtllib") then
            printfn "Reading the material library Path"
            let mlib = first.Substring(7)
            nmatLibPath <- OpenMatLib(mlib)
        else if first.StartsWith("usemtl") then
            printfn "There is a material library used and there's an error because must be read inside a group"
              
      if first.StartsWith("vn") then
        // Here I use another function to read the mesh with vertice normals
        ReadTheLines_Vn(lines, vert, [||], groups,oldbox,nmatLibPath)
      elif lines.Length > 1 then
        ReadTheLines (lines.Tail, vert, groups,oldbox,nmatLibPath)
      else 
       //
       // Find the global bounding box of the mesh from the bbox of the groups

       printfn "Here must return the MESh (NOT DONE)"// Return the mesh
       let box = BBgroups(groups)  // compute new the globall Bbox
       ({Vertices = vert ; VNormals = [||] ; groups=groups ;Bbox = box},matLibPath)

    /// /// /// ///
    //
    /// /// /// ///
    let ReadMeshWavefront2(path:string) =
      //    Shall material be introduced as an external o read here?!?
      let list_lines = Seq.toList(File.ReadLines(path)) //lines
      // infinity doesn't works for  unit of measure...
      let infi:float<metre> = (infinity |> LanguagePrimitives.FloatWithMeasure)     //inifinity with unit of measure
      let samplebox = {Pmin = [|infi; infi;infi|];
                       Pmax = [|-infi;-infi;-infi|]}
      ReadTheLines (list_lines , [||], [||],samplebox, "") // Should returnthe mesh

