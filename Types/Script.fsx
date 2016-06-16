// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#load @"Algebra.fs"
open Types.Algebra
#load "MainTypes.fs"
open Types
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

type BBox = {Pmin:Point;Pmax:Point}
type cylinder (rad:float<m>,zmax:float<m>,orig:Point,nrm:UnitVector,matname:string, sensor:bool) =
               
    // Type for a cylinder which in the local frame is oriented on +Z direction starting at z = 0 <m>
    // define the contentis
    let sensor= sensor
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
    member this.Sensor with get() = sensor
    static member Zero =  cylinder(0.<m>,0.<m>,Point(0.,0.,0.),UnitVector(0.,0.,1.),"") 
    new (rad:float<m>,zmax:float<m>,orig:Point,nrm:UnitVector,matname:string) =
        cylinder(rad,zmax,orig,nrm,matname,false)
    new (rad:float<m>) =
        cylinder(rad,10.<m>,Point(0.,0.,0.),UnitVector(1.,0.,0.),"set.mat",false)
// Define your library scripting code here

    
//        member this.X with get() = x and set(xx) = x <-xx

//type Phase = float
//[<StructAttribute>]
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

// I want to define the vector, unitvector and the two operations
(*
type Vector = 
|Vector of float list
|UnitVector of float list

let VectorToUnit(v:Vector) =
    // Transforms a vector into UnitVectpr
    match v with
    | UnitVector v -> UnitVector(v)
    | Vector v ->
            let norm = (0., v) ||> List.fold(fun acc x -> acc + x*x) |> sqrt
            v |> List.map(fun x ->  x/norm) |> UnitVector

let DotProduct(v1:Vector,v2:Vector) =
    let v1a = 
        match v1 with 
        |Vector v1-> v1 
        |UnitVector v1-> v1      

    let v2a = 
        match v2 with 
        |Vector v2-> v2
        |UnitVector v2-> v2   
    (0.,v1a,v2a) |||> List.fold2(fun acc x y -> acc + x * y )
    
let CrossProduct(v1:Vector, v2:Vector) =
    let u = 
        match v1 with 
        |Vector v1-> v1 
        |UnitVector v1-> v1      

    let v = 
        match v2 with 
        |Vector v2-> v2
        |UnitVector v2-> v2   
    
    let c1 = u.[1]*v.[2] - u.[2]*v.[1]
    let c2 = u.[2]*v.[0] - u.[0]*v.[2]
    let c3 = u.[0]*v.[1] - u.[1]*v.[0]
    match v1, v2 with
    | UnitVector v1 , UnitVector v2 ->  UnitVector([c1;c2;c3])
    | _ , _ -> Vector([c1;c2;c3])

let IsParallelTo(v1, v2, tolerance) =
    let u = 
        match v1 with 
        |Vector v1-> v1 
        |UnitVector v1-> v1      

    let v = 
        match v2 with 
        |Vector v2-> v2
        |UnitVector v2-> v2   
    let cp = 
        let cpp = CrossProduct(v2,v1)
        match cpp with
        |Vector cpp-> cpp
        |UnitVector cpp-> cpp  
    if abs(cp.[0]) < tolerance && abs(cp.[1]) < tolerance && abs(cp.[2]) < tolerance then true
    else false

let v1 = Vector([2.;1.;0.])
let v2 = UnitVector([1.;0.5;0.])
DotProduct(v2,v1)
VectorToUnit v1
CrossProduct(v2,v1) <> Vector([0.;0.;0.])
*)

/////////////////
//
////////////////

type MyClass(x0, y0, z0) =
    let mutable x = x0
    let mutable y = y0
    let mutable z = z0
    do
        printfn "Initialized object that has coordinates (%d, %d, %d)" x y z
    member this.X with get() = x and set(value) = x <- value
    member this.Y with get() = y and set(value) = y <- value
    member this.Z with get() = z and set(value) = z <- value
    new() = MyClass(0, 0, 0)
type MyStruct =
    struct
       val X : int
       val Y : int
       val Z : int
       new(x, y, z) = { X = x; Y = y; Z = z }
    end

let myStructure1 = new MyStruct(1, 2, 3) 
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
(*
type SphSurfaceLens2 =
    struct
        val SphereCentre:Point
        val Radius:float<m>
        val CosMin: float
        val Axis: UnitVector
        val Convex: bool
    new(p,r,c,a,cb) = {SphereCentre = }
*)

// testing an interface and abstract members
type nti =
    interface
        abstract member Ai: int -> int
        abstract member Bi: int[] -> int[]
        abstract member Ci: float[] -> float[]
    end
type nt(a:int,b:int[],c:float32[]) =
    new (a:int,b:int[]) =
      nt(a,b,null)  
    new (a:int,c:float32[]) =
      nt(a,null,c)  

    abstract member A() = a
    member this.B() = b
    member this.C() = c
    
    interface nti with
        member this.A() = this.A()
 

type rtest = {A:int; B:int}

type stest(a:int,b:int) =
    member this.A with get() = a
    member this.B with get() = b
    member this.new_a(c) =
        stest(c,this.B)






type test2 = {a1:int;a2:int}
let ne = {a1=0;a2 = 99}







type test( a:int, b:int) =
    let ne = {A = a;B = b} 
    member this.a1 with get() = ne.A //and set(an) = A <- an
    member this.b1 with get() = ne.B and set(bn) = ne.B <- bn
let nw = test(0,99)
let nw2 = nw.new_a(1000)
nw.a1
nw.b1
let defaulttest = test(99,00)
let nt = {nw with A = -10}
let nt = {ne with a1 = -10}
