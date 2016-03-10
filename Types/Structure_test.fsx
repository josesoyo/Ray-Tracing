#load @"C:\Users\JoseM\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\Types\Algebra.fs"
// To understand how structures work
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Types.Algebra
let d = 2.<m>
 
type Wavelength = float<m>

type Material =
  {T: float; R:float;
   n:System.Collections.Generic.IDictionary<Wavelength, float>;
   LambPPM:float;
   }


let defaultMaterial = { T=0.; R=0.; n=dict[(1.<m>, 1.)]; LambPPM=0.}


let materials = 
  dict [
         ("Iron", { defaultMaterial with T=1. })
         ("wood", {defaultMaterial with T=0.5 ;LambPPM = 200. })
       ]

materials.["Iron"]
materials.["wood"]
 
let nm = { defaultMaterial with T=2. }

            ///         ///
//
// Structure on a type (not record)
type Ray(n) =
  let wavelen : Wavelength = 1e-6<m>
  let mutable maxhits: int = n

  member this.WaveLength 
    with get() = wavelen
  member this.Hits() =
    ()
  member this.MaxHits
   with get() = maxhits
   and set(n:int) = maxhits <- n  // if it's mutable
  member this.MadFunc(hh:float) = (printfn "madfunc: %+A" (hh*wavelen))

let r = Ray(10)
r.Hits()
r.MadFunc(98.0)
r.WaveLength    // use the get()
r.MaxHits <- 9  // use the set()

// https://github.com/FSCL/FSCL.Compiler/wiki

//         ///         ///         ///     ///
// tests to define the algebra

// point3D
let a = (1.,2.,3.)
let _,_, z = a

type Point(xp, yp , zp) =
    let mutable x:float<m> = xp |> LanguagePrimitives.FloatWithMeasure
    let mutable y:float<m> = yp |> LanguagePrimitives.FloatWithMeasure
    let mutable z:float<m> = zp |> LanguagePrimitives.FloatWithMeasure

    member this.X 
     with get() = x
    member this.Y  
     with get() = y
    member this.Z 
     with get() = z
    member this.Move(dx,dy,dz) = 
        x <- x+dx
        y <- y+dy
        z <- z + dz
    // Diference between points gives a vector
    static member (-) (p1:Point,p2:Point) = Vector(float(p1.X - p2.X), float(p1.Y - p2.Y), float(p1.Z - p2.Z))
and Vector(xv, yv , zv) =
    let x:float = xv //|> LanguagePrimitives.FloatWithMeasure
    let y:float = yv //|> LanguagePrimitives.FloatWithMeasure
    let z:float = zv //|> LanguagePrimitives.FloatWithMeasure

    member this.X 
     with get() = x
    member this.Y  
     with get() = y
    member this.Z 
     with get() = z
    //member
    member this.Module() = sqrt(x*x+y*y+z*z)

    // operations that can be done
    // Sum vectors
    static member (+) (v1:Vector, v2:Vector) = Vector(v1.X + v2.X,v1.Y + v2.Y,v1.Z + v2.Z)
    static member (+) (v1:Vector, v2:UnitVector) = Vector(v1.X + v2.X,v1.Y + v2.Y,v1.Z + v2.Z)
    static member (+) (v1:UnitVector, v2:Vector) = Vector(v1.X + v2.X,v1.Y + v2.Y,v1.Z + v2.Z)
    // Sum from a point
    static member (+) (v1:Vector, p:Point) = Vector(v1.X + float(p.X),v1.Y + float (p.Y), v1.Z + float(p.Z))
    static member (+) (p:Point,v1:Vector) = Vector(v1.X + float(p.X),v1.Y + float(p.Y), v1.Z + float(p.Z))
    // Product by a scalar
    static member (*) (k:float,v:Vector) = Vector(k*v.X,k*v.Y,k*v.Z)
    // Scalar product
    static member (*) (v1:Vector, v2:Vector) = v1.X*v2.X+v1.Y*v2.Y+v1.Z*v2.Z
    static member (*) (v1:Vector, v2:UnitVector) = v1.X*v2.X+v1.Y*v2.Y+v1.Z*v2.Z
    static member (*) (v1:UnitVector, v2:Vector) = v1.X*v2.X+v1.Y*v2.Y+v1.Z*v2.Z
    // cross product
    static member (><) (v1:Vector, v2:Vector) =
        let xn = v1.Y*v2.Z - v1.Z*v2.Y
        let yn = -v1.X*v2.Z + v1.Z*v2.X
        let zn = v1.X*v2.Y - v1.X*v2.Y
        Vector(xn,yn,zn)    
    static member (><) (v1:UnitVector, v2:Vector) =
        let xn = v1.Y*v2.Z - v1.Z*v2.Y
        let yn = -v1.X*v2.Z + v1.Z*v2.X
        let zn = v1.X*v2.Y - v1.X*v2.Y
        Vector(xn,yn,zn)
    static member (><) (v1:Vector, v2:UnitVector) =
        let xn = v1.Y*v2.Z - v1.Z*v2.Y
        let yn = -v1.X*v2.Z + v1.Z*v2.X
        let zn = v1.X*v2.Y - v1.X*v2.Y
        Vector(xn,yn,zn)
    // Ar two vectors Parallel?
    member this.IsParallel(v2:Vector, tol: float) =
        let cros = this><v2     // Cross product x
        let tol = tol |> LanguagePrimitives.FloatWithMeasure
        if abs(cros.X) < tol && abs(cros.Y) < tol && abs(cros.Z) > tol then true
        else false
and UnitVector(xv:float, yv:float , zv:float) =
    let modulo = float (sqrt(xv*xv+yv*yv+zv*zv)) 

    let x:float = xv/modulo //|> LanguagePrimitives.FloatWithMeasure
    let y:float = yv/modulo //|> LanguagePrimitives.FloatWithMeasure
    let z:float = zv/modulo //|> LanguagePrimitives.FloatWithMeasure
    member this.X 
     with get() = x
    member this.Y  
     with get() = y
    member this.Z 
     with get() = z
    
    // methods on unit vector that produces a Vector type
    static member (*) (k:float,v:UnitVector) = Vector(k*v.X,k*v.Y,k*v.Z)
    static member (+) (v1:UnitVector, v2:UnitVector) = Vector(v1.X + v2.X,v1.Y + v2.Y,v1.Z + v2.Z)
    // Scalar product
    static member (*) (v1:UnitVector, v2:UnitVector) = v1.X*v2.X+v1.Y*v2.Y+v1.Z*v2.Z
    // Cross product    -   Problem if v1 = v2
    static member (><) (v1:UnitVector, v2:UnitVector) =
        let xn = v1.Y*v2.Z - v1.Z*v2.Y
        let yn = -v1.X*v2.Z + v1.Z*v2.X
        let zn = v1.X*v2.Y - v1.X*v2.Y
        UnitVector(xn,yn,zn)
// still lacks the rotation matrix:
// http://math.stackexchange.com/questions/180418/calculate-rotation-matrix-to-align-vector-a-to-vector-b-in-3d

type Matrix(row:int, col: int) =
    let rotmat = Array2D.create row col 0.//.init 3 3 (0.)// (fun i j -> i+3*j)
    member this.RotMat
        with get() = rotmat

    static member (*) (m1:Matrix,m2:Matrix) =
        // (Array2D.length2 m1.RotMat) = (Array2D.length1 m2.RotMat) condition to be multiplied
        let mult = Array2D.create (Array2D.length1 m1.RotMat) (Array2D.length2 m2.RotMat) 0.
        //m1.RotMat.LongLength
        [|0..(Array2D.length1 m1.RotMat)-1|]
        |> Array.iter( fun i ->
           [|0..(Array2D.length2 m2.RotMat)-1|] |> Array.iter( fun j ->
                     [|0..(Array2D.length1 m2.RotMat)-1|]
                     |> Array.iter(fun k -> mult.[i,j]<- mult.[i,j] + (m1.RotMat.[i,k]*m2.RotMat.[k,j]) ) // column * row
                     )
           )
        mult
    static member (+) (m1:Matrix,m2:Matrix) =
        m1.RotMat |>Array2D.mapi(fun i j x->m2.RotMat.[i,j] + x)
    static member RotateVector(a:Vector,b:Vector) =
        let v = a><b
        let modprod = (a.Module()*b.Module())
        let s = v.Module()/modprod         // sin of angle
        let c = (a*b )/modprod           // Cos of angle
        let mult = (1.-c)/(s*s)
        let vx = Array2D.create 3 3 0.  // 3x3 zeroMatrix
        vx

// some tests with points
let pp = Point (3.,3.4,4.)
let pp2 = Point (4.,4.4,5.)
let pp3 = [1..5] |> List.map(fun x -> Point(float(x),float(x),float(x)))

// some tests with vectors
let vv =Vector (1.,0.,0.)
let uv =Vector (0.,1.,0.)
pp-pp2
uv*vv
vv.Module()
pp.Move(-0.2<m>,-0.4<m>,0.<m>)
2.*vv
2.*uv
Array2D.iteri

let rn = Array2D.init 3 3 (fun i j -> i+3*j)
let rn2 = Array2D.init 3 3 (fun i j -> i)
rn |>Array2D.mapi(fun i j x->rn2.[i,j] + x) //  printfn "%+A" (i,j))//rn 


// test with matrices
let m1 =Matrix(3,3)
let m2 =Matrix(3,3)
m1.RotMat.[1,0] <- 1.
m1.RotMat.[0,1] <- 1.
m1.RotMat.[1,1] <- 2.
m2.RotMat.[0,0] <- 1.
m1 + m2
Matrix.ID(3,3)
Matrix.RotateVector(uv,vv)
let vvv = vv >< vv
vv.Module()
