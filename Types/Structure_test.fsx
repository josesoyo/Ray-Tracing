#load @"C:\Users\Jose M. Gonzalez\OneDrive\Phd\render\ray casting\Sample parts for version 2\Library1\Types\Algebra.fs"
// To understand how structures work
open Types.Algebra
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


//
// Invert the matrix in a f# real style currying

let nmat = Matrix.ID(3,3)
nmat.RotMat.[0,2] <- 3.
nmat.RotMat.[2,0] <- 4.
//nmat.RotMat.[0,1] <- 1.
nmat.RotMat.[1,0] <- 3.
//nmat.RotMat.[0,0] <- 0.
nmat.RotMat.[2,1] <- 0.5
nmat.Determinant3X3()
nmat.Minor(1,0)
nmat.Minor(0,1)
nmat.Minor(1,2)
nmat.RotMat.[1,0]
let nmat2 = nmat.Invert3X3()
nmat*nmat2                      // ID

// Concatenate functions
let square a =  a*a
let s1 a b = a*b
let quad c = square >> s1 c
quad 1 3
(3.)**2.5
snd(1,2)


type Disk(c:Point, rad:float, nrm:UnitVector) = 
  let center = c
  let radius = rad
  let D = -(nrm*(c.ToVector()))
  member this.Centre with get() = center
  member this.Radius with get() = radius
  member this.ConstantOfAPlane with get() = D

let p0 = Point (1.,4.,5.)
let norm = UnitVector(0.,1.,0.)
let d0 = Disk(p0,1.,norm)
d0.ConstantOfAPlane
.Centre
