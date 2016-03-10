namespace Types

module Algebra =
    // Types added:
    // Point(x,y,z)
    //      - this.move(dx,dy,dz)
    // Vector(v,u,w) & UnitVector(v,u,w)
    //      - sum:      +
    //      - prod      *  scalar*vect
    //      - dotprod:  *  v1*v2
    //      - crospro   >< v1^v1
    //      - this.IsParallelTo(v2, tolerance)


    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

    type Point(xp, yp , zp) =
        // must be in metre, but problematic if I use it
        //let mutable x:float<m> = xp |> LanguagePrimitives.FloatWithMeasure
        let mutable x:float = xp
        //let mutable y:float<m> = yp |> LanguagePrimitives.FloatWithMeasure
        let mutable y:float = yp 
        //let mutable z:float<m> = zp |> LanguagePrimitives.FloatWithMeasure
        let mutable z:float = zp 

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
        member this.ToUnitVector() = UnitVector(this.X,this.Y,this.Z)
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
            let zn = v1.X*v2.Y - v1.Y*v2.X
            Vector(xn,yn,zn)    
        static member (><) (v1:UnitVector, v2:Vector) =
            let xn = v1.Y*v2.Z - v1.Z*v2.Y
            let yn = -v1.X*v2.Z + v1.Z*v2.X
            let zn = v1.X*v2.Y - v1.Y*v2.X
            Vector(xn,yn,zn)
        static member (><) (v1:Vector, v2:UnitVector) =
            let xn = v1.Y*v2.Z - v1.Z*v2.Y
            let yn = -v1.X*v2.Z + v1.Z*v2.X
            let zn = v1.X*v2.Y - v1.Y*v2.X
            Vector(xn,yn,zn)
        // Ar two vectors Parallel?
        member this.IsParallelTo(v2:Vector, tol: float) =
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
        let rotmat = Array2D.create row col 0.//init as zero matrix
        member this.RotMat
            with get() = rotmat
        
        static member ID(row:int, col: int) =
            let rotmat = Matrix(row, col)
            if row = col then
                ([|0..row-1|], [|0..row-1|]) ||> Array.iter2(fun x y  -> rotmat.RotMat.[x,y] <- 1.)
                rotmat
            else if row > col then
                let row = col
                ([|0..row-1|], [|0..row-1|])||> Array.iter2(fun x y  -> rotmat.RotMat.[x,y] <- 1.)
                rotmat
            else //  if row < col then
                let col = row
                ([|0..row-1|], [|0..row-1|])||> Array.iter2(fun x y  -> rotmat.RotMat.[x,y] <- 1.)
                rotmat
           
        static member (*) (v1: float,m2:Matrix) =
            let mult = Matrix ((Array2D.length1 m2.RotMat), (Array2D.length2 m2.RotMat) )
            [|0..(Array2D.length1 m2.RotMat)-1|]
            |> Array.iter(fun x -> 
                [|0..(Array2D.length2 m2.RotMat)-1|]|> Array.iter(fun y -> mult.RotMat.[x,y] <- v1*m2.RotMat.[x,y])   
             )
            mult

        static member (*) (m1:Matrix,m2:Matrix) =
            // (Array2D.length2 m1.RotMat) = (Array2D.length1 m2.RotMat) condition to be multiplied
            let mult = Matrix ((Array2D.length1 m1.RotMat), (Array2D.length2 m2.RotMat) )
            //m1.RotMat.LongLength
            [|0..(Array2D.length1 m1.RotMat)-1|]
            |> Array.iter( fun i ->
                [|0..(Array2D.length2 m2.RotMat)-1|] |> Array.iter( fun j ->
                            [|0..(Array2D.length1 m2.RotMat)-1|]
                            |> Array.iter(fun k -> mult.RotMat.[i,j]<- mult.RotMat.[i,j] + (m1.RotMat.[i,k]*m2.RotMat.[k,j]) ) // column * row
                            )
                )
            mult
        static member (+) (m1:Matrix,m2:Matrix) =
            let ms = Matrix ((Array2D.length1 m1.RotMat), (Array2D.length2 m1.RotMat) )
            ms.RotMat |>Array2D.iteri(fun i j x-> (ms.RotMat.[i,j] <- m2.RotMat.[i,j] +  m1.RotMat.[i,j]))
            ms
        static member RotateVector(a:Vector,b:Vector) =
            // Method valid for 3X3 matrix
            let ve = a><b
            let modprod = (a.Module()*b.Module())
            let s = ve.Module()/modprod         // sin of angle
            let c = (a*b )/modprod           // Cos of angle
            if s = 0. && c = 1. then Matrix.ID(3,3) // Case are the same vector
            else
                let mult = (1.-c)/(s*s)
                let vx = Matrix(3,3)// 3x3 zeroMatrix
                vx.RotMat.[1,0] <- ve.Z
                vx.RotMat.[0,1] <- -ve.Z
                vx.RotMat.[2,0] <- -ve.Y
                vx.RotMat.[0,2] <- ve.Y
                vx.RotMat.[1,2] <- -ve.X
                vx.RotMat.[2,1] <- ve.X
                
                let cuadrado = mult*vx*vx
                let id = Matrix.ID(3,3)
                (id + vx + cuadrado) //+ (mult*cuadrado)
                //(vx,cuadrado, mult)
         

