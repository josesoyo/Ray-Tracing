module CreateRay

//  Here I will create the functions that define different types of ray generators

open Types.Algebra
open Types.types
open Random
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols


let NewRayCollimated (pos:Point) (normal:UnitVector) (sigma:float) (rMax:float) (nOfParticlesDisp:int) wvl =
    let rotpoint = Matrix.RotateVector(UnitVector(0.,0.,1.),normal)
    let rfPos() =  // position function

            SampDisk (rMax)
            |> fun x -> 
                let r, ph = fst x, snd x
                r*sin(ph), r*cos(ph)
            |> fun x -> Point(fst x,snd x,0.)
            |> fun x -> rotpoint.RotatePoint(x)
            |>  fun px -> px.MoveAndCreateNew(pos) 

    let rPos = rfPos()
    let rvect = // direction
        // collimated ray
        normal
  
    {
         Wavelenght = wvl;
         from = rPos; uvec = rvect;
         MaxLength = infi;
         OpticalPathTravelled = (sqrt(rPos.Z*rPos.Z+rPos.Y*rPos.Y)/17005.) |> LanguagePrimitives.FloatWithMeasure<m>;
         NumBounces = 0.; bounces = [];
         MaxDispersions = 3.;
         NumOfParticlesCreated = nOfParticlesDisp;
         FracOfRay = 1.;
         IndexOfRefraction = 1.
         PhaseModulation = [||]
    }


let AddPhaseModulationToRay (r:Ray) (phm:float[]) =
    {r with PhaseModulation=phm}