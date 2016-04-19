namespace BackwardRender

module BackTypes =

    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    open Types.Algebra
    open Types
    open Types.types
    open Types.ObjectTypes
    open RayTracing.RayStructureIntersection

    type Camera = {EyePoint:Point; LookAt:Vector; Up:UnitVector;PixNumH:int;PixNumW:int;PixSize:float}
    
    type Plight = {origin:Point; intensity:float}
    

    type scene = {Camera:Camera;  Elements: Object[]; Materials: System.Collections.Generic.IDictionary<string,Material> ; Plights:Plight[]}

    let DefaultBackWardMaterial = {R=0.;T=0.; n=(1.,WaveLength(0.6e-6<m>););LambPPM= 0.;MatName= ""}
