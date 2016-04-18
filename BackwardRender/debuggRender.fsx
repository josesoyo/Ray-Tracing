open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

let direct = Vector(0.0,0.,-1.)
let infi:float<m> = (infinity |> LanguagePrimitives.FloatWithMeasure) 

let Ray = {
            Wavelenght=WaveLength(0.6e-6<m>);
            from= Camera.EyePoint; uvec= direct.ToUnitVector();                 // From and direction
            MaxLength = infi                              // Max distance can travell (should be infinite by defect)
            OpticalPathTravelled= 0.<m>;                   // Optical Path Length Modified after every step with the IOR
            NumBounces = 0; bounces= [];     // Num of bounces + the positions (Just in case for the future)
            NumOfParticles = 0;                               // Num of photos -> To split in a Lambertian surface, etc...
            IndexOfRefraction=1.
            }
let intersects = scene.Elements 
                |> Array.map(fun obj -> selectType(Ray,obj,true))//IntersectionOctree (Ray,obj.Octree,obj.Mesh))// Octree
                |> Array.collect(fun x -> match x with Some x -> [|x|] | None -> [||])
                  