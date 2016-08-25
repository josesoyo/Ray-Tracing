module materials
#r @" /Types/bin/Debug/Types.dll"
open Types.types
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

let steel_low_angles = [|0..60|] 
                        |> Array.map(fun x -> 
                                            let mname = "Steel_"+string(x)
                                            //printfn "the iteration is: %s" mname
                                            {MatName= mname; R=0.01; T=0.0;LambPPM= 0.4;
                                            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))}
                                    )
                                    
let steel_high_angles =
    [|{MatName= "Steel_61"; R=0.01; T=0.;LambPPM= 0.41;
        n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_62"; R=0.015; T=0.;LambPPM= 0.41;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_63"; R=0.015; T=0.;LambPPM= 0.41;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_64"; R=0.015; T=0.;LambPPM= 0.41;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_65"; R=0.02; T=0.;LambPPM= 0.415;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_66"; R=0.02; T=0.;LambPPM= 0.415;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_67"; R=0.02; T=0.;LambPPM= 0.415;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_68"; R=0.02; T=0.;LambPPM= 0.415;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_69"; R=0.02; T=0.;LambPPM= 0.415;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_70"; R=0.02; T=0.;LambPPM= 0.42;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_71"; R=0.025; T=0.;LambPPM= 0.42;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_71"; R=0.025; T=0.;LambPPM= 0.42;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_72"; R=0.025; T=0.;LambPPM= 0.42;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_73"; R=0.03; T=0.;LambPPM= 0.42;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_74"; R=0.03; T=0.;LambPPM= 0.42;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_75"; R=0.03; T=0.;LambPPM= 0.42;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};
    
        {MatName= "Steel_76"; R=0.04; T=0.;LambPPM= 0.42;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};
        {MatName= "Steel_77"; R=0.06; T=0.;LambPPM= 0.425;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};
   
        {MatName= "Steel_78"; R=0.08; T=0.;LambPPM= 0.425;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};
        {MatName= "Steel_79"; R=0.10; T=0.;LambPPM= 0.425;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};
        {MatName= "Steel_80"; R=0.12; T=0.;LambPPM= 0.43;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_81"; R=0.16; T=0.;LambPPM= 0.42;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_82"; R=0.19; T=0.;LambPPM= 0.42;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_83"; R=0.22; T=0.;LambPPM= 0.41;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_84"; R=0.26; T=0.;LambPPM= 0.41;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_85"; R=0.30; T=0.;LambPPM= 0.40;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_86"; R=0.40; T=0.;LambPPM= 0.35;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};
 
        {MatName= "Steel_87"; R=0.5; T=0.;LambPPM= 0.30;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_88"; R=0.7; T=0.;LambPPM= 0.20;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_89"; R=0.9; T=0.;LambPPM= 0.08;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))};

        {MatName= "Steel_90"; R=1.; T=0.;LambPPM= 0.;
            n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>))} 
    |]
let mout = [|{MatName= "Mirror"; R=1.; T=0.;
                    n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 1e-6};
               {MatName= "Mirror_Half"; R=1.; T=0.;
                    n=(2.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.};
               {MatName= "BeamSplitter"; R=0.5; T=0.5;
                    n=(1.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.};
                {MatName= "Tube"; R=0.95; T=0.0; 
                    n=(1.3,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.05};
                {MatName= "air"; R=0.; T=1.; 
                    n=(1.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.};
                {MatName= "Baffle_SiC"; R=0.19526; T=0.;               // 0.  19526              //   false(0.26)  Silicon Carbide - http://refractiveindex.info/
                    n=(2.5835,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 0.00005 };//  50e-6}; no0.74
                {MatName= "Baffle_DLC"; R=1e-6; T=0.;                               //     Diamon-Like Carbon   - not know well
                    n=(1.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 500e-6};
                {MatName= "Baffle_AbsGlass"; R=1e-2; T=0.;                               //     Silicon Carbide
                    n=(1.,WaveLength(1.06e-6 |> LanguagePrimitives.FloatWithMeasure<m>));LambPPM= 500e-6}


                |]
// the absortion of steel is dependent of the treatment and it varies from 60 to 40%, so I do the intermediate case
let steel_low_angles2 = steel_low_angles |> Array.map(fun x -> {x with LambPPM = x.LambPPM+0.2})
let steel_high_angles2 = steel_high_angles  |> Array.map(fun x ->{x with LambPPM = x.LambPPM+0.2}) 


let mat = dict ( (Array.concat [mout; steel_low_angles2; steel_high_angles2])|> Array.map(fun x -> (x.MatName,x)))
printfn "Be careful with the definition of the materials\n\tSpecially for the Baffle materials, the info is not completed\n\n\tSilicon Carbide\n\tDiamon-Like Carbon\n\tAbsorbent Glass\n\tNo oxidized steel"