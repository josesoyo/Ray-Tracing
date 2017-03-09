module waveproduct
// the fact that it is not on the namespace will make it a little bit hidder than if it was on the namespace

open Types.Algebra
open Types.ObjectTypes
open Types.types


let Gauss_Plane_Wave_Product(d:SensorContent, dsc:disc,src_source:Source) = //roc:float, waist:float, diameter:float, power0:float) =
    // Perform the dot product between the gaussian beam of the interferometer and the puntual plane wave
    //
    // I need from the plane wave:
    //      - the direction and the 'weight'
    //      - Carefull: IF I AM USING THE RAY DIRECTLY THEN THE DIRECTION OF THE RAY IS NOT IN LOCAL COORDIANTES (well, neither the object would be...)
    //
    // From the gaussian:
    //      - encoded in Source type: waist (diameter), beamRadius, Beam Radius Of Curvature
    
    // sensor direction and origin
    let s_dir =  dsc.Normal           // sensor direction
    //    match src with 
    //    | Disc x -> x.Normal
    //    | _ -> failwith "The function is thought for disc sensors"
    let transmat = Matrix.RotateVector(s_dir,UnitVector(0.,0.,1.))   // rotate sensor direction to 0.,0.,1.
    
    let s_orig =  dsc.Centre         // sensor centre
    //    match src with 
    //    | Disc x -> x.Centre
    //    | _ -> failwith "The function is thought for disc sensors"
        
    // define the plane wave:
    let p_dir = transmat.RotateVector( d.Direction)                           // hitting direction in local coordinates
    let p_point = transmat.RotatePoint((d.Position-s_orig).ToPoint())         // hitting point in local coordinates
    
    if abs(p_point.Z) > 1e-10 then failwith "The z val of the value in the sensor should be zero"
    
    let f_frac = d.FracOfRay                                                   // we work with field  CAREFULL: IT IS NORMALIZED WITH RESPECT TO THE INPUT BEAM ON THE BENCH
    let plane_wave = f_frac*(transmat.RotateVector(p_dir))                                              // phase factors not included - phase goes to the fourier transform
    // define the gaussian wave
    let gaussian_wave = sqrt(src_source.Power)*exp(-(p_point.X*p_point.X+p_point.Y*p_point.Y)/src_source.Diameter/src_source.Diameter)     // Gaussian beam at defined point
    let gaussian_phase_OPL = src_source.Phase+((p_point.X*p_point.X+p_point.Y*p_point.Y)/(2.*src_source.RadiusOfCurvature))                // Phase of the gaussian beam at defined point
    abs(plane_wave.Z*gaussian_wave), gaussian_phase_OPL