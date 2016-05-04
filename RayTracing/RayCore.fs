namespace RayTracing

module intersections =
   open Types.Algebra
   open Types.types
   open Types
   open Types.ObjectTypes
   open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    
   //////////////////////////
   //
   // Intersection for Meshes
   // Intersection of a Square primitive used on Sensor for fowrard ray tracing
   //
   /////////////////////////
   //type group = {Name:string; triangles: int list [];  Normals: UnitVector [];Bbox:BBox; MatName:string}
   //type mesh = {Vertices:Point [] ; VNormals:UnitVector []; groups: group [] ;Bbox:BBox}
   
   let intersec_mesh (ray:Ray,  mesh:mesh, triangle:int list,nrmi:UnitVector,matName:string ,shape:char)= // normal is only passing
        // Method 2 from PBRT book eq 3.5
        let nodes = mesh.Vertices // *** Is wasting memory --> It copyes the direction, no waste memory *** I can: mesh.Vertices.[n0]
        let nrm =   // check that the normal is on the same side as the ray comes
            if nrmi*ray.uvec < 0. then nrmi
            else nrmi.Negate()
        let (n0,n1,n2) = (triangle.[0]-1, triangle.[1]-1, triangle.[2]-1)
        let (u0u1,u0u2) = (nodes.[n1]-nodes.[n0],nodes.[n2]-nodes.[n0]) 

        let raydir = ray.uvec
        // A couple of def:
        let s = ray.from - nodes.[n0]//ray.from
        let s1 = raydir >< u0u2 //dxe2 = ray.ray X u0u2 CrossProduct
        let s2 = s >< u0u1// sxe1 = V3D(ray.from - u0) X u0u1 CrossProduct
        // Test to check interception
        let s1Dote1 = s1*u0u1 //DotProduct
        let u = (s1*s)/ s1Dote1
        let v = (s2*raydir)/s1Dote1
        let logic =
            if shape = 't' then // intersection with a triangle condition
                if (u>0. && v>0. && (u+v)<=1.) then true
                else false
            elif shape = 's' then // intersection with a square condition - PArallelograms (Not trapezoids)
                if (u>0. && v>0. && u<=1. && v <= 1.) then true
                else false
            else 
                printfn "ERROR in the definition of the shape"
                false
        if logic then
            let t1:float<m> = ((s2*u0u2) / (s1Dote1)) |> LanguagePrimitives.FloatWithMeasure// <m>
            if t1 >  ray.MaxLength || t1 <= 1e-5<m> then [||] 
            // The collision cannot be further than the light when we do a shadow
            // The equal is because in the case s1Dote1 = 0 degenerate and there's no collision (t= infinity)
            // generates a problem in multiple transmision/reflection if ray.lenght is not inf intersecting not for shadow
            // Problem solved?
            else
                // t1 < dist (light- point) case shadow
                let newRay = {ray with OpticalPathTravelled = (ray.IndexOfRefraction*t1+ ray.OpticalPathTravelled)}
                //let newRay = {uvec=ray.uvec; from = ray.from;MaxLength = ray.length; 
                //              OpticalPathTravelled = (t1+ ray.OpticalPathTravelled)}
                let PIntersect = (u*u0u1 + v*u0u2).ToPoint()
                PIntersect.Move(nodes.[n0])
                //let PIntersect = Point3D( VIntersect.X + nodes.[n0].X,VIntersect.Y + nodes.[n0].Y,VIntersect.Z + nodes.[n0].Z )
                if t1 < 1e-6<m> && t1 > 1e-10<m> then
                    printfn "NOOO"
                //if Triangles.Length = 6 then
                //
                //  Interpolated normal from vertices normals
                //let nrm = (((1.-(u+v))*mesh.VNormals.[3]) + (u*mesh.VNormals.[4]) + (v*mesh.VNormals.[5])).ToUnitVector()
                 
                //else
                if (triangle.Length) = 3 then printfn "ERROR on triangles definition with the normal"
                //
                //  Up to now I haven't add that a mesh can be a sensor
                [|{ normal=nrm; point=PIntersect; ray=newRay;MatName=matName; t=t1}|]//; ObjectSensor=None}|]
        else
            [||]
            //type Intersection_mesh = { normal:Vector3D; point:Point3D; ray:RayFrom; mesh:mesh;t:float}

   // Prepared to accept triangles and squares as a primitive for intersection. 
   //Squares should reduce the computation time
   let intersec_tri (ray, mesh,triangle,nrm,matName)= intersec_mesh (ray, mesh,triangle,nrm,matName,'t')
   let intersec_square (ray, mesh,triangle,nrm,matName)= intersec_mesh (ray, mesh,triangle,nrm,matName,'s') // Parallelograms
   //
   
   
   
   let intersect_cyl(ray:Ray,cylinder:cylinder) =
        // intersection between Ray and a cylinder

        // Transform ray to object space - Origin and then rotate to align with axis of z cylinder
        let newRayOriginObject = cylinder.World2Obj.RotatePoint(  (ray.from-cylinder.Origin).ToPoint() )
        let newRayDirObject = cylinder.World2Obj.RotateVector(ray.uvec)                   // Rotate direction
        //Compute cylinder quadratic coeficients
        let A = newRayDirObject.X*newRayDirObject.X+newRayDirObject.Y*newRayDirObject.Y
        let B = 2.*(newRayDirObject.X*newRayOriginObject.X+newRayDirObject.Y*newRayOriginObject.Y)
        let C = newRayOriginObject.X*newRayOriginObject.X+newRayOriginObject.Y*newRayOriginObject.Y-float cylinder.Radius*float cylinder.Radius 

        //Solve equation  for t values
        let disc = B*B-4.*A*C

        if disc < 0. then [||]
        else 
            let sdisc = sqrt(disc)
            let t1 = 0.5*(-B + sdisc)/A |> LanguagePrimitives.FloatWithMeasure<m>  // A is not normalized since A not Mod(RaydirObjec)
            let t2 = 0.5*(-B- sdisc)/A  |> LanguagePrimitives.FloatWithMeasure<m>
            let z1 = (newRayOriginObject + float(t1)*newRayDirObject) //
            let z2 = (newRayOriginObject + float(t2)*newRayDirObject) //
            // Create the intersections
            //
            //
            let inter1 = 
                if (z1.Z < float cylinder.Zmax) && (z1.Z >= 0.) then //float cylinder.Zmin 
                    let normalt1 = cylinder.Obj2World.RotateVector(Vector(z1.X,z1.Y,0.))//.Normalize()
                    let ray1 =  {ray with OpticalPathTravelled = (ray.IndexOfRefraction*t1+ray.OpticalPathTravelled)}
                    let z1real = 
                        let z1rot = cylinder.Obj2World.RotatePoint(z1)
                        Point(z1rot.X+cylinder.Origin.X, z1rot.Y+cylinder.Origin.Y, z1rot.Z+cylinder.Origin.Z)
                    [|{ normal=normalt1.ToUnitVector() ; point=z1real; ray=ray1;MatName=cylinder.MaterialName; t=t1} |]//; 
                    //    ObjectSensor= if cylinder.Sensor.Exists then Some(Cylinder(cylinder))
                    //                  else None
                    //    }|]
                    //[{ normal = normalt1 ; point = z1real; ray = ray1 ; material=cylinder.material; t=t1;Nsamples=nsamples}]
                else [||]
            let inter2 = 
                if (z2.Z < float cylinder.Zmax) && (z2.Z >= 0.) then 
                    let normalt2 = cylinder.Obj2World.RotateVector(Vector(z2.X,z2.Y,0.))
                    let ray2 =  {ray with OpticalPathTravelled= (ray.IndexOfRefraction*t2+ray.OpticalPathTravelled)}
                    let z2real = 
                        let z2rot = cylinder.Obj2World.RotatePoint(z2)//.TransformBy(m=)
                        Point(z2rot.X+cylinder.Origin.X, z2rot.Y+cylinder.Origin.Y, z2rot.Z+cylinder.Origin.Z)
                    [|{ normal=normalt2.ToUnitVector(); point=z2real; ray=ray2;MatName=cylinder.MaterialName; t=t2}|]//; 
                    //    ObjectSensor= if cylinder.Sensor.Exists then Some(Cylinder(cylinder))
                    //                  else None
                    //    }|]
                    //[{ normal = normalt2 ; point = z2real; ray = ray2 ; material=cylinder.material; t=t2;Nsamples=nsamples}]
                else [||]
            Array.append inter1 inter2
    
   //type intermediateIntersection = {normal:UnitVector; point:Point;ray:Ray;MatName:string;t:float<m> }  // To use the intersect sphere
   let intersect_Sphere(ray:Ray,centre:Point,rad:float<m>, material:string) =
    // Intersction of a ray with a sphere comming from a cylinder
    let s = ray.from - centre
    let sv = s*ray.uvec
    let ss = s*s
    let adRad = float rad       // Adimensional Radius
    let discr = sv*sv - ss + adRad*adRad
    if discr < 0.0 then [||]
    else
       let t1 , t2 = (-sv + sqrt(discr))|> LanguagePrimitives.FloatWithMeasure<m>, (-sv - sqrt(discr)) |> LanguagePrimitives.FloatWithMeasure<m>
       let travel1, travel2 = ray.OpticalPathTravelled + ray.IndexOfRefraction*t1,  ray.OpticalPathTravelled + ray.IndexOfRefraction*t2
       let ray1, ray2 = {ray with OpticalPathTravelled = travel1}, {ray with OpticalPathTravelled = travel2}
       let point1, point2 = (ray.from + float(t1)*ray.uvec), (ray.from + float(t2)*ray.uvec)
       let dnormal1, dnormal2 = point1-centre, point2 - centre
       [|{normal = dnormal1.ToUnitVector();point= point1; ray= ray1; MatName = material; t= t1} ;
         {normal = dnormal2.ToUnitVector();point= point2; ray= ray2; MatName = material; t= t2} |]

   type intermediate_In_ShpLens = {Inter:Intersection; Cond:bool}  // definition for intersect_SphSurfaceLens bucle

   let intersect_SphSurfaceLens(ray:Ray,sLens:SphSurfaceLens):Intersection[]  =   
    // Intersection of a Ray with the surface of a spherical lens

    // Process: 1 - intersect with the sphere; 2 - check the conditions for the part of the sphere
    let isphere = intersect_Sphere(ray,sLens.SphCentre,sLens.RadiusOfCurvature, sLens.MaterialName)
    match isphere with 
    | [||] -> [||]
    | _ -> let intersec_costh = isphere            // Cosinus between the normal of the lens(side of the lens) and the side in which the intersection happened
                                 |> Array.map(fun x -> sLens.Axis*x.normal)
           let cond (costh:float) =
            // check if the intersection is done out of the part of the sphere that forms the lens
            if costh <= 1. && costh > sLens.CosMin then true
            else false

           let normalConcave(intersect: Intersection) (bol:bool) =
            if sLens.Convex then intersect
            else
                {intersect with normal = intersect.normal.Negate()}
           
           // return the intersections

           Array.map2(fun y x -> {Inter= x; Cond=(cond y)}) intersec_costh isphere // check if it really intersects creating an 'intermediate' type 
           |> Array.filter(fun x -> x.Cond)         // filter those points in which it really intersects
           |> Array.map(fun x -> x.Inter)           // map the intersection
           //|> Array.map(fun  x -> {normal= x.normal; point= x.point;ray= x.ray;MatName= x.MatName;t= x.t;
           //                        ObjectSensor= if sLens.Sensor.Exists then Some(SurfaceLens(sLens))
           //                                      else None
           //                        } 
           //             )
                                    
