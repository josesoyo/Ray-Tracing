module BBoxMethods
//  List of functions inside this module:
//  *BoxofIntersection:     Computes the intersection BBox of two different bbox
//  *BoxofUnion:            Computes the union of two boxes
//  *BoxBoxIntersection:    Checks if two bbox have intersection -> bool 
// 

open Types.ObjectTypes 
open Types.Algebra

let BoxofIntersection (box1:BBox, box2:BBox) =
    //Checks which one is the intersected space between two boxes
    // box1 = mesh
    let sphBox = box1
    let box = box2
    let Nmin = 
        let xmin =
            if sphBox.Pmin.X > box.Pmin.X then sphBox.Pmin.X
            else box.Pmin.X
        let ymin =
            if sphBox.Pmin.Y > box.Pmin.Y then sphBox.Pmin.Y
            else box.Pmin.Y
        let zmin =
            if sphBox.Pmin.Z > box.Pmin.Z then sphBox.Pmin.Z
            else box.Pmin.Z
        Point(xmin,ymin,zmin)
    let Nmax = 
        let xmax =
            if sphBox.Pmax.X < box.Pmax.X then sphBox.Pmax.X
            else box.Pmax.X
        let ymax =
            if sphBox.Pmax.Y < box.Pmax.Y then sphBox.Pmax.Y
            else box.Pmax.Y
        let zmax =
            if sphBox.Pmax.Z < box.Pmax.Z then sphBox.Pmax.Z
            else box.Pmax.Z
        Point(xmax,ymax,zmax)
    {Pmin= Nmin; Pmax =Nmax}

let BoxofUnion (box1:BBox, box2:BBox) =
    //Compute the union between two boxes
    let sphBox = box1
    let box = box2
    let Nmin = 
        let xmin =
            if sphBox.Pmin.X < box.Pmin.X then sphBox.Pmin.X
            else box.Pmin.X
        let ymin =
            if sphBox.Pmin.Y < box.Pmin.Y then sphBox.Pmin.Y
            else box.Pmin.Y
        let zmin =
            if sphBox.Pmin.Z < box.Pmin.Z then sphBox.Pmin.Z
            else box.Pmin.Z
        Point(xmin,ymin,zmin)
    let Nmax = 
        let xmax =
            if sphBox.Pmax.X > box.Pmax.X then sphBox.Pmax.X
            else box.Pmax.X
        let ymax =
            if sphBox.Pmax.Y > box.Pmax.Y then sphBox.Pmax.Y
            else box.Pmax.Y
        let zmax =
            if sphBox.Pmax.Z > box.Pmax.Z then sphBox.Pmax.Z
            else box.Pmax.Z
        Point(xmax,ymax,zmax)
    {Pmin= Nmin; Pmax =Nmax}


let BoxBoxIntersection (box1:BBox, box2:BBox) =
    // Check if two boxes have not null intersection
    // Check that min or max are OUT the minmax of the other box

    (*  Algorithm for box box intersection:
        - Conditions there's no intersection

	    return NOT (
		    (Rect1.Bottom < Rect2.Top) OR  ----> Top is lower value
		    (Rect1.Top > Rect2.Bottom) OR
		    (Rect1.Left > Rect2.Right) OR  
		    (Rect1.Right < Rect2.Left) )
     ------------------------ 
     My case
            Rect1 = box1 Rect2 = box2
            Right = max         Left = min
    *)
    let xlim =
        // If true they don't intersect on X axis
        if box1.Pmax.X < box2.Pmin.X || box1.Pmin.X > box2.Pmax.X then true
        else false

    let ylim =
        // If true they don't intersect on y axis
        if box1.Pmax.Y < box2.Pmin.Y || box1.Pmin.Y > box2.Pmax.Y then true
        else false

    let zlim =
        // If true they don't intersect on z axis
        if box1.Pmax.Z < box2.Pmin.Z || box1.Pmin.Z > box2.Pmax.Z then true
        else false

    if xlim || ylim || zlim then false
    else true
