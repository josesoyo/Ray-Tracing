module BBoxMethods
//  List of functions inside this module:
//  *BoxofIntersection:     Computes the intersection BBox of two different bbox
//  *BoxofUnion:            Computes the union of two boxes
// 

open Types.ObjectTypes 


let BoxofIntersection (box1:BBox, box2:BBox) =
    //Checks which one is the intersected space between two boxes
    // box1 = mesh
    let sphBox = box1
    let box = box2
    let Nmin = 
        let xmin =
            if sphBox.Pmin.[0] > box.Pmin.[0] then sphBox.Pmin.[0]
            else box.Pmin.[0]
        let ymin =
            if sphBox.Pmin.[1] > box.Pmin.[1] then sphBox.Pmin.[1]
            else box.Pmin.[1]
        let zmin =
            if sphBox.Pmin.[2] > box.Pmin.[2] then sphBox.Pmin.[2]
            else box.Pmin.[2]
        [|xmin;ymin;zmin|]
    let Nmax = 
        let xmax =
            if sphBox.Pmax.[0] < box.Pmax.[0] then sphBox.Pmax.[0]
            else box.Pmax.[0]
        let ymax =
            if sphBox.Pmax.[1] < box.Pmax.[1] then sphBox.Pmax.[1]
            else box.Pmax.[1]
        let zmax =
            if sphBox.Pmax.[2] < box.Pmax.[2] then sphBox.Pmax.[2]
            else box.Pmax.[2]
        [|xmax;ymax;zmax|]
    {Pmin= Nmin; Pmax =Nmax}

let BoxofUnion (box1:BBox, box2:BBox) =
    //Compute the union between two boxes
    let sphBox = box1
    let box = box2
    let Nmin = 
        let xmin =
            if sphBox.Pmin.[0] < box.Pmin.[0] then sphBox.Pmin.[0]
            else box.Pmin.[0]
        let ymin =
            if sphBox.Pmin.[1] < box.Pmin.[1] then sphBox.Pmin.[1]
            else box.Pmin.[1]
        let zmin =
            if sphBox.Pmin.[2] < box.Pmin.[2] then sphBox.Pmin.[2]
            else box.Pmin.[2]
        [|xmin;ymin;zmin|]
    let Nmax = 
        let xmax =
            if sphBox.Pmax.[0] > box.Pmax.[0] then sphBox.Pmax.[0]
            else box.Pmax.[0]
        let ymax =
            if sphBox.Pmax.[1] > box.Pmax.[1] then sphBox.Pmax.[1]
            else box.Pmax.[1]
        let zmax =
            if sphBox.Pmax.[2] > box.Pmax.[2] then sphBox.Pmax.[2]
            else box.Pmax.[2]
        [|xmax;ymax;zmax|]
    {Pmin= Nmin; Pmax =Nmax}