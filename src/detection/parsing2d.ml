open Util.Hops
open Util.Misc

module I = Image

type sid = int
type pid = int
type cid = int

type image_parse_data = {
  costs: (sid*pid*pid, float) hash;
  bestleftcost: (sid*pid, float) hash;
  split: (sid*pid*pid, (cid*pid) option) hash;  
  thisbest: (sid, pid*pid) hash;
  thisbestqual: (sid, float) hash;
}

type outside_parse_data = {
  ocosts: ((sid*pid*pid), float) hash;
  bestocost: ((sid*pid), float) hash;
}

type image_parse_tree = {
  nt: sid;
  bg: pid;
  en: pid;
  yield: (pid*pid) list;
(*   mutable simple: bool; *)
  left: image_parse_tree option;
  right: image_parse_tree option;
  qual: float;
  thesplit: (cid*pid) option;
}

type scene = {
  (* do we really want parsedata here? prob not... *)
  parsedata: image_parse_data;
  edgemap: bool Image.t;
  usedpixels: int Image.t;
  mutable directions: (int*int) array; (* q-p *)
  mutable parent_truecost: float;
  mutable iteration: int;
}


let new_outside_data (nsyms,npts) =
  let bigsize = min (nsyms*npts*npts) 100000 in 
  {ocosts = mkhash bigsize;
   bestocost = mkhash (nsyms*npts);
  }

let new_parse_data (nsyms,npts) = 
  let bigsize = min (nsyms*npts*npts) 100000 in 
  {costs = mkhash bigsize;
   bestleftcost = mkhash (nsyms*npts);
   split = mkhash bigsize;
   thisbest = mkhash nsyms;
   thisbestqual = mkhash nsyms;
  }

let new_scene edgemap ptruecost = 
  {iteration = -1; 
   parsedata = new_parse_data (1,1);
   edgemap = edgemap;
   usedpixels = I.map (fun x -> (-1)) edgemap;
   directions = [| |];
   parent_truecost = ptruecost;
  }


let copy_parse_data pdata = 
  {costs = Hashtbl.copy pdata.costs;
   bestleftcost = Hashtbl.copy pdata.bestleftcost;
   split = Hashtbl.copy pdata.split;
   thisbest = Hashtbl.copy pdata.thisbest;   
   thisbestqual = Hashtbl.copy pdata.thisbestqual;
  }

let copy_scene scene =
  {iteration = -1;
   parsedata = copy_parse_data scene.parsedata;
   edgemap = Image.copy scene.edgemap;
   usedpixels = Image.copy scene.usedpixels;
   directions = Array.copy scene.directions;
   parent_truecost = scene.parent_truecost;
  }
