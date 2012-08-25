open Printf
open Util.Misc
open Util.Hops 
open Util.Cops
open Abstract
open Grammar
open Graph
open Filtration
module I = Image

exception Parse_failure

let pf = 0.80 (* probability of observing an edge pixel in the foreground *)
let pb = 0.10 (* probability of observing an edge pixel in the background *)
let length_term = -. (log( (1. -. pf) /. (1. -. pb) )) (* negative log likelihood ratio *)
let edge_term = -. (log( pf *. (1. -. pb) /. ((1. -. pf) *. pb) )) (* negative log likelihood ratio *)

let dist2 (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
let canon p q = if p > q then q,p else p,q
let foi x = float_of_int x

type sid = int
type cid = int
type pid = int
type pt = int * int

type image_parse_tree = {
  nt: sid;
  bg: pid;
  en: pid;
  yield: (pid*pid) list;
  left: image_parse_tree option;
  right: image_parse_tree option;
  qual: float;
  thesplit: (cid*pid) option;
}

type parse_data = {
  costs: (sid*pid*pid, float) hash;
  bestleftcost: (sid*pid, float) hash;
  split: (sid*pid*pid, (cid*pid) option) hash;  
  thisbest: (sid, pid*pid) hash;
  thisbestqual: (sid, float) hash;
  ocosts: (sid*pid*pid, float) hash;
  bestocost: (sid*pid, float) hash;
  best_tri_cost: (cid*pid*pid*pid, float) hash;
}

question: is code slow because search is cubic, or because
best_tri_cost is getting unmanageably large?

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

(* current partition *)
type foo = {
  partition: partition;
  toplevelids: int array;
}

(* search state for reparsing *)
type thing = {
  edgemap: bool Image.t;
  usedpixels: int Image.t;
  mutable directions: (int*int) array;
  mutable parent_truecost: float;
  mutable iteration: int;
}

let new_outside_data (nsyms,npts) = {}
let new_parse_data (nsyms,npts) = {}
let new_thing edgemap ptruecost = {}
let copy_parse_data pdata = {}
let copy_thing thing = {}

add_direction
check_direction
draw_parse
harvest_best_tree
harvest_best_tree_overall
hash_of_parse_tree
print_parse_tree

voutside
  - gram
  - best_tri_cost
  - points
  - ranks
  - topnodes
  - masking_left
  - masking_right
  - pdata
  - odata

vinside
  - gram
  - best_tri_cost
  - usepts
  - points
  - ranks
  - topnodes
  - masking
  - masking2
  - masking3
  - data
  - showwork
  - prefix
  - dispim
  - name
  - ubound

new_insert_lexical_costs 
coarsen_lexical_costs
propagate_lexical_costs (call coarsen the requisite number of times)
find_best_coarse_init
find_best_coarse
newer_lift_coarse_soln
update_outside
doit (use filtration to find optimum with reuse)
get_conflict
true_cost
reparse


