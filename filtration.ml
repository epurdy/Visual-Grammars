open Printf
open Util.Misc
module I = Image

(* Represented as a union-find over integer indices. It is not exactly
   a union-find, though, because we also want to support an operation
   that breaks sets apart again. (essentially, we can undo any
   particular union operation.) *)
type partition = {
  parents: int option array;
  ranks: int array;
}

type 'a filtration = {
  depth: int;
  points: 'a array;
  mutable levels: partition array; (* finest partition first *)
  mutable maps: int array array;
}

let bs_partition = { parents=[||]; ranks=[||] }

let discrete_partition n =
  {parents = Array.init n (fun x -> None);
   ranks = Array.init n (fun x -> 0);} (* what is base rank? 0? *)

let copy_partition part =
  {parents = Array.copy part.parents;
   ranks = Array.copy part.ranks}

let rec pfind partition i =
  begin match partition.parents.(i) with
      Some j -> pfind partition j
    | None -> i
  end

let coarsen_partition coarsen points partition =
  let parents = Array.copy partition.parents in
  let ranks = Array.copy partition.ranks in
  let map = Array.init (Array.length points) (fun id -> id) in 
    Array.iteri
      begin fun id p ->
	if parents.(id) = None then begin 
	  let cp = coarsen p in
	    if cp <> p then begin 
	      let newid = find cp points in
		parents.(id) <- Some newid;
		ranks.(newid) <- max ranks.(newid) (ranks.(id)+1);
		map.(id) <- newid;
	    end
	end
      end
      points;
    {parents = parents; ranks = ranks;}, map

(* Lower the rank of the specified nodes by 1, and undo any union
   operations/parent pointers that conflict with the new rank. we
   allow duplicate ids to be passed. *)
let demote partition ids =
  let seen = Array.map (fun x -> false) partition.ranks in
    List.iter 
      begin fun id ->
	if not seen.(id) then begin
	  partition.ranks.(id) <- max 0 (partition.ranks.(id) - 1);
	  seen.(id) <- true;
	end;
      end
      ids;

    Array.iteri 
      begin fun i par ->
	match par with 
	    Some j ->
	      if partition.ranks.(i) = partition.ranks.(j) then begin
		partition.parents.(i) <- None;
	      end
	  | None -> ()
      end
      partition.parents;
    ()

let validate_partition partition =
  Array.iteri
    begin fun i par ->
      match par with 
	  Some par ->
	    assert(partition.ranks.(par) > partition.ranks.(i));
	| None ->
	    ()
    end
    partition.parents

let validate_filtration filtration =
  assert(filtration.depth = Array.length filtration.levels);
  Array.iter
    begin fun partition ->
      validate_partition partition;
    end
    filtration.levels;
  for i=0 to filtration.depth-2 do 
    for j = 0 to Array.length filtration.points-1 do 
      if filtration.levels.(i).parents.(j) = None then begin 
	let j2 = filtration.maps.(i).(j) in
	  if j = j2 then begin 
	    assert(filtration.levels.(i+1).parents.(j) = None);
(* 	    assert(filtration.levels.(i+1).ranks.(j) =  *)
(* 		filtration.levels.(i).ranks.(j)); *)
	  end
(* 	  else begin  *)
(* 	    assert(filtration.levels.(i+1).parents.(j) = Some j2); *)
(* 	    assert(filtration.levels.(i+1).ranks.(j2) >=  *)
(* 		1 + filtration.levels.(i).ranks.(j2)); *)
(* 	  end *)
      end  
      else begin 
	assert(filtration.levels.(i+1).parents.(j) = 
	    filtration.levels.(i).parents.(j));
	assert(filtration.levels.(i+1).ranks.(j) = 
	    filtration.levels.(i).ranks.(j));
      end
    done
    
  done

let rectify_maps f = 
  for i = 0 to f.depth-2 do 
    Array.iteri
      begin fun j par ->
	f.maps.(i).(j) <- pfind f.levels.(i+1) j;
      end
      f.levels.(i+1).parents;
  done

(* Demote the specified nodes in the coarsest partition, and propagate
   changes downwards so that we are still a filtration. *)
let demote_filtration f ids =
  demote f.levels.(f.depth-1) ids;
  for i= f.depth-1 downto 1 do 
    printf "demote %d\n%!" i;

    List.iter
      begin fun id ->
	f.levels.(i-1).ranks.(id) <- min f.levels.(i-1).ranks.(id)
	  f.levels.(i).ranks.(id);
      end
      ids;

    Array.iteri
      begin fun j par ->
	match par with
	    Some par ->
	      if f.levels.(i-1).ranks.(j) = f.levels.(i-1).ranks.(par) then begin
		f.levels.(i-1).parents.(j) <- None;
	      end
	  | None -> ()
      end
      f.levels.(i-1).parents;
  done;
  rectify_maps f;
  validate_filtration f

let draw_partition partition points (width,height) =
  let sqsize = 5 in
  let colors = Array.map (fun x -> (Random.int 256, Random.int 256, Random.int 256)) partition.parents in
  let im = Image.create (sqsize*width) (sqsize*height) (255,255,255) in
    Array.iteri
      begin fun i (x,y) ->
	let j = pfind partition i in
	  for a=0 to sqsize-1 do
	    for b=0 to sqsize-1 do
	      I.set im colors.(j) (x*sqsize+a, y*sqsize+b);
	    done
	  done
      end
      points;
    im

let top_level_ids part =
  let ls = ref [] in
    Array.iteri 
      begin fun i par ->
	if par = None then begin 
	  ls := i :: !ls;
	end
      end
      part.parents;
    Array.of_list (List.rev !ls)


let create_filtration points coarsenings =
  let depth = 1 + (Array.length coarsenings) in
  let partitions = Array.init depth (fun x -> bs_partition) in
  let maps = Array.init (depth-1) (fun x -> [| |]) in
    partitions.(0) <- discrete_partition (Array.length points);
    Array.iteri 
      begin fun i coarsen ->
	let nextpart, nextmap = 
	  coarsen_partition coarsen points partitions.(i) 
	in
	  partitions.(i+1) <- nextpart;
	  maps.(i) <- nextmap;
      end
      coarsenings;

    let f = {
      depth = depth;
      points = points;
      levels = partitions;
      maps = maps;
    }
    in
      rectify_maps f;
      f

let copy_filtration f =
  {depth = f.depth;
   points = Array.copy f.points;
   levels = Array.map copy_partition f.levels;
   maps = Array.map Array.copy f.maps;
  }

let make_coarsenings nlvls =
  let coarsenings = 
    Array.init
      (nlvls-1)
      begin fun i ->
	let i = i+1 in
	let coarsenint x = (x asr i) lsl i in
	let coarsen (x,y) = (coarsenint x, coarsenint y) in
	  coarsen
      end
  in
    coarsenings

let make_filtration points nlvls = 
  create_filtration (points: (int*int) array) (make_coarsenings nlvls)
