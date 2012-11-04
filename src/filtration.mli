open Util.Misc

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

val bs_partition : partition

val discrete_partition : int -> partition 

val copy_partition : partition -> partition

val coarsen_partition : ('a -> 'a) -> 'a array -> partition -> partition * int array

val pfind : partition -> int -> int

(* (\* Lower the rank of the specified nodes by 1, and undo any union *)
(*    operations/parent pointers that conflict with the new rank. we *)
(*    allow duplicate ids to be passed. *\) *)
(* let demote partition ids = *)
(*   let seen = Array.map (fun x -> false) partition.ranks in *)
(*     List.iter  *)
(*       begin fun id -> *)
(* 	if not seen.(id) then begin *)
(* 	  partition.ranks.(id) <- max 0 (partition.ranks.(id) - 1); *)
(* 	  seen.(id) <- true; *)
(* 	end; *)
(*       end *)
(*       ids; *)

(*     Array.iteri  *)
(*       begin fun i par -> *)
(* 	match par with  *)
(* 	    Some j -> *)
(* 	      if partition.ranks.(i) = partition.ranks.(j) then begin *)
(* 		partition.parents.(i) <- None; *)
(* 	      end *)
(* 	  | None -> () *)
(*       end *)
(*       partition.parents; *)
(*     () *)

(* let validate_partition partition = *)
(*   Array.iteri *)
(*     begin fun i par -> *)
(*       match par with  *)
(* 	  Some par -> *)
(* 	    assert(partition.ranks.(par) > partition.ranks.(i)); *)
(* 	| None -> *)
(* 	    () *)
(*     end *)
(*     partition.parents *)

(* let validate_filtration filtration = *)
(*   assert(filtration.depth = Array.length filtration.levels); *)
(*   Array.iter *)
(*     begin fun partition -> *)
(*       validate_partition partition; *)
(*     end *)
(*     filtration.levels; *)
(*   for i=0 to filtration.depth-2 do  *)
(*     for j = 0 to Array.length filtration.points-1 do  *)
(*       if filtration.levels.(i).parents.(j) = None then begin  *)
(* 	let j2 = filtration.maps.(i).(j) in *)
(* 	  if j = j2 then begin  *)
(* 	    assert(filtration.levels.(i+1).parents.(j) = None); *)
(* (\* 	    assert(filtration.levels.(i+1).ranks.(j) =  *\) *)
(* (\* 		filtration.levels.(i).ranks.(j)); *\) *)
(* 	  end *)
(* (\* 	  else begin  *\) *)
(* (\* 	    assert(filtration.levels.(i+1).parents.(j) = Some j2); *\) *)
(* (\* 	    assert(filtration.levels.(i+1).ranks.(j2) >=  *\) *)
(* (\* 		1 + filtration.levels.(i).ranks.(j2)); *\) *)
(* (\* 	  end *\) *)
(*       end   *)
(*       else begin  *)
(* 	assert(filtration.levels.(i+1).parents.(j) =  *)
(* 	    filtration.levels.(i).parents.(j)); *)
(* 	assert(filtration.levels.(i+1).ranks.(j) =  *)
(* 	    filtration.levels.(i).ranks.(j)); *)
(*       end *)
(*     done *)
    
(*   done *)

(* let rectify_maps f =  *)
(*   for i = 0 to f.depth-2 do  *)
(*     Array.iteri *)
(*       begin fun j par -> *)
(* 	f.maps.(i).(j) <- pfind f.levels.(i+1) j; *)
(*       end *)
(*       f.levels.(i+1).parents; *)
(*   done *)

(* Demote the specified nodes in the coarsest partition, and propagate
   changes downwards so that we are still a filtration. *)
val demote_filtration : 'a filtration -> int list -> unit

val top_level_ids : partition -> int array

val create_filtration : 'a array -> ('a -> 'a) array -> 'a filtration

val copy_filtration : 'a filtration -> 'a filtration

(* val make_filtration : (int * int) array -> int -> (int * int) filtration *)
