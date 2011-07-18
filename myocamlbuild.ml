open Printf
open Ocamlbuild_plugin;;
open Command;;

dispatch begin function
  | After_rules ->
      Pathname.define_context "util" ["util"; "vlib-ocaml"];

      rule "sanity check generation" ~deps:["%.ml.depends"; "%.native"] ~prod:"%.sanity" 
	begin fun env builder -> 
	  let nat = env "./%.native" in
	  let dir = env "%.d" in
	  let log = env "%.log" in
	    Seq[Cmd (Sh (sprintf "rm -rf sanity/%s" dir));
		Cmd (S[A "mkdir";
		       A "-p";
		       P dir]);
		Cmd (S[P nat;
		       Sh ">";
		       P log]);
		Cmd (S[A "mkdir";
		       A "-p";
		       A "sanity"]);
		mv dir "sanity";
		mv log "sanity";
		Cmd (Sh ("pwd > DEBUG_PWD"))
	       ]
	end;

      ocaml_lib ~extern:true ~dir:"+cairo" "cairo";
      ocaml_lib "vlib-ocaml/vlib";
  | _ -> ()
end;;
