open Util.Misc
open Printf

let _ = 
  let gram_name = ref "" in
  let qual_name = ref "" in
  let validation_curves = ref [] in

  let add_curve fname = (validation_curves := fname :: !validation_curves) in

  let _ = Arg.parse [
    "-gram", Arg.Set_string gram_name, "Input grammar.";
    "-qual", Arg.Set_string qual_name, "Place to write qual.";]
    add_curve
    "./prog -input in.gram -qual out.qual curve1 curve2 curve3 ..."
  in

  let infile = open_in !gram_name in
  let qualfile = open_out !qual_name in

  let validation_curves = Array.of_list (List.rev !validation_curves) in
  let validation_curves = Array.map Curve.load validation_curves in
  let validation_families = Array.map
    begin fun c -> 
      let fam = Sdf.make_full_family (Array.length c) in
	Models.EM.add_curve_data_to_family c fam
    end 
    validation_curves
  in

  let gram = (Marshal.from_channel infile: Grammar.grammar) in

  let totqual = ref 0. in
    Array.iter
      begin fun fam ->
	let inside = Parsing.inside gram fam Models.EM.strat in
	  totqual := !totqual +. inside.Parsing.qual;
      end
      validation_families;

    
    fprintf qualfile "%f\n" !totqual;

    close_in infile;
    close_out qualfile
