open Util.Misc
open Printf
open Abstract
open Grammar

let _ =
  let gramfile = ref "NOGRAMFILE" in
  let famfile = ref "NOFAMFILE" in
  let countfile = ref "NOCOUNTFILE" in

  (* parse args *)
  let _ = Arg.parse ["-gramfile", Arg.Set_string gramfile, "File containing grammar.";
		     "-famfile", Arg.Set_string famfile, "File containing SDF.";
		     "-countfile", Arg.Set_string countfile, "File to output softcounts to.";]
    (placeholder "Arg.parse") ("./prog -gramfile GRAMFILE -famfile FAMFILE -countfile COUNTFILE")
  in

  let countfile = open_out_bin !countfile in
  let gram = (Marshal.from_channel (open_in !gramfile): Grammar.grammar) in
  let fam = (Marshal.from_channel (open_in !famfile): 
	       (Models.Simple.loc_sdata,Models.Simple.loc_cdata,unit) Abstract.frozen_grammar) in
  let soft = Retrain.get_soft_counts_parallel_worker gram fam Models.Simple.strat in
    Marshal.to_channel countfile soft [];
    close_out countfile;

    ()
