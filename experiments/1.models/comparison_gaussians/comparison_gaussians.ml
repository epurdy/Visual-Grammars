open Printf

let _ = 
  let nsamples = ref 0 in
  let prefix = ref "" in
  let curves = ref [] in

  let add_curve fname =
    let c = Curve.load fname in
      curves := c :: !curves;
  in

  let _ = Arg.parse ["-prefix", Arg.Set_string prefix, "Prefix for string files";
		     "-nsamples", Arg.Set_int nsamples, "Number of samples"] 
    add_curve "foo" 
  in

  let curves = Array.of_list (List.rev !curves) in
  let gcdf = Gaussians.gaussian_curve_distro_family in
  let model, _ = gcdf.Distro.infer curves in

  let _ =
    assert (!nsamples > 0);
    assert (!prefix != "");
  in

  let samples = Array.init !nsamples
    begin fun i ->
      Gaussians.sample model
    end
  in
  let samples_no_variance = Array.init !nsamples
    begin fun i ->
      Gaussians.sample_no_variance model
    end
  in
  let samples_dec_variance = Array.init !nsamples
    begin fun i ->
      Gaussians.sample_dec_variance model 0.1
    end
  in

    Curve.save_all (fun x -> sprintf "%s.%04d.curve" !prefix x) samples;
    Curve.save_all (fun x -> sprintf "%s.%04d.novar.curve" !prefix x) samples_no_variance;
    Curve.save_all (fun x -> sprintf "%s.%04d.decvar.curve" !prefix x) samples_dec_variance
