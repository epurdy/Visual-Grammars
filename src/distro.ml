type ('thing, 'distro, 'build_data, 'inference_input, 'inference_output) 
  distro_family = {
  default_distro: unit -> 'distro;
  build: 'build_data -> 'distro;
  unpack: 'distro -> 'build_data;
  sample: 'distro -> 'thing;
  infer : 'inference_input  -> 'distro * 'inference_output;
  prob : 'distro -> 'thing -> float;
  neglog_prob : 'distro -> 'thing -> float;
  show : 'distro -> string -> string list;
}


let cross_entropy distro_family distro things =
  let n = Array.length things in
  let tot = ref 0. in
    Array.iter
      begin fun thing ->
	tot := !tot +. distro_family.neglog_prob distro thing;
      end
      things;
    !tot /. (float_of_int n)
