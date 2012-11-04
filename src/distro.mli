(** A ('thing,'distro,'build_data, 'inference_input,
    'inference_output) distro_family is a family of
    distributions. Here 'thing is the type the distribution is over,
    'distro is the type of the individual distribution, 'build_data is
    the input required to create a distribution, 'inference_input is
    the input to whatever inference algorithm, and 'inference_output
    is whatever useful data is created during inference *)

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

val cross_entropy : 
  ('thing, 'distro, 'build_data, 'inference_input, 'inference_output) distro_family
  -> 'distro -> 'thing array -> float
