type watson_distro

type watson_inference_input = {
  samples: (float * Shape.shape) array;
  gamma_shape_param: float;
  gamma_mean_param: float;
}

type watson_build_data = {
  mean_shape: Shape.shape;
  concentration: float;
}

type watson_inference_output = {
  ssap: Geometry.cpt * Geometry.cpt * Geometry.cpt * Geometry.cpt;
  totwt: float;
  eval: float; 
  evec: Geometry.cpt * Geometry.cpt;
  mode: Geometry.cpt * Geometry.cpt * Geometry.cpt;
  conc: float;
  cost: float;
}

val watson_distro_family : 
  (Shape.shape, watson_distro, watson_build_data, 
   watson_inference_input, watson_inference_output) Distro.distro_family
