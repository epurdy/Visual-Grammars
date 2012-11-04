


type ('mod_sym, 'mod_comp, 'tgt_sym, 'tgt_comp, 'dom_thing) parse_tree = {
  mod_sym: 'mod_sym Abstract.symbol;
  mod_comp: 'mod_comp Abstract.composition option;

  tgt_sym: 'mod_sym Abstract.symbol;
  tgt_comp: 'mod_comp Abstract.composition option;

  yield: 'dom_thing;
  qual: float;

  left: ('mod_sym, 'mod_comp, 'tgt_sym, 'tgt_comp, 'dom_thing) parse_tree option;
  right: ('mod_sym, 'mod_comp, 'tgt_sym, 'tgt_comp, 'dom_thing) parse_tree option;
}
