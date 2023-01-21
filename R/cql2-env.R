# ---- cql2 environments stack ----
# order of evaluation:
# cql2_global_env --> cql2_ident_env --> cql2_adv_comp_env --> cql2_core_env

# 1st environment in stack - cql2 core
cql2_core_env <- new_env(
  # cql2 basic expressions
  # Boolean expressions
  `&&` = new_logic_op("and"),
  `&` =  new_logic_op("and"),
  `||` = new_logic_op("or"),
  `|` =  new_logic_op("or"),
  `!` =  not_op,
  # comparison predicate
  # binary comparison operators
  `==` = new_comp_op("="),
  `!=` = new_comp_op("<>"),
  `<` =  new_comp_op("<"),
  `>` =  new_comp_op(">"),
  `<=` = new_comp_op("<="),
  `>=` = new_comp_op(">="),
  # is_null operator
  `is_null` = isnull_op,
  # basic math operators
  `-` = minus_op, # can be both binary and unary operator
  `+` = new_math_op("+"),
  `*` = new_math_op("*"),
  `/` = new_math_op("/"),
  # temporal literals
  timestamp =  timestamp_lit,
  date =       date_lit,
  interval =   interval_lit
)

# 2nd environment in stack - cql2 advanced comparison
cql2_adv_comp_env <- new_env(
  `%like%` =       like_op,
  `between` =      between_op,
  `%in%` =         in_op,
  casei =          casei,
  accenti =        accenti,
  s_intersects =   spatial_op("s_intersects"),
  s_contains =     spatial_op("s_contains"),
  s_crosses =      spatial_op("s_crosses"),
  s_disjoint =     spatial_op("s_disjoint"),
  s_equals =       spatial_op("s_equals"),
  s_overlaps =     spatial_op("s_overlaps"),
  s_touches =      spatial_op("s_touches"),
  s_within =       spatial_op("s_within"),
  anyinteracts =   temporal_op("anyinteracts"),
  t_after =        temporal_op("t_after"),
  t_before =       temporal_op("t_before"),
  t_contains =     temporal_op("t_contains"),
  t_disjoint =     temporal_op("t_disjoint"),
  t_during =       temporal_op("t_during"),
  t_equals =       temporal_op("t_equals"),
  t_finishedby =   temporal_op("t_finishedby"),
  t_finishes =     temporal_op("t_finishes"),
  t_intersects =   temporal_op("t_intersects"),
  t_meets =        temporal_op("t_meets"),
  t_metby =        temporal_op("t_metby"),
  t_overlappedby = temporal_op("t_overlappedby"),
  t_overlaps =     temporal_op("t_overlaps"),
  t_startedby =    temporal_op("t_startedby"),
  t_starts =       temporal_op("t_starts"),
  a_equals =       array_op("a_equals"),
  a_contains =     array_op("a_contains"),
  a_containedby =  array_op("a_containedby"),
  a_overlaps =     array_op("a_overlaps"),
  parent_env =     cql2_core_env
)

# 3rd environment in stack - cql2 properties and functions
cql2_ident_env <- new_env(parent_env = cql2_adv_comp_env)

# 4th environment in stack - R functions
cql2_global_env <- new_env(
  # basic R functions and constants
  `{` =     `{`,
  `(` =     `(`,
  `T` =     TRUE,
  `TRUE` =  TRUE,
  `F` =     FALSE,
  `FALSE` = FALSE,
  list =    list,
  c =       list,
  `:` =     function(from, to) {
    check_is_num(from)
    check_is_num(to)
    as.list(seq(from, to))
  },
  parent_env = cql2_ident_env
)


# ---- update cql2_ident_env ----
cql2_update_ident_env <- function(expr) {
  # update `cql2_ident_env` environment with all input properties
  rm(list = ls(cql2_ident_env, all.names = TRUE), envir = cql2_ident_env)
  list2env(get_all_props(expr), envir = cql2_ident_env)
  list2env(get_all_funcs(expr), envir = cql2_ident_env)
}
