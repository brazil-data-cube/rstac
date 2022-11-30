# scalar data types ----

is_str <- function(x) {
    is.character(x) && length(x) == 1
}

is_num <- function(x) {
    is.numeric(x) && length(x) == 1
}

is_bool <- function(x) {
    is.logical(x) && length(x) == 1
}

is_scalar <- function(x) {
    is_str(x) || is_num(x) || is_bool(x) ||
    inherits(x, c("cql2_math_op", "cql2_minus_op", "cql2_timestamp",
                  "cql2_date","cql2_interval", "cql2_casei_op",
                  "cql2_accenti_op", "cql2_prop_ref", "cql2_func"))
}

is_spatial <- function(x) {
    inherits(x, "sf")  && nrow(x) == 1 ||
    inherits(x, "sfc") && length(x) == 1 ||
    inherits(x, "sfg")
}

# input check ----

# check timestamp instant
is_time <- function(x) {
    is_str(x) && grep_iso_3339_date_time(x) ||
    inherits(x, "cql2_timestamp")
}

# check date instant
is_date <- function(x) {
    is_str(x) && grep_iso_3339_date(x) ||
    inherits(x, "cql2_date")
}

# check temporal instant
is_temporal <- function(x) {
    is_time(x) || is_date(x)
}

# check property name
identifier <- "^[a-zA-Z]+[0-9a-zA-Z:.$_]*$"

is_prop_name <- function(x) {
    is_str(x) && grepl(identifier, x)
}

is_func_name <- function(x) {
    is_str(x) && grepl(identifier, x)
}

# check list (array)
is_lst <- function(x) {
    is.list(x) && is.null(names(x))
}

# check object (named members)
is_obj <- function(x) {
    is.list(x) && !is.null(names(x)) && all(names(x) != "")
}

# check Boolean expression
is_bool_expr <- function(x) {
    inherits(x, c("cql2_logic_op", "cql2_not_op",
                  "cql2_in_op", "cql2_comp_op", "cql2_like_op",
                  "cql2_between_op", "cql2_inlist_op",
                  "cql2_isnull_op", "cql2_spatial_op",
                  "cql2_temporal_op", "cql2_array_pred", "logical"))
}

# check is_null operand
is_isnull_operand <- function(x) {
    is_str(x) || is_num(x) || is_bool(x) ||
    inherits(x, c("cql2_timestamp", "cql2_date", "cql2_interval",
                  "cql2_casei_op",  "cql2_accenti_op", "cql2_prop_ref",
                  "cql2_func", "cql2_geom"))
}

# check number expressions
is_num_expr <- function(x) {
    is_num(x) ||
    inherits(x, c("cql2_math_op", "cql2_minus_op",
                  "cql2_prop_ref", "cql2_func"))
}

# check character expression
is_str_expr <- function(x) {
    is_str(x) || inherits(x, c("cql2_casei_op", "cql2_accenti_op",
                               "cql2_prop_ref", "cql2_func"))
}

# check pattern expression
is_patt_expr <- function(x) {
    is_str(x) || inherits(x, c("cql2_casei_op", "cql2_accenti_op"))
}

is_casei_expr <- function(x) {
  is_str(x) || is_str_expr(x) || is_func_name(x) ||
    inherits(x, c("cql2_casei_op", "cql2_accenti_op"))
}

is_spatial_expr <- function(x) {
    is_spatial(x) || is_str(x) || is_obj(x) ||
    inherits(x, c("cql2_prop_ref", "cql2_func"))
}

is_temporal_expr <- function(x) {
    is_temporal(x) || inherits(x, c("cql2_interval", "cql2_prop_ref",
                                    "cql2_func"))
}

is_array_elem <- function(x) {
  is_str(x) || is_str_expr(x) || is_num(x) || is_temporal(x) ||
    is_spatial(x) || is_num_expr(x) || is_bool_expr(x) ||
    is_prop_name(x) || is_func_name(x) || is_casei_expr(x)
}

is_array_expr <- function(x) {
  is_scalar_lst(x) || is_array_elem(x)
}

# check list of scalars (at least one element)
is_scalar_lst <- function(x) {
    is_lst(x) && length(x) > 0 && all(vapply(x, is_scalar, TRUE))
}
