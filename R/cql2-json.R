# ---- json auxiliary functions ----

json_quote <- function(x) {
  paste0('"', x, '"')
}

json_lst <- function(x) {
  paste0("[", paste0(lapply(x, to_json), collapse = ","), "]")
}

json_obj <- function(x) {
  paste0("{", paste0(json_quote(names(x)), ":",
                     unname(lapply(x, to_json)), collapse = ","), "}")
}

# ---- convert to json ----

to_json <- function(x) UseMethod("to_json", x)

#' @export
to_json.NULL <- function(x) {
  "null"
}

#' @export
to_json.character <- function(x) {
  if (length(x) == 1) {
    json_quote(x)
  } else {
    to_json(as.list(x))
  }
}

#' @export
to_json.numeric <- function(x) {
  if (length(x) == 1) {
    num_format(x)
  } else {
    to_json(as.list(x))
  }
}

#' @export
to_json.integer <- function(x) {
  to_json.numeric(x)
}

#' @export
to_json.logical <- function(x)  {
  if (length(x) == 1) {
    if (x) "true" else "false"
  } else {
    to_json(as.list(x))
  }
}

#' @export
to_json.matrix <- function(x) {
  to_json(apply(x, 1, c, simplify = FALSE))
}

#' @export
to_json.list <- function(x) {
  if (is_lst(x))
    json_lst(x)
  else if (is_obj(x))
    json_obj(x)
  else
    stop("cannot convert list value to a valid cql2 json", call. = FALSE)
}

#' @export
to_json.cql2_spatial <- function(x) {
  if (is_str(x)) {
    return(x) # input WKT string
  }
  return(json_obj(x)) # input list representing a GeoJSON
}

#' @export
to_json.cql2_logic_op <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_not_op <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_comp_op <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_like_op <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_between_op <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_in_op <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_isnull_op <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_math_op <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_minus_op <- function(x) {
  if (length(x$args) == 1 && is_num(x$args[[1]]))
    paste0(-x$args[[1]])
  else
    json_obj(x)
}

#' @export
to_json.cql2_spatial_op <- function(x) {
  json_obj(x)
}

#' @export
to_json.sf <- function(x) {
  json_obj(get_spatial(x))
}

#' @export
to_json.sfc <- function(x) {
  json_obj(get_spatial(x))
}

#' @export
to_json.sfg <- function(x) {
  json_obj(get_spatial(x))
}

#' @export
to_json.cql2_temporal_op <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_func <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_prop_ref <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_timestamp <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_date <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2_interval <- function(x) {
  json_obj(x)
}

#' @export
to_json.cql2 <- function(x) {
  to_json(cql2_filter(x))
}

#' @export
to_json.default <- function(x) {
  stop(paste("cannot handle value of class ", class(x)), call. = FALSE)
}
