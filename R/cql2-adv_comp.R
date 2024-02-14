
# ---- advanced comparison operators ----

# - cql2_like_op
# - cql2_between_op
# - cql2_inlist_op

# ---- constructor functions ----

# like_op
like_op <- function(a, b) {
  a <- cql2_eval(a)
  b <- cql2_eval(b)
  check_is_str_expr(a)
  check_is_patt_expr(b)
  structure(list(op = "like", args = list(a, b)),
            class = c("cql2_like_op", "cql2_filter", "list"))
}

# between_op
between_op <- function(a, b, c) {
  a <- cql2_eval(a)
  b <- cql2_eval(b)
  c <- cql2_eval(c)
  check_is_num_expr(a)
  check_is_num_expr(b)
  check_is_num_expr(c)
  structure(list(op = "between", args = list(a, b, c)),
            class = c("cql2_between_op", "cql2_filter", "list"))
}

# in_op
in_op <- function(a, b) {
  a <- cql2_eval(a)
  b <- cql2_eval(b)
  check_is_scalar(a)
  check_is_scalar_lst(b)
  structure(list(op = "in", args = list(a, b)),
            class = c("cql2_in_op", "cql2_filter", "list"))
}

casei <- function(a) {
  a <- cql2_eval(a)
  check_is_casei_expr(a)
  structure(list(casei = a),
            class = c("cql2_casei_op", "cql2_filter", "list"))
}

accenti <- function(a) {
  a <- cql2_eval(a)
  check_is_casei_expr(a)
  structure(list(accenti = a),
            class = c("cql2_accenti_op", "cql2_filter", "list"))
}

# spatial_op
spatial_op <- function(op) {
  function(a, b) {
    a <- cql2_eval(a)
    b <- cql2_eval(b)
    check_is_spatial_expr(a)
    check_is_spatial_expr(b)
    if (is_spatial(a))
      a <- get_spatial(a)
    if (is_spatial(b))
      b <- get_spatial(b)
    structure(list(op = op, args = list(a, b)),
              class = c("cql2_spatial_op", "cql2_filter", "list"))
  }
}

get_spatial <- function(x) {
  UseMethod("get_spatial", x)
}

spatial_types <- c("Point", "MultiPoint", "LineString",
                   "MultiLineString", "Polygon", "MultiPolygon",
                   "GeometryCollection")

#' @export
get_spatial.character <- function(x) {
  x <- tryCatch({
    jsonlite::fromJSON(
      txt = x, simplifyVector = TRUE, simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    )
  }, error = function(e) {
    class(x) <- c("cql2_spatial", "character")
    return(x)
  })
  if (is.character(x)) return(x)
  get_spatial(x)
}

#' @export
get_spatial.list <- function(x) {
  if (!all(c("type", "coordinates") %in% names(x)))
    .error("Not a valid GeoJSON geometry.")
  if (!x$type %in% spatial_types)
    .error("GeoJSON type '%s' is not supported.", x$type)
  class(x) <- c("cql2_spatial", "list")
  x
}

#' @export
get_spatial.sf <- function(x) {
  get_spatial.sfc(sf::st_geometry(x))
}

#' @export
get_spatial.sfc <- function(x) {
  if (length(x) > 1) {
    x <- x[[1]]
    .warning("Informed geometry has multiple features. Using the first one.")
  }
  get_spatial.sfg(x[[1]])
}

#' @export
get_spatial.sfg <- function(x) {
  names(spatial_types) <- toupper(spatial_types)
  geom_type <- as.character(sf::st_geometry_type(x))
  if (!geom_type %in% names(spatial_types))
    .error("Geometry type '%s' is not supported.", geom_type)
  geom_type <- spatial_types[[geom_type]]
  structure(
    list(type = geom_type, coordinates = unclass(x)),
    class = c("cql2_spatial", "list")
  )
}

#' @export
get_spatial.GEOMETRYCOLLECTION <- function(x) {
  structure(
    list(type = "GeometryCollection", geometries = lapply(x, get_spatial)),
    class = c("cql2_spatial", "list")
  )
}

#' @export
as.character.cql2_spatial <- function(x, ...) {
  to_text(x)
}

# temporal_op
temporal_op <- function(op) {
  function(a, b) {
    a <- cql2_eval(a)
    b <- cql2_eval(b)
    check_is_temporal_expr(a)
    check_is_temporal_expr(b)
    structure(list(op = op, args = list(a, b)),
              class = c("cql2_temporal_op", "cql2_filter", "list"))
  }
}

# array_op
array_op <- function(op) {
  function(a, b) {
    a <- cql2_eval(a)
    b <- cql2_eval(b)
    check_is_array_expr(a)
    check_is_array_expr(b)
    structure(list(op = op, args = list(a, b)),
              class = c("cql2_array_op", "cql2_filter", "list"))
  }
}
