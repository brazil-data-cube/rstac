
# ---- advanced comparison operators ----

# - cql2_like_op
# - cql2_between_op
# - cql2_inlist_op

# ---- constructor functions ----

# like_op
like_op <- function(a, b) {
    a <- cql2_eval(a)
    b <- cql2_eval(b)
    stopifnot(is_str_expr(a))
    stopifnot(is_patt_expr(b))
    structure(list(op = "like", args = list(a, b)),
              class = c("cql2_like_op", "list"))
}

# between_op
between_op <- function(a, b, c) {
    a <- cql2_eval(a)
    b <- cql2_eval(b)
    c <- cql2_eval(c)
    stopifnot(is_num_expr(a))
    stopifnot(is_num_expr(b))
    stopifnot(is_num_expr(c))
    structure(list(op = "between", args = list(a, b, c)),
              class = c("cql2_between_op", "list"))
}

# in_op
in_op <- function(a, b) {
    a <- cql2_eval(a)
    b <- cql2_eval(b)
    stopifnot(is_scalar(a))
    stopifnot(is_scalar_lst(b))
    structure(list(op = "in", args = list(a, b)),
              class = c("cql2_in_op", "list"))
}

casei <- function(a) {
  a <- cql2_eval(a)
  stopifnot(is_casei_expr(a))
  structure(list(casei = a),
            class = c("cql2_casei_op", "list"))
}

accenti <- function(a, b) {
  a <- cql2_eval(a)
  stopifnot(is_casei_expr(a))
  structure(list(accenti = a),
            class = c("cql2_accenti_op", "list"))
}

# spatial_op
spatial_op <- function(op) {
    function(a, b) {
        a <- cql2_eval(a)
        b <- cql2_eval(b)
        stopifnot(is_spatial_expr(a))
        stopifnot(is_spatial_expr(b))
        if (is_spatial(a))
            a <- get_spatial(a)
        if (is_spatial(b))
            b <- get_spatial(b)
        structure(list(op = op, args = list(a, b)),
                  class = c("cql2_spatial_op", "list"))
    }
}

get_spatial <- function(x) {
    UseMethod("get_spatial", x)
}

spatial_types <- c("Point", "MultiPoint", "LineString",
                   "MultiLineString", "Polygon", "MultiPolygon",
                   "GeometryCollection")

#' @export
get_spatial.list <- function(x) {
    stopifnot(c("type", "coordinates") %in% names(x))
    stopifnot(x[["type"]] %in% spatial_types)
    stopifnot(is.list(x[["coordinates"]]))
    class(x) <- c("cql2_spatial", "list")
    x
}

#' @export
get_spatial.sf <- function(x) get_spatial.sfg(sf::st_geometry(x)[[1]])

#' @export
get_spatial.sfc <- function(x) get_spatial.sfg(x[[1]])

#' @export
get_spatial.sfg <- function(x) {
    names(spatial_types) <- toupper(spatial_types)

    geom_type <- spatial_types[[as.character(sf::st_geometry_type(x))]]
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

# temporal_op
temporal_op <- function(op) {
    function(a, b) {
        a <- cql2_eval(a)
        b <- cql2_eval(b)
        stopifnot(is_temporal_expr(a))
        stopifnot(is_temporal_expr(b))

        structure(list(op = op, args = list(a, b)),
                  class = c("cql2_temporal_op", "list"))
    }
}

# array_op
array_op <- function(op) {
  function(a, b) {
    a <- cql2_eval(a)
    b <- cql2_eval(b)
    stopifnot(is_array_expr(a))
    stopifnot(is_array_expr(b))
    structure(list(op = op, args = list(a, b)),
              class = c("cql2_array_op", "list"))
  }
}

# ---- convert to json ----

#' @exportS3Method
to_json.cql2_like_op <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_between_op <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_in_op <- function(x) json_obj(x)

# ---- convert to text ----

# is there infix NOT operator?
# like_op
#' @exportS3Method
text_not_op.cql2_like_op <- function(x)
    paste(to_text(x$args[[1]]$args[[1]]), "NOT LIKE",
          to_text(x$args[[1]]$args[[2]]))

#' @exportS3Method
to_text.cql2_like_op <- function(x)
    paste(to_text(x$args[[1]]), toupper(x$op), to_text(x$args[[2]]))

# between_op
#' @exportS3Method
text_not_op.cql2_between_op <- function(x)
    paste(to_text(x$args[[1]]$args[[1]]), "NOT BETWEEN",
          to_text(x$args[[1]]$args[[2]]), "AND",
          to_text(x$args[[1]]$args[[3]]))

#' @exportS3Method
to_text.cql2_between_op <- function(x)
    paste(to_text(x$args[[1]]), "BETWEEN",
          to_text(x$args[[2]]), "AND", to_text(x$args[[3]]))

# in_op
#' @exportS3Method
text_not_op.cql2_in_op <- function(x)
    paste(to_text(x$args[[1]]$args[[1]]), "NOT IN",
          to_text(x$args[[1]]$args[[2]]))

#' @exportS3Method
to_text.cql2_in_op <- function(x)
    paste(to_text(x$args[[1]]), "IN", to_text(x$args[[2]]))
