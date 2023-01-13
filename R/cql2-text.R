
# ---- text auxiliary functions ----

text_quote <- function(x) {
  paste0("'", x, "'")
}

text_lst <- function(v) {
  paste0("(", paste0(v, collapse = ","), ")")
}

escape <- function(x) {
  gsub("'", "''", x)
}

# ---- convert to text ----

to_text <- function(x) {
  UseMethod("to_text", x)
}

#' @export
to_text.character <- function(x) {
  if (length(x) == 1)
    text_quote(escape(x))
  else
    to_text(as.list(x))
}

#' @export
to_text.numeric <- function(x) {
  if (length(x) == 1) {
    num_format(x)
  } else {
    to_text(as.list(x))
  }
}

#' @export
to_text.integer <- function(x) {
  to_text.numeric(x)
}

#' @export
to_text.logical <- function(x) {
  if (length(x) == 1)
    if (x) "true" else "false"
  else
    to_text(as.list(x))
}

#' @export
to_text.cql2_spatial <- function(x) {
  if (is_str(x)) {
    return(x) # input WKT string
  }
  to_wkt(x) # input list representing a GeoJSON
}

#' @export
to_text.sf <- function(x) {
  to_wkt(get_spatial(x))
}

#' @export
to_text.sfc <- function(x) {
  to_wkt(get_spatial(x))
}

#' @export
to_text.sfg <- function(x) {
  to_wkt(get_spatial(x))
}

#' @export
to_text.list <- function(x) {
  if (is_lst(x))
    text_lst(lapply(x, to_text))
  else if (is_spatial(x))
    to_wkt(get_spatial(x))
  else
    stop("cannot convert list object to cql2 text", call. = FALSE)
}

#' @export
to_text.cql2_logic_op <- function(x) {
  paste(to_text(x$args[[1]]), toupper(x$op), to_text(x$args[[2]]))
}

#' @export
to_text.cql2_not_op <- function(x) {
  text_not_op(x)
}

#' @export
to_text.cql2_spatial_op <- function(x) {
  paste0(
    toupper(x$op), "(",
    to_text(x$args[[1]]), ",",
    to_text(x$args[[2]]),
    ")"
  )
}

#' @export
to_text.cql2_temporal_op <- function(x) {
  paste0(
    toupper(x$op), "(",
    to_text(x$args[[1]]), ",",
    to_text(x$args[[2]]),
    ")"
  )
}

#' @export
to_text.cql2_array_op <- function(x) {
  paste0(
    toupper(x$op), "(",
    to_text(x$args[[1]]), ",",
    to_text(x$args[[2]]),
    ")"
  )
}

#' @export
to_text.cql2_func <- function(x) {
  args <- paste0(lapply(x$`function`$args, to_text), collapse = ",")
  paste0(x$`function`$name, "(", args, ")")
}

#' @export
to_text.cql2_comp_op <- function(x) {
  paste(to_text(x$args[[1]]), x$op, to_text(x$args[[2]]))
}

#' @export
to_text.cql2_like_op <- function(x) {
  paste(to_text(x$args[[1]]), toupper(x$op), to_text(x$args[[2]]))
}

#' @export
to_text.cql2_between_op <- function(x) {
  paste(to_text(x$args[[1]]), "BETWEEN",
        to_text(x$args[[2]]), "AND", to_text(x$args[[3]]))
}

#' @export
to_text.cql2_in_op <- function(x) {
  paste(to_text(x$args[[1]]), "IN", to_text(x$args[[2]]))
}

#' @export
to_text.cql2_isnull_op <- function(x) {
  paste(to_text(x$args[[1]]), "IS NULL")
}

#' @export
to_text.cql2_math_op <- function(x) {
  paste(to_text(x$args[[1]]), x$op, to_text(x$args[[2]]))
}

#' @export
to_text.cql2_minus_op <- function(x) {
  if (length(x$args) == 1)
    paste0(x$op, to_text(x$args[[1]]))
  else
    paste(to_text(x$args[[1]]), x$op, to_text(x$args[[2]]))
}

#' @export
to_text.cql2_prop_ref <- function(x) {
  x$property[[1]]
}

#' @export
to_text.cql2_timestamp <- function(x) {
  paste0("TIMESTAMP(", to_text(x$timestamp), ")")
}

#' @export
to_text.cql2_date <- function(x) {
  paste0("DATE(", to_text(x$date), ")")
}

#' @export
to_text.cql2_casei_op <- function(x) {
  paste0("CASEI(", to_text(x$casei), ")")
}

#' @export
to_text.cql2_accenti_op <- function(x) {
  paste0("ACCENTI(", to_text(x$accenti), ")")
}

#' @export
to_text.cql2_interval <- function(x) {
  paste0("INTERVAL(", to_text(x$interval[[1]]), ",",
         to_text(x$interval[[2]]), ")")
}

#' @export
to_text.cql2 <- function(x) {
  to_text(cql2_filter(x))
}

#' @export
to_text.default <- function(x) {
  stop(paste("cannot handle value of class", class(x)), call. = FALSE)
}

# ---- not_op + some operator ----

text_not_op <- function(x) {
  UseMethod("text_not_op", x$args[[1]])
}

#' @export
text_not_op.cql2_isnull_op <- function(x) {
  paste(to_text(x$args[[1]]$args[[1]]), "IS NOT NULL")
}

# is there infix NOT operator?
# like_op
#' @export
text_not_op.cql2_like_op <- function(x) {
  paste(to_text(x$args[[1]]$args[[1]]), "NOT LIKE",
        to_text(x$args[[1]]$args[[2]]))
}

# between_op
#' @export
text_not_op.cql2_between_op <- function(x) {
  paste(to_text(x$args[[1]]$args[[1]]), "NOT BETWEEN",
        to_text(x$args[[1]]$args[[2]]), "AND",
        to_text(x$args[[1]]$args[[3]]))
}

# in_op
#' @export
text_not_op.cql2_in_op <- function(x) {
  paste(to_text(x$args[[1]]$args[[1]]), "NOT IN",
        to_text(x$args[[1]]$args[[2]]))
}

#' @export
text_not_op.default <- function(x) {
  paste("NOT", to_text(x$args[[1]]))
}

# ---- convert to wkt ----

wkt_spatial_type <- function(x) {
  if (!all(c("type", "coordinates") %in% names(x)))
    .error("Not a valid GeoJSON geometry.")
  x[["type"]]
}

wkt_spatial_switch <- function(x, ...) {
  switch(wkt_spatial_type(x), ...,
         .error("WKT type '%s' is not supported.", wkt_spatial_type(x)))
}

wkt_collection <- function(x) {
  paste0(map_chr(x$geometries, to_wkt), collapse = ",")
}

wkt_coords_chr <- function(x) {
  if (is.list(x)) {
    x <- unlist(x, recursive = FALSE)
    if (!is.numeric(x))
      .error("Invalid coordinates values")
    x <- matrix(x, ncol = 2, byrow = TRUE)
  }
  paste(apply(x, 1, wkt_coord0, simplify = TRUE), collapse = ",")
}

wkt_coord0 <- function(x) {
  paste(num_format(x), collapse = " ")
}

wkt_coord1 <- function(x) {
  wkt_coords_chr(x)
}

wkt_coord2 <- function(x) {
  paste0("(", map_chr(x, wkt_coords_chr), ")", collapse = ",")
}

wkt_coord3 <- function(x) {
  paste0("(", map_chr(x, wkt_coord2), ")", collapse = ",")
}

to_wkt <- function(x) {
  wkt_spatial_switch(
    x,
    "Point" = paste0(
      "POINT(", wkt_coord0(x$coordinates), ")"
    ),
    "MultiPoint" = paste0(
      "MULTIPOINT(", wkt_coord1(x$coordinates) , ")"
    ),
    "LineString" = paste0(
      "LINESTRING(", wkt_coord1(x$coordinates) , ")"
    ),
    "MultiLineString" = paste0(
      "MULTILINESTRING(", wkt_coord2(x$coordinates) , ")"
    ),
    "Polygon" = paste0(
      "POLYGON(", wkt_coord2(x$coordinates) , ")"
    ),
    "MultiPolygon" = paste0(
      "MULTIPOLYGON(", wkt_coord3(x$coordinates) , ")"
    ),
    "GeometryCollection" = paste0(
      "GEOMETRYCOLLECTION(", wkt_collection(x), ")"
    )
  )
}
