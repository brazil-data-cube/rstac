#' rstac print functions
#'
#' @author Felipe Carvalho and Rolf Simoes
#'
#' @name print
NULL

#### headers  ####

print_header <- function(x, ...) {
  UseMethod("print_header", x)
}

print_header.stac <- function(x, ...) {

  cat(crayon::bold("### STAC"), fill = TRUE)
}

print_header.stac_item <- function(x, ...) {

  cat(crayon::bold("### STAC Item"), fill = TRUE)
  cat("- stac_version:", crayon::bold(paste0('"', x$stac_version, '"')),
      fill = TRUE)
  cat("- id:", crayon::bold(paste0('"', x$id, '"')), fill = TRUE)

  if (!is.null(x$bbox)) {
    cat("- bbox: ")
    print_data(format_bbox(x$bbox))
  }
}

print_header.stac_items <- function(x, ...) {

  cat(crayon::bold("### STAC Items"), fill = TRUE)

  if (!is.null(x$type))
    cat("- type:", crayon::bold(paste0('"', x$type, '"')), fill = TRUE)

  if (!is.null(items_length(x)))
    cat("- numberMatched:", crayon::bold(items_matched(x)),
        fill = TRUE)
}

print_header.stac_collection <- function(x, ...) {

  cat(crayon::bold("### STAC Collection"), fill = TRUE)
  cat("- stac_version:", crayon::bold(paste0('"', x$stac_version, '"')),
      fill = TRUE)
  cat("- id:", crayon::bold(paste0('"', x$id, '"')), fill = TRUE)

  if (!is.null(x$datetime))
    cat("- description:", crayon::bold(paste0('"', x$description, '"')),
        fill = TRUE)
  if (!is.null(x$license))
    cat("- license:", crayon::bold(paste0('"', x$license, '"')), fill = TRUE)
  if (!is.null(x$extent)) {
    cat("- extent: ", fill = TRUE)

    print_extent(x$extent, pad = 2)
  }
}

print_header.stac_catalog <- function(x, ...) {

  cat(crayon::bold("### STAC Catalog"), fill = TRUE)
  cat("- stac_version:", crayon::bold(paste0('"', x$stac_version, '"')),
      fill = TRUE)
  cat("- id:", crayon::bold(paste0('"', x$id, '"')), fill = TRUE)

  if (!is.null(x$description))
    cat("- description:", crayon::bold(paste0('"', x$description, '"')),
        fill = TRUE)
}

#### prints ####

#' @export
print.stac <- function(x, ...) {

  # print headers
  print_header(x)

  # print body
  print_named(x, n = Inf, align_first = TRUE)
}

#' @export
print.stac_catalog <- function(x, n = 10, ...) {

  # print headers
  print_header(x)

  # links
  if (!is.null(x$links)) {
    links <- Filter(function(e) e$rel == "child", x$links)

    if (length(links) > 0) {
      links_chunked <- links[1:min(n, length(links))]
      cat("- links:", fill = TRUE)

      print_link_highlight(sapply(links_chunked, function(x){x$title}),
                           sapply(links_chunked, function(x){x$href}), pad = 2)

      if (n < length(links))
        cat(crayon::silver(crayon::bold(sprintf("> \U2026 with %s more links",
                                        length(links) - n))), fill = TRUE)
    }
  }
}

#' @export
print.stac_collection <- function(x, n = 10, ...) {

  # print headers
  print_header(x)

  # properties
  if (!is.null(x$properties) && length(x$properties) > 0) {
    cat("- properties:")
    print_named(x$properties, n = Inf, pad = 2)
  }

  # links
  if (!is.null(x$links)) {
    if (length(x$links) > 0) {
      cat("- links:", fill = TRUE)
      print_unnamed(x$links, n = n, pad = 2)

      if (n < length(x$links))
        cat(crayon::silver(sprintf("> \U2026 with %s more links",
                                   length(x$links) - n)), fill = TRUE)
    }
  }
}

#' @export
print.stac_items <- function(x, n = 5, ...) {

  # print headers
  print_header(x)

  cat("- features:", fill = TRUE)

  if (items_length(x) > 0) {
    feature <- x$features[seq_len(min(n, items_length(x)))]

    print_unnamed(lapply(feature, function(x){
      list(collection = x$collection,
           bbox = format_bbox(x$bbox),
           datetime = x$properties$datetime)}), pad = 2)
  }
  if (n < items_length(x)) {
    cat(crayon::silver(sprintf("> \U2026 with %s more feature(s)",
                               items_length(x) - n)), fill = TRUE)
  }
}

#' @export
print.stac_item <- function(x, n = 10, ...){

  # print headers
  print_header(x)

  # properties
  if (!is.null(x$properties) && length(x$properties) > 0) {
    cat("- properties:")
    print_named(x$properties, n = Inf, pad = 2)
  }

  if (!is.null(x$links)) {
    if (length(x$links) > 0) {
      cat("- links:", fill = TRUE)
      print_unnamed(x$links, n = Inf, pad = 2)
    }
  }

  if (length(x$assets) > 0) {
    cat("- assets:", fill = TRUE)

    print_link_highlight(names(x$assets),
                         unname(sapply(x$assets, function(x){x$href})), pad = 2)
  }

}

#### helpers ####

format_bbox <- function(bbox) {

  if (!is.null(bbox) & length(bbox) == 4)
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  else if (!is.null(bbox) & length(bbox) == 6)
    names(bbox) <- c("xmin", "ymin", "zmin", "xmax", "ymax", "zmax")

  return(bbox)
}

format_interval <- function(interval) {

  interval <- lapply(interval, function(x){
    if (is.null(x[[1]]))
      x[[1]] <- ".."
    if (is.null(x[[2]]))
      x[[2]] <- ".."

    x <- paste(x, collapse = "/")
  })

  return(interval)
}

print_data <- function(x, n, pad = 0, is_nested = FALSE, ...) {

  if (is.character(x)) {
    if (is.null(names(x)))
      cat(paste0(sapply(x, function(e) crayon::bold(paste0('"', e, '"'))),
                 collapse = ", "), sep = "", fill = TRUE)
    else
      cat(paste(names(x), sapply(x, function(e) crayon::bold(paste0('"', e, '"'))),
                collapse = ", ", sep = ": "), sep = "", fill = TRUE)
  } else if (is.numeric(x)) {
    if (is.null(names(x)))
      cat(paste0(sapply(x, function(e) crayon::bold(sprintf("%.5f", e))),
                 collapse = ", "), sep = "", fill = TRUE)
    else
      cat(paste(names(x), sapply(x, function(e) crayon::bold(sprintf("%.5f", e))),
                collapse = ", ", sep = ": "), sep = "", fill = TRUE)
  } else if (is.integer(x)) {
    if (is.null(names(x)))
      cat(paste0(sapply(x, function(e) crayon::bold(e)),
                 collapse = ", "), sep = "", fill = TRUE)
    else
      cat(paste(names(x), sapply(x, function(e) crayon::bold(e)),
                collapse = ", ", sep = ": "), sep = "", fill = TRUE)
  } else if (is.list(x)) {
    if (length(x) == 0)
      cat(fill = TRUE)
    else if (is.null(names(x)))
      print_unnamed(x, n = n, pad + 2, align_first = FALSE)
    else
      print_named(x, n = n, pad + 2, align_first = TRUE, is_nested = is_nested)
  } else
    cat("null", fill = TRUE)
}

print_named <- function(x, n, pad = 0, align_first = FALSE, is_nested = FALSE) {

  if (is.atomic(x)) {
    print_data(x)
    return()
  }

  for (k in names(x)) {
    if (k == names(x)[[1]]) {
      if (align_first & !is_nested) {
        cat("- ", k, ": ", sep = "")
      } else {
        cat(fill = TRUE)
        cat(rep(" ", pad), "- ", k, ": ", sep = "")
      }
    }
    else if (align_first)
      cat("- ", k, ": ", sep = "")
    else
      cat(rep(" ", pad), "- ", k, ": ", sep = "")

    print_data(x[[k]], n = n, pad = pad, align_first = FALSE, is_nested = TRUE)
    align_first = FALSE
  }
}

print_unnamed <- function(x, n = 10, pad = 0, align_first = FALSE) {

  for (i in seq_len(min(n, length(x)))) {
    if (align_first)
      cat("- ", sep = "")
    else
      cat(rep(" ", pad), "- ", sep = "")

    print_data(x[[i]], n = n, pad = pad, align_first = TRUE)
    align_first = FALSE
  }
}

print_extent <- function(x, pad) {

  if (!is.null(x$spatial)) {
    cat(rep(" ", pad), "- spatial: ", sep = "", fill = TRUE)
    cat(rep(" ", pad + 2), "- bbox: ", sep = "", fill = TRUE)

    print_data(lapply(x$spatial$bbox, format_bbox), n = Inf, pad = pad + 2)
  }

  if (!is.null(x$temporal)) {
    cat(rep(" ", pad), "- temporal: ", sep = "", fill = TRUE)
    cat(rep(" ", pad + 2), "- interval: ", sep = "", fill = TRUE)

    print_data(format_interval(x$temporal$interval), n = Inf, pad = pad + 2)
  }
}

print_link_highlight <- function(titles, hrefs, pad = 2) {

  for (i in seq_len(length(titles))) {
    cat(rep(" ", pad), "- ", sep = "")
    cat(crayon::bold(titles[[i]]),
        paste0('(', crayon::underline(hrefs[[i]]), ')'), sep = " ",
        fill = TRUE)
  }
}
