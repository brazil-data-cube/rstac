# headers-----------------------------------------------------------------------
print_header <- function(x, ...) {
  UseMethod("print_header", x)
}

print_header.stac_item <- function(x, ...) {
  cat(crayon::bold(crayon::magenta("### STAC Item")), fill = TRUE)
  cat("- stac_version:", crayon::green(paste0('"', x$stac_version, '"')),
      fill = TRUE)
  cat("- id:", crayon::green(paste0('"', x$id, '"')), fill = TRUE)
  if (!is.null(x$bbox) & length(x$bbox) == 4) {
    cat("- bbox:",
        paste0("  - xmin: ", crayon::red(sprintf('%.5f', x$bbox[1])), "\n",
               "  - ymin: ", crayon::red(sprintf('%.5f', x$bbox[2])), "\n",
               "  - xmax: ", crayon::red(sprintf('%.5f', x$bbox[3])), "\n",
               "  - ymax: ", crayon::red(sprintf('%.5f', x$bbox[4]))),
        fill = TRUE)

  }
}

print_header.stac_items <- function(x, ...) {
  cat(crayon::bold(crayon::magenta("### STAC Items")), fill = TRUE)

  if (!is.null(x$type)) {
    cat("- type:", crayon::green(paste0('"', x$type, '"')), fill = TRUE)
  }

  if (!is.null(items_length(x))) {
    cat("- numberReturned:", crayon::red(items_length(x)),
        fill = TRUE)
  }
}

print_header.stac_collection <- function(x, ...) {
  cat(crayon::bold(crayon::magenta("### STAC Collection")), fill = TRUE)
  cat("- stac_version:", crayon::green(paste0('"', x$stac_version, '"')),
      fill = TRUE)
  cat("- id:", crayon::green(paste0('"', x$id, '"')), fill = TRUE)
  if (!is.null(x$datetime))
    cat("- description:", crayon::green(paste0('"', x$description, '"')),
        fill = TRUE)
  if (!is.null(x$license))
    cat("- license:", crayon::green(paste0('"', x$license, '"')), fill = TRUE)
}

print_header.stac_catalog <- function(x, ...) {
  cat(crayon::bold("### STAC Catalog"), fill = TRUE)
  cat("- stac_version:", crayon::green(paste0('"', x$stac_version, '"')),
      fill = TRUE)
  cat("- id:", crayon::green(paste0('"', x$id, '"')), fill = TRUE)
  if (!is.null(x$description))
    cat("- description:", crayon::green(paste0('"', x$description, '"')),
        fill = TRUE)
}

# prints------------------------------------------------------------------------
#' @export
print.stac <- function(x, ...) {
  cat(crayon::bold(crayon::magenta("### STAC")), sep = "\n")

  named_vector("- url", x$url)
  named_vector("- params", x$params)
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

      print_link_highlight(sapply(links_chunked, function(x){
        x$title}), sapply(links_chunked, function(x){x$href}), pad = 2)

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
print.stac_items <- function(x, n = 3, ...) {

  # print headers
  print_header(x)

  cat("- feature:", fill = TRUE)
  if (items_length(x) > 0) {
    feature <- x$features[seq_len(min(n, items_length(x)))]

    print_unnamed(lapply(feature, function(x){
      list(collection = x$collection,
           bbox = format_bbox(x$bbox),
           datetime = x$properties$datetime)}), pad = 2)
  }
  if (n < items_length(x)) {
    cat(crayon::silver(sprintf("> \U2026 with %s more links",
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
      print_unnamed(x$links, n = n, pad = 2)

      if (n < length(x$links))
        cat(crayon::silver(sprintf("> \U2026 with %s more links",
                                   length(x$links) - n)), fill = TRUE)
    }
  }

  if (length(x$assets) > 0) {
    cat("- assets:", fill = TRUE)

    print_link_highlight(names(x$assets),
                         unname(sapply(x$assets, function(x){x$href})), pad = 2)
  }

}

# helpers-----------------------------------------------------------------------
print_data <- function(x, n, pad = 0, is_nested = FALSE, ...) {
  if (is.character(x))
    cat(paste0(sapply(x, function(e) crayon::green(paste0('"', e, '"'))),
               collapse = ", "), sep = "", fill = TRUE)
  else if (is.numeric(x))
    cat(paste0(sapply(x, function(e) crayon::red(e)), collapse = ", "), sep = "",
        fill = TRUE)
  else if (is.list(x)) {
    #cat(fill = TRUE)
    if (is.null(names(x)))
      print_unnamed(x, n = n, pad + 2, align_first = FALSE)
    else
      print_named(x, n = n, pad + 2, align_first = TRUE, is_nested = is_nested)

  } else
    cat("null", fill = TRUE)
}

print_named <- function(x, n, pad = 0, align_first = FALSE, is_nested = FALSE) {

  for (k in names(x)) {
    if (k == names(x)[[1]]) {
      if (align_first & !is_nested) {
        cat("- ", k, ": ", sep = "")
      } else {
        cat(fill = TRUE)
        cat(rep(" ", pad), "- ", k, ": ", sep = "")
      }
    }
    else if (align_first) {
      cat("- ", k, ": ", sep = "")
    }
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

format_bbox <- function(bbox) {

  if (!is.null(bbox) & length(bbox) == 4) {
    paste0("xmin: ", crayon::red(sprintf('%.5f ', bbox[1])),
           "ymin: ", crayon::red(sprintf('%.5f ', bbox[2])),
           "xmax: ", crayon::red(sprintf('%.5f ', bbox[3])),
           "ymax: ", crayon::red(sprintf('%.5f',  bbox[4])))
  }
}

print_link_highlight <- function(titles, hrefs, pad = 2) {

  for (i in seq_len(length(titles))) {
    cat(rep(" ", pad), "- ", sep = "")
    cat(crayon::yellow(crayon::bold(titles[[i]])),
        paste0('(', crayon::underline(hrefs[[i]]), ')'), sep = " ",
        fill = TRUE)
  }
}

#' @description function from httr package
#'
#' @references `httr`package (https://CRAN.R-project.org/package=httr)
#'
#' @noRd
named_vector <- function(title, x) {
  if (length(x) == 0) return()

  cat(title, "\n")
  cat(" -", paste0(names(x), crayon::green(as.character(x))))
}
