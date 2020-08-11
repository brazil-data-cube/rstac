# headers-----------------------------------------------------------------------
print_header <- function(x, ...) {
  UseMethod("print_header", x)
}

print_header.stac_item <- function(x, ...) {
  cat(crayon::bold(crayon::magenta("### STAC Item")), fill = TRUE)
  cat("- stac_version:", crayon::green(paste0('"', x$stac_version, '"')),
      fill = TRUE)
  cat("- id:", crayon::green(paste0('"', x$id, '"')), fill = TRUE)
  if (!is.null(x$bbox)) {
    cat("- bbox:",
        paste0("  - xmin:", crayon::green(sprintf('"%.4f"', x$bbox[1])), "\n",
               "  - ymin:", crayon::green(sprintf('"%.4f"', x$bbox[2])), "\n",
               "  - xmax:", crayon::green(sprintf('"%.4f"', x$bbox[3])), "\n",
               "  - ymax:", crayon::green(sprintf('"%.4f"', x$bbox[4]))),
      fill = TRUE)

  }
  if (!is.null(x$properties$datetime))
    cat("- datetime:", crayon::green(paste0('"', x$properties$datetime, '"')), fill = TRUE)
}

print_header.stac_items <- function(x, ...) {
  cat(crayon::bold(crayon::magenta("### STAC Items")), fill = TRUE)
}


bb_wrap = function(bb) {
  if (is.numeric(bb) && length(bb) == 4) {
    bb <-
      structure(as.double(bb), names = c("xmin", "ymin", "xmax", "ymax"))
    return(bb)
  }
  return(invisible(NULL))
}


print_header.stac_collection <- function(x, ...) {
  cat(crayon::bold(crayon::magenta("### STAC Collection")), fill = TRUE)
  cat("- stac_version:", crayon::green(paste0('"', x$stac_version, '"')), fill = TRUE)
  cat("- id:", crayon::green(paste0('"', x$id, '"')), fill = TRUE)
  if (!is.null(x$datetime))
    cat("- description:", crayon::green(paste0('"', x$description, '"')), fill = TRUE)
  if (!is.null(x$license))
    cat("- license:", crayon::green(paste0('"', x$license, '"')), fill = TRUE)
}

print_header.stac_catalog <- function(x, ...) {
  cat(crayon::bold("### STAC Catalog"), fill = TRUE)
  cat("- stac_version:", crayon::green(paste0('"', x$stac_version, '"')), fill = TRUE)
  cat("- id:", crayon::green(paste0('"', x$id, '"')), fill = TRUE)
  if (!is.null(x$description))
    cat("- description:", crayon::green(paste0('"', x$description, '"')), fill = TRUE)
}

# prints------------------------------------------------------------------------

#' @export
print.stac_catalog <- function(x, n = 10, ...) {

 print_header(x)
  # links
  if (!is.null(x$links)) {
    links <- Filter(function(e) e$rel == "child", x$links)
    if (length(links) > 0) {
      cat("- links:", fill = TRUE)
      for (i in seq_len(min(n, length(links)))) {
        e <- links[[i]]
        cat("-", crayon::red(crayon::bold(e$title)),
            paste0('(', crayon::underline(e$href), ')'), sep = " ", fill = TRUE)
      }
      if (n < length(links))
        cat(crayon::silver(crayon::bold(sprintf("> \U2026 with %s more links", length(links) - n))), fill = TRUE)
    }
  }
}

#' @export
print.stac_collection <- function(x, n = 10, ...) {

  # print headers
  print_header(x)

  # properties
  if (!is.null(x$properties) && length(x$properties) > 0) {
    cat("properties:", fill = TRUE)
    print_stac(x$properties, n = n)
  }

  # links
  if (!is.null(x$links)) {
    links <- Filter(function(e) e$rel == "child", x$links)
    if (length(links) > 0) {
      cat("- links:", fill = TRUE)
      for (i in seq_len(min(n, length(links)))) {
        e <- links[[i]]
        cat("-",
            paste0(crayon::red(crayon::bold(e$title)), ' ',
                   '(', crayon::underline(e$href), ')'), fill = TRUE)
      }
      if (n < length(links))
        cat(crayon::silver(sprintf("> \U2026 with %s more links", length(links) - n)), fill = TRUE)
    }
  }
}

print_stac <- function(x, n) {

  if (is.null(names(x)))
    print_unnamed(x, n = n, pad = 0, align_first = FALSE)
  else
    print_named(x, n = n, pad = 0, align_first = FALSE)
}

print_named <- function(x, n, pad = 0, align_first = FALSE) {

  for (k in names(x)) {
    if (align_first)
      cat("- ", k, ": ", sep = "")
    else
      cat(rep(" ", pad), "- ", k, ": ", sep = "")
    if (is.character(x[[k]]))
      cat(paste0(sapply(x[[k]], function(e) crayon::green(paste0('"', e, '"'))), collapse = ", "), sep = "", fill = TRUE)
    else if (is.numeric(x[[k]]))
      cat(paste0(sapply(x[[k]], function(e) crayon::red(e)), collapse = ", "), sep = "", fill = TRUE)
    else if (is.list(x[[k]])) {
      cat("", fill = TRUE)
      if (is.null(names(x[[k]])))
        print_unnamed(x[[k]], n = n, pad + 2, align_first = FALSE)
      else
        print_named(x[[k]], n = n, pad + 2, align_first = FALSE)
    } else
      cat("null", fill = TRUE)
    align_first = FALSE
  }
}

print_unnamed <- function(x, n, pad = 0, align_first = FALSE) {

  for (i in seq_along(x)) {
    if (align_first)
      cat("- ", sep = "")
    else
      cat(rep(" ", pad), "- ", sep = "")
    if (is.character(x[[i]]))
      cat(paste0(sapply(x[[i]], function(e) crayon::green(paste0('"', e, '"'))), collapse = ", "), sep = "", fill = TRUE)
    else if (is.numeric(x[[i]]))
      cat(paste0(sapply(x[[i]], function(e) crayon::red(e)), collapse = ", "), sep = "", fill = TRUE)
    else if (is.list(x[[i]])) {
      if (is.null(names(x[[i]])))
        print_unnamed(x[[i]], n = n, pad + 2, align_first = TRUE)
      else
        print_named(x[[i]], n = n, pad + 2, align_first = TRUE)
    } else
      cat("null", fill = TRUE)
    align_first = FALSE
  }
}

#' @export
print.stac_items <- function(x, n = 3, ...) {
  print_header(x)

  if (items_length(x) > 0) {
    for (i in seq_len(min(n, items_length(x)))) {
      feature <- x$features[[i]]

      if (!is.null(feature$collection))
        cat("- collection:", crayon::green(paste0(feature$collection)), fill = TRUE)
      if (!is.null(feature$properties$date))
        cat("- datetime:", crayon::green(paste0(feature$properties$date)), fill = TRUE)
      if (length(feature$assets) > 0) {
        cat("- assets:", fill = TRUE)
        for (j in seq_len(length(feature$assets))) {
          e <- feature$assets
          cat(" -",
              paste0(crayon::red(crayon::bold(names(e[j])))), fill = TRUE)
        }
      }
    }
  }
  if (n < items_length(x)) {
    cat(crayon::silver(sprintf("> \U2026 with %s more links", items_length(x) - n)), fill = TRUE)
  }
}

#' @export
print.stac_item <- function(x, ...){
  print_header(x)

  if (length(x$assets) > 0) {
    cat("- assets:", fill = TRUE)
    for (i in seq_len(length(x$assets))) {
      e <- x$assets
      cat(" -",
          paste0(crayon::red(crayon::bold(names(e[i]))), ' ',
                 '(', crayon::underline(e[[i]]$href), ')'), fill = TRUE)
    }
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

# TODO: stac_catalog print
# TODO: header: description, id e stac_version
#' @export
print.stac <- function(x, ...) {
  cat(crayon::bold(crayon::magenta("### STAC")), sep = "\n")

  named_vector("- url", x$url)
  named_vector("- params", x$params)
}
