#' @export
print.stac_catalog <- function(x, n = 10, ...) {

  cat("stac_version:", crayon::green(paste0('"', x$stac_version, '"')), fill = TRUE)
  cat("id:", crayon::green(paste0('"', x$id, '"')), fill = TRUE)
  if (!is.null(x$description))
    cat("description:", crayon::green(paste0('"', x$description, '"')), fill = TRUE)

  # links
  if (!is.null(x$links)) {
    links <- Filter(function(e) e$rel == "child", x$links)
    if (length(links) > 0) {
      cat("links:", fill = TRUE)
      for (i in seq_len(min(n, length(links)))) {
        e <- links[[i]]
        cat("-", crayon::red(crayon::bold(e$title)),
            paste0('(', crayon::underline(e$href), ')'), fill = TRUE)
      }
      if (n < length(links))
        cat(crayon::silver(sprintf("> \U2026 with %s more links", length(links) - n)), fill = TRUE)
    }
  }
}

print_header <- function(x, ...) {
  UseMethod("print_header", x)
}

print_header.stac_collection <- function(x, ...) {
  cat(crayon::bold(crayon::yellow("### STAC Collection")), fill = TRUE)
  cat("- stac_version:", crayon::green(paste0('"', x$stac_version, '"')), fill = TRUE)
  cat("- id:", crayon::green(paste0('"', x$id, '"')), fill = TRUE)
  if (!is.null(x$description))
    cat("- description:", crayon::green(paste0('"', x$description, '"')), fill = TRUE)
  if (!is.null(x$license))
    cat("- license:", crayon::green(paste0('"', x$license, '"')), fill = TRUE)
}

print_header.stac_catalog <- function(x, ...) {
  cat(crayon::bold(crayon::yellow("### STAC Catalog")), fill = TRUE)
  cat("- stac_version:", crayon::green(paste0('"', x$stac_version, '"')), fill = TRUE)
  cat("- id:", crayon::green(paste0('"', x$id, '"')), fill = TRUE)
  if (!is.null(x$description))
    cat("- description:", crayon::green(paste0('"', x$description, '"')), fill = TRUE)
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
        cat(crayon::silver(sprintf("# \U2026 with %s more links", length(links) - n)), fill = TRUE)
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
  if (items_length(x) >= 1) {
    items_print <- lapply(x$features, function(y) {
      # header <- sprintf(
      #   "collection: %s,
      #    bbox: %.5f, %.5f, %.5f, %.5f,
      #    datetime: %s", y$collection, y$bbox[[1]],
      #                                 y$bbox[[2]],
      #                                 y$bbox[[3]],
      #                                 y$bbox[[4]],
      #   y$properties$datetime)

      header <- sprintf( "datetime: %s", y$properties$datetime)
      item_tbl <- tibble::tibble(assets_names = c(names(y$assets)))

      items_list <- list(header, item_tbl)
    })
    names(items_print) <- rep(c("feature"), items_length(x))
    format(x, items_print, n)
  } else {
    print.default(x)
  }
}

#' @export
format.stac_items <- function(x, items_print, n, ...) {

  print_size <- n

  if (print_size >= items_length(x)) {
    print(items_print)
  } else if (print_size >= 1) {
    print(items_print[1:print_size])

    if ((items_length(x) - print_size) == 0)
      return(invisible(x))

    limit_print <-
      sprintf("# ... with more %s items to show.
      To change use <print(obj, n = ...)>",
              (items_length(x) - print_size))

    cat(crayon::bold(limit_print))
  } else {
    warning("Please set a value greater than 0.
    Use <print(obj, n = ...)> ", call. = FALSE)
  }
}


# TODO: stac_catalog print
# TODO: header: description, id e stac_version
#' @export
print.stac <- function(x, ...) {
  cat("<stac>\n")

  named_vector("$url", x$url)
  named_vector("$params", x$params)
}

#' #' @export
#' print.stac_collection <- function(x, n = 3, ...) {
#'
#'   if (length(x$links) > 1) {
#'     links_print <- lapply(x$links, function(y){
#'       as.data.frame(y, stringsAsFactors = FALSE)
#'     })
#'
#'     # TODO: print header
#'     # TODO: tibble of tibble's
#'     links_print <- do.call(rbind, links_print)
#'     links_print <- tibble::as_tibble(links_print)
#'
#'     print(links_print)
#'   } else {
#'     print.default(x)
#'   }
#' }
#'
#' #' @export
#' format.stac_collection <- function(x, links_print, n, ...) {
#'
#'   print_size <- n
#'
#'   if (print_size >= length(x$links)) {
#'     print(links_print)
#'   } else if (print_size >= 1) {
#'     print(links_print[1:print_size])
#'
#'     if ((length(x$links) - print_size) == 0)
#'       return(invisible(x))
#'
#'     limit_print <-
#'       sprintf("# ... with more %s links to show.",
#'               (length(x$links) - print_size))
#'
#'     cat(crayon::bold(limit_print))
#'   } else {
#'     warning("Please set a value greater than 0.
#'     Use <print(obj, n = ...)> ", call. = FALSE)
#'   }
#' }


# TODO: stac_item print
