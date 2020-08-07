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

#' @export
print.stac_collection <- function(x, n = 3, ...) {

  if (length(x$links) > 1) {
    links_print <- lapply(x$links, function(y){
      as.data.frame(y, stringsAsFactors = FALSE)
    })

    # TODO: print header
    # TODO: tibble of tibble's
    links_print <- do.call(rbind, links_print)
    links_print <- tibble::as_tibble(links_print)

    print(links_print)
  } else {
    print.default(x)
  }
}

#' @export
format.stac_collection <- function(x, links_print, n, ...) {

  print_size <- n

  if (print_size >= length(x$links)) {
    print(links_print)
  } else if (print_size >= 1) {
    print(links_print[1:print_size])

    if ((length(x$links) - print_size) == 0)
      return(invisible(x))

    limit_print <-
      sprintf("# ... with more %s links to show.",
              (length(x$links) - print_size))

    cat(crayon::bold(limit_print))
  } else {
    warning("Please set a value greater than 0.
    Use <print(obj, n = ...)> ", call. = FALSE)
  }
}


# TODO: stac_item print
