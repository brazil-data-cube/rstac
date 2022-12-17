#' @title Assets filter
#'
#' @description `r lifecycle::badge('deprecated')`
#'
#' @param items                a `STACItemCollection` object representing
#'  the result of `/stac/search`, \code{/collections/{collectionId}/items}.
#'
#' @param ...          additional arguments. See details.
#'
#' @param filter_fn           a `function` that will be used to filter the
#'  attributes listed in the properties.
#'
#' @return a `list` with the attributes of date, bands and paths.
#'
#' @name assets_filter
#'
#' @export
assets_filter <- function(items, ..., filter_fn = NULL) {
  UseMethod("assets_filter", items)
}

#' @rdname assets_filter
#'
#' @export
assets_filter.STACItemCollection <- function(items, ..., filter_fn = NULL) {
  # signal the deprecation to the user
  lifecycle::deprecate_soft(
    when = "0.9.2",
    what = "rstac::assets_filter()",
    with = "rstac::assets_select()"
  )
  dots <- substitute(list(...), env = environment())[-1]

  if (length(dots) > 0) {
    if (!is.null(names(dots))) .error("Invalid filter arguments.")

    for (i in seq_along(dots)) {

      items$features <- lapply(items$features, function(item) {

        sel <- vapply(item$assets, function(asset) {

          tryCatch({
            eval(dots[[i]], envir = asset, enclos = baseenv())
          }, error = function(e) { NA })
        }, logical(1))

        if (all(is.na(sel)))
          .error("Invalid condition arguments.")

        sel[is.na(sel)] <- FALSE

        item$assets <- item$assets[sel]

        item
      })
    }
  }

  if (!is.null(filter_fn)) {
    items$features <- lapply(items$features, function(item) {

      sel <- vapply(item$assets, filter_fn, logical(1))

      item$assets <- item$assets[sel]
      item
    })
  }

  items
}

#' @rdname assets_filter
#'
#' @export
assets_filter.STACItem <- function(items, ..., filter_fn = NULL) {
  # signal the deprecation to the user
  lifecycle::deprecate_soft(
    when = "0.9.2",
    what = "rstac::assets_filter()",
    with = "rstac::assets_select()"
  )
  dots <- substitute(list(...), env = environment())[-1]

  if (length(dots) > 0) {
    if (!is.null(names(dots))) .error("Invalid filter arguments.")

    for (i in seq_along(dots)) {
      sel <- vapply(items$assets, function(asset) {
        tryCatch({
          eval(dots[[i]], envir = asset, enclos = baseenv())
        }, error = function(e) { NA })
      }, logical(1))

      if (all(is.na(sel))) .error("Invalid condition arguments.")

      sel[is.na(sel)] <- FALSE
      items$assets <- items$assets[sel]
    }
  }

  if (!is.null(filter_fn)) {
    sel <- vapply(items$assets, filter_fn, logical(1))
    items$assets <- items$assets[sel]
  }

  items
}

#' @rdname items_functions
#'
#' @export
items_group <- function(items, ..., field = NULL, index = NULL) {
  # signal the deprecation to the user
  lifecycle::deprecate_soft(
    when = "0.9.2",
    what = "rstac::items_group()"
  )

  # checks if the object is STACItemCollections
  if (items_length(items) == 0) return(list(items))

  dots <- substitute(list(...), env = environment())[-1]
  if (!is.character(dots)) dots <- as.character(dots)

  if (length(index) == 0 && length(field) == 0 &&  length(dots) == 0)
    .error(paste("Either parameters 'index', 'field' or '...' parameters must",
                 "be supplied."))

  if (length(index) > 0 && (length(field) > 0 || length(dots) > 0))
    .error(paste("Only one of the parameters '...','index' or 'field' should",
                 "be supplied."))

  if (is.null(index)) {
    index <- items_reap(items, ..., field = field)

    if (!is.atomic(index))
      .error("The field must be atomic vector.")
  } else {

    if (items_matched(items) > items_length(items))
      .warning(paste("The number of matched items is greater than the number",
                     "of items length on your object. Considere to use",
                     "the 'items_fetch()' function before this operation."))
  }

  if (items_length(items) != length(index))
    .error(paste("The length of the field provided for grouping must contain",
                 "the same size as the length of the items."))

  features <- unname(tapply(X = items$features,
                            INDEX = index,
                            FUN = c, simplify = FALSE))

  lapply(features, function(x){
    items$features <- x

    items
  })
}
