#' @title Collections functions
#'
#' @description
#' These functions provide support to work with
#' `doc_collections`objects.
#'
#' \itemize{
#' \item `collections_length()`: `r lifecycle::badge('experimental')`
#' shows how many items there are in the `doc_items` object.
#'
#' \item `collections_matched()`: `r lifecycle::badge('experimental')`
#' shows how many items matched the search criteria.
#'
#' \item `collections_fetch()`: `r lifecycle::badge('experimental')`
#' request all STAC Items through pagination.
#'
#' \item `collections_next()`: `r lifecycle::badge('experimental')`
#' fetches a new page from STAC service.
#'
#' }
#'
#' @param collections     a `doc_collections` object.
#'
#' @param matched_field   a `character` vector with the path
#' where is the number of collections returned.
#'
#' @param progress        a `logical` indicating if a progress bar must be
#' shown or not. Defaults to `TRUE`.
#'
#' @param ...             additional arguments. See details.
#'
#' @details
#' Ellipsis argument (`...`) appears in different items functions and
#' has distinct purposes:
#'
#' \itemize{
#' \item `collections_fetch()` and `collections_next()`: ellipsis is used to
#' pass additional `httr` options to [GET][httr::GET] method, such as
#' [add_headers][httr::add_headers] or [set_cookies][httr::set_cookies].
#'
#' }
#'
#' @return
#'
#' \itemize{
#' \item `collections_length()`: an `integer` value.
#'
#' \item `collections_matched()`: returns an `integer` value if the STAC web
#' server does support this extension. Otherwise returns `NULL`.
#'
#' \item `collections_fetch()`: a `doc_items` with all matched items.
#'
#' \item `collections_next()`: fetches a new page from STAC service.
#'
#' }
#'
#' @examples
#' \dontrun{
#' # doc_items object
#' stac("https://cmr.earthdata.nasa.gov/stac/LPCLOUD") |>
#'   collections() |>
#'   get_request() |>
#'   collections_fetch()
#' }
#'
#' @name collections_functions
NULL



#' @rdname collections_functions
#'
#' @export
collections_next <- function(collections, ...) {
  check_collections(collections)
  # get url of the next page
  rel <- NULL
  next_link <- links(collections, rel == "next")
  if (length(next_link) == 0) {
    .error("Cannot get next link URL.", class = "next_error")
  }
  next_link <- next_link[[1]]
  res <- make_get_request(
    url = next_link$href,
    headers = next_link$headers,
    ...,
    error_msg = "Error while requesting next page"
  )
  content <- content_response_json(res)
  # return items
  doc_collections(content)
}

#' @rdname collections_functions
#'
#' @export
collections_matched <- function(collections, matched_field) {
  check_collections(collections)
  matched <- NULL
  if (is.character(matched_field) && matched_field %in% names(collections)) {
    matched <- as.numeric(collections[[matched_field]])
  }
  matched
}

#' @rdname collections_functions
#'
#' @export
collections_length <- function(collections) {
  check_collections(collections)
  return(length(collections$collections))
}

#' @rdname collections_functions
#'
#' @export
collections_fetch <- function(collections, ...,
                              progress = TRUE,
                              matched_field = NULL) {
  check_collections(collections)
  matched <- collections_matched(collections, matched_field)
  # verify if progress bar can be shown
  progress <- progress &
    (!is.null(matched) && (collections_fetch(collections) < matched))
  if (progress) {
    pb <- utils::txtProgressBar(
      min = collections_length(collections),
      max = matched,
      style = 3
    )
    # close progress bar when exit
    on.exit({
      if (progress) {
        utils::setTxtProgressBar(pb, matched)
        close(pb)
      }
    })
  }
  # Initialize the items
  next_collections <- collections
  while (TRUE) {
    # check if features is complete
    if (!is.null(matched) && (collections_length(collections) == matched)) {
      break
    }
    # protect against infinite loop
    if (!is.null(matched) && (collections_length(collections) > matched)) {
      .error(
        paste(
          "Length of returned collections (%s) is different",
          "from matched collections (%s)."
        ),
        collections_length(collections), matched
      )
    }
    next_collections <- tryCatch(
      {
        collections_next(next_collections, ...)
      },
      next_error = function(e) NULL
    )
    if (is.null(next_collections)) {
      break
    }
    collections$collections <- c(
      collections$collections,
      next_collections$collections
    )
    # update progress bar
    if (progress) {
      utils::setTxtProgressBar(pb, length(next_collections))
    }
  }
  collections
}
