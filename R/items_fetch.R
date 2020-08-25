#' @title Items function
#'
#' @description The \code{items_fetch} function returns the pagination of all
#'  items of the stac object
#'
#' @param items      a \code{stac_item_collection} object representing the
#'  request results of \code{/stac/search},
#'  \code{/collections/{collectionId}/items}, or
#'  \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @param ...        other params to be passed to \link[httr]{GET} or
#' \link[httr]{POST} methods
#'
#' @param headers    a \code{character} of named arguments to be passed as
#' HTTP request headers.
#'
#' @param progress   a \code{logical} indicating if a progress bar must be
#' shown or not. Defaults to \code{TRUE}.
#'
#' @seealso
#' \code{\link{stac}} \code{\link{stac_search}} \code{\link{collections}}
#' \code{\link{items}}
#'
#' @return a \code{stac_item_collection} object.
#'
#' @examples
#' \dontrun{
#'
#'  stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
#'       force_version = "0.8.1") %>%
#'   stac_search(collections = "MOD13Q1",
#'          bbox = c(-55.16335, -4.26325, -49.31739, -1.18355),
#'          limit = 500) %>%
#'    get_request() %>%
#'    items_fetch()
#' }
#'
#' @export
items_fetch <- function(items, ..., headers = character(), progress = TRUE) {

  # Check object class
  .check_obj(items, "stac_item_collection")

  matched <- items_matched(items)

  # verify if progress bar can be shown
  progress <- progress & (!is.null(matched) && (items_length(items) < matched))
  if (progress)
    pb <- utils::txtProgressBar(min = items_length(items), max = matched,
                                style = 3)

  while (TRUE) {


    # protect against infinite loop
    if (!is.null(matched) && (items_length(items) > matched))
      .error(paste("Length of returned items (%s) is different",
                   "from matched items (%s)."), items_length(items), matched)

    s <- attr(items, "stac")
    if (is.null(s)) break

    # get url of the next page
    next_url <- Filter(function(x) x$rel == "next", items$links)
    if (length(next_url) == 0) break

    # create a new stac object with params from the next url
    base_url <- gsub("^([^?]+)(\\?.*)?$", "\\1", next_url[[1]]$href)
    query <- substring(gsub("^([^?]+)(\\?.*)?$", "\\2", next_url[[1]]$href), 2)
    next_stac <- structure(list(url = base_url,
                                params = .query_decode(query)),
                           class = class(s))

    # get request method
    request <- attr(items, "request")
    if (is.null(request))
      request <- list(method = "get")

    # call request
    if (request$method == "get") {

      content <- get_request(next_stac, ..., headers = headers)
    } else if (request$method == "post") {

      content <- post_request(next_stac, ...,
                              enctype = request$enctype,
                              headers = headers)
    } else {

      .error("Invalid HTTP method.")
    }

    # check content response
    .check_obj(content, "stac_item_collection")

    # merge features result into resulting content
    content$features <- c(items$features, content$features)

    # update progress bar
    if (progress)
      utils::setTxtProgressBar(pb, items_length(content))

    # prepares next iteration
    items <- content
  }

  # close progress bar
  if (progress) {
    utils::setTxtProgressBar(pb, matched)
    close(pb)
  }

  # TODO: report numbers of items fetched

  return(items)
}
