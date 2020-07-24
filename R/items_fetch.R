#' @title Items function
#'
#' @author Rolf Simoes
#'
#' @description The \code{items_fetch} function returns the pagination of all
#'  items of the stac object
#'
#' @param items      A \code{stac_items} object representing the result of
#'  \code{/stac/search}, \code{/collections/{collectionId}/items}, or
#'  \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @param progress   A \code{logical} indicating if a progress bar must be
#' shown or not. Defaults to \code{TRUE}.
#'
#' @param headers    A \code{list} of named arguments to be passed as
#'  http request headers. This is used in \emph{addition} to eventual headers
#'  defined in \code{stac} object parameter.
#'
#' @seealso
#' \code{\link{stac}} \code{\link{stac_search}} \code{\link{stac_collections}}
#' \code{\link{stac_items}}
#'
#' @return
#' A \code{stac_items} object.
#'
#' @examples
#' \dontrun{
#'
#' stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'             collections = "MOD13Q1",
#'             bbox = c(-55.16335, -4.26325, -49.31739, -1.18355),
#'             limit = 500) %>%
#'     get_request() %>%
#'     items_fetch()
#' }
#'
#' @export
items_fetch <- function(items, progress = TRUE, headers = c()) {

  # Check object class
  .check_obj(items, expected = c("stac_items", "stac_item"))

  matched <- items_matched(items)

  # verify if progress bar can be shown
  progress <- progress & (!is.null(matched) && (items_length(items) < matched))
  if (progress)
    pb <- utils::txtProgressBar(min = items_length(items), max = matched,
                                style = 3)

  while (TRUE) {

    # protect against infinite loop
    if (!is.null(matched) && (items_length(items) > matched))
      stop(sprintf(paste("Length of returned items (%s) is different",
                         "from matched items (%s)."),
                   items_length(items), matched), call. = FALSE)

    s <- attr(items, "stac")
    if (is.null(s))
      return(items)

    # get url of the next page
    next_url <- Filter(function(x) x$rel == "next", items$links)
    if (length(next_url) == 0)
      return(items)

    # update stac object with params of the next url
    next_stac <- .url_to_stac(next_url[[1]]$href)
    next_stac$expected_responses <- s$expected_responses

    # get request method
    request <- attr(items, "request")
    if (is.null(request))
      request <- list(method = "get")

    # call request
    if (request$method == "get") {
      content <- get_request(next_stac, headers = headers)
    } else if (request$method == "post") {
      content <- post_request(next_stac,
                              encode = request$enctype,
                              headers = headers)
    } else {
      stop(sprintf("Invalid HTTP method."), call. = FALSE)
    }

    if (!inherits(content, "stac_items"))
      stop(sprintf("Invalid content response."), call. = FALSE)

    # merge features result into resulting content
    content$features <- c(items$features, content$features)

    # update progress bar
    if (progress)
      utils::setTxtProgressBar(pb, items_length(content))

    # prepares next iteration
    items <- content
  }

  # close progress bar
  if (progress)
    close(pb)

  return(items)
}
