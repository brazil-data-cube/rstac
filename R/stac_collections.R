#' @title Endpoint functions
#'
#' @author Rolf Simoes
#'
#' @description
#' The \code{stac_collections} function implements the WFS3 \code{/collections}
#' and \code{/collections/\{collectionId\}} endpoints (v0.8.0).
#'
#' Each endpoint retrieves specific STAC objects:
#' \itemize{
#'   \item \code{/collections}: Returns a list of STAC Collection published in
#'     the STAC service
#'   \item \code{/collections/\{collectionId\}}: Returns a single STAC
#'     Collection object
#' }
#'
#' @param url       a \code{character} informing the base url of a
#' STAC web service.
#'
#' @param collection_id a \code{character} collection id to be retrieved.
#'
#' @seealso
#' \code{\link{get_request}}, \code{\link{post_request}},
#'  \code{\link{stac_items}}
#'
#' @return
#'
#' If no \code{collection_id} is informed, \code{stac_collections} returns a
#' list of STAC Collections. Otherwise, it will return a \code{stac_collection} object
#' representing a specific STAC Collection.
#'
#' @examples
#' \dontrun{
#'
#' stac_collections("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     get_request()
#'
#' stac_collections("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'                  collection_id = "CB_64_16D_STK") %>%
#'     get_request()
#' }
#'
#' @export
stac_collections <- function(url, collection_id) {

  # check url parameter
  .check_obj(url, "character")

  if (missing(collection_id)) {

    endpoint <- "/collections"

    # TODO: add these code excerpts bellow in different file
    expected <- list("get" =
                       list(responses =
                              list("200" =
                                     list("application/json" = ""))),
                     "post" =
                       list(enctypes = c("application/x-www-form-urlencoded",
                                         "multipart/form-data"),
                            responses =
                              list("200" =
                                     list("application/json" = ""))))

  } else {
    endpoint <- paste("/collections", collection_id[[1]], sep = "/")

    # TODO: add these code excerpts bellow in different file
    expected <- list("get" =
                       list(responses =
                              list("200" =
                                     list("application/json" =
                                            "stac_collection"))),
                     "post" =
                       list(enctypes = c("application/x-www-form-urlencoded",
                                         "multipart/form-data"),
                            responses =
                              list("200" =
                                     list("application/json" =
                                            "stac_collection"))))
  }

  content <- structure(list(url = .make_url(url, endpoint = endpoint),
                            params = list(),
                            expected_responses = expected),
                       class = "stac")
  return(content)
}

#' @export
print.stac_collection <- function(x, ...) {

  if (length(x$links) > 1) {
    links_print <- lapply(x$links, function(y){

    as.matrix(
      data.frame(
        title = ifelse(is.null(y$title), "NULL", y$title),
        rel = y$rel,
        href = y$href
      )
    )
    })

    if (!is.null(getOption("max.links"))) {
      format(x, links_print)
    } else {
      options(max.links = length(links_print))
      format(x, links_print)
    }
  } else {
    print.default(x)
  }
}

#' @export
format.stac_collection <- function(x, links_print, ...) {

  print_size <- getOption("max.links")

  if (print_size >= length(x$links)) {
    print(links_print)
  } else if (print_size >= 1) {
    print(links_print[1:print_size])

    if ((length(x$links) - print_size) == 0)
      return(invisible(x))

    limit_print <-
      sprintf("# ... with more %s links to show.
      To change use <options(max.links = ...)>",
              (length(x$links) - print_size))

    cat(limit_print)
  } else {
    warning("Please set a value greater than 0.
    Use <options(max.links = ...)> ", call. = FALSE)
  }
}
