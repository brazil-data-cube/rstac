#' @title Endpoint functions
#'
#' @author Rolf Simoes
#'
#' @description
#' The \code{stac_items} function implements WFS3
#' \code{/collections/\{collectionId\}/items}, and
#' \code{/collections/\{collectionId\}/items/\{itemId\}}
#' endpoints (v0.8.0).
#'
#' Each endpoint retrieves specific STAC objects:
#' \itemize{
#'   \item \code{/collections/\{collectionId\}/items}: Returns a STAC Items
#'     collection (GeoJSON)
#'   \item \code{/collections/\{collectionId\}/items/\{itemId\}}: Returns a
#'     STAC Item (GeoJSON Feature)
#' }
#'
#' The endpoint \code{/collections/\{collectionId\}/items} accepts the same
#' filters parameters of \code{\link{stac_search}} function.
#'
#'
#' @param url       A \code{character} informing the base url of a
#' STAC web service.
#'
#' @param collection_id A \code{character} with a collection id to retrieve
#' collection details.
#'
#' @param item_id   A \code{character} with item id to be fetched.
#' Only works if the \code{collection_id} is informed. This is equivalent to
#' the endpoint \code{/collections/\{collectionId\}/items/\{itemId\}}.
#'
#' @param ...       Filter parameters. Accept the same filter parameters
#' of \code{\link{stac_search}} function.
#'
#' @param .limit    An \code{integer} defining the maximum number of results
#' to return. Defaults to 10.
#'
#' @param .next     An \code{integer} informing which set of results
#' to return. Values less than 1 means all pages will be retrieved.
#'
#' @seealso
#' \code{\link{stac_request}}, \code{\link{stac_collections}}
#'
#' @return
#' A \code{stac} object containing all request parameters to be
#' provided to \code{stac_request}.
#'
#' @examples
#' \dontrun{
#'
#' stac_items("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'            collection_id = "CB_64_16D_STK",
#'            bbox = c(-55.16335, -4.26325, -49.31739, -1.18355)) %>%
#'     stac_request()
#' stac_items("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'            collection_id = "MOD13Q1",
#'            item_id = "MOD13Q1.A2019241.h13v09.006.2019262164754") %>%
#'     stac_request()
#' }
#'
#' @export
stac_items <- function(url, collection_id, item_id, ..., .limit, .next) {

  params <- list()

  if (!missing(...))
    params <- c(params, list(...))

  params["limit"] <- .limit

  params["next"] <- .next

  endpoint <- paste("/collections", collection_id, "items", sep = "/")
  if (!missing(item_id)) {
    endpoint <- paste(endpoint, item_id, sep = "/")
    params <- list()
  }

  content <- structure(list(url = url,
                            endpoint = endpoint,
                            params = params),
                       class = c("stac"))
  return(content)
}
