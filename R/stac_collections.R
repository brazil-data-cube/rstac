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
#' @param url       A \code{character} informing the base url of a
#' STAC web service.
#'
#' @param collection_id A \code{character} collection id to be retrieved.
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
#'     stac_request()
#'
#' stac_collections("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'                  collection_id = "CB_64_16D_STK") %>%
#'     stac_request()
#' }
#'
#' @export
stac_collections <- function(url, collection_id) {

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
