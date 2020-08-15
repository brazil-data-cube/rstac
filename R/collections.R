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
#' @param s             a \code{stac} object expressing a STAC search criteria
#' provided by \code{stac}, \code{stac_search}, \code{stac_collections},
#' or \code{stac_items} functions.
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
#'  stac_collections("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     get_request()
#'
#'  stac_collections("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'                  collection_id = "MOD13Q1") %>%
#'     get_request()
#' }
#'
#' @export
collections <- function(s, collection_id) {

  # check s parameter
  .check_obj(s, "stac")

  # check mutator
  .check_mutator(s, c("stac", "collections"))

  params <- list()

  if (missing(collection_id)) {

    endpoint <- "/collections"

    # TODO: add these code excerpts bellow in different file
    expected <- list("get" =
                       list(responses =
                              list("200" =
                                     list("application/json" =
                                            "stac_catalog"))),
                     "post" =
                       list(enctypes = c("application/x-www-form-urlencoded",
                                         "multipart/form-data"),
                            responses =
                              list("200" =
                                     list("application/json" =
                                            "stac_catalog"))))

  } else {
    params[["collection_id"]] <- collection_id[[1]]
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

  content <- structure(list(url = s$url,
                            endpoint = endpoint,
                            params = params,
                            expected_responses = expected,
                            mutator = "collections"),
                       class = "stac")

  content <- build_stac(content, s)

  return(content)
}
