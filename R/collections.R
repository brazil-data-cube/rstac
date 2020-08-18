#' @title Endpoint functions
#'
#' @rdname collections
#'
#' @author Rolf Simoes
#'
#' @description
#' The \code{collections} function implements the WFS3 \code{/collections}
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
#'  \code{\link{items}}
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
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'   collections() %>%
#'   get_request()
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'   collections(collection_id = "MOD13Q1") %>%
#'   get_request()
#' }

#' @export
collections <- function(s, collection_id) {

  # check s parameter
  if (!"collections" %in% class(s))
    .check_obj(s, expected = "stac", exclusive = TRUE)

  params <- list()
  endpoint <- "/collections"
  if (!missing(collection_id)) {

    params[["collection_id"]] <- collection_id[[1]]
    endpoint <- paste("/collections", collection_id[[1]], sep = "/")
  }

  content <- build_stac(url = s$url,
                        endpoint = endpoint,
                        params = params,
                        mutator = "collections",
                        base_stac = s)
  return(content)
}

params_get_mutator.collections <- function(s) {

  # ignore 'collection_id' param
  s$params[["collection_id"]] <- NULL

  # process stac mutator
  params <- params_get_mutator.stac(s)

  return(params)
}

params_post_mutator.collections <- function(s, enctype) {

  # ignore 'collection_id' param
  s$params[["collection_id"]] <- NULL

  # process stac mutator
  params <- params_post_mutator.stac(s, enctype = enctype)

  return(params)
}

content_get_response.collections <- function(s, res) {

  # detect expected response object class
  content_class <- "stac_catalog"

  if (!is.null(s$params[["collection_id"]]))
    content_class <- "stac_collection"

  content <- structure(
    .check_response(res, "200", "application/json"),
    stac = s,
    request = list(method = "get"),
    class = content_class)

  return(content)
}

content_post_response.collections <- function(s, res, enctype) {

  # detect expected response object class
  content_class <- "stac_catalog"

  if (!is.null(s$params[["collection_id"]]))
    content_class <- "stac_collection"

  content <- structure(
    .check_response(res, "200", "application/json"),
    stac = s,
    request = list(method = "post", enctype = enctype),
    class = content_class)

  return(content)
}
