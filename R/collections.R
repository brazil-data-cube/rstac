#' @title Endpoint functions
#'
#' @rdname collections
#'
#' @description
#' The \code{collections} function implements the WFS3 \code{/collections}
#'  and \code{/collections/\{collectionId\}} endpoints
#'  (v0.8.0, v0.8.1 and v0.9.0).
#'
#' Each endpoint retrieves specific STAC objects:
#' \itemize{
#'   \item \code{/collections}: Returns a list of STAC Collection published in
#'     the STAC service
#'   \item \code{/collections/\{collectionId\}}: Returns a single STAC
#'     Collection object
#' }
#'
#' @param s             a \code{RSTACQuery} object expressing a STAC query
#' criteria.
#'
#' @param collection_id a \code{character} collection id to be retrieved.
#'
#' @seealso
#' \code{\link{get_request}}, \code{\link{post_request}},
#'  \code{\link{items}}
#'
#' @return
#' A \code{stac} object containing all search field parameters to be provided
#' to STAC API web service.
#'
#' @examples
#' \donttest{
#'
#' stac("http://brazildatacube.dpi.inpe.br/stac") %>%
#'   collections() %>%
#'   get_request()
#'
#' stac("http://brazildatacube.dpi.inpe.br/stac") %>%
#'   collections(collection_id = "MOD13Q1") %>%
#'   get_request()
#' }

#' @export
collections <- function(s, collection_id) {

  # check s parameter
  check_query_subclass(s, "stac")

  params <- list()

  subclass <- "collections"
  if (!missing(collection_id)) {

    if (length(collection_id) != 1)
      .error("Parameter `collection_id` must be a single value.")

    params[["collection_id"]] <- collection_id

    subclass <- "collection_id"
  }

  RSTACQuery(version = s$version,
             url = s$url,
             params = utils::modifyList(s$params, params),
             subclass = subclass)
}

get_endpoint.collections <- function(s) {

  return("/collections")
}

before_request.collections <- function(s) {

  check_query_verb(s, verbs = c("GET", "POST"))

  return(s)
}

after_response.collections <- function(s, res) {

  content <- content_response(res, "200", "application/json")

  RSTACDocument(content = content, s = s,
                subclass = "STACCollectionList")
}

get_endpoint.collection_id <- function(s) {

  return(paste("/collections", s$params[["collection_id"]], sep = "/"))
}

before_request.collection_id <- function(s) {

  check_query_verb(s, verbs = c("GET", "POST"))

  # ignore 'collection_id'
  s$params[["collection_id"]] <- NULL

  return(s)
}

after_response.collection_id <- function(s, res) {

  content <- content_response(res, "200", "application/json")

  RSTACDocument(content = content, s = s,
                subclass = c("STACCollection", "STACCatalog"))
}
