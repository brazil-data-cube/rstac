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
#' @param q             a \code{RSTACQuery} object expressing a STAC query
#' criteria.
#'
#' @param collection_id a \code{character} collection id to be retrieved.
#'
#' @seealso
#' \code{\link{get_request}}, \code{\link{post_request}},
#'  \code{\link{items}}
#'
#' @return
#' A \code{RSTACQuery} object with the subclass \code{collections} for
#'  \code{/collections/} endpoint, or a \code{collection_id} subclass for
#'  \code{/collections/{collection_id}} endpoint, containing all search field
#'  parameters to be provided to STAC API web service.
#'
#' @examples
#' \donttest{
#'
#' stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
#'   collections() %>%
#'   get_request()
#'
#' stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
#'   collections(collection_id = "CB4_64_16D_STK-1") %>%
#'   get_request()
#' }

#' @export
collections <- function(q, collection_id) {

  # check q parameter
  check_subclass(q, "stac")

  params <- list()

  subclass <- "collections"
  if (!is.null(collection_id)) {

    if (length(collection_id) != 1)
      .error("Parameter `collection_id` must be a single value.")

    params[["collection_id"]] <- collection_id

    subclass <- "collection_id"
  }

  RSTACQuery(version = q$version,
             base_url = q$base_url,
             params = utils::modifyList(q$params, params),
             subclass = subclass)
}

#' @export
endpoint.collections <- function(q) {

  return("/collections")
}

#' @export
before_request.collections <- function(q) {

  check_query_verb(q, verbs = c("GET", "POST"))

  return(q)
}

#' @export
after_response.collections <- function(q, res) {

  content <- content_response(res, "200", "application/json")

  RSTACDocument(content = content, q = q, subclass = "STACCollectionList")
}

#' @export
endpoint.collection_id <- function(q) {

  return(paste("/collections", q$params[["collection_id"]], sep = "/"))
}

#' @export
before_request.collection_id <- function(q) {

  check_query_verb(q, verbs = c("GET", "POST"))

  # don't send 'collection_id' in url's query string or content body
  q <- omit_query_params(q, names = "collection_id")

  return(q)
}

#' @export
after_response.collection_id <- function(q, res) {

  content <- content_response(res, "200", "application/json")

  RSTACDocument(content = content, q = q,
                subclass = c("STACCollection", "STACCatalog"))
}
