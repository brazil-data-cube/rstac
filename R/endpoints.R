#' @title endpoints functions
#'
#' @param collection_id a \code{character} collection id to be retrieved.
#'
#' @return a \code{character} with the STAC endpoint of the required version.
#'
#' @noRd
.OAFeat_collections_endpoint <- function(collection_id) {

  endpoint <- "/collections"

  if (!missing(collection_id))
    endpoint <- paste(endpoint, collection_id, sep = "/")

  return(endpoint)
}

#' @title endpoints functions
#'
#' @param collection_id a \code{character} collection id to be retrieved.
#'
#' @param feature_id  a \code{character} with item id to be fetched.
#' Only works if the \code{collection_id} is informed. This is equivalent to
#' the endpoint \code{/collections/\{collectionId\}/items/\{itemId\}}.
#'
#' @return a \code{character} with the STAC endpoint of the required version.
#'
#' @noRd
.OAFeat_items_endpoint <- function(collection_id, feature_id) {

  endpoint <- paste("/collections", collection_id, "items", sep = "/")

  if (!missing(feature_id))
    endpoint <- paste(endpoint, feature_id, sep = "/")

  return(endpoint)
}

#' @title endpoints functions
#'
#' @param version    a \code{character} with the STAC version.
#'
#' @return a \code{character} with the STAC endpoint of the required version.
#'
#' @noRd
.stac_landpage_endpoint <- function(version) {

  if (version < "0.9.0")
    return("/stac")

  return("/") # version >= "0.9.0"
}

#' @title endpoints functions
#'
#' @param version    a \code{character} with the STAC version.
#'
#' @return a \code{character} with the STAC endpoint of the required version.
#'
#' @noRd
.stac_search_endpoint <- function(version) {

  if (version < "0.9.0")
    return("/stac/search")

  return("/search") # version >= "0.9.0"
}
