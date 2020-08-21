
.OAFeat_collections_endpoint <- function(collection_id) {

  endpoint <- "/collections"

  if (!missing(collection_id))
    endpoint <- paste(endpoint, collection_id, sep = "/")

  return(endpoint)
}

.OAFeat_items_endpoint <- function(collection_id, feature_id) {

  endpoint <- paste("/collections", collection_id, "items", sep = "/")

  if (!missing(feature_id))
    endpoint <- paste(endpoint, feature_id, sep = "/")

  return(endpoint)
}

.stac_landpage_endpoint <- function(version) {

  if (version == "0.8.1")
    return("/stac")

  return("/") # version >= "0.9.0"
}

.stac_search_endpoint <- function(version) {

  if (version == "0.8.1")
    return("/stac/search")

  return("/search") # version >= "0.9.0"
}
