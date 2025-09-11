rstac_doc <- function(x, subclass) {
  structure(x, class = c(subclass, "list"), query = NULL)
}

#' @export
stac_version.rstac_doc <- function(x, ...) {
  default_version <- "0.8.0"
  if (!is.null(x$stac_version))
    return(x$stac_version)
  default_version
}

#' @export
stac_version.doc_collections <- function(x, ...) {
  if (length(x$collections) > 0)
    stac_version(x$collections[[1]])
}

#' @export
stac_version.doc_items <- function(x, ...) {
  if (!is.null(x$stac_version))
    return(x$stac_version)
  if ("features" %in% names(x) && length(x$features) > 0)
    stac_version(x$features[[1]])
}

#' @export
subclass.rstac_doc <- function(x) {
  class(x)[[1]]
}

#' @export
stac_type.rstac_doc <- function(x) {
  subclass <- subclass(x)
  switch(
    subclass,
    doc_conformance = "Conformance",
    doc_catalog = "Catalog",
    doc_collection = "Collection",
    doc_collections = "Collections",
    doc_item = "Item",
    doc_items = "Items"
  )
}

stac_subclass <- function(obj) {
  if (!is.list(obj) || is.null(names(obj)))
    .error("Invalid STAC document.")
  if ("type" %in% names(obj)) {
    if (obj$type == "Feature")
      return("doc_item")
    if (obj$type == "FeatureCollection")
      return("doc_items")
    if (obj$type == "Collection")
      return("doc_collection")
    if (obj$type == "Catalog")
      return("doc_catalog")
    .error("Invalid STAC document. Key value 'type': '", obj$type,
           "' is not a supported STAC document.")
  } else {
    if ("conformsTo" %in% names(obj))
      return("doc_conformance")
    if ("collections" %in% names(obj))
      return("doc_collections")
    if ("id" %in% names(obj) && "links" %in% names(obj))
      return("doc_collection")
    if ("links" %in% names(obj))
      return("doc_catalog")
    if ("openapi" %in% names(obj))
      return("openapi_schema")
    .error("Invalid STAC document.")
  }
}

as_rstac_doc <- function(x, base_url = NULL) {
  subclass <- stac_subclass(x)
  switch(
    subclass,
    doc_conformance = doc_conformance(x),
    doc_catalog = doc_catalog(x, base_url = base_url),
    doc_collection = doc_collection(x, base_url = base_url),
    doc_collections = doc_collections(x, base_url = base_url),
    doc_item = doc_item(x, base_url = base_url),
    doc_items = doc_items(x, base_url = base_url)
  )
}

doc_conformance <- function(x) {
  if (!is.list(x) || !"conformsTo" %in% names(x))
    .error("Invalid Conformance object.")
  rstac_doc(x, subclass = c("doc_conformance", "rstac_doc"))
}

doc_queryables <- function(x) {
  rstac_doc(x, subclass = c("doc_queryables", "rstac_doc"))
}

doc_link <- function(x, base_url = NULL) {
  if (!is.list(x) || !"href" %in% names(x) || !"rel" %in% names(x))
    .error("Invalid Link object.")
  if (!is.null(base_url))
    x[["rstac:base_url"]] <- base_url
  rstac_doc(x, subclass = c("doc_link"))
}

doc_links <- function(x, base_url = NULL) {
  if (is.null(x))
    x <- list()
  if (!is.list(x))
    .error("Invalid Links object.")
  x <- lapply(x, doc_link, base_url = base_url)
  x <- c(list(list(rel = "self", href = base_url)), x)
  rstac_doc(x, subclass = c("doc_links"))
}

doc_catalog <- function(x, base_url = NULL) {
  if (!is.list(x) || !"links" %in% names(x))
    .error("Invalid Catalog object.")
  x$links <- doc_links(x$links, base_url = base_url)
  rstac_doc(x, subclass = c("doc_catalog", "rstac_doc"))
}

doc_collection <- function(x, base_url = NULL) {
  if (!is.list(x) || !"links" %in% names(x))
    .error("Invalid Collection object.")
  x$links <- doc_links(x$links, base_url = base_url)
  rstac_doc(x, subclass = c("doc_collection", "rstac_doc"))
}

doc_collections <- function(x, base_url = NULL) {
  if (!is.list(x) || !"collections" %in% names(x))
    .error("Invalid Collections object.")
  x$links <- doc_links(x$links, base_url = base_url)
  x$collections <- lapply(x$collections, doc_collection)
  rstac_doc(x, subclass = c("doc_collections", "rstac_doc"))
}

doc_item <- function(x, base_url = NULL) {
  if (!is.list(x) || !"type" %in% names(x))
    .error("Invalid Item object.")
  if (x$type != "Feature")
    .error("Invalid Item object. Type '%s' is not supported.", x$type)
  if ("links" %in% names(x))
    x$links <- doc_links(x$links, base_url = base_url)
  rstac_doc(x, subclass = c("doc_item", "rstac_doc"))
}

doc_items <- function(x, base_url = NULL, query = NULL) {
  if (!is.list(x) || !"type" %in% names(x))
    .error("Invalid Items object.")
  if (x$type != "FeatureCollection")
    .error("Invalid Items object. Type '%s' is not supported.", x$type)
  if (!"features" %in% names(x))
    .error("Invalid Items object. Expecting 'features' key.")
  x$features <- lapply(x$features, doc_item)
  if ("links" %in% names(x))
    x$links <- doc_links(x$links, base_url = base_url)
  items <- rstac_doc(x, subclass = c("doc_items", "rstac_doc"))
  attr(items, "query") <- query
  items
}
