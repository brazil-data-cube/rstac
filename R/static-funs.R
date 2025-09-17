#' @title Static functions
#'
#' @description
#' These functions provide support to work with static catalogs.
#'
#' \itemize{
#' \item `read_stac()`: open a STAC document from an URL.
#'
#' \item `read_items()`: opens (statically) all items referred in `links`
#'   key entry of a given collection document (`doc_collection`).
#'
#' \item `links()`: extracts and filters the links of any STAC document.
#'
#' \item `link_open()`: opens (statically) the document referenced by
#'   the link. This function can resolve any relative URL.
#' }
#'
#' @param url     a `character` value with the URL to a valid STAC document.
#'
#' @param catalog  a `doc_catalog` object to fetch all `rel=="child"` links.
#'
#' @param collection  a `doc_collection` object to fetch all
#'   `rel=="item"` links.
#'
#' @param limit     an `integer` with defining the page size of items to fetch.
#'
#' @param page      an `integer` with the page number to fetch the items.
#'
#' @param progress  a `logical` indicating if a progress bar must be
#' shown or not. Defaults to `TRUE`.
#'
#' @param x       any `rstac` document with `'links'` key entry.
#'
#' @param link    a `doc_link` object, usually an element of `links` key entry.
#'
#' @param base_url  a `character` with the base URL to resolve relative links.
#'   If `NULL` (default) `rstac` will try resolve relative links using
#'   internal metadata.
#'
#' @param ...     additional arguments. See details.
#'
#' @details
#' Ellipsis argument (`...`) may appears in different items functions and
#' has distinct purposes:
#' \itemize{
#'
#' \item `stac_read()`: ellipsis is used to pass any additional parameters
#' to [read_json][jsonlite::read_json] function.
#'
#' \item `links()`: ellipsis is used to pass logical expressions to be
#' evaluated against a `doc_link` item as a filter criteria. See examples.
#'
#' }
#'
#' @return
#'
#' \itemize{
#' \item `links()`: a `doc_links` object containing a list of `link` entries.
#'
#' \item `link_open()`: a recognizable `rstac` document.
#' }
#'
#' @examples
#' \dontrun{
#'  x <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
#'      collections("CBERS4-WFI-16D-2") %>%
#'      get_request()
#'
#'  link <- links(x, rel == "items")
#'  link_open(link[[1]])
#' }
#'
#' \dontrun{
#'  wv_url <- paste0(
#'    "https://s3.eu-central-1.wasabisys.com",
#'    "/stac/openlandmap/wv_mcd19a2v061.seasconv/collection.json"
#'  )
#'  wv <- read_stac(wv_url)
#'  stac_type(wv)  # Collection
#'
#'  # reads the second page of 5 links
#'  wv_items <- read_items(wv, limit = 5, page = 2)
#'
#'  # lists all links of the collection document that are not items
#'  links(wv, rel != "item")
#'
#'  # lists all links of the items document
#'  links(wv_items)
#' }
#'
#' @name static_functions
NULL

#' @rdname static_functions
#'
#' @export
read_stac <- function(url, ...) {
  check_character(url, "STAC URL must be a character value.")
  content <- jsonlite::read_json(url, ...)
  # create an rstac doc from content and return
  as_rstac_doc(content, base_url = url)
}

#' @rdname static_functions
#'
#' @export
read_items <- function(collection, ...,
                       limit = 100,
                       page = 1,
                       progress = TRUE) {
  UseMethod("read_items", collection)
}

#' @export
read_items.doc_collection <- function(collection, ...,
                                      limit = 100,
                                      page = 1,
                                      progress = TRUE) {
  check_collection(collection)
  rel <- NULL
  link_items <- links(collection, rel == "item", ...)
  if (is.null(limit) || limit < 1)
    limit <- length(link_items)
  limit <- max(1, as.integer(limit))
  page <- max(1, as.integer(page))
  pages <- ceiling(length(link_items) / limit)
  if (page > pages)
    return(NULL)
  if (length(link_items) > limit) {
    previous_len <- (page - 1) * limit
    len <- min(limit, length(link_items) - previous_len)
    link_items <- link_items[previous_len + seq_len(len)]
  }

  # verify if progress bar can be shown
  progress <- progress && length(link_items) > 1
  if (progress) {
    pb <- utils::txtProgressBar(max = length(link_items), style = 3)
    # close progress bar when exit
    on.exit(if (progress) close(pb))
  }
  features <- list()
  for (i in seq_along(link_items)) {
    if (progress)
      utils::setTxtProgressBar(pb, i)
    item <- link_open(link_items[[i]])
    features <- c(features, list(item))
  }
  # Convert to doc_items object and return
  parent <- links(collection, rel == "self")
  if (length(parent) > 0) {
    parent <- parent[[1]]
    parent$rel <- "parent"
    parent <- list(parent)
  }
  doc_items(list(
    type = "FeatureCollection",
    features = features,
    links = parent
  ))
}

#' @rdname static_functions
#'
#' @export
read_collections <- function(catalog, ...,
                             limit = 100,
                             page = 1,
                             progress = TRUE) {
  UseMethod("read_collections", catalog)
}

#' @export
read_collections.catalog <- function(catalog, ...,
                                     limit = 100,
                                     page = 1,
                                     progress = TRUE) {
  check_catalog(catalog)
  rel <- NULL
  link_collections <- links(catalog, rel == "child", ...)
  if (is.null(limit) || limit < 1)
    limit <- length(link_collections)
  limit <- max(1, as.integer(limit))
  page <- max(1, as.integer(page))
  pages <- ceiling(length(link_collections) / limit)
  if (page > pages)
    return(NULL)
  if (length(link_collections) > limit) {
    previous_len <- (page - 1) * limit
    len <- min(limit, length(link_collections) - previous_len)
    link_collections <- link_collections[previous_len + seq_len(len)]
  }

  # verify if progress bar can be shown
  progress <- progress && length(link_collections) > 1
  if (progress) {
    pb <- utils::txtProgressBar(max = length(link_collections), style = 3)
    # close progress bar when exit
    on.exit(if (progress) close(pb))
  }
  collections <- list()
  for (i in seq_along(link_collections)) {
    if (progress)
      utils::setTxtProgressBar(pb, i)
    collection <- link_open(link_collections[[i]])
    collections <- c(collections, list(collection))
  }
  # Convert to doc_items object and return
  parent <- links(catalog, rel == "self")
  if (length(parent) > 0) {
    parent <- parent[[1]]
    parent$rel <- "parent"
    parent <- list(parent)
  }
  doc_collections(list(
    collections = collections,
    links = parent
  ))
}

#' @rdname static_functions
#'
#' @export
links <- function(x, ...) {
  UseMethod("links")
}

#' @export
links.rstac_doc <- function(x, ...) {
  exprs <- as.list(substitute(list(...), env = environment()))[-1]
  sel <- !logical(length(x$links))
  for (expr in exprs) {
    expr <- unquote(expr = expr, env =  parent.frame())
    sel <- sel & map_lgl(x$links, function(x) {
      tryCatch(
        eval(expr, envir = x),
        error = function(e) {
          FALSE
        }
      )
    })
  }
  structure(x$links[sel], class = c("doc_links", "list"))
}

#' @rdname static_functions
#'
#' @export
link_open <- function(link, base_url = NULL) {
  UseMethod("link_open", link)
}

#' @export
link_open.doc_link <- function(link, base_url = NULL) {
  check_link(link)
  url <- link$href
  if (!is.null(base_url))
    url <- resolve_url(base_url, url)
  else if ("rstac:base_url" %in% names(link))
    url <- resolve_url(link[["rstac:base_url"]], url)
  content <- jsonlite::read_json(url)
  # create an rstac doc from content and return
  as_rstac_doc(content, base_url = url)
}
