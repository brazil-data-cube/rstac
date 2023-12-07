#' @title Static functions
#'
#' @description
#' These functions provide support to work with static catalogs.
#'
#' \itemize{
#' \item `stac_read()`: open a STAC document from an URL.
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
#' @param collection  a `doc_collection` object to fetch all item links.
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
#'  x <- stac("https://brazildatacube.dpi.inpe.br/stac") %>%
#'      collections("CB4-16D-2") %>%
#'      get_request()
#'
#'  link <- links(x, rel == "items")
#'  link_open(link[[1]])
#' }
#'
#' \dontrun{
#'  x <- stac_read(
#'    "https://s3.eu-central-1.wasabisys.com/stac/openlandmap/wv_mcd19a2v061.seasconv/collection.json"
#'  )
#'
#'  read_items(x, limit = 10, page = 2) # reads the second page of 10 links
#'
#' }
#'
#' @name static_functions
NULL

#' @rdname static_functions
#'
#' @export
stac_read <- function(url, ...) {
  check_character(url, "STAC URL must be a character value.")
  content <- jsonlite::read_json(url, ...)
  # create an rstac doc from content and return
  as_rstac_doc(content, base_url = url)
}

#' @rdname static_functions
#'
#' @export
read_items <- function(collection, limit = 100, page = 1, progress = TRUE) {
  check_collection(collection)
  rel <- NULL
  link_items <- links(collection, rel == "item")
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
    features <- c(features, list(link_open(link_items[[i]])))
  }
  # Convert to doc_items object and return
  doc_items(
    x = list(type = "FeatureCollection", features = features),
    base_url = url
  )
}

#' @rdname static_functions
#'
#' @export
links <- function(x, ...) {
  exprs <- unquote(
    expr = as.list(substitute(list(...), env = environment())[-1]),
    env =  parent.frame()
  )
  sel <- !logical(length(x$links))
  for (expr in exprs) {
    sel <- sel & map_lgl(x$links, function(x) eval(expr, envir = x))
  }
  structure(x$links[sel], class = c("doc_links", "list"))
}

#' @rdname static_functions
#'
#' @export
link_open <- function(link, base_url = NULL) {
  if (is.list(link)) {
    check_link(link)
    url <- link$href
    if (!is.null(base_url))
      url <- resolve_url(base_url, url)
    else if ("rstac:base_url" %in% names(link))
      url <- resolve_url(link[["rstac:base_url"]], url)
  } else if (is.character(link))
    url <- link
  content <- jsonlite::read_json(url)
  # create an rstac doc from content and return
  as_rstac_doc(content, base_url = url)
}
