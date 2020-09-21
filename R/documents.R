
#' @export
subclass.RSTACDocument <- function(x) {

  class(x)[[1]]
}

#' @export
check_doc_subclass <- function(d, subclasses) {

  if (!subclass(d) %in% subclasses)
    .error("Expecting %s document(s).",
           paste0("`", subclasses, "`", collapse = " or "))
}

#' @export
get_doc_query <- function(d) {

  .check_obj(d, "RSTACDocument")

  attr(d, "stac")
}

#' @export
stac_version.RSTACDocument <- function(x, ...) {

  x$stac_version
}

####STACCatalog####

#' @export
print.STACCatalog <- function(x, ...) {

  cat("###STACCatalog", fill = TRUE)
  cat("attributes:", paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

#' @export
repr_html.STACCatalog <- function(obj, ...) {

  #TODO: HTML representation
}

####STACCollection####

#' @export
print.STACCollection <- function(x, ...) {

  cat("###STACCollection", fill = TRUE)
  cat("- id:", x$id, fill = TRUE)
  if (!is.null(x$title) && x$title != "")
    cat("- title:", x$title, fill = TRUE)
  if (!is.null(x$description) && x$description != "")
    cat("- description:", x$description, fill = TRUE)
  cat("- attributes:", paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

#' @export
repr_html.STACCollection <- function(obj, ...) {

  #TODO: HTML representation
}

####STACCollectionList####

#' @export
stac_version.STACCollectionList <- function(x, ...) {

  s <- get_doc_query(x)
  if (!is.null(s))
    return(stac_version(s))
  if (length(x$collections) > 0)
    return(x$collections[[1]]$stac_version)
}

#' @export
print.STACCollectionList <- function(x, n = 10, ...) {

  cat("###STACCollectionList", fill = TRUE)
  cat("- collections", sprintf("(%s item(s)):", length(x$collections)),
      fill = TRUE)
  # if (length(x$collections) > 0) cat(fill = TRUE)
  if (missing(n) && length(x$collections) < 2 * n)
    n <- length(x$collections)
  n <- min(n, length(x$collections))
  for (i in seq_len(n)) {
    e <- x$collections[[i]]
    cat(paste0("  - ", e$id), fill = TRUE)
  }
  if (n != length(x$collections))
    cat(sprintf("  - ... with %s more collection(s).",
                length(x$collections) - n), fill = TRUE)
  cat("- attributes:", paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

#' @export
repr_html.STACCollectionList <- function(obj, ...) {

  #TODO: HTML representation
}

####STACItem####

#' @export
print.STACItem <- function(x, ...) {

  cat("###STACItem", fill = TRUE)
  cat("attributes:", names(x), fill = TRUE)
  invisible(x)
}

#' @export
repr_html.STACItem <- function(obj, ...) {

  #TODO: HTML representation
}

####STACItemCollection####

#' @title STACItemCollection functions
#'
#' @description
#' The \code{items_length()} function shows how many items there are in
#' the \code{STACItemCollection} object.
#' The \code{items_matched()} function shows how many items matched the
#' search criteria. It support \code{search:metadata} (v0.8.0) and
#' \code{context} (v0.9.0) STAC API extensions.
#' The \code{items_fetch()} function request all STAC Items through
#' pagination.
#'
#' @param items      a \code{STACItemCollection} object.
#'
#' @return
#' The \code{items_length()} returns an \code{integer} value.
#' The \code{items_matched()} returns an \code{integer} value.
#' If STAC web server does not support this extension, returns \code{NULL}.
#' The \code{items_fetch()} returns an \code{STACItemCollection} with all
#' matched items.
#'
#' @examples
#' \donttest{
#'
#' x <-
#'   stac("http://brazildatacube.dpi.inpe.br/stac") %>%
#'   stac_search(collections = "MOD13Q1") %>%
#'   stac_search(bbox = c(-55.16335, -4.26325, -49.31739, -1.18355)) %>%
#'   stac_search(limit = 500) %>%
#'   get_request()
#'
#' x %>% items_length()
#' x %>% items_matched()
#' x %>% items_fetch()
#' x %>% items_length()
#'
#' }
#'
#' @name items_functions
#'
#' @export
items_length <- function(items) {

  # Check object class
  check_doc_subclass(items, c("STACItemCollection"))

  return(length(items$features))
}


#'
#' @rdname items_functions
#'
#' @export
items_matched <- function(items) {

  # STAC API (<0.9.0): "search:metadata"
  # STAC API (>=0.9.0): "context"

  # Check object class
  check_doc_subclass(items, "STACItemCollection")

  if (items$stac_version < "0.9.0")
    # STAC API < 0.9.0 extensions
    matched <- items$`search:metadata`$matched
  else
    # STAC API >= 0.9.0 extensions
    matched <- items$`context`$matched

  # try the last resort: WFS3 spec
  if (is.null(matched))
    matched <- items$numberMatched

  if (is.null(matched))
    .warning("Items matched not provided.")

  return(matched)
}

#' @param progress   a \code{logical} indicating if a progress bar must be
#' shown or not. Defaults to \code{TRUE}.
#'
#' @param ...        config parameters to be passed to \link[httr]{GET} or
#' \link[httr]{POST} methods, such as \link[httr]{add_headers} or
#' \link[httr]{set_cookies}.
#'
#' @rdname items_functions
#'
#' @export
items_fetch <- function(items, ..., progress = TRUE) {

  # Check object class
  check_doc_subclass(items, "STACItemCollection")

  matched <- items_matched(items)

  # verify if progress bar can be shown
  progress <- progress & (!is.null(matched) && (items_length(items) < matched))
  if (progress)
    pb <- utils::txtProgressBar(min = items_length(items), max = matched,
                                style = 3)

  while (TRUE) {

    # protect against infinite loop
    if (!is.null(matched) && (items_length(items) > matched))
      .error(paste("Length of returned items (%s) is different",
                   "from matched items (%s)."), items_length(items), matched)

    s <- get_doc_query(items)
    if (is.null(s)) break

    # get url of the next page
    next_url <- Filter(function(x) x$rel == "next", items$links)
    if (length(next_url) == 0) break

    # create a new stac object with params from the next url
    base_url <- gsub("^([^?]+)(\\?.*)?$", "\\1", next_url[[1]]$href)
    query <- substring(gsub("^([^?]+)(\\?.*)?$", "\\2", next_url[[1]]$href), 2)
    next_stac <- RSTACQuery(version = s$version,
                            url = base_url,
                            params = .query_decode(query),
                            subclass = "items_fetch")

    # call request
    if (s$verb == "GET") {

      content <- get_request(next_stac, ...)
    } else if (s$verb == "POST") {

      content <- post_request(next_stac, ..., encode = s$encode)
    } else {

      .error("Invalid HTTP method.")
    }

    # check content response
    check_doc_subclass(content, "STACItemCollection")

    # merge features result into resulting content
    content$features <- c(items$features, content$features)

    # update progress bar
    if (progress)
      utils::setTxtProgressBar(pb, items_length(content))

    # prepares next iteration
    items <- content
  }

  # close progress bar
  if (progress) {
    utils::setTxtProgressBar(pb, matched)
    close(pb)
  }

  return(items)
}

get_endpoint.items_fetch <- function(s) {
  ""
}

before_request.items_fetch <- function(s) {
  check_query_verb(s, c("GET", "POST"))

  return(s)
}

after_response.items_fetch <- function(s, res) {
  content <- content_response(res, "200", c("application/json",
                                            "application/geo+json"))

  RSTACDocument(content = content, s = s, subclass = "STACItemCollection")
}


#' @export
print.STACItemCollection <- function(x, n = 10, ...) {

  cat("###STACItemCollection", fill = TRUE)
  matched <- suppressWarnings(items_matched(x))
  if (!is.null(matched))
    cat("- matched features:", matched, fill = TRUE)
  cat("- features", sprintf("(%s item(s)):", length(x$features)),
      fill = TRUE)
  # if (length(x$collections) > 0) cat(fill = TRUE)
  if (missing(n) && length(x$features) < 2 * n)
    n <- length(x$features)
  n <- min(n, length(x$features))
  for (i in seq_len(n)) {
    e <- x$features[[i]]
    cat(paste0("  - ", e$id), fill = TRUE)
  }
  if (n != length(x$features))
    cat(sprintf("  - ... with %s more feature(s).",
                length(x$features) - n), fill = TRUE)
  cat("- attributes:", paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

#' @export
repr_html.STACItemCollection <- function(obj, ...) {

  #TODO: HTML representation
}

#' @export
head.STACItemCollection <- function(x, n, ...) {

  # TODO: implement head
}

#' @export
tail.STACItemCollection <- function(x, n, ...) {

  # TODO: implement tail
}
