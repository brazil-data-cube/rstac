#' @title Document development functions
#'
#' @describeIn extensions
#' The \code{RSTACDocument()} function is a constructor of
#' STAC documents. Currently, there are five STAC documents defined:
#' \itemize{
#' \item \code{STACCatalog}
#' \item \code{STACCollection}
#' \item \code{STACCollectionList}
#' \item \code{STACItem}
#' \item \code{STACItemCollection}
#' }
#'
#' Each document class is associated with STAC API endpoints.
#' As soon as new STAC documents are proposed in the specification, new
#' classes can be created in the \code{rstac} package.
#'
#' Let \code{version} parameter \code{NULL} to detect version automatically.
#'
#' @param content    a \code{list} data structure representing the JSON file
#' received in HTTP response (see \code{\link{content_response}()} function)
#'
#' @param q          a \code{RSTACQuery} object expressing the STAC query used
#' to retrieve the document.
#'
#' @param subclass   a \code{character} corresponding to the subclass of the
#' document to be created.
#'
#' @return
#' The \code{RSTACDocument()} function returns a \code{RSTACDocument} object
#' with subclass defined by \code{subclass} parameter.
#'
#' @export
RSTACDocument <- function(content, q, subclass) {

  structure(
    content,
    query = q,
    class = c(subclass, "RSTACDocument")
  )
}

#' @export
subclass.RSTACDocument <- function(x) {

  class(x)[[1]]
}

#' @export
check_subclass.RSTACDocument <- function(x, subclasses) {

  if (!subclass(x) %in% subclasses)
    .error("Expecting %s document(s).",
           paste0("`", subclasses, "`", collapse = " or "))
}

#' @title Document utils functions
#'
#' @param d \code{RSTACDocument} object
#'
#' @return a \code{RSTACQuery} object with the predecessor subclass with the
#'  fields used in the request.
#'
#' @export
get_doc_query <- function(d) {

  .check_obj(d, "RSTACDocument")

  attr(d, "query")
}

#' @export
stac_version.RSTACDocument <- function(x, ...) {

  if (is.null(x$stac_version))
    return(stac_version(get_doc_query(x)))
  x$stac_version
}

####STACCatalog####

#' @export
print.STACCatalog <- function(x, ...) {

  cat(crayon::bold("###STACCatalog"), fill = TRUE)
  cat("-", crayon::bold("id:"), x$id, fill = TRUE)
  if (!is.null(x$description) && x$description != "")
    cat("-", crayon::bold("description:"), x$description, fill = TRUE)
  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

####STACCollection####

#' @export
print.STACCollection <- function(x, ...) {

  cat(crayon::bold("###STACCollection"), fill = TRUE)
  cat("-", crayon::bold("id:"), x$id, fill = TRUE)
  if (!is.null(x$title) && x$title != "")
    cat("-", crayon::bold("title:"), x$title, fill = TRUE)
  if (!is.null(x$description) && x$description != "")
    cat("-", crayon::bold("description:"), x$description, fill = TRUE)
  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

####STACCollectionList####

#' @export
stac_version.STACCollectionList <- function(x, ...) {

  q <- get_doc_query(x)
  if (!is.null(q))
    return(stac_version(q))
  if (length(x$collections) > 0)
    return(x$collections[[1]]$stac_version)
}

#' @export
print.STACCollectionList <- function(x, n = 10, ...) {

  cat(crayon::bold("###STACCollectionList"), fill = TRUE)
  cat("-", crayon::bold("collections"),
      sprintf("(%s item(s)):", length(x$collections)), fill = TRUE)
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
  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}


####STACItem####

#' @export
print.STACItem <- function(x, ...) {

  cat(crayon::bold("###STACItem"), fill = TRUE)
  cat("-", crayon::bold("id:"), x$id, fill = TRUE)
  cat("-", crayon::bold("collection:"), x$collection, fill = TRUE)
  cat("-", crayon::bold("bbox:"), .format_bbox(x$bbox), fill = TRUE)
  cat("-", crayon::bold("datetime:"), x$properties$datetime, fill = TRUE)
  cat("-", crayon::bold("assets:"),
      paste0("'", names(x$assets), "'", collapse = ", "), fill = TRUE)
  cat("-", crayon::bold("field(s):"), paste0(names(x), collapse = ", "),
      fill = TRUE)
  invisible(x)
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
#' The \code{get_assets_name()} function returns the assets name from
#'  \code{STACItemCollection} and \code{STACItem} object.
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
#' \dontrun{
#'
#' x <-
#'   stac("http://brazildatacube.dpi.inpe.br/stac") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1") %>%
#'   stac_search(limit = 500) %>%
#'   get_request()
#'
#' x %>% items_length()
#' x %>% items_matched()
#' }
#'
#' @name items_functions
#'
#' @export
items_length <- function(items) {

  # Check object class
  check_subclass(items, "STACItemCollection")

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
  check_subclass(items, "STACItemCollection")

  if (stac_version(items) < "0.9.0")
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
#' @examples
#' \dontrun{
#' x <-
#'   stac("http://brazildatacube.dpi.inpe.br/stac") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1") %>%
#'   stac_search(limit = 500) %>%
#'   get_request()
#'
#' x %>% items_fetch()
#' }
#'
#' @rdname items_functions
#'
#' @export
items_fetch <- function(items, ..., progress = TRUE) {

  # Check object class
  check_subclass(items, "STACItemCollection")

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

    q <- get_doc_query(items)
    if (is.null(q)) break

    # get url of the next page
    next_url <- Filter(function(x) x$rel == "next", items$links)
    if (length(next_url) == 0) break

    # create a new stac object with params from the next url
    # TODO: This is not the specified behavior in spec:
    # https://github.com/radiantearth/stac-spec/blob/v0.9.0/api-spec/api-spec.md
    #base_url <- gsub("^([^?]+)(\\?.*)?$", "\\1", next_url[[1]]$href)
    params <- .querystring_decode(substring(
      gsub("^([^?]+)(\\?.*)?$", "\\2", next_url[[1]]$href), 2))

    next_stac <- RSTACQuery(version = q$version,
                            base_url = q$base_url,
                            params = utils::modifyList(q$params, params),
                            subclass = subclass(q))

    # call request
    if (q$verb == "GET") {

      content <- get_request(next_stac, ...)
    } else if (q$verb == "POST") {

      content <- post_request(next_stac, ..., encode = q$encode)
    } else {

      .error("Invalid HTTP method.")
    }

    # check content response
    check_subclass(content, "STACItemCollection")

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

#' @export
print.STACItemCollection <- function(x, n = 10, ...) {

  cat(crayon::bold("###STACItemCollection"), fill = TRUE)
  matched <- suppressWarnings(items_matched(x))
  if (!is.null(matched))
    cat("-", crayon::bold("matched feature(s):"), matched, fill = TRUE)
  cat("-", crayon::bold("features"),
      sprintf("(%s item(s)):", length(x$features)), fill = TRUE)
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
  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

#' @export
head.STACItemCollection <- function(x, n, ...) {

  # TODO: implement head
}

#' @export
tail.STACItemCollection <- function(x, n, ...) {

  # TODO: implement tail
}
