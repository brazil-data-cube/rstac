#' @title Document development functions
#'
#' @describeIn extensions
#' The `RSTACDocument()` function is a constructor of
#' STAC documents. Currently, there are five STAC documents defined:
#' \itemize{
#' \item `STACCatalog`
#' \item `STACCollection`
#' \item `STACCollectionList`
#' \item `STACItem`
#' \item `STACItemCollection`
#' }
#'
#' Each document class is associated with STAC API endpoints.
#' As soon as new STAC documents are proposed in the specification, new
#' classes can be created in the `rstac` package.
#'
#' Let `version` parameter `NULL` to detect version automatically.
#'
#' @param content    a `list` data structure representing the JSON file
#' received in HTTP response (see [content_response()] function)
#'
#' @param q          a `RSTACQuery` object expressing the STAC query used
#' to retrieve the document.
#'
#' @param subclass   a `character` corresponding to the subclass of the
#' document to be created.
#'
#' @return
#' The `RSTACDocument()` function returns a `RSTACDocument` object
#' with subclass defined by `subclass` parameter.
#'
#' @export
RSTACDocument <- function(content, q, subclass) {

  structure(
    content,
    query = q,
    class = c(subclass, "RSTACDocument", "list")
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
#' @param d `RSTACDocument` object
#'
#' @return a `RSTACQuery` object with the predecessor subclass with the
#'  fields used in the request.
#'
#' @export
doc_query <- function(d) {

  .check_obj(d, "RSTACDocument")

  attr(d, "query")
}

#' @export
stac_version.RSTACDocument <- function(x, ...) {

  if (is.null(x$stac_version))
    return(stac_version(doc_query(x)))
  x$stac_version
}

####STACCollectionList####

#' @export
stac_version.STACCollectionList <- function(x, ...) {

  q <- doc_query(x)
  if (!is.null(q))
    return(stac_version(q))
  if (length(x$collections) > 0)
    return(x$collections[[1]]$stac_version)
}

#' @title STACItemCollection functions
#'
#' @description
#' The `items_length()` function shows how many items there are in
#' the `STACItemCollection` object.
#' The `items_matched()` function shows how many items matched the
#' search criteria. It supports `search:metadata` (v0.8.0),
#' `context` (v0.9.0), and `numberMatched` (OGC WFS3 core spec).
#' The `items_fetch()` function request all STAC Items through
#' pagination.
#' The `items_datetime()` function retrieves a the `datetime`
#' field in `properties` from `STACItemCollection` and
#' `STACItem` objects.
#' The `items_bbox()` function retrieves a the `bbox`
#' field of a `STACItemCollection` or an `STACItem` object.
#' The `get_assets_name()` function returns the assets name from
#' `STACItemCollection` and `STACItem` objects.
#'
#' @param items           a `STACItemCollection` object.
#' @param matched_field   a `character` vector with the path
#' where the number of items returned in the named list is located starting from
#' the initial node of the list. For example, if the information is at position
#' `items$meta$found` of the object, it must be passed as the following
#' parameter `c("meta", "found")`.
#'
#' @return
#' The `items_length()` returns an `integer` value.
#' The `items_matched()` returns an `integer` value.
#' If STAC web server does not support this extension, returns `NULL`.
#' The `items_fetch()` returns an `STACItemCollection` with all
#' matched items.
#' The `items_datetime()` returns a `list` of all items' datetime.
#' The `items_bbox()` returns a `list` with all items'
#' bounding boxes.
#'
#' @examples
#' \dontrun{
#'
#' x <- stac("http://brazildatacube.dpi.inpe.br/stac") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1") %>%
#'   stac_search() %>%
#'   get_request()
#'
#' x %>% items_length()
#' x %>% items_matched()
#' x %>% items_datetime()
#' x %>% items_bbox()
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

#' @rdname items_functions
#'
#' @export
items_matched <- function(items, matched_field = NULL) {

  # Check object class
  check_subclass(items, "STACItemCollection")

  matched <- NULL

  # try by the matched_field provided by user. This allow users specify a
  # non-standard field for matched items.
  if (!is.null(matched_field)) {

    tryCatch({
      matched <- as.numeric(items[[matched_field]])
    },
    error = function(e) .warning(paste("The provided field was not found in",
                                       "items object.")))
  }

  if (is.null(matched)) {

    if (stac_version(items) < "0.9.0")
      # STAC API < 0.9.0 extensions
      matched <- items$`search:metadata`$matched
    else
      # STAC API >= 0.9.0 extensions
      matched <- items$`context`$matched

    # try the last resort: OGC features core spec
    if (is.null(matched))
      matched <- items$numberMatched
  }

  if (is.null(matched))
    .warning("Items matched not provided.")

  return(matched)
}

#' @param progress   a `logical` indicating if a progress bar must be
#' shown or not. Defaults to `TRUE`.
#'
#' @param ...        config parameters to be passed to [GET][httr::GET] or
#' [POST][httr::POST] methods, such as [add_headers][httr::add_headers] or
#' [set_cookies][httr::set_cookies].
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
items_fetch <- function(items, ..., progress = TRUE, matched_field = NULL) {

  # Check object class
  check_subclass(items, "STACItemCollection")

  matched <- items_matched(items, matched_field)

  # verify if progress bar can be shown
  progress <- progress & (!is.null(matched) && (items_length(items) < matched))
  if (progress)
    pb <- utils::txtProgressBar(min = items_length(items), max = matched,
                                style = 3)

  while (TRUE) {

    # check if features is complete
    if (!is.null(matched) && (items_length(items) == matched))
      break

    # protect against infinite loop
    if (!is.null(matched) && (items_length(items) > matched))
      .error(paste("Length of returned items (%s) is different",
                   "from matched items (%s)."), items_length(items), matched)

    q <- doc_query(items)
    if (is.null(q)) break

    # get url of the next page
    next_url <- Filter(function(x) x$rel == "next", items$links)
    if (length(next_url) == 0) break
    next_url <- next_url[[1]]

    # create a new stac object with params from the next url
    # check for body implementation in next link
    if (q$verb == "POST" && all(c("body", "method") %in% names(next_url))) {

      # TODO: check if spec can enforce that the same provided base url
      # must be used to proceed pagination.
      # For security concerns, here, the original base_url will be used in
      # subsequent requests of pagination

      # # update query base_url and verb to the returned one
      # q$base_url <- next_url$href

      # erase current parameters if merge == FALSE
      if (!is.null(next_url$merge) && !next_url$merge) {
        q$params <- list()
      }

      # get parameters
      params <- next_url$body

    } else {

      # TODO: check if spec can enforce that the same provided base url
      # must be used to proceed pagination.
      # For security concerns, here, the original base_url will be used in
      # subsequent requests of pagination

      # # update query base_url and verb to the returned one
      # q$base_url <- gsub("^([^?]+)(\\?.*)?$", "\\1", next_url$href)

      # get next link parameters from url
      params <- .querystring_decode(substring(
        gsub("^([^?]+)(\\?.*)?$", "\\2", next_url$href), 2))

      # verify if query params is valid
      params <- .validate_query(params = params)
    }

    # parse params
    params <- parse_params(q, params = params)

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

    # check pagination length
    if (!is.null(q$params[["limit"]]) &&
        items_length(content) > as.numeric(q$params[["limit"]])) {

      .error("STAC invalid retrieved page length.")
    }

    # check if result length is valid
    if (!is.null(matched) && !is.null(q$params[["limit"]]) &&
        (items_length(content) != as.numeric(q$params[["limit"]])) &&
        (items_length(content) + items_length(items) != matched)) {

      .error("STAC pagination error.")
    }

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

#' @rdname items_functions
#'
#' @export
items_datetime <- function(items) {

  # Check object class
  check_subclass(items, c("STACItemCollection", "STACItem"))

  if (subclass(items) == "STACItem")
    return(items$properties[["datetime"]])

  lapply(items$features, `[[`, c("properties", "datetime"))
}

#' @rdname items_functions
#'
#' @export
items_bbox <- function(items) {

  # Check object class
  check_subclass(items, c("STACItemCollection", "STACItem"))

  if (subclass(items) == "STACItem")
    return(items[["bbox"]])

  lapply(items$features, `[[`, c("bbox"))
}

#' @rdname items_functions
#'
#' @export
items_bands <- function(items) {

  # Check object class
  check_subclass(items, c("STACItemCollection", "STACItem"))

  if (subclass(items) == "STACItem")
    return(names(items[["assets"]]))

  lapply(lapply(items$features, `[[`, c("assets")), names)
}
