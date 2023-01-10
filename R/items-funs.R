#' @title Items functions
#'
#' @description
#' These functions provide support to work with
#' `STACItemCollection` and `STACItem` objects.
#'
#' \itemize{
#' \item `items_length()`: shows how many items there are in
#' the `STACItemCollection` object.
#'
#' \item `items_matched()`: shows how many items matched the
#' search criteria. It supports `search:metadata` (v0.8.0),
#' `context` (v0.9.0), and `numberMatched` (OGC WFS3 core spec).
#'
#' \item `items_fetch()`: request all STAC Items through
#' pagination.
#'
#' \item `items_next()`: fetches a new page from STAC service.
#'
#' \item `items_datetime()`: retrieves a the `datetime`
#' field in `properties` from `STACItemCollection` and
#' `STACItem` objects.
#'
#' \item `items_bbox()`: retrieves a the `bbox`
#' field of a `STACItemCollection` or an `STACItem` object.
#'
#' \item `item_assets()`: returns the assets name from
#' `STACItemCollection` and `STACItem` objects.
#'
#' \item `items_filter()`: selects only items that match some criteria
#'  (see details section).
#'
#' \item `items_reap()`: extract key values by traversing all items
#' in an `STACItemCollection` object.
#'
#' \item `items_fields()`: lists field names inside an item.
#'
#' \item `items_group()`: `r lifecycle::badge('deprecated')` organizes
#' items as elements of a list using some criteria.
#'
#' \item `items_sign()`: allow access assets by preparing its url.
#'
#' \item `items_as_sf()`: `r lifecycle::badge('experimental')` convert items to `sf` object.
#' }
#'
#' @param items           a `STACItemCollection` object.
#'
#' @param matched_field   a `character` vector with the path
#' where the number of items returned in the named list is located starting from
#' the initial node of the list. For example, if the information is at position
#' `items$meta$found` of the object, it must be passed as the following
#' parameter `c("meta", "found")`.
#'
#' @param progress        a `logical` indicating if a progress bar must be
#' shown or not. Defaults to `TRUE`.
#'
#' @param simplify        `r lifecycle::badge('deprecated')` no side-effect
#'
#' @param field           a `character` with the names of the field to
#' get the subfields values.
#'
#' @param index           an `atomic` vector with values as group index
#'
#' @param sign_fn         a `function` that receives an item as parameter
#' and returns an item signed.
#'
#' @param filter_fn       a `function` that receives an item that should
#' evaluate a `logical` value.
#'
#' @param ...             additional arguments. See details.
#'
#' @details
#' Ellipsis argument (`...`) appears in different items function and
#' has distinct purposes:
#' \itemize{
#' \item `items_matched()` and `items_assets()`: ellipsis is not used.
#'
#' \item `items_fetch()` and `items_next()`: ellipsis is used to pass
#' additional `httr` options to [GET][httr::GET] or [POST][httr::POST]
#' methods, such as [add_headers][httr::add_headers] or
#' [set_cookies][httr::set_cookies].
#'
#' \item `items_filter()`: ellipsis is used to pass logical
#' expressions to be evaluated against a `STACItem` fields as filter criteria.
#'
#' **WARNING:** the evaluation of filter expressions changed in `rstac` 0.9.2.
#' Older versions of `rstac` used `properties` field to evaluate filter
#' expressions. Below, there is an example on how to write expressions in new
#' `rstac` version:
#' ```R
#' # expression in older version
#' items_filter(stac_obj, `eo:cloud_cover` < 10)
#' # now expressions must refer to properties explicitly
#' items_filter(stac_obj, properties$`eo:cloud_cover` < 10)
#' items_filter(stac_obj, properties[["eo:cloud_cover"]] < 10)
#' ```
#'
#' \item `items_sign()`: in a near future, ellipsis will be used to append
#' key value pairs to url query string of an asset.
#' }
#'
#' `items_sign()` has `sign_fn` parameter that must be a function that
#' receives as argument an item and returns an signed item. `rstac` provides
#' `sign_bdc()` and `sign_planetary_computer()` functions to access Brazil
#' Data Cube products and Microsoft Planetary Computer catalogs, respectively.
#'
#' @return
#'
#' \itemize{
#' \item `items_length()`: an `integer` value.
#'
#' \item `items_matched()`: returns an `integer` value if STAC web server does
#' support this extension, otherwise returns `NULL`.
#'
#' \item `items_fetch()`: an `STACItemCollection` with all matched items.
#'
#' \item `items_next()`: fetches a new page from STAC service.
#'
#' \item `items_datetime()`: a `list` of all items' datetime.
#'
#' \item `items_bbox()`: returns a `list` with all items' bounding boxes.
#'
#' \item `item_assets()`: if simplify is `TRUE`, returns a `character`
#' value with all assets names of the first item. Otherwise, returns a
#' `list` with assets name for each item.
#'
#' \item `items_filter()`: a `STACItemCollection` object.
#'
#' \item `items_reap()`: a `vector` if the supplied field is atomic,
#' otherwise or a `list`.
#'
#' \item `items_fields()`: a `character` vector.
#'
#' \item `items_group()`: a `list` of `STACItemCollection` objects.
#'
#' \item `items_sign()`: a `STACItemCollection` object with signed assets url.
#'
#' \item `items_as_sf()`: a `sf` object.
#'
#' }
#'
#'
#' @examples
#' \dontrun{
#'  x <- stac("https://brazildatacube.dpi.inpe.br/stac") %>%
#'      stac_search(collections = "CB4_64_16D_STK-1") %>%
#'      stac_search(limit = 500) %>%
#'      get_request()
#'
#'  x %>% items_length()
#'  x %>% items_matched()
#'  x %>% items_datetime()
#'  x %>% items_bbox()
#'  x %>% items_fetch()
#' }
#'
#' \dontrun{
#' # Defining BDC token
#' Sys.setenv("BDC_ACCESS_KEY" = "token-123")
#'
#' # STACItem object
#' stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'     stac_search(collections = "CB4_64_16D_STK-1", limit = 100,
#'         datetime = "2017-08-01/2018-03-01",
#'         bbox = c(-48.206, -14.195, -45.067, -12.272)) %>%
#'     get_request() %>% items_sign(sign_fn = sign_bdc())
#'
#' }
#'
#' \dontrun{
#' # STACItemCollection object
#' stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'     stac_search(collections = "CB4_64_16D_STK-1", limit = 100,
#'         datetime = "2017-08-01/2018-03-01",
#'         bbox = c(-48.206, -14.195, -45.067, -12.272)) %>%
#'     get_request() %>%
#'     items_filter(properties$`eo:cloud_cover` < 10)
#'
#' # Example with AWS STAC
#' stac("https://earth-search.aws.element84.com/v0") %>%
#'   stac_search(collections = "sentinel-s2-l2a-cogs",
#'               bbox = c(-48.206, -14.195, -45.067, -12.272),
#'               datetime = "2018-06-01/2018-06-30",
#'               limit = 500) %>%
#'   post_request() %>%
#'   items_filter(filter_fn = function(x) {x$properties$`eo:cloud_cover` < 10})
#' }
#'
#' \dontrun{
#' # STACItemCollection object
#' stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'  stac_search(collections = "CB4_64_16D_STK-1", limit = 100,
#'         datetime = "2017-08-01/2018-03-01",
#'         bbox = c(-48.206, -14.195, -45.067, -12.272)) %>%
#'  get_request() %>% items_fetch(progress = FALSE)
#'
#' stac_item %>% items_reap(field = c("properties", "datetime"))
#' }
#'
#' @name items_functions
NULL

#' @rdname items_functions
#'
#' @export
items_length <- function(items) {
  UseMethod("items_length", items)
}

#' @rdname items_functions
#'
#' @export
items_length.STACItem <- function(items) {
  check_items(items)
  return(1)
}

#' @rdname items_functions
#'
#' @export
items_length.STACItemCollection <- function(items) {
  check_items(items)
  return(length(items$features))
}

#' @rdname items_functions
#'
#' @export
items_length.default <- items_length.STACItem

#' @rdname items_functions
#'
#' @export
items_matched  <- function(items, matched_field = NULL) {
  UseMethod("items_matched", items)
}

#' @rdname items_functions
#'
#' @export
items_matched.STACItem  <- function(items, matched_field = NULL) {
  check_items(items)
  return(1)
}

#' @rdname items_functions
#'
#' @export
items_matched.STACItemCollection <- function(items, matched_field = NULL) {
  check_items(items)
  matched <- NULL

  # try by the matched_field provided by user. This allow users specify a
  # non-standard field for matched items.
  if (is.character(matched_field) && matched_field %in% names(items)) {
      matched <- as.numeric(items[[matched_field]])
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
  return(matched)
}

#' @rdname items_functions
#'
#' @export
items_matched.default <- items_matched.STACItem

#' @rdname items_functions
#'
#' @export
items_fetch <- function(items, ...) {
  UseMethod("items_fetch", items)
}

#' @rdname items_functions
#'
#' @export
items_fetch.STACItemCollection <- function(items, ...,
                                           progress = TRUE,
                                           matched_field = NULL) {

  matched <- items_matched(items, matched_field)

  # verify if progress bar can be shown
  progress <- progress & (!is.null(matched) && (items_length(items) < matched))
  if (progress)
    pb <- utils::txtProgressBar(
      min = items_length(items),
      max = matched,
      style = 3
    )

  while (TRUE) {

    # check if features is complete
    if (!is.null(matched) && (items_length(items) == matched))
      break

    # protect against infinite loop
    if (!is.null(matched) && (items_length(items) > matched))
      .error(paste("Length of returned items (%s) is different",
                   "from matched items (%s)."), items_length(items), matched)

    content <- tryCatch({
      items_next(items, ...)
    },
    next_error = function(e) NULL
    )

    if (!is.null(content))
      items <- content
    else
      break

    # update progress bar
    if (progress)
      utils::setTxtProgressBar(pb, items_length(content))
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
items_next <- function(items, ...) {
  UseMethod("items_next", items)
}

#' @rdname items_functions
#'
#' @export
items_next.STACItemCollection <- function(items, ...) {
  matched <- items_matched(items)

  q <- doc_query(items)
  if (is.null(q))
    .error("Cannot get next link URL", class = "next_error")

  # get url of the next page
  next_url <- Filter(function(x) x$rel == "next", items$links)
  if (length(next_url) == 0)
    .error("Cannot get next link URL", class = "next_error")

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
      gsub("^([^?]+)(\\?.*)?$", "\\2", next_url$href), 2)
    )

    # verify if query params is valid
    params <- .validate_query(params = params)
  }

  # parse params
  params <- parse_params(q, params = params)

  next_stac <- RSTACQuery(version = q$version,
                          base_url = q$base_url,
                          params = modify_list(q$params, params),
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

  # prepares next iteration
  items <- content

  return(items)
}

#' @rdname items_functions
#'
#' @export
items_datetime <- function(items) {
  UseMethod("items_datetime", items)
}

#' @rdname items_functions
#'
#' @export
items_datetime.STACItem <- function(items) {
  check_items(items)
  if (!"datetime" %in% names(items$properties)) {
    .error("Parameter `items` is invalid.")
  }
  return(items$properties$datetime)
}

#' @rdname items_functions
#'
#' @export
items_datetime.STACItemCollection <- function(items) {
  return(map_chr(items$features, items_datetime))
}

#' @rdname items_functions
#'
#' @export
items_datetime.default <- items_datetime.STACItem

#' @rdname items_functions
#'
#' @export
items_bbox <- function(items) {
  UseMethod("items_bbox", items)
}

#' @rdname items_functions
#'
#' @export
items_bbox.STACItem <- function(items) {
  check_items(items)
  return(items$bbox)
}

#' @rdname items_functions
#'
#' @export
items_bbox.STACItemCollection <- function(items) {
  return(items_reap(items, field = "bbox"))
}

#' @rdname items_functions
#'
#' @export
items_bbox.default <- items_bbox.STACItem

#' @rdname items_functions
#'
#' @export
items_assets <- function(items, simplify = deprecated()) {
  if (!missing(simplify)) {
    deprec_parameter(
      deprec_var = "simplify",
      deprec_version = "0.9.2",
      msg = "By default, the return will be simplified."
    )
  }
  UseMethod("items_assets", items)
}

#' @rdname items_functions
#'
#' @export
items_assets.STACItem <- function(items, simplify = deprecated()) {
  check_items(items)
  return(items_fields(items, field = "assets"))
}

#' @rdname items_functions
#'
#' @export
items_assets.STACItemCollection <- function(items, simplify = deprecated()) {
  return(unique(unlist(lapply(items$features, items_assets.STACItem))))
}

#' @rdname items_functions
#'
#' @export
items_assets.default <- items_assets.STACItem

#' @rdname items_functions
#'
#' @export
items_filter <- function(items, ..., filter_fn = NULL) {
  UseMethod("items_filter", items)
}

#' @rdname items_functions
#'
#' @export
items_filter.STACItemCollection <- function(items, ..., filter_fn = NULL) {
  # check items parameter
  check_subclass(items, "STACItemCollection")

  dots <- unquote(
    expr = as.list(substitute(list(...), env = environment())[-1]),
    env =  parent.frame()
  )

  if (length(dots) > 0) {
    if (!is.null(names(dots)))
      .error("Filter expressions cannot be named.")

    show_warning <- TRUE
    for (i in seq_along(dots)) {
      if (show_warning && check_old_expression(items, dots[[i]])) {
        # NOTE: this warning will be removed in next versions. We will no
        # longer support the old way of filter evaluation
        .warning(paste(
          "In version 0.9.2, rstac changed how filter expressions are",
          "evaluated. In future versions, the expression '%s' will be",
          "evaluated against each feature in items intead of `properties`",
          "field.\nSee ?items_filter for more details on how to change",
          "your expression."
        ), deparse(dots[[i]]))
        show_warning <- FALSE
      }
      sel <- map_lgl(items$features, eval_filter_expr, expr = dots[[i]])
    }
    items$features <- items$features[sel]
  }

  if (!is.null(filter_fn)) {
    if (check_old_fn(items, filter_fn)) {
      # NOTE: this warning will be removed in next versions. We will no
      # longer support the old way of filter evaluation
      .warning(paste(
        "In version 0.9.2, rstac changed how filter function is",
        "evaluated. In future versions, the `filter_fn` parameter will be",
        "evaluated against each feature in items instead of `properties`",
        "field.\nSee ?items_filter for more details on how to change your",
        "function."
      ))
    }
    sel <- map_lgl(items$features, eval_filter_fn, filter_fn = filter_fn)
    items$features <- items$features[sel]
  }
  return(items)
}

#' @rdname items_functions
#'
#' @export
items_compact <- function(items) {
  UseMethod("items_compact", items)
}

#' @rdname items_functions
#'
#' @export
items_compact.STACItemCollection <- function(items) {
  items_filter(items, filter_fn = has_assets)
}

#' @rdname items_functions
#'
#' @export
items_reap <- function(items, field, ...) {
  if (items_length(items) == 0) return(NULL)
  UseMethod("items_reap", items)
}

#' @rdname items_functions
#'
#' @export
items_reap.STACItem <- function(items, field, ...) {
  check_items(items)
  dots <- list(...)
  if (length(dots) > 0) {
    deprec_parameter(
      deprec_var = "...",
      deprec_version = "0.9.2",
      msg = "Please, use `field` parameter instead."
    )
    field = c(field, unlist(dots, use.names = FALSE))
  }

  values <- items[[field]]
  return(values)
}

#' @rdname items_functions
#'
#' @export
items_reap.STACItemCollection <- function(items, field, ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    deprec_parameter(
      deprec_var = "...",
      deprec_version = "0.9.2",
      msg = "Please, use `field` parameter instead."
    )
    field = c(field, unlist(dots, use.names = FALSE))
  }

  values <- lapply(items$features, items_reap.STACItem, field = field)

  if (all(vapply(values, function(x) is.atomic(x) && length(x) == 1,
                 logical(1))))
    return(unlist(values))
  return(values)
}

#' @rdname items_functions
#'
#' @export
items_reap.default <- items_reap.STACItem

#' @title Utility functions
#'
#' @description This function returns the subfields of the `feature`
#' field of a `STACItemCollection` object.
#'
#' @param items a `STACItemCollection` object representing
#'  the result of `/stac/search`, \code{/collections/{collectionId}/items}.
#'
#' @param ...   a named way to provide field names to get the subfields values
#'  from the `RSTACDocument` objects.
#'
#' @param field a `character` with the names of the field to get the
#'  subfields values from the `RSTACDocument` objects.
#'
#' @return A `character` with the subfields of the `feature` field.
#'
#' @examples
#' \dontrun{
#'  # STACItemCollection object
#'  stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1", limit = 10,
#'          datetime = "2017-08-01/2018-03-01") %>%
#'   get_request()
#'
#'  stac_item %>% items_fields(field = "properties")
#' }
#'
#' @rdname items_functions
#'
#' @export
items_fields <- function(items, field = NULL, ...) {
  UseMethod("items_fields", items)
}

#' @rdname items_functions
#'
#' @export
items_fields.STACItem <- function(items, field = NULL, ...) {
  check_items(items)
  dots <- list(...)
  if (length(dots) > 0) {
    deprec_parameter(
      deprec_var = "...",
      deprec_version = "0.9.2",
      msg = "Please, use `field` parameter instead."
    )
    field = c(field, unlist(dots, use.names = FALSE))
  }
  if (length(field) == 0) {
    fields <- names(items)
  } else {
    fields <- names(items[[field]])
  }
  return(sort(fields))
}

#' @rdname items_functions
#'
#' @export
items_fields.STACItemCollection <- function(items, field = NULL, ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    deprec_parameter(
      deprec_var = "...",
      deprec_version = "0.9.2",
      msg = "Please, use `field` parameter instead."
    )
    field = c(field, unlist(dots, use.names = FALSE))
  }
  if (items_length(items) == 0)
    return(NULL)

  fields <- lapply(items$features, items_fields.STACItem, field = field)

  return(sort(unique(unlist(unname(fields)))))
}

#' @rdname items_functions
#'
#' @export
items_fields.default <- items_fields.STACItem

#' @rdname items_functions
#'
#' @export
items_sign <- function(items, sign_fn) {
  UseMethod("items_sign", items)
}

#' @rdname items_functions
#'
#' @export
items_sign.STACItem <- function(items, sign_fn) {
  check_items(items)
  return(sign_fn(items))
}

#' @rdname items_functions
#'
#' @export
items_sign.STACItemCollection <- function(items, sign_fn) {
  return(foreach_item(items, sign_fn))
}

#' @rdname items_functions
#'
#' @export
items_sign.default <- items_sign.STACItem

#' @rdname items_functions
#'
#' @export
items_as_sf <- function(items) {
  UseMethod("items_as_sf", items)
}

#' @rdname items_functions
#'
#' @export
items_as_sf.STACItem <- function(items) {
  geojsonsf::geojson_sf(to_json(items))
}

#' @rdname items_functions
#'
#' @export
items_as_sf.STACItemCollection <- function(items) {
  geojsonsf::geojson_sf(to_json(items))
}
