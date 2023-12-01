#' @title Items functions
#'
#' @description
#' These functions provide support to work with
#' `doc_items` and `doc_item` objects.
#'
#' \itemize{
#' \item `items_length()`: shows how many items there are in
#' the `doc_items` object.
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
#' \item `items_datetime()`: retrieves the `datetime`
#' field in `properties` from `doc_items` and
#' `doc_item` objects.
#'
#' \item `items_bbox()`: retrieves the `bbox`
#' field of a `doc_items` or a `doc_item` object.
#'
#' \item `item_assets()`: returns the assets name from
#' `doc_items` and `doc_item` objects.
#'
#' \item `items_filter()`: selects only items that match some criteria
#'  (see details section).
#'
#' \item `items_reap()`: extract key values by traversing all items
#' in a `doc_items` object.
#'
#' \item `items_fields()`: lists field names inside an item.
#'
#' \item `items_sign()`: allow access assets by preparing its url.
#'
#' \item `items_as_sf()`: `r lifecycle::badge('experimental')` convert items
#'   to `sf` object.
#' }
#'
#' @param items           a `doc_items` object.
#'
#' @param matched_field   a `character` vector with the path
#' where the number of items returned in the named list is located starting
#' from the initial node of the list. For example, if the information is in a
#' position `items$meta$found` of the object, it must be passed as the
#' following parameter `c("meta", "found")`.
#'
#' @param progress        a `logical` indicating if a progress bar must be
#' shown or not. Defaults to `TRUE`.
#'
#' @param field           a `character` with the names of the field to
#' get the subfields values.
#'
#' @param pick_fn         a `function` used to pick elements from items
#' addressed by `field` parameter.
#'
#' @param index           an `atomic` vector with values as the group index.
#'
#' @param sign_fn         a `function` that receives an item as a parameter
#' and returns an item signed.
#'
#' @param filter_fn       a `function` that receives an item that should
#' evaluate a `logical` value.
#'
#' @param ...             additional arguments. See details.
#'
#' @details
#' Ellipsis argument (`...`) appears in different items functions and
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
#' expressions to be evaluated against a `doc_item` field as filter criteria.
#'
#' **WARNING:** the evaluation of filter expressions changed in `rstac` 0.9.2.
#' Older versions of `rstac` used `properties` field to evaluate filter
#' expressions. Below, there is an example of how to write expressions in new
#' `rstac` version:
#' ```R
#' # expression in older version
#' items_filter(stac_obj, `eo:cloud_cover` < 10)
#' # now expressions must refer to properties explicitly
#' items_filter(stac_obj, properties$`eo:cloud_cover` < 10)
#' items_filter(stac_obj, properties[["eo:cloud_cover"]] < 10)
#' ```
#'
#' \item `items_sign()`: in the near future, ellipsis will be used to append
#' key-value pairs to the url query string of an asset.
#' }
#'
#' `items_sign()` has `sign_fn` parameter that must be a function that
#' receives as argument an item and returns a signed item. `rstac` provides
#' `sign_bdc()` and `sign_planetary_computer()` functions to access Brazil
#' Data Cube products and Microsoft Planetary Computer catalogs, respectively.
#'
#' @return
#'
#' \itemize{
#' \item `items_length()`: an `integer` value.
#'
#' \item `items_matched()`: returns an `integer` value if the STAC web server
#' does support this extension. Otherwise returns `NULL`.
#'
#' \item `items_fetch()`: a `doc_items` with all matched items.
#'
#' \item `items_next()`: fetches a new page from STAC service.
#'
#' \item `items_datetime()`: a `list` of all items' datetime.
#'
#' \item `items_bbox()`: returns a `list` with all items' bounding boxes.
#'
#' \item `item_assets()`: Returns a `character` value with all assets names
#' of the all items.
#'
#' \item `items_filter()`: a `doc_items` object.
#'
#' \item `items_reap()`: a `vector` if the supplied field is atomic,
#' otherwise or a `list`.
#'
#' \item `items_fields()`: a `character` vector.
#'
#' \item `items_group()`: a `list` of `doc_items` objects.
#'
#' \item `items_sign()`: a `doc_items` object with signed assets url.
#'
#' \item `items_as_sf()`: a `sf` object.
#'
#' }
#'
#' @examples
#' \dontrun{
#'  x <- stac("https://brazildatacube.dpi.inpe.br/stac") %>%
#'      stac_search(collections = "CB4-16D-2") %>%
#'      stac_search(datetime = "2020-01-01/2021-01-01", limit = 500) %>%
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
#' # doc_item object
#' stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'     stac_search(collections = "CB4-16D-2", limit = 100,
#'         datetime = "2017-08-01/2018-03-01",
#'         bbox = c(-48.206, -14.195, -45.067, -12.272)) %>%
#'     get_request() %>% items_sign(sign_fn = sign_bdc())
#'
#' }
#'
#' \dontrun{
#' # doc_items object
#' stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'     stac_search(collections = "CB4-16D-2", limit = 100,
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
#' # doc_items object
#' stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'  stac_search(collections = "CB4-16D-2", limit = 100,
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
items_length.doc_item <- function(items) {
  check_item(items)
  return(1)
}

#' @rdname items_functions
#'
#' @export
items_length.doc_items <- function(items) {
  check_items(items)
  return(length(items$features))
}

#' @rdname items_functions
#'
#' @export
items_length.default <- items_length.doc_item

#' @rdname items_functions
#'
#' @export
items_matched  <- function(items, matched_field = NULL) {
  UseMethod("items_matched", items)
}

#' @rdname items_functions
#'
#' @export
items_matched.doc_item  <- function(items, matched_field = NULL) {
  check_item(items)
  return(1)
}

#' @rdname items_functions
#'
#' @export
items_matched.doc_items <- function(items, matched_field = NULL) {
  check_items(items)
  matched <- NULL
  # try by the matched_field provided by user. This allow users specify a
  # non-standard field for matched items.
  if (is.character(matched_field) && matched_field %in% names(items))
    matched <- as.numeric(items[[matched_field]])
  if (is.null(matched) && "search:metadata" %in% names(items))
    matched <- items$`search:metadata`$matched
  if (is.null(matched) && "context" %in% names(items))
    matched <- items$`context`$matched
  # try the last resort: OGC features core spec
  if (is.null(matched))
    matched <- items$numberMatched
  matched
}

#' @rdname items_functions
#'
#' @export
items_matched.default <- items_matched.doc_item

#' @rdname items_functions
#'
#' @export
items_fetch <- function(items, ...) {
  UseMethod("items_fetch", items)
}

#' @rdname items_functions
#'
#' @export
items_fetch.doc_items <- function(items, ...,
                                  progress = TRUE,
                                  matched_field = NULL) {
  check_items(items)
  matched <- items_matched(items, matched_field)
  # verify if progress bar can be shown
  progress <- progress & (!is.null(matched) && (items_length(items) < matched))
  if (progress) {
    pb <- utils::txtProgressBar(
      min = items_length(items),
      max = matched,
      style = 3
    )
    # close progress bar when exit
    on.exit({
      if (progress) {
        utils::setTxtProgressBar(pb, matched)
        close(pb)
      }
    })
  }
  # Initialize the items
  next_items <- items
  while (TRUE) {
    # check if features is complete
    if (!is.null(matched) && (items_length(items) == matched))
      break
    # protect against infinite loop
    if (!is.null(matched) && (items_length(items) > matched))
      .error(paste("Length of returned items (%s) is different",
                   "from matched items (%s)."), items_length(items), matched)
    next_items <- tryCatch({
      items_next(next_items, ...)
    }, next_error = function(e) NULL)
    if (is.null(next_items))
      break
    items$features <- c(items$features, next_items$features)
    # update progress bar
    if (progress)
      utils::setTxtProgressBar(pb, length(next_items))
  }
  items
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
items_next.doc_items <- function(items, ...) {
  check_items(items)
  # get url of the next page
  rel <- NULL
  next_link <- links(items, rel == "next")
  if (length(next_link) == 0)
    .error("Cannot get next link URL.", class = "next_error")
  next_link <- next_link[[1]]
  # check for body implementation in next link
  verb <- "GET"
  if ("method" %in% names(next_link) && next_link$method %in% c("GET", "POST"))
    verb <- next_link$method
  q <- NULL
  if (verb == "POST") {
    # POST
    q <- attr(items, "query")
    if (!is.null(q)) {
      # merge content body to next body field
      if ("merge" %in% names(next_link) && next_link$merge)
        next_link$body <- modify_list(q$params, next_link$body)
      next_link$body <- parse_params(q, next_link$body)
    }
    res <- make_post_request(
      url = next_link$href,
      body = next_link$body,
      headers = next_link$headers,
      ...,
      error_msg = "Error while requesting next page"
    )
  } else if (verb == "GET") {
    # GET
    res <- make_get_request(
      url = next_link$href,
      headers = next_link$headers,
      ...,
      error_msg = "Error while requesting next page"
    )
  }
  content <- content_response_json(res)
  # return items
  doc_items(content, query = q)
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
items_datetime.doc_item <- function(items) {
  check_item(items)
  if (!"datetime" %in% names(items$properties)) {
    .error("Parameter `items` is invalid.")
  }
  items$properties$datetime
}

#' @rdname items_functions
#'
#' @export
items_datetime.doc_items <- function(items) {
  check_items(items)
  map_chr(items$features, items_datetime)
}

#' @rdname items_functions
#'
#' @export
items_datetime.default <- items_datetime.doc_item

#' @rdname items_functions
#'
#' @export
items_bbox <- function(items) {
  UseMethod("items_bbox", items)
}

#' @rdname items_functions
#'
#' @export
items_bbox.doc_item <- function(items) {
  check_item(items)
  return(items$bbox)
}

#' @rdname items_functions
#'
#' @export
items_bbox.doc_items <- function(items) {
  check_items(items)
  return(items_reap(items, field = "bbox"))
}

#' @rdname items_functions
#'
#' @export
items_bbox.default <- items_bbox.doc_item

#' @rdname items_functions
#'
#' @export
items_assets <- function(items) {
  UseMethod("items_assets", items)
}

#' @rdname items_functions
#'
#' @export
items_assets.doc_item <- function(items) {
  check_item(items)
  items_fields(items, field = "assets")
}

#' @rdname items_functions
#'
#' @export
items_assets.doc_items <- function(items) {
  check_items(items)
  sort(unique(unlist(lapply(items$features, items_assets.doc_item))))
}

#' @rdname items_functions
#'
#' @export
items_assets.default <- items_assets.doc_item

#' @rdname items_functions
#'
#' @export
items_filter <- function(items, ..., filter_fn = NULL) {
  UseMethod("items_filter", items)
}

#' @rdname items_functions
#'
#' @export
items_filter.doc_items <- function(items, ..., filter_fn = NULL) {
  check_items(items)
  init_length <- items_length(items)
  exprs <- unquote(
    expr = as.list(substitute(list(...), env = environment())[-1]),
    env =  parent.frame()
  )
  if (length(exprs) > 0) {
    if (!is.null(names(exprs)))
      .error("Filter expressions cannot be named.")
    for (i in seq_along(exprs)) {
      sel <- map_lgl(items$features, eval_filter_expr, expr = exprs[[i]])
      items$features <- items$features[sel]
    }
  }
  if (!is.null(filter_fn)) {
    sel <- map_lgl(items$features, eval_filter_fn, filter_fn = filter_fn)
    items$features <- items$features[sel]
  }
  if (items_length(items) == 0 && init_length > 0)
    .warning(paste("Filter criteria did not match any item.\n",
                   "Please, see `?items_filter` for more details on",
                   "how expressions are evaluated by `items_filter()`."))
  items
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
items_compact.doc_items <- function(items) {
  check_items(items)
  items_filter(items, filter_fn = has_assets)
}

#' @rdname items_functions
#'
#' @export
items_reap <- function(items, field, pick_fn = identity) {
  UseMethod("items_reap", items)
}

#' @rdname items_functions
#'
#' @export
items_reap.doc_item <- function(items, field, pick_fn = identity) {
  check_item(items)
  apply_deeply(items, i = field, fn = pick_fn)
}

#' @rdname items_functions
#'
#' @export
items_reap.doc_items <- function(items, field, pick_fn = identity) {
  check_items(items)
  if (items_length(items) == 0) return(NULL)
  values <- lapply(items$features, items_reap.doc_item, field = field,
                   pick_fn = pick_fn)
  is_atomic <- all(vapply(values, function(x) {
    is.atomic(x) && length(x) == 1
  }, logical(1)))
  if (is_atomic)
    return(unlist(values))
  values
}

#' @rdname items_functions
#'
#' @export
items_reap.default <- items_reap.doc_item

#' @rdname items_functions
#'
#' @export
items_fields <- function(items, field = NULL, ...) {
  UseMethod("items_fields", items)
}

#' @rdname items_functions
#'
#' @export
items_fields.doc_item <- function(items, field = NULL) {
  check_item(items)
  if (length(field) == 0) {
    fields <- names(items)
  } else {
    fields <- unique(unlist(apply_deeply(
      items, i = field, fn = names
    ), use.names = FALSE))
  }
  sort(fields)
}

#' @rdname items_functions
#'
#' @export
items_fields.doc_items <- function(items, field = NULL) {
  check_items(items)
  if (items_length(items) == 0)
    return(NULL)
  fields <- lapply(items$features, items_fields.doc_item, field = field)
  sort(unique(unlist(unname(fields))))
}

#' @rdname items_functions
#'
#' @export
items_fields.default <- items_fields.doc_item

#' @rdname items_functions
#'
#' @export
items_sign <- function(items, sign_fn) {
  UseMethod("items_sign", items)
}

#' @rdname items_functions
#'
#' @export
items_sign.doc_item <- function(items, sign_fn) {
  check_item(items)
  sign_fn(items)
}

#' @rdname items_functions
#'
#' @export
items_sign.doc_items <- function(items, sign_fn) {
  check_items(items)
  foreach_item(items, sign_fn)
}

#' @rdname items_functions
#'
#' @export
items_sign.default <- items_sign.doc_item

#' @rdname items_functions
#'
#' @export
items_as_sf <- function(items) {
  UseMethod("items_as_sf", items)
}

#' @rdname items_functions
#'
#' @export
items_as_sf.doc_item <- function(items) {
  check_item(items)
  geojsonsf::geojson_sf(to_json(items))
}

#' @rdname items_functions
#'
#' @export
items_as_sf.doc_items <- function(items) {
  items_as_sf.doc_item(items)
}
