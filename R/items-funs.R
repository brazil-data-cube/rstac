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
#' \item `items_reap()`: traverses all items in a `doc_items` object and
#' extracts values based on the specified field path. It is useful for
#' retrieving nested elements from STAC items.
#'
#' \item `items_fields()`: lists field names inside an item.
#'
#' \item `items_sign()`: allow access assets by preparing its url.
#'
#' \item `items_as_sf()`: `r lifecycle::badge('experimental')` convert items
#'   to `sf` object.
#'
#' \item `items_as_sfc()`: `r lifecycle::badge('experimental')` convert items
#'   to `sfc` object.
#'
#' \item `items_intersects()`: `r lifecycle::badge('experimental')` indicates
#'   which items intersects a given geometry.
#'
#' \item `items_properties()`: lists properties names inside an item.
#'
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
#' @param field           A `character` vector specifying the path to the
#' field from which to extract subfield values.
#' For example, `c("assets", "*")` will traverse all assets from each item.
#'
#' @param pick_fn         a `function` used to pick elements from items
#' addressed by `field` parameter.
#'
#' @param sign_fn         a `function` that receives an item as a parameter
#' and returns an item signed.
#'
#' @param filter_fn       a `function` that receives an item that should
#' evaluate a `logical` value.
#'
#' @param crs       a `character` representing the geometry projection.
#'
#' @param geom       a `sf` or `sfc` object.
#'
#' @param selection  an `integer` vector containing the indices of the items
#'   to select.
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
#' \item `items_filter()`: ellipsis is used to pass logical expressions to
#' be evaluated against a `doc_item` field as filter criteria. Expressions
#' must be evaluated as a logical value where `TRUE` selects the item
#' and `FALSE` discards it. Multiple expressions are combine with `AND`
#' operator. `items_filter()` uses non-standard evaluation to evaluate
#' its expressions. That means users must escape any variable or call to
#' be able to use them in the expressions. The escape is done by using
#' `double-curly-braces`, i.e., `{{variable}}`.

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
#' \item `item_assets()`: returns a `character` value with all assets names
#' of all items.
#'
#' \item `items_filter()`: a `doc_items` object.
#'
#' \item `items_reap()`: a `vector` if the supplied field is atomic,
#' otherwise or a `list`.
#'
#' \item `items_fields()`: a `character` vector.
#'
#' \item `items_sign()`: a `doc_items` object with signed assets url.
#'
#' \item `items_as_sf()`: a `sf` object.
#'
#' \item `items_as_sfc()`: a `sfc` object.
#'
#' \item `items_as_tibble()`: a `tibble` object.
#'
#' \item `items_intersects()`: a `logical` vector.
#'
#' \item `items_properties()`: returns a `character` value with all properties
#' of all items.
#'
#' \item `items_select()`: select features from an items object.
#'
#' }
#'
#' @examples
#' \dontrun{
#' x <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
#'   stac_search(collections = "CBERS4-WFI-16D-2") %>%
#'   stac_search(datetime = "2020-01-01/2021-01-01", limit = 500) %>%
#'   get_request()
#'
#' x %>% items_length()
#' x %>% items_matched()
#' x %>% items_datetime()
#' x %>% items_bbox()
#' x %>% items_fetch()
#' }
#'
#' \dontrun{
#' # Defining BDC token
#' Sys.setenv("BDC_ACCESS_KEY" = "token-123")
#'
#' # doc_item object
#' stac("https://data.inpe.br/bdc/stac/v1/") %>%
#'   stac_search(
#'     collections = "CB4-16D-2",
#'     limit = 100,
#'     datetime = "2017-08-01/2018-03-01",
#'     bbox = c(-48.206, -14.195, -45.067, -12.272)
#'   ) %>%
#'   get_request() %>%
#'   items_sign(sign_fn = sign_bdc())
#' }
#'
#' \dontrun{
#' # doc_items object
#' stac("https://data.inpe.br/bdc/stac/v1/") %>%
#'   stac_search(
#'     collections = "CBERS4-WFI-16D-2",
#'     limit = 100,
#'     datetime = "2017-08-01/2018-03-01",
#'     bbox = c(-48.206, -14.195, -45.067, -12.272)
#'   ) %>%
#'   get_request() %>%
#'   items_filter(properties$`eo:cloud_cover` < 10)
#'
#' # Example with AWS STAC
#' stac("https://earth-search.aws.element84.com/v0") %>%
#'   stac_search(
#'     collections = "sentinel-s2-l2a-cogs",
#'     bbox = c(-48.206, -14.195, -45.067, -12.272),
#'     datetime = "2018-06-01/2018-06-30",
#'     limit = 500
#'   ) %>%
#'   post_request() %>%
#'   items_filter(filter_fn = function(x) {
#'     x$properties$`eo:cloud_cover` < 10
#'   })
#' }
#'
#' \dontrun{
#' # doc_items object
#' stac_item <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
#'   stac_search(
#'     collections = "CBERS4-WFI-16D-2",
#'     limit = 100,
#'     datetime = "2017-08-01/2018-03-01",
#'     bbox = c(-48.206, -14.195, -45.067, -12.272)
#'   ) %>%
#'   get_request() %>%
#'   items_fetch(progress = FALSE)
#'
#' stac_item %>% items_reap(c("properties", "datetime"))
#'
#' # Extract all asset URLs from each item
#' stac_item %>% items_reap(c("assets", "*"), \(x) x$href)
#'
#' stac_item %>% items_as_sf()
#'
#' stac_item %>% items_as_tibble()
#'
#' stac_item %>% items_select(c(1, 4, 10, 20))
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
items_length.doc_items <- function(items) {
  check_items(items)
  return(length(items$features))
}

#' @rdname items_functions
#'
#' @export
items_matched <- function(items, matched_field = NULL) {
  UseMethod("items_matched", items)
}

#' @rdname items_functions
#'
#' @export
items_matched.doc_items <- function(items, matched_field = NULL) {
  check_items(items)
  matched <- NULL
  # try by the matched_field provided by user. This allow users specify a
  # non-standard field for matched items.
  if (is.character(matched_field) && matched_field %in% names(items)) {
    matched <- as.numeric(items[[matched_field]])
  }
  if (is.null(matched) && "search:metadata" %in% names(items)) {
    matched <- items$`search:metadata`$matched
  }
  if (is.null(matched) && "context" %in% names(items)) {
    matched <- items$`context`$matched
  }
  # try the last resort: OGC features core spec
  if (is.null(matched)) {
    matched <- items$numberMatched
  }
  matched
}

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
    if (!is.null(matched) && (items_length(items) == matched)) {
      break
    }
    # protect against infinite loop
    if (!is.null(matched) && (items_length(items) > matched)) {
      .error(paste(
        "Length of returned items (%s) is different",
        "from matched items (%s)."
      ), items_length(items), matched)
    }
    next_items <- tryCatch(
      {
        items_next(next_items, ...)
      },
      next_error = function(e) NULL
    )
    if (is.null(next_items)) {
      break
    }
    items$features <- c(items$features, next_items$features)
    # update progress bar
    if (progress) {
      utils::setTxtProgressBar(pb, length(next_items))
    }
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
  if (length(next_link) == 0) {
    .error("Cannot get next link URL.", class = "next_error")
  }
  next_link <- next_link[[1]]
  # check for body implementation in next link
  verb <- "GET"
  if ("method" %in% names(next_link) && next_link$method %in% c("GET", "POST")) {
    verb <- next_link$method
  }
  q <- NULL
  if (verb == "POST") {
    # POST
    q <- attr(items, "query")
    if (!is.null(q)) {
      # merge content body to next body field
      if ("merge" %in% names(next_link) && next_link$merge) {
        next_link$body <- modify_list(q$params, next_link$body)
      }
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
    .error("Item has no datetime field.")
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
items_assets <- function(items) {
  UseMethod("items_assets", items)
}

#' @rdname items_functions
#'
#' @export
items_assets.doc_item <- function(items) {
  check_item(items)
  if (!"assets" %in% names(items)) {
    .error("Item has no assets.")
  }
  names(items$assets)
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
items_assets.default <- function(items) {
  if (!"assets" %in% names(items)) {
    .error("Item has no assets.")
  }
  names(items$assets)
}

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
  init_length <- items_length(items)
  exprs <- as.list(substitute(list(...), env = environment()))[-1]
  if (length(exprs) > 0) {
    if (!is.null(names(exprs))) {
      .error("Filter expressions cannot be named.")
    }
    for (expr in exprs) {
      expr <- unquote(expr = expr, env = parent.frame())
      sel <- map_lgl(items$features, eval_filter_expr, expr = expr)
      items$features <- items$features[sel]
    }
  }
  if (!is.null(filter_fn)) {
    sel <- map_lgl(items$features, eval_filter_fn, filter_fn = filter_fn)
    items$features <- items$features[sel]
  }
  if (items_length(items) == 0 && init_length > 0) {
    .warning(paste(
      "Filter criteria did not match any item.\n",
      "Please, see `?items_filter` for more details on",
      "how expressions are evaluated by `items_filter()`."
    ))
  }
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
  if (items_length(items) == 0) {
    return(NULL)
  }
  values <- lapply(items$features, items_reap.doc_item,
    field = field,
    pick_fn = pick_fn
  )
  is_atomic <- all(vapply(values, function(x) {
    is.atomic(x) && length(x) == 1
  }, logical(1)))
  if (is_atomic) {
    return(unlist(values))
  }
  values
}

#' @rdname items_functions
#'
#' @export
items_reap.default <- function(items, field, pick_fn = identity) {
  apply_deeply(items, i = field, fn = pick_fn)
}

#' @rdname items_functions
#'
#' @export
items_fields <- function(items, field = NULL) {
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
      items,
      i = field, fn = names
    ), use.names = FALSE))
  }
  sort(fields)
}

#' @rdname items_functions
#'
#' @export
items_fields.doc_items <- function(items, field = NULL) {
  check_items(items)
  if (items_length(items) == 0) {
    return(NULL)
  }
  fields <- apply_deeply(items, i = c("features", "*", field), fn = names)
  sort(unique(unlist(unname(fields))))
}

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
items_sign.default <- function(items, sign_fn) {
  sign_fn(items)
}

#' @rdname items_functions
#'
#' @export
items_as_sf <- function(items, ..., crs = 4326) {
  UseMethod("items_as_sf", items)
}

#' @rdname items_functions
#'
#' @export
items_as_sf.doc_item <- function(items, ..., crs = 4326) {
  check_item(items)
  data <- sf::st_sf(
    items_as_tibble(items),
    geometry = items_as_sfc(items, crs = crs)
  )
  class(data) <- c("sf", "tbl_df", "tbl", "data.frame")
  data
}

#' @rdname items_functions
#'
#' @export
items_as_sf.doc_items <- function(items, ..., crs = 4326) {
  check_items(items)
  data <- sf::st_sf(
    items_as_tibble(items),
    geometry = items_as_sfc(items, crs = crs)
  )
  # class(data) <- c("sf", "tbl_df", "tbl", "data.frame")
  data
}

#' @rdname items_functions
#'
#' @export
items_as_sfc <- function(items, crs = 4326) {
  UseMethod("items_as_sfc", items)
}

#' @rdname items_functions
#'
#' @export
items_as_sfc.doc_item <- function(items, crs = 4326) {
  check_item(items)
  sf::st_sfc(get_geom(items$geometry), crs = crs)
}

#' @rdname items_functions
#'
#' @export
items_as_sfc.doc_items <- function(items, crs = 4326) {
  check_items(items)
  sf::st_sfc(lapply(items$features, get_geom), crs = crs)
}

#' @rdname items_functions
#'
#' @export
items_as_tibble <- function(items) {
  UseMethod("items_as_tibble", items)
}

#' @rdname items_functions
#'
#' @export
items_as_tibble.doc_item <- function(items) {
  check_item(items)
  non_atomic <- non_atomic_properties(items)
  items$properties[non_atomic] <- lapply(items$properties[non_atomic], list)
  data <- list(items$properties)
  data <- do.call(mapply, args = c(list(FUN = c, SIMPLIFY = FALSE), data))
  structure(
    data,
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = if (length(data)) c(NA, -length(data[[1]])) else integer(0)
  )
}

#' @rdname items_functions
#'
#' @export
items_as_tibble.doc_items <- function(items) {
  check_items(items)
  non_atomic <- non_atomic_properties(items)
  properties <- items_fields(items, "properties")
  data <- lapply(items$features, function(item) {
    # fill unavailable properties
    unavailable_properties <- setdiff(properties, names(item$properties))
    item$properties[unavailable_properties] <- NA
    # update non-atomic properties
    item$properties[non_atomic] <- lapply(item$properties[non_atomic], list)
    # return properties from items in the same order
    # to avoid errors in the `mapply`
    item$properties[properties]
  })
  data <- do.call(mapply, args = c(list(FUN = c, SIMPLIFY = FALSE), data))
  structure(
    data,
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = if (length(data)) c(NA, -length(data[[1]])) else integer(0)
  )
}

#' @rdname items_functions
#'
#' @export
items_intersects <- function(items, geom, ..., crs = 4326) {
  UseMethod("items_intersects", items)
}

#' @rdname items_functions
#'
#' @export
items_intersects.doc_item <- function(items, geom, ..., crs = 4326) {
  check_item(items)
  items_geom <- items_as_sfc(items, crs = crs)
  geom <- sf::st_transform(geom, crs = crs)
  apply(sf::st_intersects(items_geom, geom), 1, any) > 0
}

#' @rdname items_functions
#'
#' @export
items_intersects.doc_items <- function(items, geom, ..., crs = 4326) {
  check_items(items)
  items_geom <- items_as_sfc(items, crs = crs)
  geom <- sf::st_transform(geom, crs = crs)
  apply(sf::st_intersects(items_geom, geom), 1, any) > 0
}

#' @rdname items_functions
#'
#' @export
items_properties <- function(items) {
  UseMethod("items_properties", items)
}

#' @rdname items_functions
#'
#' @export
items_properties.doc_item <- function(items) {
  check_item(items)
  sort(names(items$properties))
}

#' @rdname items_functions
#'
#' @export
items_properties.doc_items <- function(items) {
  check_items(items)
  sort(unique(unlist(lapply(items$features, function(item) {
    names(item$properties)
  }))))
}

#' @rdname items_functions
#'
#' @export
items_select <- function(items, selection) {
  UseMethod("items_select", items)
}

#' @rdname items_functions
#'
#' @export
items_select.doc_items <- function(items, selection) {
  check_items(items)
  items$features <- items$features[selection]
  # clear numberMatched information
  if ("search:metadata" %in% names(items)) {
    items$`search:metadata`$matched <- NULL
  }
  if ("context" %in% names(items)) {
    items$`context`$matched <- NULL
  }
  if ("numberMatched" %in% names(items)) {
    items$numberMatched <- NULL
  }
  items
}
