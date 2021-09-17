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

#' @export
stac_version.STACCollectionList <- function(x, ...) {

  q <- doc_query(x)
  if (!is.null(q))
    return(stac_version(q))
  if (length(x$collections) > 0)
    return(x$collections[[1]]$stac_version)
}

####Items function####

#' @title STACItemCollection functions
#'
#' @description
#' The `items_length()` function shows how many items there are in
#' the `STACItemCollection` object.
#'
#' The `items_matched()` function shows how many items matched the
#' search criteria. It supports `search:metadata` (v0.8.0),
#' `context` (v0.9.0), and `numberMatched` (OGC WFS3 core spec).
#'
#' The `items_fetch()` function request all STAC Items through
#' pagination.
#'
#' The `items_datetime()` function retrieves a the `datetime`
#' field in `properties` from `STACItemCollection` and
#' `STACItem` objects.
#'
#' The `items_bbox()` function retrieves a the `bbox`
#' field of a `STACItemCollection` or an `STACItem` object.
#'
#' The `item_assets()` function returns the assets name from
#' `STACItemCollection` and `STACItem` objects.
#'
#' @param items           a `STACItemCollection` object.
#' @param matched_field   a `character` vector with the path
#' where the number of items returned in the named list is located starting from
#' the initial node of the list. For example, if the information is at position
#' `items$meta$found` of the object, it must be passed as the following
#' parameter `c("meta", "found")`.
#' @param simplify        a `logical` should return only the assets name of the
#'  first item? if not a `list` with all assets name will be returned. Default
#'  is `FALSE`.
#' @param ...             additional arguments. See details.
#' @param progress        a `logical` indicating if a progress bar must be
#' shown or not. Defaults to `TRUE`.
#'
#' @details
#' For `items_fetch` ellipsis is used to pass additional `httr` options to
#'  [GET][httr::GET] or [POST][httr::POST] methods, such as
#'  [add_headers][httr::add_headers] or [set_cookies][httr::set_cookies].
#'
#' For `items_filter` function is used to pass logical expressions.
#'
#' @return
#' The `items_assets()` if simplify is `TRUE`, returns a `character` value with
#'  all assets names of the first item. Otherwise, returns a `list` with assets
#'  name for each item.
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
#'   stac_search(limit = 500) %>%
#'   get_request()
#'
#' x %>% items_length()
#' x %>% items_matched()
#' x %>% items_datetime()
#' x %>% items_bbox()
#' x %>% items_fetch()
#' }
#'
#' @name items_functions
#'
#' @export
items_length <- function(items) {

  UseMethod("items_length", items)
}

#' @rdname items_functions
#'
#' @export
items_length.STACItem <- function(items) {

  return(1)
}

#' @rdname items_functions
#'
#' @export
items_length.STACItemCollection <- function(items) {

  return(length(items$features))
}

#' @rdname items_functions
#'
#' @export
items_matched  <- function(items, ...) {

  UseMethod("items_matched", items)
}

#' @rdname items_functions
#'
#' @export
items_matched.STACItem  <- function(items, ...) {

  return(1)
}

#' @rdname items_functions
#'
#' @export
items_matched.STACItemCollection <- function(items, ..., matched_field = NULL) {

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

#' @rdname items_functions
#'
#' @export
items_fetch <- function(items, ...) {

  UseMethod("items_fetch", items)
}

#' @rdname items_functions
#'
#' @export
items_fetch.STACItem <- function(items, ...) {

  return(items)
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

  UseMethod("items_datetime", items)
}

#' @rdname items_functions
#'
#' @export
items_datetime.STACItem <- function(items) {

  return(items$properties[["datetime"]])
}

#' @rdname items_functions
#'
#' @export
items_datetime.STACItemCollection <- function(items) {

  lapply(items$features, `[[`, c("properties", "datetime"))
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
items_bbox.STACItem <- function(items) {

  return(items[["bbox"]])
}

#' @rdname items_functions
#'
#' @export
items_bbox.STACItemCollection <- function(items) {

  lapply(items$features, `[[`, c("bbox"))
}

#' @rdname items_functions
#'
#' @export
items_assets <- function(items, ...) {

  UseMethod("items_assets", items)
}

#' @rdname items_functions
#'
#' @export
items_assets.STACItem <- function(items, ...) {

  return(items_fields(items, "assets"))
}

#' @rdname items_functions
#'
#' @export
items_assets.STACItemCollection <- function(items, ..., simplify = FALSE) {

  if (simplify)
    return(items_fields(items, "assets"))
  lapply(lapply(items$features, `[[`, c("assets")), names)
}


#' @title Utility functions
#'
#' @description This function filters for the attributes contained in the STAC
#'  properties.
#'
#' @param items a `STACItemCollection` object representing
#'  the result of `/stac/search`, \code{/collections/{collectionId}/items}.
#'
#' @param ...   expressions used to filter items of a `STACItemCollection`
#'  object.
#' @param fn    a `function` that will be used to filter the attributes
#'  listed in the properties.
#'
#' @return a `STACItemCollection` object with the filtered properties.
#'
#' @examples
#' \donttest{
#' # STACItemCollection object
#' stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'  stac_search(collections = "CB4_64_16D_STK-1", limit = 100,
#'         datetime = "2017-08-01/2018-03-01",
#'         bbox = c(-48.206,-14.195,-45.067,-12.272)) %>%
#'  get_request() %>% items_filter(`eo:cloud_cover` < 10)
#'
#' # Example with AWS STAC
#' items <- stac("https://earth-search.aws.element84.com/v0") %>%
#'   stac_search(collections = "sentinel-s2-l2a-cogs",
#'               bbox = c(-48.206,-14.195,-45.067,-12.272),
#'               datetime = "2018-06-01/2018-06-30",
#'               limit = 500) %>%
#'   post_request()
#'
#'   items %>%
#'    items_filter(fn = function(x) {x[["eo:cloud_cover"]] < 10})
#' }
#'
#' @export
items_filter <- function(items, ..., fn = NULL) {

  dots <- substitute(list(...))[-1]

  if (length(dots) > 0) {

    if (!is.null(names(dots)))
      .error("Invalid filter arguments.")

    for (i in seq_along(dots)) {

      sel <- vapply(items$features, function(f) {
        eval(dots[[i]], envir = f$properties)
      }, logical(1))

      items$features <- items$features[sel]
    }
  }

  if (!is.null(fn)) {

    sel <- vapply(items$features, function(f) {
      fn(f$properties)
    }, logical(1))

    items$features <- items$features[sel]
  }

  items
}

#' @title Utility functions
#'
#' @description This function returns the values of a field of the
#'  `STACItemCollections` object. If the values of the specified field are
#'  not atomic the return will be in list form, if they are, it will be returned
#'  in vector form.
#'
#' @param items  a `STACItemCollection` object representing
#'  the result of `/stac/search`, \code{/collections/{collectionId}/items}.
#'
#' @param ...   a named way to provide fields names to get the
#'  subfields values from the `RSTACDocument` objects.
#'
#' @param field a `character` with the names of the field to
#'  get the subfields values from the `RSTACDocument` objects.
#'
#' @return a `vector` if the supplied field is atomic, or a list if not.
#'
#' @examples
#' \donttest{
#' # STACItemCollection object
#' stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'  stac_search(collections = "CB4_64_16D_STK-1", limit = 100,
#'         datetime = "2017-08-01/2018-03-01",
#'         bbox = c(-48.206,-14.195,-45.067,-12.272)) %>%
#'  get_request() %>% items_fetch(progress = FALSE)
#'
#' stac_item %>% items_reap(field = c("properties", "datetime"))
#' }
#'
#' @export
items_reap <- function(items, ..., field = NULL) {

  # checks if the object is STACItemCollections
  if (items_length(items) == 0) return(NULL)

  dots <- substitute(list(...))[-1]
  if (!is.character(dots)) dots <- as.character(dots)

  if (length(dots) > 0 && length(field) > 0)
    .error("Only one of the parameters '...' or 'field' must be supplied.")

  if (length(field) == 0 && length(dots) == 0)
    return(items$features)

  values <- lapply(items$features, `[[`, c(dots, field))

  if (all(vapply(values, is.null, logical(1))))
    .error("The provided field does not exist.")

  if (all(vapply(values, is.atomic, logical(1))))
    return(unlist(values))
  values
}

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
#' \donttest{
#' # STACItemCollection object
#' stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'  stac_search(collections = "CB4_64_16D_STK-1", limit = 10,
#'         datetime = "2017-08-01/2018-03-01") %>%
#'  get_request()
#'
#' stac_item %>% items_fields(field = c("properties"))
#' }
#'
#' @name items_utils
#'
#' @export
items_fields <- function(items, ..., field = NULL) {
  UseMethod("items_fields", items)
}

#' @rdname items_utils
#'
#' @export
items_fields.STACItemCollection <- function(items, ..., field = NULL) {

  dots <- substitute(list(...))[-1]
  if (!is.character(dots)) dots <- as.character(dots)

  if (length(field) > 0 && length(dots) > 0)
    .error("Only one of the parameters '...' or 'field' must be supplied.")

  if (length(field) == 0 && length(dots) == 0)
    return(names(items$features[[1]]))
  names(items$features[[1]][[c(dots, field)]])
}

#' @rdname items_utils
#'
#' @export
items_fields.STACItem <- function(items, ..., field = NULL) {

  dots <- substitute(list(...))[-1]
  if (!is.character(dots)) dots <- as.character(dots)

  if (length(field) > 0 && length(dots) > 0)
    .error("Only one of the parameters '...' or 'field' must be supplied.")

  if (length(field) == 0 && length(dots) == 0)
    return(names(items))
  names(items[[c(dots, field)]])
}

#' @title Utility functions
#'
#' @description This function groups the items contained within the
#'  `STACItemCollection` object according to some specified fields. Each
#'  index in the returned list contains items belonging to the same group.
#'
#' @param items a `STACItemCollection` object representing the result of
#' `/stac/search`, \code{/collections/{collectionId}/items}.
#'
#' @param ...   a named way to provide field names to get the subfields values
#'  from the `RSTACDocument` objects.
#'
#' @param field a `character` with the names of the field to get the
#'  subfields values from the `RSTACDocument` objects.
#'
#' @param index a `character` with the indexes to be grouped. It can be
#'  used with the function [items_reap].
#'
#' @return A `list` in which each index corresponds to a group with its
#'  corresponding `STACItemCollection` objects.
#'
#' @examples
#' \donttest{
#' # STACItemCollection object
#' stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'  stac_search(collections = "CB4_64_16D_STK-1", limit = 100,
#'         datetime = "2017-08-01/2018-03-01",
#'         bbox = c(-48.206,-14.195,-45.067,-12.272)) %>%
#'  get_request() %>% items_fetch(progress = FALSE)
#'
#'  stac_item %>% items_group(., field = c("properties", "bdc:tiles"))
#' }
#'
#' @export
items_group <- function(items, ..., field = NULL, index = NULL) {

  # checks if the object is STACItemCollections
  if (items_length(items) == 0) return(list(items))

  dots <- substitute(list(...))[-1]
  if (!is.character(dots)) dots <- as.character(dots)

  if (length(index) == 0 && length(field) == 0 &&  length(dots) == 0)
    .error(paste("Either parameters 'index', 'field' or '...' parameters must",
                 "be supplied."))

  if (length(index) > 0 && (length(field) > 0 || length(dots) > 0))
    .error(paste("Only one of the parameters '...','index' or 'field' should",
                 "be supplied."))

  if (is.null(index)) {
    index <- items_reap(items, ..., field = field)

    if (!is.atomic(index))
      .error("The field must be atomic vector.")
  } else {

    if (items_matched(items) > items_length(items))
      .warning(paste("The number of matched items is greater than the number",
                     "of items length on your object. Considere to use",
                     "the 'items_fetch()' function before this operation."))
  }

  if (items_length(items) != length(index))
    .error(paste("The length of the field provided for grouping must contain",
                 "the same size as the length of the items."))

  features <- unname(tapply(X = items$features,
                            INDEX = index,
                            FUN = c, simplify = FALSE))

  lapply(features, function(x){
    items$features <- x

    items
  })
}

#' @title Utility functions
#'
#' @description A utility function to create signatures in hrefs of each
#'  asset in the STAC items. To create the signatures, the provided function
#'  must return a function, being a factory function, so the manufactured
#'  function will sign the hrefs returning one feature per interaction.
#'
#' @param items   a `STACItemCollection` object representing
#'  the result of `/stac/search`, \code{/collections/{collectionId}/items}.
#'
#' @param sign_fn a `function` to assign each assets in STAC items.
#'
#' @return A `STACItemCollection` object with the signed assets according to the
#'  supplied parameter function.
#' @examples
#' \dontrun{
#'
#' # Defining BDC token
#' Sys.setenv("BDC_ACCESS_KEY" = <your_bdc_access_key>)
#'
#' # STACItem object
#' stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'  stac_search(collections = "CB4_64_16D_STK-1", limit = 100,
#'         datetime = "2017-08-01/2018-03-01",
#'         bbox = c(-48.206,-14.195,-45.067,-12.272)) %>%
#'  get_request() %>% items_sign(sign_fn = sign_bdc)
#'
#' }
#'
#' @export
items_sign <- function(items, sign_fn = NULL) {

  if (is.null(sign_fn)) {
    return(items)
  }

  if (!is.null(items_matched(items))) {
    if (items_length(items) != items_matched(items))
      .warning(paste("The number of items in this object does not match the",
                     "total number of items in the item. If you want to get",
                     "all items, use `items_fetch()`"))
  }

  # assign each item obj
  items[["features"]] <- lapply(items[["features"]], function(item){

    item <- sign_fn(item)

    item
  })

  items
}

####Assets function####

#' @title Assets functions
#'
#' @description This function returns the `date`, `band` and
#'  `URL` fields for each assets of an `STACItemCollection` object.
#'  For the URL you can add the GDAL library drivers for the following schemes:
#'  HTTP/HTTPS files, S3 (AWS S3) and GS (Google Cloud Storage).
#'
#' @param items        a `STACItemCollection` object representing
#'  the result of `/stac/search`, \code{/collections/{collectionId}/items}.
#'
#' @param assets_names `r lifecycle::badge('deprecated')`
#'  use `asset_names` parameter instead.
#'
#' @param asset_names  a `character` with the assets names to be
#'  filtered. If `NULL` (default) all assets will be returned..
#'
#' @param sort         a `logical` if true the dates will be sorted
#'  in increasing order. By default, the dates are sorted.
#'
#' @param gdal_vsi_resolution a `logical`  if true, gdal drivers are
#'  included in the URL of each asset. The following schemes are supported:
#'  HTTP/HTTPS files, S3 (AWS S3) and GS (Google Cloud Storage).
#'
#' @param ...          additional arguments. See details.
#'
#' @param fn           a `function` that will be used to filter the attributes
#'  listed in the properties.
#'
#' @return a `list` with the attributes of date, bands and paths.
#'
#' @examples
#' \donttest{
#' # STACItemCollection object
#' stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'  stac_search(collections = "CB4_64_16D_STK-1", limit = 100,
#'         datetime = "2017-08-01/2018-03-01",
#'         bbox = c(-48.206,-14.195,-45.067,-12.272)) %>%
#'  get_request() %>% items_fetch(progress = FALSE)
#'
#' stac_item %>% assets_list(assets_names = c("EVI", "NDVI"))
#' }
#'
#' @name assets_function
NULL

#' @rdname assets_function
#' @export
assets_list <- function(items, asset_names = NULL,
                        sort = TRUE, gdal_vsi_resolution = TRUE,
                        assets_names = deprecated()) {


  if (lifecycle::is_present(assets_names)) {

    # Signal the deprecation to the user
    lifecycle::deprecate_soft("0.9.1-5",
                              "rstac::assets_download(assets_names = )",
                              "rstac::assets_download(asset_names = )")

    # Deal with the deprecated argument for compatibility
    asset_names <- assets_names
  }

  if (is.null(asset_names))
    asset_names <- items_fields(items, "assets")

  timeline <- items_reap(items, field = c("properties", "datetime"))
  index    <- seq_along(timeline)
  if (sort) index <- order(timeline)

  timeline <- timeline[index]
  assets   <- list(date = rep(timeline, length(unique(asset_names))))

  for (b in asset_names) {

    href <- items_reap(items, field = c("assets", b, "href"))[index]

    if (gdal_vsi_resolution) {

      # for http or https schema
      paste_index <- grepl("^http|[s]://.*", href)
      if (any(paste_index))
        href[paste_index] <- paste("/vsicurl", href[paste_index], sep = "/")

      # for S3 schema
      paste_index <- grepl("^s3://.*", href)
      if (any(paste_index))
        href[paste_index] <- paste("/vsis3", gsub("^s3://(.*)$", "\\1",
                                                  href[paste_index]), sep = "/")
      # for gs schema
      paste_index <- grepl("^gs://.*", href)
      if (any(paste_index))
        href[paste_index] <- paste("/vsigs", gsub("^gs://(.*)$", "\\1",
                                                  href[paste_index]), sep = "/")
    }
    assets$band <- c(rep(b, length(href)), assets$band)
    assets$path <- c(href,  assets$path)
  }
  assets
}

#' @rdname assets_function
#'
#' @export
assets_select <- function(items, asset_names) {

  if (!all(asset_names %in% items_assets(items, simplify = TRUE)))
    .error("Invalid 'asset_names' parameter.")

  items$features <- lapply(items$features, function(item) {
    item$assets <- item$assets[asset_names]

    item
  })

  items
}

#' @rdname assets_function
#'
#' @export
assets_filter <- function(items, ..., fn = NULL) {

  dots <- substitute(list(...))[-1]

  if (length(dots) > 0) {

    if (!is.null(names(dots)))
      .error("Invalid filter arguments.")

    for (i in seq_along(dots)) {

      items$features <- lapply(items$features, function(item) {

        sel <- vapply(item$assets, function(asset) {
          eval(dots[[i]], envir = asset)
        }, logical(1))

        item$assets <- item$assets[sel]

        item
      })
    }
  }

  if (!is.null(fn)) {

    items$features <- lapply(items$features, function(item) {

      sel <- vapply(item$assets, function(asset) { fn(asset) }, logical(1))

      item$assets <- item$assets[sel]
      item
    })
  }

  items
}
