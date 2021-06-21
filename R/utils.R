#' @title Utility functions
#'
#' @param bbox        a \code{numeric} vector with only features that have a
#' geometry that intersects the bounding box are selected. The bounding box is
#' provided as four or six numbers, depending on whether the coordinate
#' reference system includes a vertical axis (elevation or depth):
#' \itemize{ \item Lower left corner, coordinate axis 1
#'           \item Lower left corner, coordinate axis 2
#'           \item Lower left corner, coordinate axis 3 (optional)
#'           \item Upper right corner, coordinate axis 1
#'           \item Upper right corner, coordinate axis 2
#'           \item Upper right corner, coordinate axis 3 (optional) }
#'
#'
#' @return A \code{numeric} with the bbox provided,or an error if the supplied
#'   \code{bbox} does not meet the specifications.
#'
#' @noRd
.parse_bbox <- function(bbox) {

  if (!length(bbox) %in% c(4, 6))
    .error("Param `bbox` must have 4 or 6 numbers, not %s.", length(bbox))

  return(bbox)
}

#' @title Utility functions
#'
#' @param limit       an \code{integer} defining the maximum number of results
#' to return. If not informed it defaults to the service implementation.
#'
#' @return A \code{integer} with the limit provided,or an error if the supplied
#'  \code{limit} has a length different from 1.
#'
#' @noRd
.parse_limit <- function(limit) {

  if (length(limit) != 1)
    .error("Parameter `limit` must be a single value.")

  limit <- as.character(limit)

  limit_int <- suppressWarnings(as.integer(limit))

  if (any(is.na(as.integer(limit))) || as.character(limit_int) != limit)
    .error("Param `limit` must be an integer.")

  return(limit)
}

#' @title Utility functions
#'
#' @param feature_id  a \code{character} with item id to be fetched.
#' Only works if the \code{collection_id} is informed. This is equivalent to
#' the endpoint \code{/collections/\{collectionId\}/items/\{featureId\}}.
#'
#' @return A \code{character} with the parameter provided,or an error if the
#'  supplied \code{feature_id} has a length different from 1.
#'
#' @noRd
.parse_feature_id <- function(feature_id) {

  if (length(feature_id) != 1)
    .error("Parameter `feature_id` must be a single value.")

  return(feature_id)
}

#' @title Utility functions
#'
#' @param collections a \code{character} vector of collection IDs to include in
#' the search for items. Only items in one of the provided collections will be
#' searched.
#'
#' @return A \code{list} of collections.
#'
#' @noRd
.parse_collections <- function(collections) {

  if (length(collections) == 1 && !is.list(collections))
    collections <- list(collections)

  return(collections)
}

#' @title Utility functions
#'
#' @param ids         a \code{character} vector with item IDs. All other filter
#' parameters that further restrict the number of search results are ignored.
#'
#' @return A \code{list} with the ids.
#'
#' @noRd
.parse_ids <- function(ids) {

  if (length(ids) == 1 && !is.list(ids))
    ids <- list(ids)

  return(ids)
}

#' @title Utility functions
#'
#' @param intersects  a \code{character} value expressing GeoJSON geometries
#' objects as specified in RFC 7946. Only returns items that intersect with
#' the provided polygon.
#'
#' @return A \code{character} with the validate polygon.
#'
#' @noRd
.parse_geometry <- function(geom) {

  # TODO: validate polygon
  geom
}

#' @title Utility functions
#'
#' @param items       \code{STACItemCollection} object representing the result
#'  of \code{/stac/search} or \code{/collections/{collectionId}/items}.
#'
#' @return A \code{numeric} with the length of a \code{STACItemCollection}
#'  object.
#'
#' @noRd
.parse_items_size <- function(items) {

  if (items_length(items) != items_matched(items))
    .message(paste("The length of items in your object, does not correspond",
                   "with the total of matched items. Consider using the",
                   "function `items_fetch()`. By default, items_max = %d"),
             items_length(items))

  return(items_length(items))
}


#' @title Utility functions
#'
#' @description Auxiliary function to check whether the date time follows
#' RFC 3339 standard.
#'
#' @param datetime Either a date-time or an interval, open or closed.
#' Date and time expressions adhere to RFC 3339. Open intervals are
#' expressed using double-dots.
#' Examples:
#' \itemize{
#'   \item A date-time: \code{"2018-02-12T23:20:50Z"}
#'   \item A closed interval: \code{"2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"}
#'   \item Open intervals: \code{"2018-02-12T00:00:00Z/.."} or
#'     \code{"../2018-03-18T12:31:12Z"}
#' }
#'
#' @return An error if the date does not follow the specified standards or the
#'  date time provided as \code{character}.
#'
#' @noRd
.parse_datetime <- function(datetime) {

  # check if the date time provided is an open interval
  check_interval <-
    grepl("(?=^(\\..\\/.*)).+|(.*/\\..)", datetime, perl = TRUE)

  if (check_interval) {
    # regex to separate the open interval elements
    split_datetime <- strsplit(datetime, "(\\/\\..)|(\\..\\/)", perl = TRUE)
    split_datetime <- split_datetime[[1]][which(unlist(split_datetime) != "")]

    # checking if date time is in the RFC standards
    match_rfc <- .check_rfc_3339(split_datetime)

    if (!match_rfc)
      .error(paste("The interval date time provided is not in RFC format,",
                   "please check the RFC 3339 rules."))

    return(datetime)
  } else {

    # Splits the vector elements with the dates by the backslash
    split_datetime <- strsplit(datetime, "/", perl = TRUE)
    split_datetime <- unlist(split_datetime)

    # In case the vector has two elements it is a closed date time
    if (length(split_datetime) == 2) {
      # Checks if there is FALSE value in vector
      if (!all(.check_rfc_3339(split_datetime)))
        .error(paste0("The date time provided not follow the RFC 3339 format,",
                      "please check the RFC 3339 rules."))

      # formatting the closed date time according to the RFC
      interval_dt <- as.POSIXct(split_datetime,
                                tz = "UTC",
                                tryFormats = c("%Y-%m-%dT%H:%M:%SZ",
                                               "%Y-%m-%d"))

      # Check the interval, if the interval is wrong an error is returned
      if (interval_dt[1] > interval_dt[2]) {
        .error(paste("The closed date time provided is not in correct",
                     "interval, the first date time shold be less than",
                     "second."))
      }

      return(datetime)
    }

    # Check if date time is a fixed interval
    else {
      if (!all(.check_rfc_3339(split_datetime)) || length(split_datetime) != 1)
        .error(paste("The date time provided not follow the RFC 3339 format,",
                     "please check the RFC 3339 rules."))

      return(datetime)
    }
  }
}

#' @title Utility functions
#'
#' @description Auxiliary function to check that the provided date time follows
#' the standards of RFC 3339
#'
#' @param datetime Either a date-time or an interval, open or closed.
#' Date and time expressions adhere to RFC 3339. Open intervals are
#' expressed using double-dots.
#' Examples:
#' \itemize{
#'   \item A date-time: \code{"2018-02-12T23:20:50Z"}
#'   \item A closed interval: \code{"2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"}
#'   \item Open intervals: \code{"2018-02-12T00:00:00Z/.."} or
#'     \code{"../2018-03-18T12:31:12Z"}
#' }
#'
#' @return A \code{logical} if TRUE the date time provided is correct,
#' otherwise not.
#'
#' @noRd
.check_rfc_3339 <- function(datetime) {

  # Standard regexp of RFC 3339
  pattern_rfc   <- "^\\d{4}-\\d{2}-\\d{2}?(T\\d{2}:\\d{2}:\\d{2}Z)?$"
  check_pattern <- grepl(pattern_rfc, datetime, perl = TRUE)

  return(check_pattern)
}

#' @title Utility functions
#'
#' @param msg   a \code{character} string with format error message.
#'
#' @param ...   values to be passed to \code{msg} parameter.
#'
#' @noRd
.error <- function(msg, ...) {

  stop(sprintf(msg, ...), call. = FALSE)
}

#' @title Utility functions
#'
#' @param msg   a \code{character} string with format text message.
#'
#' @param ...   values to be passed to \code{msg} parameter.
#'
#' @noRd
.message <- function(msg, ...) {

  message(sprintf(msg, ...))
}

#' @title Utility functions
#'
#' @param msg   a \code{character} string with format warning message.
#'
#' @param ...   values to be passed to \code{msg} parameter.
#'
#' @noRd
.warning <- function(msg, ...) {

  warning(sprintf(msg, ...), call. = FALSE)
}

#' @title Utility functions
#'
#' @param obj       an \code{object} to compare.
#'
#' @param expected  a \code{character} with the expected classes.
#'
#' @noRd
.check_obj <- function(obj, expected) {

  obj_name <- as.character(substitute(obj))

  if (missing(obj))
    .error("Param `%s` is missing.", obj_name)

  if (!inherits(obj, expected))
    .error("Invalid %s value in `%s` param.",
           paste0("`", expected, "`", collapse = " or "), obj_name)
}


#' @title uUtility functions
#'
#' @rdname http_request
#'
#' @description
#' \code{.make_url} is a helper function to generate url. The returned
#' url is formed by appending \code{endpoint} at the end of base url
#' informed by \code{url} parameter. If \code{endpoint} has multiple elements
#' it will be collapsed using \code{'/'} character.
#'
#' Note that \code{.make_url} function differs from standards of relative URI
#' path resolution (RFC 3986). Any existing path in base url
#' is maintained in the final url, and a simple string contatenation is made
#' whithout including any character separator. For this reason, this function
#' does not support the query and fragment URI components in the base url.
#'
#' @param url         a \code{character} informing the base url of a
#' STAC web service.
#'
#' @param endpoint    a \code{character} a path to be appended in the final
#' url.
#'
#' @param params      a named \code{list} with all url query parameters to be
#' appended in the url.
#'
#' @return
#' \code{.make_url} returns an url to access STAC endpoints.
#'
#' @noRd
.make_url <- function(url, endpoint = "", params = list()) {

  # remove trailing '/' char
  if (substring(url, nchar(url)) == "/")
    url <- substring(url, 1, nchar(url) - 1)

  endpoint <- paste0(endpoint, collapse = "/")

  # TODO: URI resolution for previous existing query and fragment URI components
  # in informed url.
  res <- paste0(url, endpoint)

  if (length(params) > 0) {

    if (is.null(names(params)))
      stop("URL query values must be named.", call. = FALSE)
    params <- .querystring_encode(params)
    res <- paste(res, params, sep = "?")
  }

  return(res)
}

#' @title Utility functions
#'
#' @param params a \code{list} of parameters received from stac objects.
#'
#' @return a \code{character} representing the encode parameters of the query.
#'
#' @noRd
.querystring_encode <- function(params) {

  if (!is.null(names(params)))
    return(paste(names(params),
                 vapply(unname(params), paste0, collapse = ",", character(1)),
                 sep = "=", collapse = "&"))
  return(paste0(params, collapse = ","))
}

#' @title Utility functions
#'
#' @param querystring a \code{character} with the query to be decoded.
#'
#' @return a \code{list} with the query params.
#'
#' @noRd
.querystring_decode <- function(querystring) {

  # first decode and remove all coded spaces
  querystring <- URLdecode(querystring)

  values <- lapply(strsplit(querystring, split = "&")[[1]],
                   function(x) strsplit(x, split = "=")[[1]])

  params <- lapply(values, `[[`, 2)
  names(params) <- vapply(values, `[[`, 1, FUN.VALUE = character(1))

  return(params)
}

#' @title Utility functions
#'
#' @param params a \code{list} with the parameters of query.
#'
#' @return a \code{list} with the query parameters.
#'
#' @noRd
.validate_query <- function(params) {

  if (!is.null(params$query) && is.character(params$query)) {
    params$query <- jsonlite::fromJSON(params$query)

    if (is.list(params$query))
      params$query <- list(params$query)
  }

  return(params)
}

#' @title Utility functions
#'
#' @description
#' These function retrieves information about either \code{rstac} queries
#' (\code{RSTACQuery} objects) or \code{rstac} documents
#' (\code{RSTACDocument} objects).
#'
#' @param x        either a \code{RSTACQuery} object expressing a STAC query
#' criteria or any \code{RSTACDocument}.
#'
#' @param ...      config parameters to be passed to \link[httr]{GET}
#' method, such as \link[httr]{add_headers} or \link[httr]{set_cookies}.
#'
#' @return
#' The \code{stac_version()} function returns a \code{character} STAC API
#' version.
#'
#' @name utilities
#'
#' @export
stac_version <- function(x, ...) {

  UseMethod("stac_version")
}

#' @title Utility functions
#'
#' @description This function returns the \code{date}, \code{band} and
#'  \code{URL} fields for each assets of an \code{STACItemCollection} object.
#'  For the URL you can add the GDAL library drivers for the following schemes:
#'  HTTP/HTTPS files, S3 (AWS S3) and GS (Google Cloud Storage).
#'
#' @param items               a \code{STACItemCollection} object representing
#'  the result of \code{/stac/search}, \code{/collections/{collectionId}/items}.
#'
#' @param assets_names        a \code{character} with the assets names to be
#'  filtered. If \code{NULL} (default) all assets will be returned.
#'
#' @param sort                a \code{logical} if true the dates will be sorted
#'  in increasing order. By default, the dates are sorted.
#'
#' @param gdal_vsi_resolution a \code{logical}  if true, gdal drivers are
#'  included in the URL of each asset. The following schemes are supported:
#'  HTTP/HTTPS files, S3 (AWS S3) and GS (Google Cloud Storage).
#'
#' @return a \code{list} with the attributes of date, bands and paths.
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
#' @export
assets_list <- function(items, assets_names = NULL, sort = TRUE,
                        gdal_vsi_resolution = TRUE) {

  if (is.null(assets_names))
    assets_names <- items_fields(items, "assets")

  timeline <- items_reap(items, field = c("properties", "datetime"))
  index    <- seq_along(timeline)
  if (sort) index <- order(timeline)

  timeline <- timeline[index]
  assets   <- list(date = rep(timeline, length(unique(assets_names))))

  for (b in assets_names) {

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

#' @title Utility functions
#'
#' @description This function returns the values of a field of the
#'  \code{STACItemCollections} object. If the values of the specified field are
#'  not atomic the return will be in list form, if they are, it will be returned
#'  in vector form.
#'
#' @param items               a \code{STACItemCollection} object representing
#'  the result of \code{/stac/search}, \code{/collections/{collectionId}/items}.
#'
#' @param ...                 a named way to provide fields names to get the
#'  subfields values from the \code{RSTACDocument} objects.
#'
#' @param field               a \code{character} with the names of the field to
#'  get the subfields values from the \code{RSTACDocument} objects.
#'
#' @return a \code{vector} if the supplied field is atomic, or a list if not.
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
#' @description This function returns the subfields of the \code{feature}
#' field of a \code{STACItemCollection} object.
#'
#' @param items  a \code{STACItemCollection} object representing
#'  the result of \code{/stac/search}, \code{/collections/{collectionId}/items}.
#'
#' @param ...    a named way to provide field names to get the subfields values
#'  from the \code{RSTACDocument} objects.
#'
#' @param field a \code{character} with the names of the field to get the
#'  subfields values from the \code{RSTACDocument} objects.
#'
#' @return a \code{character} with the subfields of the \code{feature} field.
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
#' @export
items_fields <- function(items, ..., field = NULL) {

  # checks if the object is STACItemCollections
  if (items_length(items) == 0) return(NULL)

  dots <- substitute(list(...))[-1]
  if (!is.character(dots)) dots <- as.character(dots)

  if (length(field) > 0 && length(dots) > 0)
    .error("Only one of the parameters '...' or 'field' must be supplied.")

  if (length(field) == 0 && length(dots) == 0)
    return(names(items$features[[1]]))
  names(items$features[[1]][[c(dots, field)]])
}


#' @title Utility functions
#'
#' @description This function groups the items contained within the
#'  \code{STACItemCollection} object according to some specified fields. Each
#'  index in the returned list contains items belonging to the same group.
#'
#' @param items               a \code{STACItemCollection} object representing
#'  the result of \code{/stac/search}, \code{/collections/{collectionId}/items}.
#'
#' @param ...    a named way to provide field names to get the subfields values
#'  from the \code{RSTACDocument} objects.
#'
#' @param field  a \code{character} with the names of the field to get the
#'  subfields values from the \code{RSTACDocument} objects.
#'
#' @param index a \code{character} with the indexes to be grouped. It can be
#'  used with the function \link{items_reap}.
#'
#' @return a \code{list} in which each index corresponds to a group with its
#'  corresponding \code{STACItemCollection} objects.
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
#' @param bbox        a \code{numeric} vector with only features that have a
#' geometry that intersects the bounding box are selected. The bounding box is
#' provided as four or six numbers, depending on whether the coordinate
#' reference system includes a vertical axis (elevation or depth):
#' \itemize{ \item Lower left corner, coordinate axis 1
#'           \item Lower left corner, coordinate axis 2
#'           \item Lower left corner, coordinate axis 3 (optional)
#'           \item Upper right corner, coordinate axis 1
#'           \item Upper right corner, coordinate axis 2
#'           \item Upper right corner, coordinate axis 3 (optional) }.
#'
#' @return a \code{character} with \code{bbox} formatted based on min and max
#'  values.
#'
#' @noRd
.format_bbox <- function(bbox) {

  if (!is.null(bbox) & length(bbox) == 4)
    return(paste(c("xmin:", "ymin:", "xmax:", "ymax:"),
                 sprintf("%.5f", bbox), collapse = ", "))

  if (!is.null(bbox) & length(bbox) == 6)
    return(paste(c("xmin:", "ymin:", "zmin:", "xmax:", "ymax:", "zmax:"),
                 sprintf("%.5f", bbox), collapse = ", "))
}
