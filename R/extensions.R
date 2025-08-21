#' @title Extension development functions
#'
#' @description
#' Currently, there are five STAC documents defined in STAC spec:
#' \itemize{
#' \item `doc_catalog`
#' \item `doc_collection`
#' \item `doc_collections`
#' \item `doc_item`
#' \item `doc_items`
#' }
#'
#' Each document class is associated with STAC API endpoints.
#' As soon as new STAC documents are proposed in the specification, new
#' classes can be created in the `rstac` package.
#'
#' Let `version` parameter `NULL` to detect version automatically.
#'
#' Basically, there are two types of extensions in STAC specification:
#' \enumerate{
#' \item STAC documents extensions: these extensions can be defined in
#' different elements of the document specification.
#' \item STAC API extensions: these extensions are associated with the
#' interaction between the client and server through API and may add new
#' elements in the STAC documents or just filter the elements to be returned
#' in the documents.
#' }
#' Here, we will focus on the second type of extension.
#'
#' To let `rstac` package perform some behavior according to an
#' STAC API extension we need define some functions. These functions
#' can be implemented in three environments:
#' \enumerate{
#' \item In `rstac` package by including new functions make a
#' GitHub pull request on `rstac` repository
#' (<https://github.com/brazil-data-cube/rstac>)
#' \item In a new package by using `rstac` as dependent package
#' \item In a script that loads `rstac` into the environment
#' }
#' All these places may impose specific requirements, however the core
#' logic to implement an extension is the same.
#'
#' These functions are intended for those who want to implement new STAC API
#' extensions. An extension must define a subclass name and implement all the
#' following S3 generic methods for that subclass:
#' \itemize{
#' \item `before_request()`: allows handling query parameters before
#' submit them to the HTTP server, usually sets up the query endpoint;
#' \item `after_request()`: allows to check and parse document received
#' by the HTTP server;
#' }
#'
#' These methods will work 'behind the scenes' when a `rstac_query` object
#' representing a user query are passed to a request function
#' (e.g. `get_request()` or `post_request()`). The calling order is:
#' \enumerate{
#' \item begin of `get_request()` or `post_request()`
#' \item if STAC API version is not defined, try detect it
#' \item call `before_request()`
#' \item send HTTP request
#' \item receive HTTP response
#' \item `after_response()`
#' \item end of `get_request()` or `post_request()`
#' }
#'
#' Besides that, the extension must expose a function to receive user
#' parameters and return a `rstac_query` object with a subclass
#' associated with the above S3 methods. This function must accept as its
#' first parameter a `rstac_query` object representing the actual query.
#' To keep the command flow consistency, the function needs to check the
#' subclass of the input query. After that, it must set new or changes the
#' input query parameters according to the user input and, finally,
#' return the new query as a `rstac_query` object.
#'
#' You can see examples on how to implement an STAC API extension by looking at
#' `stac.R`, `collections.R`, `items.R`, `stac_search.R`,
#' and `ext_query.R` source files. These files implement core STAC API
#' endpoints, as well as the query API extension.
#'
#' There are also some utility functions described in **Functions**
#' section bellow that can help the extension development.
#'
#'
#' @param q                a `rstac_query` object expressing a STAC query
#' criteria.
#'
#' @param res              a `httr` `response` object.
#' @param simplify_vector  a `logical` describing whether length-one nested
#' lists should be simplified into vectors. Defaults to TRUE. Can also be set
#' for an entire session via e.g. \code{options(rstac.simplify_vector = FALSE)}.
#' @param params           a `list` with params to add in request.
#'
#' @return
#' A `rstac_query` object for `before_request()` and
#' `after_response()` functions.
#'
#' @seealso [ext_query()]
#'
#' @name extensions
#'
#' @keywords internal
NULL

#' @title Extension development functions
#'
#' @rdname extensions
before_request <- function(q) {
  UseMethod("before_request", q)
}

#' @title Extension development functions
#'
#' @rdname extensions
after_response <- function(q, res, simplify_vector = TRUE) {
  UseMethod("after_response", q)
}

#' @title Extension development functions
#'
#' @rdname extensions
parse_params <- function(q, params) {
  UseMethod("parse_params", q)
}

#' @describeIn extensions
#' The `content_response` function checks if the request's
#' response is in accordance with the allowed status codes and content-types.
#' It returns the parsed content response.
#'
#' @param res           a `httr` `response` object.
#'
#' @param status_codes  a `character` vector with successful
#' status codes.
#'
#' @param content_types a `character` vector with all acceptable
#' responses' content type.
#'
#' @param key_message   a `character` vector with the JSON keys to show the
#' requested API message.
#'
#' @return
#' The `content_response()` function returns a `list` data structure
#' representing the JSON file received in HTTP response
content_response <- function(res, status_codes, content_types, key_message,
                             simplify_vector = TRUE) {
  # convert any json extension
  if (!grepl(content_types, httr::http_type(res))) {
    .error("HTTP content type response '%s' not defined for this operation.",
           httr::http_type(res))
  }

  # parse content
  content <- httr::content(res,
                           type = "application/json",
                           encoding = "UTF-8",
                           simplifyVector = simplify_vector,
                           simplifyDataFrame = FALSE,
                           simplifyMatrix = FALSE)

  # test for allowed status codes
  status_code <- as.character(httr::status_code(res))
  if (!status_code %in% status_codes) {
    message <- ""
    if (is.atomic(content))
      message <- content
    else if (any(key_message %in% names(content))) {
      message <- content[[which(names(content) %in% key_message)[[1]]]]
    }
    .error("HTTP status '%s'. %s", status_code, message)
  }

  return(content)
}

#' @describeIn extensions
#' The `check_query_verb()` function allows you to define which HTTP
#' verbs are allowed. It is useful for establishing which verbs will be
#' supported by an extension.
#'
#' @param q       a `rstac_query` object.
#'
#' @param verbs   a `character` vector with allowed HTTP request methods
#'
#' @param msg     a `character` with a personalized error message
check_query_verb <- function(q, verbs, msg = NULL) {

  if (!q$verb %in% verbs) {
    if (is.null(msg))
      msg <- sprintf("HTTP verb '%s' is not defined for the query '%s'.",
                     q$verb, subclass(q))
    .error(msg)
  }
}

#' @describeIn extensions
#' The `check_query()` function specifies which type of query
#' object (`rstac_query`) is expected in the function extension.
#'
#' @param x         a `rstac_query` object expressing a STAC query
#' criteria.
#'
#' @param classes   a `character` vector with all allowed S3 sub-classes
check_query <- function(x, classes = NULL) {
  if (!inherits(x, "rstac_query"))
    .error("Invalid rstac_query value.")
  if (!is.null(classes) && !any(classes %in% subclass(x)))
    .error("Expecting %s query.", paste0("`", classes, "`", collapse = " or "))
}

#' @describeIn extensions
#' The `subclass()` function returns a `character` representing the
#' subclass name of `rstac_query` objects.
subclass <- function(x) {
  UseMethod("subclass", x)
}

#' @describeIn extensions
#' The `set_query_endpoint()` function defines the endpoint of a query.
#'  If `params` parameter is passed, each value must be an entry of params
#'  field of the given query. The corresponding param value will be used as
#'  value replacement of `%s` occurrences in the `endpoint` string. After
#'  the replacement, all params in this list will be removed.
#'
#' @param q          a `rstac_query` object.
#'
#' @param endpoint   a `character` vector with the format string with the
#'    endpoint url.
#'
#' @param params     a `character` vector with the params entries to replace
#'    all `%s` occurrences in the endpoint string.
#'
set_query_endpoint <- function(q, endpoint, params = NULL) {
  if (any(!params %in% names(q$params)))
    .error("Invalid param(s) %s.",
           paste("`", setdiff(params, names(q$params)), "`", collapse = ", "))
  values <- unname(q$params[params])
  q$endpoint <- do.call(sprintf, args = c(list(fmt = endpoint), values))
  q$params[params] <- NULL
  q
}

content_response_json <- function(res, simplify_vector = TRUE) {
  content_response(
    res = res,
    status_codes = "200",
    content_types = "application/.*json",
    key_message = c("message", "description", "detail"),
    simplify_vector = simplify_vector
  )
}
