#' @title Extension development functions
#'
#' @description
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
#' To let \code{rstac} package perform some behavior according to an
#' STAC API extension we need define some functions. These functions
#' can be implemented in three environments:
#' \enumerate{
#' \item In \code{rstac} package by including new functions make a
#' GitHub pull request on \code{rstac} repository
#' (\url{https://github.com/brazil-data-cube/rstac})
#' \item In a new package by using \code{rstac} as dependent package
#' \item In a script that loads \code{rstac} into the environment
#' }
#' All these places may impose specific requirements, however the core
#' logic to implement an extension is the same.
#'
#' These functions are intended for those who want to implement new STAC API
#' extensions. An extension must define a subclass name and implement all the
#' following S3 generic methods for that subclass:
#' \itemize{
#' \item \code{endpoint()}: returns the endpoint value of the extension.
#' Endpoints that vary between STAC API versions can be properly returned by
#' checking the \code{version} field of \code{RSTACQuery} object.
#' \item \code{before_request()}: allows handling query parameters before
#' submit them to the HTTP server;
#' \item \code{after_request()}: allows to check and parse document received
#' by the HTTP server;
#' }
#'
#' These methods will work 'behind the scenes' when a \code{RSTACQuery} object
#' representing a user query are passed to a request function
#' (e.g. \code{get_request()} or \code{post_request()}). The calling order is:
#' \enumerate{
#' \item begin of \code{get_request()} or \code{post_request()}
#' \item if STAC API version is not defined, try detect it
#' \item call \code{endpoint()}
#' \item call \code{before_request()}
#' \item send HTTP request
#' \item receive HTTP response
#' \item \code{after_response()}
#' \item end of \code{get_request()} or \code{post_request()}
#' }
#'
#' Besides that, the extension must expose a function to receive user
#' parameters and return a \code{RSTACQuery} object with a subclass
#' associated with the above S3 methods. This function must accept as its
#' first parameter a \code{RSTACQuery} object representing the actual query.
#' To keep the command flow consistency, the function needs to check the
#' subclass of the input query. After that, it must set new or changes the
#' input query parameters according to the user input and, finally,
#' return the new query as a \code{RSTACQuery} object.
#'
#' You can see examples on how to implement an STAC API extension by looking at
#' \code{stac.R}, \code{collections.R}, \code{items.R}, \code{stac_search.R},
#' and \code{ext_query.R} source files. These files implement core STAC API
#' endpoints, as well as the query API extension.
#'
#' There are also some utility functions described in \strong{Functions}
#' section bellow that can help the extension development.
#'
#'
#' @param q       a \code{RSTACQuery} object expressing a STAC query
#' criteria.
#'
#' @param res     a \code{httr} \code{response} object.
#'
#' @return
#' A \code{character} endpoint value for \code{endpoint()} function.
#' A \code{RSTACQuery} object for \code{before_request()} and
#' \code{after_response()} functions.
#'
#' @seealso \code{\link{ext_query}}
#'
#' @name extensions
NULL

#' @title Extension development functions
#' @rdname extensions
#' @export
endpoint <- function(q) {

  UseMethod("endpoint")
}

#' @title Extension development functions
#' @rdname extensions
#' @export
before_request <- function(q) {

  UseMethod("before_request")
}

#' @title Extension development functions
#' @rdname extensions
#' @export
after_response <- function(q, res) {

  UseMethod("after_response")
}

#' @describeIn extensions
#' The \code{content_response} function checks if the request's
#' response is in accordance with the allowed status codes and content-types.
#' It returns the parsed content response.
#'
#' @param res     a \code{httr} \code{response} object.
#'
#' @param status_codes  a \code{character} vector with successful
#' status codes.
#'
#' @param content_types a \code{character} vector with all acceptable
#' responses' content type.
#'
#' @return
#' The \code{content_response()} function returns a \code{list} data structure
#' representing the JSON file received in HTTP response
#'
#' @export
content_response <- function(res, status_codes, content_types) {

  # convert any json extension
  content_type <- httr::http_type(res)
  if (grepl("application/.*json", content_type))
    content_type <- "application/json"

  # parse content
  content <- httr::content(res,
                           type = content_type,
                           encoding = "UTF-8",
                           simplifyVector = TRUE,
                           simplifyDataFrame = FALSE,
                           simplifyMatrix = FALSE)

  # test for allowed status codes
  status_code <- as.character(httr::status_code(res))
  if (!status_code %in% status_codes) {
    message <- ""
    if (is.atomic(content))
      message <- content
    else if (!is.null(content[["description"]]))
      message <- content[["description"]]

    .error("HTTP status '%s'. %s", status_code, message)
  }

  # test for allowed content types
  if (!httr::http_type(res) %in% content_types)
    .error("HTTP content type response '%s' not defined for this operation.",
           httr::http_type(res))

  return(content)
}

#' @describeIn extensions
#' The \code{check_query_verb()} function allows you to define which HTTP
#' verbs are allowed. It is useful for establishing which verbs will be
#' supported by an extension.
#'
#' @param q       a \code{RSTACQuery} object.
#'
#' @param verbs   a \code{character} vector with allowed HTTP request methods
#'
#' @param msg     a \code{character} with a personalized error message
#'
#' @export
check_query_verb <- function(q, verbs, msg = NULL) {

  if (!q$verb %in% verbs) {
    if (is.null(msg))
      msg <- sprintf("HTTP verb '%s' is not defined for the query '%s'.",
                     q$verb, subclass(q))
    .error(msg)
  }
}

#' @describeIn extensions
#' The \code{check_subclass()} function specifies which type of query
#' objects (\code{RSTACQuery}) or document objects (\code{RSTACDocument})
#' are expected in the function extension.
#'
#' @param x            either a \code{RSTACQuery} object expressing a STAC query
#' criteria or any \code{RSTACDocument}.
#'
#' @param subclasses   a \code{character} vector with all allowed S3 subclasses
#'
#' @export
check_subclass <- function(x, subclasses) {

  UseMethod("check_subclass")
}

#' @describeIn extensions
#' The \code{subclass()} function returns a \code{character} representing the
#' subclass name of either \code{RSTACQuery} or \code{RSTACDocument} S3 classes.
#'
#' @export
subclass <- function(x) {

  UseMethod("subclass")
}

#' @describeIn extensions
#' The \code{omit_query_params()} function was created to omit the paths that
#'  are defined as query parameters to simplify the creation of a query.
#'  Therefore, use this method only in endpoints that specify a parameter in
#'  their paths.
#'
#' @param q       a \code{RSTACQuery} object.
#'
#' @param names   a \code{character} vector with the names do omit.
#'
#' @export
omit_query_params <- function(q, names) {

  .check_obj(names, "character")
  q$omitted <- unname(names)
  q
}
