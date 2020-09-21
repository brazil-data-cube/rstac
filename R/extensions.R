
#' @title Extension development functions
#'
#' @description
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
#' To let \code{rstac} package perform some behavior according to an
#' STAC API extension we need define some functions. These functions
#' can be implemented in three environments:
#' \enumerate{
#' \item In \code{rstac} package by including new functions make a
#' GitHub pull request on \code{rstac} repository
#' (\url{https://github.com/nrazil-data-cube/rstac})
#' \item In a new package by using \code{rstac} as dependent package
#' \item In a script that loads \code{rstac} into the enviornment
#' }
#' All these places may impose specific requirements, however the core
#' logic to implement an extension is the same.
#'
#' These functions are intended for those who want to implement new STAC API
#' extensions. An extension must define a subclass name and implement all the
#' following S3 generic methods for that subclass:
#' \itemize{
#' \item \code{get_endpoint()}: returns the endpoint value of the extension.
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
#' \item call \code{get_endpoint()}
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
#' @param s       a \code{RSTACQuery} object expressing a STAC query
#' criteria.
#'
#' @param res     a \code{httr} \code{response} object.
#'
#' @return
#' A \code{character} endpoint value for \code{get_endpoint()} function.
#' A \code{RSTACQuery} object for \code{before_request()} and
#' \code{after_response()} functions.
#'
#' @seealso \code{\link{extension_query}}
#'
#' @name extensions
#'
#' @export
get_endpoint <- function(s) {

  UseMethod("get_endpoint")
}

#' @rdname extensions
#'
#' @export
before_request <- function(s) {

  UseMethod("before_request")
}

#' @rdname extensions
#'
#' @export
after_response <- function(s, res) {

  UseMethod("after_response")
}

#' @describeIn extensions
#' The \code{RSTACQuery()} function is a constructor of \code{RSTACQuery} objects.
#' Every extension must implement a subclass of \code{RSTACQuery} to represent
#' its queries. This is done by informing to the \code{subclass} parameter
#' the extension's subclass name.
#'
#' The \code{params} parameter is a named \code{list} where user parameters
#' must be stored. It is important to know if previous query parameters needs
#' to be keeped in the new query. If so, it is recommended do use
#' \code{\link[utils]{modifyList}()} function to merge the old and new
#' query parameters.
#'
#' If the \code{version} parameter is \code{NULL}, \code{rstac} will detect
#' STAC API version automatically.
#'
#' In general, if you are implementing a new subclass, the parameters
#' \code{version} and \code{url} will be the same as the previous query. The
#' \code{params} parameter will be merged with previous query. And subclass
#' is the extension's subclass name.
#'
#' @param version    a \code{character} with the STAC version.
#'
#' @param url        a \code{character} informing the base URL of a
#' STAC web service.
#'
#' @param params     a named \code{list} with all URL query parameters to be
#' appended in the URL.
#'
#' @param subclass   a \code{character} corresponding to the subclass of the
#' object to be created.
#'
#' @return
#' The \code{RSTACQuery()} function returns a \code{STACQuery} object with
#' subclass defined by \code{subclass} parameter.
#'
#' @export
RSTACQuery <- function(version = NULL, url, params = list(), subclass) {

  structure(
    list(version = version,
         url = url,
         endpoint = NULL,
         params = params,
         verb = "GET",
         encode = NULL
    ), class = c(subclass, "RSTACQuery"))
}

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
#' @param s          a \code{RSTACQuery} object expressing the STAC query used
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
RSTACDocument <- function(content, s, subclass) {

  structure(
    content,
    stac = s,
    class = c(subclass, "RSTACDocument")
  )
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
#' @param verbs   a \code{character} vector with allowed HTTP request methods
#'
#' @export
check_query_verb <- function(s, verbs) {

  if (!s$verb %in% verbs)
    .error("HTTP verb '%s' not defined for this query operation.", s$verb)
}

#' @describeIn extensions
#' The \code{check_query_subclass()} function specifies which type of query
#' objects (\code{RSTACQuery}) are expected in the function extension.
#'
#' @param subclasses   a \code{character} vector with all allowed S3 subclasses
#'
#' @export
check_query_subclass <- function(s, subclasses) {

  if (!subclass(s) %in% subclasses)
    .error("Expecting %s query.",
           paste0("`", subclasses, "`", collapse = " or "))
}

#' @describeIn extensions
#' The \code{check_doc_subclass()} function specifies which type of query
#' objects (\code{RSTACDocument}) are expected in the function extension.
#'
#' @export
check_doc_subclass <- function(s, subclasses) {

  if (!subclass(s) %in% subclasses)
    .error("Expecting %s document(s).",
           paste0("`", subclasses, "`", collapse = " or "))
}
