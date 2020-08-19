#' @title Extension functions
#'
#' @description
#' These functions are intended for those who want to implement new STAC API
#' extensions.
#'
#' Any extension that needs to handle the content of HTTP request params
#' (query string for GET and content-body for POST) before a GET (or a
#' POST) request is made must implement the \code{params_get_request} (or
#' the \code{params_post_request}) S3 generic method.
#'
#' Also, to make some check or data manipulation on the document returned by
#' a GET request (POST request) before give it back to the user, an extension
#' needs to implement the \code{content_get_response}
#' (\code{content_post_response}) S3 generic method.
#'
#' All these implemented methods will work 'behind the scenes' when a
#' \code{stac} object representing a user query of the extension are passed
#' to either \code{get_request} or \code{post_request} functions. This
#' object is returned by the \emph{extension exported function} and it can
#' be created by \code{.build_stac} function.
#'
#' An \emph{extension exported function} is typically a function that
#' receives as its first parameter an \code{stac} object (with derived
#' subclass or not), sets new or changes parameters on it, and returns a new
#' derived \code{stac} object with the extension's subclass. This object
#' can be either passed to others extensions or to HTTP request functions
#' (\code{get_request} and \code{post_request}). In the last case,
#' the S3 generic methods above will be triggered.
#'
#' For an example on how to implement an extension, see the \code{ext_query.R}
#' source file that implements the STAC API query extension.
#'
#' @param s       a \code{stac} object expressing a STAC search criteria
#' provided by the extension interacting function.
#'
#' @param enctype a \code{character} informing the request body
#' Content-Type. Accepted types are \code{'json'} (\code{'application/json'}),
#' \code{'form'} (\code{'application/x-www-form-urlencoded'}),
#' and \code{'multipart'} (\code{'multipart/form-data'}). Defaults to
#' \code{'json'}.
#'
#' @param res     a \code{httr} \code{response} object.
#'
#' @seealso \code{\link{extension_query}}, \code{\link{get_request}},
#' \code{\link{post_request}}, \code{\link{.build_stac}}
#'
#' @name extensions
params_get_request <- function(s) {

  UseMethod("params_get_request")
}

#' @rdname extensions
params_post_request <- function(s, enctype) {

  UseMethod("params_post_request")
}

#' @rdname extensions
content_get_response <- function(s, res) {

  UseMethod("content_get_response")
}

#' @rdname extensions
content_post_response <- function(s, res, enctype) {

  UseMethod("content_post_response")
}
