#' @title Document development functions
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
#' @keywords internal
RSTACDocument <- function(content, q = NULL, subclass = NULL) {
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

  if (!all(subclass(x) %in% subclasses))
    .error("Expecting %s document(s).",
           paste0("`", subclasses, "`", collapse = " or "))
}

#' @title Document utils functions
#'
#' @param d an `RSTACDocument` object
#'
#' @return a `RSTACQuery` object with the predecessor subclass with the
#'  fields used in the request.
#'
#' @keywords internal
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
