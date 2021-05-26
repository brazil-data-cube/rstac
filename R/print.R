#' @title Printing functions
#' @name print
#' @description The print function covers all objects in the rstac package:
#'
#' \itemize{
#' \item \code{\link{stac}}: returns a \code{STACCatalog} document from
#'   \code{/stac} (v0.8.0) or \code{/} (v0.9.0 or v1.0.0) endpoint.
#' \item \code{\link{stac_search}}: returns a \code{STACItemCollection}
#'   document from \code{/stac/search} (v0.8.0) or \code{/search}
#'   (v0.9.0 or v1.0.0) endpoint containing all Items that match
#'   the provided search predicates.
#' \item \code{\link{collections}}: implements the \code{/collections} and
#'   \code{/collections/\{collectionId\}} endpoints. The former returns
#'   a \code{STACCollectionList} document that lists all collections published
#'   by the server, and the later returns a single \code{STACCollection}
#'   document that describes a unique collection.
#' \item \code{\link{items}}: retrieves a \code{STACItemCollection} document
#'   from \code{/collections/\{collectionId\}/items} endpoint and a
#'   \code{STACItem} document from
#'   \code{/collections/\{collectionId\}/items/\{itemId\}} endpoints.
#' }
#'
#' The rstac package objects visualization is based on markdown, a
#' lightweight markup language. You can paste the output into any
#' markdown editor for a better visualization.
#'
#' Call \code{print()} function to print the rstac's objects.
#' You can determine how many items will be printed using \code{n} parameter.
#'
#' @param x    either a \code{RSTACQuery} object expressing a STAC query
#' criteria or any \code{RSTACDocument}.
#'
#' @param n    number of entries to print. Each object has its own rule of
#' truncation: the \code{STACCollection} objects will print
#' 10 links by default. If the object has less than 20 collections, all
#' collections will be shown. In \code{STACItemCollection}, 10 features
#' will be printed by default. To show all entries, use \code{n = Inf}.
#'
#' @param ...  other parameters passed in the functions.
#'
#' @param tail A \code{logical} value indicating if last features in
#' STACItemCollection object must be show.
#'
#' @seealso
#' \code{\link{stac}} \code{\link{stac_search}} \code{\link{collections}}
#' \code{\link{items}}
#'
#' @examples
#' \donttest{
#' # STACItemCollection object
#' stac_item_collection <-
#'   stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1",
#'          bbox = c(-47.02148, -12.98314, -42.53906, -17.35063),
#'          limit = 15) %>%
#'   get_request()
#'
#' print(stac_item_collection, n = 10)
#'
#' # STACCollectionList object
#' stac_collection <-
#'     stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'     collections() %>%
#'     get_request()
#'
#' print(stac_collection, n = 5)
#'
#' # RSTACQuery object
#' obj_rstac <- stac("https://brazildatacube.dpi.inpe.br/stac/")
#'
#' print(obj_rstac)
#' }
NULL

####RSTACQuery####

#' @title Printing functions
#' @rdname print
#' @export
print.RSTACQuery <- function(x, ...) {

  cat(crayon::bold("###RSTACQuery"), fill = TRUE)
  cat("-", crayon::bold("url:"), x$base_url, fill = TRUE)
  cat("-", crayon::bold("params:"), fill = TRUE)
  for (n in names(x$params)) {
    cat(paste0("  - ", n, ": ", paste(x$params[[n]], collapse = ",")),
        fill = TRUE)
  }
  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

####STACCatalog####

#' @title Printing functions
#' @rdname print
#' @export
print.STACCatalog <- function(x, ...) {

  cat(crayon::bold("###STACCatalog"), fill = TRUE)
  cat("-", crayon::bold("id:"), x$id, fill = TRUE)
  if (!is.null(x$description) && x$description != "")
    cat("-", crayon::bold("description:"), x$description, fill = TRUE)

  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

####STACCollectionList####

#' @title Printing functions
#' @rdname print
#' @export
print.STACCollectionList <- function(x, n = 10, ...) {

  cat(crayon::bold("###STACCollectionList"), fill = TRUE)
  cat("-", crayon::bold("collections"),
      sprintf("(%s item(s)):", length(x$collections)), fill = TRUE)
  # if (length(x$collections) > 0) cat(fill = TRUE)
  if (missing(n) && length(x$collections) < 2 * n)
    n <- length(x$collections)
  n <- min(n, length(x$collections))
  for (i in seq_len(n)) {
    e <- x$collections[[i]]
    cat(paste0("  - ", e$id), fill = TRUE)
  }
  if (n != length(x$collections))
    cat(sprintf("  - ... with %s more collection(s).",
                length(x$collections) - n), fill = TRUE)
  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

####STACCollection####

#' @title Printing functions
#' @rdname print
#' @export
print.STACCollection <- function(x, ...) {

  cat(crayon::bold("###STACCollection"), fill = TRUE)
  cat("-", crayon::bold("id:"), x$id, fill = TRUE)
  if (!is.null(x$title) && x$title != "")
    cat("-", crayon::bold("title:"), x$title, fill = TRUE)
  if (!is.null(x$description) && x$description != "")
    cat("-", crayon::bold("description:"), x$description, fill = TRUE)
  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

####STACItemCollection####

#' @title Printing functions
#' @rdname print
#' @export
print.STACItemCollection <- function(x, n = 10, ..., tail = FALSE) {

  cat(crayon::bold("###STACItemCollection"), fill = TRUE)
  matched <- suppressWarnings(items_matched(x))
  if (!is.null(matched))
    cat("-", crayon::bold("matched feature(s):"), matched, fill = TRUE)
  cat("-", crayon::bold("features"),
      sprintf("(%s item(s)):", length(x$features)), fill = TRUE)
  if (missing(n) && length(x$features) < 2 * n)
    n <- length(x$features)
  n <- min(n, length(x$features))

  seq_it <- seq_len(n)
  if (tail)
    seq_it <- seq.int(to = length(x$features), length.out = n)

  for (i in seq_it) {
    e <- x$features[[i]]
    cat(paste0("  - ", e$id), fill = TRUE)
  }
  if (n != length(x$features))
    cat(sprintf("  - ... with %s more feature(s).",
                length(x$features) - n), fill = TRUE)
  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

####STACItem####

#' @title Printing functions
#' @rdname print
#' @export
print.STACItem <- function(x, ...) {

  cat(crayon::bold("###STACItem"), fill = TRUE)
  cat("-", crayon::bold("id:"), x$id, fill = TRUE)
  cat("-", crayon::bold("collection:"), x$collection, fill = TRUE)
  cat("-", crayon::bold("bbox:"), .format_bbox(x$bbox), fill = TRUE)
  cat("-", crayon::bold("datetime:"), x$properties$datetime, fill = TRUE)
  cat("-", crayon::bold("assets:"),
      paste0("'", names(x$assets), "'", collapse = ", "), fill = TRUE)
  cat("-", crayon::bold("field(s):"), paste0(names(x), collapse = ", "),
      fill = TRUE)
  invisible(x)
}
