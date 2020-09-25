#' @title Printing functions
#'
#' @name print
#'
#' @description The print functions cover all objects in the rstac package:
#' \itemize{
#'   \item \code{\link{stac}}: Returns a \code{STACCatalog} document in
#'    \code{/stac} (V0.8.0 or below) or \code{/} (V0.9.0 or above) endpoint.
#'   \item \code{\link{stac_search}}: Returns a \code{STACItemCollection}
#'    document in \code{/stac/search} (V0.8.0 or below) or
#'    \code{/search} (V0.9.0 or above)  endpoint with a group of Items matching
#'    the provided search predicates.
#'   \item \code{\link{collections}}: Return a \code{STACCollectionList}
#'   document by listing of collections contained in the catalog in
#'   \code{/collections} endpoint and in \code{/collections/\{collectionId\}}
#'   endpoint return a single \code{STACCollection} document.
#'   \item \code{\link{items}}: Return a \code{STACItemCollection} document in
#'   \code{/collections/\{collectionId\}/items} and a \code{STACItem} document
#'   in \code{/collections/\{collectionId\}/items/\{itemId\}} WFS3 endpoints.
#'  }
#'
#' The rstac package objects visualization is based on \strong{Markdown}, a
#'  lightweight markup language, so you can paste the output into any
#'  \strong{Markdown} editor for a better visualization.
#'
#' For printing use the \code{print()} function directly, since the package has
#'  a generic implementation for its objects. For console output control, you
#'  have the option to determine how many items you want to see through the
#'  \code{n} in \code{print} objects parameters, the following objects have the
#'  \code{n} parameter:
#'  \itemize{
#'   \item \code{\link{items}}
#'   \item \code{\link{collections}}
#'   \item \code{\link{stac}}
#'  }
#'
#' @param x either a \code{RSTACQuery} object expressing a STAC query
#' criteria or any \code{RSTACDocument}.
#'
#' @param n number of lines to view on each object. Each object has its own type
#'  of truncation in lines. In the \code{stac_collection} object, by default, 10
#'  links will be shown, but if the object has less than 20 collections, all
#'  the collections will be displayed. In \code{STACItemCollection}, by default,
#'  10 features will be shown. If you want to show all lines of a rstac object,
#'  use `n = Inf`.
#'
#' @param ... other parameters passed in the functions.
#'
#' @param tail To show the last lines of an object.
#'
#' @seealso
#' \code{\link{stac}} \code{\link{stac_search}} \code{\link{collections}}
#' \code{\link{items}}
#'
#' @examples
#' \donttest{
#'
#' # STACItemCollection object
#' stac_item_collection <-
#'   stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1",
#'          bbox = c(-47.02148, -12.98314, -42.53906, -17.35063),
#'          limit = 15) %>%
#'   get_request()
#'
#' print(stac_item_collection, n = 10)
#'
#' # STACCollectionList object
#' stac_collection <-
#'     stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
#'     collections() %>%
#'     get_request()
#'
#' print(stac_collection, n = 5)
#'
#' # RSTACQuery object
#' obj_rstac <- stac("http://brazildatacube.dpi.inpe.br/stac/")
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
    cat(paste0("  - ", n, ":"), fill = TRUE)
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
  # if (length(x$collections) > 0) cat(fill = TRUE)
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


#'
#' #### prints ####
#'
#' #' @title Printing functions
#' #' @rdname print
#' #' @export
#' print.stac <- function(x, ...) {
#'
#'   # print headers
#'   print_header(x)
#'
#'   # print body
#'   print_named(x, n = Inf, align_first = FALSE)
#' }
#'
#' # TODO: show IDS items and searching links by self
#'
#' #' @export
#' print.stac_collection_list <- function(x, n = 10, ...) {
#'
#'   # print header
#'   print_header(x)
#'
#'   titles <- sapply(x$collections, function(x){
#'     x$id
#'   })
#'
#'   hrefs <- sapply(x$collections, function(x){
#'     links <- Filter(function(e) e$rel == "self", x$links)
#'
#'     if (length(links) > 0)
#'       return(links[[1]]$href)
#'     return(NA)
#'   })
#'   print_link_highlight(titles, hrefs, pad = 0)
#' }
#'
#'
#' #' @title Printing functions
#' #' @rdname print
#' #' @export
#' print.stac_catalog <- function(x, n = 10, ...) {
#'
#'   # print headers
#'   print_header(x)
#'
#'   # links
#'   if (!is.null(x$links)) {
#'     links <- Filter(function(e) e$rel == "child", x$links)
#'
#'     if (length(links) > 0) {
#'       links_chunked <- links[1:min(n, length(links))]
#'       cat("- links:", fill = TRUE)
#'
#'       print_link_highlight(sapply(links_chunked, function(x){x$title}),
#'                            sapply(links_chunked, function(x){x$href}), pad = 2)
#'
#'       if (n < length(links))
#'         cat(crayon::silver(crayon::bold(sprintf("> \U2026 with %s more links",
#'                                                 length(links) - n))), fill = TRUE)
#'     }
#'   }
#' }
#'
#' #' @title Printing functions
#' #' @rdname print
#' #' @export
#' print.stac_collection <- function(x, n = 10, ...) {
#'
#'   # print headers
#'   print_header(x)
#'
#'   # properties
#'   if (!is.null(x$properties) && length(x$properties) > 0) {
#'     cat("- properties:")
#'     print_named(x$properties, n = Inf, pad = 2)
#'   }
#'
#'   # links
#'   if (!is.null(x$links)) {
#'     if (length(x$links) > 0) {
#'       cat("- links:", fill = TRUE)
#'       print_unnamed(x$links, n = n, pad = 2)
#'
#'       if (n < length(x$links))
#'         cat(crayon::silver(sprintf("> \U2026 with %s more links",
#'                                    length(x$links) - n)), fill = TRUE)
#'     }
#'   }
#' }
#'
#' #' @title Printing functions
#' #' @rdname print
#' #' @export
#' print.stac_item_collection <- function(x, n = 5, ...) {
#'
#'   # print headers
#'   print_header(x)
#'
#'   cat("- features:", fill = TRUE)
#'
#'   if (items_length(x) > 0) {
#'     feature <- x$features[seq_len(min(n, items_length(x)))]
#'
#'     print_unnamed(lapply(feature, function(x){
#'       list(collection = x$collection,
#'            bbox = format_bbox(x$bbox),
#'            datetime = x$properties$datetime)}), pad = 2)
#'   }
#'   if (n < items_length(x)) {
#'     cat(crayon::silver(sprintf("> \U2026 with %s more feature(s)",
#'                                items_length(x) - n)), fill = TRUE)
#'   }
#' }
#'
#' #' @title Printing functions
#' #' @rdname print
#' #' @export
#' print.stac_item <- function(x, ...){
#'
#'   # print headers
#'   print_header(x)
#'
#'   # properties
#'   if (!is.null(x$properties) && length(x$properties) > 0) {
#'     cat("- properties:")
#'     print_named(x$properties, n = Inf, pad = 2)
#'   }
#'
#'   if (!is.null(x$links)) {
#'     if (length(x$links) > 0) {
#'       cat("- links:", fill = TRUE)
#'       print_unnamed(x$links, n = Inf, pad = 2)
#'     }
#'   }
#'
#'   if (length(x$assets) > 0) {
#'     cat("- assets:", fill = TRUE)
#'
#'     print_link_highlight(names(x$assets),
#'                          unname(sapply(x$assets, function(x){x$href})), pad = 2)
#'   }
#' }
#'
#' #### helpers ####
#'
#' format_bbox <- function(bbox) {
#'
#'   if (!is.null(bbox) & length(bbox) == 4)
#'     names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
#'   else if (!is.null(bbox) & length(bbox) == 6)
#'     names(bbox) <- c("xmin", "ymin", "zmin", "xmax", "ymax", "zmax")
#'
#'   return(bbox)
#' }
#'
#' format_interval <- function(interval) {
#'
#'   interval <- lapply(interval, function(x){
#'     if (is.null(x[[1]]))
#'       x[[1]] <- ".."
#'     if (is.null(x[[2]]))
#'       x[[2]] <- ".."
#'
#'     x <- paste(x, collapse = "/")
#'   })
#'
#'   return(interval)
#' }
#'
#' print_data <- function(x, n, pad = 0, is_nested = FALSE, ...) {
#'
#'   if (is.character(x)) {
#'     if (is.null(names(x)))
#'       cat(paste0(sapply(x, function(e) crayon::bold(paste0('"', e, '"'))),
#'                  collapse = ", "), sep = "", fill = TRUE)
#'     else
#'       cat(paste(names(x), sapply(x, function(e) crayon::bold(paste0('"', e, '"'))),
#'                 collapse = ", ", sep = ": "), sep = "", fill = TRUE)
#'   } else if (is.integer(x)) {
#'     if (is.null(names(x)))
#'       cat(paste0(sapply(x, function(e) crayon::bold(e)),
#'                  collapse = ", "), sep = "", fill = TRUE)
#'     else
#'       cat(paste(names(x), sapply(x, function(e) crayon::bold(e)),
#'                 collapse = ", ", sep = ": "), sep = "", fill = TRUE)
#'   } else if (is.numeric(x)) {
#'     if (is.null(names(x)))
#'       cat(paste0(sapply(x, function(e) crayon::bold(sprintf("%.5f", e))),
#'                  collapse = ", "), sep = "", fill = TRUE)
#'     else
#'       cat(paste(names(x), sapply(x, function(e) crayon::bold(sprintf("%.5f", e))),
#'                 collapse = ", ", sep = ": "), sep = "", fill = TRUE)
#'   } else if (is.list(x)) {
#'     if (length(x) == 0)
#'       cat(fill = TRUE)
#'     else if (is.null(names(x)))
#'       print_unnamed(x, n = n, pad + 2, align_first = FALSE)
#'     else
#'       print_named(x, n = n, pad + 2, align_first = TRUE, is_nested = is_nested)
#'   } else
#'     cat("null", fill = TRUE)
#' }
#'
#' print_named <- function(x, n, pad = 0, align_first = FALSE, is_nested = FALSE) {
#'
#'   if (is.atomic(x)) {
#'     print_data(x)
#'     return()
#'   }
#'
#'   for (k in names(x)) {
#'     if (k == names(x)[[1]]) {
#'       if (align_first & !is_nested) {
#'         cat("- ", k, ": ", sep = "")
#'       } else {
#'         cat(fill = TRUE)
#'         cat(rep(" ", pad), "- ", k, ": ", sep = "")
#'       }
#'     }
#'     else if (align_first)
#'       cat("- ", k, ": ", sep = "")
#'     else
#'       cat(rep(" ", pad), "- ", k, ": ", sep = "")
#'
#'     print_data(x[[k]], n = n, pad = pad, align_first = FALSE, is_nested = TRUE)
#'     align_first = FALSE
#'   }
#' }
#'
#' print_unnamed <- function(x, n = 10, pad = 0, align_first = FALSE) {
#'
#'   for (i in seq_len(min(n, length(x)))) {
#'     if (align_first)
#'       cat("- ", sep = "")
#'     else
#'       cat(rep(" ", pad), "- ", sep = "")
#'
#'     print_data(x[[i]], n = n, pad = pad, align_first = TRUE)
#'     align_first = FALSE
#'   }
#' }
#'
#' print_extent <- function(x, pad) {
#'
#'   if (!is.null(x$spatial)) {
#'     cat(rep(" ", pad), "- spatial: ", sep = "", fill = TRUE)
#'     cat(rep(" ", pad + 2), "- bbox: ", sep = "", fill = TRUE)
#'
#'     print_data(lapply(x$spatial$bbox, format_bbox), n = Inf, pad = pad + 2)
#'   }
#'
#'   if (!is.null(x$temporal)) {
#'     cat(rep(" ", pad), "- temporal: ", sep = "", fill = TRUE)
#'     cat(rep(" ", pad + 2), "- interval: ", sep = "", fill = TRUE)
#'
#'     print_data(format_interval(x$temporal$interval), n = Inf, pad = pad + 2)
#'   }
#' }
#'
#' print_link_highlight <- function(titles, hrefs, pad = 2) {
#'
#'   for (i in seq_len(length(titles))) {
#'     cat(rep(" ", pad), "- ", sep = "")
#'     cat(crayon::bold(titles[[i]]),
#'         paste0('(', crayon::underline(hrefs[[i]]), ')'), sep = " ",
#'         fill = TRUE)
#'   }
#' }
