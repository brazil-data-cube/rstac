#' @title Printing functions
#' @name print
#' @description The print function covers all objects in the rstac package:
#'
#' \itemize{
#' \item [stac()]: returns a `STACCatalog` document from
#'   `/stac` (v0.8.0) or `/` (v0.9.0 or v1.0.0) endpoint.
#' \item [stac_search()]: returns a `STACItemCollection`
#'   document from `/stac/search` (v0.8.0) or `/search`
#'   (v0.9.0 or v1.0.0) endpoint containing all Items that match
#'   the provided search predicates.
#' \item [collections()]: implements the `/collections` and
#'   \code{/collections/\{collectionId\}} endpoints. The former returns
#'   a `STACCollectionList` document that lists all collections published
#'   by the server, and the later returns a single `STACCollection`
#'   document that describes a unique collection.
#' \item [items()]: retrieves a `STACItemCollection` document
#'   from \code{/collections/\{collectionId\}/items} endpoint and a
#'   `STACItem` document from
#'   \code{/collections/\{collectionId\}/items/\{itemId\}} endpoints.
#' }
#'
#' The rstac package objects visualization is based on markdown, a
#' lightweight markup language. You can paste the output into any
#' markdown editor for a better visualization.
#'
#' Call `print()` function to print the rstac's objects.
#' You can determine how many items will be printed using `n` parameter.
#'
#' @param x    either a `RSTACQuery` object expressing a STAC query
#' criteria or any `RSTACDocument`.
#'
#' @param n    number of entries to print. Each object has its own rule of
#' truncation: the `STACCollection` objects will print
#' 10 links by default. If the object has less than 20 collections, all
#' collections will be shown. In `STACItemCollection`, 10 features
#' will be printed by default. To show all entries, use `n = Inf`.
#'
#' @param ...  other parameters passed in the functions.
#'
#' @param tail A `logical` value indicating if last features in
#' STACItemCollection object must be show.
#'
#' @seealso
#' [stac()] [stac_search()] [collections()]
#' [items()]
#'
#' @examples
#' \dontrun{
#'  # STACItemCollection object
#'  stac_item_collection <-
#'    stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'    stac_search(collections = "CB4_64_16D_STK-1",
#'           bbox = c(-47.02148, -17.35063, -42.53906, -12.98314),
#'           limit = 15) %>%
#'    get_request()
#'
#'  print(stac_item_collection, n = 10)
#'
#'  # STACCollectionList object
#'  stac_collection <-
#'      stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'      collections() %>%
#'      get_request()
#'
#'  print(stac_collection, n = 5)
#'
#'  # RSTACQuery object
#'  obj_rstac <- stac("https://brazildatacube.dpi.inpe.br/stac/")
#'
#'  print(obj_rstac)
#' }
NULL

# ---- RSTACQuery ----

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

# ---- STACCatalog ----

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

# ---- STACCollectionList ----

#' @rdname print
#' @export
print.STACCollectionList <- function(x, n = 10, ...) {
  cat(crayon::bold("###STACCollectionList"), fill = TRUE)
  cat("-", crayon::bold("collections"),
      sprintf("(%s item(s)):", length(x$collections)), fill = TRUE)

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

# ---- STACCollection ----

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

# ---- STACItemCollection ----

#' @rdname print
#' @export
print.STACItemCollection <- function(x, n = 10, ..., tail = FALSE) {
  cat(crayon::bold("###STACItemCollection"), fill = TRUE)
  matched <- suppressWarnings(items_matched(x))
  if (!is.null(matched))
    cat("-", crayon::bold("matched feature(s):"), matched, fill = TRUE)

  if (!is.null(matched))
    cat("-", crayon::bold("features"),
        sprintf("(%s item(s) / %s not fetched):",
                length(x$features), matched - length(x$features)), fill = TRUE)
  else
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

  cat("-", crayon::bold("assets:"),
      paste0(items_assets(x, simplify = TRUE), collapse = ", "),
      fill = TRUE)
  cat("-", crayon::bold("other field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

# ---- STACItem ----

#' @rdname print
#' @export
print.STACItem <- function(x, ...) {
  cat(crayon::bold("###STACItem"), fill = TRUE)
  cat("-", crayon::bold("id:"), x$id, fill = TRUE)
  cat("-", crayon::bold("collection:"), x$collection, fill = TRUE)
  cat("-", crayon::bold("bbox:"), .format_bbox(x$bbox), fill = TRUE)
  cat("-", crayon::bold("datetime:"), x$properties$datetime, fill = TRUE)
  cat("-", crayon::bold("assets:"),
      paste0(items_assets(x, simplify = TRUE), collapse = ", "),
      fill = TRUE)
  cat("-", crayon::bold("other field(s):"), paste0(names(x), collapse = ", "),
      fill = TRUE)
  invisible(x)
}

# ---- Queryables ----

#' @rdname print
#' @export
print.Queryables <- function(x, n = 10, ...) {
  cat(crayon::bold("###Queryables"), fill = TRUE)

  if (missing(n) && length(x$properties) < 2 * n) {
    n <- length(x$properties)
  }
  n <- min(n, length(x$properties))
  if (n > 0) {
    seq_it <- seq_len(n)
    cat("-", crayon::bold("properties"), fill = TRUE)
    for (i in seq_it) {
      e <- names(x$properties[i])
      cat(paste0("  - ", e), fill = TRUE)
    }
  }
  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

# ---- Conformance ----

#' @rdname print
#' @export
print.Conformance <- function(x, n = 5, ...) {
  cat(crayon::bold("###Conformance"), fill = TRUE)

  if (missing(n) && length(x$conformsTo) < 2 * n) {
    n <- length(x$conformsTo)
  }
  n <- min(n, length(x$conformsTo))
  if (n > 0) {
    seq_it <- seq_len(n)
    cat("-", crayon::bold("conformsTo: "), fill = TRUE)
    for (i in seq_it) {
      e <- x$conformsTo[[i]]
      cat(paste0("  - ", e), fill = TRUE)
    }
  }
  invisible(x)
}
