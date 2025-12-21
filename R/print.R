#' @title Printing functions
#' @name print
#' @description The print function covers all objects in the rstac package:
#'
#' \itemize{
#' \item [stac()]: returns a `doc_catalog` document from
#'   `/stac` (v0.8.0) or `/` (v0.9.0 or v1.0.0) endpoint.
#' \item [stac_search()]: returns a `doc_items`
#'   document from `/stac/search` (v0.8.0) or `/search`
#'   (v0.9.0 or v1.0.0) endpoint containing all Items that match
#'   the provided search predicates.
#' \item [collections()]: implements the `/collections` and
#'   \code{/collections/\{collectionId\}} endpoints. The former returns
#'   a `doc_collections` document that lists all collections published
#'   by the server, and the later returns a single `doc_collection`
#'   document that describes a unique collection.
#' \item [items()]: retrieves a `doc_items` document
#'   from \code{/collections/\{collectionId\}/items} endpoint and a
#'   `doc_item` document from
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
#' @param x    either a `rstac_query` object expressing a STAC query
#' criteria or any `rstac_doc`.
#'
#' @param n    number of entries to print. Each object has its own rule of
#' truncation: the `doc_collection` objects will print
#' 10 links by default. If the object has less than 20 collections, all
#' collections will be shown. In `doc_items`, 10 features
#' will be printed by default. To show all entries, use `n = Inf`.
#'
#' @param ...  other parameters passed in the functions.
#'
#' @param tail A `logical` value indicating if last features in
#' doc_items object must be show.
#'
#' @seealso
#' [stac()] [stac_search()] [collections()]
#' [items()]
#'
#' @examples
#' \dontrun{
#'  # doc_items object
#'  stac_item_collection <-
#'    stac("https://data.inpe.br/bdc/stac/v1/") %>%
#'    stac_search(
#'      collections = "CBERS4-WFI-16D-2",
#'      bbox = c(-47.02148, -17.35063, -42.53906, -12.98314),
#'      limit = 15) %>%
#'    get_request()
#'
#'  print(stac_item_collection, n = 10)
#'
#'  # doc_collections object
#'  stac_collection <-
#'    stac("https://data.inpe.br/bdc/stac/v1/") %>%
#'    collections() %>%
#'    get_request()
#'
#'  print(stac_collection, n = 5)
#'
#'  # rstac_query object
#'  obj_rstac <- stac("https://data.inpe.br/bdc/stac/v1/")
#'
#'  print(obj_rstac)
#' }
NULL

# ---- rstac_query ----

#' @rdname print
#' @export
print.rstac_query <- function(x, ...) {
  cat(crayon::bold("###rstac_query"), fill = TRUE)
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

# ---- doc_catalog ----

#' @rdname print
#' @export
print.doc_catalog <- function(x, ...) {
  cat(crayon::bold("###Catalog"), fill = TRUE)
  cat("-", crayon::bold("id:"), x$id, fill = TRUE)
  if (!is.null(x$description) && x$description != "")
    cat("-", crayon::bold("description:"), x$description, fill = TRUE)

  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

# ---- doc_collections ----

#' @rdname print
#' @export
print.doc_collections <- function(x, n = 10, ...) {
  cat(crayon::bold("###Collections"), fill = TRUE)
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

# ---- doc_collection ----

#' @rdname print
#' @export
print.doc_collection <- function(x, ...) {
  cat(crayon::bold("###Collection"), fill = TRUE)
  cat("-", crayon::bold("id:"), x$id, fill = TRUE)
  if (!is.null(x$title) && x$title != "")
    cat("-", crayon::bold("title:"), x$title, fill = TRUE)
  if (!is.null(x$description) && x$description != "")
    cat("-", crayon::bold("description:"), x$description, fill = TRUE)
  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

# ---- doc_items ----

#' @rdname print
#' @export
print.doc_items <- function(x, n = 10, ..., tail = FALSE) {
  cat(crayon::bold("###Items"), fill = TRUE)
  matched <- suppressWarnings(items_matched(x))
  if (!is.null(matched)) {
    cat("-", crayon::bold("matched feature(s):"), matched, fill = TRUE)
    cat("-", crayon::bold("features"),
        sprintf("(%s item(s) / %s not fetched):",
                length(x$features), matched - length(x$features)), fill = TRUE)
  } else
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
      paste0(items_assets(x), collapse = ", "),
      fill = TRUE)
  cat("-", crayon::bold("item's fields:"),
      paste0(items_fields(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

# ---- doc_item ----

#' @rdname print
#' @export
print.doc_item <- function(x, ...) {
  cat(crayon::bold("###Item"), fill = TRUE)
  cat("-", crayon::bold("id:"), x$id, fill = TRUE)
  cat("-", crayon::bold("collection:"), x$collection, fill = TRUE)
  cat("-", crayon::bold("bbox:"), format_bbox(x$bbox), fill = TRUE)
  cat("-", crayon::bold("datetime:"), x$properties$datetime, fill = TRUE)
  cat("-", crayon::bold("assets:"),
      paste0(items_assets(x), collapse = ", "),
      fill = TRUE)
  cat("-", crayon::bold("item's fields:"),
      paste0(items_fields(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

# ---- doc_queryables ----

#' @rdname print
#' @export
print.doc_queryables <- function(x, n = 10, ...) {
  cat(crayon::bold("###Queryables"), fill = TRUE)
  if (missing(n) && length(x$properties) < 2 * n) {
    n <- length(x$properties)
  }
  n <- min(n, length(x$properties))
  cat("-", crayon::bold("properties"),
      sprintf("(%s entries(s)):", length(x$properties)), fill = TRUE)
  if (n > 0) {
    seq_it <- seq_len(n)
    for (i in seq_it) {
      e <- names(x$properties[i])
      cat(paste0("  - ", e), fill = TRUE)
    }
    if (n != length(x$properties))
      cat(sprintf("  - ... with %s more entry(ies).",
                  length(x$properties) - n), fill = TRUE)
  }
  cat("-", crayon::bold("field(s):"), paste0(names(x), collapse = ", "),
      fill = TRUE)
  invisible(x)
}

# ---- doc_conformance ----

#' @rdname print
#' @export
print.doc_conformance <- function(x, n = 10, ...) {
  cat(crayon::bold("###Conformance"), fill = TRUE)
  if (missing(n) && length(x$conformsTo) < 2 * n) {
    n <- length(x$conformsTo)
  }
  n <- min(n, length(x$conformsTo))
  cat("-", crayon::bold("conformances"),
      sprintf("(%s entries(s)):", length(x$conformsTo)), fill = TRUE)
  if (n > 0) {
    seq_it <- seq_len(n)
    for (i in seq_it) {
      e <- x$conformsTo[[i]]
      cat(paste0("  - ", e), fill = TRUE)
    }
    if (n != length(x$conformsTo))
      cat(sprintf("  - ... with %s more entry(ies).",
                  length(x$conformsTo) - n), fill = TRUE)
  }
  invisible(x)
}

# ---- Links ----

#' @rdname print
#' @export
print.doc_link <- function(x, ...) {
  cat(crayon::bold("###Link"), fill = TRUE)
  if ("title" %in% names(x))
    cat("-", crayon::bold(x$title), fill = TRUE)
  cat("-", crayon::bold("href:"), x$href, fill = TRUE)
  cat("-", crayon::bold("rel:"), x$rel, fill = TRUE)
  cat("-", crayon::bold("field(s):"),
      paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

#' @rdname print
#' @export
print.doc_links <- function(x, n = 10, ...) {
  cat(crayon::bold("###Links"), fill = TRUE)
  if (missing(n) && length(x) < 2 * n)
    n <- length(x)
  n <- min(n, length(x))
  cat("-", crayon::bold("links"),
      sprintf("(%s entries(s)):", length(x)), fill = TRUE)
  if (n > 0) {
    seq_it <- seq_len(n)
    seq_format <- format(seq_it, width = min(3, floor(log10(n)) + 1))
    for (i in seq_it) {
      if ("title" %in% names(x[[i]])) {
        cat(seq_format[[i]], crayon::bold(x[[i]]$title),
            paste0("(", x[[i]]$href, ")"), fill = TRUE)
      } else if ("rel" %in% names(x[[i]])) {
        cat(seq_format[[i]], crayon::bold(paste0("[", x[[i]]$rel, "]")),
            paste0("(", x[[i]]$href, ")"), fill = TRUE)
      } else
        cat(seq_format[[i]], paste0("(", x[[i]]$href, ")"), fill = TRUE)
    }
    if (n != length(x))
      cat(sprintf("  ... with %s more link(s).", length(x) - n), fill = TRUE)
  }
  invisible(x)
}

# ---- OpenAPI specification ----

#' @rdname print
#' @export
print.doc_openapi_specification <- function(x, n = 10, ...) {
  cat(crayon::bold("###OpenAPI Specification"), fill = TRUE)
  cat("-", crayon::bold("OpenAPI version:"), x$openapi, fill = TRUE) # "openapi" field is required
  cat("-", crayon::bold("API title:"), x$info$title, fill = TRUE) # "info" "title" field is required
  cat("-", crayon::bold("API version:"), x$info$version, fill = TRUE) # "info" "version" field is required

  if ("components" %in% names(x)) {
    if ("schemas" %in% names(x$components)) {
      if (missing(n) && length(x$components$schemas) < 2 * n) {
        n <- length(x$components$schemas)
      }
      n <- min(n, length(x$components$schemas))
      cat("-", crayon::bold("schemas"),
        sprintf("(%s entries(s)):", length(x$components$schemas)),
        fill = TRUE
      )
      if (n > 0) {
        seq_it <- seq_len(n)
        for (i in seq_it) {
          e <- names(x$components$schemas[i])
          cat(paste0("  - ", e), fill = TRUE)
        }
        if (n != length(x$components$schemas)) {
          cat(sprintf(
            "  - ... with %s more entry(ies).",
            length(x$components$schemas) - n
          ), fill = TRUE)
        }
      }
    }
  }

  cat("-", crayon::bold("field(s):"), paste0(names(x), collapse = ", "),
    fill = TRUE
  )

  invisible(x)
}
