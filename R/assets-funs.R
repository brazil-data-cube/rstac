#' @title Assets functions
#'
#' @description
#' These functions provide support to work with `doc_items` and
#' `doc_item` item objects.
#'
#' \itemize{
#' \item `assets_download()`: Downloads the assets provided by the STAC API.
#'
#' \item `assets_url()`: `r lifecycle::badge('experimental')` Returns a
#'  character vector with each asset href. For the URL, you can add the
#'  GDAL library drivers for the following schemes: HTTP/HTTPS files,
#'  S3 (AWS S3) and GS (Google Cloud Storage).
#'
#' \item `assets_select()`: `r lifecycle::badge('experimental')` Selects the
#'  assets of each item by its name (`asset_names` parameter), by expressions
#'  (`...` parameter), or by a selection function (`select_fn` parameter).
#'  Note: This function can produce items with empty assets. In this case,
#'  users can use the `items_compact()` function to remove items with no
#'  assets.
#'
#' \item `assets_rename()`: `r lifecycle::badge('experimental')` Rename each
#'  asset using a named list or a function.
#' }
#'
#' @param items       a `doc_item` or `doc_items` object
#' representing the result of `/stac/search`,
#' \code{/collections/{collectionId}/items} or
#' \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @param asset_names a `character` vector with the names of the assets
#' to be selected.
#'
#' @param output_dir  a `character` directory in which the assets will be
#' saved. Default is the working directory (`getwd()`)
#'
#' @param overwrite   a `logical` if TRUE will replace the existing file,
#' if FALSE, a warning message is shown.
#'
#' @param items_max   a `numeric` corresponding to how many items will be
#' downloaded.
#'
#' @param progress    a `logical` indicating if a progress bar must be
#' shown or not. Defaults to `TRUE`.
#'
#' @param use_gdal    a `logical` indicating if the file should be downloaded
#' by GDAL instead httr package.
#'
#' @param download_fn a `function` to handle download of assets for
#' each item to be downloaded. Using this function, you can change the
#' hrefs for each asset, as well as the way download is done.
#'
#' @param append_gdalvsi a `logical` value. If true, gdal drivers are
#' included in the URL of each asset. The following schemes are supported:
#' HTTP/HTTPS files, S3 (AWS S3) and GS (Google Cloud Storage).
#'
#' @param create_json a `logical` indicating if a JSON file with item
#' metadata (`doc_item` or `doc_items`) must be created in the
#' output directory.
#'
#' @param select_fn a `function` to select assets an item
#' (`doc_item` or `doc_items`). This function receives as parameter
#' the asset element and, optionally, the asset name. Asset elements
#' contain metadata describing spatial-temporal objects. Users can provide
#' a function to select assets based on this metadata by returning a
#' logical value where `TRUE` selects the asset, and `FALSE` discards it.
#'
#' @param mapper      either a named `list` or a `function` to rename assets
#' of an item (`doc_item` or `doc_items`). In the case of a named
#' list, use `<old name> = <new name>` to rename the assets. The function
#' can be used to rename the assets by returning a `character` string using
#' the metadata contained in the asset object.
#'
#' @param field       a `character` with the name of the asset field to
#' return.
#'
#' @param ...         additional arguments. See details.
#'
#' @details
#' Ellipsis argument (`...`) appears in different assets functions and
#' has distinct purposes:
#' \itemize{
#' \item `assets_download()`: ellipsis is used to pass
#' additional `httr` options to [GET][httr::GET] or [POST][httr::POST]
#' methods, such as [add_headers][httr::add_headers] or
#' [set_cookies][httr::set_cookies].
#'
#' \item `assets_select()`: ellipsis is used to pass expressions that will
#' be evaluated against each asset metadata. Expressions must be evaluated as
#' a logical value where `TRUE` selects the asset and `FALSE` discards it.
#' Multiple expressions are combine with `AND` operator. Expressions can
#' use `asset` helper functions (i.e. `asset_key()`, `asset_eo_bands()`,
#' and `asset_raster_bands()`). Multiple expressions are combined with
#' `AND` operator. `assets_select()` uses non-standard evaluation to evaluate
#' its expressions. That means users must escape any variable or call to
#' be able to use them in the expressions. The escape is done by using
#' `double-curly-braces`, i.e., `{{variable}}`.
#'
#' **WARNING:** Errors in the evaluation of expressions are
#' considered as `FALSE`.
#'
#' \item `assets_rename()`: ellipsis is used to pass named parameters
#' to be processed in the same way as the named list in `mapper` argument.
#' }
#'
#' @return
#'
#' \itemize{
#' \item `assets_download()`: returns the same input object item
#' (`doc_item` or `doc_items`) where `href` properties point to
#' the download assets.
#'
#' \item `assets_url()`: returns a character vector with all assets `href`
#' of an item (`doc_item` or `doc_items`).
#'
#' \item `assets_select()`: returns the same input object item
#' (`doc_item` or `doc_items`) with the selected assets.
#'
#' \item `assets_rename()`: returns the same input object item
#' (`doc_items` or `doc_item`) with the assets renamed.
#' }
#'
#' @examples
#' \dontrun{
#'  # assets_download function
#'  stac("https://data.inpe.br/bdc/stac/v1/") %>%
#'    stac_search(collections = "CBERS4-WFI-16D-2",
#'                datetime = "2019-06-01/2019-08-01") %>%
#'    stac_search() %>%
#'    get_request() %>%
#'    assets_download(asset_names = "thumbnail", output_dir = tempdir())
#' }
#'
#' \dontrun{
#'  # assets_url function
#'  stac_item <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
#'    stac_search(
#'      collections = "CBERS4-WFI-16D-2",
#'      limit = 100,
#'      datetime = "2017-08-01/2018-03-01",
#'      bbox = c(-48.206,-14.195,-45.067,-12.272)) %>%
#'    get_request() %>% items_fetch(progress = FALSE)
#'
#'  stac_item %>% assets_url()
#' }
#'
#' \dontrun{
#'  # assets_select function
#'  stac_item <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
#'    stac_search(
#'      collections = "CBERS4-WFI-16D-2",
#'      limit = 100,
#'      datetime = "2017-08-01/2018-03-01",
#'      bbox = c(-48.206,-14.195,-45.067,-12.272)) %>%
#'    get_request() %>% items_fetch(progress = FALSE)
#'
#'  stac_item %>% assets_select(asset_names = "NDVI")
#' }
#'
#' \dontrun{
#' items <- stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
#'   stac_search(collections = c("landsat-8-c2-l2", "sentinel-2-l2a"),
#'               bbox = c(xmin = -64.85976089, ymin = -10.49199395,
#'                        xmax = -64.79272527, ymax =-10.44736091),
#'               datetime = "2019-01-01/2019-06-28",
#'               limit = 50) %>%
#'   post_request()
#'
#' # Selects assets by name
#' items <- assets_select(items,
#'                        asset_names = c("B02", "B03", "SR_B1", "SR_B2"))
#' # Renames the landsat assets
#' items <- assets_rename(items,
#'                        SR_B1 = "blue",
#'                        SR_B2 = "green",
#'                        B02   = "blue",
#'                        B03   = "green")
#' # Get the assets url's
#' assets_url(items)
#' }
#'
#' @seealso
#' [stac_search()], [items()], [get_request()]
#'
#' @name assets_functions
NULL

#' @rdname assets_functions
#'
#' @export
assets_download <- function(items,
                            asset_names = NULL,
                            output_dir = getwd(),
                            overwrite = FALSE, ...,
                            use_gdal = FALSE,
                            download_fn = NULL) {
  # check output dir
  if (!dir.exists(output_dir))
    .error(paste("The directory provided does not exist.",
                 "Please, provide an existing directory."))
  UseMethod("assets_download", items)
}

#' @rdname assets_functions
#'
#' @export
assets_download.doc_item <- function(items,
                                     asset_names = NULL,
                                     output_dir = getwd(),
                                     overwrite = FALSE, ...,
                                     use_gdal = FALSE,
                                     create_json = FALSE,
                                     download_fn = NULL) {
  if (!is.null(asset_names)) {
    in_assets <- asset_names %in% items_assets(items)
    if (!all(asset_names %in% items_assets(items))) {
      .warning("Item does not have asset(s) '%s'.",
               paste(asset_names[!in_assets], collapse = ", "))
    }
    items <- assets_select(items = items, asset_names = asset_names)
  }
  items$assets <- lapply(
    items$assets, asset_download, output_dir = output_dir,
    overwrite = overwrite, use_gdal = use_gdal, download_fn = download_fn, ...
  )
  if (create_json) {
    file <- "item.json"
    if ("id" %in% names(items)) {
      file <- paste0(items$id, ".json")
    }
    cat(to_json(items), file = file.path(output_dir, file))
  }
  items
}

#' @rdname assets_functions
#'
#' @export
assets_download.doc_items <- function(items,
                                      asset_names = NULL,
                                      output_dir = getwd(),
                                      overwrite = FALSE, ...,
                                      use_gdal = FALSE,
                                      download_fn = NULL,
                                      create_json = TRUE,
                                      items_max = Inf,
                                      progress = TRUE) {
  # remove empty items
  items <- items_compact(items)
  items_max <- max(0, min(items_length(items), items_max))
  # verify if progress bar can be shown
  progress <- progress && items_max > 1
  if (progress) {
    pb <- utils::txtProgressBar(max = items_max, style = 3)
    # close progress bar when exit
    on.exit(if (progress) close(pb))
  }
  items$features <- items$features[seq_len(items_max)]
  for (i in seq_len(items_max)) {
    if (progress)
      utils::setTxtProgressBar(pb, i)
    items$features[[i]] <- assets_download(
      items = items$features[[i]], asset_names = asset_names,
      output_dir = output_dir, overwrite = overwrite,
      use_gdal = use_gdal, create_json = FALSE, download_fn = download_fn, ...
    )
  }
  if (create_json)
    cat(to_json(items), file = file.path(output_dir, "items.json"))
  items
}

#' @rdname assets_functions
#'
#' @export
assets_download.default <- assets_download.doc_item

#' @rdname assets_functions
#'
#' @export
assets_url <- function(items, asset_names = NULL, append_gdalvsi = FALSE) {
  UseMethod("assets_url", items)
}

#' @rdname assets_functions
#'
#' @export
assets_url.doc_item <- function(items,
                                asset_names = NULL,
                                append_gdalvsi = FALSE) {
  if (is.null(asset_names)) {
    asset_names <- items_assets(items)
  }
  url <- unlist(lapply(asset_names, function(asset_name) {
    items <- assets_select(items = items, asset_names = asset_name)

    is_empty <- !has_assets(items)
    if (is_empty) {
      .warning("Item does not have asset '%s'.", asset_name)
    }
    return(items$assets[[asset_name]]$href)
  }))
  if (append_gdalvsi) {
    url <- gdalvsi_append(url)
  }
  return(url)
}

#' @rdname assets_functions
#'
#' @export
assets_url.doc_items <- function(items,
                                 asset_names = NULL,
                                 append_gdalvsi = FALSE) {
  if (is.null(asset_names)) {
    asset_names <- items_assets(items)
  }
  url <- unlist(lapply(asset_names, function(asset_name) {
    items <- assets_select(items = items, asset_names = asset_name)

    has_empty <- !all(has_assets(items))
    if (has_empty) {
      items <- items_compact(items)
      .warning("Some items does not have asset name '%s'.", asset_name)
    }
    return(items_reap(items, field = c("assets", asset_name, "href")))
  }))
  if (append_gdalvsi) {
    url <- gdalvsi_append(url)
  }
  return(url)
}

#' @rdname assets_functions
#'
#' @export
assets_url.default <- assets_url.doc_item

#' @rdname assets_functions
#'
#' @export
assets_select <- function(items, ..., asset_names = NULL, select_fn = NULL) {
  UseMethod("assets_select", items)
}

#' @rdname assets_functions
#'
#' @export
assets_select.doc_item <- function(items, ...,
                                   asset_names = NULL,
                                   select_fn = NULL) {
  exprs <- as.list(substitute(list(...), env = environment()))[-1]
  init_length <- length(items$assets)
  if (!is.null(asset_names)) {
    asset_names <- intersect(names(items$assets), asset_names)
    items$assets <- items$assets[asset_names]
  }
  if (length(exprs) > 0) {
    if (!is.null(names(exprs)))
      .error("Select expressions cannot be named.")
    for (expr in exprs) {
      expr <- unquote(expr = expr, env =  parent.frame())
      sel <- map_lgl(names(items$assets), function(key) {
        select_eval(key = key, asset = items$assets[[key]], expr = expr)
      })
      items$assets <- items$assets[sel]
    }
  }
  if (!is.null(select_fn)) {
    sel <- map_lgl(names(items$assets), function(key) {
      val <- select_exec(key = key, asset = items$assets[[key]],
                         select_fn = select_fn)
      return(val)
    })
    items$assets <- items$assets[sel]
  }
  if (length(items$assets) == 0 && init_length > 0)
    .warning(paste("Filter criteria did not match any asset.\n",
                   "Please, see `?assets_select` for more details on",
                   "how expressions are evaluated by `assets_select()`."))
  items
}

#' @rdname assets_functions
#'
#' @export
assets_select.doc_items <- function(items, ...,
                                    asset_names = NULL,
                                    select_fn = NULL) {
  items <- foreach_item(
    items, assets_select, asset_names = asset_names, ...,
    select_fn = select_fn
  )
  return(items)
}

#' @rdname assets_functions
#'
#' @export
assets_select.default <- assets_select.doc_item

#' @rdname assets_functions
#'
#' @export
assets_rename <- function(items, mapper = NULL, ...) {
  UseMethod("assets_rename", items)
}

#' @rdname assets_functions
#'
#' @export
assets_rename.doc_item <- function(items, mapper = NULL, ...) {
  dots <- list(...)
  if (is.function(mapper)) {
    new_names <- as.list(map_chr(items$assets, mapper, use_names = TRUE))
  } else {
    new_names <- as.list(mapper)
  }
  if (length(dots) > 0) {
    new_names <- modify_list(new_names, dots)
  }
  new_names <- unlist(new_names)
  if (length(new_names) == 0) {
    .error("Parameters `mapper` or `...` must be informed.")
  }
  if (!all(nzchar(names(new_names)))) {
    .error("Parameters `mapper` and `...` must contains named values.")
  }
  asset_names <- names(items$assets)
  # remove empty names
  new_names <- new_names[nzchar(new_names)]
  asset_names[asset_names %in% names(new_names)] <-
    unname(new_names[asset_names[asset_names %in% names(new_names)]])
  names(items$assets) <- asset_names

  return(items)
}

#' @rdname assets_functions
#'
#' @export
assets_rename.doc_items <- function(items, mapper = NULL, ...) {
  return(foreach_item(items, assets_rename, mapper = mapper, ...))
}

#' @rdname assets_functions
#'
#' @export
assets_rename.default <- assets_rename.doc_item

#' @rdname assets_functions
#'
#' @export
has_assets <- function(items) {
  UseMethod("has_assets", items)
}

#' @rdname assets_functions
#'
#' @export
has_assets.doc_item <- function(items) {
  length(items$assets) > 0
}

#' @rdname assets_functions
#'
#' @export
has_assets.doc_items <- function(items) {
  map_lgl(items$features, has_assets)
}

#' @rdname assets_functions
#'
#' @export
has_assets.default <- has_assets.doc_item

#' @rdname assets_functions
#'
#' @export
asset_key <- function() {
  if (!"key" %in% names(asset_context))
    .error("Must be used inside `assets_select()`")
  asset_context$key
}

#' @rdname assets_functions
#'
#' @export
asset_eo_bands <- function(field) {
  val <- asset_get("eo:bands")[[1]]
  if (missing(field))
    return(val)
  val[[field]]
}

#' @rdname assets_functions
#'
#' @export
asset_raster_bands <- function(field) {
  val <- asset_get("raster:bands")[[1]]
  if (missing(field))
    return(val)
  val[[field]]
}
