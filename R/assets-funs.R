#' @title Assets functions
#'
#' @description
#' These functions provide support to work with `STACItemCollection` and
#' `STACItem` objects.
#'
#' \itemize{
#' \item `assets_download()`: Downloads the assets provided by the STAC API.
#'
#' \item `assets_url()`: `r lifecycle::badge('experimental')` Returns a list
#'  with href of each feature.
#'  For the URL you can add the GDAL library drivers for the following schemes:
#'  HTTP/HTTPS files, S3 (AWS S3) and GS (Google Cloud Storage).
#'
#' \item `assets_select()`: `r lifecycle::badge('experimental')` Selects the
#'  assets of each feature by its name.
#'
#' \item `assets_rename()`: `r lifecycle::badge('experimental')` Rename each
#'  asset feature name by an expression or a function.
#' }
#'
#' @param items       a `STACItem` or `STACItemCollection` object
#'  representing the result of `/stac/search`,
#'  \code{/collections/{collectionId}/items} or
#'  \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @param asset_names a `character` vector with the assets names to be selected.
#'
#' @param output_dir  a `character` directory in which the assets will be
#'  saved. Default is the working directory (`getwd()`)
#'
#' @param overwrite   a `logical` if TRUE will replaced the existing file,
#'  if FALSE a warning message is shown.
#'
#' @param items_max   a `numeric` corresponding how many items will be
#'  downloaded.
#'
#' @param progress    a `logical` indicating if a progress bar must be
#'  shown or not. Defaults to `TRUE`.
#'
#' @param download_fn a `function` to handle the list of assets for each item.
#'  Using this function you can change the hrefs for each asset, as well as use
#'  another request verb, such as POST.
#'
#' @param fn          `r lifecycle::badge('deprecated')`
#'  use `download_fn` parameter instead.
#'
#' @param append_gdalvsi a `logical`  if true, gdal drivers are
#'  included in the URL of each asset. The following schemes are supported:
#'  HTTP/HTTPS files, S3 (AWS S3) and GS (Google Cloud Storage).
#'
#' @param filter_fn   a `function` that will be used to filter the
#'  attributes listed in the properties.
#'
#' @param keep_empty_items a `logical` indicating if empty items should be
#'  kept.
#'
#' @param names_fn a `function` used to produce assets names from an
#'  asset object.
#'
#' @param ...      config parameters to be passed to [GET][httr::GET],
#'  such as [add_headers][httr::add_headers] or
#'  [set_cookies][httr::set_cookies]. Used in `assets_download` function.
#'
#' @return
#'
#' \itemize{
#' \item `assets_download()`: returns the same object as the provided item
#' (`STACItemCollection` or `STACItem`), however the `href` property points to
#' the location where the asset was downloaded
#'
#' \item `assets_url()`: returns a character vector with `href` of each feature.
#'
#' \item `assets_select()`: returns the same object as the provided item
#' (`STACItemCollection` or `STACItem`), however with the selected assets.
#'
#' \item `assets_rename()`: returns the same object as the provided item
#' (`STACItemCollection` or `STACItem`), however with the assets renamed.
#' }
#'
#' @examples
#' \dontrun{
#'  # assets_download function
#'  stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'    stac_search(collections = "CB4_64_16D_STK-1",
#'                datetime = "2019-06-01/2019-08-01") %>%
#'    stac_search() %>%
#'    get_request() %>%
#'    assets_download(asset_names = "thumbnail", output_dir = tempdir())
#' }
#'
#' \dontrun{
#'  # assets_url function
#'  stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1", limit = 100,
#'          datetime = "2017-08-01/2018-03-01",
#'          bbox = c(-48.206,-14.195,-45.067,-12.272)) %>%
#'   get_request() %>% items_fetch(progress = FALSE)
#'
#'  stac_item %>% assets_url()
#' }
#'
#' \dontrun{
#'  # assets_select function
#'  stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1", limit = 100,
#'          datetime = "2017-08-01/2018-03-01",
#'          bbox = c(-48.206,-14.195,-45.067,-12.272)) %>%
#'   get_request() %>% items_fetch(progress = FALSE)
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
#' items <- assets_select(items, c("B02", "B03", "SR_B1", "SR_B2"))
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
#' @name assets_function
NULL

#' @rdname assets_function
#'
#' @export
assets_download <- function(items,
                            asset_names = NULL,
                            output_dir = getwd(),
                            overwrite = FALSE,
                            items_max = Inf,
                            progress = TRUE,
                            download_fn = NULL, ...,
                            fn = deprecated()) {

  # check output dir
  if (!dir.exists(output_dir))
    .error(paste("The directory provided does not exist.",
                 "Please specify a valid directory."))

  UseMethod("assets_download", items)
}

#' @rdname assets_function
#'
#' @export
assets_download.STACItem <- function(items,
                                     asset_names = NULL,
                                     output_dir = ".",
                                     overwrite = FALSE,
                                     items_max = Inf,
                                     progress  = TRUE,
                                     download_fn = NULL, ...,
                                     fn = deprecated()) {
  if (!missing(fn)) {
    deprec_parameter(
      deprec_var = "fn",
      deprec_version = "0.9.1-6",
      msg = "Please, use `download_fn` parameter instead."
    )
    download_fn <- fn
  }

  if (!is.null(asset_names))
    items <- assets_select(items = items, asset_names = asset_names)

  items <- asset_download(
    item = items,
    output_dir = output_dir,
    overwrite = overwrite,
    fn = download_fn, ...
  )

  return(items)
}

#' @rdname assets_function
#'
#' @export
assets_download.STACItemCollection <- function(items,
                                               asset_names = NULL,
                                               output_dir = ".",
                                               overwrite = FALSE,
                                               items_max = Inf,
                                               progress = TRUE,
                                               download_fn = NULL, ...,
                                               fn = deprecated()) {
  if (!missing(fn)) {
    deprec_parameter(
      deprec_var = "fn",
      deprec_version = "0.9.1-6",
      msg = "Please, use `download_fn` parameter instead."
    )
    download_fn <- fn
  }

  if (!is.null(asset_names))
    items <- assets_select(items = items, asset_names = asset_names)

  # check if items length corresponds with items matched
  if (!missing(items_max)) {

    if (items_max > items_length(items))
      items_max <- .parse_items_size(items)
  } else {

    items_max <- .parse_items_size(items)
  }

  # verify if progress bar can be shown
  progress <- progress && (!is.null(items_max)) && items_max > 1

  if (progress)
    pb <- utils::txtProgressBar(min = 0, max = items_max, style = 3, width = 50)

  for (i in seq_len(items_max)) {

    if (progress)
      utils::setTxtProgressBar(pb, i)

    items$features[[i]] <- asset_download(
      item = items$features[[i]],
      output_dir = output_dir,
      overwrite = overwrite,
      fn = download_fn, ...
    )
  }

  # close progress bar
  if (progress)
    close(pb)

  return(items)
}

#' @rdname assets_function
#'
#' @export
assets_url <- function(items, asset_names = NULL, append_gdalvsi = TRUE) {
  UseMethod("assets_url", items)
}

#' @rdname assets_function
#'
#' @export
assets_url.STACItem <- function(items,
                                asset_names = NULL,
                                append_gdalvsi = TRUE) {
  assets_url.STACItemCollection(items = items,
                                asset_names = asset_names,
                                append_gdalvsi = append_gdalvsi)
}

#' @rdname assets_function
#'
#' @export
assets_url.STACItemCollection <- function(items,
                                          asset_names = NULL,
                                          append_gdalvsi = TRUE) {
  if (is.null(asset_names)) {
    asset_names <- items_assets(items)
  }
  url <- unlist(lapply(asset_names, function(asset_name) {
    items <- assets_select(items = items,
                           asset_names = asset_name,
                           keep_empty_items = TRUE)

    empty_items <- items_empty_assets(items)
    if (any(empty_items)) {
      items[["features"]] <- items[["features"]][!empty_items]
      .warning("Some items does not have asset name '%s'.", asset_name)
    }
    return(items_reap(items, field = c("assets", asset_name, "href")))
  }))
  if (append_gdalvsi) {
    url <- gdalvsi_append(url)
  }
  return(url)
}

#' @rdname assets_function
#'
#' @export
assets_select <- function(items, asset_names = NULL, filter_fn = NULL, ...) {
  UseMethod("assets_select", items)
}

#' @rdname assets_function
#'
#' @export
assets_select.STACItem <- function(items,
                                   asset_names = NULL,
                                   filter_fn = NULL, ...) {
  if (!is.null(asset_names)) {
    assets_in_items <- asset_names %in% items_assets(items)
    if (!all(assets_in_items))
      .error("There is no item with asset(s) '%s'.",
             paste(asset_names[!assets_in_items], collapse = ", "))
    items[["assets"]] <- items[["assets"]][asset_names]
  }

  if (!is.null(filter_fn)) {
    sel <- vapply(items[["assets"]], filter_fn, logical(1))
    items[["assets"]] <- items[["assets"]][sel]
  }

  return(items)
}

#' @rdname assets_function
#'
#' @export
assets_select.STACItemCollection <- function(items,
                                             asset_names = NULL,
                                             filter_fn = NULL,
                                             keep_empty_items = TRUE) {
  if (!is.null(asset_names)) {
    assets_in_items <- asset_names %in% items_assets(items)
    if (!all(assets_in_items))
      .error("There is no item with asset(s) '%s'.",
             paste(asset_names[!assets_in_items], collapse = ", "))

    items[["features"]] <- lapply(items[["features"]], function(item) {
      asset_names <- intersect(names(item[["assets"]]), asset_names)
      item[["assets"]] <- item[["assets"]][asset_names]
      return(item)
    })
  }

  if (!is.null(filter_fn)) {
    items[["features"]] <- lapply(items[["features"]], function(item) {
      sel <- vapply(item[["assets"]], filter_fn, logical(1))
      item[["assets"]] <- item[["assets"]][sel]
      return(item)
    })
  }

  if (!is.null(asset_names) || !is.null(filter_fn)) {
    empty_assets <- items_empty_assets(items)

    if (!keep_empty_items) {
      items[["features"]] <- items[["features"]][!empty_assets]
    } else if (any(empty_assets) && missing(keep_empty_items)) {
      .warning(paste(
        "Some items were left with empty assets. To remove them,",
        "set parameter `keep_empty_items = FALSE`."
      ))
    }
  }
  return(items)
}

#' @export
assets_rename <- function(items, names_fn = NULL, ...) {
  if (!is.null(names_fn) && !is.function(names_fn)) {
    .error("Parameter 'names_fn' must be a function.")
  }
  UseMethod("assets_rename", items)
}

#' @export
assets_rename.STACItem <- function(items, names_fn = NULL, ...) {
  dots <- c(...)
  if (!all(nzchar(names(dots)))) {
    .error("Renaming parameters must be named.")
  }

  if (length(dots) > 0) {
    asset_names <- names(items[["assets"]])
    asset_names[asset_names %in% names(dots)] <-
      unname(dots[asset_names[asset_names %in% names(dots)]])
    names(items[["assets"]]) <- asset_names
  }

  if (!is.null(names_fn)) {
    asset_names <- names(items[["assets"]])

    new_names <- vapply(items[["assets"]], names_fn,
                        FUN.VALUE = character(1), USE.NAMES = TRUE)
    new_names <- new_names[nzchar(new_names)]

    asset_names[asset_names %in% names(new_names)] <-
      unname(new_names[asset_names[asset_names %in% names(new_names)]])
    names(items[["assets"]]) <- asset_names
  }
  return(items)
}

#' @export
assets_rename.STACItemCollection <- function(items, names_fn = NULL, ...) {
  dots <- c(...)
  if (!all(nzchar(names(dots)))) {
    .error("Renaming parameters must be named.")
  }

  if (length(dots) > 0) {
    items[["features"]] <- lapply(items[["features"]], function(feature) {
      asset_names <- names(feature[["assets"]])
      asset_names[asset_names %in% names(dots)] <-
        unname(dots[asset_names[asset_names %in% names(dots)]])
      names(feature[["assets"]]) <- asset_names
      return(feature)
    })
  }

  if (!is.null(names_fn)) {
    items[["features"]] <- lapply(items[["features"]], function(feature) {
      asset_names <- names(feature[["assets"]])

      new_names <- vapply(feature[["assets"]], names_fn,
                          FUN.VALUE = character(1), USE.NAMES = TRUE)
      new_names <- new_names[nzchar(new_names)]

      asset_names[asset_names %in% names(new_names)] <-
        unname(new_names[asset_names[asset_names %in% names(new_names)]])
      names(feature[["assets"]]) <- asset_names
      return(feature)
    })
  }
  return(items)
}

items_empty_assets <- function(items) {
  return(vapply(items_reap(items, field = "assets"), length,
                FUN.VALUE = integer(1), USE.NAMES = FALSE) == 0)

}
