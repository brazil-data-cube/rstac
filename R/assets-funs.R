#' @title Assets functions
#'
#' @description
#' These functions provide support to work with `STACItemCollection` and
#' `STACItem` objects.
#'
#' \itemize{
#' \item `assets_download()`: Downloads the assets provided by the STAC API.
#'
#' \item `assets_url()`: Returns a list with href of each feature.
#'  For the URL you can add the GDAL library drivers for the following schemes:
#'  HTTP/HTTPS files, S3 (AWS S3) and GS (Google Cloud Storage).
#'
#' \item `assets_select()`: Selects the assets of each feature by its name.
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
#' @param ...         config parameters to be passed to [GET][httr::GET],
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
#' \item `assets_url()`: returns a list with `href` of each feature.
#'
#' \item `assets_select()`: returns the same object as the provided item
#' (`STACItemCollection` or `STACItem`), however with the selected assets.
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
  items <- assets_select(items = items, asset_names = asset_names)
  url <- unlist(lapply(items_assets(items), function(asset_name) {
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
    if (!all(asset_names %in% items_assets(items)))
      .error("Invalid 'asset_names' parameter.")
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
                                             filter_fn = NULL) {
  if (!is.null(asset_names)) {
    if (!all(asset_names %in% items_assets(items, simplify = TRUE)))
      .error("Invalid 'asset_names' parameter.")

    items$features <- lapply(items$features, function(item) {
      item$assets <- item$assets[asset_names]
      item
    })
  }

  if (!is.null(filter_fn)) {
    items$features <- lapply(items$features, function(item) {
      sel <- vapply(item$assets, filter_fn, logical(1))
      item$assets <- item$assets[sel]
      item
    })
  }

  items
}

#' @rdname assets_function
#'
#' @export
assets_select.STACItem <- function(items,
                                   asset_names = NULL,
                                   filter_fn = NULL) {
  if (!is.null(asset_names)) {
    if (!all(asset_names %in% items_assets(items)))
      .error("Invalid 'asset_names' parameter.")
    items$assets <- items$assets[asset_names]
  }

  if (!is.null(filter_fn)) {
    sel <- vapply(items$assets, filter_fn, logical(1))
    items$assets <- items$assets[sel]
  }

  items
}
