#' @title Downloads assets via STAC API
#'
#' @description The `assets_download` function downloads the assets
#' provided by the STAC API.
#'
#' @param items       a `STACItem` or `STACItemCollection` object
#'  representing the result of `/stac/search`,
#'  \code{/collections/{collectionId}/items} or
#'  \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @param assets_name `r lifecycle::badge('deprecated')`
#'  use `asset_names` parameter instead.
#'
#' @param asset_names a `character` with the assets names to be filtered.
#'
#'
#' @param output_dir  a `character` directory in which the assets will be
#'  saved.
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
#' @param fn          a `function` to handle the list of assets for each item.
#'  Using this function you can change the hrefs for each asset, as well as use
#'  another request verb, such as POST.
#'
#' @param ...         config parameters to be passed to [GET][httr::GET] or
#' [POST][httr::POST] methods, such as [add_headers][httr::add_headers] or
#' [set_cookies][httr::set_cookies].
#'
#' @seealso
#' [stac_search()], [items()], [get_request()]
#'
#' @examples
#' \dontrun{
#' stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1",
#'               datetime = "2019-06-01/2019-08-01") %>%
#'   stac_search() %>%
#'   get_request() %>%
#'   assets_download(asset_names = "thumbnail", output_dir = ".",
#'   overwrite = FALSE)
#' }
#'
#' @return The same `STACItemCollection` or `STACItem` object, with
#' the link of the item pointing to the directory where the assets were saved.
#'
#' @export
assets_download <- function(items,
                            asset_names = NULL,
                            output_dir = ".",
                            overwrite = FALSE,
                            items_max = Inf,
                            progress  = TRUE,
                            fn = NULL, ...,
                            assets_name = deprecated()) {

  #check the object subclass
  check_subclass(items, subclasses = c("STACItemCollection", "STACItem"))

  if (lifecycle::is_present(assets_name)) {

    # signal the deprecation to the user
    lifecycle::deprecate_soft("0.9.1-5",
                              "rstac::assets_download(assets_name = )",
                              "rstac::assets_download(asset_names = )")

    # deal with the deprecated argument for compatibility
    asset_names <- assets_name
  }

  # check output dir
  if (!dir.exists(output_dir))
    .error(paste("The directory provided does not exist.",
                 "Please specify a valid directory."))


  if (is.null(asset_names))
    asset_names <- items_fields(items, "assets")

  # if the object is a STACItem
  if (subclass(items) == "STACItem") {
    items <- .item_download(stac_item   = items,
                            asset_names = asset_names,
                            output_dir  = output_dir,
                            overwrite   = overwrite, ...)
    return(items)
  }

  # check if items length corresponds with items matched
  if (!missing(items_max)) {

    if (items_max > items_length(items))
      items_max <- .parse_items_size(items)

  } else {
    # Queries that return without features
    if (items_length(items) == 0)
      .error(paste("Query provided returned 0 items.",
                   "Please verify your query."))

    items_max <- .parse_items_size(items)
  }

  # verify if progress bar can be shown
  progress <- progress && (!is.null(items_max)) && items_max > 1
  if (progress)
    pb <- utils::txtProgressBar(min = 0, max = items_max, style = 3, width = 50)

  for (i in seq_len(items_max)) {

    # toggle bar
    if (progress)
      utils::setTxtProgressBar(pb, i)

    items$features[[i]] <- .item_download(stac_item   = items$features[[i]],
                                          asset_names = asset_names,
                                          output_dir  = output_dir,
                                          overwrite   = overwrite,
                                          fn = fn, ...)
  }
  # close progress bar
  if (progress)
    close(pb)

  return(items)
}

#' @title Helper function of `assets_download` function
#'
#' @description the `.item_download` function downloads the assets of a
#'  stac_item
#'
#' @param stac_item   a  `stac_item` object expressing a STAC
#'  search criteria provided by `stac_item` function.
#'
#' @param asset_names a `character` with the assets names to be filtered.
#'
#' @param output_dir  a `character` directory in which the images will be
#'  saved.
#'
#' @param overwrite   a `logical` if TRUE will replaced the existing file,
#'  if FALSE a warning message is shown.
#'
#' @param ...         config parameters to be passed to [GET][httr::GET] or
#' [POST][httr::POST] methods, such as [add_headers][httr::add_headers] or
#' [set_cookies][httr::set_cookies].
#'
#' @return The same `stac_item` object, but with the link of the item
#'  pointing to the directory where the assets were saved.
#'
#' @noRd
.item_download <- function(stac_item, asset_names, output_dir,
                           overwrite, ..., fn) {

  assets <- .select_assets(stac_item[["assets"]], asset_names)

  stac_item[["assets"]] <- lapply(assets, function(asset){

    if (!is.null(fn))
      return(fn(asset))

    # create a full path name
    dest_file <- paste0(output_dir, "/",
                        gsub(".*/([^?]*)\\??.*$", "\\1", asset$href))

    tryCatch({
      httr::GET(url = asset$href,
                httr::write_disk(path = dest_file, overwrite = overwrite), ...)

    }, error = function(e){
      .error("Error while downloading %s. \n%s", asset$href, e$message)
    })

    asset$href <- dest_file

    asset
  })

  return(stac_item)
}

#' @title Helper function of `assets_download` function
#'
#' @description The `.file_ext` is function to extract the extension
#' from a file
#'
#' @param asset_url  a `character` URL provided from a `stac_search`.
#'
#' @return A `character` of the extracted file extension.
#'
#' @noRd
.file_ext <- function(asset_url) {

  # remove query string from url
  asset_url[[1]] <- sub("\\?.+", "", asset_url[[1]])

  pos <- regexpr("\\.([[:alnum:]]+)$", asset_url[[1]])
  if (pos < 0) return("")
  return(substring(asset_url[[1]], pos + 1))
}

#' @title Helper function of `assets_download` function
#'
#' @description The helper function `.select_assets` selects the names of
#' each asset provided by users
#'
#' @param assets_list  a `list` with the information of each item provided
#' by API STAC
#'
#' @param asset_names  a `character` with the assets names to be filtered.
#'
#' @return A `list` in the same format as the list of assets, but with the
#'  selected assets names.
#'
#' @noRd
.select_assets <- function(assets, asset_names) {

  if (!all(asset_names %in% names(assets))) {
    .error(paste("The provided assets names do not match with the",
                 "assets names in the document."))
  }
  assets <- assets[names(assets) %in% asset_names]

  return(assets)
}

#' @title Assets functions
#'
#' @description This function returns the `date`, `band` and
#'  `URL` fields for each assets of an `STACItemCollection` object.
#'  For the URL you can add the GDAL library drivers for the following schemes:
#'  HTTP/HTTPS files, S3 (AWS S3) and GS (Google Cloud Storage).
#'
#' @param items        a `STACItemCollection` object representing
#'  the result of `/stac/search`, \code{/collections/{collectionId}/items}.
#'
#' @param assets_names `r lifecycle::badge('deprecated')`
#'  use `asset_names` parameter instead.
#'
#' @param asset_names  a `character` with the assets names to be
#'  filtered. If `NULL` (default) all assets will be returned..
#'
#' @param sort         a `logical` if true the dates will be sorted
#'  in increasing order. By default, the dates are sorted.
#'
#' @param gdal_vsi_resolution a `logical`  if true, gdal drivers are
#'  included in the URL of each asset. The following schemes are supported:
#'  HTTP/HTTPS files, S3 (AWS S3) and GS (Google Cloud Storage).
#'
#' @param ...          additional arguments. See details.
#'
#' @param fn           a `function` that will be used to filter the attributes
#'  listed in the properties.
#'
#' @return a `list` with the attributes of date, bands and paths.
#'
#' @examples
#' \donttest{
#' # STACItemCollection object
#' stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'  stac_search(collections = "CB4_64_16D_STK-1", limit = 100,
#'         datetime = "2017-08-01/2018-03-01",
#'         bbox = c(-48.206,-14.195,-45.067,-12.272)) %>%
#'  get_request() %>% items_fetch(progress = FALSE)
#'
#' stac_item %>% assets_list(assets_names = c("EVI", "NDVI"))
#' }
#'
#' @name assets_function
NULL

#' @rdname assets_function
#' @export
assets_list <- function(items, asset_names = NULL,
                        sort = TRUE, gdal_vsi_resolution = TRUE,
                        assets_names = deprecated()) {


  if (lifecycle::is_present(assets_names)) {

    # Signal the deprecation to the user
    lifecycle::deprecate_soft("0.9.1-5",
                              "rstac::assets_download(assets_names = )",
                              "rstac::assets_download(asset_names = )")

    # Deal with the deprecated argument for compatibility
    asset_names <- assets_names
  }

  if (is.null(asset_names))
    asset_names <- items_fields(items, "assets")

  timeline <- items_reap(items, field = c("properties", "datetime"))
  index    <- seq_along(timeline)
  if (sort) index <- order(timeline)

  timeline <- timeline[index]
  assets   <- list(date = rep(timeline, length(unique(asset_names))))

  for (b in asset_names) {

    href <- items_reap(items, field = c("assets", b, "href"))[index]

    if (gdal_vsi_resolution) {

      # for http or https schema
      paste_index <- grepl("^http|[s]://.*", href)
      if (any(paste_index))
        href[paste_index] <- paste("/vsicurl", href[paste_index], sep = "/")

      # for S3 schema
      paste_index <- grepl("^s3://.*", href)
      if (any(paste_index))
        href[paste_index] <- paste("/vsis3", gsub("^s3://(.*)$", "\\1",
                                                  href[paste_index]), sep = "/")
      # for gs schema
      paste_index <- grepl("^gs://.*", href)
      if (any(paste_index))
        href[paste_index] <- paste("/vsigs", gsub("^gs://(.*)$", "\\1",
                                                  href[paste_index]), sep = "/")
    }
    assets$band <- c(rep(b, length(href)), assets$band)
    assets$path <- c(href,  assets$path)
  }
  assets
}

#' @rdname assets_function
#'
#' @export
assets_select <- function(items, asset_names) {

  UseMethod("assets_select", items)
}

#' @rdname assets_function
#'
#' @export
assets_select.STACItemCollection <- function(items, asset_names) {

  if (!all(asset_names %in% items_assets(items, simplify = TRUE)))
    .error("Invalid 'asset_names' parameter.")

  items$features <- lapply(items$features, function(item) {
    item$assets <- item$assets[asset_names]

    item
  })

  items
}

#' @rdname assets_function
#'
#' @export
assets_select.STACItem <- function(items, asset_names) {

  if (!all(asset_names %in% items_assets(items)))
    .error("Invalid 'asset_names' parameter.")

  items$assets <- items$assets[asset_names]

  items
}

#' @rdname assets_function
#'
#' @export
assets_filter <- function(items, ..., fn = NULL) {

  UseMethod("assets_filter", items)
}

#' @rdname assets_function
#'
#' @export
assets_filter.STACItemCollection <- function(items, ..., fn = NULL) {

  dots <- substitute(list(...))[-1]

  if (length(dots) > 0) {

    if (!is.null(names(dots)))
      .error("Invalid filter arguments.")

    for (i in seq_along(dots)) {

      items$features <- lapply(items$features, function(item) {

        sel <- vapply(item$assets, function(asset) {

          tryCatch({
            eval(dots[[i]], envir = asset, enclos = baseenv())
          }, error = function(e) { NA })
        }, logical(1))

        if (all(is.na(sel)))
          .error("Invalid condition arguments.")

        sel[is.na(sel)] <- FALSE

        item$assets <- item$assets[sel]

        item
      })
    }
  }

  if (!is.null(fn)) {

    items$features <- lapply(items$features, function(item) {

      sel <- vapply(item$assets, function(asset) { fn(asset) }, logical(1))

      item$assets <- item$assets[sel]
      item
    })
  }

  items
}

#' @rdname assets_function
#'
#' @export
assets_filter.STACItem <- function(items, ..., fn = NULL) {

  dots <- substitute(list(...))[-1]

  if (length(dots) > 0) {

    if (!is.null(names(dots)))
      .error("Invalid filter arguments.")

    for (i in seq_along(dots)) {

      sel <- vapply(items$assets, function(asset) {

        tryCatch({
          eval(dots[[i]], envir = asset, enclos = baseenv())
        }, error = function(e) { NA })
      }, logical(1))

      if (all(is.na(sel)))
        .error("Invalid condition arguments.")

      sel[is.na(sel)] <- FALSE

      items$assets <- items$assets[sel]
    }
  }

  if (!is.null(fn)) {

    sel <- vapply(items$assets, function(asset) { fn(asset) }, logical(1))

    items$assets <- items$assets[sel]
  }

  items
}
