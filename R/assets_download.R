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
#' @param assets_name a `character` with the assets names to be filtered.
#'
#' @param output_dir  a `character` directory in which the assets will be
#'  saved.
#'
#' @param overwrite   a `logical` if TRUE will replaced the existing file,
#'  if FALSE a warning message is shown. (`FALSE`, default).
#'
#' @param items_max   a `numeric` corresponding how many items will be
#'  downloaded.
#'
#' @param progress    a `logical` indicating if a progress bar must be
#'  shown or not. (`TRUE`, default).
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
#'   stac_search(collections = "CB4_64_16D_STK-1") %>%
#'   stac_search(limit = 2) %>%
#'   get_request() %>%
#'   assets_download(assets_name = "thumbnail", output_dir = ".")
#' }
#'
#' @return The same `STACItemCollection` or `STACItem` object, with
#' the link pointing to the directory where the assets were saved.
#'
#' @export
assets_download <- function(items,
                            assets_name,
                            output_dir = ".",
                            overwrite = FALSE,
                            items_max = Inf,
                            progress = TRUE,
                            ...) {

  #check the object subclass
  check_subclass(items, subclasses = c("STACItemCollection", "STACItem"))

  # check output dir
  if (!dir.exists(output_dir))
    .error(paste("The directory provided does not exist.",
                 "Please specify a valid directory."))

  # if the object is a STACItem
  if (subclass(items) == "STACItem") {
    items <- .item_download(stac_item   = items,
                            assets_name = assets_name,
                            output_dir  = output_dir,
                            overwrite   = overwrite, ...)
    return(items)
  }

  # check if items length corresponds with items matched
  if (!missing(items_max)) {
    if (items_max > items_length(items)) {
      .warning(paste("The number of specified items is greater than the number",
                     "of items length on your object. By default,",
                     "items_max = %d"),
               items_length(items))

      items_max <- .parse_items_size(items)
    }
  } else {
    # Queries that return without features
    if (items_length(items) == 0)
      .error(paste("Query provided returned 0 items.",
                   "Please verify your query."))

    items_max <- .parse_items_size(items)
  }

  # verify if progress bar can be shown
  progress <- progress & (!is.null(items_max))
  if (progress)
    pb <- utils::txtProgressBar(min = 0, max = items_max, style = 3, width = 50)

  for (i in seq_len(items_max)) {

    # toggle bar
    if (progress)
      utils::setTxtProgressBar(pb, i)

    items$features[[i]] <- .item_download(stac_item   = items$features[[i]],
                                          assets_name = assets_name,
                                          output_dir  = output_dir,
                                          overwrite   = overwrite, ...)
  }
  # close progress bar
  if (progress)
    close(pb)

  return(items)
}

# helpers ----------------------------------------------------------------------

#' @title Helper function of `assets_download` function
#'
#' @description the `.item_download` function downloads the assets of a
#'  `STACItem` object.
#'
#' @param stac_item   a  `stac_item` object expressing a STAC
#'  search criteria provided by `stac_item` function.
#'
#' @param assets_name a `character` with the assets names to be filtered.
#'
#' @param output_dir  a `character` directory in which the images will be
#'  saved.
#'
#' @param overwrite  a `logical` if TRUE will replaced the existing file,
#'  if FALSE a warning message is shown.
#'
#' @param ...       config parameters to be passed to [GET][httr::GET] or
#' [POST][httr::POST] methods, such as [add_headers][httr::add_headers] or
#' [set_cookies][httr::set_cookies].
#'
#' @return The same `stac_item` object, but with the link of the item
#'  pointing to the directory where the assets were saved.
#'
#' @noRd
.item_download <- function(stac_item, assets_name, output_dir, overwrite, ...) {

  feat_id <- stac_item[["id"]]
  assets  <- .select_assets(stac_item[["assets"]], assets_name)

  for (i in seq_along(assets)) {
    # store the names of assets
    asset_name <- names(assets[i])
    asset_href <- assets[[i]]$href
    file_ext   <- .file_ext(asset_href)

    # create a full path name
    dest_file  <- sprintf("%s/%s_%s.%s", output_dir, feat_id, asset_name,
                          file_ext)

    tryCatch({
      httr::GET(url = asset_href,
                httr::write_disk(path = dest_file, overwrite = overwrite), ...)

    }, error = function(error) {
      .warning(paste("\n", error, "in ", asset_href))
    })

    if (file.exists(dest_file)) {
      stac_item[["assets"]][[asset_name]]$href <- dest_file
    }
  }
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
#' @param assets_names a `character` with the assets names to be filtered.
#'
#' @return A `list` in the same format as the list of assets, but with the
#'  selected assets names.
#'
#' @noRd
.select_assets <- function(assets, assets_names) {

  # If not provided the assets name, by default all assets will be used
  if (missing(assets_names)) {
    return(assets)
  }

  if (!all(assets_names %in% names(assets))) {
    .error(paste("The provided assets names do not match with the",
                 "assets names in the document."))
  }
  assets <- assets[names(assets) %in% assets_names]

  return(assets)
}
