#' @title Downloads assets via STAC API
#'
#' @description The \code{assets_download} function downloads the assets
#' provided by the STAC API.
#'
#' @param items       a \code{STACItem} or \code{STACItemCollection} object
#'  representing the result of \code{/stac/search},
#'  \code{/collections/{collectionId}/items} or
#'  \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @param assets_name a \code{character} with the assets names to be filtered.
#'
#' @param output_dir  a \code{character} directory in which the assets will be
#'  saved.
#'
#' @param overwrite   a \code{logical} if TRUE will replaced the existing file,
#'  if FALSE a warning message is shown.
#'
#' @param items_max   a \code{numeric} corresponding how many items will be
#'  downloaded.
#'
#' @param progress    a \code{logical} indicating if a progress bar must be
#'  shown or not. Defaults to \code{TRUE}.
#'
#' @param ...         config parameters to be passed to \link[httr]{GET} or
#' \link[httr]{POST} methods, such as \link[httr]{add_headers} or
#' \link[httr]{set_cookies}.
#'
#' @seealso
#' \code{\link{stac_search}}, \code{\link{items}}, \code{\link{get_request}}
#'
#' @examples
#' \dontrun{
#' stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1") %>%
#'   stac_search(limit = 2) %>%
#'   get_request() %>%
#'   assets_download(assets_name = "thumbnail", output_dir = ".",
#'   overwrite = FALSE)
#' }
#'
#' @return The same \code{STACItemCollection} or \code{STACItem} object, with
#' the link of the item pointing to the directory where the assets were saved.
#'
#' @export
assets_download <- function(items, assets_name, output_dir = ".",
                            overwrite = FALSE, items_max = Inf, progress = TRUE,
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

    items$features[[i]] <- .item_download(items$features[[i]],
                                          assets_name, output_dir,
                                          overwrite,...)
  }
  # close progress bar
  if (progress)
    close(pb)

  return(items)
}

# helpers ----------------------------------------------------------------------

#' @title Helper function of \code{assets_download} function
#'
#' @description the \code{.item_download} function downloads the assets of a
#'  stac_item
#'
#' @param stac_item   a  \code{stac_item} object expressing a STAC
#'  search criteria provided by \code{stac_item} function.
#'
#' @param assets_name a \code{character} with the assets names to be filtered.
#'
#' @param output_dir  a \code{character} directory in which the images will be
#'  saved.
#'
#' @param overwrite  a \code{logical} if TRUE will replaced the existing file,
#'  if FALSE a warning message is shown.
#'
#' @param ...       config parameters to be passed to \link[httr]{GET} or
#' \link[httr]{POST} methods, such as \link[httr]{add_headers} or
#' \link[httr]{set_cookies}.
#'
#' @return The same \code{stac_item} object, but with the link of the item
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

    }, error = function(error){
      .warning(paste("\n", error, "in ", asset_href))
    })

    if (file.exists(dest_file)) {
      stac_item[["assets"]][[asset_name]]$href <- dest_file
    }
  }
  return(stac_item)
}

#' @title Helper function of \code{assets_download} function
#'
#' @description The \code{.file_ext} is function to extract the extension
#' from a file
#'
#' @param asset_url  a \code{character} URL provided from a \code{stac_search}.
#'
#' @return A \code{character} of the extracted file extension.
#'
#' @noRd
.file_ext <- function(asset_url) {

  pos <- regexpr("\\.([[:alnum:]]+)$", asset_url[[1]])
  if (pos < 0) return("")
  return(substring(asset_url[[1]], pos + 1))
}

#' @title Helper function of \code{assets_download} function
#'
#' @description The helper function \code{.select_assets} selects the names of
#' each asset provided by users
#'
#' @param assets_list  a \code{list} with the information of each item provided
#' by API STAC
#'
#' @param assets_names a \code{character} with the assets names to be filtered.
#'
#' @return A \code{list} in the same format as the list of assets, but with the
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
