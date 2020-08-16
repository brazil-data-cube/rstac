#' @title assets download
#'
#' @author Felipe Carvalho and Rolf Simoes
#'
#' @description The \code{assets_download} function downloads the assets
#' provided by the STAC API.
#'
#' @param items       a \code{stac_items} or \code{stac_item} object
#'  representing the result of \code{/stac/search},
#'   \code{/collections/{collectionId}/items} or
#'  \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @param assets_name a \code{character} with the assets names to be filtered.
#'
#' @param output_dir  a \code{character} directory in which the images will be
#'  saved.
#'
#' @param progress    a \code{logical} indicating if a progress bar must be
#'  shown or not. Defaults to \code{TRUE}.
#'
#' @param headers    a \code{character} of named arguments to be passed as
#' HTTP request headers.
#'
#' @seealso
#' \code{\link{stac_search}}, \code{\link{get_request}},
#'  \code{\link{post_request}}
#'
#' @examples
#' \dontrun{
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'  stac_search(collections = "MOD13Q1",
#'             bbox = c(-55.16335, -4.26325, -49.31739, -1.18355),
#'             limit = 2) %>%
#'  get_request() %>%
#'  assets_download(assets_name = c("thumbnail"), output_dir = ".")
#' }
#'
#' @return The same \code{stac_items} or \code{stac_item} object, with the
#' link of the item pointing to the directory where the assets were saved.
#'
#' @export
assets_download <- function(items, assets_name, output_dir = ".",
                            progress = TRUE, headers = c()) {

  #check the object class
  .check_obj(items, expected = c("stac_items", "stac_item"))

  # check output dir
  if (!dir.exists(output_dir))
    .error(paste("The directory provided does not exist.",
                 "Please specify a valid directory."))

  if (inherits(items, "stac_item")) {
    items <- .item_download(stac_item   = items,
                            assets_name = assets_name,
                            output_dir  = output_dir)

    return(items)
  }

  items_len <- items_length(items)
  # Queries that return without features
  if (items_len == 0)
    .error(paste("Query provided returned 0 items.",
                 "Please verify your query"))

  # verify if progress bar can be shown
  progress <- progress & (!is.null(items_len))
  if (progress)
    pb <- utils::txtProgressBar(min = 0, max = items_len, style = 3, width = 50)

  for (i in seq_len(items_len)) {

    # toggle bar
    if (progress)
      utils::setTxtProgressBar(pb, i)

    items$features[[i]] <- .item_download(items$features[[i]],
                                          assets_name, output_dir)
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
#' @param headers    a \code{character} of named arguments to be passed as
#' HTTP request headers.
#'
#' @return The same \code{stac_item} object, but with the link of the item
#'  pointing to the directory where the assets were saved.
#'
#' @noRd
.item_download <- function(stac_item, assets_name, output_dir, headers = c()) {

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
                httr::write_disk(path = dest_file),
                httr::add_headers(headers))

    }, error = function(error){
      warning(paste("\n", error, "in ", asset_href))
    })

    if (file.exists(dest_file)) {
      stac_item[["assets"]][[asset_name]]$href <- dest_file
    }
  }
  return(stac_item)
}

#' @title Helper function of \code{assets_download} function
#'
#' @author Implemented by \code{tools package}
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
  pos   <- regexpr("\\.([[:alnum:]]+)$", asset_url)
  str_t <- ifelse(pos > -1L, substring(asset_url, pos + 1L), "")

  return(str_t)
}

#' @title Helper function of \code{assets_download} function
#'
#' @author Felipe Carvalho
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
    .error(paste("The provided assets names do not match with the API",
                 "assets names. By default, all assets will be used"))
  }
  assets <- assets[names(assets) %in% assets_names]

  return(assets)
}
