#' @title assets download
#'
#' @author Felipe Carvalho and Rolf Simoes
#'
#' @description The \code{assets_download} function downloads the assets
#' provided by the STAC API
#'
#' @param res A \code{stac_items} or \code{stac_item} objects expressing a STAC
#'  search criteria provided by \code{stac_search} functions.
#'
#' @param assets_name  A \code{character} with the assets names to be filtered.
#'
#' @param output_dir  A \code{character} directory in which the images will be
#'  saved.
#'
#' @param progress    A \code{logical} indicating if a progress bar must be
#'  shown or not. Defaults to \code{TRUE}.
#'
#' @seealso
#' \code{\link{stac_search}}, \code{\link{get_request}},
#'  \code{\link{post_request}}
#'
#' @examples
#' \dontrun{
#'
#' stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'             collections = "MOD13Q1",
#'             bbox = c(-55.16335, -4.26325, -49.31739, -1.18355), limit = 10) %>%
#'     get_request() %>%
#'     assets_download(assets_name = c("thumbnail"), output_dir = "./")
#' }
#'
#' @return The same \code{stac_items} object, but with the link of the item
#'  pointing to the directory where the assets were saved.
#'
#' @export
assets_download <- function(res, assets_name = c(), output_dir = "./",
                            progress = TRUE) {

  #check the object class
  .check_obj(res, expected = c("stac_items", "stac_item"))

  # check output dir
  if (!dir.exists(output_dir))
    stop(sprintf("The directory provided does not exist.
                  Please specify a valid directory."), call. = FALSE)

  if (inherits(res, "stac_item")) {
    res <- .item_download(stac_item   = res,
                          assets_name = assets_name,
                          output_dir  = output_dir)

    return(res)
  }

  items_len <- items_length(res)
  # Queries that return without features
  if (items_len == 0)
    stop(sprintf("Query provided returned 0 items.
                  Please verify your query"), call. = FALSE)

  # verify if progress bar can be shown
  progress <- progress & (!is.null(items_len))
  if (progress)
    pb <- utils::txtProgressBar(min = 0, max = items_len, style = 3, width = 50)

  for (feature in 1:items_len) {
    # toggle bar
    if (progress)
      utils::setTxtProgressBar(pb, feature)

    res$features[[feature]] <- .item_download(res$features[[feature]],
                                             assets_name, output_dir)
  }
  # close progress bar
  if (progress)
    close(pb)

  return(res)
}

#' @title items function
#'
#' @author Felipe Carvalho and Rolf Simoes
#'
#' @description  The \code{items_assets} function lists the names of each assets
#' from each STAC item.
#'
#' @param obj_stac A \code{stac} object expressing a STAC search criteria
#' provided by \code{stac_items} functions.
#'
#' @return A \code{list} with information of the assets of each item, where
#' each index represents one item.
#'
#' @examples
#' \dontrun{
#'
#' obj_stac <- stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'             collections = "MOD13Q1",
#'             bbox = c(-55.16335, -4.26325, -49.31739, -1.18355)) %>%
#'     stac_request()
#'
#' items_names <- items_assets(obj_stac)
#' }
#'
#' @export
items_assets <- function(obj_stac) {

  if (!inherits(obj_stac, "stac_items"))
    stop(sprintf("Invalid `stac_items` object."), call. = FALSE)

  items_len <- rstac::items_length(obj_stac)
  if (items_len == 0)
    stop(sprintf("Query provided returned 0 items.
                 Please verify your query"), call. = FALSE)

  res_features <- obj_stac$features
  items_assets <- lapply(res_features, function(feature){
    list(collection_name = feature[["id"]],
         assets_name     = names(feature[["assets"]]))
  })

  return(items_assets)
}

# helpers ----------------------------------------------------------------------

#' @title Helper function of \code{assets_download} function
#'
#' @description the \code{.item_download} function downloads the assets of a
#'  stac_item
#'
#' @param stac_item A  \code{stac_item} object expressing a STAC
#'  search criteria provided by \code{stac_item} function.
#'
#' @param assets_name A \code{character} with the assets names to be filtered.
#'
#' @param output_dir A \code{character} directory in which the images will be
#'  saved.
#'
#' @return The same \code{stac_item} object, but with the link of the item
#'  pointing to the directory where the assets were saved.
#'
#' @noRd
.item_download <- function(stac_item, assets_name = c(), output_dir = "./") {

  feat_id <- stac_item[["id"]]
  assets  <- .select_assets(stac_item[["assets"]], assets_name)

  for (asset in 1:length(assets)) {
    # store the names of assets
    asset_name <- names(assets[asset])
    asset_href <- assets[[asset]]$href
    file_ext   <- .file_ext(asset_href)

    # create a full path name
    dest_file  <- sprintf("%s/%s_%s.%s", output_dir, feat_id, asset_name,
                         file_ext)

    tryCatch({
      # TODO: ver o config
      httr::GET(url      = asset_href,
                httr::write_disk(path = dest_file))
      #curl::curl_download(url      = asset_href,
      #                    destfile = dest_file)
    }, error = function(error){
      message(paste("\n", error, "in ", asset_href))
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
#' @param asset_url A \code{character} URL provided from a \code{stac_search}.
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
#' @param assets_list A \code{list} with the information of each item provided
#' by API STAC
#'
#' @param assets_names A \code{character} with the assets names to be filtered.
#'
#' @return A \code{list} in the same format as the list of assets, but with the
#'  selected assets names.
#'
#' @noRd
.select_assets <- function(assets_list = list(), assets_names = c()) {

  # If not provided the assets name, by default all assets will be used
  if (length(assets_names) == 0) {
    return(assets_list)
  }

  index_filter <- which(names(assets_list) %in% assets_names)
  if (length(index_filter) == 0) {
    warning("The provided assets names do not match with the API assets names.
             By default, all assets will be used", call. = FALSE)
    return(assets_list)
  }
  assets_list <- assets_list[index_filter]

  return(assets_list)
}
