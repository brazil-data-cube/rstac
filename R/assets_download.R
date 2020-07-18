#' @title stac download
#'
#' @author Felipe Carvalho and Rolf Simoes
#'
#' @description The \code{assets_download} function downloads the assets
#' provided by the STAC API
#'
#' @param res ...
#' @param output_dir ...
#' @param curl_header ...
#' @param assets_name ...
#'
#' @return ...
#'
#' @export
assets_download <- function(res, output_dir = "./", curl_header = list(),
                            assets_name = c()){
  #browser()

  # check the object class
  if (!inherits(res, "stac_items"))
    stop(sprintf("Invalid `stac_items` object."), call. = FALSE)

  items_len <- items_length(res)
  if (items_len == 0)
    stop(sprintf("Query provided returned 0 items.
                 Please verify your query"), call. = FALSE)


  if (!dir.exists(output_dir))
    stop(sprintf("The directory provided does not exist.
                  Please specify a valid directory."), call. = FALSE)

  # TODO: Ajustar a barra de download
  # TODO: verificar como baixar as imagens cortadas usando vci direto do servidor

  # pb <- progress::progress_bar$new(
  #   format = "  downloading [:bar] :percent eta: :eta",
  #   total = items_len, clear = FALSE, width = 60)
  res_features <- res$features
  for (feature in 1:items_len) {
    #pb$tick()

    # TODO: verificar se o param n ta vazio
    assets   <- .select_assets(
                            assets_list  = res_features[[feature]][["assets"]],
                            assets_names = assets_name)
    feat_id  <- res_features[[feature]][["id"]]

    for (asset in 1:length(assets)) {

      asset_name <- names(assets[asset])
      asset_href <- assets[[asset]]$href
      file_ext   <- .file_ext(asset_href)

      dest_file <- sprintf("%s/%s_%s.%s", output_dir, feat_id, asset_name,
                           file_ext)

      tryCatch({
        curl::curl_download(url      = asset_href,
                            destfile = dest_file)
      }, error = function(error){
        message(paste(error, "in ", asset_href))
      })

      if (file.exists(dest_file))
        res$features[[feature]][["assets"]][[asset_name]]$href <-
          dest_file
    }
  }

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
#' @export
items_assets <- function(obj_stac){

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

#'@description function from \code{tools} package
#'
#'
.file_ext <- function(asset_url){
  pos   <- regexpr("\\.([[:alnum:]]+)$", asset_url)
  str_t <- ifelse(pos > -1L, substring(asset_url, pos + 1L), "")

  return(str_t)
}

#'
#'
#'
#'
.select_assets <- function(assets_list = list(), assets_names = c()){
  index_filter <- which(names(assets_list) %in% assets_names)
  if (length(index_filter) == 0) {
    warning("The provided assets names do not match with the API assets names.
             By default, all assets will be used", call. = FALSE)
    return(assets_list)
  }
  assets_list <- assets_list[index_filter]
  return(assets_list)
}
