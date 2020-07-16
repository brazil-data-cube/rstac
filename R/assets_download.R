#' @title stac download
#'
#' @author ...
#'
#' @description baixar as imagens que vem do res e organizar em repo
#'
#' @param res ...
#' @param output_dir ...
#'
#' @export
assets_download <- function(res, output_dir = "./", curl_hearder = list()){

  # check the object class
  if (!inherits(res, "stac_items"))
    stop(sprintf("Invalid `stac_items` object."), call. = FALSE)

  items_len <- rstac::items_length(res)
  if (items_len == 0)
    stop(sprintf("Query provided returned 0 items.
                 Please verify your query"), call. = FALSE)


  # TODO: Verificar a quantidade de items por pagina
  # items_total <- rstac::items_fetch()
  # TODO: Verificar a quantidade de items <<- item_fetch
  # TODO: Ajustar a barra de download

  # pb <- progress::progress_bar$new(
  #   format = "  downloading [:bar] :percent eta: :eta",
  #   total = items_len, clear = FALSE, width = 60)
  res_features <- res$features
  for (feature in 1:items_len) {
    #pb$tick()

    assets   <- res_features[[feature]][["assets"]]
    feat_id  <- res_features[[feature]][["id"]]

    for (asset in 1:length(assets)) {
      asset_name <- names(assets[asset])
      asset_href <- assets[[asset]]$href

      #TODO:  Verificar o formato do arquivo antes de baixor
      curl::curl_download(url      = asset_href,
                          destfile = sprintf("%s/%s_%s.tif",
                                             output_dir,
                                             feat_id,
                                             asset_name),
                          handle   =  curl_hearder)
    }

  }

  return(invisible(NULL))
}


