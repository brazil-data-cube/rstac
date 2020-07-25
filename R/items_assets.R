#' @title items function
#'
#' @author Felipe Carvalho
#'
#' @description  The \code{items_assets} function lists the names of each assets
#' from each STAC item.
#'
#' @param items  a \code{stac} object expressing a STAC search criteria
#' provided by \code{stac_items} functions.
#'
#' @return A \code{list} with information of the assets of each item, where
#' each index represents one item.
#'
#' @examples
#' \dontrun{
#'
#' obj_stac <- stac_search("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'             collections = "MOD13Q1",
#'             bbox = c(-55.16335, -4.26325, -49.31739, -1.18355)) %>%
#'     stac_request()
#'
#' items_assets(obj_stac)
#' }
#'
#' @export
items_assets <- function(items) {

  # check object
  .check_obj(items, "stac_items")

  if (items_length(items) == 0)
    .error(paste("Query provided returned 0 items.",
                 "Please verify your query"))

  items_assets <- lapply(items$features, function(feature) {
    list(collection_name = feature[["id"]],
         assets_name     = names(feature[["assets"]]))
  })

  return(items_assets)
}
