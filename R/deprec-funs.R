#' @title STACItemCollection function
#'
#' @description
#' `r lifecycle::badge('deprecated')` Use \code{\link{items_assets}()} function
#'  instead.
#'
#' @param items a `STACItemCollection` or `STACItem` object.
#'
#' @examples
#' \dontrun{
#'
#' x <- stac("http://brazildatacube.dpi.inpe.br/stac") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1") %>%
#'   stac_search() %>%
#'   get_request()
#'
#' x %>% items_assets()
#' }
#'
#' @export
items_bands <- function(items) {

  # signal the deprecation to the user
  lifecycle::deprecate_soft("0.9.1-5",
                            "rstac::items_bands(items = )",
                            "rstac::items_assets(items = )")

  return(items_assets(items))
}
