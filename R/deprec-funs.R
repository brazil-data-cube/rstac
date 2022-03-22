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
  lifecycle::deprecate_soft(
    when = "0.9.1-5",
    what = "rstac::items_bands()",
    with = "rstac::items_assets()"
  )

  return(items_assets(items))
}


#' @title STACItemCollection function
#'
#' @description
#' `r lifecycle::badge('deprecated')` Use \code{\link{assets_gdalvfs}()} function
#'  instead.
#'
#' @param items a `STACItemCollection` or `STACItem` object.
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
#' @return a items `STACItemCollection` or `STACItem` object with gdal virtual
#' file system added.
#'
#' @examples
#' \dontrun{
#'
#' x <- stac("http://brazildatacube.dpi.inpe.br/stac") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1") %>%
#'   stac_search() %>%
#'   get_request()
#'
#' x %>% assets_append_gdalvfs()
#' }
#'
#' @export
assets_list <- function(items,
                        asset_names = NULL,
                        sort = TRUE,
                        gdal_vsi_resolution = TRUE,
                        assets_names = deprecated()) {

  # signal the deprecation to the user
  lifecycle::deprecate_soft(
    when = "0.9.1-5",
    what = "rstac::assets_list()",
    with = "rstac::assets_append_gdalvfs()"
  )

  if (!missing(assets_names))

    asset_names <- .deprec_parameter(
      deprec_var = assets_names,
      dest_var = asset_names,
      deprec_version = "0.9.1-5",
      env = environment()
    )

  assets_append_gdalvfs(
    items = items,
    asset_names = asset_names,
    sort = sort,
    gdal_vsi_resolution = gdal_vsi_resolution
  )
}


