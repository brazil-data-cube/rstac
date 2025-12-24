
asset_context <- new.env(parent = emptyenv())

asset_install <- function(key, asset) {
  val <- as.list(asset_context)
  if ("key" %in% names(asset_context))
    rm(list = c("key", "asset"), envir = asset_context)
  if (is.character(key)) {
    assign("key", key, envir = asset_context)
    assign("asset", asset, envir = asset_context)
  }
  val
}

asset_get <- function(field) {
  if (!"key" %in% names(asset_context))
    .error("Must be used inside `assets_select()`")
  asset_context$asset[[field]]
}

select_check_eval <- function(val) {
  if (!is.logical(val)) {
    .error(paste(
      "Select expressions must be evaluated as logical.",
      "If you want select assets by name, use `asset_names`",
      "parameter."
    ))
  }
  if (length(val) != 1) {
    .error("Select function must return a logical value of length 1.")
  }
}

select_eval <- function(key, asset, expr) {
  old <- asset_install(key, asset)
  on.exit(asset_install(old$key, old$asset))
  val <- tryCatch({
    eval(expr, envir = asset, enclos = parent.env(parent.frame()))
  }, error = function(e) {
    return(FALSE)
  })
  if (length(val) == 0)
    return(FALSE)
  select_check_eval(val)
  return(val)
}

select_exec <- function(key, asset, select_fn) {
  args <- if (length(formals(select_fn)) >= 2)
    list(asset, key) else list(asset)
  val <- do.call(select_fn, args = args, envir = parent.env(parent.frame()))
  select_check_eval(val)
  return(val)
}

asset_download <- function(asset,
                           output_dir,
                           overwrite, ...,
                           use_gdal = FALSE,
                           download_fn = NULL) {
  if (!is.null(download_fn))
    return(download_fn(asset))
  # create a full path name
  out_file <- path_normalize(output_dir, url_get_path(asset$href))
  out_dir <- dirname(out_file)
  if (!dir.exists(out_dir))
    dir.create(out_dir, recursive = TRUE)
  stopifnot(dir.exists(out_dir))
  if (use_gdal) {
    if (file.exists(out_file) && !overwrite)
      .error("File already exists. Use `overwrite=TRUE`.")
    if (file.exists(out_file))
      unlink(out_file)
    sf::gdal_utils(
      util = "translate",
      source = gdalvsi_append(asset$href),
      destination = out_file, ...
    )
    if (!file.exists(out_file)) {
      .error("Download failed. File: '%s'.", asset$href)
    }
  } else {
    make_get_request(
      url = asset$href,
      httr::write_disk(path = out_file, overwrite = overwrite),
      error_msg = "Error while downloading", ...
    )
  }
  asset$href <- out_file
  asset
}
