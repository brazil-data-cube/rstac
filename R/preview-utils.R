#' Plot preview images
#'
#' This is a helper function to plot preview assets
#' (e.g. quicklook, thumbnail, rendered_preview).
#' Currently, only png, jpeg and jpg formats are supported.
#'
#' @param url   image URL to be plotted.
#'
#' @return A rastergrob grob from package `grid`.
#'
#' @export
preview_plot <- function(url) {
  preview_check(url)
  img <- preview_read_file(url)
  plot(1:10, type = "n", axes = FALSE, xlab = "", ylab = "")
  grid::grid.raster(img)
}

preview_file_type <- function(url) {
  gsub(".*\\.(.+)$", "\\1", url_get_path(url))
}

preview_switch <- function(url, ...) {
  type <- preview_file_type(url)
  switch(type, ..., .error("File type '%s' not supported", type))
}

preview_check <- function(url) {
  preview_switch(
    url,
    png = if (!requireNamespace("png", quietly = TRUE))
      .error(paste(
        "This function requires `png` package. Please, use",
        "install.packages('png')."
      ))
    ,
    jpeg = if (!requireNamespace("jpeg", quietly = TRUE))
      .error(paste(
        "This function requires `jpeg` package. Please, use",
        "install.packages('jpeg')."
      ))
    ,
    jpg = if (!requireNamespace("jpeg", quietly = TRUE))
      .error(paste(
        "This function requires `jpeg` package. Please, use",
        "install.packages('jpeg')."
      ))
  )
}

# nocov start

preview_read_file <- function(url) {
  temp_file <- tempfile(fileext = paste0(".", preview_file_type(url)))
  on.exit(unlink(temp_file))
  make_get_request(
    url = url,
    httr::write_disk(path = temp_file, overwrite = TRUE),
    error_msg = "Error while downloading"
  )
  preview_switch(
    url,
    png = png::readPNG(temp_file),
    jpeg = jpeg::readJPEG(temp_file),
    jpg = jpeg::readJPEG(temp_file)
  )
}

# nocov end
