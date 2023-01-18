#' Plot preview images
#'
#' This is a helper function to plot preview assets
#' (e.g. quicklook, thumbnail, rendered_preview).
#' Currently, only png and jpeg formats are supported.
#'
#' @param url   image URL to be plotted.
#'
#' @return A rastergrob grob from package `grid`.
#'
#' @export
preview_plot <- function(url) {
  img <- preview_read_file(url)
  plot(1:10, ty = "n", axes = F, xlab = "", ylab = "")
  grid::grid.raster(img)
}

preview_file_type <- function(url) {
  gsub(".*\\.(.+)$", "\\1", url_get_path(url))
}

preview_switch <- function(url, ...) {
  type <- preview_file_type(url)
  switch(type, ..., .error("File type '%s' not supported", type))
}

# nocov start

preview_read_file <- function(url) {
  temp_file <- tempfile(fileext = paste0(".", preview_file_type(url)))
  on.exit(unlink(temp_file))
  utils::download.file(url, destfile = temp_file, quiet = TRUE)
  preview_switch(
    url,
    png = {
      if (!requireNamespace("png", quietly = TRUE))
        .error(paste(
          "This function requires `png` package. Please, use",
          "install.packages('png')."
        ))
      png::readPNG(temp_file)
    },
    jpeg = {
      if (!requireNamespace("jpeg", quietly = TRUE))
        .error(paste(
          "This function requires `jpeg` package. Please, use",
          "install.packages('jpeg')."
        ))
      jpeg::readJPEG(temp_file)
    })
}

# nocov end
