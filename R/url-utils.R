make_url <- function(url, endpoint = "", params = list()) {
  # remove trailing '/' char
  if (substring(url, nchar(url)) == "/")
    url <- substring(url, 1, nchar(url) - 1)

  endpoint <- paste0(endpoint, collapse = "/")

  res <- paste0(url, endpoint)

  return(res)
}

make_get_request <- function(url, ..., error_msg = "Error while requesting") {
  tryCatch({
    httr::GET(url, ...)
  },
  error = function(e) {
    if (!is.null(error_msg))
      .error(paste(error_msg, "'%s'. \n%s"), url, e$message)
  })
}

.querystrings_encode <- function(params) {
  return(lapply(params, paste0, collapse = ","))
}

.querystring_decode <- function(querystring) {
  # first decode and remove all coded spaces
  querystring <- URLdecode(querystring)
  querystring_spplited <- strsplit(querystring, split = "&")[[1]]
  # remove empty spaces
  querystring_spplited <- querystring_spplited[nzchar(querystring_spplited)]
  values <- lapply(querystring_spplited,
                   function(x) regmatches(x, regexpr("=", x), invert = TRUE)[[1]])
  params <- lapply(values, `[[`, 2)
  names(params) <- map_chr(values, `[[`, 1)
  return(params)
}

.validate_query <- function(params) {

  if (!is.null(params$query) && is.character(params$query)) {
    params$query <- jsonlite::fromJSON(params$query, simplifyVector = FALSE)

    if (is.list(params$query))
      params$query <- list(params$query)
  }

  return(params)
}

gdalvsi_schema <- function(url) {
  if (grepl("^(.+):.*$", url)) gsub("^(.+):.*$", "\\1", url)
}

gdalvsi_switch <- function(url, ...) {
  switch(gdalvsi_schema(url), ...)
}

gdalvsi_append <- function(url) {
  map_chr(url, function(x) {
    gdalvsi_switch(
      x,
      https = , http = paste("/vsicurl", x, sep = "/"),
      s3 = paste("/vsis3", gsub("^s3://", "", x), sep = "/"),
      gs = paste("/vsigs", gsub("^gs://", "", x), sep = "/"),
      url
    )
  })
}

# bbox is a numeric vector provided as four or six numbers, depending on
# whether the coordinate reference system includes a vertical axis
# (elevation or depth):
# - xmin, ymin, zmin (optional)
# - xmax, ymax, zmax (optional).
format_bbox <- function(bbox) {
  if (!is.null(bbox) & length(bbox) == 4)
    return(paste(c("xmin:", "ymin:", "xmax:", "ymax:"),
                 sprintf("%.5f", bbox), collapse = ", "))

  if (!is.null(bbox) & length(bbox) == 6)
    return(paste(c("xmin:", "ymin:", "zmin:", "xmax:", "ymax:", "zmax:"),
                 sprintf("%.5f", bbox), collapse = ", "))
}

asset_download <- function(asset,
                           output_dir,
                           overwrite, ...,
                           download_fn = NULL) {
  if (!is.null(download_fn))
    return(download_fn(asset))

  # create a full path name
  path <- url_get_path(asset$href)
  out_file <- path_normalize(output_dir, path)
  dir_create(out_file)

  make_get_request(
    url = asset$href,
    httr::write_disk(path = out_file, overwrite = overwrite),
    ...,
    error_msg = "Error in downloading"
  )
  asset$href <- path

  asset
}

path_normalize <- function(...) {
  path <- file.path(...)
  path <- gsub("\\\\", "/", path)
  path <- gsub("/{2,}", "/", path)
  path <- gsub("/+$", "", path)
  return(path.expand(path))
}

url_get_path <- function(url) {
  return(httr::parse_url(url)[["path"]])
}

dir_create <- function(path) {
  path <- path_get_dir(path)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    if (!dir.exists(path)) {
      .error("Cannot create directory '%s'", path)
    }
  }
  return(path)
}

path_get_dir <- function(path) {
  return(gsub("^\\.", "", dirname(path)))
}
