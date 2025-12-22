remove_2slash_segments <- function(path) {
  while (grepl("([^:])//", path)) {
    path <- gsub("([^:])//", "\\1/", path)
  }
  path
}

remove_dot_segments <- function(path) {
  while (grepl("[^/]+/\\.\\./?", path)) {
    path <- gsub("[^/]+/\\.\\./?", "", path)
  }
  path <- gsub("(\\./)+", "", path)
  gsub("/\\.$", "/", path)
}

remove_last_segment <- function(path) {
  gsub("/[^/]*$", "", path)
}

resolve_url <- function(url, new_url) {
  parsed_url <- httr::parse_url(url)
  parsed_url$path <- remove_2slash_segments(parsed_url$path)
  if (is.null(new_url) || new_url == "") {
    return(httr::build_url(parsed_url))
  }
  parsed_new <- httr::parse_url(new_url)
  parsed_new$path <- remove_2slash_segments(parsed_new$path)
  if (!is.null(parsed_new$scheme)) {
    return(new_url)
  } else {
    if (!is.null(parsed_new$hostname)) {
      parsed_url$hostname <- parsed_new$hostname
      parsed_url$path <- parsed_new$path
      parsed_url$query <- parsed_new$query
      parsed_url$params <- parsed_new$params
      parsed_url$fragment <- parsed_new$fragment
    } else if (parsed_new$path != "") {
      if (startsWith(parsed_new$path, "/")) {
        path <- parsed_new$path
      } else {
        path <- remove_last_segment(parsed_url$path)
        path <- paste(path, parsed_new$path, sep = "/")
      }
      parsed_url$path <- remove_dot_segments(path)
      parsed_url$query <- parsed_new$query
      parsed_url$params <- parsed_new$params
      parsed_url$fragment <- parsed_new$fragment
    } else if (!is.null(parsed_new$query)) {
      parsed_url$query <- parsed_new$query
    } else if (!is.null(parsed_new$params)) {
      parsed_url$params <- parsed_new$params
      path <- remove_last_segment(parsed_url$path)
      path <- paste(path, parsed_new$path, sep = "/")
      parsed_url$path <- remove_dot_segments(path)
      parsed_url$query <- parsed_new$query
    } else if (!is.null(parsed_new$fragment)) {
      parsed_url$fragment <- parsed_new$fragment
    }
  }
  httr::build_url(parsed_url)
}

is_url_file <- function(url) {
  parsed_url <- httr::parse_url(url)
  grepl("/[^/]+\\.[^/]+$", parsed_url$path)
}

url_normalize <- function(url) {
  if (!is_url_file(url)) {
    url <- paste0(gsub("/$", "", url), "/")
  }
  url
}

make_get_request <- function(url, ..., headers = NULL, error_msg = NULL) {
  if (!is.null(headers)) {
    headers <- httr::add_headers(headers)
  }
  tryCatch(
    {
      httr::GET(url, headers, ...)
    },
    error = function(e) {
      if (!is.null(error_msg)) {
        .error(paste(error_msg, "'%s'. \n%s"), url, e$message)
      }
    }
  )
}

make_post_request <- function(url, ..., body,
                              encode = c("json", "multipart", "form"),
                              headers = NULL,
                              error_msg = NULL) {
  # check request settings
  encode <- encode[[1]]
  check_body_encode(encode)
  if (!is.null(headers)) {
    headers <- httr::add_headers(headers)
  }
  tryCatch(
    {
      httr::POST(url, body = body, encode = encode, headers, ...)
    },
    error = function(e) {
      if (!is.null(error_msg)) {
        .error(paste(error_msg, "'%s'. \n%s"), url, e$message)
      }
    }
  )
}

query_encode <- function(params) {
  return(lapply(params, paste0, collapse = ","))
}

gdalvsi_schema <- function(url) {
  if (grepl("^(.+):.*$", url)) gsub("^(.+):.*$", "\\1", url)
}

gdalvsi_switch <- function(url, ...) {
  switch(gdalvsi_schema(url),
    ...
  )
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
  if (!is.null(bbox) & length(bbox) == 4) {
    return(paste(c("xmin:", "ymin:", "xmax:", "ymax:"),
      sprintf("%.5f", bbox),
      collapse = ", "
    ))
  }

  if (!is.null(bbox) & length(bbox) == 6) {
    return(paste(c("xmin:", "ymin:", "zmin:", "xmax:", "ymax:", "zmax:"),
      sprintf("%.5f", bbox),
      collapse = ", "
    ))
  }
}

path_normalize <- function(...) {
  path <- file.path(...)
  path <- gsub("\\\\", "/", path)
  path <- gsub("/{2,}", "/", path)
  path <- gsub("/+$", "", path)
  path <- gsub("[/\\?<>\\:*|\":]", "", path)
  path <- gsub("[[:cntrl:]]", "", path)
  path <- gsub("^[.]+$", "", path)
  path <- gsub("^(con|prn|aux|nul|com[0-9]|lpt[0-9])([.].*)?$", "", path)
  path <- gsub("[. ]+$", "", path)
  return(normalizePath(path.expand(path), mustWork = FALSE))
}

url_get_path <- function(url) {
  parsed_url <- httr::parse_url(url)
  return(parsed_url$path)
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

check_body_encode <- function(encode) {
  valid_encodes <- c("json", "multipart", "form")
  if (!encode %in% valid_encodes) {
    .error(
      "Invalid body `encode` '%s'. Allowed `encode` are %s.",
      encode, paste0("'", valid_encodes, "'", collapse = ", ")
    )
  }
}
