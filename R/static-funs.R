#' @rdname request
#' @export
stac_read <- function(url, ...) {
  check_character(url, "STAC URL must be a character value.")
  content <- jsonlite::read_json(url)
  # create an rstac doc from content and return
  as_rstac_doc(content, base_url = url)
}

read_items <- function(collection, limit = 100, page = 1, progress = TRUE) {
  check_collection(collection)
  rel <- NULL
  link_items <- links(collection, rel == "item")
  if (is.null(limit) || limit < 1)
    limit <- length(link_items)
  limit <- max(1, as.integer(limit))
  page <- max(1, as.integer(page))
  pages <- ceiling(length(link_items) / limit)
  if (page > pages)
    return(NULL)
  if (length(link_items) > limit) {
    previous_len <- (page - 1) * limit
    len <- min(limit, length(link_items) - previous_len)
    link_items <- link_items[previous_len + seq_len(len)]
  }

  # verify if progress bar can be shown
  progress <- progress && length(link_items) > 1
  if (progress) {
    pb <- utils::txtProgressBar(max = length(link_items), style = 3)
    # close progress bar when exit
    on.exit(if (progress) close(pb))
  }
  features <- list()
  for (i in seq_along(link_items)) {
    if (progress)
      utils::setTxtProgressBar(pb, i)
    features <- c(features, list(link_open(link_items[[i]])))
  }
  # Convert to doc_items object and return
  doc_items(
    x = list(type = "FeatureCollection", features = features),
    base_url = url
  )
}
