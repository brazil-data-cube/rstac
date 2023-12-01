links <- function(x, ...) {
  exprs <- as.list(substitute(list(...)))[-1]
  sel <- !logical(length(x$links))
  for (expr in exprs) {
    sel <- sel & map_lgl(x$links, function(x) eval(expr, envir = x))
  }
  structure(x$links[sel], class = c("doc_links", "list"))
}

link_open <- function(link) {
  if (is.list(link)) {
    check_link(link)
    url <- link$href
    if ("rstac:base_url" %in% names(link))
      url <- resolve_url(link[["rstac:base_url"]], url)
  } else if (is.character(link))
    url <- link
  content <- jsonlite::read_json(url)
  # create an rstac doc from content and return
  as_rstac_doc(content)
}
