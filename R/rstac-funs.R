# doc_items <- function(items) {
#   if (!"features" %in% names(items)) {
#     stac_version <- "1.0.0"
#     if (length(items) > 0 && "stac_version" %in% names(items[[1]])) {
#       stac_version <- items[[1]]$stac_version
#     }
#     items <- list(
#       type = "FeatureCollection",
#       stac_version = stac_version,
#       features = items
#     )
#   }
#   items$features <- lapply(items$features, doc_item)
#   structure(items, class = c("doc_items", "rstac_doc", "list"))
# }
