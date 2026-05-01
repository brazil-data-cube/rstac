testthat::test_that("static functions", {
  testthat::skip_on_cran()

  # read_stac with static catalog URL
  catalog_url <- "https://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json"
  cat <- rstac::read_stac(catalog_url)

  # navigate to wv_mcd19a2v061.seasconv collection using filter
  wv_links <- rstac::links(cat, grepl("wv_mcd19a2v061.seasconv[^.]", href))
  wv <- rstac::link_open(wv_links[[1]])

  testthat::expect_s3_class(
    object = wv,
    class = c("doc_collection", "rstac_doc")
  )

  # links function
  item_links <- rstac::links(wv, rel == "item")

  testthat::expect_s3_class(
    object = item_links,
    class = c("doc_links", "list")
  )

  testthat::expect_true(length(item_links) > 0)

  # link_open function
  if (length(item_links) > 0) {
    item <- rstac::link_open(item_links[[1]])

    testthat::expect_s3_class(
      object = item,
      class = c("doc_item", "rstac_doc")
    )
  }
})

testthat::test_that("read_stac normalizes local file paths", {
  testthat::skip_on_cran()

  # create a temporary STAC collection file
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  collection_json <- '{
    "type": "Collection",
    "id": "test-collection",
    "links": [
      {
        "rel": "self",
        "href": "collection.json"
      },
      {
        "rel": "item",
        "href": "items/test-item.json",
        "type": "application/json"
      }
    ]
  }'

  collection_path <- file.path(tmp_dir, "collection.json")
  writeLines(collection_json, collection_path)

  # read_stac with local file path
  col <- rstac::read_stac(collection_path)

  # check that base_url is normalized to file:// with absolute path
  # (second link is the original self link from JSON)
  testthat::expect_true(grepl("^file://", col$links[[2]]$`rstac:base_url`))

  # clean up
  unlink(tmp_dir, recursive = TRUE)
})
