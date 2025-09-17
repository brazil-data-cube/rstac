testthat::test_that("assets functions", {
  # skip cran check test
  testthat::skip_on_cran()

  # assets_download-----------------------------------------------------------
  testthat::expect_equal(
    object = stac("https://data.inpe.br/bdc/stac/v1/") %>%
      stac_search(
        collections = "CBERS4-WFI-16D-2",
        datetime    = "2019-09-01/2019-11-01",
        bbox        = c(-47.02148, -12.98314, -42.53906, -17.35063),
        limit       = 0) %>%
      get_request() %>%
      assets_download(asset_names = c("blue", "evi"),
                      create_json = FALSE) %>%
      items_length(),
    expected = 0
  )

  testthat::expect_error(
    stac("https://data.inpe.br/bdc/stac/v1/") %>%
      get_request() %>%
      assets_download(asset_names = c("blue", "evi"))
  )

  # error - wrong path
  testthat::expect_error(
    stac("https://data.inpe.br/bdc/stac/v1/") %>%
      stac_search(collections = "CBERS4-WFI-16D-2",
                  datetime    = "2019-09-01/2019-11-01",
                  limit       = 1) %>%
      get_request() %>%
      assets_download(asset_names = c("thumbnail"),
                      output_dir = "./non-existing-dir/")
  )

  # verify output object
  testthat::expect_equal(
    object = {
      x <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
        stac_search(collections = "CBERS4-WFI-16D-2",
                    datetime    = "2019-09-01/2019-11-01",
                    limit       = 1) %>%
        get_request() %>%
        assets_download(asset_names = c("thumbnail"),
                        output_dir = tempdir(),
                        create_json = FALSE)
      subclass(x)
    },
    expected = "doc_items"
  )

  testthat::expect_equal(
    object = {
      x <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
        stac_search(collections = "CBERS4-WFI-16D-2",
                    datetime    = "2019-09-01/2019-11-01",
                    limit       = 1) %>%
        get_request() %>%
        assets_download(asset_names = c("thumbnail"),
                        items_max = 2,
                        download_fn = function(x) { x },
                        output_dir = tempdir(),
                        create_json = FALSE,
                        overwrite = TRUE)
      subclass(x)
    },
    expected = "doc_items"
  )

  testthat::expect_equal(
    object = {
      x <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
        collections("CBERS4-WFI-16D-2") %>%
        items("CB4-16D_V2_000002_20230509") %>%
        get_request() %>%
        assets_download(asset_names = c("thumbnail"),
                        output_dir = tempdir(),
                        create_json = FALSE,
                        overwrite = TRUE)
      subclass(x)
    },
    expected = "doc_item"
  )

  testthat::expect_equal(
    object = {
      x <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
        collections("CBERS4-WFI-16D-2") %>%
        items("CB4-16D_V2_000002_20230509") %>%
        get_request() %>%
        assets_download(asset_names = c("thumbnail"),
                        items_max = 2,
                        download_fn = function(x) {x},
                        output_dir = tempdir(),
                        create_json = FALSE,
                        overwrite = TRUE)
      subclass(x)
    },
    expected = "doc_item"
  )

  stac_items <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
    stac_search(collections = "CBERS4-WFI-16D-2") %>%
    stac_search(limit = 2) %>%
    get_request()

  stac_item <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
    collections("CBERS4-WFI-16D-2") %>%
    items("CB4-16D_V2_000002_20230509") %>%
    get_request()

  # assets_select-----------------------------------------------------------
  # return the same object after select?
  testthat::expect_s3_class(
    object = assets_select(stac_items, asset_names = "BAND13"),
    class = c("doc_items", "rstac_doc")
  )

  # return the same object after select?
  testthat::expect_s3_class(
    object = assets_select(stac_item, asset_names = "BAND13"),
    class = c("doc_item", "rstac_doc")
  )

  # were the asset selected?
  testthat::expect_equal(
    object = items_assets(assets_select(stac_items, asset_names = "BAND13")),
    expected = "BAND13"
  )

  # were the asset selected?
  testthat::expect_equal(
    object = items_assets(
      assets_select(stac_items,
                    `eo:bands`[[1]]$min == -10000,
                    asset_names = c("BAND14", "EVI"))),
    expected = "EVI"
  )

  # were the asset selected?
  testthat::expect_equal(
    object = items_assets(assets_select(stac_item, asset_names = "BAND13")),
    expected = "BAND13"
  )

  expect_error(assets_select(stac_item, select_fn = function(x) x))
  expect_error(assets_select(stac_item, select_fn = function(x) c(TRUE, FALSE)))

  expect_error(asset_get("eo:bands"))
  expect_equal(
    object = items_assets(
      assets_select(stac_item,
                    "green" %in% asset_get("eo:bands")[[1]]$common_name)
    ),
    expected = "BAND14"
  )

  expect_length(
    object = items_assets(
      suppressWarnings(assets_select(stac_item, 10 %in% asset_get("eo:band")))
    ),
    n = 0
  )

  expect_length(
    object = items_assets(
      suppressWarnings(assets_select(stac_item, "B1" %in% asset_get("eo:band")))
    ),
    n = 0
  )

  # assets_rename-------------------------------------------------------------
  selected_items <- assets_select(stac_items,
                                  asset_names = c("BAND13", "BAND14"))
  selected_item <- assets_select(stac_item,
                                 asset_names = c("BAND13", "BAND14"))

  testthat::expect_error(
    assets_rename(selected_item)
  )

  testthat::expect_error(
    assets_rename(selected_item, "BAND13" = "B14", "B14")
  )

  testthat::expect_s3_class(
    object = assets_rename(selected_items, c("BAND13" = "B13")),
    class = c("doc_items", "rstac_doc")
  )

  testthat::expect_s3_class(
    object = assets_rename(selected_item, c("BAND13" = "B13")),
    class = c("doc_item", "rstac_doc")
  )

  testthat::expect_s3_class(
    object = assets_rename(selected_items, list("BAND13" = "B13")),
    class = c("doc_items", "rstac_doc")
  )

  testthat::expect_s3_class(
    object = assets_rename(selected_item, list("BAND13" = "B13")),
    class = c("doc_item", "rstac_doc")
  )

  testthat::expect_s3_class(
    object = assets_rename(selected_items,
                           list(BAND13 = "B13"),
                           BAND14 = "B14"),
    class = c("doc_items", "rstac_doc")
  )

  testthat::expect_s3_class(
    object = assets_rename(selected_item,
                           list(BAND13 = "B13"),
                           BAND14 = "B14"),
    class = c("doc_item", "rstac_doc")
  )

  testthat::expect_equal(
    object = items_assets(assets_rename(selected_items,
                                        list(BAND13 = "B13"),
                                        BAND14 = "B14")),
    expected = c("B13", "B14")
  )

  testthat::expect_equal(
    object = items_assets(assets_rename(selected_item,
                           list(BAND13 = "B13"),
                           BAND14 = "B14")),
    expected = c("B13", "B14")
  )

  testthat::expect_equal(
    object = items_assets(
      assets_rename(selected_item, mapper = function(x) {
        paste0(x[["eo:bands"]][[1]]$common_name)
      })
    ),
    expected = c("blue", "green")
  )

  # assets_url----------------------------------------------------------
  testthat::expect_equal(
    object =  class(assets_url(stac_items)),
    expected = "character"
  )

  testthat::expect_equal(
    object = class(assets_url(stac_item)),
    expected = "character"
  )

  testthat::expect_equal(
    object =  gdalvsi_append("s3://abc.com"),
    expected = "/vsis3/abc.com"
  )

  testthat::expect_equal(
    object =  gdalvsi_append("gs://abc.com"),
    expected = "/vsigs/abc.com"
  )

  testthat::expect_equal(
    object = gdalvsi_append("http://abc.com"),
    expected = "/vsicurl/http://abc.com"
  )

  testthat::expect_equal(
    object = gdalvsi_append("https://abc.com"),
    expected = "/vsicurl/https://abc.com"
  )

  testthat::expect_equal(
    object =  gdalvsi_append("httpdds://abc.com"),
    expected = "httpdds://abc.com"
  )

  testthat::expect_error(
    assets_select(stac_items, filter_fn = function(x) {
      if ("eo:bands" %in% names(x))
        return(x$`eo:bands` < 6)
      return(FALSE)
    })
  )

  testthat::expect_error(
    assets_select(stac_items, filter_fn = function(x) {
      if ("eo:bands" %in% names(x))
        return(x$`eo:bands` < 6)
      return(FALSE)
    })
  )

  testthat::expect_s3_class(
    object = assets_select(stac_item, select_fn = function(x) {
      if ("eo:bands" %in% names(x))
        return(x$`eo:bands`[[1]]$min == 0)
      return(FALSE)
    }),
    class = c("doc_item", "rstac_doc")
  )

  # return the same object after filter?
  testthat::expect_error(
    object = assets_select(stac_items, filter_fn = function(x) {
      if ("eo:bands" %in% names(x))
        return(x$`eo:bands`[[1]]$min == 0)
      return(FALSE)
    })
  )

  # assets_select-----------------------------------------------------------
  testthat::expect_equal(
    object = {class(assets_select(stac_items, `eo:bands`[[1]]$min == 0))},
    expected = c("doc_items", "rstac_doc", "list")
  )

  testthat::expect_equal(
    object = {class(assets_select(stac_items, select_fn = function(x) {
      if ("eo:bands" %in% names(x))
        return(x$`eo:bands`[[1]]$min == 0)
      return(FALSE)
    }))},
    expected = c("doc_items", "rstac_doc", "list")
  )

  testthat::expect_equal(
    object = class(assets_select(stac_item, `eo:bands`[[1]]$min == 0)),
    expected = c("doc_item", "rstac_doc", "list")
  )

  testthat::expect_error(
    object = assets_select(stac_item, a = `eo:bands` < 6),
  )

  testthat::expect_warning(
    object = assets_select(stac_item, `eo:dbandsd` < 6),
  )

  testthat::expect_error(
    object = assets_select(stac_items, a = `eo:bands` < 6),
  )

  testthat::expect_warning(
    object = assets_select(stac_items, `eo:dbandsd` < 6),
  )

  testthat::expect_equal(
    object = {
      class(assets_select(stac_item, select_fn = function(x) {
        if ("eo:bands" %in% names(x))
          return(x$`eo:bands`[[1]]$min == 0)
        return(FALSE)
      }))
    },
    expected = c("doc_item", "rstac_doc", "list")
  )
})
