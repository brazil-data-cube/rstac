testthat::test_that("stac search object", {
  # skip cran check test
  testthat::skip_on_cran()
  testthat::expect_warning(
    stac("https://landsatlook.usgs.gov/sat-api/stac", force_version = "0.7.0")
  )

  testthat::expect_equal(
    suppressWarnings(
      endpoint(stac("https://landsatlook.usgs.gov/sat-api/stac",
                    force_version = "0.7.0")
      )
    ),
    expected = "/stac"
  )

  testthat::expect_equal(
    suppressWarnings(
      endpoint(stac("https://landsatlook.usgs.gov/sat-api/stac",
                    force_version = "0.8.1") %>%
                 stac_search()

      )
    ),
    expected = "/stac/search"
  )

  testthat::expect_equal(
    suppressWarnings(
      endpoint(stac("https://landsatlook.usgs.gov/sat-api/stac",
                    force_version = "1.0.0")
      )
    ),
    expected = "/"
  )

  # no stac version detected
  testthat::expect_error(
    stac("https://landsatlook.usgs.gov/sat-api/stac") %>%
      stac_search(
        collections = "landsat-c2l2-sr",
        datetime = "2019-01-01/2019-01-31",
        limit = 1) %>%
      post_request()
  )

  # Error when creating the stac object by parameter bbox
  testthat::expect_error(
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(bbox = c(-55.16335, -4.26325, -49.31739))
  )

  # check object class of stac_search
  testthat::expect_equal(
    object  = class(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(bbox = "-48.19039,-16.00871,-41.6341,-11.91345")),
    expected = c("search", "RSTACQuery")
  )

  # check object class of stac_search
  testthat::expect_s3_class(
    object  = rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(bbox = c(-48.19039, -16.00871,
                                  -41.6341, -11.91345,
                                  -18.00871, -42.12)),
    class = c("search", "RSTACQuery")
  )

  # check object class of stac_search
  testthat::expect_s3_class(
    object  = before_request(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(bbox = c(-48.19039, -16.00871,
                                    -41.6341, -11.91345,
                                    -18.00871, -42.12))),
    class = c("search", "RSTACQuery")
  )

  # check object class of stac_search
  testthat::expect_error(
    object  = endpoint(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(bbox = c(-48.19039, -16.00871,
                                    -41.6341, -11.91345,
                                    -18.00871, -42.12)))
  )

  # check object class of stac_search
  testthat::expect_error(
    object  = after_response(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(bbox = c(-48.19039, -16.00871,
                                    -41.6341, -11.91345,
                                    -18.00871, -42.12)), NULL)
  )

  # check object class of stac_search
  testthat::expect_s3_class(
    object  = rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(bbox = c(-48.19039, -16.00871,
                                  -41.6341, -11.91345)),
    class = c("search", "RSTACQuery")
  )

  # check object class of stac_search
  testthat::expect_s3_class(
    object  =  rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(limit = 10),
    class = c("search", "RSTACQuery")
  )

  testthat::expect_error(
    object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      collections("CB4_64_16D_STK-1") %>%
      items(c("CB4_64_16D_STK_v001_022023_2020-07-11_2020-07-26", "dddd"))
  )

  testthat::expect_error(
    object = suppressWarnings(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(limit = "dddd")
    )
  )

  testthat::expect_error(
    object = suppressWarnings(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(limit = c(1, 2))
    )
  )

  # check object class of stac_search
  testthat::expect_s3_class(
    object  =  rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(ids = c(1, 2)),
    class = c("search", "RSTACQuery")
  )

  # check object class of stac_search
  testthat::expect_s3_class(
    object  =  rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(intersects = list("aaa")),
    class = c("search", "RSTACQuery")
  )

  # check object class of stac_search
  testthat::expect_s3_class(
    object = rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(collections = "ssss", ids = "aaa,bbb,ccc"),
    class = c("search", "RSTACQuery")
  )

  # check GET request from stac_search object
  testthat::expect_equal(
    object   = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(bbox = c(-48.19039, -16.00871,
                                    -41.6341, -11.91345)) %>%
        get_request()),
    expected = "STACItemCollection"
  )

  # check for invalid stac endpoint
  testthat::expect_error(
    object   =  rstac::stac("https://brazildataddddde.dpi.inpe.br/stac/") %>%
      rstac::stac_search(bbox = c(-48.19039, -16.00871,
                                  -41.6341, -11.91345)) %>%
      get_request()
  )

  testthat::expect_error(
    object  =  rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(intersects = "aaa")
  )

  # Check extensions ---------------------------------------------------------

  # check extension query - wrong construction
  testthat::expect_error(
    object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      ext_query("bdc:tile" == "022024") %>%
      rstac::stac_search(datetime = "2018-01-01/..")
  )

  # check extension query - wrong query
  testthat::expect_error(
    object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(datetime = "2018-01-01/..") %>%
      ext_query("bdc:tile" == "022024", test = "test") %>%
      stac_search(datetime = "2018-01-01/..")
  )

  # check extension query - wrong parameter
  testthat::expect_error(
    object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(datetime = "2018-01-01/..") %>%
      ext_query("bdc:tile" + "022024") %>%
      stac_search(datetime = "2018-01-01/..")
  )

  # check extension query - wrong request
  testthat::expect_error(
    object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(datetime = "2018-01-01/..") %>%
      ext_query("bdc:tile" == "022024") %>%
      get_request()
  )

  s_search <-
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
    rstac::stac_search()

  # Check each operation in query extension ----------------------------------

  # 'in' operation
  testthat::expect_equal(
    object   =  subclass(ext_query(s_search, "bdc:tile" %in% "022024")),
    expected = c("ext_query", "search")
  )

  # 'neq' operation
  testthat::expect_equal(
    object   = subclass(ext_query(s_search, "bdc:tile" != "022024")),
    expected = c("ext_query", "search")
  )

  # 'eq' operation
  testthat::expect_equal(
    object   = subclass(ext_query(s_search, "bdc:tile" == "022024")),
    expected = c("ext_query", "search")
  )

  # 'lt' operation
  testthat::expect_equal(
    object   = subclass(ext_query(s_search, "eo:cloud_cover" < 50)),
    expected = c("ext_query", "search")
  )

  # 'lte' operation
  testthat::expect_equal(
    object   = subclass(ext_query(s_search, "eo:cloud_cover" <= 50)),
    expected = c("ext_query", "search")
  )

  # 'gt' operation
  testthat::expect_equal(
    object   = subclass(ext_query(s_search, "eo:cloud_cover" > 50)),
    expected = c("ext_query", "search")
  )

  # 'gte' operation
  testthat::expect_equal(
    object   = subclass(ext_query(s_search, "eo:cloud_cover" >= 50)),
    expected = c("ext_query", "search")
  )

  # 'startsWith' operation
  testthat::expect_equal(
    object   = subclass(ext_query(s_search, "bdc:tile" %startsWith% "022")),
    expected = c("ext_query", "search")
  )

  # 'endsWith' operation
  testthat::expect_equal(
    object   =  subclass(ext_query(s_search, "bdc:tile" %endsWith% "20")),
    expected = c("ext_query", "search")
  )

  # 'contains' operation
  testthat::expect_equal(
    object   =  subclass(ext_query(s_search, "bdc:tile" %contains% "20")),
    expected = c("ext_query", "search")
  )

  # wrong operation
  testthat::expect_error(
    object = ext_query(s_search, "bdc:tile" %ddddd% "20"),
  )

  # error in try to use get method for ext query
  testthat::expect_error(
    object = ext_query(s_search, "bdc:tile" == "022024") %>%
      get_request()
  )

  # sucess requisition
  testthat::expect_s3_class(
    object = ext_query(s_search, "bdc:tile" %in% "022024") %>%
      post_request(),
    class = "STACItemCollection"
  )

  # Check print function------------------------------------------------------

  # check object
  testthat::expect_output(
    object   = print(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(datetime = "2018-01-01/2018-07-01", limit = 10) %>%
        get_request(), n = 10),
    regexp = "###STACItemCollection"
  )

  testthat::expect_output(
    object   = print(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(datetime = "2018-01-01/2018-07-01", limit = 10) %>%
        get_request(), n = 10),
    regexp = "###STACItemCollection"
  )

  # Check errors in fixed date time-------------------------------------------
  # check fixed date time
  testthat::expect_error(
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(datetime = "20-02-2012T00:00:00Z")
  )

  # check fixed date time
  testthat::expect_error(
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(datetime = "20-2012-20T00:00:00Z")
  )

  # check fixed date time
  testthat::expect_error(
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(datetime = "20-02-2012")
  )

  # check fixed date time
  testthat::expect_equal(
    object     = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(datetime = "2019-02-12T00:00:00Z")),
    expected   = c("search")
  )

  # Check errors in closed date time------------------------------------------

  # check closed date time
  testthat::expect_error(
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(
        datetime = "2019-02-12T00:00:00Z/2018-03-18T12:31:12Z")
  )

  # check fixed date time
  testthat::expect_equal(
    object = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(
          datetime = "2018-02-12T00:00:00Z/2018-03-18T12:31:12Z")),
    expected = c("search")
  )

  # Check errors in interval date time----------------------------------------

  # check interval date time
  testthat::expect_error(
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(datetime = "./2018-03-18T12:31:12Z")
  )

  # check interval date time  - wrong pattern
  testthat::expect_error(
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(datetime = "../1008-03-2018T12:31:12Z")
  )

  # check interval date time
  testthat::expect_error(
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::search(datetime = "2018-03-18T12:31:12Z/.")
  )

  # check interval date time - wrong pattern
  testthat::expect_error(
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search(datetime = "20-03-2018T12:31:12Z/..")
  )

  # check fixed date time
  testthat::expect_equal(
    object   = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(datetime = "2018-03-20T12:31:12Z/..")),
    expected = c("search")
  )
})

testthat::test_that("stac collection object", {
  # skip cran check test
  testthat::skip_on_cran()

  # stac_collections----------------------------------------------------------

  # check object class of stac collections
  s_col <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
    rstac::collections() %>%
    get_request()

  testthat::expect_equal(
    object   =  subclass(s_col),
    expected = "STACCollectionList"
  )

  testthat::expect_equal(
    object   =  stac_version(s_col),
    expected = "0.9.0"
  )

  testthat::expect_equal(object = {
    mock_obj <- s_col
    attributes(mock_obj)$query <- NULL
    stac_version(mock_obj)
  },
  expected = "0.9.0"
  )

  # check print stac object
  testthat::expect_output(
    object = print(rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
                     rstac::collections()),
    regexp = "###RSTACQuery"
  )

  testthat::expect_output(
    object = print(s_col),
    regexp = "collections"
  )

  testthat::expect_equal(
    object   =  attributes(s_col)$query$endpoint,
    expected = "/collections"
  )

  # check object class of stac collections
  s_colid <-
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
    rstac::collections(collection_id = "CB4_64_16D_STK-1")

  testthat::expect_null(
    object   = s_colid$endpoint
  )

  # check print stac object
  testthat::expect_output(
    object = print(s_colid),
    regexp = "- collection_id"
  )

  # check request from stac collections object
  testthat::expect_equal(
    object   = subclass(s_colid %>% get_request()),
    expected = "STACCollection"
  )

  # check print stac_collection object
  testthat::expect_output(
    object   = print((s_colid %>% get_request())),
    regexp   = "###STACCollection"
  )
})

testthat::test_that("stac object", {
  # skip cran check test
  testthat::skip_on_cran()

  stac_catalog <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
    get_request()

  testthat::expect_equal(
    object = stac_version(stac_catalog),
    expected = "0.9.0"
  )

  # check object class of stac
  testthat::expect_equal(
    object   = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/")),
    expected = "stac"
  )

  # check request from stac object
  testthat::expect_equal(
    object   = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        get_request()),
    expected = "STACCatalog"
  )

  # check print stac_collection object
  testthat::expect_output(
    object   = print(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac") %>%
        get_request()),
    regexp   = "###STACCatalog"
  )
})

testthat::test_that("stac item object", {
  # skip cran check test
  testthat::skip_on_cran()

  # not provide collection id
  testthat::expect_error(
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      items(bbox = c(-48.19039, -16.00871, -41.6341, -11.91345))
  )

  # wrong date
  testthat::expect_error(
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      items(
        bbox       = c(-48.19039, -16.00871, -41.6341, -11.91345),
        datetime   = "2018-02-01/.",
        feature_id = "CB4_64-1")
  )

  # wrong bbox
  testthat::expect_error(
    rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      items(
        bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
        datetime      = "2018-02-01/..",
        feature_id = "CB4_64-1")
  )

  # stac_collection_list object
  testthat::expect_equal(
    object = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/",
                  force_version = "0.9.0") %>%
        collections("CB4_64-1") %>%
        items(
          bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
          limit         = 10,
          datetime      = "2018-02-01/..")),
    expected = c("items")
  )

  # stac_item object
  testthat::expect_equal(
    object = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        collections("CB4_64-1") %>%
        items(
          bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
          limit         = 10,
          datetime      = "2018-02-01/..",
          feature_id    = "MOD13Q1.A2019241.h13v09.006.2019262164754")),
    expected = c("item_id")
  )

  # stac_item object
  testthat::expect_equal(
    object = stac_version(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        collections("CB4_64-1") %>%
        items(
          bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
          limit         = 10,
          datetime      = "2018-02-01/..",
          feature_id    = "MOD13Q1.A2019241.h13v09.006.2019262164754")),
    expected = "0.9.0"
  )

  stac_item <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
    collections("CB4_64_16D_STK-1") %>%
    items(
      bbox       = c(-48.19039, -16.00871, -41.6341, -11.91345),
      limit      = 10,
      datetime   = "2018-02-01/..",
      feature_id = "CB4_64_16D_STK_v001_021027_2020-07-11_2020-07-26") %>%
    get_request()

  # output test
  testthat::expect_output(
    object   = print(stac_item),
    regexp   = "###STACItem"
  )

  # output test
  testthat::expect_equal(
    object   = items_length(stac_item),
    expected = 1
  )

  # output test
  testthat::expect_equal(
    object   = items_matched(stac_item),
    expected = 1
  )

  testthat::expect_warning(
    {
      mock_obj <- list(stac_version = "0.78.0")
      class(mock_obj) <- c("STACItemCollection", "RSTACDocument", "list")
      items_matched(mock_obj)
    }
  )

  # output test
  testthat::expect_equal(
    object   = items_length(items_fetch(stac_item)),
    expected = 1
  )
})

testthat::test_that("queryables object", {
  testthat::expect_error(
    stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      collections() %>%
      queryables()
  )

  testthat::expect_equal(
    object = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        queryables()),
    expected = c("queryables", "stac")
  )
  testthat::expect_equal(
    object = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        queryables()),
    expected = c("queryables", "stac")
  )

  testthat::expect_equal(
    object = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        collections("sentinel") %>%
        queryables()),
    expected = c("queryables", "collection_id")
  )

  testthat::expect_equal(
    object = class(
      stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
        queryables() %>%
        get_request()),
    expected = c("Queryables", "RSTACDocument", "list")
  )

  testthat::expect_equal(
    object = class(
      stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
        collections(collection_id = "sentinel-2-l2a") %>%
        queryables() %>%
        get_request()),
    expected = c("Queryables", "RSTACDocument", "list")
  )

  testthat::expect_output(
    object = print(
      stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
        collections(collection_id = "sentinel-2-l2a") %>%
        queryables() %>%
        get_request()),
    regexp = "Queryables"
  )
})

testthat::test_that("conformance object", {
  testthat::expect_error(
    stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      collections() %>%
      conformance()
  )

  testthat::expect_equal(
    object = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        conformance()),
    expected = c("conformance", "stac")
  )

  testthat::expect_equal(
    object = subclass(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        conformance()),
    expected = c("conformance", "stac")
  )

  testthat::expect_equal(
    object = class(
      stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
        conformance() %>%
        get_request()),
    expected = c("Conformance", "RSTACDocument", "list")
  )

  testthat::expect_output(
    object = print(
      stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
        conformance() %>%
        get_request()),
    regexp = "Conformance"
  )
})
