testthat::test_that("signature functions", {
    #---- BDC provider ----#
    # stac item collection from bdc
    stac_items <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
      stac_search(collections = "CBERS4-WFI-16D-2", limit = 1) %>%
      ext_query("bdc:tile" == "007004") %>%
      post_request()

    # stac item from bdc
    stac_item <- stac("https://data.inpe.br/bdc/stac/v1/") %>%
      collections("CBERS4-WFI-16D-2") %>%
      items("CB4-16D_V2_000002_20230509") %>%
      get_request()

    # return the same object after signature?
    testthat::expect_s3_class(
      object = stac_items %>% items_sign(sign_fn = sign_bdc("AAAA-BBB")),
      class = c("doc_items", "rstac_doc")
    )

    # return the same object after signature?
    testthat::expect_s3_class(
      object = stac_item %>% items_sign(sign_fn = sign_bdc("AAAA-BBB")),
      class = c("doc_item", "rstac_doc")
    )

    items_signed <- items_sign(stac_items, sign_fn = sign_bdc("AAAA-BBB"))
    href_items <- items_signed$features[[1]]$assets[[1]]$href

    # is the token being added at the end of the url?
    testthat::expect_equal(
      gsub("^([^?]+)(\\?.*)?$", "\\2", href_items), "?access_token=AAAA-BBB"
    )

    item_signed <- items_sign(stac_item, sign_fn = sign_bdc("AAAA-BBB"))
    href_item <- item_signed$assets[[1]]$href

    # is the token being added at the end of the url?
    testthat::expect_equal(
      gsub("^([^?]+)(\\?.*)?$", "\\2", href_item), "?access_token=AAAA-BBB"
    )

    Sys.setenv("BDC_ACCESS_KEY" = "CCCC-DDD")
    items_signed <- items_sign(stac_items, sign_fn = sign_bdc())
    href_items <- items_signed$features[[1]]$assets[[1]]$href

    # is the token being added at the end of the url with env var?
    testthat::expect_equal(
      gsub("^([^?]+)(\\?.*)?$", "\\2", href_items), "?access_token=CCCC-DDD"
    )

    item_signed <- items_sign(stac_item, sign_fn = sign_bdc())
    href_item <- item_signed$assets[[1]]$href

    # is the token being added at the end of the url with env var?
    testthat::expect_equal(
      gsub("^([^?]+)(\\?.*)?$", "\\2", href_item), "?access_token=CCCC-DDD"
    )

    Sys.setenv("BDC_ACCESS_KEY" = "")

    # an error is expected if no option is provided
    testthat::expect_error(
      items_sign(stac_items, sign_fn = sign_bdc())
    )

    # an error is expected if no option is provided
    testthat::expect_error(
      items_sign(stac_item, sign_fn = sign_bdc())
    )

    #---- MS provider ----#

    # stac item collection from ms
    stac_items <-
      stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
      stac_search(collections = "sentinel-2-l2a",
                  bbox = c(-47.02148, -42.53906, -12.98314, -17.35063),
                  limit = 1) %>%
      get_request()

    # stac item from ms
    stac_item <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
      collections("landsat-c2-l2") %>%
      items("LC09_L2SR_083075_20230608_02_T1") %>%
      get_request()

    # return the same object after signature?
    testthat::expect_s3_class(
      object = suppressWarnings(
        items_sign(stac_items, sign_planetary_computer)
      ),
      class = c("doc_items", "rstac_doc")
    )

    # provided wrong url
    testthat::expect_error(
      object = suppressWarnings(
        items_sign(stac_items,
                   sign_fn = sign_planetary_computer(token_url = "test"))
      )
    )

    # return the same object after signature?
    testthat::expect_s3_class(
      object = suppressWarnings(
        items_sign(stac_item, sign_fn = sign_planetary_computer())
      ),
      class = c("doc_item", "rstac_doc")
    )

    items_signed <- suppressWarnings(
      items_sign(stac_items, sign_fn = sign_planetary_computer())
    )
    href_items <- items_signed$features[[1]]$assets[[1]]$href

    # is the token being added at the end of the url?
    testthat::expect_match(
      object = href_items,
      regexp = "se"
    )

    item_signed <- suppressWarnings(
      items_sign(stac_item, sign_fn = sign_planetary_computer())
    )
    href_item <- item_signed$assets[[1]]$href

    # is the token being added at the end of the url?
    testthat::expect_match(
      object = href_item,
      regexp = "se"
    )
})
