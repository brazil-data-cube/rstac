testthat::test_that("signature functions", {
  vcr::use_cassette("bdc_signatures",{

    #--- BDC provider ----#

    # stac item collection from bdc
    stac_items <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(collections = "CB4_64_16D_STK-1",
                  limit = 1) %>%
      ext_query("bdc:tile" == "021027") %>%
      post_request()

    # stac item from bdc
    stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      collections("CB4_64_16D_STK-1") %>%
      items("CB4_64_16D_STK_v001_022023_2020-07-11_2020-07-26") %>%
      get_request()

    # return the same object after signature?
    testthat::expect_s3_class(
      object = items_sign(stac_items, sign_bdc("AAAA-BBB")),
      class = c("STACItemCollection", "RSTACDocument")
    )

    # return the same object after signature?
    testthat::expect_s3_class(
      object = stac_item %>% items_sign(sign_bdc("AAAA-BBB")),
      class = c("STACItem", "RSTACDocument")
    )

    items_signed <- items_sign(stac_items, sign_bdc("AAAA-BBB"))
    href_items <- items_signed$features[[1]]$assets[[1]]$href

    # is the token being added at the end of the url?
    testthat::expect_equal(
      gsub("^([^?]+)(\\?.*)?$", "\\2", href_items), "?access_token=AAAA-BBB"
    )

    item_signed <- items_sign(stac_item, sign_bdc("AAAA-BBB"))
    href_item <- item_signed$assets[[1]]$href

    # is the token being added at the end of the url?
    testthat::expect_equal(
      gsub("^([^?]+)(\\?.*)?$", "\\2", href_item), "?access_token=AAAA-BBB"
    )

    Sys.setenv("BDC_ACCESS_KEY" = "CCCC-DDD")
    items_signed <- items_sign(stac_items, sign_bdc())
    href_items <- items_signed$features[[1]]$assets[[1]]$href

    # is the token being added at the end of the url with env var?
    testthat::expect_equal(
      gsub("^([^?]+)(\\?.*)?$", "\\2", href_items), "?access_token=CCCC-DDD"
    )

    item_signed <- items_sign(stac_item, sign_bdc())
    href_item <- item_signed$assets[[1]]$href

    # is the token being added at the end of the url with env var?
    testthat::expect_equal(
      gsub("^([^?]+)(\\?.*)?$", "\\2", href_item), "?access_token=CCCC-DDD"
    )

    Sys.setenv("BDC_ACCESS_KEY" = "")

    # an error is expected if no option is provided
    testthat::expect_error(
      items_sign(stac_items, sign_bdc())
    )

    # an error is expected if no option is provided
    testthat::expect_error(
      items_sign(stac_item, sign_bdc())
    )
  })

  vcr::use_cassette("ms_signatures",{

    #--- MS provider ----#

    # stac item collection from ms
    stac_items <-
      stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
      stac_search(collections = "landsat-8-c2-l2",
                  bbox = c(-47.02148, -12.98314, -42.53906, -17.35063),
                  limit = 1) %>%
      get_request()

    # stac item from ms
    stac_item <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
      collections("landsat-8-c2-l2") %>%
      items("LC08_L2SP_217072_20210909_02_T1") %>%
      get_request()

    # return the same object after signature?
    testthat::expect_s3_class(
      object = suppressWarnings(
        items_sign(stac_items, sign_planetary_computer)
      ),
      class = c("STACItemCollection", "RSTACDocument")
    )

    # return the same object after signature?
    testthat::expect_s3_class(
      object = suppressWarnings(
        items_sign(stac_item, sign_planetary_computer())
      ),
      class = c("STACItem", "RSTACDocument")
    )

    items_signed <- suppressWarnings(
      items_sign(stac_items, sign_planetary_computer())
    )
    href_items <- items_signed$features[[1]]$assets[[1]]$href

    # is the token being added at the end of the url?
    testthat::expect_match(
      object = href_items,
      regexp = "se"
    )

    item_signed <- suppressWarnings(
      items_sign(stac_item, sign_planetary_computer())
    )
    href_item <- item_signed$assets[[1]]$href

    # is the token being added at the end of the url?
    testthat::expect_match(
      object = href_item,
      regexp = "se"
    )

    # provided wrong url
    testthat::expect_error(
      object = suppressWarnings(
        items_sign(stac_items,
                   sign_planetary_computer(token_url = "test"))
      )
    )
  })
})
