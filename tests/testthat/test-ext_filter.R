conformance_test <- function(q, expected_number) {
  expect_equal(
    object = items_matched(get_request(q)),
    expected = expected_number
  )
}

test_that("doc_conformance Test 7", {
  skip_if({
    httr::http_error(httr::HEAD("https://cql2test.ldproxy.net/ne110m4cql2"))
  }, "Test server is offline")
  q <- rstac::stac(base_url = "https://cql2test.ldproxy.net/ne110m4cql2",
                   force_version = "0.9.0")
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      NAME == "Luxembourg"
    ),
    expected_number = 1
  )

  req <- stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
      stac_search(limit = 5)

  polygon <- list(
    type = "Polygon",
    coordinates = list(
      matrix(c(-62.34499836, -8.57414572,
               -62.18858174, -8.57414572,
               -62.18858174, -8.15351185,
               -62.34499836, -8.15351185,
               -62.34499836, -8.57414572),
             ncol = 2, byrow = TRUE)
    )
  )
  # 'S_INTERSECTS' spatial operator with polygon and geometry property
  res <- req %>%
    ext_filter(collection == "sentinel-2-l2a" &&
                 s_intersects(geometry, {{polygon}}) &&
                 datetime > "2019-01-01" &&
                 datetime < "2019-02-02")
  res <- post_request(res)

  expect_s3_class(res, "doc_items")
  res2 <- items_next(res)
  expect_s3_class(res2, "doc_items")
  expect_equal(object = items_length(res2), expected = items_length(res))

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      NAME >= "Luxembourg"
    ),
    expected_number = 84
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      NAME > "Luxembourg"
    ),
    expected_number = 83
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      NAME <= "Luxembourg"
    ),
    expected_number = 94
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      NAME < "Luxembourg"
    ),
    expected_number = 93
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      NAME != "Luxembourg"
    ),
    expected_number = 176
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      POP_EST == 37589262
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      POP_EST >= 37589262
    ),
    expected_number = 39
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      POP_EST > 37589262
    ),
    expected_number = 38
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      POP_EST <= 37589262
    ),
    expected_number = 139
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      POP_EST < 37589262
    ),
    expected_number = 138
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      POP_EST != 37589262
    ),
    expected_number = 176
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      !is_null(name)
    ),
    expected_number = 243
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      is_null(name)
    ),
    expected_number = 0
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      name == 'København'
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      !is_null(start)
    ),
    expected_number = 3
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      !is_null(`"date"`)
    ),
    expected_number = 3
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      is_null(`"date"`)
    ),
    expected_number = 240
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      `"date"` == date('2022-04-16')
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      `"date"` >= date('2022-04-16')
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      `"date"` > date('2022-04-16')
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      `"date"` <= date('2022-04-16')
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      `"date"` < date('2022-04-16')
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      `"date"` != date('2022-04-16')
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      start == timestamp('2022-04-16T10:13:19Z')
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      boolean == TRUE
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      boolean == FALSE
    ),
    expected_number = 1
  )
})

test_that("doc_conformance Test 12", {
  skip_if({
    httr::http_error(httr::HEAD("https://cql2test.ldproxy.net/ne110m4cql2"))
  }, "Test server is offline")
  q <- rstac::stac(base_url = "https://cql2test.ldproxy.net/ne110m4cql2",
                   force_version = "0.9.0")
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      name %like% "B_r%"
    ),
    expected_number = 3
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      !name %like% "B_r%"
    ),
    expected_number = 240
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      between(pop_other, 1000000, 3000000)
    ),
    expected_number = 75
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      !between(pop_other, 1000000, 3000000)
    ),
    expected_number = 168
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      name %in% c('Kiev','kobenhavn','Berlin','athens','foo')
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      !name %in% c('Kiev','kobenhavn','Berlin','athens','foo')
    ),
    expected_number = 241
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      pop_other %in% c(1038288,1611692,3013258,3013257,3013259)
    ),
    expected_number = 3
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      !pop_other %in% c(1038288,1611692,3013258,3013257,3013259)
    ),
    expected_number = 240
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      `"date"` %in% c(date('2021-04-16'), date('2022-04-16'),
                      date('2022-04-18'))
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      !`"date"` %in% c(date('2021-04-16'), date('2022-04-16'),
                       date('2022-04-18'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      start %in% c(timestamp('2022-04-16T10:13:19Z'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      !start %in% c(timestamp('2022-04-16T10:13:19Z'))
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      boolean %in% c(TRUE)
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      !boolean %in% c(FALSE)
    ),
    expected_number = 2
  )
})

test_that("doc_conformance Test 16", {
  skip_if({
    httr::http_error(httr::HEAD("https://cql2test.ldproxy.net/ne110m4cql2"))
  }, "Test server is offline")
  q <- rstac::stac(base_url = "https://cql2test.ldproxy.net/ne110m4cql2",
                   force_version = "0.9.0")

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      casei(name) == casei('KIEV')
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      casei(name) == casei('kiev')
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      casei(name) == casei('Kiev')
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      casei(name) == casei('København')
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      casei(name) == casei('københavn')
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      casei(name) == casei('KØBENHAVN')
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      casei(name) %like% casei('B_r%')
    ),
    expected_number = 3
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      casei(name) %like% casei('b_r%')
    ),
    expected_number = 3
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      casei(name) %like% casei('B_R%')
    ),
    expected_number = 3
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      casei(name) %in% c(casei('Kiev'), casei('kobenhavn'),
                         casei('Berlin'),casei('athens'),
                         casei('foo'))
    ),
    expected_number = 3
  )
})

test_that("doc_conformance Test 25", {
  skip_if({
    httr::http_error(httr::HEAD("https://cql2test.ldproxy.net/ne110m4cql2"))
  }, "Test server is offline")
  q <- rstac::stac(base_url = "https://cql2test.ldproxy.net/ne110m4cql2",
                   force_version = "0.9.0")

  polygon <- list(
    type = "Polygon",
    coordinates = list(
      rbind(
        c(0, 40),
        c(10, 40),
        c(10, 50),
        c(0, 50),
        c(0, 40)
      )
    )
  )
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_intersects(geom, {{polygon}})
    ),
    expected_number = 8
  )

  linestring <- list(
    type = "LineString",
    coordinates = rbind(
      c(0, 40),
      c(10, 50)
    )
  )
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_intersects(geom, {{linestring}})
    ),
    expected_number = 4
  )

  point <- list(
    type = "Point",
    coordinates = c(7.02, 49.92)
  )
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_intersects(geom, {{point}})
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      s_intersects(geom, {{polygon}})
    ),
    expected_number = 7
  )

  linestring <- list(
    type = "LineString",
    coordinates = rbind(
      c(-60, -90),
      c(-60, 90)
    )
  )
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_rivers_lake_centerlines")),
      s_intersects(geom, {{linestring}})
    ),
    expected_number = 2
  )
})

test_that("doc_conformance Test 34", {
  skip_if({
    httr::http_error(httr::HEAD("https://cql2test.ldproxy.net/ne110m4cql2"))
  }, "Test server is offline")
  q <- rstac::stac(base_url = "https://cql2test.ldproxy.net/ne110m4cql2",
                   force_version = "0.9.0")
  polygon <- list(
    type = "Polygon",
    coordinates = list(
      rbind(
        c(0, 40),
        c(10, 40),
        c(10, 50),
        c(0, 50),
        c(0, 40)
      )
    )
  )
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_disjoint(geom, {{polygon}})
    ),
    expected_number = 169
  )

  linestring <- list(
    type = "LineString",
    coordinates = rbind(
      c(0, 40),
      c(10, 50)
    )
  )
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_disjoint(geom, {{linestring}})
    ),
    expected_number = 173
  )

  point <- list(
    type = "Point",
    coordinates = c(7.02, 49.92)
  )
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_disjoint(geom, {{point}})
    ),
    expected_number = 176
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      s_disjoint(geom, {{polygon}})
    ),
    expected_number = 236
  )

  linestring <- list(
    type = "LineString",
    coordinates = rbind(
      c(-60, -90),
      c(-60, 90)
    )
  )
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_rivers_lake_centerlines")),
      s_disjoint(geom, {{linestring}})
    ),
    expected_number = 11
  )

  point <- "{\"type\":\"Point\",\"coordinates\":[6.1300028,49.6116604]}"
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      s_equals(geom, {{point}})
    ),
    expected_number = 1
  )

  polygon <- "POLYGON((6.043073357781111 50.128051662794235,6.242751092156993 49.90222565367873,6.186320428094177 49.463802802114515,5.897759230176348 49.44266714130711,5.674051954784829 49.529483547557504,5.782417433300907 50.09032786722122,6.043073357781111 50.128051662794235))"
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_touches(geom, {{polygon}})
    ),
    expected_number = 3
  )

  options(stac_digits = 16)
  point <- "{\"type\":\"Point\",\"coordinates\":[6.242751092156993,49.90222565367873]}"
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_touches(geom, {{point}})
    ),
    expected_number = 2
  )

  linestring <- "{\"type\":\"LineString\",\"coordinates\":[[6.043073357781111,50.128051662794235],[6.242751092156993,49.90222565367873]]}"
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_touches(geom, {{linestring}})
    ),
    expected_number = 3
  )

  linestring <- list(
    type = "LineString",
    coordinates = rbind(
      c(-60, -90),
      c(-60, 90)
    )
  )
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_rivers_lake_centerlines")),
      s_crosses(geom, {{linestring}})
    ),
    expected_number = 2
  )

  linestring <- list(
    type = "LineString",
    coordinates = rbind(
      c(7, 50),
      c(8, 51)
    )
  )
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_contains(geom, {{linestring}})
    ),
    expected_number = 1
  )

  point <- list(
    type = "Point",
    coordinates = c(7.02, 49.92)
  )
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_contains(geom, {{point}})
    ),
    expected_number = 1
  )
})

test_that("doc_conformance Test 38", {
  skip_if({
    httr::http_error(httr::HEAD("https://cql2test.ldproxy.net/ne110m4cql2"))
  }, "Test server is offline")
  q <- rstac::stac(base_url = "https://cql2test.ldproxy.net/ne110m4cql2",
                   force_version = "0.9.0")

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_after(`"date"`,date('2022-04-16'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_before(`"date"`,date('2022-04-16'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_disjoint(`"date"`,date('2022-04-16'))
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_equals(`"date"`,date('2022-04-16'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_intersects(`"date"`,date('2022-04-16'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_after(`"date"`, interval('2022-01-01','2022-12-31'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_before(`"date"`, interval('2022-01-01','2022-12-31'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_disjoint(`"date"`, interval('2022-01-01','2022-12-31'))
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_equals(`"date"`, interval('2022-01-01','2022-12-31'))
    ),
    expected_number = 0
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_equals(`"date"`, interval('2022-04-16','2022-04-16'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_intersects(start,timestamp('2022-04-16T10:13:19Z'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_before(start,timestamp('2022-04-16T10:13:19Z'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_disjoint(start,timestamp('2022-04-16T10:13:19Z'))
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_equals(start,timestamp('2022-04-16T10:13:19Z'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_equals(interval(start,end),interval('2021-04-16T10:15:59Z','2022-04-16T10:16:06Z'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_intersects(interval(start,end),interval('2022-04-16T10:13:19Z','2022-04-16T10:15:09Z'))
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_contains(interval(start,end),interval('2022-04-16T10:13:19Z','2022-04-16T10:15:09Z'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_meets(interval(start,end),interval('2022-04-16T10:13:19Z','2022-04-16T10:15:10Z'))
    ),
    expected_number = 0
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_metby(interval(start,end),interval('2022-04-16T10:13:19Z','2022-04-16T10:15:10Z'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_overlappedby(interval(start,end),interval('2020-04-16T10:13:19Z','2022-04-16T10:15:10Z'))
    ),
    expected_number = 2
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_overlaps(interval(start,end),interval('2022-04-16T10:13:19Z','2023-04-16T10:15:10Z'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_startedby(interval(start,end),interval('2022-04-16T10:13:19Z','2023-04-16T10:15:10Z'))
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      t_starts(interval(start,end),interval('2022-04-16T10:13:19Z','2023-04-16T10:15:10Z'))
    ),
    expected_number = 0
  )
})

test_that("doc_conformance Test 45", {
  skip_if({
    httr::http_error(httr::HEAD("https://cql2test.ldproxy.net/ne110m4cql2"))
  }, "Test server is offline")
  q <- rstac::stac(base_url = "https://cql2test.ldproxy.net/ne110m4cql2",
                   force_version = "0.9.0")
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      "København" == name
    ),
    expected_number = 1
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      "København" <= name
    ),
    expected_number = 137
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      "København" < name
    ),
    expected_number = 136
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      "København" >= name
    ),
    expected_number = 107
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      "København" > name
    ),
    expected_number = 106
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      "København" != name
    ),
    expected_number = 242
  )

  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      name == nameascii
    ),
    expected_number = 230
  )
})

test_that("scalar data types contructors", {
  # "string": character strings
  expect_output(cql2_text("string"), regexp = "'string'")
  expect_output(cql2_json("string"), regexp = '"string"')

  # "number": numbers including integers and floating point values
  expect_output(cql2_json(3.14), regexp = '3.14')
  expect_output(cql2_text(3.14), regexp = "3.14")

  # integer
  expect_output(cql2_json(3L), regexp = '3')
  expect_output(cql2_text(3L), regexp = "3")

  # "boolean": booleans
  expect_output(cql2_json(TRUE), regexp = 'true')
  expect_output(cql2_text(TRUE), regexp = 'true')
  expect_output(cql2_json(T), regexp = 'true')
  expect_output(cql2_text(T), regexp = 'true')
  expect_output(cql2_json(FALSE), regexp = 'false')
  expect_output(cql2_text(FALSE), regexp = 'false')

  boolean4 <- cql2(F)
  expect_output(cql2_json(F), regexp = 'false')
  expect_output(cql2_text(F), regexp = 'false')

  # "timestamp": an instant with a granularity of a second or smaller
  expect_output(
    cql2_json(timestamp("1985-07-16T05:32:00Z")),
    regexp = '{"timestamp":"1985-07-16T05:32:00Z"}',
    fixed = TRUE
  )
  expect_output(
    cql2_text(timestamp("1985-07-16T05:32:00Z")),
    regexp = "TIMESTAMP('1985-07-16T05:32:00Z')",
    fixed = TRUE
  )

  # "date": an instant with a granularity of a day
  expect_output(
    cql2_json(date("1985-07-16")),
    regexp = '{"date":"1985-07-16"}',
    fixed = TRUE
  )
  expect_output(
    cql2_text(date("1985-07-16")),
    regexp = "DATE('1985-07-16')",
    fixed = TRUE
  )
})

test_that("cql2 contructors", {
  # ---- new logic op ----
  logic_operator <- new_logic_op("and")
  expect_error(logic_operator(FALSE, NULL))
  expect_error(logic_operator(FALSE, "NULL"))
  expect_error(logiSSSc_operator(FALSE, 3))
  l_op <- logic_operator(TRUE, TRUE)
  expect_s3_class(l_op, "cql2_logic_op")
  expect_output(print(l_op), "true AND true")

  # ---- not op ----
  not_operator <- not_op(FALSE)
  expect_s3_class(not_operator, "cql2_not_op")
  expect_output(print(not_operator), "NOT false")
  expect_output(
    cat(to_json(not_operator)), '{"op":"not","args":[false]}', fixed = TRUE
  )

  # ---- new comp op ----
  new_comp_operator <- new_comp_op("=")
  comp_op <- new_comp_operator(1, 2)
  expect_s3_class(comp_op, "cql2_comp_op")
  expect_output(print(comp_op), regexp = "1 = 2")

  # ---- is null op ----
  null_operator <- isnull_op(1)
  expect_s3_class(null_operator, "cql2_isnull_op")
  expect_output(print(null_operator), regexp = "1 IS NULL")
  expect_output(
    cat(to_json(null_operator)),
    '{"op":"isNull","args":[1]}',
    fixed = TRUE
  )

  # ---- new math op ----
  new_math_operator <- new_math_op("+")
  expect_error(new_math_operator("a", 2))
  expect_error(new_math_operator(NULL, 2))
  math_op <- new_math_operator(1, 2)
  expect_s3_class(math_op, "cql2_math_op")
  expect_output(print(math_op), regexp = "1 + 2", fixed = TRUE)
  expect_output(
    cat(to_json(math_op)),
    '{"op":"+","args":[1,2]}',
    fixed = TRUE
  )

  # ---- minus op ----
  expect_error(minus_op("ddd"))
  expect_error(minus_op(NULL))
  expect_error(minus_op(TRUE))
  minus_operator1 <- minus_op(1)
  minus_operator2 <- minus_op(1, 2)
  expect_s3_class(minus_operator1, "cql2_minus_op")
  expect_output(print(minus_operator1), "-1")
  expect_output(
    cat(to_json(minus_operator1)),
    '-1',
    fixed = TRUE
  )

  expect_s3_class(minus_operator2, "cql2_minus_op")
  expect_output(print(minus_operator2), "1 - 2")
  expect_output(
    cat(to_json(minus_operator2)),
    '{"op":"-","args":[1,2]}',
    fixed = TRUE
  )

  # ---- timestamp literal ----
  expect_error(timestamp_lit("test"))
  expect_error(timestamp_lit(NULL))
  expect_error(timestamp_lit(TRUE))
  expect_error(timestamp_lit(1.2))
  expect_error(timestamp_lit("2019-01-01"))

  ts_lit <- timestamp_lit("2019-01-01T01:00:00Z")
  expect_s3_class(ts_lit, "cql2_timestamp")
  expect_output(
    print(ts_lit), "TIMESTAMP('2019-01-01T01:00:00Z')", fixed = TRUE
  )

  # ---- date literal ----
  expect_error(date_lit("test"))
  expect_error(date_lit(NULL))
  expect_error(date_lit(TRUE))
  expect_error(date_lit(1.2))
  expect_error(date_lit("2019-01-01T01:00:00Z"))

  dt_lit <- date_lit("2019-01-01")
  expect_s3_class(dt_lit, "cql2_date")
  expect_output(
    print(dt_lit), "DATE('2019-01-01')", fixed = TRUE
  )

  # ---- interval literal ----
  expect_error(interval_lit("test", ".."))
  expect_error(interval_lit("..", "test"))
  expect_error(interval_lit(NULL, NULL))
  expect_error(interval_lit(TRUE, FALSE))
  expect_error(interval_lit(1.2, 2.1))

  int_lit <- interval_lit("2019-01-01")
  expect_s3_class(int_lit, "cql2_interval")
  expect_output(
    print(int_lit), "INTERVAL('2019-01-01','..')", fixed = TRUE
  )

  # ---- func def ----
  function_definition <- func_def("test")
  function_df <- function_definition(a = 1, b = 2)
  expect_s3_class(function_df, "cql2_func")
  expect_output(print(function_df), "test(1,2)", fixed = TRUE)
  expect_output(
    cat(to_json(function_df)),
    '{"function":{"name":"test","args":{"a":1,"b":2}}}',
    fixed = TRUE
  )

  # ---- like op ----
  expect_error(like_op("test", 2))
  expect_error(like_op(2, "test"))
  expect_error(like_op(NULL, "2"))
  expect_error(like_op("2", NULL))
  expect_error(like_op(TRUE, "2"))
  expect_error(like_op("2", FALSE))
  like_operator <- like_op("test1", "test2")
  expect_s3_class(like_operator, "cql2_like_op")
  expect_output(print(like_operator), "'test1' LIKE 'test2'")
  expect_output(
    cat(to_json(like_operator)),
    '{"op":"like","args":["test1","test2"]}',
    fixed = TRUE
  )

  # ---- between op ----
  expect_error(between_op("test", 2, "test"))
  expect_error(between_op(2, "test", 2))
  expect_error(between_op("test", 2, 2))
  expect_error(between_op("2", NULL, NULL))
  expect_error(between_op(TRUE, "2", FALSE))
  between_operator <- between_op(2, 1, 3)
  expect_s3_class(between_operator, "cql2_between_op")
  expect_output(print(between_operator), "2 BETWEEN 1 AND 3")
  expect_output(
    cat(to_json(between_operator)),
    '{"op":"between","args":[2,1,3]}',
    fixed = TRUE
  )

  # ---- in op ----
  expect_error(in_op("test", 2))
  expect_error(in_op("test", 2))
  expect_error(in_op(NULL, 2))
  expect_error(in_op(TRUE, 2))
  in_operator <- in_op(1, list(2, 3))
  expect_s3_class(in_operator, "cql2_in_op")
  expect_output(print(in_operator), "1 IN (2,3)", fixed = TRUE)
  expect_output(
    cat(to_json(in_operator)),
    '{"op":"in","args":[1,[2,3]]}',
    fixed = TRUE
  )

  # ---- casei ----
  expect_error(casei(2))
  expect_error(casei(NULL))
  expect_error(casei(TRUE))
  casei_expr <- casei("test")
  expect_s3_class(casei_expr, "cql2_casei_op")
  expect_output(print(casei_expr), "CASEI('test')", fixed = TRUE)

  # ---- accenti ----
  expect_error(accenti(2))
  expect_error(accenti(NULL))
  expect_error(accenti(TRUE))
  accenti_expr <- accenti("test")
  expect_s3_class(accenti_expr, "cql2_accenti_op")
  expect_output(print(accenti_expr), "ACCENTI('test')", fixed = TRUE)

  # ---- spatial op ----
  spatial_operator <- spatial_op("test")
  expect_error(spatial_operator(NULL, NULL))
  expect_error(spatial_operator(FALSE, TRUE))
  expect_error(spatial_operator("test", TRUE))
  expect_error(spatial_operator("test", 2))
  expect_error(spatial_operator(
    list(type = "Pointed", coordinates = c(1, 2)), "POINT(1,3)"
  ))
  sp1 <- spatial_operator(
    list(type = "Point", coordinates = c(1, 2)), "POINT(1,3)"
  )
  expect_s3_class(sp1, "cql2_spatial_op")
  expect_output(print(sp1), "TEST(POINT(1 2),POINT(1,3))", fixed = TRUE)

  pt_sfg <- sf::st_point(c(1, 2))
  ls_sfc <- sf::st_sfc(sf::st_linestring(matrix(seq_len(10), 5, 2)), crs = 4326)
  outer <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)
  hole1 <- matrix(c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)
  hole2 <- matrix(c(5, 5, 5, 6, 6, 6, 6, 5, 5, 5), ncol = 2, byrow = TRUE)
  poly_sf <- sf::st_sf(
    geom = sf::st_sfc(sf::st_polygon(list(outer, hole1, hole2)), crs = 4326)
  )
  gc <- sf::st_geometrycollection(
    list(sf::st_point(seq_len(2)), sf::st_linestring(matrix(seq_len(6), 3)))
  )

  sp2 <- spatial_operator(pt_sfg, ls_sfc)
  expect_s3_class(sp2, "cql2_spatial_op")
  expect_output(
    print(sp2),
    "TEST(POINT(1 2),LINESTRING(1 6,2 7,3 8,4 9,5 10))",
    fixed = TRUE
  )

  sp3 <- spatial_operator(poly_sf, gc)
  expect_s3_class(sp3, "cql2_spatial_op")
  expect_output(
    print(sp3),
    "TEST(POLYGON((0 0,10 0,10 10,0 10,0 0),(1 1,1 2,2 2,2 1,1 1),(5 5,5 6,6 6,6 5,5 5)),GEOMETRYCOLLECTION(POINT(1 2),LINESTRING(1 4,2 5,3 6)))",
    fixed = TRUE
  )

  # ---- temporal op ----
  temporal_operator <- temporal_op("test")
  expect_error(temporal_operator(FALSE, TRUE))
  expect_error(temporal_operator(NULL, NULL))
  expect_error(temporal_operator(1, 2))

  temp <- temporal_operator("2019-01-01", "2019-02-01T01:01:01Z")
  expect_s3_class(temp, "cql2_temporal_op")
  expect_output(
    print(temp), "TEST('2019-01-01','2019-02-01T01:01:01Z')", fixed = TRUE
  )

  # ---- array op ----
  array_operator <- array_op("test")
  expect_error(array_operator(mean, TRUE))
  expect_error(array_operator("ddd", NULL))

  array_obj <- array_operator(list(1, 2), c(TRUE, FALSE))
  expect_s3_class(array_obj, "cql2_array_op")
  expect_output(print(array_obj), "TEST((1,2),(true,false))", fixed = TRUE)
})

test_that("cql2 output functions", {
  # ---- null ----
  expect_output(cat(to_json(NULL)), "null")

  # ---- character ----
  expect_output(cat(to_json("a")), "a")
  expect_output(cat(to_json(c("a", "b"))), '["a","b"]', fixed = TRUE)

  expect_output(cat(to_text("a")), "a")
  expect_output(cat(to_text(c("a", "b"))), "('a','b')", fixed = TRUE)

  # ---- numeric ----
  expect_output(cat(to_json(1)), "1")
  expect_output(cat(to_json(c(1, 2))), '[1,2]', fixed = TRUE)

  expect_output(cat(to_text(1)), "1")
  expect_output(cat(to_text(c(1, 2))), '(1,2)', fixed = TRUE)

  # ---- logical ----
  expect_output(cat(to_json(TRUE)), "true")
  expect_output(cat(to_json(c(TRUE, FALSE))), '[true,false]', fixed = TRUE)

  expect_output(cat(to_text(TRUE)), "true")
  expect_output(cat(to_text(c(TRUE, FALSE))), '(true,false)', fixed = TRUE)

  # ---- logical ----
  expect_output(cat(to_json(TRUE)), "true")
  expect_output(cat(to_json(c(TRUE, FALSE))), '[true,false]', fixed = TRUE)

  expect_output(cat(to_text(TRUE)), "true")
  expect_output(cat(to_text(c(TRUE, FALSE))), '(true,false)', fixed = TRUE)

  # ---- list ----
  expect_error(cat(to_json(to_json(list(2, a = 2)))))
  expect_error(cat(to_json(to_text(list(2, a = 2)))))

  pt <- list(type = "Point", coordinates = c(1, 2))
  expect_output(cat(to_json(list(a = 2))), '{"a":2}', fixed = TRUE)
  expect_output(cat(to_json(c(TRUE, FALSE))), '[true,false]', fixed = TRUE)
  expect_output(cat(to_json(pt)), '{"type":"Point","coordinates":[1,2]}', fixed = TRUE)

  expect_output(cat(to_text(TRUE)), "true")
  expect_output(cat(to_text(c(TRUE, FALSE))), '(true,false)', fixed = TRUE)
  expect_output(cat(to_text(pt)), 'POINT(1 2)', fixed = TRUE)

  # ---- sfg, sfc sf ----
  options(stac_digits = NULL)
  pt_sfg <- sf::st_point(c(1, 2))
  ls_sfc <- sf::st_sfc(sf::st_linestring(matrix(seq_len(10), 5, 2)), crs = 4326)
  outer <- matrix(c(100.0, 0.0, 101.0, 0.0,
                    101.0, 1.0, 100.0, 1.0,100.0, 0.0),
                  ncol = 2, byrow = TRUE)
  hole1 <- matrix(c(100.8, 0.8, 100.8, 0.2, 100.2, 0.2,
                    100.2, 0.8, 100.8, 0.8),
                  ncol = 2, byrow = TRUE)
  poly_sf <- sf::st_sf(
    geom = sf::st_sfc(sf::st_polygon(list(outer, hole1)), crs = 4326)
  )

  poly_sf1 <- sf::st_sf(
    geom = sf::st_sfc(sf::st_polygon(list(hole1)), crs = 4326)
  )

  geom_col <- sf::st_geometrycollection(
    list(sf::st_point(seq_len(2)), sf::st_linestring(matrix(seq_len(6), 3)))
  )

  # sfg
  expect_output(
    cat(to_json(pt_sfg)), '{"type":"Point","coordinates":[1,2]}', fixed = TRUE
  )
  expect_output(cat(to_text(pt_sfg)), 'POINT(1 2)', fixed = TRUE)

  # sfc
  expect_output(
    cat(to_json(ls_sfc)),
    '{"type":"LineString","coordinates":[[1,6],[2,7],[3,8],[4,9],[5,10]]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(ls_sfc)), 'LINESTRING(1 6,2 7,3 8,4 9,5 10)', fixed = TRUE
  )

  # sf
  expect_output(
    cat(to_json(poly_sf)),
    '{"type":"Polygon","coordinates":[[[100,0],[101,0],[101,1],[100,1],[100,0]],[[100.8,0.8],[100.8,0.2],[100.2,0.2],[100.2,0.8],[100.8,0.8]]]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(poly_sf)),
    'POLYGON((100 0,101 0,101 1,100 1,100 0),(100.8 0.8,100.8 0.2,100.2 0.2,100.2 0.8,100.8 0.8))',
    fixed = TRUE
  )

  # geometry collection
  expect_output(
    cat(to_json(geom_col)),
    '{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[1,2]},{"type":"LineString","coordinates":[[1,4],[2,5],[3,6]]}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(geom_col)),
    'GEOMETRYCOLLECTION(POINT(1 2),LINESTRING(1 4,2 5,3 6))',
    fixed = TRUE
  )

  # list
  linestring <- list(
    type = "LineString",
    coordinates = matrix(
      c(-62.5573, -8.4332, -62.2179, -8.3681),
      ncol = 2, byrow = TRUE
    )
  )
  expect_output(
    cat(to_json(linestring)),
    '{"type":"LineString","coordinates":[[-62.5573,-8.4332],[-62.2179,-8.3681]]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(linestring)),
    'LINESTRING(-62.5573 -8.4332,-62.2179 -8.3681)',
    fixed = TRUE,

  )
  expect_error(to_text(list(a = 1, NULL)))
  b <- structure(list(a = 1), class = "my-class")
  expect_error(to_json(b))

  # character
  expect_output(
    object = cat(to_json(get_spatial("POINT(1 2)"))),
    "POINT(1 2)",
    fixed = TRUE
  )
  expect_output(
    object = cat(to_text(get_spatial("POINT(1 2)"))),
    "POINT(1 2)",
    fixed = TRUE
  )

  # ---- cql2 ----
  expect_output(
    print(to_text(cql2(quote(a > 1)))),
    "a > 1"
  )
})

test_that("cql2 helper functions", {
  bbox <- c(-122.2751, 47.5469, -121.9613, 47.7458)
  time_range <- cql2_interval("2020-12-01", "2020-12-31")
  area_of_interest <- cql2_bbox_as_geojson(bbox)

  expect_equal(class(area_of_interest), "list")
  expect_equal(class(time_range), "call")
  expect_equal(class(cql2_date("2020-01-01")), "call")
  expect_equal(class(cql2_timestamp("2020-01-01T12:00:00Z")), "call")
  expect_error(cql2_date("2020-01-55"))
  expect_error(cql2_timestamp("2020-01-01"))
  expect_output(
    object = {cql2_json(
      collection == "landsat-c2-l2" &&
        t_intersects(datetime, !!time_range) &&
        s_intersects(geometry, !!area_of_interest)
    )},
    regexp = "-121.9613"
  )
})
