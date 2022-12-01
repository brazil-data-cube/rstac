conformance_test <- function(q, expected_number) {
  expect_equal(
    object = items_matched(get_request(q)),
    expected = expected_number
  )
}

test_that("Conformance Test 7", {
  q <- rstac::stac(base_url = "https://cql2test.ldproxy.net/ne110m4cql2",
                   force_version = "0.9.0")
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      NAME == "Luxembourg"
    ),
    expected_number = 1
  )

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

test_that("Conformance Test 12", {
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

test_that("Conformance Test 16", {
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

test_that("Conformance Test 25", {
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

test_that("Conformance Test 34", {
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

  point <- 'POINT(6.1300028 49.6116604)'
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_populated_places_simple")),
      s_equals(geom, {{point}})
    ),
    expected_number = 1
  )

  polygon <- 'POLYGON((6.043073357781111 50.128051662794235,6.242751092156993 49.90222565367873,6.186320428094177 49.463802802114515,5.897759230176348 49.44266714130711,5.674051954784829 49.529483547557504,5.782417433300907 50.09032786722122,6.043073357781111 50.128051662794235))'
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_touches(geom, {{polygon}})
    ),
    expected_number = 3
  )

  point <- 'POINT(6.242751092156993 49.90222565367873)'
  conformance_test(
    q = ext_filter(
      items(collections(q, "ne_110m_admin_0_countries")),
      s_touches(geom, {{point}})
    ),
    expected_number = 2
  )

  linestring <- 'LINESTRING(6.043073357781111 50.128051662794235,6.242751092156993 49.90222565367873)'
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

test_that("Conformance Test 38", {
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

test_that("Conformance Test 45", {
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
