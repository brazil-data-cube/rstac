test_that("scalar data types contructors", {
  # "string": character strings
  string <- cql2("string")
  expect_equal(class(string[["filter"]]), "character")
  expect_output(cat(to_json(string[["filter"]])), regexp = '"string"')
  expect_output(cat(to_text(string[["filter"]])), regexp = "'string'")

  # "number": numbers including integers and floating point values
  number <- cql2(3.14)
  expect_equal(class(number[["filter"]]), "numeric")
  expect_output(cat(to_json(number[["filter"]])), regexp = '3.14')
  expect_output(cat(to_text(number[["filter"]])), regexp = "3.14")
  integer <- cql2(3L)
  expect_equal(class(integer[["filter"]]), "integer")
  expect_output(cat(to_json(integer[["filter"]])), regexp = '3')
  expect_output(cat(to_text(integer[["filter"]])), regexp = "3")

  # "boolean": booleans
  boolean <- cql2(TRUE)
  expect_equal(class(boolean[["filter"]]), "logical")
  expect_output(cat(to_json(boolean[["filter"]])), regexp = 'true')
  expect_output(cat(to_text(boolean[["filter"]])), regexp = 'true')
  boolean2 <- cql2(T)
  expect_equal(class(boolean2[["filter"]]), "logical")
  expect_output(cat(to_json(boolean2[["filter"]])), regexp = 'true')
  expect_output(cat(to_text(boolean2[["filter"]])), regexp = 'true')
  boolean3 <- cql2(FALSE)
  expect_equal(class(boolean3[["filter"]]), "logical")
  expect_output(cat(to_json(boolean3[["filter"]])), regexp = 'false')
  expect_output(cat(to_text(boolean3[["filter"]])), regexp = 'false')
  boolean4 <- cql2(F)
  expect_equal(class(boolean4[["filter"]]), "logical")
  expect_output(cat(to_json(boolean4[["filter"]])), regexp = 'false')
  expect_output(cat(to_text(boolean4[["filter"]])), regexp = 'false')

  # "timestamp": an instant with a granularity of a second or smaller
  timestamp <- cql2(timestamp("1985-07-16T05:32:00Z"))
  expect_s3_class(timestamp[["filter"]], "cql2_timestamp")
  expect_output(
    cat(to_json(timestamp[["filter"]])),
    regexp = '\\{"timestamp":"1985-07-16T05:32:00Z"\\}'
  )
  expect_output(
    cat(to_text(timestamp[["filter"]])),
    regexp = 'TIMESTAMP'
  )
  expect_error(cql2(timestamp("1985-07-16")))

  # "date": an instant with a granularity of a day
  date <- cql2(date("1985-07-16"))
  expect_s3_class(date[["filter"]], "cql2_date")
  expect_output(
    cat(to_json(date[["filter"]])),
    regexp = '\\{"date":"1985-07-16"\\}'
  )
  expect_output(
    cat(to_text(date[["filter"]])),
    regexp = 'DATE'
  )
  expect_error(cql2(date("1985-07-16T05:32:00Z")))
})

test_that("property references contructor", {
  # property reference in a scalar expression
  prop <- cql2(avg(winSpeed))
  expect_s3_class(prop[["filter"]], "cql2_func")
  expect_output(
    cat(to_json(prop[["filter"]])),
    regexp = '{"function":{"name":"avg","args":[{"property":"winSpeed"}]}}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(prop[["filter"]])),
    regexp = 'avg(winSpeed)',
    fixed = TRUE
  )
})

test_that("standard comparison predicates", {
  # binary comparison predicates
  comp1 <- cql2(city == "Toronto")
  expect_s3_class(comp1[["filter"]], "cql2_comp_op")
  expect_output(
    cat(to_json(comp1[["filter"]])),
    regexp = '{"op":"=","args":[{"property":"city"},"Toronto"]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(comp1[["filter"]])),
    regex =  "city = 'Toronto'",
    fixed = TRUE
  )
  expect_error(cql2(city == c("Toronto", "Toronto2")))
  # binary comparison predicates
  comp5 <- cql2(avg(windSpeed) < 4)
  expect_s3_class(comp5[["filter"]], "cql2_comp_op")
  expect_output(
    cat(to_json(comp5[["filter"]])),
    regexp = '{"op":"<","args":[{"function":{"name":"avg","args":[{"property":"windSpeed"}]}},4]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(comp5[["filter"]])),
    regex =  "avg(windSpeed) < 4",
    fixed = TRUE
  )
  # binary comparison predicates
  comp5 <- cql2(balance - 150.0 > 0)
  expect_s3_class(comp5[["filter"]], "cql2_comp_op")
  expect_output(
    cat(to_json(comp5[["filter"]])),
    regexp = '{"op":">","args":[{"op":"-","args":[{"property":"balance"},150]},0]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(comp5[["filter"]])),
    regex =  "balance - 150 > 0",
    fixed = TRUE
  )
  # binary comparison predicates
  comp5 <- cql2(updated >= date('1970-01-01'))
  expect_s3_class(comp5[["filter"]], "cql2_comp_op")
  expect_output(
    cat(to_json(comp5[["filter"]])),
    regexp = '{"op":">=","args":[{"property":"updated"},{"date":"1970-01-01"}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(comp5[["filter"]])),
    regex =  "updated >= DATE('1970-01-01')",
    fixed = TRUE
  )
  # binary comparison predicates
  comp5 <- cql2(!is_null(geometry))
  expect_s3_class(comp5[["filter"]], "cql2_not_op")
  expect_output(
    cat(to_json(comp5[["filter"]])),
    regexp = '{"op":"not","args":[{"op":"isNull","args":[{"property":"geometry"}]}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(comp5[["filter"]])),
    regex =  "geometry IS NOT NULL",
    fixed = TRUE
  )
})

test_that("advanced comparison operators", {
  # example of a LIKE predicate
  comp1 <- cql2(name %like% "Smith%")
  expect_s3_class(comp1[["filter"]], "cql2_like_op")
  expect_output(
    cat(to_json(comp1[["filter"]])),
    regexp = '{"op":"like","args":[{"property":"name"},"Smith%"]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(comp1[["filter"]])),
    regex =  "name LIKE 'Smith%'",
    fixed = TRUE
  )
  expect_error(cql2(name %like% 123))
  expect_error(cql2(name %like% TRUE))
  expect_error(cql2(name %like% geometry))
  expect_error(cql2(name %like% date("1920-01-01")))
  # examples of a BETWEEN predicate
  comp2 <- cql2(between(depth, 100, 150))
  expect_s3_class(comp2[["filter"]], "cql2_between_op")
  expect_output(
    cat(to_json(comp2[["filter"]])),
    regexp = '{"op":"between","args":[{"property":"depth"},100,150]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(comp2[["filter"]])),
    regex =  "depth BETWEEN 100 AND 150",
    fixed = TRUE
  )
  expect_error(cql2(between(depth, TRUE, FALSE)))
  expect_error(cql2(between(depth, date('2000-01-01'), date('2000-01-02'))))
  # examples of a IN predicate
  comp3 <- cql2(cityName %in% c('Toronto','Frankfurt','Tokyo','New York'))
  expect_s3_class(comp3[["filter"]], "cql2_in_op")
  expect_output(
    cat(to_json(comp3[["filter"]])),
    regexp = '{"op":"in","args":[{"property":"cityName"},["Toronto","Frankfurt","Tokyo","New York"]]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(comp3[["filter"]])),
    regex =  "cityName IN ('Toronto','Frankfurt','Tokyo','New York')",
    fixed = TRUE
  )
  # examples of a NOT IN predicate
  comp4 <- cql2(!category %in% c(1, 2, 3, 4))
  expect_s3_class(comp4[["filter"]], "cql2_not_op")
  expect_output(
    cat(to_json(comp4[["filter"]])),
    regexp = '{"op":"in","args":[{"property":"cityName"},["Toronto","Frankfurt","Tokyo","New York"]]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(comp4[["filter"]])),
    regex =  "category NOT IN (1,2,3,4)",
    fixed = TRUE
  )
})

test_that("case-insensitive comparison", {
  # example case-insensitive comparison
  casei <- cql2(casei(road_class) %in% c(casei('aaaa'), casei('bbbb')))
  expect_s3_class(casei[["filter"]], "cql2_in_op")
  expect_output(
    cat(to_json(casei[["filter"]])),
    regexp = '{"op":"in","args":[{"casei":{"property":"road_class"}},[{"casei":"aaaa"},{"casei":"bbbb"}]]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(casei[["filter"]])),
    regex =  "CASEI(road_class) IN ([CASEI('aaaa'),CASEI('bbbb'))",
    fixed = TRUE
  )
  expect_error(cql2(casei(123)))
  expect_error(cql2(casei(TRUE)))
  expect_error(cql2(casei(date("1920-01-01"))))
})

test_that("accent-insensitive comparison", {
  # example case-insensitive comparison
  accenti <- cql2(accenti(etat_vol) == accenti('débárquér'))
  expect_s3_class(accenti[["filter"]], "cql2_comp_op")
  expect_output(
    cat(to_json(accenti[["filter"]])),
    regexp = '{"op":"=","args":[{"accenti":{"property":"etat_vol"}},{"accenti":"débárquér"}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(accenti[["filter"]])),
    regex =  "ACCENTI(etat_vol) = ACCENTI('débárquér')",
    fixed = TRUE
  )
  expect_error(cql2(accenti(123)))
  expect_error(cql2(accenti(TRUE)))
  expect_error(cql2(accenti(date("1920-01-01"))))
})

test_that("spatial data types and literal values", {
  # spatial literal example
  poly_sfg <- sf::st_polygon(
    list(
      matrix(c(-62.34499836, -8.57414572,
               -62.18858174, -8.57414572,
               -62.18858174, -8.15351185,
               -62.34499836, -8.15351185,
               -62.34499836, -8.57414572),
             ncol = 2, byrow = TRUE)
    )
  )
  poly_sfc <- sf::st_sfc(poly_sfg, crs = 4326)
  poly_sf <- sf::st_sf(poly_sfc)
  spatial_sfg <- cql2({{poly_sfg}})
  spatial_sfc <- cql2({{poly_sfc}})
  spatial_sf <- cql2({{poly_sf}})
  # tests with sfg object
  expect_s3_class(spatial_sfg$filter, "sfg")
  expect_output(
    cat(to_json(spatial_sfg[["filter"]])),
    regexp = '{"type":"Polygon","coordinates":[[[-62.34499836,-8.57414572],[-62.18858174,-8.57414572],[-62.18858174,-8.15351185],[-62.34499836,-8.15351185],[-62.34499836,-8.57414572]]]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(spatial_sfg[["filter"]])),
    regexp = 'POLYGON((-62.34499836 -8.57414572,-62.18858174 -8.57414572,-62.18858174 -8.15351185,-62.34499836 -8.15351185,-62.34499836 -8.57414572))',
    fixed = TRUE
  )
  # tests with sfc object
  expect_s3_class(spatial_sfc$filter, "sfc")
  expect_output(
    cat(to_json(spatial_sfc[["filter"]])),
    regexp = '{"type":"Polygon","coordinates":[[[-62.34499836,-8.57414572],[-62.18858174,-8.57414572],[-62.18858174,-8.15351185],[-62.34499836,-8.15351185],[-62.34499836,-8.57414572]]]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(spatial_sfc[["filter"]])),
    regexp = 'POLYGON((-62.34499836 -8.57414572,-62.18858174 -8.57414572,-62.18858174 -8.15351185,-62.34499836 -8.15351185,-62.34499836 -8.57414572))',
    fixed = TRUE
  )
  # tests with sf object
  expect_s3_class(spatial_sf$filter, "sf")
  expect_output(
    cat(to_json(spatial_sf[["filter"]])),
    regexp = '{"type":"Polygon","coordinates":[[[-62.34499836,-8.57414572],[-62.18858174,-8.57414572],[-62.18858174,-8.15351185],[-62.34499836,-8.15351185],[-62.34499836,-8.57414572]]]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(spatial_sf[["filter"]])),
    regexp = 'POLYGON((-62.34499836 -8.57414572,-62.18858174 -8.57414572,-62.18858174 -8.15351185,-62.34499836 -8.15351185,-62.34499836 -8.57414572))',
    fixed = TRUE
  )

  poly_lst <- list(
    type = "Polygon",
    coordinates =
      list(
        rbind(
          c(-62.34500, -8.574146),
          c(-62.18858, -8.574146),
          c(-62.18858, -8.153512),
          c(-62.34500, -8.153512),
          c(-62.34500, -8.574146)
        )
      )
  )

  # TODO: add linestring and multilinestring
  # TODO: add point and multipoint
  # TODO: see bbox data type
})

test_that("spatial Operators", {
  # example spatial predicate
  poly_sfg <- sf::st_polygon(
    list(
      matrix(c(-62.34499836, -8.57414572,
               -62.18858174, -8.57414572,
               -62.18858174, -8.15351185,
               -62.34499836, -8.15351185,
               -62.34499836, -8.57414572),
             ncol = 2, byrow = TRUE)
    )
  )
  # intersects operator
  s_intersects <- cql2(s_intersects(geometry, {{poly_sfg}}))
  expect_s3_class(s_intersects[["filter"]], "cql2_spatial_op")
  expect_output(
    cat(to_json(s_intersects[["filter"]])),
    regexp = '{"op":"s_intersects","args":[{"property":"geometry"},{"type":"Polygon","coordinates":[[[-62.34499836,-8.57414572],[-62.18858174,-8.57414572],[-62.18858174,-8.15351185],[-62.34499836,-8.15351185],[-62.34499836,-8.57414572]]]}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(s_intersects[["filter"]])),
    regexp = 'S_INTERSECTS(geometry,POLYGON((-62.34499836 -8.57414572,-62.18858174 -8.57414572,-62.18858174 -8.15351185,-62.34499836 -8.15351185,-62.34499836 -8.57414572)))',
    fixed = TRUE
  )
  expect_error(cql2(st_intersects(geometry, {{poly_sfg}})))
  expect_error(cql2(S_INTERSECTS(geometry, {{poly_sfg}})))
  # contains operator
  s_contains <- cql2(s_contains(geometry, {{poly_sfg}}))
  expect_s3_class(s_contains[["filter"]], "cql2_spatial_op")
  expect_output(
    cat(to_json(s_contains[["filter"]])),
    regexp = '{"op":"s_contains","args":[{"property":"geometry"},{"type":"Polygon","coordinates":[[[-62.34499836,-8.57414572],[-62.18858174,-8.57414572],[-62.18858174,-8.15351185],[-62.34499836,-8.15351185],[-62.34499836,-8.57414572]]]}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(s_contains[["filter"]])),
    regexp = 'S_CONTAINS(geometry,POLYGON((-62.34499836 -8.57414572,-62.18858174 -8.57414572,-62.18858174 -8.15351185,-62.34499836 -8.15351185,-62.34499836 -8.57414572)))',
    fixed = TRUE
  )
  expect_error(cql2(st_contains(geometry, {{poly_sfg}})))
  expect_error(cql2(S_CONTAINS(geometry, {{poly_sfg}})))
  # crosses operator
  s_crosses <- cql2(s_crosses(geometry, {{poly_sfg}}))
  expect_s3_class(s_crosses[["filter"]], "cql2_spatial_op")
  expect_output(
    cat(to_json(s_crosses[["filter"]])),
    regexp = '{"op":"s_crosses","args":[{"property":"geometry"},{"type":"Polygon","coordinates":[[[-62.34499836,-8.57414572],[-62.18858174,-8.57414572],[-62.18858174,-8.15351185],[-62.34499836,-8.15351185],[-62.34499836,-8.57414572]]]}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(s_crosses[["filter"]])),
    regexp = 'S_CROSSES(geometry,POLYGON((-62.34499836 -8.57414572,-62.18858174 -8.57414572,-62.18858174 -8.15351185,-62.34499836 -8.15351185,-62.34499836 -8.57414572)))',
    fixed = TRUE
  )
  expect_error(cql2(st_crosses(geometry, {{poly_sfg}})))
  expect_error(cql2(S_CROSSES(geometry, {{poly_sfg}})))
  expect_error(cql2(s_croses(geometry, {{poly_sfg}})))
  # disjoint operator
  s_disjoint <- cql2(s_disjoint(geometry, {{poly_sfg}}))
  expect_s3_class(s_disjoint[["filter"]], "cql2_spatial_op")
  expect_output(
    cat(to_json(s_disjoint[["filter"]])),
    regexp = '{"op":"s_disjoint","args":[{"property":"geometry"},{"type":"Polygon","coordinates":[[[-62.34499836,-8.57414572],[-62.18858174,-8.57414572],[-62.18858174,-8.15351185],[-62.34499836,-8.15351185],[-62.34499836,-8.57414572]]]}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(s_disjoint[["filter"]])),
    regexp = 'S_DISJOINT(geometry,POLYGON((-62.34499836 -8.57414572,-62.18858174 -8.57414572,-62.18858174 -8.15351185,-62.34499836 -8.15351185,-62.34499836 -8.57414572)))',
    fixed = TRUE
  )
  expect_error(cql2(st_disjoint(geometry, {{poly_sfg}})))
  expect_error(cql2(S_DISJOINT(geometry, {{poly_sfg}})))
  expect_error(cql2(s_disjont(geometry, {{poly_sfg}})))
  # equals operator
  s_equals <- cql2(s_equals(geometry, {{poly_sfg}}))
  expect_s3_class(s_equals[["filter"]], "cql2_spatial_op")
  expect_output(
    cat(to_json(s_equals[["filter"]])),
    regexp = '{"op":"s_equals","args":[{"property":"geometry"},{"type":"Polygon","coordinates":[[[-62.34499836,-8.57414572],[-62.18858174,-8.57414572],[-62.18858174,-8.15351185],[-62.34499836,-8.15351185],[-62.34499836,-8.57414572]]]}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(s_equals[["filter"]])),
    regexp = 'S_EQUALS(geometry,POLYGON((-62.34499836 -8.57414572,-62.18858174 -8.57414572,-62.18858174 -8.15351185,-62.34499836 -8.15351185,-62.34499836 -8.57414572)))',
    fixed = TRUE
  )
  expect_error(cql2(st_equals(geometry, {{poly_sfg}})))
  expect_error(cql2(S_EQUALS(geometry, {{poly_sfg}})))
  expect_error(cql2(s_equal(geometry, {{poly_sfg}})))
  # overlaps operator
  s_overlaps <- cql2(s_overlaps(geometry, {{poly_sfg}}))
  expect_s3_class(s_overlaps[["filter"]], "cql2_spatial_op")
  expect_output(
    cat(to_json(s_overlaps[["filter"]])),
    regexp = '{"op":"s_overlaps","args":[{"property":"geometry"},{"type":"Polygon","coordinates":[[[-62.34499836,-8.57414572],[-62.18858174,-8.57414572],[-62.18858174,-8.15351185],[-62.34499836,-8.15351185],[-62.34499836,-8.57414572]]]}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(s_overlaps[["filter"]])),
    regexp = 'S_OVERLAPS(geometry,POLYGON((-62.34499836 -8.57414572,-62.18858174 -8.57414572,-62.18858174 -8.15351185,-62.34499836 -8.15351185,-62.34499836 -8.57414572)))',
    fixed = TRUE
  )
  expect_error(cql2(st_overlaps(geometry, {{poly_sfg}})))
  expect_error(cql2(S_OVERLAPS(geometry, {{poly_sfg}})))
  expect_error(cql2(s_overlap(geometry, {{poly_sfg}})))
  # touches operator
  s_touches <- cql2(s_touches(geometry, {{poly_sfg}}))
  expect_s3_class(s_overlaps[["filter"]], "cql2_spatial_op")
  expect_output(
    cat(to_json(s_touches[["filter"]])),
    regexp = '{"op":"s_touches","args":[{"property":"geometry"},{"type":"Polygon","coordinates":[[[-62.34499836,-8.57414572],[-62.18858174,-8.57414572],[-62.18858174,-8.15351185],[-62.34499836,-8.15351185],[-62.34499836,-8.57414572]]]}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(s_touches[["filter"]])),
    regexp = 'S_TOUCHES(geometry,POLYGON((-62.34499836 -8.57414572,-62.18858174 -8.57414572,-62.18858174 -8.15351185,-62.34499836 -8.15351185,-62.34499836 -8.57414572)))',
    fixed = TRUE
  )
  expect_error(cql2(st_touches(geometry, {{poly_sfg}})))
  expect_error(cql2(S_TOUCHES(geometry, {{poly_sfg}})))
  expect_error(cql2(s_touche(geometry, {{poly_sfg}})))
  # within operator
  s_within <- cql2(s_within(geometry, {{poly_sfg}}))
  expect_s3_class(s_overlaps[["filter"]], "cql2_spatial_op")
  expect_output(
    cat(to_json(s_within[["filter"]])),
    regexp = '{"op":"s_within","args":[{"property":"geometry"},{"type":"Polygon","coordinates":[[[-62.34499836,-8.57414572],[-62.18858174,-8.57414572],[-62.18858174,-8.15351185],[-62.34499836,-8.15351185],[-62.34499836,-8.57414572]]]}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text(s_within[["filter"]])),
    regexp = 'S_WITHIN(geometry,POLYGON((-62.34499836 -8.57414572,-62.18858174 -8.57414572,-62.18858174 -8.15351185,-62.34499836 -8.15351185,-62.34499836 -8.57414572)))',
    fixed = TRUE
  )
  expect_error(cql2(st_within(geometry, {{poly_sfg}})))
  expect_error(cql2(S_WITHIN(geometry, {{poly_sfg}})))
  expect_error(cql2(s_with(geometry, {{poly_sfg}})))
})

test_that("temporal data types and literal values", {
  # Interval examples
  interval1 <- cql2(interval('1969-07-16', '1969-07-24'))
  expect_s3_class(interval1[["filter"]], "cql2_interval")
  expect_output(
    cat(to_json((interval1[["filter"]]))),
    regexp = '{"interval":["1969-07-16","1969-07-24"]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text((interval1[["filter"]]))),
    regexp = "INTERVAL('1969-07-16','1969-07-24')",
    fixed = TRUE
  )
  interval2 <- cql2(interval('1969-07-16T05:32:00Z', '1969-07-24T16:50:35Z'))
  expect_s3_class(interval2[["filter"]], "cql2_interval")
  expect_output(
    cat(to_json((interval2[["filter"]]))),
    regexp = '{"interval":["1969-07-16T05:32:00Z","1969-07-24T16:50:35Z"]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text((interval2[["filter"]]))),
    regexp = "INTERVAL('1969-07-16T05:32:00Z','1969-07-24T16:50:35Z')",
    fixed = TRUE
  )
  interval3 <- cql2(interval('2019-09-09', '..'))
  expect_s3_class(interval3[["filter"]], "cql2_interval")
  expect_output(
    cat(to_json((interval3[["filter"]]))),
    regexp = '{"interval":["2019-09-09",".."]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text((interval3[["filter"]]))),
    regexp = "INTERVAL('2019-09-09','..')",
    fixed = TRUE
  )
})

test_that("temporal Operators", {
  # intersects
  t_intersects <- cql2(
    t_intersects(event_date,
                 interval("1969-07-16T05:32:00Z", "1969-07-24T16:50:35Z"))
  )
  expect_s3_class(t_intersects[["filter"]], "cql2_temporal_op")
  expect_output(
    cat(to_json((t_intersects[["filter"]]))),
    regexp = '{"op":"t_intersects","args":[{"property":"event_date"},{"interval":["1969-07-16T05:32:00Z","1969-07-24T16:50:35Z"]}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text((t_intersects[["filter"]]))),
    regexp = "T_INTERSECTS(event_date,INTERVAL('1969-07-16T05:32:00Z','1969-07-24T16:50:35Z'))",
    fixed = TRUE
  )
  t_intersects2 <- cql2(
    t_intersects(date("1969-07-16"),
                 interval("1969-07-16T05:32:00Z", "1969-07-24T16:50:35Z"))
  )
  expect_output(
    cat(to_json((t_intersects2[["filter"]]))),
    regexp = '{"op":"t_intersects","args":[{"date":"1969-07-16"},{"interval":["1969-07-16T05:32:00Z","1969-07-24T16:50:35Z"]}]}',
    fixed = TRUE
  )
  expect_output(
    cat(to_text((t_intersects2[["filter"]]))),
    regexp = "T_INTERSECTS(DATE('1969-07-16'),INTERVAL('1969-07-16T05:32:00Z','1969-07-24T16:50:35Z'))",
    fixed = TRUE
  )
})

# TODO: add condition to skip
conformance_request <- function(data_source, filter) {
  url <- paste0(
    "https://cql2test.ldproxy.net/ne110m4cql2/collections/",
    data_source, "/items"
  )

  res <- httr::content(
    httr::GET(
      url = url,
      query = list(f = "json", filter = filter[["filter"]]),
      httr::verbose()
    ),
    type = "application/json",
    encoding = "UTF-8",
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
  res[["numberMatched"]]
}

conformance_test <- function(data_source, filter, expected_number) {
  expect_equal(
    conformance_request(
      data_source = data_source,
      filter = filter
  ),
  expected = expected_number
  )
}
# TODO: implement
test_that("Conformance Test 4", {

})

test_that("Conformance Test 7", {
  conformance_test(
    data_source = "ne_110m_admin_0_countries",
    filter = cql2(NAME == "Luxembourg", lang = "cql2-text"),
    expected_number = 1
  )

  conformance_test(
    data_source = "ne_110m_admin_0_countries",
    filter = cql2(NAME >= "Luxembourg", lang = "cql2-text"),
    expected_number = 84
  )

  conformance_test(
    data_source = "ne_110m_admin_0_countries",
    filter = cql2(NAME > "Luxembourg", lang = "cql2-text"),
    expected_number = 83
  )

  conformance_test(
    data_source = "ne_110m_admin_0_countries",
    filter = cql2(NAME <= 'Luxembourg', lang = "cql2-text"),
    expected_number = 94
  )

  conformance_test(
    data_source = "ne_110m_admin_0_countries",
    filter = cql2(NAME < 'Luxembourg', lang = "cql2-text"),
    expected_number = 93
  )

  conformance_test(
    data_source = "ne_110m_admin_0_countries",
    filter = cql2(NAME != 'Luxembourg', lang = "cql2-text"),
    expected_number = 176
  )

  conformance_test(
    data_source = "ne_110m_admin_0_countries",
    filter = cql2(POP_EST == 37589262, lang = "cql2-text"),
    expected_number = 1
  )

  conformance_test(
    data_source = "ne_110m_admin_0_countries",
    filter = cql2(POP_EST >= 37589262, lang = "cql2-text"),
    expected_number = 39
  )

  conformance_test(
    data_source = "ne_110m_admin_0_countries",
    filter = cql2(POP_EST > 37589262, lang = "cql2-text"),
    expected_number = 38
  )

  conformance_test(
    data_source = "ne_110m_admin_0_countries",
    filter = cql2(POP_EST <= 37589262, lang = "cql2-text"),
    expected_number = 139
  )

  conformance_test(
    data_source = "ne_110m_admin_0_countries",
    filter = cql2(POP_EST < 37589262, lang = "cql2-text"),
    expected_number = 138
  )

  conformance_test(
    data_source = "ne_110m_admin_0_countries",
    filter = cql2(POP_EST != 37589262, lang = "cql2-text"),
    expected_number = 176
  )

  conformance_test(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2(!is_null(name), lang = "cql2-text"),
    expected_number = 243
  )

  conformance_test(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2(is_null(name), lang = "cql2-text"),
    expected_number = 0
  )

  conformance_test(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2(name == 'København', lang = "cql2-text"),
    expected_number = 1
  )

  # TODO: is this correctly? review this
  conformance_test(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2(!is_null('"date"'), lang = "cql2-text"),
    expected_number = 3
  )
  # TODO: is this correctly? review this
  conformance_test(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2(is_null('"date"'), lang = "cql2-text"),
    expected_number = 243
  )
  # TODO: is this correctly? review this
  conformance_test(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2(`date` == date('2022-04-16'), lang = "cql2-text"),
    expected_number = 1
  )

  conformance_test(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2(!is_null(start), lang = "cql2-text"),
    expected_number = 3
  )

  conformance_test(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2(start == timestamp('2022-04-16T10:13:19Z'), lang = "cql2-text"),
    expected_number = 1
  )

  conformance_test(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2(boolean == TRUE, lang = "cql2-text"),
    expected_number = 2
  )

  conformance_test(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2(boolean == FALSE, lang = "cql2-text"),
    expected_number = 1
  )
})

# TODO: implement
test_that("Conformance Test 8", {

})

# TODO: implement
test_that("Conformance Test 9", {

})

test_that("Conformance Test 45", {
  # TODO: add condition to skip
  conformance_request <- function(data_source, filter) {
    url <- paste0(
      "https://cql2test.ldproxy.net/ne110m4cql2/collections/",
      data_source, "/items"
    )

    res <- httr::content(
      httr::GET(url = url, query = list(f = "json",
                                        filter = filter[["filter"]])),
      type = "application/json",
      encoding = "UTF-8",
      simplifyVector = TRUE,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    )
    res[["numberMatched"]]
  }
  expect_equal(
    object = conformance_request(
      data_source = "ne_110m_populated_places_simple",
      filter = cql2("København" == name, lang = "cql2-text")
    ),
    expected = 1
  )
  # res 2
  expect_equal(object =
                 conformance_request(
                   data_source = "ne_110m_populated_places_simple",
                   filter = cql2("København" <= name, lang = "cql2-text")
                 ), 137)
  # res 3
  res3 <- conformance_request(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2("København" < name, lang = "cql2-text")
  )
  expect_equal(res3[["numberMatched"]], 136)
  # res 4
  res4 <- conformance_request(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2("København" >= name, lang = "cql2-text")
  )
  expect_equal(res4[["numberMatched"]], 107)
  # res 5
  res5 <- conformance_request(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2("København" > name, lang = "cql2-text")
  )
  expect_equal(res5[["numberMatched"]], 106)
  # res 6
  res6 <- conformance_request(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2("København" != name, lang = "cql2-text")
  )
  expect_equal(res6[["numberMatched"]], 242)
  # res 7
  res7 <-
  expect_equal(conformance_request(
    data_source = "ne_110m_populated_places_simple",
    filter = cql2(name == nameascii, lang = "cql2-text")
  ), 230)
})

#' req <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
#'   rstac::stac_search(limit = 5)
#'
#' # Equal operator '=' with collection property
#' req %>% ext_filter(collection == "sentinel-2-l2a") %>% post_request()
#'
#' # Not equal operator '!=' with collection property
#' req %>% ext_filter(collection != "sentinel-2-l2a") %>% post_request()
#'
#' # Less than operator '<' with cloud_cover property
#' req %>% ext_filter(`eo:cloud_cover` < 10) %>% post_request()
#'
#' # Greater than operator '>' with vegetation_percentage property
#' req %>% ext_filter(`s2:vegetation_percentage` > 50) %>% post_request()
#'
#' # Less than or equal operator '<=' with datetime property
#' req %>% ext_filter(datetime <= "1986-01-01") %>% post_request()
#'
#' # Greater than or equal '>=' with AND operator
#' req %>% ext_filter(collection == "sentinel-2-l2a"   &&
#'                    `s2:vegetation_percentage` >= 50 &&
#'                    `eo:cloud_cover` <= 10) %>% post_request()
#' # Advanced comparison operators
#' # 'LIKE' operator
#' req %>% ext_filter(collection %like% "modis%") %>% post_request()
#'
#' # 'IN' operator
#' req %>% ext_filter(collection %in% c("modis-64A1-061",
#'                                      "landsat-c2-l2",
#'                                      "sentinel-2-l2a") &&
#'                    datetime == "2019-01-01") %>%
#'   post_request()
#'
#' # Spatial operator
#' # Lets create a polygon with sf package
#' polygon <- sf::st_polygon(
#'   list(
#'     matrix(c(-62.34499836, -8.57414572,
#'              -62.18858174, -8.57414572,
#'              -62.18858174, -8.15351185,
#'              -62.34499836, -8.15351185,
#'              -62.34499836, -8.57414572),
#'            ncol = 2, byrow = TRUE)
#'   )
#' )
#' # 'S_INTERSECTS' spatial operator with polygon and geometry property
#' req %>% ext_filter(collection == "sentinel-2-l2a" &&
#'                    s_intersects(geometry, {{polygon}})) %>% post_request()
#'
#' # 'S_CONTAINS' spatial operator with point and geometry property
#' point <- sf::st_point(c(-62.45792211, -8.61158488))
#' req %>% ext_filter(collection == "landsat-c2-l2" &&
#'                    s_contains(geometry, {{point}})) %>% post_request()
#'
#' # 'S_CROSSES' spatial operator with linestring and geometry property
#' linestring <- sf::st_linestring(
#'   matrix(c(-62.55735320, -8.43329465,
#'            -62.21791603, -8.36815014),
#'          ncol = 2, byrow = TRUE)
#' )
#' req %>% ext_filter(collection == "landsat-c2-l2" &&
#'                    s_crosses(geometry, {{linestring}})) %>% post_request()
#'
#' # Temporal operator
#' # 'T_INTERSECTS' temporal operator with datetime property
#' req %>%
#'  ext_filter(collection == "landsat-c2-l2" &&
#'             t_intersects(datetime,
#'             interval("1985-07-16T05:32:00Z", "1985-07-24T16:50:35Z"))) %>%
#'  post_request()
#'
#' # 'T_DURING' temporal operator with datetime property
#' req %>%
#'  ext_filter(collection == "landsat-c2-l2" &&
#'             t_during(datetime,
#'             interval("2022-07-16T05:32:00Z", ".."))) %>%
#'  post_request()
#'
#' # 'T_BEFORE' temporal operator with datetime property
#' req %>%
#'  ext_filter(collection == "landsat-c2-l2" &&
#'             t_before(datetime, timestamp("2022-07-16T05:32:00Z"))) %>%
#'  post_request()
#'
#' # 'T_AFTER' temporal operator with datetime property
#' req %>%
#'  ext_filter(collection == "landsat-c2-l2" &&
#'             t_after(datetime, timestamp("2022-07-16T05:32:00Z"))) %>%
#'   post_request()
