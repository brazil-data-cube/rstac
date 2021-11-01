# rstac (development version)

# rstac 0.9.1-5 (Released 2021-11-01)

* Add the `rstac` reference paper in README
* Add feature name on print items objects
* Create `items_next` function
* Adjust download helpers to access data from Microsoft's Planetary Computer
* Update news.md of v0.9.0-1 

# rstac 0.9.1-4 (Released 2021-09-14)

* Fix typos
* Update docs
* Update tests
* Update fixtures files

# rstac 0.9.1-3 (Released 2021-06-25)

* Fix in bug related with `items_fetch` in using bbox as parameter #41
* Update fixtures and introduces new parameter in items_fetch #39

# rstac 0.9.1-2 (Released 2021-05-28)

* Fix pagination in items_fetch function to conform to STAC API spec
* Improve README

# rstac 0.9.0-2 (Released 2021-03-24)

* Change version 0.9.0 to 0.9.0-2 in DESCRIPTION
* Fix assets without extension in assets_download function

# rstac 0.9.0-1 (Released 2020-10-20)

* Update `ext_query` function
* Add `magrittr` package in depends
* Add function items_bands
* Introduces `items_bbox` and `items_datetime` functions
* Update print.R file
* Fix items_fetch
* Update `assets_download.R` and others functions
* Add function 'get_assets_name'
* Add overwrite and items_max parameter; solve issue #10
* Fix bug in stac_matched and update documentation
* Change function name `.verify_datime()'` to `parse_datetime()`
* Rename function `extension_query` to `ext_query`

# rstac 0.9.0 (Released 2020-09-18)

* Fix issue from CRAN.
* Preparing package to send to CRAN.
* Update documentation.
* Add function for extensions creating.
* Add new tests.
* Support for STAC version 0.9.0 and 0.8.1.

# rstac 0.8.1 (Released 2020-08-24)

* Support for STAC version 0.8.0 and 0.8.1.
* Add functions to download and support analysis from items objects.
* Add new tests.
* License: `MIT <https://raw.githubusercontent.com/brazil-data-cube/rstac/b-0.9.0/LICENSE>`_ .
* Update documentation.
* Add CI support.
* Add tests for CI.
* Unit-tests with code coverage.
