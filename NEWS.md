# rstac (development version)

# rstac 1.0.1 (Released 2024-07-18)

* Add support to `jpg` in `preview_plot()` function (#161)
* Fix variable in filter expressions (non-standard evaluation) (#160)
* Fix `limit` parameter as integer type in `stac_search()` and `items()`
* Improve `items_reap()` documentation (#152)

# rstac 1.0.0 (Released 2024-02-14)

* Add support to static catalogs;
* `read_stac()`: read any STAC document from an URL (e.g. Catalog, Collection, Items, or Item);
* `links()`: extract and filter links from a STAC document;
* `link_open()`: open the document referenced by the provided link;
* `read_items()`: read items listed in links section of the provided Collection document;
* `read_collections()`: read collections listed in links section of the provided Catalog document;
* Provide new functions to facilitate work with items;
* `items_as_sf()` / `items_as_sfc()`: convert items into sf objects
* `items_intersects()`: is a helper function to test what items intersect some given geometry.

# rstac 0.9.2-4 (Released 2023-06-15)

* Fix vignette BDC errors

# rstac 0.9.2-3 (Released 2023-04-17)

* Fix renewing mpc tokens per session
* Include support to check package with github actions
* Add issue template

# rstac 0.9.2-2 (Released 2023-02-02)

* Fix mpc vignette

# rstac 0.9.2-1 (Released 2023-01-21)

* Fix intersects bug in `stac_search()` function
* Vignettes are now fault tolerant

# rstac 0.9.2 (Released 2023-01-10)

* Add support to CQL2 filter extension by function `ext_filter()`
* Add `conformance()` and `queryables()` endpoints functions
* Add `assets_rename()` and `assets_url()` assets functions
* Add `items_compact()` function
* General improvements in items functions
* Other minor improvements
* Deprecated `assets_filter()` and `items_group()` functions

# rstac 0.9.1-6 (Released 2021-11-05)

* Fix examples and README where bbox parameter is in wrong order
* Now items without the "match" property can be downloaded
* Introduce a bbox checker
* Fix appveyor bug

# rstac 0.9.1-5 (Released 2021-11-01)

* Add support to `lifecycle` package
* Documentation has been changed from `Rd` to `markdown`
* New functions that support assets and items manipulation have been added: `assets_select`, `assets_filter`, and `items_filter`
* The documentation was reviewed
* New examples and tests were added
* `rstac` now supports signatures in `href` using the `items_sign` function
* Function `items_bands()` was changed to `items_assets()` 
* Parameters `assets_name` and `assets_names` are now called `asset_names`
* Add the `rstac` reference paper in README
* Add feature name on print items objects
* Create `items_next` function to do a single pagination request 
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
