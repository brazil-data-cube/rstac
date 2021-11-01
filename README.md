
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rstac <img src="inst/extdata/img/logo.png" align="right" width="120"/>

R Client Library for SpatioTemporal Asset Catalog (rstac)

<!-- badges: start -->

[![Software
License](https://img.shields.io/badge/license-MIT-green)](https://github.com/brazil-data-cube/rstac/blob/master/LICENSE)
<!-- [![Build Status](https://drone.dpi.inpe.br/api/badges/brazil-data-cube/rstac/status.svg)](https://drone.dpi.inpe.br/brazil-data-cube/rstac) -->
[![Build
Status](https://cloud.drone.io/api/badges/OldLipe/rstac/status.svg)](https://cloud.drone.io/OldLipe/rstac)
[![Build
status](https://ci.appveyor.com/api/projects/status/73w7h6u46l1587jj?svg=true)](https://ci.appveyor.com/project/OldLipe/rstac)
[![codecov](https://codecov.io/gh/brazil-data-cube/rstac/branch/master/graph/badge.svg?token=ILQLPW19UT)](https://app.codecov.io/gh/brazil-data-cube/rstac)
[![Software Life
Cycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/rstac)](https://cran.r-project.org/package=rstac)
[![STAC
API](https://img.shields.io/badge/STAC%20API-v1.0.0-informational)](https://github.com/radiantearth/stac-api-spec)
[![Join us at
Discord](https://img.shields.io/discord/689541907621085198?logo=discord&logoColor=ffffff&color=7389D8)](https://discord.com/channels/689541907621085198#)
<!-- badges: end -->

STAC is a specification of files and web services used to describe
geospatial information assets. The specification can be consulted in
<https://stacspec.org/>.

R client library for STAC (`rstac`) was designed to fully support STAC
API v1.0.0. It also supports earlier versions (&gt;= v0.8.0).

## Installation

``` r
# install via CRAN 
install.packages("rstac")
```

### Development version

To install the development version of `rstac`, run the following
commands

``` r
# load necessary libraries
library(devtools)
install_github("brazil-data-cube/rstac")
```

Importing `rstac` package:

``` r
library(rstac)
library(magrittr) # for pipe (%>%) in examples
```

## Usage

`rstac` implements the following STAC endpoints:

| **STAC** endpoints                           | `rstac` functions            | API version |
|:---------------------------------------------|:-----------------------------|:------------|
| `/`                                          | `stac()`                     | &gt;= 0.9.0 |
| `/stac`                                      | `stac()`                     | &lt; 0.9.0  |
| `/collections`                               | `collections()`              | &gt;= 0.9.0 |
| `/collections/{collectionId}`                | `collections(collection_id)` | &gt;= 0.9.0 |
| `/collections/{collectionId}/items`          | `items()`                    | &gt;= 0.9.0 |
| `/collections/{collectionId}/items/{itemId}` | `items(feature_id)`          | &gt;= 0.9.0 |
| `/search`                                    | `stac_search()`              | &gt;= 0.9.0 |
| `/stac/search`                               | `stac_search()`              | &lt; 0.9.0  |

These functions can be used to retrieve information from a STAC API
service. The code bellow creates a `stac` object and list the available
collections of the STAC API of the [Brazil Data
Cube](http://brazildatacube.org/) project of the Brazilian National
Space Research Institute [INPE](http://www.inpe.br/).

``` r
s_obj <- stac("https://brazildatacube.dpi.inpe.br/stac/")

get_request(s_obj)
#> ###STACCatalog
#> - id: bdc
#> - description: Brazil Data Cube Catalog
#> - field(s): description, id, stac_version, links
```

The variable `s_obj` stores information to connect to the Brazil Data
Cube STAC web service. The `get_request` method makes a HTTP GET
connection to it and retrieves a STAC Catalog document from the server.
Each `links` entry is an available collection that can be accessed via
STAC API.

In the code bellow, we get some STAC items of `CB4_64_16D_STK-1`
collection that intersects the bounding box passed to the `bbox`
parameter. To do this, we call the `stac_search` function that
implements the STAC `/search` endpoint. The returned document is a STAC
Item Collection (a geojson containing a feature collection).

``` r
it_obj <- s_obj %>% 
    stac_search(collections = "CB4_64_16D_STK-1",
                bbox = c(-47.02148, -12.98314, -42.53906, -17.35063)) %>%
    get_request()

it_obj
#> ###STACItemCollection
#> - matched feature(s): 264
#> - features (10 item(s) / 254 not fetched):
#>   - CB4_64_16D_STK_v001_022024_2021-09-14_2021-09-29
#>   - CB4_64_16D_STK_v001_022025_2021-09-14_2021-09-29
#>   - CB4_64_16D_STK_v001_022024_2021-08-29_2021-09-13
#>   - CB4_64_16D_STK_v001_022025_2021-08-29_2021-09-13
#>   - CB4_64_16D_STK_v001_022024_2021-08-13_2021-08-28
#>   - CB4_64_16D_STK_v001_022025_2021-08-13_2021-08-28
#>   - CB4_64_16D_STK_v001_022024_2021-07-28_2021-08-12
#>   - CB4_64_16D_STK_v001_022025_2021-07-28_2021-08-12
#>   - CB4_64_16D_STK_v001_022024_2021-07-12_2021-07-27
#>   - CB4_64_16D_STK_v001_022025_2021-07-12_2021-07-27
#> - assets: 
#> EVI, NDVI, CMASK, BAND13, BAND14, BAND15, BAND16, CLEAROB, TOTALOB, thumbnail, PROVENANCE
#> - other field(s): type, links, context, features
```

The `rstac` uses the [httr](https://github.com/r-lib/httr) package to
manage HTTP requests, allowing the use of tokens from the authorization
protocols OAuth 1.0 or 2.0 as well as other configuration options. In
the code below, we present an example of how to pass a parameter token
on a HTTP request.

``` r
it_obj <- s_obj %>% 
    stac_search(collections = "CB4_64_16D_STK-1",
                bbox = c(-47.02148, -12.98314, -42.53906, -17.35063)) %>%
    get_request(add_headers("x-api-key" = "MY-TOKEN"))
```

In addition to the functions mentioned above, the `rstac` package
provides some extra functions for handling items and to bulk download
the assets.

### Items functions

`rstac` provides some functions to facilitates the interaction with STAC
data. In the example bellow, we get how many items matched the search
criteria:

``` r
# it_obj variable from the last code example
it_obj %>% items_matched()
#> [1] 264
```

However, if we count how many items there are in `it_obj` variable, we
get `10`, meaning that more items could be fetched from the STAC
service:

``` r
it_obj %>% items_length()
#> [1] 10
```

``` r
# fetch all items from server 
# (but don't stored them back in it_obj)
it_obj %>% items_fetch(progress = FALSE) %>%
    items_length()
#> [1] 264
```

### Download assets

All we’ve got in previous example was metadata to STAC Items, including
links to geospatial data called `assets`. To download all `assets` in a
STAC Item Collection we can use `assets_download()` function, that
returns an update STAC Item Collection referring to the downloaded
assets. The code bellow downloads the `thumbnail` assets (.png files) of
`10` items stored in `it_obj` variable.

``` r
download_items <- it_obj %>% 
    assets_download(assets_name = "thumbnail")
```

## How to contribute?

The `rstac` package was implemented based on an extensible architecture,
so feel free to contribute by implementing new STAC API
[extensions/fragments](https://github.com/radiantearth/stac-api-spec/tree/master/fragments)
based on the STAC API specifications.

1.  Make a project
    [fork](https://docs.github.com/en/github/getting-started-with-github/fork-a-repo).
2.  Create a file inside the `R/` directory called
    `ext_{extension_name}.R`.
3.  In the code, you need to specify a subclass name
    (e.g.`ext_subclass`) for your extension in
    [`RSTACQuery`](https://github.com/OldLipe/rstac/blob/49370251033cca26c6da5b1a38f6d4fa4a83bb96/R/documents.R#L33-L40)
    function constructor, and implement the S3 generics methods:
    [`get_endpoint`](https://github.com/OldLipe/rstac/blob/49370251033cca26c6da5b1a38f6d4fa4a83bb96/R/extensions.R#L87-L90),
    [`before_request`](https://github.com/OldLipe/rstac/blob/49370251033cca26c6da5b1a38f6d4fa4a83bb96/R/extensions.R#L93-L96),
    and
    [`after_response`](https://github.com/OldLipe/rstac/blob/49370251033cca26c6da5b1a38f6d4fa4a83bb96/R/extensions.R#L99-L102).
    Using these S3 generics methods you can define how parameters must
    be submitted to the HTTP request and the types of the returned
    documents responses. See the implemented
    [ext\_query](https://github.com/brazil-data-cube/rstac/blob/master/R/ext_query.R)
    API extension as an example.  
4.  Make a [Pull
    Request](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request)
    on the branch [dev](https://github.com/OldLipe/rstac/tree/dev).

## Getting help

You can get a full explanation about each STAC (v1.0.0) endpoint at
[STAC API
spec](https://github.com/radiantearth/stac-api-spec/tree/master/ogcapi-features).
A detailed documentation with examples on how to use each endpoint and
other functions available in the `rstac` package can be obtained by
typing `?rstac` in R console.

## Citation

To cite rstac in publications use:

R. Simoes, F. C. de Souza, M. Zaglia, G. R. de Queiroz, R. D. C. dos
Santos and K. R. Ferreira, “Rstac: An R Package to Access Spatiotemporal
Asset Catalog Satellite Imagery,” 2021 IEEE International Geoscience and
Remote Sensing Symposium IGARSS, 2021, pp. 7674-7677, doi:
10.1109/IGARSS47720.2021.9553518.
