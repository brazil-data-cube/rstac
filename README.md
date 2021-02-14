
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rstac <img src="inst/extdata/img/logo.png" align="right" width="120"/>

R Client Library for SpatioTemporal Asset Catalog (rstac)

<!-- badges: start -->

[![Software
License](https://img.shields.io/badge/license-MIT-green)](https://github.com/brazil-data-cube/rstac/blob/master/LICENSE)
[![Build
Status](https://drone.dpi.inpe.br/api/badges/brazil-data-cube/rstac/status.svg)](https://drone.dpi.inpe.br/brazil-data-cube/rstac)
[![Build
status](https://ci.appveyor.com/api/projects/status/73w7h6u46l1587jj?svg=true)](https://ci.appveyor.com/project/OldLipe/rstac)
[![codecov](https://codecov.io/gh/brazil-data-cube/rstac/branch/master/graph/badge.svg?token=ILQLPW19UT)](https://codecov.io/gh/brazil-data-cube/rstac)
[![Software Life
Cycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/rstac)](https://cran.r-project.org/package=rstac)
[![Join us at
Discord](https://img.shields.io/discord/689541907621085198?logo=discord&logoColor=ffffff&color=7389D8)](https://discord.com/channels/689541907621085198#)
<!-- badges: end -->

STAC is a specification of files and web services used to describe
geospatial information assets. The specification can be consulted in
<https://stacspec.org/>.

R client library for STAC (`rstac`) was designed to fully support STAC
v0.8.1 and v0.9.0. As STAC spec is evolving fast and reaching its
maturity, we plan update `rstac` to support upcoming STAC 1.0.0 version
soon.

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

| **STAC** endpoints                           | `rstac` functions            |
| :------------------------------------------- | :--------------------------- |
| `/stac`                                      | `stac()`                     |
| `/collections`                               | `collections()`              |
| `/collections/{collectionId}`                | `collections(collection_id)` |
| `/collections/{collectionId}/items`          | `items()`                    |
| `/collections/{collectionId}/items/{itemId}` | `items(feature_id)`          |
| `/stac/search`                               | `stac_search()`              |

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

In the code bellow, we get some STAC items of `MOD13Q1` collection that
intersects the bounding box passed to the `bbox` parameter. To do this,
we call the `stac_search` function that implements the STAC
`/stac/search` endpoint. The returned document is a STAC Item Collection
(a geojson containing a feature collection).

``` r
it_obj <- s_obj %>% 
    stac_search(collections = "CB4_64_16D_STK-1",
                bbox = c(-47.02148, -12.98314, -42.53906, -17.35063)) %>%
    get_request()

it_obj
#> ###STACItemCollection
#> - matched feature(s): 212
#> - features (10 item(s)):
#>   - CB4_64_16D_STK_v001_022025_2020-07-27_2020-08-11
#>   - CB4_64_16D_STK_v001_022024_2020-07-27_2020-08-11
#>   - CB4_64_16D_STK_v001_022024_2020-07-11_2020-07-26
#>   - CB4_64_16D_STK_v001_022025_2020-07-11_2020-07-26
#>   - CB4_64_16D_STK_v001_022024_2020-06-25_2020-07-10
#>   - CB4_64_16D_STK_v001_022025_2020-06-25_2020-07-10
#>   - CB4_64_16D_STK_v001_022024_2020-06-09_2020-06-24
#>   - CB4_64_16D_STK_v001_022025_2020-06-09_2020-06-24
#>   - CB4_64_16D_STK_v001_022024_2020-05-24_2020-06-08
#>   - CB4_64_16D_STK_v001_022025_2020-05-24_2020-06-08
#> - field(s): type, links, context, features
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
criteria, which shows `210`:

``` r
# it_obj variable from the last code example
it_obj %>% items_matched()
#> [1] 212
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
#> [1] 212
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
[extensions](https://github.com/radiantearth/stac-spec/tree/v0.9.0/api-spec/extensions)
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

You can get a full explanation about each STAC (v0.9.0) endpoint at
[STAC spec
GitHub](https://github.com/radiantearth/stac-spec/tree/v0.9.0). A
detailed documentation with examples on how to use each endpoint and
other functions available in the `rstac` package can be obtained by
typing `?rstac` in R console.
