# rstac <img src="inst/extdata/img/logo.png" align="right" width="120" />
R Client Library for SpatioTemporal Asset Catalog (rstac)

[![Travis build status](https://travis-ci.com/OldLipe/stac.R.svg?branch=master)](https://travis-ci.com/OldLipe/stac.R) [![Build status](https://ci.appveyor.com/api/projects/status/73w7h6u46l1587jj?svg=true)](https://ci.appveyor.com/project/OldLipe/stac-r) [![codecov](https://codecov.io/gh/OldLipe/stac.R/branch/master/graph/badge.svg)](https://codecov.io/gh/OldLipe/stac.R)

STAC is a specification of files and web services used to describe geospatial 
information assets. The specification can be consulted in 
[https://stacspec.org/].

R client library for STAC (`rstac`) was designed to fully support STAC v0.8.1. 
As STAC spec is evolving fast and reaching its maturity, we plan update `rstac` 
to support upcoming STAC 1.0.0 version soon.

## Installation

To install the development version of `rstac`, run the following commands

```R
library(devtools)
install_github("brazil-data-cube/rstac")
```

## Usage

In this version, we implemented the following STAC endpoints
- `'/stac'`
- `'/collections'`
- `'/collections/{collectionId}'`
- `'/collections/{collectionId}/items'`
- `'/collections/{collectionId}/items/{itemId}'`
- `'/stac/search'`

Let us begin our example by creating a `stac` object and list the available 
collections of the STAC API of the 
[Brazil Data Cube](http://brazildatacube.org/) project of the Brazilian 
National Space Research Institute (INPE).

```R
# Create a stac object
s_obj <- stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0")
get_request(s_obj) %>%
    print()
#### STAC Catalog
#- stac_version: "0.8.0"
#- id: "bdc"
#- description: "Brazil Data Cube Catalog"
#- links:
#  - CB4_64 (http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/collections/CB4_64)
#  - CB4_64_16D_STK (http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/collections/CB4_64_16D_STK)
#  - CB4_64_16D_STK_v1 (http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/collections/CB4_64_16D_STK_v1)
#  - CB4_64_v1 (http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/collections/CB4_64_v1)
#  - CB4A_55 (http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/collections/CB4A_55)
#  - CB4A_55_1M_STK (http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/collections/CB4A_55_1M_STK)
#  - CB4MOSBR_64 (http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/collections/CB4MOSBR_64)
#  - CB4MOSBR_64_1M_STK (http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/collections/CB4MOSBR_64_1M_STK)
#  - CB4MOSBR_64_3M_MED (http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/collections/CB4MOSBR_64_3M_MED)
#  - CB4MOSBR_64_3M_STK (http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/collections/CB4MOSBR_64_3M_STK)
#> … with 17 more links
```

Here, the variable `s_obj` stores informations to connect to the Brazil Data 
Cube STAC web service. The `get_request` method makes a HTTP GET connection
and retrieves a STAC Catalog with all available collections.

```R
# Create a stac_items object and return STAC items
it_obj <- 
    stac_search(s_obj,
                collections = "MOD13Q1",
                bbox = c(-55.16335, -4.26325, -49.31739, -1.18355)) %>%
    get_request()
```

You can get a full explanation about each STAC (v0.8.1) endpoint at [STAC spec GitHub](https://github.com/radiantearth/stac-spec/tree/v0.8.1).

In addition to the functions mentioned above, the `rstac` package provides some 
extra functions for handling items and bulk download the assets.

### Items functions

```R
# Count how many items matched the search criteria
it_obj %>% items_matched()

# Count how many items are in the `stac_items` object
it_obj %>% items_length()
```

### Download assets

```R
# Downloads the assets provided by the STAC API
download_items <- 
  it_obj %>% assets_download(assets_name = c("thumbnail"))
```

## How to contribute?

The `rstac` package was implemented based on an extensible architecture, so 
feel free to contribute by implementing new STAC API 
[extensions](https://github.com/radiantearth/stac-spec/tree/v0.8.1/api-spec/extensions) 
based on the STAC API specifications.

1. Make a project 
[fork](https://docs.github.com/en/github/getting-started-with-github/fork-a-repo)
2. Create a file inside the `R/` directory called `ext_{extension_name}.R`.
3. In the code, you need to specify a subclass name (e.g.`ext_subclass`) for 
your extension and implement the S3 generics methods, `params_get_request`, 
`content_get_response` (for HTTP GET) and/or `params_post_request`, 
`content_post_response` (for HTTP POST). Using these S3 generics methods you 
can define how parameters must be submited to the HTTP request and the types 
of the returned documents responses. See the implemented [ext_query](https://github.com/OldLipe/rstac/blob/master/R/extension_query.R) 
API extension as an example.  
4. Make a [Pull Request](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request) on the branch dev.
