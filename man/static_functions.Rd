% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/static-funs.R
\name{static_functions}
\alias{static_functions}
\alias{read_stac}
\alias{read_items}
\alias{read_collections}
\alias{links}
\alias{link_open}
\title{Static functions}
\usage{
read_stac(url, ...)

read_items(collection, ..., limit = 100, page = 1, progress = TRUE)

read_collections(catalog, ..., limit = 100, page = 1, progress = TRUE)

links(x, ...)

link_open(link, base_url = NULL)
}
\arguments{
\item{url}{a \code{character} value with the URL to a valid STAC document.}

\item{...}{additional arguments. See details.}

\item{collection}{a \code{doc_collection} object to fetch all
\code{rel=="item"} links.}

\item{limit}{an \code{integer} with defining the page size of items to fetch.}

\item{page}{an \code{integer} with the page number to fetch the items.}

\item{progress}{a \code{logical} indicating if a progress bar must be
shown or not. Defaults to \code{TRUE}.}

\item{catalog}{a \code{doc_catalog} object to fetch all \code{rel=="child"} links.}

\item{x}{any \code{rstac} document with \code{'links'} key entry.}

\item{link}{a \code{doc_link} object, usually an element of \code{links} key entry.}

\item{base_url}{a \code{character} with the base URL to resolve relative links.
If \code{NULL} (default) \code{rstac} will try resolve relative links using
internal metadata.}
}
\value{
\itemize{
\item \code{links()}: a \code{doc_links} object containing a list of \code{link} entries.

\item \code{link_open()}: a recognizable \code{rstac} document.
}
}
\description{
These functions provide support to work with static catalogs.

\itemize{
\item \code{stac_read()}: open a STAC document from an URL.

\item \code{read_items()}: opens (statically) all items referred in \code{links}
key entry of a given collection document (\code{doc_collection}).

\item \code{links()}: extracts and filters the links of any STAC document.

\item \code{link_open()}: opens (statically) the document referenced by
the link. This function can resolve any relative URL.
}
}
\details{
Ellipsis argument (\code{...}) may appears in different items functions and
has distinct purposes:
\itemize{

\item \code{stac_read()}: ellipsis is used to pass any additional parameters
to \link[jsonlite:read_json]{read_json} function.

\item \code{links()}: ellipsis is used to pass logical expressions to be
evaluated against a \code{doc_link} item as a filter criteria. See examples.

}
}
\examples{
\dontrun{
 x <- stac("https://brazildatacube.dpi.inpe.br/stac") \%>\%
     collections("CB4-16D-2") \%>\%
     get_request()

 link <- links(x, rel == "items")
 link_open(link[[1]])
}

\dontrun{
 wv_url <- paste0(
   "https://s3.eu-central-1.wasabisys.com",
   "/stac/openlandmap/wv_mcd19a2v061.seasconv/collection.json"
 )
 wv <- read_stac(wv_url)
 stac_type(wv)  # Collection

 # reads the second page of 5 links
 wv_items <- read_items(wv, limit = 5, page = 2)

 # lists all links of the collection document that are not items
 links(wv, rel != "item")

 # lists all links of the items document
 links(wv_items)
}

}
