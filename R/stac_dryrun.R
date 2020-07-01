
#' @title stac functions
#'
#' @author Rolf Simoes
#'
#' @description This function returns the URL requests that would be
#' performed by stac commands without actually running them.
#'
#' @param expr       A stac command expression to be evaluated.
#'
#' @return A \code{character} string containing the http URL request of the
#' stac command. If no \code{expr} is informed, returns \code{FALSE}.
#'
#' @examples
#' stac_dryrun(stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0"))
#' stac_dryrun({
#'   stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'               collections = "MOD13Q1",
#'               bbox = c(-55.16335, -4.26325, -49.31739, -1.18355))
#'             })
#'
#' @export
stac_dryrun <- function(expr) {
  if (!missing(expr)) {

    .dryrun <- TRUE
    return((expr))
    .dryrun # just to avoid warnings like 'not used variable'
  }

  dynGet(".dryrun", ifnotfound = FALSE)
}
