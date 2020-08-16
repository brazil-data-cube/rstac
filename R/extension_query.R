#' @title Extension functions
#'
#' @author Rolf Simoes
#'
#' @description
#' The \code{extension_query} function implements query API extension (v0.8.0)
#' that can be used in \code{/stac/search} endpoint using HTTP POST method
#' (\code{\link{post_request}}). It allows that additional fields and
#' operators can be passed to \code{\link{stac_search}} function (see examples).
#' Each filter entry is an expression formed by
#' \code{<field> <operator> <value>} terms, where \code{<field>} refers to
#' a valid items' property. Supported \code{<fields>} depends
#' on STAC API service implementation. The users must rely on service providers'
#' documentation to know which properties can be passed to do the search.
#'
#' The \code{extension_query} function accepts the following \code{<operators>}
#' \itemize{
#' \item \code{==} corresponds to '\code{eq}'
#' \item \code{!=} corresponds to '\code{neq}'
#' \item \code{<} corresponds to '\code{lt}'
#' \item \code{<=} corresponds to '\code{lte}'
#' \item \code{>} corresponds to '\code{gt}'
#' \item \code{>=} corresponds to '\code{gte}'
#' \item \code{\%startsWith\%} corresponds to '\code{startsWith}' and implements
#' a string prefix search operator.
#' \item \code{\%endsWith\%} corresponds to '\code{endsWith}' and implements a
#' string suffix search operator.
#' \item \code{\%contains\%}: corresponds to '\code{contains}' and implements a
#' string infix search operator.
#' \item \code{\%in\%}: corresponds to '\code{in}' and implements a vector search
#' operator.
#' }
#'
#' @param s             a \code{stac} object expressing a STAC search criteria
#' provided by \code{stac}, \code{stac_search}, \code{stac_collections},
#' or \code{stac_items} functions.
#'
#' @param ...           entries with format \code{<field> <operator> <value>}.
#'
#' @seealso \code{\link{stac_search}}, \code{\link{post_request}}
#'
#' @return A \code{stac} object containing all request parameters to be
#' passed to \code{stac_search} function.
#'
#' @examples
#' \dontrun{
#'
#' stac(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'   stac_search(collections = "CB4_64_16D_STK") %>%
#'   extension_query("bdc:tile" == "022024") %>%
#'   post_request()
#' }
#'
#' @export
extension_query <- function(s, ...) {

  # check s parameter
  .check_obj(s, "stac")

  # check mutator
  .check_mutator(s, c("search", "ext_query"))

  params <- list()

  dots <- substitute(list(...))[-1]
  tryCatch({
    ops <- lapply(dots, function(x) as.character(x[[1]]))
    keys <- lapply(dots, function(x) as.character(x[[2]]))
    values <- lapply(dots, function(x) eval(x[[3]]))
  }, error = function(e) {

    .error("Invalid query expression.")
  })

  ops <- lapply(ops, function(op) {
    if (op == "==") return("eq")
    if (op == "!=") return("neq")
    if (op == "<") return("lt")
    if (op == "<=") return("lte")
    if (op == ">") return("gt")
    if (op == ">=") return("gte")
    if (op == "%startsWith%") return("startsWith")
    if (op == "%endsWith%") return("endsWith")
    if (op == "%contains%") return("contains")
    if (op == "%in%") return("in")
    .error("Invalid operator '%s'.", op)
  })

  uniq_keys <- unique(keys)
  entries <- lapply(uniq_keys, function(k) {
    res <- lapply(values[keys == k], c)
    names(res) <- ops[keys == k]
    return(res)
  })
  names(entries) <- uniq_keys

  params[["query"]] <- entries

  # TODO: add these code excerpts bellow in different file
  expected <- list("post" =
                     list(enctypes = c("application/json"),
                          responses =
                            list("200" =
                                   list("application/geo+json" = "stac_items",
                                        "application/json" = "stac_items"))))

  content <- build_stac(url = s$url,
                        endpoint = "/stac/search",
                        params = params,
                        expected_responses = expected,
                        mutator = "ext_query",
                        old_stac = s)

  return(content)
}
