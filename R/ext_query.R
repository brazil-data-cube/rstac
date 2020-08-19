#' @title Extension functions
#'
#' @author Rolf Simoes
#'
#' @description
#' The \code{extension_query} is the interacting function of the STAC API
#' query extension. It can be used after a call to \code{stac_search} function.
#' It allows that additional fields and operators be queried to an STAC API
#' service to perform a complex filter.
#'
#' The function accepts multiple filter criteria. Each filter entry is an
#' expression formed by \code{<field> <operator> <value>} terms, where
#' \code{<field>} refers to a valid item property. Supported \code{<fields>}
#' depends on STAC API service implementation. The users must rely on service
#' providers' documentation to know which properties can be passed to do the
#' STAC API.
#'
#' The \code{extension_query} function allows the following \code{<operators>}
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
#' \item \code{\%in\%}: corresponds to '\code{in}' and implements a vector
#' search operator.
#' }
#'
#' Besides this function, the following S3 generic methods were implemented
#' to get things done for this extension:
#' \itemize{
#' \item \code{params_get_request} for class \code{ext_type}: raises an error
#' when \code{get_request} is called after a call to \code{extension_query}
#' function.
#' \item The \code{params_post_request} for class \code{ext_type}: calls the
#' default content request params handling.
#' \item The \code{content_get_response} for class \code{ext_query}:
#' raises an error when \code{get_request} is called after the
#' \code{extension_query} function.
#' \item The \code{content_post_response} for class \code{ext_query}: calls
#' the default content response handling.
#' }
#'
#' @param s             a \code{stac} object expressing a STAC search criteria
#' provided by \code{stac_search} function.
#'
#' @param ...           entries with format \code{<field> <operator> <value>}.
#'
#' @inheritParams extensions
#'
#' @seealso \code{\link{stac_search}}, \code{\link{post_request}},
#' \code{\link{params_get_request}}, \code{\link{params_post_request}},
#' \code{\link{content_get_response}}, \code{\link{content_post_response}}
#'
#' @return A \code{ext_query} object containing all request parameters to be
#' passed to \code{post_request} function.
#'
#' @examples
#' \dontrun{
#' # filter items that has 'bdc:tile' property equal to '022024'
#' stac(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'   stac_search(collections = "CB4_64_16D_STK") %>%
#'   extension_query("bdc:tile" == "022024") %>%
#'   post_request()
#' }
#'
#' @export
extension_query <- function(s, ...) {

  # check s parameter
  .check_obj(s, expected = c("search", "ext_query"))

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

  if (length(entries) == 0)
    return(s)

  names(entries) <- uniq_keys

  params[["query"]] <- entries

  content <- .build_stac(url = s$url,
                        endpoint = "/stac/search",
                        params = params,
                        subclass = "ext_query",
                        base_stac = s)

  return(content)
}

#' @rdname extension_query
params_get_request.ext_query <- function(s) {

  .error(paste0("STAC API query extension is not supported by HTTP GET method.",
                "Try use `post_request` method instead."))
}

#' @rdname extension_query
params_post_request.ext_query <- function(s, enctype) {

  # process search params
  params <- params_post_request.search(s, enctype = enctype)

  return(params)
}

#' @rdname extension_query
content_get_response.ext_query <- function(s, res) {

  .error(paste0("STAC API query extension is not supported by HTTP GET method.",
                "Try use `post_request` method instead."))
}

#' @rdname extension_query
content_post_response.ext_query <- function(s, res, enctype) {

  content <- content_post_response.search(s, res = res, enctype = enctype)

  return(content)
}
