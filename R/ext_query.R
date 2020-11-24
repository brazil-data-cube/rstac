#' @title Extension functions
#'
#' @description
#' The \code{ext_query()} is the \emph{exported function} of the STAC API
#' query extension. It can be used after a call to \code{stac_search()} function.
#' It allows that additional fields and operators other than those defined in
#' \code{stac_search()} function be used to make a complex filter.
#'
#' The function accepts multiple filter criteria. Each filter entry is an
#' expression formed by \code{<field> <operator> <value>}, where
#' \code{<field>} refers to a valid item property. Supported \code{<fields>}
#' depends on STAC API service implementation. The users must rely on service
#' providers' documentation to know which properties can be used by this
#' extension.
#'
#' The \code{ext_query()} function allows the following \code{<operators>}
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
#' \item The \code{endpoint()} for subclass \code{ext_query}
#' \item The \code{before_request()} for subclass \code{ext_query}
#' \item The \code{after_response()} for subclass \code{ext_query}
#' }
#' See source file \code{ext_query.R} for an example on how implement new
#' extensions.
#'
#' @param q      a \code{RSTACQuery} object expressing a STAC query
#' criteria.
#'
#' @param ...    entries with format \code{<field> <operator> <value>}.
#'
#' @seealso \code{\link{stac_search}}, \code{\link{post_request}},
#' \code{\link{endpoint}}, \code{\link{before_request}},
#' \code{\link{after_response}}, \code{\link{content_response}}
#'
#' @return
#' A \code{RSTACQuery} object  with the subclass \code{ext_query} containing
#'  all request parameters to be passed to \code{post_request()} function.
#'
#' @examples
#' \donttest{
#' library(magrittr)
#'
#' stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1") %>%
#'   ext_query("bdc:tile" == c("022024")) %>%
#'   post_request()
#' }
#'
#' @export
ext_query <- function(q, ...) {

  # check s parameter
  check_subclass(q, c("search", "ext_query"))

  # get the env parent
  env_parent <- parent.frame()

  params <- list()
  if (!is.null(substitute(list(...))[-1])) {
    dots <- substitute(list(...))[-1]
    tryCatch({
      ops <- lapply(dots, function(x) as.character(x[[1]]))
      keys <- lapply(dots, function(x) as.character(x[[2]]))
      values <- lapply(dots, function(x) eval(x[[3]], env_parent))
    }, error = function(e) {

      .error("Invalid query expression.")
    })
  }

  values <- .parse_ops(values, ops)

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
    return(q)

  names(entries) <- uniq_keys
  params[["query"]] <- entries

  RSTACQuery(version = q$version,
             base_url = q$base_url,
             params = utils::modifyList(q$params, params),
             subclass = "ext_query")
}

#' @export
endpoint.ext_query <- function(q) {

  # using endpoint from search document
  endpoint.search(q)
}

#' @export
before_request.ext_query <- function(q) {

  msg <- paste0("Query extension param is not supported by HTTP GET",
                "method. Try use `post_request()` method instead.")

  check_query_verb(q, verbs = "POST", msg = msg)

  return(q)
}

#' @export
after_response.ext_query <- function(q, res) {

  content <- content_response(res, "200", c("application/geo+json",
                                            "application/json"))

  RSTACDocument(content = content, q = q, subclass = "STACItemCollection")
}
