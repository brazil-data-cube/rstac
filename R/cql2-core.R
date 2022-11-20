
# cql2 core classes ----

# - cql2_logic_op
# - cql2_not_op
# - cql2_comp_op
# - cql2_isnull_op
# - cql2_math_op
# - cql2_minus_op
# - cql2_time
# - cql2_date
# - cql2_interval
# - cql2_prop_ref

# constructor functions ----

# Boolean expressions
new_logic_op <- function(op) {
    function(a, b) {
        a <- cql2_eval(a)
        b <- cql2_eval(b)
        stopifnot(is_bool_expr(a))
        stopifnot(is_bool_expr(b))
        structure(list(op = op, args = list(a, b)),
                  class = c("cql2_logic_op", "list"))
    }
}

not_op <- function(a) {
    a <- cql2_eval(a)
    stopifnot(is_bool_expr(a))
    structure(list(op = "not", args = list(a)),
              class = c("cql2_not_op", "list"))
}

# binary comparison operators
new_comp_op <- function(op) {
    function(a, b) {
        a <- cql2_eval(a)
        b <- cql2_eval(b)
        stopifnot(is_scalar(a))
        stopifnot(is_scalar(b))
        structure(list(op = op, args = list(a, b)),
                  class = c("cql2_comp_op", "list"))
    }
}

# is_null operator
isnull_op <- function(a) {
    a <- cql2_eval(a)
    stopifnot(is_isnull_operand(a))
    structure(list(op = "isNull", args = list(a)),
              class = c("cql2_isnull_op", "list"))
}

# basic math operators
new_math_op <- function(op) {
    function(a, b = NULL) {
        a <- cql2_eval(a)
        b <- cql2_eval(b)
        stopifnot(is_num_expr(a))
        stopifnot(is_num_expr(b))
        structure(list(op = op, args = list(a, b)),
                  class = c("cql2_math_op", "list"))
    }
}

minus_op <- function(a, b) {
    a <- cql2_eval(a)
    stopifnot(is_num_expr(a))
    if (missing(b))
        args <- list(a)
    else {
        b <- cql2_eval(b)
        stopifnot(is_num_expr(b))
        args <- list(a, b)
    }
    structure(list(op = "-", args = args),
              class = c("cql2_minus_op", "list"))
}

# temporal literals
timestamp_lit <- function(x) {
    x <- cql2_eval(x)
    stopifnot(is_time(x))
    structure(list(timestamp = x),
              class = c("cql2_timestamp", "list"))
}

date_lit <- function(x) {
    x <- cql2_eval(x)
    stopifnot(is_date(x))
    structure(list(date = x),
              class = c("cql2_date", "list"))
}

interval_lit <- function(start = "..", end = "..") {
    start <- cql2_eval(start)
    end <- cql2_eval(end)
    if (start != "..")
        stopifnot(is_temporal(start))
    if (end != "..")
        stopifnot(is_temporal(end))
    structure(list(interval = list(start, end)),
              class = c("cql2_interval", "list"))
}

# input property identifiers
prop_ref <- function(a) {
    a <- cql2_eval(a)
    stopifnot(is_prop_name(a))
    structure(list(property = a),
              class = c("cql2_prop_ref", "list"))
}

get_all_props <- function(expr) {
    props <- all_names(expr)
    names(props) <- props
    lapply(props, prop_ref)
}

# input property identifiers
func_def <- function(a) {
    stopifnot(is_func_name(a))
    function(...) {
        structure(list(`function` = list(name = a, args = list(...))),
                  class = c("cql2_func", "list"))
    }
}

get_all_funcs <- function(expr) {
    funcs <- all_calls(expr)
    names(funcs) <- funcs
    lapply(funcs, func_def)
}

# cql2 environments ----

# cql2
cql2_global_env <- new_env(
    # basic R functions and constants
    `{` =     `{`,
    `(` =     `(`,
    `T` =     TRUE,
    `TRUE` =  TRUE,
    `F` =     FALSE,
    `FALSE` = FALSE,
    list =    list,
    c =       list,
    `:` =     function(from, to) {
        stopifnot(is_num(from))
        stopifnot(is_num(to))
        as.list(seq(from, to))
    }
)

# environment of expression's identifiers (i.e. properties and functions)
cql2_ident_env <- new_env(global_env = cql2_global_env)

# environment for cql2 core evaluation
cql2_core_env <- new_env(
    # cql2 basic expressions
    # Boolean expressions
    `&&` = new_logic_op("and"),
    `&` =  new_logic_op("and"),
    `||` = new_logic_op("or"),
    `|` =  new_logic_op("or"),
    `!` =  not_op,
    # comparison predicate
    # binary comparison operators
    `==` = new_comp_op("="),
    `!=` = new_comp_op("<>"),
    `<` =  new_comp_op("<"),
    `>` =  new_comp_op(">"),
    `<=` = new_comp_op("<="),
    `>=` = new_comp_op(">="),
    # is_null operator
    `is_null` = isnull_op,
    # basic math operators
    `-` = minus_op, # can be both binary and unary operator
    `+` = new_math_op("+"),
    `*` = new_math_op("*"),
    `/` = new_math_op("/"),
    # temporal literals
    timestamp =  timestamp_lit,
    date =       date_lit,
    interval =   interval_lit,
    global_env = cql2_global_env
)

# convert to cql2 ----

cql2_update_ident_env <- function(expr, queryables = NULL, functions = NULL) {

    # order of evaluation:
    # cql2_env --> cql2_core_env --> cql2_ident_env.
    # update `ident_env` environment with all input properties
    rm(list = ls(cql2_ident_env, all.names = TRUE), envir = cql2_ident_env)
    list2env(get_all_props(expr), envir = cql2_ident_env) # ENDPOINT: queryables
    list2env(get_all_funcs(expr), envir = cql2_ident_env) # ENDPOINT: functions
}
