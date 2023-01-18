eval_filter_expr <- function(f, expr) {
  # NOTE: this tryCatch will be removed in next versions.
  # We will no longer support the old way of filter evaluation
  val <- tryCatch({
    f$properties$properties <- NULL
    eval(expr, envir = f$properties,
         enclos = parent.env(parent.frame()))
  }, error = function(e) {
    return(NULL)
  })

  if (length(val) == 0) {
    val <- tryCatch({
      eval(expr, envir = f, enclos = parent.env(parent.frame()))
    }, error = function(e) {
      return(FALSE)
    })
  }

  if (length(val) == 0) {
    val <- FALSE
  }
  return(val)
}

eval_filter_fn <- function(f, filter_fn) {
  # NOTE: this tryCatch will be removed in next versions.
  # We will no longer support the old way of filter evaluation
  val <- tryCatch({
    f$properties$properties <- NULL
    filter_fn(f$properties)
  }, error = function(e) {
    return(NULL)
  })

  if (length(val) == 0) {
    val <- tryCatch({
      filter_fn(f)
    }, error = function(e) {
      return(FALSE)
    })
  }

  if (length(val) == 0) {
    val <- FALSE
  }
  return(val)
}

# NOTE: this function will be removed in next versions.
# We will no longer support the old way of filter evaluation
check_old_expression <- function(items, expr) {
  val <- map_lgl(items$features, function(f) {
    f$properties$properties <- NULL
    tryCatch({
      val <- eval(expr, envir = f$properties,
                  enclos = parent.env(parent.frame()))
      is.logical(val) && length(val) > 0
    }, error = function(e) {
      return(FALSE)
    })
  })
  return(any(val))
}

# NOTE: this function will be removed in next versions.
# We will no longer support the old way of filter evaluation
check_old_fn <- function(items, fn) {
  val <- map_lgl(items$features, function(f) {
    f$properties$properties <- NULL
    tryCatch({
      val <- fn(f$properties)
      is.logical(val) && length(val) > 0
    }, error = function(e) {
      return(FALSE)
    })
  })
  return(any(val))
}
