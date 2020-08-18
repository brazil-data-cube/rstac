# TODO: documentation

params_get_request <- function(s) {

  UseMethod("params_get_request")
}

params_post_request <- function(s, enctype) {

  UseMethod("params_post_request")
}

content_get_response <- function(s, res) {

  UseMethod("content_get_response")
}

content_post_response <- function(s, res, enctype) {

  UseMethod("content_post_response")
}
