#'
#'
#'
.query_format <- function(query){
  if(length(query) > 0){
    if("bbox" %in% names(query)){
      query$bbox <- paste(query$bbox, collapse = ",")
    }
  } else{
    return(invisible(NULL))
  }
  return(query)
}
