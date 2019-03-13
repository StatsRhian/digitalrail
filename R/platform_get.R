#' Get function
#'
#' This functions allows me to get a table from the digital rail platform, and decode the json format into a tibble.
#' @param table_name The name of the table which you wawnt to get
#' @keywords api
#' @export
#'
#'
platform_get <- function(table_name){
  get_url <- "http://digitalrail-platform:8091/datapoint/table/get"
  r <- httr::GET(get_url, query = list(table = table_name))
  input_data <- content(r)$object$data
  names_data <- content(r)$object$attributes

  df <- lapply(input_data, function(x){
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })

  df <- tibble::as.tibble(t(as.data.frame(df)))
  names(df) <- names_data
  return(df)
}



