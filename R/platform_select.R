#' Select function
#'
#' This functions allows me to retrieving specific table records from the digital rail platform, and decode the json format into a tibble.
#' @param table_name The name of the table which you wawnt to get
#' @param previous_seconds The number of previous second to pull
#' @keywords api
#' @export
#'
#'
platform_select <- function(table_name, previous_seconds){

  #Take current time in milliseconds
  current_time <- as.numeric(as.POSIXct(Sys.time())) * 1000
  start_time <- current_time - previous_seconds * 1000

  select_url <- "http://digitalrail-platform:8091/datapoint/table/record/select"

  attributes = list(list(name = "accelerometer_x", type = "float", as = ""),
                    list(name = "accelerometer_y", type = "float", as = ""),
                    list(name = "accelerometer_z", type = "float", as = ""),
                    list(name = "gyroscope_x", type = "float", as = ""),
                    list(name = "gyroscope_y", type = "float", as = ""),
                    list(name = "gyroscope_z", type = "float", as = ""),
                    list(name = "humidity", type = "float", as = ""),
                    list(name = "node_id", type = "text", as = ""),
                    list(name = "temperature", type = "float", as = ""),
                    list(name = "time", type = "timestamp", as = "")
  )
  clauses = list(
    list(column = "time", operator = ">", value = round(start_time, 0), type = "timestamp")
  )
  request_body <- jsonlite::toJSON(list(table = table_name, attributes = attributes, clauses = clauses), pretty = TRUE, auto_unbox = TRUE)

  # Send request
  r <- httr::POST(select_url, body = request_body, encode = "json")

  # Unpack JSON response
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
