#' The function that read data & create batch id column
#' 
#' @importFrom data.table fread
#' @importFrom stringr str_extract
#' 
#' @param data_dir the path of csv file
#' @param indicator the indicator of columns
#' 

read_df <- function( data_dir, indicator = NULL ){
  # read csv file as data.table
  df = data.table::fread(input = data_dir)
  
  # batch id
  df[, batch := stringr::str_extract(string = batch, pattern = "..[0-9]+") ]
  
  # column name
  if( !is.null(x = indicator) ){
    idx = !names(x = df) %in% c("batch", "filtration", "group")
    names(x = df)[idx] <- paste0(indicator, "_", names(x = df)[idx] )
  }
  
  return( df)
}
