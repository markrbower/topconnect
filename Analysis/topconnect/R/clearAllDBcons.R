clearAllDBcons <- function() {
  #' clearAllDBcons
  #' 
  #' @export
  #' @examples
  #' clearAllDBcons()

  all_cons <- DBI::dbListConnections(RMySQL::MySQL())
  for ( con in all_cons )
    DBI::dbDisconnect(con)
  all_cons <- DBI::dbListConnections(RMySQL::MySQL())
  return( length(all_cons) )
}