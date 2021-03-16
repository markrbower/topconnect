markAsProcessed <- function( dbName, table, subject, channel, suid, timestamp, flag, hostname='localhost', password='' ) {
  #' markAsProcessed
  #' 
  #' @export
  #' @examples
  #' case <- list()
  #' case['subject'] <- "testSubject"
  #' case['channel'] <- "testChannel"
  #' case['event_start'] <- 0
  #' conn <- db( 'testProject' )
  #' markAsProcessed( conn, case, flag=1 )

  conn <- topconnect::db( dbName, host=hostname, password=password )
  query <- paste0( 'update ', table, ' set done=',flag,' where subject=\'',subject,'\' AND session=\'',suid,'\' AND channel=\'',channel,'\' AND timestamp=',timestamp,';' )
  DBI::dbGetQuery( conn, query )
  DBI::dbDisconnect( conn )
  
  return( flag )
}
