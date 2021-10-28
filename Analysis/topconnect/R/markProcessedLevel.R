markProcessedLevel <- function( compArgs, case, newProcessLevel ) {
#markProcessedLevel <- function( dbName, table, subject, channel, suid, timestamp, flag, hostname='localhost', password='' ) {
  #' markProcessedLevel
  #' 
  #' @export
  #' @examples
  #' case <- list()
  #' case['subject'] <- "testSubject"
  #' case['channel'] <- "testChannel"
  #' case['event_start'] <- 0
  #' conn <- db( 'testProject' )
  #' markAsProcessed( conn, case, flag=1 )

  #print( "In markAsProcessed" )
  dbp <- compArgs$findClass('databasepProvider')
  conn <- dbp$getConnection()
  query <- paste0( 'update ', table, ' set done=',flag,' where subject=\'',subject,'\' AND session=\'',suid,'\' AND channel=\'',channel,'\' AND timestamp=', case['centerTime'],';' )
  DBI::dbGetQuery( conn, query )
  DBI::dbDisconnect( conn )
  
  return( flag )
}
