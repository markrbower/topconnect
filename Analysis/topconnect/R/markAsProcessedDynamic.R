markAsProcessedDynamic <- function( progressFields, compArgs, case, newProcessLevel ) {
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
  dbp <- compArgs$findClass('databaseProvider')
  conn <- dbp$connect()
  table <- compArgs$get('progress')
  subject <- compArgs$get('subject')
  session <- case$UUID
  channel <- compArgs$get('channel')
  timestamp <- case['centerTime']
  
  query <- paste0( 'update ', table, ' set done=',newProcessLevel,' where ', progressFields$whereConditionString, ";" )
  print( paste0( "markAsProcessedDynamic: ", query ) )
  DBI::dbGetQuery( conn, query )
  DBI::dbDisconnect( conn )
  
  return( newProcessLevel )
}
