currentProcessedLevel <- function( compArgs, case, expectedProcessLevel ) {
  #' currentProcessedLevel
  #' 
  #' @export
  #' @examples
  #' currentProcessedLevel()

  T <- case$centerTime # Assumes this comes from an SQL select from the "tasks" table

  #print( paste0( "currentProcessedLevel: dbName: ", dbName ) )
  #print( paste0( "currentProcessedLevel: hostname: ", hostname ) )
  #print( paste0( "currentProcessedLevel: password: ", password ) )
  
  dbp <- compArgs$findClass('databaseProvider')
  conn <- dbp$connect()
  table <- compArgs$get('progress')
  subject <- compArgs$get('subject')
  session <- case$UUID
  channel <- tools::file_path_sans_ext( compArgs$get('channel' ) )
  timestamp <- compArgs$get('centerTime')
  
  #conn <- topconnect::db( db_user="root", dbname=dbName, host=hostname, password=password )
  # If an entry doesn't exist, make one.
  query <- paste0( "select count(*) as count from ", table, " where subject=\'", subject,"\' and channel=\'", channel,"\' and session=\'", session, "\' and timestamp=", timestamp,";" )
  rs <- DBI::dbGetQuery( conn, query )
  count <- rs$count
  if ( count==0 ) {
    query <- paste0( "insert into ", table, " (subject,session,channel,timestamp,done) values (\'",subject,"\',\'", session, "\',\'", channel, "\', ", timestamp, ",0);" )
    rs <- DBI::dbGetQuery( conn, query )
  }
  
  query <- paste0( "select done from ", table, " where subject=\'", subject,"\' and session=\'", session,"\' and channel=\'", channel, "\' and timestamp=", timestamp,";" )
  rs <- DBI::dbGetQuery( conn, query )
  value <- rs$done
  
  DBI::dbDisconnect( conn )
  return( value == expectedProcessLevel )
}
