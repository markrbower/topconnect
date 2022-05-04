currentProcessedLevelDynamic <- function( progressFields, compArgs, case, expectedProcessLevel ) {
  #' currentProcessedLevel
  #' 
  #' @export
  #' @examples
  #' currentProcessedLevel()

  T <- case$centerTime # Assumes this comes from an SQL select from the "tasks" table

  dbp <- compArgs$findClass('databaseProvider')
  conn <- dbp$connect()
  table <- compArgs$get('progress')
  print( paste0( "currentProcessedLevel: ", table ) )
  subject <- compArgs$get('subject')
  session <- case$UUID
  channel <- tools::file_path_sans_ext( compArgs$get('channel' ) )
  print( channel )
  timestamp <- case['centerTime']
  if ( is.na(session) | is.na(timestamp) ) {
    print( "ERROR: topconnect::currentProcesssedLevel encountered a bad case.")
    return(FALSE)
  }
  
  #conn <- topconnect::db( db_user="root", dbname=dbName, host=hostname, password=password )
  # If an entry doesn't exist, make one.
  query <- paste0( "select count(*) as count from ", table, " where ", progressFields$whereConditionString, ";" )
  print( query )
  rs <- DBI::dbGetQuery( conn, query )
  count <- rs$count
  print( paste0( "topconnect: currentProcessedLevel: count = ", count ) )
  if ( count==0 ) {
    query <- paste0( "insert into ", table, " (", progressFields$insertNames, ",done) values (", progressFields$insertValues, ",", expectedProcessLevel, ");" )
    print( paste0( "topconnect: currentProcessedLevel: query = ", query ) )
    rs <- DBI::dbGetQuery( conn, query )
  }
  
  query <- paste0( "select done from ", table, " where ", progressFields$whereConditionString, ";" )
  rs <- DBI::dbGetQuery( conn, query )
  value <- rs$done
  
  DBI::dbDisconnect( conn )
  return( value == expectedProcessLevel )
}
