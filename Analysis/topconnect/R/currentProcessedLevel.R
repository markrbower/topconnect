currentProcessedLevel <- function( dbName, table, subject, channel, suid, timestamp, hostname='localhost', password='' ) {
  #' currentProcessedLevel
  #' 
  #' @export
  #' @examples
  #' currentProcessedLevel()

  T <- timestamp
  
  conn <- topconnect::db( dbname=dbName, host=hostname, password=password )
  # If an entry doesn't exist, make one.
  query <- paste0( 'select count(*) as count from ', table, ' where subject=\'',subject,'\' and channel=\'', channel,'\' and session=\'', suid, '\' and timestamp=', T,';' )
  rs <- DBI::dbGetQuery( conn, query )
  count <- rs$count
  if ( count==0 ) {
    query <- paste0( 'insert into ', table, ' (subject,session,channel,timestamp,done) values (\'',subject,'\',\'', suid, '\',\'', channel, '\', ', timestamp, ', 0);' )
    rs <- DBI::dbGetQuery( conn, query )
  }
  
  query <- paste0( 'select done from ', table, ' where subject=\'',subject,'\' and session=\'', suid,'\' and channel=\'', channel, '\' and timestamp=', T,';' )
  rs <- DBI::dbGetQuery( conn, query )
  value <- rs$done
  DBI::dbDisconnect( conn )
  return( value )
}
