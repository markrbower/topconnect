getFileMetadata <- function( ac ) {
  filename <- file.path( ac$get('path'), ac$get('channel'), fsep=.Platform$file.sep )
  db_provider <- ac$findClass( 'databaseProvider' )
  conn <- db_provider$get_connection()
  argCompMet <- getFileMetadata( conn, ac )
  argComp$add( argCompMet )
  DBI::dbDisconnect( conn )
}

