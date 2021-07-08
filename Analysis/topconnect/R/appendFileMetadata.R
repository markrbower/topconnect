appendFileMetadata <- function( fileename, ac ) {
  db_provider <- ac$findClass( 'databaseProvider' )
  conn <- db_provider$get_connection()
  argCompMet <- RFactories::metadataInformer( ac )
  argComp$add( argCompMet )
  DBI::dbDisconnect( conn )
  return( argComp )
}

