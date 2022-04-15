appendFileMetadata <- function( compArgs, filename ) {
  #' @export
  db_provider <- compArgs$findClass( 'databaseProvider' )
  print( paste0( "topconnect :: db_provider: ", !is.null(conn) ) )
  conn <- db_provider$connect()
  print( paste0( "topconnect :: conn: ", !is.null(conn) ) )
  mi <- RFactories::metadataInformer( filename=filename, compArgs=compArgs)
  compArgs$add( mi )
  DBI::dbDisconnect( conn )
  return( compArgs )
}

