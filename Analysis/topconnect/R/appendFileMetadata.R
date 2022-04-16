appendFileMetadata <- function( compArgs, filename ) {
  #' @export
  db_provider <- compArgs$findClass( 'databaseProvider' )
  print( paste0( "topconnect :: db_provider: ", !is.null(db_provider) ) )
  conn <- db_provider$connect()
  print( paste0( "topconnect :: conn: ", !is.null(conn) ) )
  mi <- RFactories::metadataInformer( filename=filename, compArgs=compArgs)
  print( paste0( "RFactories :: mi: ", !is.null(mi) ) )
  compArgs$add( mi )
  DBI::dbDisconnect( conn )
  return( compArgs )
}

