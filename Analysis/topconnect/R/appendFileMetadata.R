appendFileMetadata <- function( compArgs, filename ) {
  #' @export
  db_provider <- compArgs$findClass( 'databaseProvider' )
  conn <- db_provider$connect()
  mi <- RFactories::metadataInformer( filename=filename, compArgs=compArgs)
  compArgs$add( mi )
  DBI::dbDisconnect( conn )
  return( compArgs )
}

