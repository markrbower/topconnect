RSiter <- function( resultset ) {
  #' @export
  #'
  # Arguments:
  # resultset    : A recordset from a MySQL query.
  #
  library( iterators )
  library( itertools )
  
  print( paste0( "nrows in resultset: ", nrow(resultset) ) )
  it <- iter( resultset, by="row" )
  
  nextEl <- function() {
    n <- nextElem(it)
  }

  obj <- list(nextElem=nextEl,hasNext=ihasNext)
  obj <- ihasNext(it)
  class(obj) <- c('ihasNext', 'abstractiter', 'iter', 'RSiter')
  attr( obj, "size" ) <- nrow( resultset )
  return( obj )
}
