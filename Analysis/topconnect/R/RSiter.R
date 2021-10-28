RSiter <- function( resultset ) {
  #' @export
  #'
  # Arguments:
  # resultset    : A recordset from a MySQL query.
  #
  library( iterators )
  library( itertools )
  
  print( paste0( "nrows in resultset: ", nrow(resultset) ) )
  it <- itertools::ihasNext( iterators::iter( resultset, by="row" ) )
  
  nextEl <- function() {
    n <- iterators::nextElem(it)
  }
  
  hasNx <- function() {
    return( itertools::hasNext(it) )
  }

  obj <- list(nextElem=nextEl,hasNext=hasNx)
  class(obj) <- c('ihasNext', 'abstractiter', 'iter', 'RSiter')
  attr( obj, "size" ) <- nrow( resultset )
  return( obj )
}
