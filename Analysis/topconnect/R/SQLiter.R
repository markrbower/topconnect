SQLiter <- function( db, query ) {
  #' @export
  #'
  # Arguments:
  # db    : MySQL database connection
  # query : "select ..." - it is assumed that this will always be a "select" query.
  #
  library( RMySQL )
  library( iterators )
  library( itertools )

  resultset <- dbGetQuery( db, query )
  it <- iter( resultset, by="row" )
  
  init <- function() {
    it <<- iter( resultset, by="row" )
  }

  nextEl <- function() {
    n <- nextElem(it)
  }
  
  obj <- list(init=init,nextElem=nextEl)
  returnable <- ihasNext(obj)
  
  class(returnable) <- c('ihasNext', 'abstractiter', 'iter', 'SQLiter')
  
  attr( returnable, "size" ) <- nrow( resultset )
  return( returnable )
}
