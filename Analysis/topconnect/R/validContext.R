validContext <- function(context) {
  #' getContextFromTaskTable
  #' 
  #' Provides default vaules for project and database use based on the previous values.
  #' 
  #' @export
  #' @examples
  #' getContextFromTaskTable()
  #  if ( !is.element('a',names(a)) | is.null(a$a) ) {

  # Check that all elements are present
  '%!in%' <- function(x,y)!('%in%'(x,y))
  attach(context)
  result <- TRUE
  nm <- names(context)
  if ( "institution" %!in% nm |
       "lab" %!in% nm |
       "experiment" %!in% nm |
       "data" %!in% nm |
       "subject" %!in% nm |
       "signalType" %!in% nm |
       "centerTime" %!in% nm ) {
    result <- FALSE
  } else {
  # Check that the values are valid.
    if ( nchar( context$institution )==0 |
         nchar( context$lab )==0 |
         nchar( context$experiment )==0 |
         nchar( context$data )==0 |
         nchar( context$subject )==0 |
         nchar( context$signalType )==0 |
         is.null( context$centerTime ) ) {
      result <- FALSE
    }
  }
  detach(context)
  return( result )
}
