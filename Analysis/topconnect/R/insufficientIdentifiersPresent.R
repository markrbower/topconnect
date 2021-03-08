insufficientIdentifiersPresent <- function( context ) {
  #' insufficientIdentifiersPresent
  #' 
  #' Provides default vaules for project and database use based on the previous values.
  #' 
  #' @export
  #' @examples
  #' insufficientIdentifiersPresent( context )

  # Check that all elements are present
  '%!in%' <- function(x,y)!('%in%'(x,y))
  attach(context)
  result <- FALSE
  nm <- names(context)
  if ( "institution" %!in% nm |
       "experiment" %!in% nm |
       "subject" %!in% nm |
       "lab" %!in% nm |
       "service" %!in% nm |
       "taskName" %!in% nm |
       "signalType" %!in% nm |
       "centerTime" %!in% nm ) {
    result <- TRUE
  } else {
  # Check that the values are valid.
    if ( nchar( context$institution )==0 |
         nchar( context$experiment )==0 |
         nchar( context$subject )==0 |
         nchar( context$lab )==0 |
         nchar( context$service )==0 |
         nchar( context$taskName )==0 |
         nchar( context$signalType )==0 |
         is.null( context$centerTime ) ) {
      result <- TRUE
    }
  }
  detach(context)
  return( result )
}
