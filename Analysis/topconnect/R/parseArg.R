parseArg <- function( args, name ) {
  #
  # args can be a list or a compArgs
  #
  if ( class(args) == "list" ) {
    idx <- which( names(args) == name )
    if ( length(idx) > 0 ) {
      return( unname( unlist( args[name] ) ) )
    } else {
      idx <- which( args == name) # Check against the args, rather than the name
      if ( length(idx) > 0 ) {
        return("-1")
      } else {
        return("")
      }
    }
  } else if ( class(args) == "argumentComposite" ) {
    if ( args$isValid( name ) ) {
      return( args$get( name ) )
    } else {
      cat( "parseArg: Did not find argument: ", name )
      return( "" )
    }
  }
}
