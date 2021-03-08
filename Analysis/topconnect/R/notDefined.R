notDefined <- function( val ) {
  # For use in conjunction with parseArgs.R
  # Determines whether "strx$name" has been set with a meaningful value; i.e., is an emtpry string or a string of "-1".
  return( val=="-1" | nchar(val)==0 )
}
