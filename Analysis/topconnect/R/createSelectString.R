createSelectString <- function( fields, compArgs ) {
# For example of how to use, this is called from "NPI_parameters"
#
# progressFields <- c(subject='varchar(32)',channel='varchar(32)',sesssion='varchar(32)',timestamp='bigint',cw='int',CCthreshold='float',EDthreshold='float',blackout='float')
#
# subject=\'", subject,"\' and channel=\'", channel,"\' and session=\'", session, "\' and timestamp=", timestamp,";" 
  
  # 'session' is a special case, here. It is in "metadataInformer$get('case')",
  # but it is not in compArgs.
  
  
  result <- ''
  notFirstFlag <- FALSE
  for ( name in names(fields) ) {
    if (notFirstFlag) {
      result <- paste0( result, " AND " )
    }
    notFirstFlag <- TRUE
    if ( startsWith( fields[name], 'varchar' ) ) {
      result <- paste0( result, name, "=\'", compArgs$get(name), "\'" )
    } else if ( fields[name] == 'float' ) {
      result <- paste0( result, "cast( ", name, " as decimal(16,4)) =", compArgs$get(name) )
    } else {
      result <- paste0( result, name, "=", compArgs$get(name) )
    }
  }
  return( result )
}



