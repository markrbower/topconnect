# progressStrings <- list('selectString'=topconnect$createSelectString(progressFields),'updateNames'=topconnect::createInsertNames(progressFields),'insertValues'=topconnect::createInsertValues(progressFields), 'updateString'=topconnect::createUpdateString(progressFields) )

createInsertValuesString <- function( fields, compArgs ) {
  # For example of how to use, this is called from "NPI_parameters"
  #
  # progressFields <- c(subject='varchar(32)',channel='varchar(32)',sesssion='varchar(32)',timestamp='bigint',cw='int',CCthreshold='float',EDthreshold='float',blackout='float')
  #
  # subject=\'", subject,"\' and channel=\'", channel,"\' and session=\'", session, "\' and timestamp=", timestamp,";" 
  result <- ''
  notFirstFlag <- FALSE
  for ( name in names(fields) ) {
    if (notFirstFlag) {
      result <- paste0( result, "," )
    }
    notFirstFlag <- TRUE
    if ( startsWith( fields[name], 'varchar' ) ) {
      result <- paste0( result, "\'", compArgs$get(name), "\'" )
    } else {
      result <- paste0( result, compArgs$get(name) )
    }
  }
  return(result)
}
