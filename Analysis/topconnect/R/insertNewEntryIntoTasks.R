insertNewEntryIntoTasks <- function( conn, context ) {
  if ( "argumentComposite" %in% class(context) ) { # convert
    info <- Sys.info()
    file_info <- context$get('info')
    context['institution'] <- file_info$header$institution
    context['lab'] <- context$get('lab')
    context['experiment'] <- context$get('experiment')
    context['subject'] <- context$get('subject')
    context['path'] <- context$get('path')
    context['service'] <- context$get('dbname')
    context['signalType'] <- context$get('signalType')    
    context['iterationType'] <- context$get('iterationType')
    centerTime <- 0
    parameters <- ''  
  }
  # For use in "persistValidSeizuresAsTasks.R" among others.
  query <- paste0( 'insert into tasks (username,institution,lab,nodename,experiment,subject,path,' )
  query <- paste0( query, 'service,taskName,signalType,iterationType,centerTime,parameters,UUID) values (' )
  query <- paste0( query, '\'', info['user'], '\',' )
  query <- paste0( query, '\'', context$institution, '\',' )
  query <- paste0( query, '\'', context$lab, '\',' )
  query <- paste0( query, '\'', info['nodename'], '\',' )
  query <- paste0( query, '\'', context$experiment, '\',' )
  query <- paste0( query, '\'', context$subject, '\',' )
  query <- paste0( query, '\'', context$path, '\',' )
  query <- paste0( query, '\'', context$service, '\',' )
  query <- paste0( query, '\'', 'validSeizure', '\',' )
  query <- paste0( query, '\'', context$signalType, '\',' )
  query <- paste0( query, '\'', context$iterationType, '\',' )
  query <- paste0( query,  centerTime,', ' )
  query <- paste0( query, '\'', parameters, '\',' )
  query <- paste0( query, '\'', uuid::UUIDgenerate(), '\'' )
  query <- paste0( query, ');')
  DBI::dbGetQuery( conn, query )
}