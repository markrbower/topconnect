caseIter <- function( ac, nbrCases=0, vectorCases=NULL ) {
  #
  # Inputs:
  #   ac - compArgs object
  #   nbrCases - if 0, then all cases are computed
  #                 > 0, then
  #   veectorCases - if not null, this overrides nbrCases.
  #
  #' @export
  #
  # seizure_cases_stuff_here_…  [ Note that cases must be capable of describing individual seizures or entire files ]
  conn <- topconnect::db( db_user=ac$get('user'), project=ac$get('dbname'), host=ac$get('hostname'), password=ac$get('password') )
  print("conn ", conn )
  query <- paste0("select * from tasks where subject=\'",ac$get('subject'),"\' and taskName='validSeizure' order by centerTime;")
  print( query )
  taskRecordset <- DBI::dbGetQuery( conn, query )
  if ( nrow( taskRecordset) == 0 ) { # analyze the entire data file
    query <- paste0("select * from tasks where subject=\'",ac$get('subject'),"\' order by centerTime;")
    print( query )
    taskRecordset <- DBI::dbGetQuery( conn, query )
    # If the result is empty, then create a new entry to be returned.
    topconnect:::insertNewEntryIntoTasks( conn, ac )
    # Re-run the query to get the new result
    query <- paste0("select * from tasks where subject=\'",ac$get('subject'),"\' order by centerTime;")
    print( query)
    taskRecordset <- DBI::dbGetQuery( conn, query )
    print( "leaving")
  }
  DBI::dbDisconnect( conn )
  print( paste0( "nrow in taskRecordset: ", nrow(taskRecordset)) )
  # Determine whether a given case contains a "parameters" entry and convert these to fields.
  if ( "parameters" %in% names(taskRecordset) ) {
    new_taskRecordset <- data.frame()
    for ( idx in seq(1,nrow(taskRecordset)) ) {
      case <- taskRecordset[idx,]
      if ( "parameters" %in% names(case) ) {
        parmString <- unlist( stringr::str_split( case['parameters'], ':::' ) )
        if ( length(parmString) > 3 ) {
          for ( ps in parmString ) {
            parts <- unlist(stringr::str_split( ps, '::' ) )
            str <- paste0( 'case <- cbind( case, ', parts[1], '=', parts[2], ')' )
            eval(parse(text=str))
          }
        }
      }
      new_taskRecordset <- rbind( new_taskRecordset, case )
    }
    taskRecordset <- new_taskRecordset
  }
  
  #print( nrow( taskRecordset) )
  N <- nrow(taskRecordset)
  
  if ( !is.null( vectorCases) ) {
    topconnect::RSiter( taskRecordset[ vectorCases, ] )
  } else {
    if ( nbrCases == 0 ) {
      topconnect::RSiter( taskRecordset )
    } else if ( nbrCases == 1 ) {
      topconncet::RSiter( taskRecordset[ runif( 1, 1, N ), ] )
    } else {  
      N <- nrow(taskRecordset)
      dN <- N / (nbrCases-1)
      t <- round( c( seq( 1, N, dN ), N) )
      t <- unique( t )
      topconnect::RSiter( taskRecordset[ t, ] )
    }
  }

}


