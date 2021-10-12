caseIter <- function( ac ) {
  #
  #
  #' @export
  #
  # seizure_cases_stuff_here_…  [ Note that cases must be capable of describing individual seizures or entire files ]
  conn <- topconnect::db( db_user=ac$get('user'), project=ac$get('dbname'), host=ac$get('hostname'), password=ac$get('password') )
  query <- paste0("select * from tasks where subject=\'",ac$get('subject'),"\' and taskName='validSeizure' order by centerTime;")
  taskRecordset <- DBI::dbGetQuery( conn, query )
  if ( nrow( taskRecordset) == 0 ) { # analyze the entire data file
    query <- paste0("select * from tasks where subject=\'",ac$get('subject'),"\' order by centerTime;")
    taskRecordset <- DBI::dbGetQuery( conn, query )
    # If the result is empty, then create a new entry to be returned.
    topconnect:::insertNewEntryIntoTasks( conn, ac )
    # Re-run the query to get the new result
    query <- paste0("select * from tasks where subject=\'",ac$get('subject'),"\' order by centerTime;")
    taskRecordset <- DBI::dbGetQuery( conn, query )
  }
  DBI::dbDisconnect( conn )
  #print( nrow( taskRecordset) )
  topconnect::RSiter( taskRecordset )
}

