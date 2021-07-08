caseIter <- function( ac ) {
  #
  #
  #
  # seizure_cases_stuff_here_…  [ Note that cases must be capable of describing individual seizures or entire files ]
  conn <- topconnect::db( db_user=ac$get('user'), project=ac$get('dbname'), host=ac$get('hostname'), password=ac$get('password') )
  query <- paste0("select * from tasks where subject=\'",ac$get('subject'),"\' and taskName='validSeizure' order by centerTime;")
  taskRecordset <- DBI::dbGetQuery( conn, query )
  if ( nrow( taskRecordset) == 0 ) { # analyze the entire data file
    query <- paste0("select * from tasks where subject=\'",variables$subject,"\' order by centerTime;")
    taskRecordset <- DBI::dbGetQuery( conn, query )
  }
  DBI::dbDisconnect( conn )
  #print( nrow( taskRecordset) )
  topconnect::RSiter( taskRecordset )
}

