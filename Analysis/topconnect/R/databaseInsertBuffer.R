databaseInsertBuffer <- function( dbName, table, fields, limit, updates=NULL ) {
  #' databaseInsertBuffer
  #' 
  #' @export
  # Arguments:
  # conn    : MySQL database connection
  # query : "select ..." - it is assumed that this will always be a "select" query.
  #
  # Test:
  # In MySQL:
  # mysql> create a MySQL table: create table test (a int,b int);
  #
  # In R:
  # > conn <- dbConnect( MySQL(), user='root', password='', dbname='markdb_projectKoala', host='localhost' )
  # > fields <- c('a','b','c','d')
  # > dib <- databaseInsertBuffer( , 'test', fields, 2 )
  # > dib$insert( c(a=1,b=2,c=3,d=4) )
  # > dib$insert( c(b=4,a=3,c=5,d=6) )
  # > dib$insert( c(a=5,b=6,c=7,d=8) )
  #
  # In MySQL:
  # mysql> 'select * from test;' should show the first two inserts, but not the last.
  #
  # In R:
  # > dib$toString()
  # [1] "insert into test (a,b) values (5,6)"
  # ... showing the partial query buffering the next values to be stored.
  options(stringsAsFactors = FALSE);

  insertTable <- table
  insertFields <- fields
  updateFields <- updates
  updateCount <- 0
  updateLimit <- limit
  query <- ''
  hasValues <- 0
  
  initialize <- function() {
    query <<- paste0( "insert into ", insertTable, " (" )
    query <<- paste0( query, paste0( fields, collapse="," ) )
    query <<- paste0( query, ") values " )
    hasValues <<- 0
  }    

  insert <- function( values ) {
    # If 'fields' is a named vector, assign values based on field names.
    # Else, assume that the values are entered in the order of 'fields' at initialization.
#    print( updateCount )
    fieldnames <- names( values )
    if ( is.null( fieldnames ) ) {
      fieldnames <<- fields
    }
    if ( updateCount %% updateLimit > 0 ) {
      query <<- paste0( query, "," )
    }
    
    query <<- paste0( query, "(" )
    # Match the order of the fieldnames
    idxs <- match( fields, fieldnames )
    # Generate string, placing character variables inside quotes
    str <- ""
    firstFlag <- 1
    for ( idx in idxs ) {
      if ( firstFlag ) {
        firstFlag <- 0
      } else {
        str <- paste0(str, "," )
      }
      if ( class(values[[idx]]) == 'character' ) {
        if ( is.na(values[[idx]]) ) {
          str <- paste0( str, "null" )
        } else {
          if ( length(values[[idx]])>1 ) {
            str <- paste0( str, "'" )
            str <- paste0( str, paste0( unlist(values[[idx]]), collapse=',' ) )
            str <- paste0( str, "'" )
          } else {
            str <- paste0( str, "'", values[[idx]], "'" )
          }
        }
      } else if ( class(values[[idx]]) == 'list' ) {  
        str <- paste0( str, "'" )
        str <- paste0( str, paste0( unlist(values[[idx]]), collapse=',' ) )       
        str <- paste0( str, "'" )
      } else {
        str <- paste0( str, values[[idx]] )
      }
    }

    # Replace "NA" with "null"
    str <- stringr::str_replace_all( str, "NA", "null" )
    str <- stringr::str_replace_all( str, "NaN", "null" )
    hasValues <<- 1
    
#    query <<- paste0( query, paste0( paste0( names(values), "=", unlist(values) ), collapse="," ) )
#    print(str)
    query <<- paste0( query, str  )
    query <<- paste0( query, ")" )
    updateCount <<- updateCount + 1
    if ( updateCount %% updateLimit == 0 ) {
#      print( updateCount )
      flush()
      initialize()
    }
  }
  
  flush <- function() {
    if ( !is.null( updateFields ) ) {
      updateString <- paste0(updateFields,collapse=",")
      if ( nchar(updateString) > 0 ) {
        query <<- paste0( query, " ON DUPLICATE KEY UPDATE ", updateString, "=VALUES(", updateString, ");" )
      }
    } else {
      query <<- paste0( query, ";" )
    }
    if ( hasValues==1 ) {
#      print(query)
      tryCatch({
        conn1 <- topconnect::db( dbName )
        DBI::dbGetQuery( conn1, query )
      }, error=function(e) {
        tryCatch({
          print( "Second try" )
          write( query, file="DIB_error1.txt", append=TRUE)
          print( query )
          
          conn2 <- topconnect::db( dbName )
          DBI::dbGetQuery( conn2, query )
        }, error=function(e) {
          tryCatch({
            print( "Third try" )
            write( query, file="DIB_error2.txt", append=TRUE)
            print( query )
            
            conn3 <- topconnect::db( dbName )            
            DBI::dbGetQuery( conn3, query )
          }, error=function(e) {
            print( 'Failed to connect to database.')
            print( e )
          }, finally={
            DBI::dbDisconnect( conn3 )
          }) # Third
        }, finally={
          DBI::dbDisconnect( conn2 )
        }) # Second
      }, finally={
        DBI::dbDisconnect( conn1 )
      }) # First
    }
  }
  
  toString <- function() {
    query
  }
  
  updateNumber <- function() {
    return( updateLimit )
  }
  
  obj <- list(initialize=initialize,insert=insert,flush=flush,toString=toString,updateNumber=updateNumber)
  class(obj) <- c('databaseInsertBuffer')
  initialize()
  return( obj )
}
