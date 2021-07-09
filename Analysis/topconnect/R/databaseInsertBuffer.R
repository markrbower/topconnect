databaseInsertBuffer <- function( dbName, table, fields, limit, updates=NULL, dbuser='root', host='localhost', password='' ) {
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
  # > conn <- topconnect::db( RMySQL::MySQL(), user='root', password='', dbname='MRE_test', host='localhost' )
  # > fields <- c('value')
  # > dib <- databaseInsertBuffer( 'MRE_test', 'test', fields, 2 )
  # > dib$insert( c(value=1) )
  # > dib$insert( c(value=2) )
  # > dib$insert( c(value=3) )
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
  dbname <- dbName
  hostname <- host
  dbuser <- dbuser
  password <- password
  
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
    #print( "DIB: insert" )
    fieldnames <- names( values )
    # Remove any NA values
    fieldnames <- fieldnames[!is.na(fieldnames)]
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
    #print( paste0( "DIB: ", updateCount ) )
    if ( updateCount %% updateLimit == 0 ) {
#      print( updateCount )
      flush()
      initialize()
    }
  }
  
  # "insert" that can accept vectors as well as scalars
  add <- function( values ) {
    
    # What about fieldnames?!?!?
    fieldnames <- names(values)
    fieldnames <- fieldnames[which(!is.na(fieldnames))]
    for ( v in values ) {
      names(v) <- fieldnames[1]
      insert( v )
    }
  }
  
  flush <- function() {
    #print( "DIB: flush")
    if ( !is.null( updateFields ) ) {
      updateString <- paste0(updateFields,collapse=",")
      if ( nchar(updateString) > 0 ) {
        query <<- paste0( query, " ON DUPLICATE KEY UPDATE ", updateString, "=VALUES(", updateString, ");" )
      }
    } else {
      query <<- paste0( query, ";" )
    }
    #print( paste0( "DIB: flush: ", dbname ) )
    if ( hasValues==1 ) {
#      print(query)
      tryCatch({
        conn1 <- topconnect::db( dbname=dbname, host=hostname, db_user=dbuser, password=password )
        DBI::dbGetQuery( conn1, query )
      }, error=function(e) {
        tryCatch({
          print( "Second try" )
          write( query, file="DIB_error1.txt", append=TRUE)
          print( query )
          
          conn2 <- topconnect::db( dbname=dbname, host=hostname, db_user=dbuser, password=password )
          DBI::dbGetQuery( conn2, query )
        }, error=function(e) {
          tryCatch({
            print( "Third try" )
            write( query, file="DIB_error2.txt", append=TRUE)
            print( query )
            
            conn3 <- topconnect::db( dbname=dbname, host=hostname, db_user=dbuser, password=password )            
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
  
  obj <- list(initialize=initialize,insert=insert,add=add,flush=flush,toString=toString,updateNumber=updateNumber)
  class(obj) <- c('databaseInsertBuffer')
  initialize()
  return( obj )
}
