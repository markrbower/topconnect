databaseUpdateBuffer <- function( dbname, update_table, updateLimit, static_fields, static_values, identity_field, update_field, dbuser='root', host='localhost', password='' ) {
  #
  #' @export
  #
  # Functions useful for updating a MySQL database table in an efficient manner.
  # Based from 'databaseInsertBuffer'
  #
  # There are three types of values:
  # 1. static fields: "subject=1255, channel='...'"
  # 2. identity fields: "clusterid=117"
  # 3. update fields: "label='signal'"
  #
  # Arguments:
  # conn    : MySQL database connection
  #
  # Test:
  # In MySQL:
  # mysql> create a MySQL table: create table test (a int,b int,c int,d int);
  #
  # In R:
  # First, fill the table:
  " COMMENT
  fields <- c('a','b','c','d')
  dib <- databaseInsertBuffer( 'testDB', 'test', fields, 2 )
  dib$insert( c(a=1,b=2,c=3,d=4) )
  dib$insert( c(b=2,a=1,c=5,d=5) )
  dib$insert( c(a=2,b=2,c=7,d=6) )
  dib$insert( c(a=3,b=3,c=9,d=7) )
  "
  #
  # In MySQL:
  # > select * from test; # The values for 'c' should be 3 and 5.
  #
  # Second, update the table:
  " COMMENT
  dub <- databaseUpdateBuffer( 'testDB', 'test', 2, c('a','b'), c(1,2), 'c', 'd' )
  dub$update( 3, 18 )
  dub$update( 5, 19 )
  "
  #
  # In MySQL:
  # > select * from test; # The values for 'd' should NOW be 18 and 19.
  #
  library(stringr)
  options( scipen = 999 )
  
  updateCount <- 0
  query <- ''
  static_str <- ''
  hasData <- 0
  notFirstFlag <- TRUE

#  UPDATE mytable SET title = CASE
#  WHEN id = 1 THEN 'Great Expectations';
  
  initialize <- function() {
    notFirstFlag <<- FALSE 
    static_str <<- "\nWHERE "
    if ( !is.null(static_fields) ) {
      if ( notFirstFlag ) {
        static_str <- paste0( static_str, " AND " )        
      }
      notFirstFlag <- TRUE
      for ( idx in seq_along(static_fields) ) {
        if ( class(static_values[[idx]]) == "character" ) {
          static_str <<- paste0( static_str, static_fields[[idx]],"='", static_values[[idx]], "'" )
        } else {
          static_str <<- paste0( static_str, static_fields[[idx]],"=", static_values[[idx]] )
        }
      }
    }    
    query <<- paste0( "UPDATE ", update_table, " SET ", update_field, " = \nCASE" )
    hasData <<- 0
  }    

  update <- function( identity_value, update_value ) {
    if ( class(identity_field) == "character" ) {
      str <- paste0( "\n\tWHEN ", identity_field,"='", identity_value, "' THEN '", update_value, "'" )
    } else {
      str <- paste0( "\n\tWHEN ", identity_field,"=", identity_value, " THEN ", update_value )
    }

    # Replace "NA" with "null"
    str <- stringr::str_replace_all( str, "NA", "null" )
    str <- stringr::str_replace_all( str, "NaN", "null" )
    str <- stringr::str_replace_all( str, "Inf", "null" )
    
    query <<- paste0( query, str  )
    updateCount <<- updateCount + 1
    hasData <<- 1
#    print( paste0( "DUB: ", updateCount ) )
    if ( updateCount %% updateLimit == 0 ) {
      flush()
      initialize()
    }
  }
  
  flush <- function() {
    if ( hasData == 1 ) {
      query <<- paste0( query, "\n\tELSE ", update_field, "\nEND;" )
      query <<- paste0( query, static_str, ";" )

      tryCatch({
        print( "making conn1" )
        print( paste0( "dbname=", dbname ) )
        print( paste0( "host=", host ) )
        print( paste0( "dbuser=", dbuser ) )
        print( paste0( "password=", password ) )
        conn1 <- topconnect::db( dbname=dbname, host=host, db_user=dbuser, password=password )
        DBI::dbGetQuery( conn1, query )
      }, error=function(e) {
        tryCatch({
          #print( "Second try" )
          write( query, file="DIB_error1.txt", append=TRUE)
          write( "1", file="DIB_error1.txt", append=TRUE)
          write( dbname, file="DIB_error1.txt", append=TRUE)
          write( host, file="DIB_error1.txt", append=TRUE)
          write( dbuser, file="DIB_error1.txt", append=TRUE)
          write( password, file="DIB_error1.txt", append=TRUE)
          write( e, file="DIB_error1.txt", append=TRUE)
          #        print( query )
          
          conn2 <- topconnect::db( dbname=dbname, host=host, db_user=dbuser, password=password )
          DBI::dbGetQuery( conn2, query )
        }, error=function(e) {
          tryCatch({
            print( "Third try" )
            write( query, file="DIB_error2.txt", append=TRUE)
            print( query )
            
            conn3 <- topconnect::db( dbname=dbname, host=host, db_user=dbuser, password=password )            
            DBI::dbGetQuery( conn3, query )
          }, error=function(e) {
            print( 'Failed to connect to database.')
            print( e )
          }, finally={
            print( "Clearing conn3" )
            DBI::dbDisconnect( conn3 )
          }) # Third
        }, finally={
          print( "Clearing conn2" )
          DBI::dbDisconnect( conn2 )
        }) # Second
      }, finally={
        #print( "Clearing conn1" )
        DBI::dbDisconnect( conn1 )
      }) # First
    }
  }
  
  # This assumes the input is a dataframe.
  run <- function( df ) {
#    cat( "In DUB: ", nrow(df), "\n" )
    if ( !is.null(df) ) {
      if ( length(df)>0 ) {
        if ( nrow(df)>0 ) {
          tryCatch({
            for ( idx in 1:nrow(df) ) {
              #          print( df[idx,] )
              update( df$time[idx], df$clusterid[idx] )
            }
          },
          error=function(cond) {
            print( paste0( idx, " of ", nrow(df) ) )
            print( paste0( "WARN: Update database error"))
            print(cond)
          })
        }
      }
    }
    flush()
    initialize()
    return(nrow(df))
  }

  toString <- function() {
    query
  }
  
  obj <- list(initialize=initialize,update=update,flush=flush,run=run,toString=toString)
  class(obj) <- c('databaseUpdateBuffer')
  initialize()
  return( obj )
}
