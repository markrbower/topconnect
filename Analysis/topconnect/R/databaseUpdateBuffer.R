databaseUpdateBuffer <- function( conn, update_table, updateLimit, static_fields, static_values, identity_field, update_field ) {
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
  dib <- databaseInsertBuffer( conn, 'test', fields, 2 )
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
  dub <- databaseUpdateBuffer( conn, 'test', 2, c('a','b'), c(1,2), 'c', 'd' )
  dub$update( 3, 18 )
  dub$update( 5, 19 )
  "
  #
  # In MySQL:
  # > select * from test; # The values for 'd' should NOW be 18 and 19.
  #
  updateCount <- 0
  query <- ''

#  UPDATE mytable SET title = CASE
#  WHEN id = 1 THEN 'Great Expectations';
  
  initialize <- function() {
    static_str <<- ""
    if ( !is.null(static_fields) ) {
  #    print( static_fields )
  #    print( static_values )
      static_str <<- paste( paste0( static_fields, "=", static_values, " AND" ), collapse=' ' )
  #    print( static_str )
    }
    query <<- paste0( "UPDATE ", update_table, " SET ", update_field, " = CASE" )
  }    

  update <- function( identity_value, update_value ) {
    if ( nchar(static_str) > 0 ) {
      str <- paste0( " WHEN ", static_str, " ", identity_field,"=", identity_value, " THEN ", update_value )
#    print(str)
    } else {
      if ( class(identity_field) == "character" ) {
        str <- paste0( " WHEN ", identity_field,"='", identity_value, "' THEN '", update_value, "'" )
      } else {
        str <- paste0( " WHEN ", identity_field,"=", identity_value, " THEN ", update_value )
      }
    }
    query <<- paste0( query, str  )
    updateCount <<- updateCount + 1
    if ( updateCount %% updateLimit == 0 ) {
      flush()
      initialize()
    }
  }
  
  flush <- function() {
    query <<- paste0( query, " ELSE ", update_field, " END;" )
    print(query)
    DBI::dbSendQuery( conn, query )
  }
  
  toString <- function() {
    query
  }
  
  obj <- list(initialize=initialize,update=update,flush=flush,toString=toString)
  class(obj) <- c('databaseInsertBuffer')
  initialize()
  return( obj )
}
