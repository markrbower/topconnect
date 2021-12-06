db <- function( ... ) {
  #' db
  #' 
  #' @export
  #' @examples
  #' conn <- db('testProject')
  #
  # Connects to a MySQL database called <dbName>.
  #
  # There are six possible inputs:
  # project: the name of the project (default: working directory name)
  # db_user: the username for database access (default: Sys.info$user)
  # vault_user: the username for vault access (default: Sys.info$user)
  # vault_key : the keyword for the vault secret (default: <project>_password)
  # host: the name of the database computer/server (default: localhost)
  # dbname: the name of the database to access (default: <project>)

  args <- list( ... )
  # Defaults
  if ( is.element("project",names(args) ) ) {
    project <- args[[match("project",names(args))]]
  } else {
    # If the user supplied a single variable, then assume that is the project.
    if ( length(args) == 1 ) {
      project <- args[[1]]
    } else {
      project <- basename( getwd() )
    }
  }
  sysinfo <- Sys.info()
  db_user <- "root"
  vault_user <- sysinfo['user']
  vault_key <- paste0( db_user, "_password" )
  host <- 'localhost'
  dbname <- project
  
  # Replace with any arguments
  for ( arg in names(args) ) {
    switch( arg,
            "db_user"    = {db_user <-    args[[arg]]},
            "vault_user" = {vault_user <- args[[arg]]},
            "vault_key"  = {vault_key <-  args[[arg]]},
            "host"       = {host <-       args[[arg]]},
            "password"   = {password <-   args[[arg]]},
            "dbname"     = {dbname <-     args[[arg]]}
    )
  }
  
  #print( paste0( "db: user: ", db_user ) )
  #print( paste0( "db: host: ", host ) )
  #print( paste0( "db: pass: ", password ) )
  #print( paste0( "db: dbname: ", dbname ) )
  
  if ( exists( "password" ) ) {
    conn <- DBI::dbConnect( RMySQL::MySQL(),
                            user=db_user,password=password,
                            host=host,dbname=dbname)
  } else { # First, look for a Singleton containing values
    cat( "ERROR: topconnect: Please supply a password for the database.\n" )
#    context <- SingletonInR$new()
#    #print( paste0( "db: ", names(context) ) )
#    contextNames <- names( context$value )
#    if ( 'host' %in% contextNames & 'password' %in% contextNames ) {
#      host <- context$value$host
#      password <- context$value$password
#      #print( host )
#      #print( password )
#      #print( db_user )
#      #print( dbname )
#      conn <- DBI::dbConnect( RMySQL::MySQL(),
#                              user=db_user,password=password,
#                              host=host,dbname=dbname)
#    } else { # try to get info from the secret_vault
#      #print( paste0( "db: getting password from valut"))
#      conn <- DBI::dbConnect( RMySQL::MySQL(),
#                              user=db_user,password=topsecret::get(name=vault_key),
#                              host=host,dbname=dbname)
#    }
  }
  
  return( conn )
}
