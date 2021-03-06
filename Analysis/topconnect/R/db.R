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
  library( SingletonsInR )

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

  if ( exists( "password" ) ) {
    conn <- DBI::dbConnect( RMySQL::MySQL(),
                            user=db_user,password=password,
                            host=host,dbname=dbname)
  } else { # First, look for a Singleton containing values
    context <- SingletonInR$new()
    contextNames <- names( context$value )
    if ( 'hostname' %in% contextNames & 'password' %in% contextNames ) {
      hostname <- context$value$hostname
      password <- context$value$password
      print( hostname )
      print( password )
      print( db_user )
      print( dbname )
      conn <- DBI::dbConnect( RMySQL::MySQL(),
                              user=db_user,password=password,
                              host=hostname,dbname=dbname)
      print( "Connected" )
    } else { # try to get info from the secret_vault
      conn <- DBI::dbConnect( RMySQL::MySQL(),
                              user=db_user,password=topsecret::get(name=vault_key),
                              host=host,dbname=dbname)
    }
  }
  
  return( conn )
}
