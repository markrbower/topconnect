getContextFromTaskTable <- function( conn, ... ) {
  #' getContextFromTaskTable
  #' 
  #' Provides default vaules for project and database use based on the previous values.
  #' 
  #' @export
  #' @examples
  #' getContextFromTaskTable()
  #  
  # create table tasks (username varchar(128),institution varchar(64),lab varchar(32), nodename varchar(128),
  #                    experiment varchar(64),subject varchar(32),path varchar(256),service varchar(128),
  #                    taskName varchar(128),signalType varchar(32),iterationtype varchar(32),
  #                    centerTime bigint, parameters text,
  #                    UUID varchar(36) not null, done boolean,
  #                    created timestamp default current_timestamp,
  #                    modified timestamp default current_timestamp on update current_timestamp,primary key (UUID)); 
  #  
  #   conn <- db( "testProject" )
  #   getContextFromTaskTable(conn, path='/Users/markrbower/Dropbox/Documents/Data',
  #         data='rodent-MSO',institution='Yale',lab='NSME',subject='Halo11',
  #         experiment='testProject', centerTime=0,
  #         signalType='AP',iterationType='directory')  
  #  

  # This function needs a few variables to allow it to create the 'context' variable.
  #
  # What if "..." is an argumentComposite?
  if ( class(args) != 'argumentCompenent' ) {
    args <- list(...)
  } else {
    args <- list(...)
    args <- unlist( args )
  } 
  # Now, "args" is EITHER a list or an argComps!
  # I don't see that these two structures can be used interchangeably,
  # so two options are:
  # 1. a different getContext... for each option,
  # 2. an access function that works for both lists and argComps structures.
  #    Could 'parseArg' be re-written to do this?
  
  

  fixedFields <- c('username','nodename','path','taskName','institution','lab','experiment','subject',
                   'iterationType','signalType','centerTime','service','dbName','UUID','hostname','password','')
  
  if ( (length(args)==1) & ('argumentComposite' %in% class(args[[1]])) ) {
    argComp <- args
    args <- list()
    for ( fieldname in fixedFields ) {
      if ( argComp$isValid(fieldname) ) {
        args <- append( args, argComp$get(fieldname) )  
      }      
    }
  } else {
    args <- unlist(args)
  }
  
  #  1.	Load arguments into a data structure called context.
  print( 'Getting context' )
  context <- list()
  context$username <- parseArg( args, 'username' )
  if ( !( "username" %in% names(context) ) ) {
    context$username <- unname( Sys.info()['user'] )
  }
  context$nodename <- unname( Sys.info()['nodename'] )
  context$path <- parseArg( args, 'path' )
  context$taskName <- parseArg( args, 'taskName' )
  context$institution <- parseArg( args, 'institution' )
  context$lab <- parseArg( args, 'lab' )
  context$experiment <- parseArg( args, 'experiment' )
  context$subject <- parseArg( args, 'subject' )
  context$iterationType <- parseArg( args, 'iterationType' )
  context$signalType <- parseArg( args, 'signalType' )
  context$centerTime <- parseArg( args, 'centerTime' )
  context$service <- parseArg( args, 'service' )
  context$db_user <- parseArg( args, 'db_user' )
  context$hostname <- parseArg( args, 'hostname' )
  if ( !( "service" %in% names(context) ) ) { # try 'dbName'
    context$service <- parseArg( args, 'dbName' )
  }
  if ( ( "UUID" %in% names(args) ) ) { # try 'dbName'
    context$UUID <- parseArg( args, 'UUID' )
  }
  # How to handle parameters?
  # Look at the names of the args and find outliers to fixed fields
  '%!in%' <- function(x,y)!('%in%'(x,y)) # https://stackoverflow.com/questions/5831794/opposite-of-in
  idx <- which( names(args) %!in% fixedFields )
  
  context$parameters <- paste( names(args[idx]),args[idx],sep="::",collapse=":::") #  f <- str_split( context$parameters, '::' )

  #  2.	If prior existence in database cannot be established, notify user the previous instance is being loaded, and fill in missing values.
   if ( topconnect::insufficientIdentifiersPresent( context ) ) { # then re-load the previous instance.
#    if ( !validContext(context) ) { # the label cannot be constructed
    # Determine if there are any tasks in the table
     query <- 'select count(*) as count from tasks;'
     rs <- DBI::dbGetQuery( conn, query )
     if ( rs$count == 1 ) {
       cat( crayon::bgGreen( crayon::white( "NOTICE: THE PREVIOUS RUN PARAMETERS ARE BEING USED.\n" ) ) )
       cat( crayon::bgGreen( crayon::white( "        IF YOU WANT TO UTILIZE A NEW SET OF PARAMETERS, PLEASE PROVIDE A NEW SET OF VALUES.\n") ) )
        query <- paste0( 'select * from tasks inner join (select UUID,max(modified) as most_recent from tasks group by UUID limit 1) ms on tasks.UUID=ms.UUID and modified=most_recent;')
        rs <- DBI::dbGetQuery( conn, query )
        if ( notDefined( context$username  ) ) { context$username <- rs$username }
        if ( notDefined( context$nodename  ) ) { context$nodename <- rs$nodename }
        if ( notDefined( context$path  ) ) { context$path <- rs$path }
        if ( notDefined( context$taskName  ) ) { context$taskName <- rs$taskName }
        if ( notDefined( context$institution  ) ) { context$institution <- rs$institution }
        if ( notDefined( context$lab  ) ) { context$lab <- rs$lab }
        if ( notDefined( context$experiment  ) ) { context$experiment <- rs$experiment }
        if ( notDefined( context$subject  ) ) { context$subject <- rs$subject }
        if ( notDefined( context$iterationType  ) ) { context$iterationType <- rs$iterationType }
        if ( notDefined( context$signalType  ) ) { context$signalType <- rs$signalType }
        if ( notDefined( context$centerTime  ) ) { context$centerTime <- rs$centerTime }
        if ( notDefined( context$service  ) ) { context$service <- rs$service }
        if ( notDefined( context$parameters  ) ) { context$parameters <- rs$parameters }
        if ( notDefined( context$UUID  ) ) { context$UUID <- rs$UUID }

        #  context$label <- labelFromContext( context )
        if ( notDefined( context$UUID  ) ) { context$UUID <- uuid::UUIDgenerate() }
     }
   } else { # find the entry and upload the UUID
     query <- paste0('select UUID from tasks where institution=\'',parseArg(args,'institution'),'\' and ')
     query <- paste0( query, 'experiment=\'',parseArg(args,'experiment'),'\' and ')
     query <- paste0( query, 'subject=\'',parseArg(args,'subject'),'\' and ')
     query <- paste0( query, 'lab=\'',parseArg(args,'lab'),'\' and ')
     query <- paste0( query, 'service=\'',parseArg(args,'service'),'\' and ')
     query <- paste0( query, 'taskName=\'',parseArg(args,'taskName'),'\' and ')
     query <- paste0( query, 'signalType=\'',parseArg(args,'signalType'),'\' and ')
     query <- paste0( query, 'centertime=\'',parseArg(args,'centerTime'),'\';')
     rs <- DBI::dbGetQuery( conn, query )
     if ( nrow(rs) == 1 ) {
       context$UUID = rs$UUID
     }
   }
   # Validate the context and reject any context that does not contain all required fields.
   if ( topconnect::insufficientIdentifiersPresent( context ) ) {
     return
   }
  
   # Does this still matter?!
   # Unpack the parameters and make separate entries for each.
   if ( ( "parameters" %in% names(args) ) ) {
     parmString <- unlist( stringr::str_split( parseArg( args, 'parameters' ), ':::' ) )
     for ( ps in parmString ) {
       parts <- unlist(stringr::str_split( ps, '::' ) )
       str <- paste0( 'context[[\"', parts[1], '\"]] = \'', parts[2], '\'' )
       eval(parse(text=str))
     }
  }
  
  # If context$UUID is empty, create a new one.
  if ( !( "UUID" %in% names(args) ) ) {
    context$UUID = uuid::UUIDgenerate()    
  }
  
  #  3.	Determine whether this instance exists in the database.
  query <- paste0( 'select count(*) as count from tasks where UUID=\'', parseArg( args, '' ), '\';' )
  rs <- DBI::dbGetQuery( conn, query )
  if ( rs$count == 0 ) { # This UUID does not exist in the database
    #  ⁃	If not, create the instance in the database
    query <- "insert into tasks (username,nodename,path,taskName,institution,lab,experiment,subject,iterationType,UUID,parameters,signalType,centerTime,service,done) values "
    query <- paste0( query, "(\'", parseArg( args, 'username'      ), "\',\'", parseArg( args, 'nodename'  ), "\',\'" )
    query <- paste0( query, "\'",  parseArg( args, 'path'          ), "\',\'", parseArg( args, 'taskName'  ), "\',\'" )
    query <- paste0( query, "\'",  parseArg( args, 'institution'   ), "\',\'", parseArg( args, 'lab'       ), "\',\'" )
    query <- paste0( query, "\'",  parseArg( args, 'experiment'    ), "\',\'", parseArg( args, 'subject'   ), "\',\'" )
    query <- paste0( query, "\'",  parseArg( args, 'iterationType' ), "\',\'", parseArg( args, 'UUID'      ), "\',\'" )
    query <- paste0( query, "\'",  parseArg( args, 'parameters'    ), "\',\'", parseArg( args, 'signalType'), "\'," )
    query <- paste0( query,        parseArg( args, 'centerTime'    ), ",\'",   parseArg( args, 'service'   ), "\', 0);" )
    rs <- DBI::dbGetQuery( conn, query )
  } else { # This UUID does exist in the database 
    #  ⁃	If so, update the database values. Perhaps updating the 'modified' value is the most important aspect of this step.
    query <- paste0( "update tasks set modified=now(), " )
    query <- paste0( query, "username=\'",    parseArg( args, 'username'), "\',   nodename=\'",     parseArg( args, 'nodename'), "\'," )
    query <- paste0( query, "path=\'",        parseArg( args, 'path'), "\',       taskName=\'",     parseArg( args, 'taskName'), "\'," )
    query <- paste0( query, "institution=\'", parseArg( args, 'institution'), "\', lab=\'",         parseArg( args, 'lab'), "\'," )
    query <- paste0( query, "experiment=\'",  parseArg( args, 'experiment'), "\', UUID=\'",         parseArg( args, 'UUID'), "\'," )
    query <- paste0( query, "subject=\'",     parseArg( args, 'subject'), "\',    iterationType=\'",parseArg( args, 'iterationType'), "\'," )
    query <- paste0( query, "parameters=\'",  parseArg( args, 'parameters'), "\', signalType=\'",   parseArg( args, 'signalType'), "\'," )
    query <- paste0( query, "centerTime=",    parseArg( args, 'centerTime'), ",   service=\'",      parseArg( args, 'service'), "\', done=0 " )
    query <- paste0( query, "where UUID=\'",  parseArg( args, 'UUID'), "\';" )
    print( query )
    rs <- DBI::dbGetQuery( conn, query )
  }
  
  #  4.	Return context
  return( context )
}
