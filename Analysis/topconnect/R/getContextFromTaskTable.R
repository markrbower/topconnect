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
  args <- unlist( list(...) )
  fixedFields <- c('username','nodename','path','taskName','institution','lab','experiment','subject',
                   'iterationType','signalType','centerTime','service','dbName','UUID','hostname','password','')
    
  #  1.	Load arguments into a data structure called context.
  print( 'Getting context' )
  context <- list()
  context$username <- parseArg( args, 'username' )
  if ( nchar(context$username) == 0 ) {
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
  context$hostname <- parseArg( args, 'hostname' )
  if ( nchar( context$service ) == 0 ) { # try 'dbName'
    context$service <- parseArg( args, 'dbName' )
  }
  context$UUID <- parseArg( args, 'UUID' )
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
     query <- paste0('select UUID from tasks where institution=\'',context$institution,'\' and ')
     query <- paste0( query, 'experiment=\'',context$experiment,'\' and ')
     query <- paste0( query, 'subject=\'',context$subject,'\' and ')
     query <- paste0( query, 'lab=\'',context$lab,'\' and ')
     query <- paste0( query, 'service=\'',context$service,'\' and ')
     query <- paste0( query, 'taskName=\'',context$taskName,'\' and ')
     query <- paste0( query, 'signalType=\'',context$signalType,'\' and ')
     query <- paste0( query, 'centertime=\'',context$centerTime,'\';')
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
  if ( !stringi::stri_isempty(context$parameters) ) {
    parmString <- unlist( stringr::str_split( context$parameters, ':::' ) )
    for ( ps in parmString ) {
      parts <- unlist(stringr::str_split( ps, '::' ) )
      str <- paste0( 'context[[\"', parts[1], '\"]] = \'', parts[2], '\'' )
      eval(parse(text=str))
    }
  }
  
  # If context$UUID is empty, create a new one.
  if ( notDefined( context$UUID ) | nchar( context$UUID )==0 ) {
    context$UUID = uuid::UUIDgenerate()    
  }
  
  #  3.	Determine whether this instance exists in the database.
  query <- paste0( 'select count(*) as count from tasks where UUID=\'', context$UUID, '\';' )
  rs <- DBI::dbGetQuery( conn, query )
  if ( rs$count == 0 ) { # This UUID does not exist in the database
    #  ⁃	If not, create the instance in the database
    query <- "insert into tasks (username,nodename,path,taskName,institution,lab,experiment,subject,iterationType,UUID,parameters,signalType,centerTime,service,done) values "
    query <- paste0( query, "(\'", context$username, "\',\'", context$nodename, "\',\'", context$path, "\',\'", context$taskName, "\', \'", context$institution, "\',\'", context$lab, "\',\'", context$experiment, "\'," )
    query <- paste0( query, "\'", context$subject, "\',\'", context$iterationType, "\',\'", context$UUID, "\', " )
    query <- paste0( query, "\'", context$parameters, "\',\'", context$signalType, "\',", context$centerTime, ", " )
    query <- paste0( query, "\'", context$service, "\', 0 );" )
    rs <- DBI::dbGetQuery( conn, query )
  } else { # This UUID does exist in the database 
    #  ⁃	If so, update the database values. Perhaps updating the 'modified' value is the most important aspect of this step.
    query <- paste0( "update tasks set modified=now(), " )
    query <- paste0( query, "username=\'", context$username, "\', nodename=\'", context$nodename, "\', path=\'", context$path, "\', taskName=\'", context$taskName, "\', institution=\'", context$institution, "\', lab=\'", context$lab, "\', experiment=\'", context$experiment, "\', " )
    query <- paste0( query, "subject=\'", context$subject, "\',iterationType=\'",context$iterationType, "\',UUID=\'", context$UUID, "\', " )
    query <- paste0( query, "parameters=\'", context$parameters, "\', signalType=\'", context$signalType, "\',centerTime=", context$centerTime, ", " )
    query <- paste0( query, "service=\'", context$service, "\',done=0 " )
    query <- paste0( query, "where UUID=\'", context$UUID, "\';" )
    print( query )
    rs <- DBI::dbGetQuery( conn, query )
  }
  
  #  4.	Return context
  return( context )
}
