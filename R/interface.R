#' @title Import and storage of Medela chest drain data files
#' @name DrainData
#' @aliases DrainData draindata

#' @description
#' This function is used to import one or more Medela chest drain data files and store the data for later analysis.  The return value is a data storage class containing all methods required to access and summarise the data.

#' @details
#' The data storage class is written using an OOP-style of programming, so that the data can be accessed in different formats at any point using a single R object as the point of reference.  This minimises the possibilities for programming mistakes by the end user, but does result in code that may look a little strange to users who may be more used to the standard procedural style of R programming.  The best way to understand the code is using an example - see vignette('DrainData', package='DrainData') for an overview.

#' @return
#' An object of class DrainData.

#' @examples
#' \dontrun{
#' vignette('DrainData', package='DrainData')
#' }

#' @param files a character vector giving either one or more paths to specific Excel files, or one or more folders from which all .xlsx files will be imported

#' @param merge_by_name logical flag indicating if different data files containing patients with the same (non-blank) name and DOB should be merged to the same patient identifier

#' @param merge_by_name logical flag indicating if information should be printed to the console (FALSE) or suppressed (TRUE)


DrainData <- setRefClass('DrainData',
	fields = list(Therapy='data.frame', Data='data.frame', Events='data.frame', ids='character'), 

	methods = list(

	initialize = function(files, merge_by_name=TRUE, quiet=FALSE){
		"Import one or more Medela chest drain files"
		
		if(!is.character(files)){
			stop('The argument to files must be a character vector naming one or more Excel files and/or folders containing files to be imported')
		}
		if(!is.logical(merge_by_name) || length(merge_by_name)!=1){
			stop('The argument to merge_by_name must be a logical value of length 1')
		}
		if(!is.logical(quiet) || length(quiet)!=1){
			stop('The argument to quiet must be a logical value of length 1')
		}
		
		newdata <- read_files(files, merge_by_name=merge_by_name, quiet=quiet, current_therapy=blank_therapy_df)
		
		.self$Therapy <- newdata$Therapy
		.self$Data <- newdata$Data
		.self$Events <- newdata$Events
		
	},
	
	AddData = function(files, merge_by_name=TRUE, quiet=FALSE){
		"Import one or more Medela chest drain files and add these to the currently stored files"
		
		if(!is.character(files)){
			stop('The argument to files must be a character vector naming one or more Excel files and/or folders containing files to be imported')
		}
		if(!is.logical(merge_by_name) || length(merge_by_name)!=1){
			stop('The argument to merge_by_name must be a logical value of length 1')
		}
		if(!is.logical(quiet) || length(quiet)!=1){
			stop('The argument to quiet must be a logical value of length 1')
		}
		
		newdata <- read_files(files, merge_by_name=merge_by_name, quiet=quiet, current_therapy=.self$Therapy)
		
		# Need to detect and warn if duplicated data is added?
		
		.self$Therapy <- rbind(.self$Therapy, newdata$Therapy)
		.self$Data <- rbind(.self$Data, newdata$Data)
		.self$Events <- rbind(.self$Events, newdata$Events)
		
	},
	
#	RemoveData method - TODO
	
	GetTherapy = function(Identifier='all'){
		"Extract therapy information"
		
		if(Identifier=='all'){
			Identifier <- unique(.self$Therapy$Identifier)
		}
		
		toret <- .self$Therapy %>%
			filter(.data$Identifier %in% Identifier) %>%
		    arrange(.data$Identifier, .data$FilePath)
		
		return(toret)
		
	},

	GetData = function(Identifier='all'){
		"Extract data"
		
		if(Identifier=='all'){
			Identifier <- unique(.self$Therapy$Identifier)
		}
		
		toret <- .self$Data %>%
		filter(.data$Identifier %in% Identifier) %>%
	    arrange(.data$Identifier, .data$DateTime)
		
		return(toret)
		
	},

	GetEvents = function(Identifier='all'){
		"Extract events"
		
		if(Identifier=='all'){
			Identifier <- unique(.self$Therapy$Identifier)
		}
		
		toret <- .self$Events %>%
			filter(.data$Identifier %in% Identifier) %>%
			arrange(.data$Identifier, .data$Date)
		
		return(toret)
		
	}
	
	
))
