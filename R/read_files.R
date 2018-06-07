# Used by initialise and some of the tests:
blank_therapy_df <- data.frame(Identifier=character(0), FilePath=character(0), Hospital=character(0), FirstName=character(0), LastName=character(0), DateOfBirth=as.Date(character(0)), Sex=character(0), Remarks=character(0), stringsAsFactors=FALSE)

# Not exported - used by initialise and add data:
read_files <- function(files, merge_by_name, quiet, current_therapy){
	
	stopifnot(all(names(current_therapy)==names(blank_therapy_df)))
	
	# Remove non-existant files:
	ok <- file.exists(files)
	if(any(!ok)){
		if(!quiet){
			cat('Note: removing ', sum(!ok), ' non-existant files/directories:\n', sep='')
			for(i in which(!ok)){
				cat('\t', files[i], '\n', sep='')
			}
		}
		files <- files[ok]
	}

	# Expand directories out to files:
	finf <- file.info(files)[,'isdir']
	dirs <- files[finf]
	files <- c(files[!finf], unlist(lapply(dirs, function(x) file.path(x, list.files(x, pattern='.xlsx$')))))

	# Remove non-supported file types:
	ok <- grepl('.xlsx$', files)
	if(any(!ok)){
		if(!quiet){
			cat('Note: removing ', sum(!ok), ' unsupported file types:\n', sep='')
			for(i in which(!ok)){
				cat('\t', files[i], '\n', sep='')
			}
		}
		files <- files[ok]
	}

	# Remove duplicates (although this won't catch an equivalent absolute and relative file path)
	dupes <- duplicated(files)
	if(any(dupes)){
		if(!quiet)
			cat('Note: removing ', sum(dupes), ' duplicated file paths\n', sep='')
		files <- files[!dupes]
	}

	if(length(files)==0){
		stop('No valid file/directory paths supplied', call.=FALSE)
	}

	if(!quiet)
		cat('Attempting to import ', length(files), ' Excel files...\n', sep='')


	# Required sheet names can be set by the user:
	sheetnames <- getOption('DrainDataSheets')
	stopifnot(is.character(sheetnames) && length(sheetnames)==3)
	dfl <- getOption('DrainFluidLead')
	stopifnot(is.numeric(dfl) && length(dfl)==1 && dfl>=0)
	
	alltherapy=alldata=allevents <- vector('list', length(length(files)))
	fileok <- rep(TRUE, length(files))
	ids <- c(current_therapy$Identifier, '')  # Add blank ID here to force a new one to be generated in the while loop
	if(!quiet)
		pb <- txtProgressBar(style=3)
	for(i in 1:length(files)){
	
		file <- files[i]
		newtherapy=newdata=newevents=id <- NULL
		success <- try({
	
		# Check required sheets:
		sheets <- excel_sheets(file)
		if(!all(sheetnames %in% sheets)){
			stop(paste0('Required sheet name "', sheetnames[which(!sheetnames %in% sheets)[1]], '" not found in file ', file, '\n'), call.=FALSE)
		}
	
		# Get therapy:
		newtherapy <- read_excel(file, sheets[1], col_names=FALSE, col_types='text')
		reqdat <- c('Hospital','First name','Last name','Date of birth','Sex','Remarks')
		if(!all(reqdat %in% newtherapy[[1]])){
			stop(paste0('Required data entry "', reqdat[which(! reqdat %in% newtherapy[[1]])[1]], '" not found in the ', sheets[1], ' sheet of file ', file, '\n'), call.=FALSE)
		}
		vals <- newtherapy[[2]]
		names(vals) <- newtherapy[[1]]
		vals <- vals[reqdat]
		names(vals)[2:4] <- c('FirstName','LastName','DateOfBirth')
		
		newtherapy <- as.data.frame(do.call('cbind', as.list(c('FilePath'=file, vals))), stringsAsFactors=FALSE)
		if(!is.na(suppressWarnings(as.numeric(newtherapy$DateOfBirth)))){
			newtherapy$DateOfBirth <- as.Date('1989-12-31')+as.numeric(newtherapy$DateOfBirth)
		}
		newtherapy$DateOfBirth <- as.Date(newtherapy$DateOfBirth)
		if(is.na(newtherapy$Remarks)){
			newtherapy$Remarks <- ''
		}

		# Generate a patient ID:
		randomid <- TRUE
		if(merge_by_name){
			matching <- current_therapy$FirstName == newtherapy$FirstName & current_therapy$LastName == newtherapy$LastName & current_therapy$DateOfBirth == newtherapy$DateOfBirth
			if(any(matching)){
				matchid <- unique(current_therapy$Identifier[matching])
				if(length(matchid)!=1){
					stop(paste0('Failed to merge new data from ', file, ' with existing records due to conflicting Identifier numbers (', paste(matchid, collapse=', '), ') for the same patient name/DOB'), call.=FALSE)
				}
				id <- matchid
				randomid <- FALSE
			}
		}
		if(randomid){
			id <- ''
			while(id%in%ids)
				id <- paste(sample(LETTERS, 4), collapse='')
			ids <- c(ids, id)
		}
	
		newtherapy <- c('Identifier'=id, newtherapy)
		stopifnot(all(names(newtherapy)==names(blank_therapy_df)))
	
		# Get data:
		newdata <- read_excel(file, sheets[2], col_names=TRUE)
		reqdat <- c('Date','Preset Pressure','Pressure','Air leak','Fluid')
		if(!all(reqdat %in% names(newdata))){
			stop(paste0('Required column "', reqdat[which(! reqdat %in% names(newdata)[1])], '" not found in the ', sheets[2], ' sheet of file ', file, '\n'), call.=FALSE)
		}
		newdata <- newdata[,reqdat]
		names(newdata)[c(1,2,4)] <- c('DateTime','PresetPressure','AirLeak')
		
		# Apply correction as sent by Bo which is just to use the minimum for the current and (a default of) 10 future readings:
		adjfluid <- apply(sapply(0:dfl, function(x) lead(newdata$Fluid, x)), 1, min, na.rm=TRUE)
		raf <- rev(adjfluid)
		cmin <- cummin(raf)
		if(any(cmin < raf)){
			stop(paste0("Decreasing cumulative fluid detected for file ", file, " after correcting with a lead of ", dfl, " observations - try increasing e.g. options(DrainFluidLead=20)"))
		}
		newdata <- cbind(Identifier=id, Date=as.Date(newdata$DateTime), newdata, AdjustedFluid=adjfluid)
	
		# Get events:
		newevents <- read_excel(file, sheets[3], col_names=TRUE)
		reqdat <- c('Date','Event')
		if(!all(reqdat %in% names(newevents))){
			stop(paste0('Required column "', reqdat[which(! reqdat %in% names(newevents)[1])], '" not found in the ', sheets[3], ' sheet of file ', file, '\n'), call.=FALSE)
		}
		newevents <- newevents[,reqdat]
		newevents <- cbind(Identifier=id, newevents)
	
		})
		if(inherits(success, 'try-error')){
			cat('Importing file ', file, ' failed\n', sep='')
			fileok[i] <- FALSE
		}else{
			alltherapy[[i]] <- as.data.frame(newtherapy, stringsAsFactors=FALSE)
			alldata[[i]] <- as.data.frame(newdata, stringsAsFactors=FALSE)
			allevents[[i]] <- as.data.frame(newevents, stringsAsFactors=FALSE)
		}
		
		if(!quiet)
			setTxtProgressBar(pb, i/length(files))
	
	}
	if(!quiet){
		close(pb)
		cat('Successfully read ', sum(fileok), ' files\n', sep='')
	}

	alltherapy <- do.call('rbind', alltherapy[fileok])
	alldata <- do.call('rbind', alldata[fileok])
	allevents <- do.call('rbind', allevents[fileok])

	# Return the new data:
	return(list(Therapy=alltherapy, Data=alldata, Events=allevents))
	
}