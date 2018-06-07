.onLoad <- function(lib, pkg){
	
	if(is.null(getOption('DrainDataSheets'))){
		options(DrainDataSheets=c('Therapy','Data','Events'), DrainFluidLead=10)
	}

	if(is.null(getOption('DrainFluidLead'))){
		options(DrainFluidLead=10)
	}
	
	packageStartupMessage("")
	packageStartupMessage("-- Welcome to the DrainData package by Matt Denwood and Bo Holbek --")
	packageStartupMessage("")
}