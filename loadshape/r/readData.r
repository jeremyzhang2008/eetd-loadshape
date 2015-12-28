readInputFiles = function(inLoadFile,
	inTemperatureFile=NULL,
	inPredTemperatureFile=NULL,
	inOccupancyFile=NULL,
	timeStampFile=NULL,
	intervalMinutes=15,
	verbose = 1) {
	
	if (verbose > 2) { print("starting readInputFiles()") }
	if (verbose > 3) { 
		print(inLoadFile)
	}
	
	loadDat = read.table(loadFile,
		header=F,sep=",",as.is=T)
	tLoad = getTime(loadDat[,1])
	yLoad = loadDat[,2]
	
	if (is.null(timeStampFile)) {
		# Make predictions for the same times for which load data are provided.
		tPred = tLoad
	} else {
		# Use timestamps that are explicitly provided
		tPredDat = read.table(timeStampFile,header=F,sep=",",as.is=T)
		tPred = getTime(tPredDat[,1])
	}
	
	if (!is.null(occFile)) {
		occDat = read.table(occFile, header=F,sep=",",as.is=T)
		tOcc = getTime(occDat[,1])
		yOcc = occDat[,2]
	}
	
	if (is.null(inTemperatureFile)) {
		# Temperature data are provided
		tempDat = read.table(inTemperatureFile)
		tTemp = tempDat[,1]
		yTemp = tempDat[,2]
		
	}		
	if (is.null(inPredTemperatureFile) {
		if (!is.null(inTemperatureFile)) {
			# Prediction temperatures not provided in a separate file
			# so take them from the training temperature data file.
			predTempDat = read.table(inTemperatureFile,header=F,sep=",",as.is=T)
			tPredTemp = predTempDat[,1]
			yPredTemp = predTempDat[,2]		
	} else {
		# No prediction temperature data are available at all
	}
	
	
	dataStruct = createDataStructure(tLoad,yLoad,tPred,tTemp,yTemp,tOther,yOther,
		xPredThresh=xPredThresh,verbose=verbose)
	
	return(dataStruct)

}


