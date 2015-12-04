readInputFiles = function(inLoadFile,
	inTemperatureFile=NULL,
	inPredTemperatureFile=NULL,
	inOccupancyFile=NULL,
	timeStampeFile=NULL,
	intervalMinutes=15,
	verbose = 1) {

	if (verbose > 2) { print("starting readInputFiles()") }
	if (verbose > 3) { 
		print(inLoadFile)
	}
	
	loadDat = read.table(inLoadFile,as.is=T,sep=",",header=F)
	loadTime = getTime(loadDat[,1])	
	dataLoad = loadDat[,2]

	if (is.null(timeStampFile)) {
		# If a timestamp file isn't provided, make predictions for the same times as the 
		# load data
		   timeStampFile=inLoadFile
	}	
	# Read prediction times 
	predTimeStamp = read.table(timeStampFile,as.is=T,sep=",",header=F)
	predTime = getTime(predTimeStamp[,1])
	predTimeNum = as.numeric(predTime)

	# Aggregate load data to a reasonable interval length. intervalMinutes controls
	# the timescale at which the baseline is being fit (typically will be something
	# like 15 minutes or an hour but it could be a day or a week or ten seconds). 
	# We start by aggregating to a timescale finer than
	# intervalMinutes but not by a lot. 
	# If for example intervalMinutes=15, we aggregate to 5-minute chunks. 
	# If the original data are at, say, 20 seconds, then this way we avoid carrying 
	# around tens or hundreds of times more data than needed.
	aggregateMinutes = intervalMinutes/3
	t0 = min(loadTime,na.rm=T)
	tLoadMinutesSinceStart = difftime(loadTime,t0,units="mins") 
	
	# If the time period between measurements is less than intervalMinutes/3, then
	# we want to accumulate multiple measurements into our time intervals: two or more
	# measurements get the same value of intervalLoadSinceStart. But if the
	# time period between measurements is already longer than intervalMinutes/3, then just
	# use the measurements as they are; each measurement gets its own 
	# value of intervalLoadSinceStart. 
	intervalLoadSinceStart = 1+floor(tLoadMinutesSinceStart/aggregateMinutes)
	dataLoadAggregated = aggregate(dataLoad,by=list(intervalLoadSinceStart),mean,
		na.action=na.omit)[,2]

		
	# the timestamp for the interval is the time at the _end_ of the interval, and for
	# some purposes we might want to take that into account. The previous version of this
	# program (through end 2015) tried to do that, but did it wrong and caused problems
	# when comparing the baseline prediction to data. In the typical case, one wants to 
	# compare the data reported at specific timestamps to the prediction for those 
	# timestamps; the fact that the timestamp is actually reporting the load over a prior
	# period is irrelevant. So here we associate the load measurement with the start of the 
	# time interval by using min rather than max in the aggregate() function below.
	loadTimeNum = as.numeric(loadTime)		
	timeLoadAggregatedNum = aggregate(loadTimeNum,by=list(intervalLoadSinceStart),min,
		na.rm=T)[,2]
	timeLoadAggregated = as.POSIXlt(timeLoadAggregatedNum,origin="1970-01-01")	
	dataTime = timeLoadAggregated  


	# We will only use temperature in the model if we have temperature data and if those
	# data span the range of times being predicted. 
	doTemperatureModel = F
	temperatureVec = NULL
	if (!is.null(inTemperatureFile)) {
		# inTemperatureFile has the temperature data to use for training	
		doTemperatureModel = T 
		if (verbose > 3) {
			print(inTemperatureFile)
		}
		temperatureDat = read.table(inTemperatureFile,as.is=T,sep=",",header=F)
		dataTemp = temperatureDat[,2]
		# discard NAs
		iokTemp = which(!is.na(dataTemp))
		
		temperatureDat = temperatureDat[iokTemp,]
		dataTemp = temperatureDat[,2]
						
		# Interpolate temperature data to desired timestamps. 
		# (In principle, the temperature we want should be the mean temperature in the time 
		# interval BEFORE the load timestamp, just as the load is the mean load in the 
		# time interval BEFORE the load timestamp. In practice, temperatures don't 
		# change much in 15 minutes or an hour, so we'll just interpolate. 
		# 
		## Note added 12/2015: We should revisit this. It's quite reasonable to fit a 
		# baseline on 2- or 3- or even 6-hour intervals, and with the current system
		# the temperatures could be fairly wrong. 
		tempTime = getTime(temperatureDat[,1])		
		tempTimeNum = as.numeric(tempTime)	
	
		if (verbose > 3) { print("interpolating temperatures")}
		temperatureVec = approx(tempTimeNum,dataTemp,timeLoadAggregatedNum,
			rule=1)$y	
	}
	# Now we have time, historic load, and historic temperature, all at the same times
	
	predTempVec = NULL
	# Read temperature data for prediction periods (if provided)
	if (doTemperatureModel) {
		if (is.null(inPredTemperatureFile)) {
			# Training temperatures were provided, but there's no 
			# prediction temperature file...need to get prediction temperatures
			# from the training file. 
			predTempVec = approx(tempTimeNum,dataTemp,predTimeNum,rule=1)$y
		} else  {
			# There's a prediction temperature file, so use it
			temperaturePredDat = read.table(inPredTemperatureFile,as.is=T,sep=",",header=F)
			# discard times when temperature prediction is NA
			iok = which(!is.na(temperaturePredDat[,2]))
			temperaturePredDat=temperaturePredDat[iok,]
			predTempVec = temperaturePredDat[,2]	
			predTempTime = getTime(temperaturePredDat[,1])	
			predTempTimeNum = as.numeric(predTempTime)
		
			predTempVec = approx(predTempTimeNum,predTempVec,predTimeNum,rule=1)$y
		}	
		if (sum(is.na(predTempVec)) > 0 ) {
			if (verbose > 1) {
				doTemperatureModel=F
				stop("Error: prediction temperature data don't span prediction time range.")
			}
		}			
	}
			
	# Read additional predictive variable file if provided		
	if (!is.null(xDatFile)) {
		xDat = read.table(xDatFile, header=T,sep=",",as.is=T)
		t = getTime(xDatFile[,1])
		xDat = occDat[,2]
	}
			
	if (verbose > 3) { print("done reading input files; defining variables")}	
	loadVec = dataLoadAggregated
	tempVec = temperatureVec
	
	# Remove time and temperature data for times with NA temperatures
	if (!is.null(tempVec)) {
		if (sum(is.na(tempVec)) > 0) {
			if (verbose > 1) {print("Removed some NA temperatures")}
			abad = which(is.na(tempVec))
			dataTime = dataTime[-abad]
			loadVec = loadVec[-abad]
			tempVec = tempVec[-abad]
		}
	}

	
	Out = NULL
	Out$dataTime = dataTime
	Out$loadVec = loadVec
	Out$tempVec = tempVec
	Out$predTime = predTime
	Out$predTempVec = predTempVec
	Out$doTemperatureModel = doTemperatureModel
	if (verbose > 2) { print("leaving readInputFiles") }
	return(Out)
}	
	
}	
	
	

readInputFiles = function(inLoadFile,inTemperatureFile=NULL,
	inPredTemperatureFile=NULL, inOccupancyFile=NULL,
	timeStampFile=NULL,
	verbose=1,intervalMinutes=15)	 {
	# This file is from baseline.R, the model that does not use occupancy data.
	# We want to modify it to use occupancy data. Probably better to rewrite it altogether.
	
	if (verbose > 2) { print("starting readInputFiles()") }
	if (verbose > 3) { 
		print(inLoadFile)
	}
	loadDat = read.table(inLoadFile,as.is=T,sep=",",header=F)
	loadTime = getTime(loadDat[,1])	
	dataLoad = loadDat[,2]
	
	if (is.null(timeStampFile)) {
	   timeStampFile=inLoadFile
	}	
	# Read prediction times 
	predTimeStamp = read.table(timeStampFile,as.is=T,sep=",",header=F)
	predTime = getTime(predTimeStamp[,1])
	predTimeNum = as.numeric(predTime)

	# Aggregate load data to a reasonable interval length. intervalMinutes controls
	# the timescale at which the baseline is being fit (typically will be something
	# like 15 minutes or an hour). We start by aggregating to a timescale finer than
	# intervalMinutes but not by a lot. 
	# If for example intervalMinutes=15, we aggregate to 5-minute chunks. 
	# If the original data are at, say, 20 seconds, then this way we avoid carrying 
	# around tens or hundreds of times more data than needed.
	aggregateMinutes = intervalMinutes/3
	t0 = min(loadTime,na.rm=T)
	tLoadMinutesSinceStart = difftime(loadTime,t0,units="mins") 
	
	# If the time period between measurements is less than intervalMinutes/3, then
	# we want to accumulate multiple measurements into our time intervals: two or more
	# measurements get the same value of intervalLoadSinceStart. But if the
	# time period between measurements is already longer than intervalMinutes/3, then just
	# use the measurements as they are; each measurement gets its own intervalLoadSinceStart. 
	intervalLoadSinceStart = 1+floor(tLoadMinutesSinceStart/aggregateMinutes)
	dataLoadAggregated = aggregate(dataLoad,by=list(intervalLoadSinceStart),mean,
		na.action=na.omit)[,2]
	
	# the timestamp for the interval is the time at the _end_ of the interval
	loadTimeNum = as.numeric(loadTime)		
	timeLoadAggregatedNum = aggregate(loadTimeNum,by=list(intervalLoadSinceStart),max,
		na.rm=T)[,2]
	timeLoadAggregated = as.POSIXlt(timeLoadAggregatedNum,origin="1970-01-01")	

	doTemperatureModel = F
	temperatureVec = NULL
	if (!is.null(inTemperatureFile)) {
		# inTemperatureFile has the temperature data to use for training	
		doTemperatureModel = T 
		if (verbose > 3) {
			print(inTemperatureFile)
		}
		temperatureDat = read.table(inTemperatureFile,as.is=T,sep=",",header=F)
		dataTemp = temperatureDat[,2]
		# discard NAs
		iokTemp = which(!is.na(dataTemp))
		
		temperatureDat = temperatureDat[iokTemp,]
		dataTemp = temperatureDat[,2]
						
		# Interpolate temperature data to desired timestamps. 
		# (In principle, the temperature we want should be the mean temperature in the time 
		# interval BEFORE the load timestamp, just as the load is the mean load in the 
		# time interval BEFORE the load timestamp. In practice, temperatures don't 
		# change much in 15 minutes or an hour, so we'll just interpolate. 
		tempTime = getTime(temperatureDat[,1])		
		tempTimeNum = as.numeric(tempTime)	
	
		if (verbose > 3) { print("interpolating temperatures")}
		temperatureVec = approx(tempTimeNum,dataTemp,timeLoadAggregatedNum,
			rule=1)$y	
	}
	# Now we have time, historic load, and historic temperature, all at the same times
	dataTime = timeLoadAggregated  
	
	predTempVec = NULL
	# Read temperature data for prediction periods (if provided)
	if (doTemperatureModel) {
		if (is.null(inPredTemperatureFile)) {
			# Training temperatures were provided, but there's no 
			# prediction temperature file...need to get prediction temperatures
			# from the training file. 
			predTempVec = approx(tempTimeNum,dataTemp,predTimeNum,rule=1)$y
		} else  {
			# There's a prediction temperature file, so use it
			temperaturePredDat = read.table(inPredTemperatureFile,as.is=T,sep=",",header=F)
			# discard times when temperature prediction is NA
			iok = which(!is.na(temperaturePredDat[,2]))
			temperaturePredDat=temperaturePredDat[iok,]
			predTempVec = temperaturePredDat[,2]	
			predTempTime = getTime(temperaturePredDat[,1])	
			predTempTimeNum = as.numeric(predTempTime)
		
			predTempVec = approx(predTempTimeNum,predTempVec,predTimeNum,rule=1)$y
		}	
		if (sum(is.na(predTempVec)) > 0 ) {
			if (verbose > 1) {
				doTemperatureModel=F
				stop("Error: prediction temperature data don't span prediction time range.")
			}
		}			
	}
			
	if (verbose > 3) { print("done reading input files; defining variables")}	
	loadVec = dataLoadAggregated
	tempVec = temperatureVec
	
	# Remove time and temperature data for times with NA temperatures
	if (!is.null(tempVec)) {
		if (sum(is.na(tempVec)) > 0) {
			if (verbose > 1) {print("Removed some NA temperatures")}
			abad = which(is.na(tempVec))
			dataTime = dataTime[-abad]
			loadVec = loadVec[-abad]
			tempVec = tempVec[-abad]
		}
	}

	
	Out = NULL
	Out$dataTime = dataTime
	Out$loadVec = loadVec
	Out$tempVec = tempVec
	Out$predTime = predTime
	Out$predTempVec = predTempVec
	Out$doTemperatureModel = doTemperatureModel
	if (verbose > 2) { print("leaving readInputFiles") }
	return(Out)
}	
