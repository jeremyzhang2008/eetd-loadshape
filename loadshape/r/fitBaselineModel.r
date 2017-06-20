# --------------------------------------------------
# Building Energy Baseline Analysis Package
#
# Copyright (c) 2013, The Regents of the University of California, Department
# of Energy contract-operators of the Lawrence Berkeley National Laboratory.
# All rights reserved.
# 
# The Regents of the University of California, through Lawrence Berkeley National
# Laboratory (subject to receipt of any required approvals from the U.S.
# Department of Energy). All rights reserved.
# 
# If you have questions about your rights to use or distribute this software,
# please contact Berkeley Lab's Technology Transfer Department at TTD@lbl.gov
# referring to "Building Energy Baseline Analysis Package (LBNL Ref 2014-011)".
# 
# NOTICE: This software was produced by The Regents of the University of
# California under Contract No. DE-AC02-05CH11231 with the Department of Energy.
# For 5 years from November 1, 2012, the Government is granted for itself and
# others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide
# license in this data to reproduce, prepare derivative works, and perform
# publicly and display publicly, by or on behalf of the Government. There is
# provision for the possible extension of the term of this license. Subsequent to
# that period or any extension granted, the Government is granted for itself and
# others acting on its behalf a nonexclusive, paid-up, irrevocable worldwide
# license in this data to reproduce, prepare derivative works, distribute copies
# to the public, perform publicly and display publicly, and to permit others to
# do so. The specific term of the license can be identified by inquiry made to
# Lawrence Berkeley National Laboratory or DOE. Neither the United States nor the
# United States Department of Energy, nor any of their employees, makes any
# warranty, express or implied, or assumes any legal liability or responsibility
# for the accuracy, completeness, or usefulness of any data, apparatus, product,
# or process disclosed, or represents that its use would not infringe privately
# owned rights.
# --------------------------------------------------

readInputFiles = function(loadFile, timeStampFile=NULL, inTemperatureFile=NULL, 
                          inPredTemperatureFile=NULL, xFile=NULL, predXfile=NULL, outGoodnessOfFitFile=NULL,
                          fahrenheit=T, verbose=0) {
  if (verbose > 2) { print("  Starting readInputFiles()")}
  tTemp = NULL
  TempC = NULL
  TempF = NULL
  tTempPred = NULL
  predTempC = NULL
  predTempF = NULL
  tX = NULL
  xMat = NULL
  tPredX = NULL
  xPredMat = NULL
  
  loadDat = read.table(loadFile, header=F, sep=",", as.is=T)
  tLoad = getTime(loadDat[,1])
  yLoad = loadDat[,2]
  
  if (is.null(timeStampFile)) { 
    tPred = tLoad
  } else {
    tPredDat = read.table(timeStampFile, header=F, sep=",", as.is=T)
    tPred = getTime(tPredDat[,1])
  }
  
  if (!is.null(inTemperatureFile)) {
    tempDat = read.table(inTemperatureFile, header=F, sep=",", as.is=T)
    tTemp = getTime(tempDat[,1])
    yTemp = tempDat[,2]
    if (fahrenheit) {
      TempF = yTemp
      TempC = (yTemp-32)*5/9
    } else {
      TempC = yTemp
      TempF = yTemp*9/5 + 32
    }
  }
  
  # If only the input temperature file is provided, use it for both fit and prediction periods
  if (is.null(inPredTemperatureFile) && !is.null(inTemperatureFile)) {
    inPredTemperatureFile = inTemperatureFile
  }
  
  if (!is.null(inPredTemperatureFile)) {
    tempPredDat = read.table(inPredTemperatureFile, header=F, sep=",", as.is=T)
    tTempPred = getTime(tempPredDat[,1])
    yPredTemp = tempPredDat[,2]
    if (fahrenheit) {
      predTempF = yPredTemp
      predTempC = (yPredTemp-32)*5/9
    } else {
      predTempC = yPredTemp
      predTempF = yPredTemp*9/5 + 32
    }
  }
  
  if (!is.null(xFile)) {
    xDat = read.table(xFile, header=F, sep=",", as.is=T)
    tX = getTime(xDat[,1])
    xMat = xDat[,-1]
  }  
  
  # If only one file of additional predictive variables is provided, use it for both fit and prediction periods
  if (is.null(predXfile) && !is.null(xFile)) { 
    predXfile = xFile
  }
  
  if (!is.null(predXfile)) {
    xPredDat = read.table(predXfile, header=F, sep=",", as.is=T)
    tPredX = getTime(xPredDat[,1])
    xPredMat = xPredDat[,-1]
  }
  
  
  Out = NULL
  Out$fit = NULL
  Out$fit$tLoad = tLoad
  Out$fit$yLoad = yLoad
  Out$fit$tTemp = tTemp
  Out$fit$TempF= TempF
  Out$fit$TempC = TempC
  Out$fit$tX = tX
  Out$fit$xMat = xMat
  
  Out$pred = NULL
  Out$pred$tPred = tPred
  Out$pred$tTemp = tTempPred
  Out$pred$TempF = predTempF
  Out$pred$TempC = predTempC
  Out$pred$tX = tPredX
  Out$pred$xMat = xPredMat
  
  if (verbose > 2) { print("Leaving readInputFiles()")}
  return(Out)
}

trimData = function(timeVec, dataVec, startTime = NULL, endTime = NULL, verbose=0) {
  if (verbose > 0) { print("starting trimData")}
  if (is.null(dataVec)) { return(NULL)}
  if (is.null(startTime)) { startTime = timeVec[1] }
  if (is.null(endTime)) { endTime = tail(timeVec,1) }
  
  okTime = (startTime <= timeVec & timeVec <= endTime)
  Out = NULL
  Out$timeVec = timeVec[okTime]
  Out$dataVec = dataVec[okTime]
  
  if (verbose > 0) { print("leaving trimData") }
  return(Out)
}


getTime = function(timeInfo,verbose=1,format=NULL, tz="") {
  # given a vector of timestamps using one of the following timestamp formats, return
  # a vector of POSIXlt times:
  # 1. Year-month-day Hours:Minutes
  # 1. Year-month-day Hours:Minutes:Seconds
  # 2. Seconds since 1970-01-01 00:00:00
  # 3. Milliseconds since 1970-01-01 00:00:00 (as generated by sMAP for example)
  # 
  if (verbose > 3) { print("  Starting getTime()")}
  
  if (any(class(timeInfo) %in% c("POSIXct","POSIXlt"))) { return(timeInfo) }
  
  if (verbose > 4) { print(timeInfo[1]) }
  if (!is.null(format)) {
    time = strptime(timeInfo,format=format,tz=tz)
    return(time)
  }	
  
  ### Much of the following should be replaced by calls to functions in the lubridate library
  if(grepl(":",timeInfo[1])[1]) {
    nColons = sapply(regmatches(timeInfo[1], gregexpr(":", timeInfo[1])), length)
    if(grepl("/", timeInfo[1])[1]) {
      sepChar = "/"
    } else {
      sepChar = "-"
    }  
    if (nColons == 1) {
      if (sepChar =="/") {
        format = "%Y/%m/%d %H:%M"
      } else {
        format = "%Y-%m-%d %H:%M"
      }
    } else {
      if (sepChar == "/") {
        format = "%Y/%m/%d %H:%M:%S"
      } else {  
        format = "%Y-%m-%d %H:%M:%S"
      }
    }
    time = strptime(timeInfo,format=format,tz=tz)
    
  } else {
    if (!is.numeric(timeInfo[1])) {
      stop("Time is not in a recognized format")
    }
    if (timeInfo[1] > 3e9) {
      # If time is in seconds, then this would represent sometime after 2066 
      # so assume time is in milliseconds	
      timeNum = timeInfo/1000
    } else {
      timeNum = timeInfo
    }		
    time = as.POSIXlt(timeNum,origin="1970-01-01",tz=tz)
  }
  
  if (verbose > 3) { print("leaving getTime()")}
  return(time)	
}

calcTOW = function(timeVec, intervalMinutes) {
  minuteOfWeek = 24 * 60 * timeVec$wday + 60 * timeVec$hour + timeVec$min
  minuteOfDay = 60 * timeVec$hour + timeVec$min
  intervalOfWeek = 1 + floor(minuteOfWeek / intervalMinutes)
  intervalOfDay = 1 + floor(minuteOfDay / intervalMinutes)
  Out = NULL
  Out$minuteOfWeek = minuteOfWeek
  Out$minuteOfDay = minuteOfDay
  Out$intervalOfWeek = intervalOfWeek
  Out$intervalOfDay = intervalOfDay
  return(Out)
}

#
imputeTimeSeries = function(timeVec, yVec, xTime=NULL, xMat=NULL, outTimes=NULL, 
                            timeFactor="TOW", intervalMinutes = NULL, verbose=0) {
  if (verbose > 3) { print("   Starting imputeTimeSeries()")}
  if (!(timeFactor %in% c("TOW","TOD"))) { stop("Unrecognized time pattern in imputeTimeSeries.") }
  if (is.null(outTimes)) { outTimes = timeVec }
  if (is.null(intervalMinutes)) {intervalMinutes = median(diff(as.numeric(timeVec)))/60 }
  
  # If we have data for the output times, fill them in
  nOut = length(outTimes)
  yOut = rep(NA, nOut)
  
  okT = timeVec %in% outTimes
  if ( any(okT) ) {
    tmatch = match(timeVec[okT], outTimes)
    yOut[tmatch] = yVec[okT]
  }
  
  if (!any(is.na(yOut))) { 
    # No imputation needed: we are done.
    return(yOut)
  } else {
    fitIntervals = calcTOW(timeVec)
    predIntervals = calcTOW(outTimes)
    
    weekNum = floor(1 + as.numeric(difftime(timeVec,timeVec[1],units="weeks")))
    predWeekNum = floor(1 + as.numeric(difftime(outTimes,timeVec[1],units="weeks")))
    
    uWeekNums = unique(c(weekNum, predWeekNum))
    
    maxInterpLen = 4 * (60/intervalMinutes)  # Number of intervals in 4 hours
    # Interpolate short runs of missing data.
    isNA = is.na(yOut)
    rl = rle(isNA)
    runsNA = rep(rl$lengths, rl$lengths)
    
    doInterp = which( rl$values==T & rl$lengths <= maxInterpLen )
    
    if (length(doInterp) > 0) {
      yInterp = approx(as.numeric(outTimes[!isNA]),yOut[!isNA],as.numeric(outTimes))[[2]]
      yOut[isNA] = yInterp[isNA]
    }
  }
  
  if (any(is.na(yOut))) {
    # For longer stretches of NA,
    # fit a model that uses just (TOW or TOD effect) and week effect.
    # Use the data augmentation trick to 
    # give each week effect a very weak prior of 0.
    # This should give us at least some non-NA estimate for every time.
    yAugmented= c(yVec,rep(0,length(uWeekNums)))
    augmentX = rep(10000,length(uWeekNums))
    if (timeFactor == "TOW") {
      fitDf = data.frame(factor(c(fitIntervals$intervalOfWeek, augmentX)), factor(c(weekNum, uWeekNums)))
      predDf = data.frame(factor(c(predIntervals$intervalOfWeek,augmentX)), factor(c(predWeekNum,uWeekNums)))
      names(fitDf) = c("intervalOfWeek", "weekNum")
      names(predDf) = c("intervalOfWeek", "weekNum")
    } else {
      fitDf = data.frame(factor(c(fitIntervals$intervalOfDay, augmentX)), factor(c(weekNum, uWeekNums)))
      predDf = data.frame(factor(c(predIntervals$intervalOfDay, augmentX)), factor(c(predWeekNum, uWeekNums)))
      names(fitDf) = c("intervalOfDay","weekNum")
      names(predDf) = c("intervalOfDay","weekNum")
    }
    
    amod = lm(yAugmented ~ . + 0, data = fitDf, na.action = na.exclude)
    ypred = predict(amod,predDf)[1:nOut]  # We just want predicted data; After the first nOut predictions are the week effects.
    
    okNA = is.na(yOut)
    yOut[okNA] = ypred[okNA]
  }  
  
  if (!is.null(xMat)) {
    # If we have an explanatory matrix, use it.
    okX = xTime %in% timeVec  
    if (any(okX)) {
      XX = matrix(NA, nrow=length(timeVec), ncol=ncol(xMat))
      tXmatch = match(xTime[okX], timeVec)
      XX[tXmatch,] = xMat[okX,]
      
      XXaugment = matrix(0, nrow=length(uWeekNums), ncol=ncol(xMat))
      XX = rbind(XX,XXaugment)
      fitDf = cbind(fitDf, XX)
      predDf = cbind(predDf, XX)
      
      bmod = lm(yAugmented ~ . + 0, data = fitDf, na.action = na.exclude)
      ypred = predict(bmod,predDf)[1:nOut]  # We just want predicted data; After the first nOut predictions  are the week effects.
      
      okNA = is.na(yOut)
      yOut[okNA] = ypred[okNA]
    }
    
  }
  
  return(yOut)
}


putTimeSeriesOnNiceIntervals = function(timeVec,yVec,outStart=NULL,outEnd=NULL, intervalMinutes=NULL,  
                                        verbose=0) {
  # Takes an input time series recorded at arbitrary time intervals, outputs 
  # an interpolated time series of mean values at regular intervals.
  # Output starts at the top of the hour of the first data point (or at an optionally specified
  # starting location), at specified intervals. 
  #
  # We need to handle the case when we have data at either higher or lower frequency
  # than desired for the output. First deal with the higher-frequency case (take the
  # mean of all of the data in the interval), then handle the lower-frequency case.
  #
  if (verbose > 3) {print("   Starting putTimeSeriesOnNiceIntervals()")}
  timeIn = getTime(timeVec)
  numTimeIn = as.numeric(timeIn)
  
  if (is.null(outStart)) { 
    # if no time is specified for the start of the output series, start at the top of
    # the hour for the first hour that has data.
    outStartDay = strptime(timeIn[1],format="%Y-%m-%d") 
    outStart = outStartDay + timeIn[1]$hour*3600 #We have to do this because there are midnight dates like "2015-01-01 PST" with no hour given
  }
  numTimeStart = as.numeric(getTime(outStart))
  
  if (is.null(outEnd)) {
    numTimeEnd = as.numeric(tail(timeIn,1))
  } else {
    numTimeEnd = as.numeric(getTime(outEnd))
  }
  
  if (is.null(intervalMinutes)) { intervalMinutes = median(diff(as.numeric(timeIn)))/60 }
  
  outTimeNum = seq(from=numTimeStart,to=numTimeEnd,by=intervalMinutes*60)
  
  intervalsSinceStart = (numTimeIn-numTimeStart)/(intervalMinutes*60)
  nIntervalsSinceStart = ceiling(intervalsSinceStart)
  maxIntervals = ceiling((numTimeEnd-numTimeStart)/(intervalMinutes*60))
  
  aggregatedDat = aggregate(yVec,by=list(nIntervalsSinceStart),
                            mean,na.rm=T)
  
  aggInterval = aggregatedDat[[1]]
  okNotTooEarly = aggInterval >= 0
  okNotTooLate = aggInterval <= maxIntervals
  
  outTime = getTime(outTimeNum)
  nPoints = length(outTime)
  
  yOut = rep(NA,nPoints)
  ok = okNotTooEarly & okNotTooLate
  yOut[aggInterval[ok]+1] = aggregatedDat[[2]][ok]	
  yOut = yOut[1:nPoints]
  
  
  Out = NULL
  Out$time = outTime
  Out$y = yOut
  Out$intervalMinutes = intervalMinutes
  
  if(verbose > 3) { print( "   Leaving putTimeSeriesOnNiceIntervals()" ) }
  return(Out)
}


prepareTimeSeries = function(inputDat, tStart=NULL, tEnd=NULL, intervalMinutes=NULL, verbose=0) {
  if (verbose > 3) { print("  Starting prepareTimeSeries()")}
  tTemp = NULL
  TempF = NULL
  TempC = NULL
  tXorig = NULL
  xMatOrig = NULL
  xMat = NULL
  tLoad = NULL
  yLoad = NULL
  
  tLoadOrig = inputDat$tLoad
  yLoadOrig = inputDat$yLoad
  tPred = inputDat$tPred  # Times at which we ultimately want predictions
  tTemp = inputDat$tTemp
  TempF = inputDat$TempF
  TempC = inputDat$TempC
  tXorig = inputDat$tX
  xMatOrig = inputDat$xMat

  ####
  # If tPred isn't provided but load data are provided, use the start and end times from the load data.
  # 
  if (!is.null(tPred)) { 
    tStart = tPred[1]
    tEnd = tail(tPred,1) 
  } else {
    tPred = tLoadOrig # Output times weren't specified, so use the load times
    tStart = tLoadOrig[1]
    tEnd = tail(tLoadOrig,1)
  }
  if (is.null(intervalMinutes)) { 
    if (!is.null(tLoadOrig)) {
      intervalMinutes = median(diff(as.numeric(tLoadOrig)))/60 
    } else {
      stop("No intervalMinutes was specified in prepareTimeSeries(), and one can't be determined.")
    }
  }  
  
  ####
  # If load data are provided, put them on regular intervals. (This may be unnecessary, if they already are!)
  # Unlike the predictive variables that are handled below, if there are missing load data we don't impute values. 
  # We do interpolate between non-NA values, though; so if we have data every hour, and someone wants the load every
  # 15 minutes, we interpolate to generate that. But if we are missing an hour's data, it gets an NA.
  if (!is.null(yLoadOrig) ) {
    LoadInfo = putTimeSeriesOnNiceIntervals(
      tLoadOrig, yLoadOrig, outStart = tStart, outEnd = tEnd,
      intervalMinutes = intervalMinutes, verbose = verbose
    )
    timePoints = LoadInfo$time
    LoadInterp = approx(as.numeric(LoadInfo$time), LoadInfo$y, as.numeric(timePoints))
    tLoad = timePoints
    yLoad = LoadInterp$y
  } else {
    timePoints = tPred
  }
  
  ####
  timeIntervals = calcTOW(timePoints)
  intervalOfWeek = timeIntervals$intervalOfWeek
   
  if (is.null(tStart)) { tStart = timePoints[1] }
  if (is.null(tEnd)) { tEnd = tail(timePoints,1) }
  
  if (!is.null(TempF)) {
    if (verbose > 2) { print(paste("  Putting temperature data on nice intervals")) }
    
    TempInfo = putTimeSeriesOnNiceIntervals(
      tTemp, TempF, outStart = NULL, outEnd = NULL,
      intervalMinutes = intervalMinutes, verbose = verbose
    )
    
    TempF = rep(NA, length(timePoints))
    oktT = TempInfo$time %in% timePoints
    if (any(oktT)) {
      matchtT = match(TempInfo$time[oktT], timePoints)
      TempF[matchtT] = TempInfo$y[oktT]
    }
    
    if ( any(is.na(TempF) ) ) {
      okNA = is.na(TempF)
      TempImp = imputeTimeSeries(TempInfo$time, TempInfo$y, xTime=NULL, xMat=NULL, outTimes=timePoints, 
                                  timeFactor="TOD", intervalMinutes = intervalMinutes, verbose = verbose) 
 
      
      TempF[okNA] = TempImp[okNA]
      TempC = (TempF - 32) * 5 / 9
    }
  }
  
  if (!is.null(xMatOrig)) {
    if (verbose > 3) {
      print(paste("Putting additional variables on nice intervals"))
    }
    xMat = matrix(NA, nrow=length(timePoints), ncol=ncol(xMatOrig))
    for (icol in 1:ncol(xMatOrig)) { 
      xInfo = putTimeSeriesOnNiceIntervals(
        tXorig, matrix(xMatOrig[,1]), outStart = NULL,
        outEnd = NULL,intervalMinutes = intervalMinutes,
        verbose = verbose
      )
      if (icol ==1 ) {
        xNice = matrix(NA, nrow=length(xInfo$y), ncol= ncol(xMatOrig))
      }
      xNice[,icol] = xInfo$y
      txNice = xInfo$time
    }
    
    oktX = txNice %in% timePoints
    if (any(oktX)) {
      matchtX = match(txNice[oktX], timePoints)
      xMat[matchtX,] = xNice[oktX,]
    }
    if ( any(is.na(xMat)) ){
      if ( any(is.na(xMat[,1]))) {
        # Impute missing values in the first column using a TOW model
        xCol = imputeTimeSeries(txNice, xNice[,1], outTimes=timePoints, 
                               timeFactor="TOW", intervalMinutes = intervalMinutes, verbose = verbose) 
        okNA = which(is.na(xMat[,1]))
        xMat[okNA,1] = xCol[okNA]
      }
      if (ncol(xMat) > 1) {
        # Impute missing values in subsequent columns by fitting a model using all previous columns, plus TOW.
        for (icol in 2:ncol(xMat)) {
          if (any(is.na(xMat[,icol]))) {
            xCol = imputeTimeSeries(txNice,xNice[,icol], timeFactor="TOW", xTime = txNice, 
                                    as.matrix(xNice[,1:(icol-1)]),
                                    intervalMinutes = intervalMinutes, verbose = verbose) 
            okNA = which(is.na(xMat[,icol]))
            xMat[okNA,icol] = xCol[okNA]
          }
        }
      }
    }  
  }
  Out = NULL
  Out$time = timePoints
  Out$tPred = tPred
  Out$load = yLoad
  Out$TempF = TempF
  Out$TempC = TempC
  Out$xMat = xMat
  Out$intervalOfWeek = intervalOfWeek
  Out$intervalMinutes = intervalMinutes
  
  if (verbose > 3) { print("Leaving prepareTimeSeries()")}
  return(Out)
}


#
defineTemperatureKnots = function(TempF, Tknots = c(40, 55, 65, 75, 90), verbose=0) {
  if (verbose > 4) { print("   Starting defineTemperatureKnots()")}
  Tknots = c(-1000,Tknots,1000)
  nKnots = length(Tknots)
  okKnots = rep(T,nKnots)
  
  numInBin = 0
  for (iKnot in 2:nKnots) {
    # how many temperatures are between Tknots[iKnot-1] and Tknots[iKnot]?
    numInBin = numInBin + sum((Tknots[iKnot-1] < TempF) & (TempF < Tknots[iKnot]))
    if (numInBin < 10) { 
      okKnots[iKnot] = F
      if (verbose > 4) {
        print(paste("Bin",iKnot,"doesn't have enough points. Temps",
                      Tknots[iKnot-1],Tknots[iKnot]))
      }	 
    } else {
      numInBin=0
    }
  }
  Tknots = Tknots[okKnots]
  if (verbose > 4) { print("Leaving defineTemperatureKnots()")}
  return(Tknots)
}

#
makeTempMatrix = function(TempF, Tknots, verbose=0) {
	# Input: vector of temperatures (degrees F)
	# Output: A matrix that breaks each temperature into bins, suitable for feeding
	# into a linear regression so as to get a piecewise-continuous model. For example,
	# with Tknots = c(40, 55, 65, 75, 90), a temperature of 58 will yield the following row of the 
	# matrix:
	# 40 15 10 3 0
	# this means "40 degrees up to 40F, plus 15 degrees to get to 55F, plus 10 degrees
	# to get to 65 F, plus 3 degrees to get to 68F, plus 0 degrees in the bin above 80F"
	if (verbose > 2) { print("  Starting makeTempMatrix()") }
	
	Tbinwidth = diff(Tknots) 
	nKnots = length(Tknots)

	nCol = nKnots-1
	outMat = matrix(0,nrow=length(TempF),ncol=nCol)

	outMat[,1] = ifelse(TempF < Tknots[2], TempF, Tknots[2])
	if (nCol > 1) {
		for (iKnot in 2:nCol) {
			outMat[,iKnot] =  ifelse(TempF > Tknots[iKnot], TempF-Tknots[iKnot], 0)
			outMat[,iKnot] = ifelse(outMat[,iKnot] > Tbinwidth[iKnot], 
				Tbinwidth[iKnot], outMat[,iKnot])
		}	
	}	
	Out = NULL
	Out$Tknots = Tknots
	Out$Tmat = outMat
	
	if (verbose > 2) { print("  Leaving makeTempMatrix()") }

	return(Out)	
}

fillGaps = function(xvec,minGap=3.1, verbose=0) {
	# input xvec is a vector of T and F, representing whether time period is an
	# occupied mode or not, e.g. c(F,F,F,T,F,F,T,T,T,F,T,T,F,F,F,F,F)
	# If there are a few F sprinkled in among T, replace them with T: 
	if (verbose > 4) { print("    Starting fillGaps()")}
	rl = rle(xvec)  # run lengths
	y = rep(rl$lengths, rl$lengths)
	y[xvec==T] = F
	z = rep(F,length(xvec))
	z[y <= minGap] = T
	if (verbose > 4) { print("    Leaving fillGaps()")}
	return(z)
}	

findOccUnocc = function(intervalOfWeek, loadVec, TempF, intervalMinutes,verbose=1) {
	if (verbose > 4) { print("    Starting findOccUnocc()") }
	# Figure out which times of week a building is in one of two modes
	#  (called 'occupied' or 'unoccupied'). This is NOT based on whether 
	# occupants are present: rather, in "occupied mode" the building is load is more
	# sensitive to outdoor air temperature than in "unoccupied mode."

	uTOW = sort(unique(intervalOfWeek))
	nTOW = length(uTOW)
	
	# Define 'occupied' and 'unoccupied' based on a regression
	# of load on outdoor temperature: times of week that the regression usually
	# underpredicts the load will be called 'occupied', the rest are 'unoccupied'
	# This is not foolproof but usually works well. 
	
	TempF50 = TempF-50
	TempF50[TempF > 50] = 0
	TempF62 = TempF-62
	TempF62[TempF < 62] = 0
	
	TempFbetween = TempF-50
	TempFbetween[TempF < 50] = 0
	TempFbetween[TempF > 62] = 0
	
	if (verbose > 4) {
		print("    Fitting temperature regression")
	}
	amod = lm(loadVec ~ TempF50 + TempFbetween + TempF62,na.action=na.exclude)
	
	okocc = rep(0,nTOW)
	for (itow in 1:nTOW) {
		okTOW = intervalOfWeek==uTOW[itow]
		# if the regression underpredicts the load more than 65% of the time
		# then assume it's an occupied period
		if ( sum(residuals(amod)[okTOW]>0,na.rm=T) > 0.65*sum(okTOW) ) {
			okocc[itow]=T
		}
	}
	firstMat = cbind(uTOW,okocc)
	
	# if there are short intervals that are tagged as "unoccupied" but they're bounded on both
	# sides by "occupied" periods, re-label them as "occupied."
	outMat = firstMat
	minGap = 3*(60/intervalMinutes) + 0.1  # Fill gaps of 3.1 hours or less.
	outMat[,2] = fillGaps(outMat[,2],minGap = minGap )
	
	if (verbose > 4) { print("    Leaving findOccUnocc()") }
	return(outMat)
}



defineModelVariables = function(inputDat, xPredThresh = 0.2, okInclude = NULL, includeStartup = T, verbose = 0) {
  # Create a data structure containing time, load, temperature, and predictive variables
  #
  # * Impute temperature and other predictive variables if necessary.
  # * Create temperature matrices to fit a piecewise-linear dependence on temperature
  # 	- fit separate temperature dependence for occupied and unoccupied modes, and
  #		startup period
  # *	Optionally, create separate matrices of other predictive variables to allow
  #		different behavior when the variable is above vs below a specified
  #		percentile (specified by xPredThresh). For instance, if a predictor
  #		variable is the number of active WiFi connections, you could allow a
  #		different relationship between load and number of connections when the
  #		number is below the 20th percentile than when it is above the 20th percentile.
  #
  if (verbose > 3) { print("   Starting defineModelVariables()") }
  tempMatrices = NULL
  Xfit = NULL
  xMatrices=NULL

  predTempMatrices = NULL
  Xpred = NULL
  predXmatrices = NULL
  
  timeCategories = NULL
  tempInfo = NULL
  
  nPoints = length(inputDat$fit$time)
  nPredPoints = length(inputDat$pred$tPred)
  
  fitDat = inputDat$fit
  predDat = inputDat$pred
  
  intervalMinutes = fitDat$intervalMinutes
  
  if (!is.null(fitDat$TempF)) {
    
    # Separate times of the week into "unoccupied mode", "startup mode",
    # and "occupied mode." Modes are based on when the building is more or
    # less sensitive to outdoor air temperature, not on when the building is
    # actually occupied. (More sensitive to outdoor temperature presumably means
    # the building is heated or cooled within a tighter band).
    if (is.null(okInclude)) { okInclude = rep(T, length(fitDat$load))}
    timeCatDefinitions = determineTimeCategories(fitDat$time[okInclude], fitDat$load[okInclude], intervalMinutes,
                                                 fitDat$TempF[okInclude],  includeStartup = includeStartup, verbose = verbose)
    timeCategories = setTimeCategories(fitDat$time, timeCatDefinitions, intervalMinutes, verbose=verbose)
    timeCategoriesPred = setTimeCategories(predDat$tPred, timeCatDefinitions, intervalMinutes, verbose=verbose)
    
    tempMatrices = NULL
    predTempMatrices = NULL
    Tknots = NULL
    for (icol in 1:ncol(timeCategories)) {
      Tknots[[icol]] = defineTemperatureKnots(fitDat$TempF[timeCategories[,icol]], Tknots = c(55, 65, 80))
      tm =  makeTempMatrix(fitDat$TempF[timeCategories[,icol]], Tknots[[icol]])
      TmatCatOnly = tm$Tmat
      # TmatCatOnly is the temperature matrix for just those times that are in
      # the time category. But what we want is a matrix that has one row per
      # data point in the entire dataset, but is filled with zeroes except for
      # those times. So build that now.
      TmatCat = matrix(0,nrow = nPoints,ncol = ncol(TmatCatOnly))
      TmatCat[timeCategories[,icol],] = TmatCatOnly
      tempMatrices[[icol]] = TmatCat
      
      if (!is.null(predDat$TempF)) {
        ptm = makeTempMatrix(predDat$TempF[timeCategoriesPred[,icol]], Tknots[[icol]])
        pTmatCatOnly = ptm$Tmat
        predTmatCat = matrix(0,nrow = nPredPoints,ncol = ncol(TmatCatOnly))
        predTmatCat[timeCategoriesPred[,icol],] = pTmatCatOnly
        predTempMatrices[[icol]] = predTmatCat
      }
    }
  }
  
  if (!is.null(fitDat$xMat)) {
    Xfit = as.matrix(fitDat$xMat)
    Xpred = as.matrix(predDat$xMat)
    
    xMatrices = NULL
    predXmatrices = NULL
    
    xMatrices[[1]] = matrix(0,nrow=nPoints,ncol=ncol(Xfit))
    xMatrices[[2]] = matrix(0,nrow=nPoints,ncol=ncol(Xfit))
    predXmatrices[[1]] = matrix(0, nrow=nPredPoints,ncol=ncol(Xfit)) 
    predXmatrices[[2]] = matrix(0, nrow=nPredPoints,ncol=ncol(Xfit)) 
    
    
    for (icol in 1:ncol(Xfit)) {
      knotpoint = quantile(Xfit[,icol], xPredThresh )
      xMatrices[[1]][,icol] = ifelse(Xfit[,icol] > knotpoint, knotpoint, Xfit[,icol])
      xMatrices[[2]][,icol] = ifelse(Xfit[,icol] <= knotpoint, Xfit[,icol]-knotpoint,0)
      
      predXmatrices[[1]][,icol] = ifelse(Xpred[,icol] > knotpoint, knotpoint, Xpred[,icol])
      predXmatrices[[2]][,icol] = ifelse(Xpred[,icol] <= knotpoint, Xpred[,icol]-knotpoint,0)
    }
  } else {
    Xfit = NULL
    Xpred = NULL
    xMatrices = NULL
    predXmatrices = NULL
  }
  
  Out = NULL
  Out$fit = NULL
  Out$fit$intervalOfWeek = fitDat$intervalOfWeek
  Out$fit$tempMatrices = tempMatrices
  Out$fit$xMat = Xfit
  Out$fit$xMatrices = xMatrices
  
  Out$pred = NULL
  Out$pred$intervalOfWeek = predDat$intervalOfWeek
  Out$pred$tempMatrices = predTempMatrices
  Out$pred$xMat = Xpred
  Out$pred$xMatrices = predXmatrices
  
  
  if (verbose > 2) {
    print("   Leaving DefineModelVariables()")
  }
  return(Out)
}



determineTimeCategories = function(tLoad, load, intervalMinutes, temp = NULL,  startMinutes =
                                     121, includeStartup = T, verbose = 0) {
  if (verbose > 2) {
    print("  Starting determineTimeCategories")
  }
  
  timeIntervals = calcTOW(tLoad)
  intervalOfWeek = timeIntervals$intervalOfWeek
  
  uTOW = sort(unique(intervalOfWeek))
  nInterval = length(uTOW)
  if (is.null(temp)) {
    # We can't determine temperature dependence, so just put everything into "unoccupied mode"
    occ = rep(F,nInterval)
    nonOcc = rep(T,nInterval)
    Out = cbind(uTOW,nonOcc,occ,occ,occ)
  } else {
    occMat = findOccUnocc(intervalOfWeek,load,temp,intervalMinutes,
                          verbose = 1)
    
    
    occIntervals = occMat[occMat[,2]==1,1]
    okOcc = intervalOfWeek %in% occIntervals
    
    # Now determine which times of the week are in the "startup period" each day
    # Define "startup" as: any 'occupied' period within startMinutes of the start of the day,
    # where the start of the day is defined as the first period of the day
    # after 4 a.m. that the building is in "occupied mode."
    okStart = rep(F,length(uTOW))
    
    timeOfDay = tLoad$hour + tLoad$min / 60
    okAfter4 = timeOfDay > 4 # after 4 a.m.
    
    startIntervals = NULL
    for (iDay in 0:6) {
      # run through days of the week
      okDay = tLoad$wday == iDay
      ok =  okDay & okAfter4 & okOcc
      
      if (sum(okOcc[ok] ) > 1) {
        # At least some times on this day of the week, the building is in
        # 'occupied' mode.
        
        # For all examples of this day of the week in the dataset, find the start time. Take 20th percentile to
        # summarize the start time for this day of the week.
        startTimes = aggregate(timeOfDay[ok],by = list(tLoad$yday[ok]),min,na.rm =
                                 T)$x
        thisDayStartTime = quantile(startTimes,p = 0.2)[1]
        
        minOfWeekStart = iDay * 24 * 60 + thisDayStartTime * 60
        minOfWeekEnd = minOfWeekStart + startMinutes
        
        intervalOfWeekStart = 1 + floor(minOfWeekStart / intervalMinutes)
        intervalOfWeekEnd = 1 + floor(minOfWeekEnd / intervalMinutes)
        startIntervals = c(startIntervals, intervalOfWeekStart:intervalOfWeekEnd)
      }
    }
    
    occ = rep(F,nInterval)
    occ[uTOW %in% occIntervals] = T
    nonOcc = !occ
    
    occStart = rep(F,nInterval)
    
    if (includeStartup) {
      occStart[uTOW %in% startIntervals] = T
    }
    occNonStart = occMat[,2] & !occStart
    
    # There can be some funny cases: If there's a very brief occupied period, the rules above will lead to putting 
    # some times in "occ startup" that should be unoccupied. Fix this:
    occStart[nonOcc] = F
    occNonStart[nonOcc] = F 
    
 
    
    Out = cbind(uTOW,nonOcc,occ,occStart,occNonStart)
    
    # If any column is full of zeros -- the category is completely empty -- then leave out that column
    ntot = apply(Out, 2, sum)
    Out = Out[,ntot > 0]
  }
  
  if (verbose > 2) { print("  Leaving determineTimeCategories()")}
  return(Out)
}

setTimeCategories = function(tLoad, timeCategoryDefinitions, intervalMinutes, verbose=0) {
  intervalOfWeek = calcTOW(tLoad)$intervalOfWeek
  timeCategoryMat = matrix(F,nrow=length(tLoad),ncol=ncol(timeCategoryDefinitions)-1)
  for (icol in 1:ncol(timeCategoryMat)) {
    timeCategoryMat[,icol] = intervalOfWeek %in% timeCategoryDefinitions[timeCategoryDefinitions[,1+icol]==1,"uTOW"]
  }
  return(timeCategoryMat)
}

prepareDataFrame = function(dataStruct, verbose=0) {
	# Put together a single data frame, for use in fitting the model, based on
	# data from the data structure. 
	#
	# This can probably be done a lot simpler by using cbind to put pieces together,
  if (verbose > 1) print(" Starting prepareDataFrame()")
  predFrame = data.frame(as.factor(dataStruct$intervalOfWeek))
  names(predFrame) = "intervalOfWeek"
  if (!is.null(dataStruct$tempMatrices)) {
    for (i in 1:length(dataStruct$tempMatrices)) {
      TM = dataStruct$tempMatrices[[i]]
      dimnames(TM)[[2]] = as.list(paste("TempBin",i,".",1:ncol(TM),sep=""))
      if(any(!is.na(TM) & TM != 0)){
        predFrame = cbind(predFrame, TM)
      }  
    }
  }
  if (!is.null(dataStruct$xMat)) {
    for (i in 1:length(dataStruct$xMatrices)) {
      XM = dataStruct$xMatrices[[i]]
      dimnames(XM)[[2]] = as.list(paste("X",i,".",1:ncol(XM),sep=""))
      if (any(!is.na(XM) & XM != 0)) {
        predFrame = cbind(predFrame, XM)
      }
    } 
  }
  if (verbose > 1) print(" Leaving prepareDataFrame()")
  return(predFrame)
}

######### XXXPNP
fitMultipleModels = function(dataStructure) {
  timeVec = getTime(timeVec)
  timeNum = as.numeric(timeVec)
  
  # calculate total length of dataset, in days
  totalIntervalLength = (tail(timeNum,1) - timeNum[1]) / (24 * 3600)
  nPoints = length(yVec)
  
  nFits = 1 + floor(totalIntervalLength / timescaleDays)
  individualLength = totalIntervalLength / nFits
  timestep = individualLength * 24 * 60 * 60 # timestep in seconds
  
  predMatrix = matrix(0,nrow = nPoints,ncol = nFits)
  weightMatrix = matrix(0,nrow = nPoints,ncol = nFits)
  SEmatrix = matrix(0,nrow = nPoints,ncol = nFits)
  
  for (i in 1:nFits) {
    if (verbose > 3) {
      print(paste("Doing fit number",i,"of",nFits))
    }
    centralTime = getTime(timeNum[1] + (i - 1) * timestep)
    tDiff = as.numeric(difftime(timeVec,centralTime,units = "days"))
    weightvec = timescaleDays ^ 2 / (timescaleDays ^ 2 + tDiff ^ 2)
    amod = lm(yVec ~ . + 0,data = predFrame, na.action = na.exclude,
              weight = weightvec)
    SE <- sqrt(sum(residuals(amod) ^ 2, na.rm = T) / amod$df.residual)
    
    weightMatrix[,i] = weightvec
    predMatrix[,i] = predict(amod,predFrame)
    SEmatrix[,i] = SE
  }
  
  yPred = apply(predMatrix * weightMatrix,1,sum) / apply(weightMatrix,1,sum)
  StdErr = apply(SEmatrix * weightMatrix,1,sum) / apply(weightMatrix,1,sum)
}

#
fitModel = function(yVec, fitFrame, weightVec = NULL, predFrame=NULL, verbose=0) {
  # Fit a linear model to predict yVec as a linear combination of columns of predFrame (which may include factors).
  # Inputs:
  #  timeVec: one timestamp per data point (may be numeric, string in Y-m-d H:M format,
  #		or a POSIX time). This is used to make final predictions that adapt with time
  #		if the behavior of the system changes with time.
  #  yVec: the variable to be fit. Numeric vector. May contain NA.
  #  predFrame: A data frame or vector of predictive variables. May contain "time-of-week"
  #		variables as a column of type "factor." 
  if (verbose > 2) { print("starting fitModel") }
  if (is.null(predFrame)) { predFrame = fitFrame }
  if (is.null(weightVec)) { weightVec = rep(1,length(yVec))}
  amod = lm(yVec ~ .+0,data=fitFrame, weights=weightVec, na.action=na.exclude)
  
  modelPredictions = predict(amod, predFrame, se.fit=F)
  
  SE <- sqrt( sum( residuals(amod)^2, na.rm=T ) / amod$df.residual )
  
  
  Out = NULL
  Out$yFit = predict(amod)
  Out$yPred = modelPredictions
  Out$SE = SE
  Out$SEfit = NULL
  Out$predMatrix = predFrame
  if (verbose > 2) { print("leaving fitModel") }
  return(Out)
}


GoodnessOfFit = function(time1, loadVec, time2, baselinePred, verbose=1) {
	fail=F	
	if (verbose > 1) { print("starting GoodnessOfFit()") }
	if (length(loadVec) != length(baselinePred)) {
		if (verbose > 0) { 
			print("Warning: GoodnessOfFit: vector length mismatch")
			fail = T 
		}
	}
	if (sum(abs(as.numeric(time1) - as.numeric(time2))) > 0) {
		if (verbose > 0) {
			print("Warning: GoodnessOfFit: timestamps do not match") 
			fail = T
		}
	}
	
	if (fail) { 
		# something's wrong, return NAs
		resid = rep(NA,length(time1)) 
	} else { 
		resid = loadVec-baselinePred
	}	
	iHour = time1$year*366*24+time1$yday*24+time1$hour
	ok_8AM_6PM = 7 < time1$hour & time1$hour < 19
	
	loadVecHour = aggregate(loadVec,by=list(iHour),mean,na.action=na.omit)[,2]
	baselinePredHour = aggregate(baselinePred,by=list(iHour),mean,na.action=na.omit)[,2]
	residHour = loadVecHour - baselinePredHour
		
	RMSE_Interval = sqrt(mean(resid^2,na.rm=T))
	RMSE_Interval_Daytime = sqrt(mean(resid[ok_8AM_6PM]^2,na.rm=T))
	
	RMSE_Hour = sqrt(mean(residHour^2,na.rm=T))

	MAPE_Interval = mean(abs(resid/loadVec),na.rm=T)*100
	MAPE_Interval_Daytime = mean(abs(resid[ok_8AM_6PM]/loadVec[ok_8AM_6PM]),
		na.rm=T)*100
	
	MAPE_Hour = mean(abs(residHour/loadVecHour),na.rm=T)*100	
	
	corr_Interval = cor(loadVec,baselinePred,use="complete.obs")
	corr_Interval_Daytime = cor(loadVec[ok_8AM_6PM],baselinePred[ok_8AM_6PM],use="complete.obs")
	corr_Hour = cor(loadVecHour,baselinePredHour,use="complete.obs")

	if(verbose > 1) { print("leaving GoodnessOfFit()") }
	Out = NULL
	Out$RMSE_Interval = RMSE_Interval
	Out$MAPE_Interval = MAPE_Interval
	Out$corr_Interval = corr_Interval
	Out$RMSE_Hour = RMSE_Hour
	Out$MAPE_Hour = MAPE_Hour
	Out$corr_Hour = corr_Hour
	Out$RMSE_Interval_Daytime = RMSE_Interval_Daytime
	Out$MAPE_Interval_Daytime = MAPE_Interval_Daytime
	Out$corr_Interval_Daytime = corr_Interval_Daytime
	return(Out)
}

prepareAndFitModel = function(readyDat, weightVec = null, xPredThresh = xPredThresh, includeStartup = includeStartup, verbose=verbose) {
  if (verbose > 1) { print(" Starting prepareAndFitModel()")}
  if (!is.null(weightVec)) {
    # If weights are provided, use only the highly weighted data points to determine the occupied and unoccupied periods.
    # We do it this way because, especially in a long data series, the times-of-day of occupied and unoccupied modes can change.
    okInclude = (weightVec/max(weightVec)) > 0.2
  } else {
    okInclude = NULL
  }
  # 
  # We do defineModelVariables inside prepareAndFitModel so that it is run again for each set of weight
  # vectors. This allows weights to be used in determining occupied/unoccupied periods rather than
  # just doing that once.
  
  dataStruct = defineModelVariables(readyDat, xPredThresh = xPredThresh, okInclude = okInclude, includeStartup = includeStartup, 
                                    verbose = verbose)
  if (verbose > 2) { print(" Preparing data frame for fitting the model")}
  fitDf = prepareDataFrame(dataStruct$fit, verbose=verbose)
  predDf = prepareDataFrame(dataStruct$pred, verbose=verbose)
  if (verbose > 4) { print(head(fitDf))}
  if (verbose > 4) { print(head(predDf))}
  
  if (verbose > 2) { print("  Fitting the model")}
  modelResults = fitModel(readyDat$fit$load, fitDf, weightVec = weightVec, predFrame = predDf, verbose = 5)
  
  if (verbose > 1) { print(" Leaving prepareAndFitModel()")}
  Out = modelResults
  return(Out)
}

makeBaseline = function(readyDat, timescaleDays, xPredThresh=0.2, backwardOnly=F, includeStartup = includeStartup, verbose = 0) {
  # calculate total length of fit period, in days
  timeNum = as.numeric(readyDat$fit$time)
  totalIntervalLength = (tail(timeNum,1) - timeNum[1]) / (24 * 3600)
  
  nFits = floor(totalIntervalLength / timescaleDays)
  individualLength = totalIntervalLength / nFits
  timestep = individualLength * 24 * 60 * 60 # difference between "central time points" in seconds
  
  nFitPoints = length(readyDat$fit$time)
  fitLoadMatrix = matrix(0,nrow = nFitPoints,ncol = nFits)
  fitWeightMatrix = matrix(0,nrow = nFitPoints,ncol = nFits)
  
  nPredPoints = length(readyDat$pred$time)
  predLoadMatrix = matrix(0,nrow = nPredPoints,ncol = nFits)
  predWeightMatrix = matrix(0,nrow = nPredPoints,ncol = nFits)
  centralTimes = rep(NA, nFits)
  
  for (i in 1:nFits) {
    if (verbose > 3) {
      print(paste("Doing fit number",i,"of",nFits))
    }
    centralTime = getTime(timeNum[1] + (i-0.5) * timestep)
    centralTimes[i] = as.numeric(centralTime)
    
    tFitDiff = as.numeric(difftime(readyDat$fit$time,centralTime,units = "days"))
    tPredDiff = as.numeric(difftime(readyDat$pred$time,centralTime,units = "days"))
    
    fitWeightVec = timescaleDays ^ 2 / (timescaleDays ^ 2 + tFitDiff ^ 2)
    predWeightVec = timescaleDays ^ 2 / (timescaleDays ^ 2 + tPredDiff ^ 2)

    if (backwardOnly) { 
      fitWeightVec[timeNum > as.numeric(centralTime)] = 0
      predWeightVec[timeNum > as.numeric(centralTime)] = 0
    }
    
    results = prepareAndFitModel(
      readyDat, weightVec = fitWeightVec, xPredThresh = xPredThresh, includeStartup = includeStartup, verbose =
        verbose
    )
    
    #amod = lm(yVec ~ . + 0,data = predFrame, na.action = na.exclude,
    #          weight = weightvec)
    #SE <- sqrt(sum(residuals(amod) ^ 2, na.rm = T) / amod$df.residual)
    
    fitLoadMatrix[,i] = results$yFit
    fitWeightMatrix[,i] = fitWeightVec
    
    predLoadMatrix[,i] = results$yPred
    predWeightMatrix[,i] = predWeightVec
    
    
  }
  
  # Note: the following multiplications are elementwise, not matrix multiplications.
  yPred = apply(predLoadMatrix * predWeightMatrix,1,sum) / apply(predWeightMatrix,1,sum) 
  yFit = apply(fitLoadMatrix * fitWeightMatrix,1,sum) / apply(fitWeightMatrix,1,sum)
  
  # All of the calculations above are done with "nice" time intervals, but the user could ask for any
  # timestamps (like, what's the prediction at 2016-08-12 04:17:22), so interpolate to the times they want.
  yPred = approx(as.numeric(readyDat$pred$time),yPred,as.numeric(readyDat$pred$tPred))[[2]]
  
  Out = NULL
  Out$fit = NULL
  Out$pred = NULL
  
  Out$fit$time = readyDat$fit$time
  Out$fit$yLoad = yFit
  Out$fit$yLoadMatrix = fitLoadMatrix
  Out$fit$weightMatrix = fitWeightMatrix
  Out$fit$centralTimes = centralTimes
  Out$fit$timeStepSeconds = timestep
  
  Out$pred$time  = readyDat$pred$time
  Out$pred$yLoad = yPred
  Out$pred$LoadMatrix = predLoadMatrix
  Out$pred$weightMatrix = predWeightMatrix
  Out$pred$tPred = readyDat$pred$tPred
  Out$pred$yPred = yPred
  return(Out)
}

preProcessData = function(inputDat, verbose = 0) {
  if (verbose > 1) { print(" Defining time series for fit period.") }
  fitDat = prepareTimeSeries(inputDat$fit, tStart = inputDat$fit$tLoad[1], tEnd = tail(inputDat$fit$tLoad,1),
                              intervalMinutes = inputDat$intervalMinutes, verbose = verbose)
  if (is.null(inputDat$intevalMinutes)) { inputDat$intervalMinutes = fitDat$intervalMinutes}
  if (verbose > 1) { print(" Defining time series for prediction period.") }
  predDat = prepareTimeSeries(
    inputDat$pred, tStart = inputDat$pred$tPred[1], tEnd = tail(inputDat$pred$tPred,1),
    intervalMinutes = inputDat$intervalMinutes, verbose = verbose
  )
  
  readyDat = list(fitDat, predDat)
  names(readyDat) = c("fit","pred")
  return(readyDat)
}

leadingError = function(xvec, yvec, n=length(xvec)/5) {
  # Calculate the running leading rms error, for a window length specified by n.
  err = sqrt((yvec-xvec)^2)
  ce = filter(err, c(rep(0,n),rep(1,n)/n))
  return(ce)
}

main = function(loadFile, timeStampFile = NULL, inTemperatureFile = NULL, 
                inPredTemperatureFile =NULL, xFile = NULL, predXfile = NULL, 
                outGoodnessOfFitFile = NULL, outBaselineFile = NULL,
                timescaleDays = timescaleDays, fahrenheit = T, includeStartup = T, xPredThresh = 0.2,
                intervalMinutes = 15, 
                writeOutputFiles = T, verbose = 0,
                returnOutput = T) {
  if (verbose > 0) print("Starting main()")
  inputDat = readInputFiles(
    loadFile, timeStampFile = timeStampFile,
    inTemperatureFile = inTemperatureFile,
    inPredTemperatureFile = inPredTemperatureFile,
    xFile = xFile,
    predXfile = predXfile,
    outGoodnessOfFitFile = outGoodnessOfFitFile, 
    verbose = verbose
  )
  inputDat$intervalMinutes = intervalMinutes
  
  readyDat = preProcessData(inputDat, verbose = verbose)
  
  baselineInfo = makeBaseline(readyDat, timescaleDays = timescaleDays, includeStartup = includeStartup, verbose = verbose)
  
  trainingGOF = GoodnessOfFit(readyDat$fit$time, readyDat$fit$load,
                              baselineInfo$fit$time, baselineInfo$fit$yLoad,
                              verbose=verbose)
  
  if (!is.null(readyDat$pred$load)) {
    predGOF = GoodnessOfFit(readyDat$pred$time, readyDat$pred$load,
                          baselineInfo$pred$time, baselineInfo$pred$yLoad,
                          verbose=verbose)
  } else {
    predGOF = NULL
  }
  if (writeOutputFiles) {
    if (!is.null(outGoodnessOfFitFile)) {
    write(t(cbind(names(trainingGOF),round(unlist(trainingGOF),3))),
          outGoodnessOfFitFile,ncol=2,sep=",")
    }
    
    if (!is.null(outBaselineFile)) {
      baselineOut = cbind(as.character(baselineInfo$pred$tPred),round(baselineInfo$pred$yPred,2))
      write(t(baselineOut),outBaselineFile,sep=",",ncol=2)
    }
  }
  
  Out = NULL
  Out$inputDat = inputDat
  Out$processedInputDat = readyDat
  Out$baselineInfo = baselineInfo
  Out$trainingGOF = trainingGOF
  Out$predGOF = predGOF
  if (verbose > 0) print("Leaving main()")
  
  if (returnOutput) {
    return(Out)
  } else {
    return()
  } 
}

