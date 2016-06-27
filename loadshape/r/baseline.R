#!/usr/bin/env Rscript
# 
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
source("fitBaselineModel.R")

library("optparse")
option_list = list(
	make_option(c("-l","--loadFile"),
		help="Name of load data file (Required)"),
	make_option(c("-s","--timeStampFile"),
		help="Name of file that contains timestamps of baseline predictions (Required)"),			
	make_option(c("-t","--temperatureFile"), 
		help="Name of temperature data file (Optional, but required if model is to use temperature)"),
	make_option(c("-f","--fahrenheit"),
		default = T,
		help="Temperatures are in Fahrenheit? [default %default]"),	
	make_option(c("-p","--predictTemperatureFile"),
		help="Name of forecast or prediction temperature file (Optional)"),
	make_option(c("-x","--xFile"),
		default = NULL,
		help="Name of file of additional explanatory variables (Optional)"),
	make_option(c("-X","--XFile"),
		default = NULL,
		help="Name of forecast or prediction explanatory variables file (Optional)"),
	make_option(c("-o","--outputBaselineFile"),
		default="baseline.csv",
		help="Name of output file for the baseline [default %default]"),
	make_option(c("-e","--errorStatisticsFile"),
		default="errorStatisticsFile.csv",
		help="Name of output file for goodness-of-fit statistics [default %default]"),
	make_option(c("-d","--timescaleDays"),
		default=14,
		help="timescale for weighting function [default %default]"),
	make_option(c("-i","--intervalMinutes"),
		default=15,
		help="length of a Time Of Week interval [default %default]"),			
	make_option(c("-v","--verbosity"),
		default=1,
		help="determine what progress and error reports to print (non-neg integer) [default %default]")	
	)
	
opt = parse_args(OptionParser(option_list=option_list))	
	
#

##################################

if (is.null(opt$loadFile)) {
	stop("Error: no input Load File is defined.")
} else {
	loadFile=opt$loadFile
}
if(is.null(opt$timeStampFile)) {
	stop("Error: no file of output timestamps is defined.")
} else {
	timeStampFile = opt$timeStampFile
}


inTemperatureFile = opt$temperatureFile
inPredTemperatureFile = opt$predictTemperatureFile
timescaleDays = opt$timescaleDays
outBaselineFile = opt$outputBaselineFile
timeStampFile = opt$timeStampFile
xFile = opt$xFile
predXfile = opt$XFile
outGoodnessOfFitFile = opt$errorStatisticsFile
verbosity = opt$verbosity
intervalMinutes = opt$intervalMinutes
fahrenheit = opt$fahrenheit


if (!is.logical(fahrenheit)) {
	stop(
		paste("Error: fahrenheit must be logical (True or False); current value is",
			fahrenheit
		)	
	)		
}


if (verbosity > 1) {  
 	print(
 	  paste("loadFile =",loadFile, "timeStampFile=",timeStampFile)
 	)
} 	  
# 	print(
# 	  paste( "inTemperatureFile =",inTemperatureFile, "inPredTemperatureFile =",
# 	   inPredTemperatureFile
# 	  )
# 	)   
# 	print(paste("xFile =", xFile, "predxFile =" predxFile))
# 	print(paste(
# 	  "outBaselineFile = ", outBaselineFile,
# 	  "intervalMinutes = ", intervalMinutes,
# 	  "timescaleDays =",timescaleDays)) 
# }

writeOutputFiles = T 
returnOutput = F

main(loadFile=loadFile,
	timeStampFile=timeStampFile,
	inTemperatureFile=inTemperatureFile,
	inPredTemperatureFile=inPredTemperatureFile,
	xFile=xFile,
	predXfile=predXfile,
	outBaselineFile=outBaselineFile,
	outGoodnessOfFitFile=outGoodnessOfFitFile,
	intervalMinutes=intervalMinutes,
	timescaleDays=timescaleDays, 
	fahrenheit = fahrenheit,
	writeOutputFiles = writeOutputFiles,
	returnOutput = returnOutput,
	verbose=verbosity)

if (verbosity > 1) { print("Done.") }	
	