Changes from previous version:
I. Additional predictive variables may be provided (optional).   

A file name can be provided for a file that contains predictive data. The first column is the timestamp, subsequent columns are data values (one column per variable). One exemplary use case is to provide data related to occupancy, such as the number of active Wi-Fi connections at a given time, the number of occupied hotel rooms, the number of outgoing phone calls, number of cash register transactions, etc. 

Each of these predictive variables is included in the model using a piecewise-continuous linear relationship with electric load: all else equal, load is assumed to change linearly with the variable up to a certain point, then linearly above that point but with a different slope. The point at which the slope changes is calculated as a percentile of the variable's value during the fit period, with a default of the 20th percentile (expressed as 0.2).  That is, there is one slope up to the 20th percentile of the variable's value, and another slope for higher values of the variable. 

II. There are now three categories of temperature relationships rather than two. Previously, we attempted to determine whether the building was in "occupied" or "unoccupied" mode, and estimated a different relationship between load and outdoor air temperature in each mode. Now there are three categories: unoccupied, occupied-startup, and occupied-non-startup. The occupied-startup period for a given day of the week comprises the first two hours that the building is in occupied mode. 

III. Pre-processing of all of the data has been changed. If predictive variables -- either outdoor air temperature or other variables such as occupancy-related variables -- have data gaps, these are now filled in.  Short gaps of missing data (less than 3.1 hours) are filled in with linear interpolation. Missing data in longer gaps  are imputed from a statistical model that includes either "time of week" or "time of day" effects (depending on the variable being imputed). Either model also includes a "week" effect. 

For example, suppose a day of temperature data is missing. A statistical model is fit to the entire set of temperature data:
predicted temperature = (week effect) + (time of day effect)
And the resulting predictions are used to fill in the missing data.

The time-of-day effect is used to impute missing temperature data.
The time-of-week effect is used to impute missing values of additional predictive variables (see I above).

IV. There has been substantial re-factoring of the code to make it more maintainable, extensible, and modular. As one of many changes, the R functions have been separated from the code that runs the functions from the command line. The file baseline.R is just a skeleton that reads and parses command-line inputs and then calls the R function main(). That function and all supporting functions are in a separate single file, called fitBaselineModel.R.  

The command line function call is backward-compatible with previous versions. You can still create a baseline with a function like:
./Baseline.R -l loadData.csv -s predTimestamps.csv -t temperatureData.csv -x predictiveVariables.csv





