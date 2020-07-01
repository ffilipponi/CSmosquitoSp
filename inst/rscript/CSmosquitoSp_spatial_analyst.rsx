##CSmosquitoSp=group
##input_DB=file
##aoi=string
##start_date=string
##end_date=string
##output=output vector

# Title         : CSmosquitoSp spatial analyst (QGis processing R script)
# Description   : Perform spatially-oriented analysis of Citizen Scientists mosquito reports for specific Area Of Interest and a selected time interval
# Date          : Jul 2020
# Version       : 0.5
# Licence       : GPL v3
# Authors       : Federico Filipponi
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>

# ##################################
#>print("Loading required libraries")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(CSmosquitoSp))

#>print("Get variables ...")

# set input mosquito DataBase file path
input_path <- normalizePath(path=input_DB, winslash="/", mustWork=T)

# check 'start_date' and 'end_date' and convert to 'Date' class
start_date_check <- try(as.Date(start_date, format= "%Y-%m-%d"))
if(class(start_date_check) == "try-error" || is.na(start_date_check)){
  stop("'start_date' argument should be either an object of class 'Date' or a character object with format 'YYYY-MM-DD'")
} else {
  start_date <- as.Date(start_date)
}

end_date_check <- try(as.Date(end_date, format= "%Y-%m-%d"))
if(class(end_date_check) == "try-error" || is.na(end_date_check)){
  stop("'end_date' argument should be either an object of class 'Date' or a character object with format 'YYYY-MM-DD'")
} else {
  end_date <- as.Date(end_date)
}

#>print(paste("Processing mosquito DataBase: ", input_path, sep=""))
#>print(paste("Area Of Interest: '", aoi, "'", sep=""))
#>print(paste("Analysed time interval: ", start_date, " - ", end_date, sep=""))

#>print("Import mosquito DataBase ...")
# import mosquito DataBase
CSMS_db <- CSmosquitoSp.import(input_path, type="ZanzaMapp")
#>print("Mosquito DataBase imported")

print("Analyse data ...")
# perform spatial analysis
CSMS_result <- CSmosquitoSp.analysis(input_db=CSMS_db, aoi=aoi, start_date=start_date, end_date=end_date)
#>print("CSmosquitoSp analysis: Done")

#>print("Export results ...")
# export result to QGis
# save results from CSmosquitoSp spatial analysis to disk
# out_poly <- as(CSMS_result@aoi, "Spatial")
out_poly <- as(CSMS_result@aoi, "sf")
output <- out_poly
#>print("Export results: Done")
