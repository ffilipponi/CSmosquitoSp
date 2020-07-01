#!/usr/bin/Rscript
# Title         : CSmosquitoSp spatial analyst
# Description   : Perform spatially-oriented analysis of Citizen Scientists mosquito reports for specific Area Of Interest and a selected time interval
# Date          : Jul 2020
# Version       : 0.5.0
# Licence       : GPL v3
# Authors       : Federico Filipponi
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>
# ######################################################################

# ##################################
# Load required libraries
library(CSmosquitoSp)

# ###########################################################
# set variables
# ###########################################################

# set input ZanzaMapp DataBase file path
input_path <- normalizePath(path="C:/zanzamapp/DB_zanzamApp_20180228.csv", winslash="/", mustWork=T)

# set output directory
output_path <- normalizePath(path="C:/zanzamapp/", winslash="/", mustWork=FALSE)

# set Area Of Interest (currently supported: 'Roma', 'Latina', 'Lazio', 'Italia', 'Italia_comuni', 'Roma_municipi', 'Roma_griglia_1000m', 'Roma_griglia_250m', 'Roma_griglia_100m')
aoi <- c("Lazio")

# set start date
start_date <- as.Date("2017-09-01")

# set end date
end_date <- as.Date("2017-09-30")

# ##################################
# Analyse data

# import ZanzaMapp DataBase
CSMS_db <- CSmosquitoSp.import(input_path, type="ZanzaMapp")

# perform spatial analysis
CSMS_result <- CSmosquitoSp.analysis(input_db=CSMS_db, aoi=aoi, start_date=start_date, end_date=end_date)

# or as alternative set parameters directly in the function
CSMS_result_roma_201707 <- CSmosquitoSp.analysis(input_db=CSMS_db, aoi="Roma", start_date=as.Date("2017-07-01"), end_date=as.Date("2017-07-31"))

# # alternatively load an external polygon vector layer
# library(sf)
# area <- sf::st_read(dsn="/media/DATA/zanzamapp/auxiliary_files/Lazio.shp")
# # perform spatial analysis using the external polygon
# CSMS_result <- CSmosquitoSp.analysis(input_db=CSMS_db, aoi=area, start_date=start_date, end_date=end_date, aoi_name="Area_name")

# ##################################
# Export results

# save results from CSmosquitoSp spatial analysis to disk
CSmosquitoSp.export(x=CSMS_result, output_path=output_path)

# save all results from CSmosquitoSp spatial analysis to disk (included points and export to KML format)
# CSmosquitoSp.export(x=CSMS_result, output_path=output_path, points=TRUE, kml=TRUE)

# ##################################
# Generate maps

# plot map of perceived mosquito abundance
CSmosquitoSp.plot(x=CSMS_result, field="VAL_PMFA")
# plot map of sampling effort
CSmosquitoSp.plot(x=CSMS_result, field="REP_DC_POP")

# plot map of Weighted average of DayTime mosquito (i.e. Aedes albopictus) with Italian title and footer and with caption figure
CSmosquitoSp.plot(x=CSMS_result, field="DT_V_AVG", maptitle="Perceived Aedes albopictus abundance", lang = "ITA", caption_logo = TRUE)

# plot map of population (any numeric field of 'names(CSMS_result$aoi)' is supported)
CSmosquitoSp.plot(x=CSMS_result, field="POPULATION")

# save plot to PNG file
plot_field <- "VAL_AVG"
png_filename <- normalizePath(path=paste(output_path, "/", 
                                         paste(CSmosquitoSp:::aoi_extent$aoi[which(CSmosquitoSp:::aoi_extent$aoi == CSMS_result@aoi_name)], "_", as.character(CSMS_result@start_date), "_", as.character(CSMS_result@end_date), sep=""),
                                         "_map_", plot_field, ".png", sep=""), winslash="/", mustWork=FALSE)
CSmosquitoSp.plot(x=CSMS_result, field=plot_field, filename=png_filename)

# ##################################
# Display results in R

# show CSmosquitoSp spatial analysis results
CSMS_result

# get table of spatial points used for the analysis
CSMS_points <- st_set_geometry(CSMS_result@points, NULL)

# get table with the results from the spatial analysis
CSMS_table <- st_set_geometry(CSMS_result@aoi, NULL)

# display the descrition of the resulting field names
CSmosquitoSp:::result_legend

# ##################################
# Development section

# perform spatial analysis using the DB 'full_clean' option
CSMS_result <- CSmosquitoSp.analysis(input_db=CSMS_db, aoi=aoi, start_date=start_date, end_date=end_date, full_clean=TRUE)

# you can edit the source code of 'CSmosquitoSp.analysis' to modify how the values are calculated, and load it as a source
source("C:/CSmosquitoSp_analysis.R")
# perform spatial analysis
CSMS_result <- CSmosquitoSp.analysis(input_db=CSMS_db, aoi=aoi, start_date=start_date, end_date=end_date)
