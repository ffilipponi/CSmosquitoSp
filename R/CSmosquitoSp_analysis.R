# Title         : CSmosquitoSp spatial analyst
# Description   : Perform spatially-oriented analysis of Citizen Scientists mosquito reports for specific Area Of Interest and a selected time interval
# Date          : Jul 2020
# Version       : 0.5
# Licence       : GPL v3
# Authors       : Federico Filipponi, Mattia Manica
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>

# ######################################################################
#' @title CSmosquitoSp spatial analysis
#'
#' @description This function perform spatial analysis for a selected AOI
#' using data from the CSmosquitoSp DataBase
#'
#' @param input_db DataFrame. Input CSmosquitoSp DataBase imported using function \code{\link[CSmosquitoSp]{CSmosquitoSp.import}}.
#' @param aoi Character. Area Of Interest where to perform the spatial analysis.
#' @param aoi_name Character. Name for the Area Of Interest when 'aoi' argument is supplied as external file.
#' @param start_date Character. Input start date, either an object of class \code{\linkS4class{Date}} or a character object with format 'YYYY-MM-DD'.
#' @param end_date Character. Input end date, either an object of class \code{\linkS4class{Date}} or a character object with format 'YYYY-MM-DD'.
#' @param method Character. Method to be used for the analysis.
#' The following options are supported: 'CM' (Class Median), 'RC' (ReClass) (default to 'CM').
#' @param full_clean Logical. Wheter the full cleaning of the input DataBase
#' should be performed prior the analysis.
#' @param ... Additional arguments to be passed through to function \code{\link{CSmosquitoSp.analysis}}
#'
#' @return
#' The function returns an object of class \code{\linkS4class{CSmosquitoSp.obj}} containing the following slots:
#' \itemize{
#'   \item points Report spatial points used for the analysis
#'   \item  aoi Spatial polygons of the selected Area Of Interest containing the results from the spatial analysis
#'   \item  aoi_name Name of the selected Area Of Interest
#'   \item  start_date Start day selected for the analysis
#'   \item  end_date End day selected for the analysis
#'   \item  method Method used for the analysis (currently 'CM' and 'RC' are supported)
#' }
#'
#' @author Federico Filipponi, Mattia Manica
#'
#' @keywords CSmosquitoSp spatial analysis
#'
#' @seealso \code{\link[CSmosquitoSp]{CSmosquitoSp.plot}} \code{\link[CSmosquitoSp]{CSmosquitoSp.export}} \code{\link[CSmosquitoSp]{CSmosquitoSp.import}}
#'
#' @examples
#' \dontrun{
#' # perform CSmosquitoSp spatial analysis
#' for the metropolitan area of the city of Rome
#' CSMS_roma <- CSmosquitoSp.analysis(input_db=input_db,
#' aoi="Roma", start_date="2017-09-01", end_date="2017-09-01")
#'
#' # perform CSmosquitoSp spatial analysis using the 'full_clean' option
#' for the metropolitan area of the city of Rome
#' CSMS_roma_full_clean <- CSmosquitoSp.analysis(input_db=input_db,
#' aoi="Roma", start_date="2017-09-01", end_date="2017-09-01", full_clean=TRUE)
#' }
#'
#' @import sp
#' @import sf
#' @import plyr
#' @import methods
#' @importFrom sf st_as_sf
#' @importFrom sf st_intersection
#' @importFrom sf st_crs
#' @importFrom sf st_geometry_type
#' @importFrom sf st_area
#' @importFrom sf st_transform
#' @importFrom sp merge
#' @importFrom plyr ddply
#' @importFrom plyr summarize
#' @importFrom methods new
#' @importFrom stats sd
#' @importFrom stats time
#' @importFrom utils read.table
#' @importFrom units set_units
#' @export
#

# define 'CSmosquitoSp.analysis' function
CSmosquitoSp.analysis <- function(input_db, aoi, start_date, end_date, method="CM", full_clean=FALSE, aoi_name=NULL, ...){

  # ##################################
  # Check input variables

  # check 'start_date' argument
  if(class(start_date) != "Date"){
    start_date <- tryCatch({
      as.Date(start_date)
    }, error=function(e){
      stop("'start_date' argument should be either an object of class 'Date' or a character object with format 'YYYY-MM-DD'")
    })
  }

  # check 'end_date' argument
  if(class(end_date) != "Date"){
    end_date <- tryCatch({
      as.Date(end_date)
    }, error=function(e){
      stop("'end_date' argument should be either an object of class 'Date' or a character object with format 'YYYY-MM-DD'")
    })
  }

  # check if 'end_date' is greater or equal to 'start_date'
  if(end_date < start_date)
    stop("Argument 'end_date' should be equal or greater than 'start_date'")
  
  # check source DataBase of CSmosquitoSp input table
  if(!(comment(input_db) %in% c("ZanzaMapp"))){
    stop("'input_db' argument should be an object of class 'data.frame' created using 'CSmosquitoSp.import' function")
  }

  # require the 'sp' package
  if(!requireNamespace("sp", quietly = TRUE))
    stop("Package 'sp' is required to run this function.\nYou can install using the command:\ninstall.packages('sp')")
  # require the 'spatialEco' package
  if(!requireNamespace("sf", quietly = TRUE))
    stop("Package 'sf' is required to run this function.\nYou can install using the command:\ninstall.packages('sf')")
  # require the 'plyr' package
  if(!requireNamespace("plyr", quietly = TRUE))
    stop("Package 'plyr' is required to run this function.\nYou can install using the command:\ninstall.packages('plyr')")
  
  # check if 'aoi' is an object of class 'character'
  if(class(aoi)[1] == "character"){
    if(!(aoi %in% aoi_extent$aoi)){
      stop(paste("'aoi' argument must be an object of class 'sf' or one of the following: \n'", paste(aoi_extent$aoi, collapse = "', '"), "'", sep=""))
      }
    externalaoi <- FALSE
    } else {
      # check if 'aoi' is an object of class 'sf'
      if(max(class(aoi) == "sf") == 1){
        
        # check if input vector data has 'POLYGON' or 'MULTIPOLYGON' geometry type
        if((sum(sf::st_geometry_type(aoi) == "MULTIPOLYGON") < length(aoi$geometry)) & (sum(sf::st_geometry_type(aoi) == "POLYGON") < length(aoi$geometry)))
          stop("Input vector aoi data is not a geometry of type 'MULTIPOLYGON'")
        
        # check if external aoi has the required fields in the attribute table
        if(sum(as.numeric(c("POP_DENS", "CODE", "AREA", "POPULATION") %in% names(aoi))) < 4){
          
          warning("Input 'aoi' is an object of class 'sf' but does not contain any of the fields: \n'CODE', 'AREA', 'POPULATION', 'POP_DENS'.\nTrying to compute the missing fields.")
          
          # set values for missing vector object fields
          # 'CODE'
          # use a unique FID code
          if(!("CODE" %in% names(aoi))){
            warning("'aoi' dataset does not contain field 'CODE'.\nUnique FID is set to features.")
            aoi$CODE <- as.vector(1:length(aoi$geometry))
          }
          
          # 'AREA'
          if(!("AREA" %in% names(aoi))){
            warning("'aoi' dataset does not contain field 'AREA'.\n'AREA' in km^2 is computed from features.")
            aoi$AREA <- sf::st_area(aoi)
            aoi$AREA <- units::set_units(aoi$AREA, km^2)
          } else {
            warning("'aoi' dataset contains field 'AREA'.\nIf units of 'AREA' field is not km^2, change it accordingly prior the analysis.")
          }
          
          # 'POP_DENS'
          if(!("POP_DENS" %in% names(aoi))){
            if("POPULATION" %in% names(aoi)){
              warning("'aoi' dataset does not contain field 'POP_DENS'.\n'POP_DENS' is computed from 'POPULATION' and 'AREA' fields.")
              aoi$POP_DENS <- as.double(aoi$POPULATION / aoi$AREA)
            } else {
              warning("'aoi' dataset does not contain field 'POP_DENS'.\n'POP_DENS' is set to 'NA'.")
              aoi$POP_DENS <- rep(as.numeric(NA), length(aoi$geometry))
            }
          } else {
            warning("'aoi' dataset contains field 'POP_DENS'.\nIf units of 'POP_DENS' field is not 'inhabitants / km^2', change it accordingly prior the analysis.")
          }
          
          # 'POPULATION'
          if(!("POPULATION" %in% names(aoi))){
            warning("'aoi' dataset does not contain field 'POPULATION'.\n'POPULATION' is set to 'NA'.")
            aoi$POPULATION <- rep(as.numeric(NA), length(aoi$geometry))
          }
          
        }
        
        # check if CRS is WGS84 (EPSG: 4326) and convert coordinates in case it is not
        if(sf::st_crs(aoi) != sf::st_crs(4326)){  
          warning("'aoi' CRS is not in WGS84 (EPSG: 4326).\nWill reproject the dataset.")
          aoi <- sf::st_transform(aoi, 4326)
        }
        
        externalaoi <- TRUE
      } else {
        externalaoi <- FALSE
      }
    }

  # check if 'method' argument for the value to be used is one of: 'RC', 'CM'
  if(!(method %in% c("RC", "CM")))
    stop("'method' argument must be one of the following: 'RC', 'CM'")

  # set variables for the analysis

  # set option to avoid scientific notation
  options(scipen = 20)

  # # load data from the package
  # data(aoi_extent)
  # aoi_extent <- CSmosquitoSp:::aoi_extent

  # set variables from external aoi
  if(externalaoi){
    aoi_poly <- aoi
    # get 'aoi_name'
    if(is.null(aoi_name)){
      aoi <- "external"
      } else {
        if(aoi_name %in% aoi_extent$aoi){
          aoi <- "external"
        } else {
          aoi <- aoi_name
        }
      }
    aoi_extent <- as.data.frame(rbind(as.matrix(aoi_extent), c(aoi, aoi, "aoi_poly", aoi, aoi, as.double(st_bbox(aoi_poly)[1]), as.double(st_bbox(aoi_poly)[3]), as.double(st_bbox(aoi_poly)[2]), as.double(st_bbox(aoi_poly)[4]), as.double(0.05), as.double(0.05))))
  }

  # compute number of days for the analysis
  number_days <- as.integer(as.integer(end_date - start_date) + 1)

  # ##################################
  # Import and clean DB
  # ##################################

  # # import CSmosquitoSp DB
  # db <- read.table(file=normalizePath(path=input_db, winslash="/", mustWork=T), header=T, sep=";", dec=".")
  db <- input_db
  db_type <- comment(input_db)
  input_db$date  <- as.POSIXct(as.character(db$ts), tz="Europe/Rome", origin = "1970-01-01")
  
  # process CSmosquitoSp DataBase of type 'ZanzaMapp'
  if(db_type == 'ZanzaMapp'){
    # check if 'input_db' is a valid standard CSmosquitoSp DataBase
    db_col_check <- c("locationid","ts","lat","lng","val","deviceid","tilex","tiley","time","place","bites","devicelat","devicelon","devicetype")
    if(sum(as.integer(names(db) == db_col_check)) < 14)
      stop("Argument 'input_db' is not a valid standard CSmosquitoSp DataBase")
    
    # add more explicit date information
    db$longitude <- as.numeric(db$lng)
    db$latitude <- as.numeric(db$lat)
    db$date  <- as.POSIXct(as.character(db$ts), tz="Europe/Rome", origin = "1970-01-01")
    db$year  <- as.numeric(strftime(db$date,format = "%Y" ))
    db$month <- as.numeric(strftime(db$date,format = "%m" ))
    db$week  <- as.numeric(strftime(db$date,format = "%W" ))
    db$jday  <- as.numeric(strftime(db$date,format = "%j" ))
    db$oraH  <- as.numeric(strftime(db$date, format = "%H"))
    db$weekday  <- as.numeric(strftime(db$date, format = "%w"))
    # convert ts column (string) to date
    #db$datetime <- strptime(db$ts, format="%d/%m/%Y %H:%M", tz="Europe/Rome")
    # convert to POSIX
    #db$posix <- as.numeric(strptime(db$ts, format="%d/%m/%Y %H:%M", tz="Europe/Rome"))
    db$posix <- as.numeric(as.POSIXct(as.character(db$ts), tz="Europe/Rome"))
    db$yearM <- paste(db$year,db$month,sep="-")
    db$yearW <- paste(db$year,db$week,sep="-")
    
    # create a new coding 'RC' for reported mosquito number
    db$RC <- db$val
    db$RC[db$RC==30] <- 3
    db$RC[db$RC==11] <- 2
    
    # create a new coding 'CM' for reported mosquito number
    db$CM <- db$val
    db$CM[db$CM==1] <- 2
    db$CM[db$CM==30] <- 34
    db$CM[db$CM==11] <- 17
    
    # create a field for the values to be used
    if(method== "RC"){
      db$value <- db$RC
      outmethod <- "RC (ReClass)"
    }
    if(method == "CM"){
      db$value <- db$CM
      outmethod <- "CM (Class Median)"
    }
    
    # create a quality flag for reported mosquito
    db$qf_val <- db$val
    db$qf_val[db$qf_val>0] <- 1
    
    # create quality flag for device coordinates existence
    db$qf_coord <- rep(1, nrow(db))
    missing_device_coordinates <- which(is.na(db$devicelat) | is.na(db$devicelon))
    missing_coordinates <- which(is.na(db$lat) | is.na(db$lng)) #there are no missing coordinates
    no_full_coordinates <- which(is.na(db$devicelat) | is.na(db$devicelon) | is.na(db$lat) | is.na(db$lng))
    db$qf_coord[missing_device_coordinates] <- 0
    
    # create backup of the input db
    db_bkp <- db
    
    # ##################################
    # Subset the data
    # ##################################
    
    # ##################################
    # create spatial subset
    db <- droplevels(subset(db, lng > as.numeric(as.character(aoi_extent$xmin[which(aoi_extent$aoi == aoi)])) &
                              lng < as.numeric(as.character(aoi_extent$xmax[which(aoi_extent$aoi == aoi)])) &
                              lat > as.numeric(as.character(aoi_extent$ymin[which(aoi_extent$aoi == aoi)])) &
                              lat < as.numeric(as.character(aoi_extent$ymax[which(aoi_extent$aoi == aoi)])) ))
    
    # ##################################
    # create temporal subset
    # db <- droplevels(subset(db, date < "2016-12-01 00:00:00 GMT"))
    db <- droplevels(subset(db, date >= paste(as.character(start_date), "00:00:00", "CEST", sep=" ") & date <= paste(as.character(end_date), "23:59:59", "CEST", sep=" ")))
    
    # check if DB still contains valid reports
    if(nrow(db) == 0)
      stop(paste("There are no available reports for selected AOI and temporal range after DataBase cleaning and subsetting.\nConsider the use of another AOI or a different temporal range.\nInput DB has temporal range: ", strftime(min(input_db$date), format = "%Y-%m-%d"), " - ", strftime(max(input_db$date),format = "%Y-%m-%d"), sep=""))
    
    # ##################################
    # Clean the data
    # ##################################
    
    # ##################################
    # Spatial cleaning
    
    # replace default coordinates if device coordinates are different
    default_coord_list <- which(db$lat == 41.89963 & db$lng == 12.487030)
    for(l in default_coord_list){
      if(!is.na(db$devicelat[l]) & !is.na(db$devicelon[l])){
        db$lat[l] <- as.double(db$devicelat[l])
        db$lng[l] <- as.double(db$devicelon[l])
      }
    }
    
    # remove all points with duplicated coordinates ('lat' and 'lng')
    suppressWarnings(db$latlng <- as.numeric(paste(20000000000+(as.numeric(db$lat*100000000)), 20000000000+(as.numeric(db$lng*100000000)), sep="")))
    latlng_ll_rle <- rle(db$latlng)
    duplicates_ll_list <- which(db$latlng %in% latlng_ll_rle$values[which(latlng_ll_rle$lengths>1)])
    if(length(duplicates_ll_list > 0)){
      db <- db[-duplicates_ll_list,]
    }
    
    # remove all points with duplicated coordinates ('devicelat' and 'devicelon')
    suppressWarnings(db$devicelatlng <- as.numeric(paste(20000000000+(as.numeric(db$devicelat*100000000)), 20000000000+(as.numeric(db$devicelon*100000000)), sep="")))
    latlng_dll_rle <- rle(db$devicelatlng)
    duplicates_dll_list <- which(db$devicelatlng %in% latlng_dll_rle$values[which(latlng_dll_rle$lengths>1)])
    if(length(duplicates_dll_list > 0)){
      db <- db[-duplicates_dll_list,]
    }
    
    # remove all records with device coordinates far more than 500 meters from coordinates
    if(full_clean){
      max_coord_distance <- 0.005 # set maximum distance in degrees (1km corresponds approximatively to 0.01 degrees)
      coordinates_distance <- as.double(sqrt((db$lng - db$devicelon)^2 + (db$lat - db$devicelat)^2))
      coordinates_discard <- which(coordinates_distance > max_coord_distance)
      if(length(coordinates_discard > 0)){
        db <- db[-coordinates_discard,]
      }
    }
    
    # ##################################
    # Temporal cleaning
    
    # remove all records created by the same user in less than 30 seconds
    if(full_clean){
      max_seconds <- 30 # set minimum time range between two reports from the same device (in seconds)
      # get deviceids with more than one report
      deviceid_multiple_ap <- tapply(db$locationid, db$deviceid, function(x) length(unique(x)))
      deviceid_multiple_ap <- deviceid_multiple_ap[which(deviceid_multiple_ap > 1)]
      deviceid_multiple <- as.vector(sort(as.numeric(names(deviceid_multiple_ap))))
      db$stposix <- rep(0, nrow(db)) # create quality flag for short-time repeated records
      for(d in 1:length(deviceid_multiple)){
        devsig <- droplevels(subset(db, deviceid == deviceid_multiple[d]))
        devsig <- devsig[order(devsig$posix),] # order by date
        for(r in 1:(nrow(devsig)-1)){
          posix_check <- as.integer((devsig$posix[r+1] - devsig$posix[r]) <= max_seconds)
          if(posix_check == 1){
            db$stposix[which(db$locationid == devsig$locationid[r])] <- 1
          }
        }
      }
      # remove records
      posix_discard <- which(db$stposix == 1)
      if(length(posix_discard > 0)){
        db <- db[-posix_discard,]
      }
      # remove unnecessary fields
      db <- within(db, rm("stposix"))
    }
    
    # remove unnecessary fields
    db <- within(db, rm("latlng", "devicelatlng"))
    
    # ##################################
    # check if DB still contains valid reports
    if(nrow(db) == 0)
      stop(paste("There are no available reports for selected AOI and temporal range after DataBase cleaning and subsetting.\nConsider the use of another AOI or a different temporal range.\nInput DB has temporal range: ", strftime(min(input_db$date), format = "%Y-%m-%d"), " - ", strftime(max(input_db$date), format = "%Y-%m-%d"), sep=""))
    
    # ##################################
    # Perform spatial analysis
    # ##################################
    
    # # set path of AOI polygon shapefile
    # poly_path <- normalizePath(path=paste(aux_path, "/", aoi_extent$filename[which(aoi_extent$aoi == aoi)], ".shp", sep=""), winslash="/", mustWork=T)
    # # import polygons with area definition
    # aoi_poly <- rgdal::readOGR(dsn=poly_path, layer=as.character(aoi_extent$filename[which(aoi_extent$aoi == aoi)]))
    if(!externalaoi){
      aoi_poly <- get(as.character(aoi_extent$filename[which(aoi == aoi_extent$aoi)]))
    }
    
    # # use 'sp' package
    # db_spatial <- sp::SpatialPointsDataFrame(coords=data.frame(cbind(db$lng,db$lat)), data=db, proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    # use 'sf' package
    db_spatial <- sf::st_as_sf(db, coords=c("lng","lat"), crs=c("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    
    # perform spatial overlay
    db_overlay_aoi <- suppressMessages(suppressWarnings(sf::st_intersection(db_spatial, aoi_poly)))
    
    # create output
    aoi_data <- aoi_poly
    
    # add CSmosquitoSp DataBase type as attribute field
    aoi_data$DATABASE <- as.character(db_type)
    
    # ##################################
    # Analysis 1: compute average number of mosquitos weighted for the number of devices
    
    # count number of different records and number of different devices for each polygon
    avg_wgt <- plyr::ddply(db_overlay_aoi,c("CODE","value"), plyr::summarize,
                           n = length(value),
                           n_id = length(unique(deviceid))) # it uses the number of devices instead number of reports 'length(unique(locationid))'
    
    # # function for weighted mean
    # val_w <- function(x,y){
    #   sum(x*y)/sum(y)
    # }
    
    # compute weighted average mosquito number for each polygon
    avg_wgt_db <- plyr::ddply(avg_wgt,c("CODE"), plyr::summarize,
                              value    = val_w(value,n),
                              value_id = val_w(value,n_id))
    
    # rescale value between 0 and 1 in polygons
    result_aoi <- data.frame(CODE=as.character(avg_wgt_db$CODE), VAL_WAVG=avg_wgt_db$value_id, VAL_WSAVG=avg_wgt_db$value_id/max(avg_wgt_db$value_id))
    
    # join the results
    aoi_data <- merge(x=aoi_data, y=result_aoi, by="CODE", all.x=TRUE)
    
    # ##################################
    # Analysis 2: compute average number of mosquitoes
    
    # compute average mosquito number for each polygon
    avg_val <- tapply(db_overlay_aoi$value, db_overlay_aoi$CODE, mean)
    # compute perceived mosquitoes fraction abundance
    pmfa <- avg_val/34.0
    
    result_aoi <- data.frame(CODE=c(dimnames(avg_val)[[1]]), VAL_AVG=as.vector(avg_val), VAL_PMFA=as.vector(pmfa))
    
    # join the results
    aoi_data <- sp::merge(x=aoi_data, y=result_aoi, by="CODE", all.x=TRUE)
    
    # ##################################
    # Analysis 3: compute class from number of mosquitos
    
    # create discrete classes from average number (round method is used)
    if(method == "RC"){
      aoi_data$VAL_CLASS <- round(aoi_data$VAL_AVG)
    }
    if(method == "CM"){
      aoi_data$VAL_CLASS <- ifelse(aoi_data$VAL_AVG == 0, 0, ifelse(aoi_data$VAL_AVG > 0 & aoi_data$VAL_AVG <=3, 1, ifelse(aoi_data$VAL_AVG > 3 & aoi_data$VAL_AVG <= 30, 2, ifelse(aoi_data$VAL_AVG > 30, 3, NA) )))
    }
    
    # ##################################
    # Analysis 4: compute standard deviation of number of mosquitos
    
    # compute average mosquito number for each polygon
    sd_val <- tapply(db_overlay_aoi$value, db_overlay_aoi$CODE, sd)
    result_aoi <- data.frame(CODE=c(dimnames(sd_val)[[1]]), VAL_STDEV=as.vector(sd_val))
    
    # join the results
    aoi_data <- sp::merge(x=aoi_data, y=result_aoi, by="CODE", all.x=TRUE)
    
    # compute 'Coefficient or Variation (CV)' also known as 'Relative Standard Deviation (RSD)'
    aoi_data$VAL_CV <- aoi_data$VAL_STDEV / aoi_data$VAL_AVG
    
    # ##################################
    # Analysis 5: compute number of report and percentage of positive records
    
    # compute number of report for each polygon (also daily report number)
    count_val <- tapply(db_overlay_aoi$locationid, db_overlay_aoi$CODE, function(x) length(unique(x)))
    # count percentage of positive records
    count_pos <- tapply(db_overlay_aoi$value, db_overlay_aoi$CODE, function(x) sum(as.integer(x > 0))/length(x)*100)
    
    result_aoi <- data.frame(CODE=c(dimnames(avg_val)[[1]]), POS_REP=as.vector(count_pos), REP_COUNT=as.vector(count_val), REP_DCOUNT=as.vector(as.vector(count_val/number_days)))
    
    # join the results
    aoi_data <- sp::merge(x=aoi_data, y=result_aoi, by="CODE", all.x=TRUE)
    
    # compute ratio between number of report and popoluation (daily)
    aoi_data$REP_DC_POP <- as.double(aoi_data$REP_COUNT / number_days / as.numeric(aoi_data$POPULATION))
    # compute ratio between number of report and area (in square km)
    aoi_data$REP_DC_ARE <- as.double(aoi_data$REP_COUNT / number_days / as.numeric(aoi_data$AREA))
    
    # ##################################
    # Analysis 6: compute number of single devices used for reporting mosquito number
    
    # compute number of report for each polygon (also daily report number)
    count_val <- tapply(db_overlay_aoi$deviceid, db_overlay_aoi$CODE, function(x) length(unique(x)))
    result_aoi <- data.frame(CODE=c(dimnames(avg_val)[[1]]), DEV_COUNT=as.vector(count_val), DEV_DCOUNT=as.vector(as.vector(count_val/number_days)))
    
    # join the results
    aoi_data <- sp::merge(x=aoi_data, y=result_aoi, by="CODE", all.x=TRUE)
    
    # compute ratio between number of devices and popoluation (daily)
    aoi_data$DEV_DC_POP <- as.double(aoi_data$DEV_COUNT / number_days / as.numeric(aoi_data$POPULATION))
    
    # compute ratio between number of devices and area (in square km)
    aoi_data$DEV_DC_ARE <- as.double(aoi_data$DEV_COUNT / number_days / as.numeric(aoi_data$AREA))
    
    # ##################################
    # Analysis 7: compute number of mosquito reported during DayTime
    
    # create subset for Daytime reports
    db_overlay_df <- as.data.frame(db_overlay_aoi)
    db_daytime <- droplevels(subset(db_overlay_df, time == "m" | time == "p" | (time == "o" & oraH > 6 & oraH <= 20)))
    
    # create quality flag of biting mosquito during daytime
    db_spatial$qf_daytime <- rep(0, nrow(db_spatial))
    db_spatial$qf_daytime[which(db_spatial$locationid %in% db_daytime$locationid)] <- rep(1, nrow(db_daytime))
    
    if(nrow(db_daytime) > 0){
      # compute average daytime mosquito number for each polygon
      daytime_avg_val <- tapply(db_daytime$value, db_daytime$CODE, mean)
      result_daytime_avg_val <- data.frame(CODE=c(dimnames(daytime_avg_val)[[1]]), DT_V_AVG=as.vector(daytime_avg_val))
      # create discrete classes from daytime mosquito average number (round method is used)
      if(method == "RC"){
        result_daytime_avg_val$DT_V_CLASS <- as.integer(round(result_daytime_avg_val$DT_V_AVG))
      }
      if(method == "CM"){
        result_daytime_avg_val$DT_V_CLASS <- as.integer(ifelse(result_daytime_avg_val$DT_V_AVG == 0, 0, ifelse(result_daytime_avg_val$DT_V_AVG > 0 & result_daytime_avg_val$DT_V_AVG <=3, 1, ifelse(result_daytime_avg_val$DT_V_AVG > 3 & result_daytime_avg_val$DT_V_AVG <= 30, 2, ifelse(result_daytime_avg_val$DT_V_AVG > 30, 3, NA) ))))
      }
      
      # compute weighted number of daytime mosquito for each polygon
      # count number of different records and number of different devices for each polygon
      daytime_avg_wgt <- plyr::ddply(db_daytime,c("CODE","value"), plyr::summarize,
                                n = length(value),
                                n_id = length(unique(deviceid))) # it uses the number of devices instead number of reports 'length(unique(locationid))'
      
      # compute weighted average mosquito number for each polygon
      daytime_avg_wgt_db <- plyr::ddply(daytime_avg_wgt,c("CODE"), plyr::summarize,
                                   value    = val_w(value,n),
                                   value_id = val_w(value,n_id))
      
      # rescale value between 0 and 1 in polygons
      result_daytime_avg_val$DT_V_WAVG <- as.vector(daytime_avg_wgt_db$value_id)
      result_daytime_avg_val$DT_V_WSAVG <- as.vector(daytime_avg_wgt_db$value_id/max(daytime_avg_wgt_db$value_id))
      
      # join the results
      aoi_data <- sp::merge(x=aoi_data, y=result_daytime_avg_val, by="CODE", all.x=TRUE)
    } else {
      aoi_data$DT_V_AVG <- rep(NA, length(aoi_data$CODE))
      aoi_data$DT_V_CLASS <- rep(NA, length(aoi_data$CODE))
      aoi_data$DT_V_WAVG <- rep(NA, length(aoi_data$CODE))
      aoi_data$DT_V_WSAVG <- rep(NA, length(aoi_data$CODE))
    }
    
    # ##################################
    # Analysis 8: compute number of biting mosquito reported during DayTime
    
    # create subset of biting daytime
    db_daytime_bites <- droplevels(subset(db_daytime, bites > 0))
    
    # create quality flag of biting daytime
    db_spatial$qf_daytime_b <- rep(0, nrow(db_spatial))
    db_spatial$qf_daytime_b[which(db_spatial$locationid %in% db_daytime_bites$locationid)] <- rep(1, nrow(db_daytime_bites))
    
    if(nrow(db_daytime_bites) > 0){
      # compute average daytime mosquito number for each polygon
      daytime_bites_avg_val <- tapply(db_daytime_bites$value, db_daytime_bites$CODE, mean)
      result_daytime_bites_avg_val <- data.frame(CODE=c(dimnames(daytime_bites_avg_val)[[1]]), DT_B_AVG=as.vector(daytime_bites_avg_val))
      # create discrete classes from daytime biting mosquito average number (round method is used)
      if(method == "RC"){
        result_daytime_bites_avg_val$DT_B_CLASS <- as.integer(round(result_daytime_bites_avg_val$DT_B_AVG))
      }
      if(method == "CM"){
        result_daytime_bites_avg_val$DT_B_CLASS <- as.integer(ifelse(result_daytime_bites_avg_val$DT_B_AVG == 0, 0, ifelse(result_daytime_bites_avg_val$DT_B_AVG > 0 & result_daytime_bites_avg_val$DT_B_AVG <=3, 1, ifelse(result_daytime_bites_avg_val$DT_B_AVG > 3 & result_daytime_bites_avg_val$DT_B_AVG <= 30, 2, ifelse(result_daytime_bites_avg_val$DT_B_AVG > 30, 3, NA) ))))
      }
      
      # compute weighted number of biting daytime mosquito for each polygon
      # count number of different records and number of different devices for each polygon
      daytimeb_avg_wgt <- plyr::ddply(db_daytime_bites,c("CODE","value"), plyr::summarize,
                                 n = length(value),
                                 n_id = length(unique(deviceid))) # it uses the number of devices instead number of reports 'length(unique(locationid))'
      
      # compute weighted average mosquito number for each polygon
      daytimeb_avg_wgt_db <- plyr::ddply(daytimeb_avg_wgt,c("CODE"), plyr::summarize,
                                    value    = val_w(value,n),
                                    value_id = val_w(value,n_id))
      
      # rescale value between 0 and 1 in polygons
      result_daytime_bites_avg_val$DT_B_WAVG <- as.vector(daytimeb_avg_wgt_db$value_id)
      result_daytime_bites_avg_val$DT_B_WSAVG <- as.vector(daytimeb_avg_wgt_db$value_id/max(daytimeb_avg_wgt_db$value_id))
      
      # join the results
      aoi_data <- sp::merge(x=aoi_data, y=result_daytime_bites_avg_val, by="CODE", all.x=TRUE)
    } else {
      aoi_data$DT_B_AVG <- rep(NA, length(aoi_data$CODE))
      aoi_data$DT_B_CLASS <- rep(NA, length(aoi_data$CODE))
      aoi_data$DT_B_WAVG <- rep(NA, length(aoi_data$CODE))
      aoi_data$DT_B_WSAVG <- rep(NA, length(aoi_data$CODE))
    }
    
    ###################################
    # Analysis 9: compute number of mosquito reported during NightTime
    
    # create subset for nighttime
    db_overlay_df <- as.data.frame(db_overlay_aoi)
    db_nighttime <- droplevels(subset(db_overlay_df, time == "n" | (time == "o" & oraH > 20 & oraH <= 6)))
    
    # create quality flag of biting nighttime
    db_spatial$qf_nighttime <- rep(0, nrow(db_spatial))
    db_spatial$qf_nighttime[which(db_spatial$locationid %in% db_nighttime$locationid)] <- rep(1, nrow(db_nighttime))
    
    if(nrow(db_nighttime) > 0){
      # compute average nighttime mosquito number for each polygon
      nighttime_avg_val <- tapply(db_nighttime$value, db_nighttime$CODE, mean)
      result_nighttime_avg_val <- data.frame(CODE=c(dimnames(nighttime_avg_val)[[1]]), NT_V_AVG=as.vector(nighttime_avg_val))
      # create discrete classes from nighttime mosquito average number (round method is used)
      if(method == "RC"){
        result_nighttime_avg_val$NT_V_CLASS <- as.integer(round(result_nighttime_avg_val$NT_V_AVG))
      }
      if(method == "CM"){
        result_nighttime_avg_val$NT_V_CLASS <- as.integer(ifelse(result_nighttime_avg_val$NT_V_AVG == 0, 0, ifelse(result_nighttime_avg_val$NT_V_AVG > 0 & result_nighttime_avg_val$NT_V_AVG <=3, 1, ifelse(result_nighttime_avg_val$NT_V_AVG > 3 & result_nighttime_avg_val$NT_V_AVG <= 30, 2, ifelse(result_nighttime_avg_val$NT_V_AVG > 30, 3, NA) ))))
      }
      
      # compute weighted number of nighttime mosquito for each polygon
      # count number of different records and number of different devices for each polygon
      nighttime_avg_wgt <- plyr::ddply(db_nighttime,c("CODE","value"), plyr::summarize,
                                n = length(value),
                                n_id = length(unique(deviceid))) # it uses the number of devices instead number of reports 'length(unique(locationid))'
      
      # compute weighted average mosquito number for each polygon
      nighttime_avg_wgt_db <- plyr::ddply(nighttime_avg_wgt,c("CODE"), plyr::summarize,
                                   value    = val_w(value,n),
                                   value_id = val_w(value,n_id))
      
      # rescale value between 0 and 1 in polygons
      result_nighttime_avg_val$NT_V_WAVG <- as.vector(nighttime_avg_wgt_db$value_id)
      result_nighttime_avg_val$NT_V_WSAVG <- as.vector(nighttime_avg_wgt_db$value_id/max(nighttime_avg_wgt_db$value_id))
      
      # join the results
      aoi_data <- sp::merge(x=aoi_data, y=result_nighttime_avg_val, by="CODE", all.x=TRUE)
    } else {
      aoi_data$NT_V_AVG <- rep(NA, length(aoi_data$CODE))
      aoi_data$NT_V_CLASS <- rep(NA, length(aoi_data$CODE))
      aoi_data$NT_V_WAVG <- rep(NA, length(aoi_data$CODE))
      aoi_data$NT_V_WSAVG <- rep(NA, length(aoi_data$CODE))
    }
    
    # ##################################
    # Analysis 10: compute number of biting mosquito reported during NightTime
    
    # create subset of biting nighttime
    db_nighttime_bites <- droplevels(subset(db_nighttime, bites > 0))
    
    # create quality flag of biting nighttime
    db_spatial$qf_nighttime_b <- rep(0, nrow(db_spatial))
    db_spatial$qf_nighttime_b[which(db_spatial$locationid %in% db_nighttime_bites$locationid)] <- rep(1, nrow(db_nighttime_bites))
    
    if(nrow(db_nighttime_bites) > 0){
      # compute average nighttime mosquito number for each polygon
      nighttime_bites_avg_val <- tapply(db_nighttime_bites$value, db_nighttime_bites$CODE, mean)
      result_nighttime_bites_avg_val <- data.frame(CODE=c(dimnames(nighttime_bites_avg_val)[[1]]), NT_B_AVG=as.vector(nighttime_bites_avg_val))
      # compute nighttime mosquito classes
      # create discrete classes from nighttime mosquito average number (round method is used)
      if(method == "RC"){
        result_nighttime_bites_avg_val$NT_B_CLASS <- as.integer(round(result_nighttime_bites_avg_val$NT_B_AVG))
      }
      if(method == "CM"){
        result_nighttime_bites_avg_val$NT_B_CLASS <- as.integer(ifelse(result_nighttime_bites_avg_val$NT_B_AVG == 0, 0, ifelse(result_nighttime_bites_avg_val$NT_B_AVG > 0 & result_nighttime_bites_avg_val$NT_B_AVG <=3, 1, ifelse(result_nighttime_bites_avg_val$NT_B_AVG > 3 & result_nighttime_bites_avg_val$NT_B_AVG <= 30, 2, ifelse(result_nighttime_bites_avg_val$NT_B_AVG > 30, 3, NA) ))))
      }
      
      # compute weighted number of nighttime mosquito for each polygon
      # count number of different records and number of different devices for each polygon
      nighttimeb_avg_wgt <- plyr::ddply(db_nighttime_bites,c("CODE","value"), plyr::summarize,
                                 n = length(value),
                                 n_id = length(unique(deviceid))) # it uses the number of devices instead number of reports 'length(unique(locationid))'
      
      # compute weighted average mosquito number for each polygon
      nighttimeb_avg_wgt_db <- plyr::ddply(nighttimeb_avg_wgt,c("CODE"), plyr::summarize,
                                    value    = val_w(value,n),
                                    value_id = val_w(value,n_id))
      
      # rescale value between 0 and 1 in polygons
      result_nighttime_bites_avg_val$NT_B_WAVG <- as.vector(nighttimeb_avg_wgt_db$value_id)
      result_nighttime_bites_avg_val$NT_B_WSAVG <- as.vector(nighttimeb_avg_wgt_db$value_id/max(nighttimeb_avg_wgt_db$value_id))
      
      # join the results
      aoi_data <- sp::merge(x=aoi_data, y=result_nighttime_bites_avg_val, by="CODE", all.x=TRUE)
    } else {
      aoi_data$NT_B_AVG <- rep(NA, length(aoi_data$CODE))
      aoi_data$NT_B_CLASS <- rep(NA, length(aoi_data$CODE))
      aoi_data$NT_B_WAVG <- rep(NA, length(aoi_data$CODE))
      aoi_data$NT_B_WSAVG <- rep(NA, length(aoi_data$CODE))
    }
    
    # ##################################
    # Analysis 11:
    
    # ### put here your analysis
    
    # ##################################
    # Output function results
    result_CSmosquitoSp <- new("CSmosquitoSp.obj")
    result_CSmosquitoSp@DB <- as.character(db_type)
    result_CSmosquitoSp@points <- db_spatial
    result_CSmosquitoSp@aoi <- aoi_data
    result_CSmosquitoSp@aoi_name <- as.character(aoi)
    result_CSmosquitoSp@start_date <- as.Date(start_date)
    result_CSmosquitoSp@end_date <- as.Date(end_date)
    result_CSmosquitoSp@method <- outmethod
    return(result_CSmosquitoSp)
    
  } else {
   stop("Currently only CSmosquitoSp DataBase of type 'ZanzaMapp' is supported") 
  }
}
