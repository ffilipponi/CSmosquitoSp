# Title         : CSmosquitoSp export
# Description   : Export results from spatial analysis of CSmosquitoSp dataset for a specific Area Of Interest and a selected time interval
# Date          : Jul 2020
# Version       : 0.5
# Licence       : GPL v3
# Authors       : Federico Filipponi
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>

# ######################################################################
#' @title CSmosquitoSp export
#'
#' @description This function export results computed using \code{\link[CSmosquitoSp]{CSmosquitoSp.analysis}} function
#' a specific Area Of Interest and a selected temporal range.
#'
#' @param x Input object of class \code{\linkS4class{CSmosquitoSp.obj}}
#' @param output_path Character. Path where maps are saved on disk.
#' @param points Logical. When set to TRUE export points used for the analysis also in 'shapefile' format (default to FALSE).
#' @param kml Logical. When set to TRUE results are also exported in 'kml' format (default to FALSE).
#' @param sensitive Logical. When set to TRUE export the sensitive information in records (default to FALSE).
#'
#' @return
#' The function export the results generated using function \code{\link[CSmosquitoSp]{CSmosquitoSp.analysis}} to disk.
#' Output vector file format are 'shapefile' and 'kml' (optional).
#'
#' @author Federico Filipponi
#'
#' @keywords CSmosquitoSp export
#'
#' @seealso \code{\link[CSmosquitoSp]{CSmosquitoSp.analysis}} \code{\link[CSmosquitoSp]{CSmosquitoSp.plot}}
#'
#' @examples
#' \dontrun{
#' # perform CSmosquitoSp spatial analysis
#' for the metropolitan area of the city of Rome
#' CSMS_roma <- CSmosquitoSp.analysis(input_db=input_db, aux_path=aux_path,
#' aoi="Roma", start_date="2017-09-01", end_date="2017-09-01")
#'
#' # export result
#' CSmosquitoSp.export(x=CSMS_result, output_path="C:/output")
#'
#' # export result also in kml format
#' CSmosquitoSp.export(x=CSMS_result, output_path="C:/output", kml=TRUE)
#'
#' # export also points used for the analysis
#' CSmosquitoSp.export(x=CSMS_result, output_path="C:/output", points=TRUE)
#' }
#'
#' @import sp
#' @import sf
#' @import methods
#' @importFrom utils write.table
#' @importFrom sf st_write
#' @importFrom rgdal writeOGR
#' @importFrom methods new
#' @importFrom methods as
#' @export

# define 'CSmosquitoSp.export' function
CSmosquitoSp.export <- function(x, output_path, points=FALSE, kml=FALSE, sensitive=FALSE){

  # ##################################
  # check input arguments

  # check if output path exists
  if(!is.null(output_path)){
    output_path <- normalizePath(output_path, winslash="/", mustWork=FALSE)
    if(!file.exists(output_path)){
      dir.create(output_path, recursive = T)
    }
  }

  # check if 'x' argument is of class 'CSmosquitoSp.obj'
  if(!(class(x) %in% c("CSmosquitoSp.obj")))
    stop("'x' argument must be one an object of class 'CSmosquitoSp.obj'\ngenerated using function CSmosquitoSp.analysis()")

  # set option to avoid scientific notation
  options(scipen = 20)

  # load data from the package
  # data(aoi_extent)
  aoi_extent <- aoi_extent

  if(!(x@aoi_name %in% aoi_extent$aoi)){
    aoi_fname <- x@aoi_name
  } else {
    aoi_fname <- aoi_extent$aoi[which(aoi_extent$aoi == x@aoi_name)]
  }

  # set basename for output files
  out_basename <- paste(aoi_fname, "_", as.character(x@start_date), "_", as.character(x@end_date), sep="")

  # ####################################
  # Remove sensitive information

  if(!sensitive){
    x@points$deviceid <- NULL
    x@points$devicetype <- NULL
  } else {
    class(x@points$deviceid) <- "character"
  }

  # ####################################
  # Export data

  # to CSV file
  # convert to data frame
  export_df <- st_set_geometry(x@aoi, NULL)
  write.table(export_df, file=normalizePath(path=paste(output_path, "/", out_basename, ".dataset.csv", sep=""), winslash="/", mustWork=F), sep="\t", dec=".", quote=FALSE, col.names=TRUE, row.names=FALSE, na="", fileEncoding="UTF-8")

  # to shapefile
  # # using 'sp' package
  # rgdal::writeOGR(x@aoi, dsn=normalizePath(path=paste(output_path, "/", out_basename, ".data.shp", sep=""), winslash="/", mustWork=F), layer=aoi, driver="ESRI Shapefile")
  # using 'sf' package
  sf::st_write(x@aoi, dsn=normalizePath(path=paste(output_path, "/", out_basename, ".data.shp", sep=""), winslash="/", mustWork=F), driver="ESRI Shapefile", factorsAsCharacter=TRUE, delete_layer=TRUE, quiet=TRUE)

  # write fields description
  # result_legend <- CSmosquitoSp:::result_legend
  write.table(result_legend, file=normalizePath(path=paste(output_path, "/", out_basename, ".dataset_fields", ".csv", sep=""), winslash="/", mustWork=F), sep="\t", dec=".", quote=FALSE, col.names=TRUE, row.names=FALSE)

  # export analysis points
  if(points){
    # convert field types to character in order to avoid warnigs related to long numeric format
    class(x@points$posix) <- "character"
    class(x@points$date) <- "character"

    # save cleaned and subsetted CSmosquitoSp DataBase to CSV
    write.table(x@points, file=normalizePath(path=paste(output_path, "/", out_basename, ".points", ".csv", sep=""), winslash="/", mustWork=F), sep="\t", dec=".", quote=FALSE, col.names=TRUE, row.names=FALSE, na="", fileEncoding="UTF-8")
    # to shapefile
    # # using 'sp' package
    # rgdal::writeOGR(x@points, dsn=normalizePath(path=paste(output_path, "/", out_basename, ".points.shp", sep=""), winslash="/", mustWork=F), layer="points", driver="ESRI Shapefile")
    # using 'sf' package
    sf::st_write(x@points, dsn=normalizePath(path=paste(output_path, "/", out_basename, ".points.shp", sep=""), winslash="/", mustWork=F), driver="ESRI Shapefile", factorsAsCharacter=TRUE, delete_layer=TRUE, quiet=TRUE)

    # export to kml format
    if(kml){
      # to kml file
      # convert from 'sf' to 'Spatial' (sp) object
      export_aoi <- as(x@aoi, "Spatial")
      # export using 'sp' package
      rgdal::writeOGR(export_aoi, dsn=normalizePath(path=paste(output_path, "/", out_basename, ".data.kml", sep=""), winslash="/", mustWork=F), layer=aoi, driver="KML", dataset_options=list("NameField=CODE", "DescriptionField=VAL_PMFA", "AltitudeMode=clampToGround"), overwrite_layer=TRUE)
      # to kml file
      # convert from 'sf' to 'Spatial' (sp) object
      export_points <- as(x@points, "Spatial")
      # export using 'sp' package
      rgdal::writeOGR(export_points, dsn=normalizePath(path=paste(output_path, "/", out_basename, ".points.kml", sep=""), winslash="/", mustWork=F), layer=aoi, driver="KML", dataset_options=list("NameField=CODE", "DescriptionField=VALUE", "AltitudeMode=clampToGround"), overwrite_layer=TRUE)

    }
  }
}
