# Title         : CSmosquitoSp import
# Description   : Import CSmosquitoSp DataBase
# Date          : Jul 2020
# Version       : 0.5
# Licence       : GPL v3
# Authors       : Federico Filipponi
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>

# ######################################################################
#' @title CSmosquitoSp import
#'
#' @description This function import CSmosquitoSp DataBase to be used for spatial analysis using function \code{\link[CSmosquitoSp]{CSmosquitoSp.analysis}}
#'
#' @param x Character. Input path where CSmosquitoSp DataBase in CSV format is stored on disk.
#' @param type Character. Type of the imported Citizen Science mosquito DataBase (at present only 'ZanzaMapp' type is supported)
#'
#' @return
#' The function return a DataFrame containing the CSmosquitoSp DataBase
#'
#' @author Federico Filipponi
#'
#' @keywords CSmosquitoSp import
#'
#' @seealso \code{\link[CSmosquitoSp]{CSmosquitoSp.analysis}} \code{\link[CSmosquitoSp]{CSmosquitoSp.plot}} \code{\link[CSmosquitoSp]{CSmosquitoSp.export}}
#'
#' @examples
#' \dontrun{
#' # set input path
#' input_path <- normalizePath(path=system.file("external/CSmosquitoSp_database.csv"),
#' winslash="/", mustWork=TRUE)
#' # import mosquito DataBase of type 'ZanzaMapp'
#' input_db <- CSmosquitoSp.import(x=input_path, type="ZanzaMapp")
#'
#' ###
#' # directly input mosquito DataBase file path in the function
#' input_db <- CSmosquitoSp.import(x="C:/CSmosquitoSp_database.csv", type="ZanzaMapp")
#' }
#' @importFrom utils read.table
#' @export

# define 'CSmosquitoSp.plot' function
CSmosquitoSp.import <- function(x, type=NULL){

  # ##################################
  # check input arguments

  # check if input path exists
  if(!file.exists(x))
    stop("Input file does not exist on disk")
  
  if(is.null(type))
    stop("Input file type not defined using argument 'type'")
  
  if(type != "ZanzaMapp")
    stop("At present only 'ZanzaMapp' mosquito DataBase is supported")
  
  # import ZanzaMapp DataBase
  if(type == "ZanzaMapp"){
    # check if input file is a standard ZanzaMapp DataBase
    if(readLines(con=x, n=1) != c("locationid,ts,lat,lng,val,deviceid,tilex,tiley,time,place,bites,devicelat,devicelon,devicetype"))
      stop("Input file is not a standard ZanzaMapp DataBase")
    
    # import ZanzaMapp DB
    CSmosquitoSp_db <- read.table(file=normalizePath(path=x, winslash="/", mustWork=T), header=T, sep=",", dec=".")
    comment(CSmosquitoSp_db) <- "ZanzaMapp"
    
    return(CSmosquitoSp_db)
  }
}
