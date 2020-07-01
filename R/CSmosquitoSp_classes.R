# Title         : Classes for the CSmosquitoSp package
# Description   : Define classes for the CSmosquitoSp package
# Date          : Jul 2020
# Version       : 0.5
# Licence       : GPL v3
# Authors       : Federico Filipponi
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>

# ######################################################################
#
#' Class "CSmosquitoSp.obj"
#'
#' @name CSmosquitoSp.obj
#' @aliases CSmosquitoSp.obj-class
#' @description Result from \code{\link[CSmosquitoSp]{CSmosquitoSp.analysis}} stored as a \code{\linkS4class{CSmosquitoSp.obj}} object.
#' Slots for \code{CSmosquitoSp.obj} objects include: (1) the reported points from the mosquito DataBase;
#' (2) the AOI spatial polygons containing results from the spatial analysis of the mosquito DataBase.
#' @docType class
#' @rdname CSmosquitoSp-class
#' @section Objects from the class: Objects are created by calls to \code{\link[CSmosquitoSp]{CSmosquitoSp.analysis}}.
#'
#' @seealso \code{\link[CSmosquitoSp]{CSmosquitoSp.analysis}} \code{\link[CSmosquitoSp]{CSmosquitoSp.plot}} \code{\link[CSmosquitoSp]{CSmosquitoSp.export}}
#'
#' @slot DB Character. Type of the imported Citizen Science mosquito DataBase
#' @slot points SpatialPointsDataFrame. Report spatial points used for the analysis
#' @slot aoi SpatialPolygonsDataFrame. Spatial polygons of the selected Area Of Interest containing the results from the spatial analysis
#' @slot aoi_name Character. Name of the selected Area Of Interest
#' @slot start_date Date. Start day selected for the analysis
#' @slot end_date Date. End day selected for the analysis
#' @slot method Character. Method used for the analysis (currently 'RC' and 'CM' are supported)
#' 
#' @author Federico Filipponi
#'
#' @exportClass CSmosquitoSp.obj

setClass("CSmosquitoSp.obj",
         slots=c(DB="character",
                        points="sf",
                        aoi="sf",
                        aoi_name="character",
                        start_date="Date",
                        end_date="Date",
                        method="character"),
                        prototype=prototype(DB=NULL, points=NULL, aoi=NULL, aoi_name=NULL, start_date=NULL, end_date=NULL, method=NULL)
         )
