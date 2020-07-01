# Title         : Methods for the CSmosquitoSp package
# Description   : Define methods for the CSmosquitoSp package
# Date          : Jul 2020
# Version       : 0.5
# Licence       : GPL v3
# Authors       : Federico Filipponi
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>

# ######################################################################

#' @title CSmosquitoSp methods
#' @description This function define methods for the CSmosquitoSp package for class \code{\linkS4class{CSmosquitoSp.obj}}
#' a specific Area Of Interest and a selected temporal range.
#' 
#' @name show
#' @aliases show show,CSmosquitoSp.obj-method
#' @rdname CSmosquitoSp_methods
#' 
#' @param object CSmosquitoSp.obj. Object of class \code{\linkS4class{CSmosquitoSp.obj}}
#' 
#' @author Federico Filipponi
#' 
#' @importFrom utils data
#' @importFrom utils str
#' @exportMethod show

# create function to show object of class 'CSmosquitoSp.obj'
setMethod('show', signature(object='CSmosquitoSp.obj'),
          function(object){
            cat("CSmosquitoSp object of class 'CSmosquitoSp.obj'", '\n')
            cat('\n')
            cat('DataBase type:',  as.character(object@DB), '\n')
            cat('\n')
            cat('Number of spatial points:',  nrow(object@points), '\n')
            cat('Start date:\t', as.character(object@start_date), '\n')
            cat('End date:\t', as.character(object@end_date), '\n')
            cat('Method:\t', as.character(object@method), '\n')
            cat('\n')
            cat('Spatial polygon information', '\n')
            cat('\n')
            cat('Number of polygons:', length(object@aoi), '\n')
            cat('AOI:\t\t', object@aoi_name,'\n')
            cat('Spatial Bounding Box:', '\n')
            cat(show(st_bbox(object@aoi)))
            cat(show(st_crs(object@aoi)), '\n')
            cat('\n')
            cat('Spatial polygon data.frame has',  nrow(object@aoi), 'records', '\n')
            cat(str(object@aoi), '\n')
          }
)
