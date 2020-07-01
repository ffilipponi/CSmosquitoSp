#' @title CSmosquitoSp val_w
#' @description Function for weighted mean
#'
#' @param x Numeric. Value x
#' @param y Numeric. Value y
#' 
#' @author Federico Filipponi
#'
#' @export


# define function for weighted mean
val_w <- function(x,y){
  sum(x*y)/sum(y)
}
