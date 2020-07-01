# Title         : CSmosquitoSp plot
# Description   : Create maps from spatial analysis results of CSmosquitoSp dataset for a specific Area Of Interest
# Date          : Jul 2020
# Version       : 0.5
# Licence       : GPL v3
# Authors       : Federico Filipponi
# Maintainer    : Federico Filipponi <federico.filipponi@gmail.com>

# ######################################################################
#' @title CSmosquitoSp plot
#'
#' @description This function create spatial maps for a selected AOI using the results
#' generated from spatial analysis of CSmosquitoSp data
#'
#' @param x Input object of class \code{\linkS4class{CSmosquitoSp.obj}}
#' @param field Character. Fields to be mapped from the 'aoi' slot of object of class \code{\linkS4class{CSmosquitoSp.obj}}
#' @param maptitle Character. 'Map title.
#' @param lang Character. Language to use for map footer ('ENG' or 'ITA').
#' @param filename Character. Plot result to speified file path.
#' @param caption_logo Logical. Add the caption logo (currently available only for 'ZanzaMapp' DataBase) to the map (Default to FALSE).
#' @param ... Additional arguments to be passed through to function \code{\link{CSmosquitoSp.plot}}
#'
#' @return
#' The function return a plot for the selected field computed from \code{\link[CSmosquitoSp]{CSmosquitoSp.analysis}} within the Area Of Interest
#'
#' @author Federico Filipponi
#'
#' @keywords CSmosquitoSp plot
#'
#' @seealso \code{\link[CSmosquitoSp]{CSmosquitoSp.analysis}} \code{\link[CSmosquitoSp]{CSmosquitoSp.export}}
#'
#' @examples
#' \dontrun{
#' # perform CSmosquitoSp spatial analysis
#' for the metropolitan area of the city of Rome
#' CSMS_roma <- CSmosquitoSp.analysis(input_db=input_db, aux_path=aux_path,
#' aoi="Roma", start_date="2017-09-01", end_date="2017-09-30")
#'
#' # plot result
#' CSmosquitoSp.plot(x=CSMS_roma, field="VAL_PMFA")
#' }
#'
#' @import sf
#' @import methods
#' @import ggplot2
#' @import ggpubr
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_fill_distiller
#' @importFrom ggpubr ggarrange
#' @export

# define 'CSmosquitoSp.plot' function
CSmosquitoSp.plot <- function(x, field="VAL_PMFA", maptitle="Perceived mosquito abundance", lang="ENG", filename=NULL, caption_logo=FALSE, ...){

  # ##################################
  # check input arguments

  # log_trans = FALSE
  
  # check if output path exists
  if(!is.null(filename)){
    filename <- normalizePath(filename, winslash="/", mustWork=FALSE)
    if(!file.exists(dirname(filename))){
      dir.create(dirname(filename))
    }
  }

  # check if 'x' argument is of class 'CSmosquitoSp.obj'
  if(!(class(x) %in% c("CSmosquitoSp.obj")))
    stop("'x' argument must be one an object of class 'CSmosquitoSp.obj'\ngenerated using function CSmosquitoSp.analysis()")

  # check if 'field' argument is supported
  if(!(field %in% names(x@aoi)))
    stop("'field' argument must be a field name of the 'aoi' slot in object 'CSmosquitoSp.obj'")
  # if(!(field %in% c("VAL_PMFA", "REP_DC_POP")))
  #   warning("'field' argument should be one of the following in order to generate standard plots:\n* 'VAL_AVG'\n* 'DT_V_AVG'")
  
  # load data from the package
  # data(aoi_extent)
  # aoi_extent <- CSmosquitoSp:::aoi_extent
  
  # set external-aoi variable
  external_aoi <- FALSE
  
  # set variables for external aoi
  if(!(x@aoi_name %in% aoi_extent$aoi)){
    external_aoi <- TRUE
    aoi_extent <- as.data.frame(rbind(as.matrix(aoi_extent), c(x@aoi_name, x@aoi_name, "aoi_poly", x@aoi_name, x@aoi_name, as.double(st_bbox(x@aoi)[1]), as.double(st_bbox(x@aoi)[3]), as.double(st_bbox(x@aoi)[2]), as.double(st_bbox(x@aoi)[4]), as.double(0.05), as.double(0.05))))
  }

  # ##################################
  # get lines for overlay
  aoi_line_name <- as.character(aoi_extent$contour_filename[which(aoi_extent$aoi == x@aoi_name)])
  if(external_aoi){
    aoi_line <- x@aoi
  } else {  
    aoi_line <- get(aoi_line_name)
  }

  # # set default column number
  # df_colnum <- 21
  # set column number
  df_colnum <- as.integer(which(names(x@aoi) == field))

  # ##################################
  # set parameters based on selected 'field'
  
  if(!(field %in% names(x@aoi))){
    stop("'field' argument must be a field name of the 'aoi' slot in object 'CSmosquitoSp.obj'")
  } else {
    if(!is.numeric(as.data.frame(x@aoi[,df_colnum])[,1]))
      stop("'field' argument must be a numeric field of the 'aoi' slot in object 'CSmosquitoSp.obj'")
    
    # set color palette
    scale_palette <- "YlOrRd"
    
    # convert numeric values to class values for plotting
    x@aoi$Legenda <- as.numeric(as.data.frame(x@aoi)[,df_colnum])
    # names(x@aoi)[which(names(x@aoi) == "Legenda")] <- field ### il nome viene duplicato
  }
  if(field == "VAL_PMFA"){
    # set title and color rules
    maptitle <- c("Perceived mosquito abundance")
    scale_palette <- "YlOrRd"
  }
  if(field == "REP_DC_POP"){
    # set title and color rules
    maptitle <- c("Sampling effort")
    scale_palette <- "Greens"
    # log_trans = TRUE
    # rescaled_values <- c(0,0.00000001,0.0000001,0.000001,0.00001,0.0001)
  }
  
  # # set log transform
  # if(log_trans == TRUE){
  #   # x@aoi[,df_colnum] <- as.vector(log10(st_drop_geometry(x@aoi)[,df_colnum]))
  #   field_perc <- as.vector(quantile(st_drop_geometry(x@aoi)[,df_colnum], probs = c(0.01, 0.99), na.rm=TRUE))
  #   rescaled_values <- scales::rescale(10^(seq(field_perc[1], field_perc[2], length.out = 6)))
  # }

  # #######
  # set plot parameters

  # set plot footer
  if(lang == "ITA"){
    ref_period_text <- "Periodo di riferimento"
    if(maptitle == "Perceived mosquito abundance"){
      maptitle <- "Abbondanza di zanzare percepito"
    }
  }
  if(lang == "ENG"){
    ref_period_text <- "Reference period"
  }
  
  # set plot subtitles
  map_subtitle <- as.character(aoi_extent$description[which(aoi_extent$aoi == x@aoi_name)])
  caption_text <- paste(ref_period_text , ": ", sprintf("%02i", as.numeric(strftime(x@start_date,format = "%d" ))), "/", sprintf("%02i", as.numeric(strftime(x@start_date,format = "%m" ))), "/", as.numeric(strftime(x@start_date,format = "%Y" )),
                   " - ", sprintf("%02i", as.numeric(strftime(x@end_date,format = "%d" ))), "/", sprintf("%02i", as.numeric(strftime(x@end_date,format = "%m" ))), "/", as.numeric(strftime(x@end_date,format = "%Y" )), sep="")
  polygon_lwd <- as.double(as.character(aoi_extent$lwd[which(aoi_extent$aoi == x@aoi_name)]))
  contour_lwd <- as.double(as.character(aoi_extent$lwd_overlay[which(aoi_extent$aoi == x@aoi_name)]))

  # #######
  # create plot object
  zm_plot <- ggplot2::ggplot() +
    # mapTheme() +
    ggplot2::theme(
      text = ggplot2::element_text(color = "black"),
      plot.title = ggplot2::element_text(size = 18, color = "black"),
      plot.subtitle = ggplot2::element_text(size = 14, color = "black"),
      plot.caption = ggplot2::element_text(face="italic", size=10),
      axis.text.x = ggplot2::element_text(size=10),
      axis.text.y = ggplot2::element_text(angle=90, size=10),
      panel.background = ggplot2::element_rect(fill="transparent", color="black"),
      panel.grid.major = ggplot2::element_line(size=0.5, color="#343434", linetype=3),
      panel.border = ggplot2::element_rect(color = "black", fill=NA, size=1.0),
      panel.ontop = TRUE
    ) +
    ggplot2::xlim(c(as.double(as.character(aoi_extent$xmin[which(aoi_extent$aoi == x@aoi_name)])), as.double(as.character(aoi_extent$xmax[which(aoi_extent$aoi == x@aoi_name)])))) +
    ggplot2::ylim(c(as.double(as.character(aoi_extent$ymin[which(aoi_extent$aoi == x@aoi_name)])), as.double(as.character(aoi_extent$ymax[which(aoi_extent$aoi == x@aoi_name)])))) +
    ggplot2::geom_sf(data=x@aoi, aes(fill=Legenda), color="black", size=polygon_lwd) +
    # scale_fill_manual(values=color_rules, drop=FALSE) +
    ggplot2::geom_sf(data=aoi_line, fill=NA, color="black", size=contour_lwd) +
    ggplot2::labs(title=maptitle, subtitle=map_subtitle, caption=caption_text)

  # apply color ramp
  zm_plot <- zm_plot + ggplot2::labs(fill=field) + ggplot2::scale_fill_distiller(palette=scale_palette, direction=1, na.value="#f2f2f2")
  # zm_plot <- zm_plot + ggplot2::labs(fill=field) + ggplot2::scale_fill_distiller(palette=scale_palette, direction=1, values=rescaled_values, na.value="#f2f2f2")
  
  # add caption logo image
  if(caption_logo){
    if(x@DB == "ZanzaMapp"){
      # get map caption image
      map_caption <- get("map_caption")
      # plot with caption image
      map_caption_plot <- plot(map_caption) + ggpubr::bgcolor("white")
      z_plot <- ggpubr::ggarrange(zm_plot, map_caption_plot, ncol=1, nrow=2, heights=c(1, 0.3))
    } else {
      z_plot <- zm_plot
    }
  } else {
    z_plot <- zm_plot
  }

  # generate plot
  if(!is.null(filename)){
    png(file=filename, units="px", res=300, width=1800, height=1800, bg="white", pointsize=6)
    # # using 'sp' package
    # print(sp::spplot(x@aoi, zcol=field, main=paste(aoi_extent$description[which(aoi_extent$aoi == x@aoi_name)], "\nNumero medio di zanzare", sep=""), sub=sub_title,
    #              col.regions=zanza.palette, cuts=5, at=breaks, scales=list(draw=TRUE, tck=c(1,0)), col="grey26", lwd=0.5))
    # using 'sf' package
    print(z_plot)
    dev.off()
  } else {
    # # using 'sp' package
    # print(sp::spplot(x@aoi, zcol=field, main=paste(aoi_extent$description[which(aoi_extent$aoi == x@aoi_name)], "\nNumero medio di zanzare", sep=""), sub=sub_title,
    #                    col.regions=zanza.palette, cuts=5, at=breaks, scales=list(draw=TRUE, tck=c(1,0)), col="grey26", lwd=0.5))
    # using 'sf' package
    print(z_plot)
    }

}
