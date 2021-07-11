#' @name ggheatmap_plotlist
#' @title Show ggheatmap Plot List
#' @description Display the basic elements of the ggheatmap
#' @param ggheatmap heatmap, the result of ggheatmap
#'
#' @return plotlist
#' @example
#' #Please see examples in \link{ggheatmap}
#' @export

ggheatmap_plotlist <- function(ggheatmap){
  do.call('wrap_plots',ggheatmap[[1]])+
    plot_annotation(tag_levels = list(paste("plotlist",1:length(ggheatmap[[1]]))))
  }
