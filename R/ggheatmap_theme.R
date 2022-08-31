#' @name ggheatmap_theme
#' @title The Theme of ggheatmap
#' @description Set the styles of the drawing elements of each component of the
#'     ggheatmap except cluster tree.
#' @param ggheatmap the result of ggheatmap
#' @param plotlist integer,the plotlist in ggheatmap. Use \link{ggheatmap_plotlist}
#' @param theme list, the theme of plotlist. More detail can see \link{theme}
#'
#' @return ggheatmap
#' @example
#' #Please see examples in \link{ggheatmap}
#' @export

ggheatmap_theme <- function(ggheatmap,plotlist,theme){
  if(max(plotlist)>length(ggheatmap$plotlist)){
    message("The plotlist should be included in 1 ~",length(ggheatmap$plotlist))
  }else{
    for(i in 1:length(plotlist)){
      num <- plotlist[i]
      ggheatmap[[num]] <- ggheatmap[[num]]+theme[[i]]
    }
  }
  return(ggheatmap)
}
