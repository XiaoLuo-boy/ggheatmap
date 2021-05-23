#' @name ggheatmap
#' @title ggplot2 Version of Heatmap
#' @author Baiwei Luo
#' @param data input data(matrix or data.frame)
#' @param color the color of heatmap
#' @param legendName character,the title of heatmap legend
#' @param scale character,the way of scale data("none", "row" or "column")
#' @param cluster_rows whether rows should be clustered(TRUE of FALSE)
#' @param cluster_cols whether column should be clustered(TRUE of FALSE)
#' @param dist_method character,the method parameter of dist function. see \link{dist}
#' @param hclust_method character,the method parameter of hclust function, see \link{hclust}
#' @param text_angle_rows numeric,the angle of y-axis label
#' @param text_angle_cols numeric,the angle of x-axis label
#' @param text_color_rows character,the color of y-axis label
#' @param text_color_cols character,the color of x-axis label
#' @param text_face_rows character,the face of y-axis label, see \link{element_text}
#' @param text_face_cols character,the face of x-axis label, see \link{element_text}
#' @param text_just_rows a vector(hjust,vjust) for the hjust/vjust of y-axis label
#' @param text_just_cols a vector(hjust,vjust) for the hjust/vjust of x-axis label
#' @param text_show_rows a character you want to show for y-axis
#' @param text_show_cols a character you want to show for x-axis
#' @param text_position_rows character,the position of y-axis label("right" or "left")
#' @param text_position_cols character,the position of x-axis label("bottom" or "top")
#' @param annotation_cols a data.frame for column annotation
#' @param annotation_rows a data.frame for row annotation
#' @param annotation_color a list for annotation color
#' @param annotation_width  a numeric for annotation width
#' @param show_cluster_cols whether show column cluster tree(TRUE of FALSE)
#' @param show_cluster_rows whether show row cluster tree(TRUE of FALSE)
#' @param cluster_num a numeric for cut cluster tree
#' @param tree_height_rows row cluster tree height
#' @param tree_height_cols column cluster tree height
#' @param tree_color_rows a character for row cluster tree color
#' @param tree_color_cols a character for column cluster tree color
#' @param levels_rows a character for  y-axis label levels
#' @param levels_cols a character for  x-axis label levels
#' @return p
#' @import ggplot2
#' @import ggpubr
#' @import aplot
#' @import tibble
#' @import factoextra
#' @import tidyr
#' @import grDevices
#' @import stats
#' @examples
#' #Create data
#' library(ggheatmap)
#' set.seed(123)
#' df <- matrix(runif(600,0,10),ncol = 12)
#' colnames(df) <- paste("sample",1:12,sep = "")
#' rownames(df) <- sapply(1:50, function(x)paste(sample(LETTERS,3,replace = FALSE),collapse = ""))
#' head(df)
#'
#' #example 1:The usage of cluster_rows/cluster_cols
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE)
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = FALSE)
#'
#' #example 2:Scale data
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,scale = "column")
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,scale = "row")
#'
#' #example 3:Set legend title
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,legendName = "score")
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,legendName = NULL)
#'
#' #example 4:Adjust the coordinate label
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,text_angle_rows = 330,text_just_rows = c(1,0))
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,text_angle_cols = 45,text_just_cols = c(1,1))
#'
#' #example 5:Custom label color
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,text_color_cols = "red")
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,
#'             text_color_cols = c(rep("black",6),"red",rep("black",5)))
#'
#' #example 6:Custom label face
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,text_face_rows = "bold")
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,
#'           text_color_rows = c("red",rep("black",49)),
#'           text_face_rows = c("bold",rep("italic",49)))
#'
#' #example 7:Adjust the color and height of the cluster tree
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,show_cluster_cols = FALSE)
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,tree_height_rows = 0.05,
#'            tree_height_cols = 0.05)
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,cluster_num = c(5,4))
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,cluster_num = c(5,4),
#'           tree_color_rows = c("#3B4992FF","#EE0000FF","#008B45FF",
#'           "#631879FF","#008280FF"),
#'           tree_color_cols = c("#0073C2FF", "#EFC000FF" ,"#868686FF", "#CD534CFF"))
#'
#' #example 8:Comprehensive usage1
#' text_rows <- sample(rownames(df),3)
#' ggheatmap(df,scale = "row",cluster_rows  = TRUE,cluster_cols = TRUE,cluster_num = c(2,3),
#'           text_show_rows = text_rows,
#'           text_face_rows = "bold",
#'           text_color_rows = "red",
#'           text_angle_cols = 45,
#'           text_just_cols = c(1,1),
#'           tree_color_rows = c( "#3B4992FF","#EE0000FF"),
#'           tree_color_cols = c("#1F77B4FF","#FF7F0EFF","#2CA02CFF"))
#'
#' #sample 9:Annotation
#' row_metaData <- data.frame(exprtype=sample(c("Up","Down"),50,replace = TRUE),
#'                            genetype=sample(c("Metabolism","Immune","None"),50,replace = TRUE))
#' rownames(row_metaData) <- rownames(df)
#' col_metaData <- data.frame(tissue=sample(c("Normal","Tumor"),12,replace = TRUE),
#'                            risklevel=sample(c("High","Low"),12,replace = TRUE))
#' rownames(col_metaData) <- colnames(df)
#' exprcol <- c("#EE0000FF","#008B45FF" )
#' names(exprcol) <- c("Up","Down")
#' genecol <- c("#EE7E30","#5D9AD3","#D0DFE6FF")
#' names(genecol) <- c("Metabolism","Immune","None")
#' tissuecol <- c("#98D352","#FF7F0E")
#' names(tissuecol) <- c("Normal","Tumor")
#' riskcol <- c("#EEA236FF","#46B8DAFF")
#' names(riskcol) <- c("High","Low")
#' col <- list(exprtype=exprcol,genetype=genecol,tissue=tissuecol,risklevel=riskcol)
#'
#' ggheatmap(df,cluster_rows = TRUE,cluster_cols = TRUE,scale = "row",
#'                       cluster_num = c(5,3),
#'                       tree_color_rows = c("#3B4992FF","#EE0000FF","#008B45FF",
#'                       "#631879FF","#008280FF"),
#'                       tree_color_cols = c("#1F77B4FF","#FF7F0EFF","#2CA02CFF"),
#'                       annotation_rows = row_metaData,
#'                       annotation_cols = col_metaData,
#'                       annotation_color = col
#' )
#'
#' @export
NULL


ggheatmap <- function(data,
                       color=colorRampPalette(c( "#0073c2","white","#efc000"))(100),
                       legendName="Express",
                       scale="none",
                       cluster_rows = FALSE,
                       cluster_cols = FALSE,
                       dist_method="euclidean",
                       hclust_method="complete",
                       text_angle_rows=0,
                       text_angle_cols=0,
                       text_color_rows=NULL,
                       text_color_cols=NULL,
                       text_face_rows=NULL,
                       text_face_cols=NULL,
                       text_just_rows=NULL,
                       text_just_cols=NULL,
                       text_position_rows="right",
                       text_position_cols="bottom",
                       text_show_rows=waiver(),
                       text_show_cols=waiver(),
                       annotation_cols=NULL,
                       annotation_rows=NULL,
                       annotation_color,
                       annotation_width=0.03,
                       show_cluster_cols=TRUE,
                       show_cluster_rows=TRUE,
                       cluster_num=NULL,
                       tree_height_rows=0.1,
                       tree_height_cols=0.1,
                       tree_color_rows=NULL,
                       tree_color_cols=NULL,
                       levels_rows=NULL,
                       levels_cols=NULL
                       ){
                         gene=NULL
                         cluster=NULL
  #step1.scale data
                         if(scale=="column"){
                         scale_df <- scale(data)
                       }else if(scale=="row"){
                         scale_df <- t(scale(t(data)))
                       }else{
                         scale_df <- data
                       }
  #step2. draw cluster tree
                        dat <- as.data.frame(scale_df)%>%
                             rownames_to_column(var = "gene")%>%
                             tidyr::gather(key = "cluster",value = "expression",-gene)
                         if(cluster_rows){
                           row_clust <- hclust(dist(as.matrix(scale_df),method = dist_method),method = hclust_method)
                           roworder <- row_clust$labels[row_clust$order]

                           dat$gene <- factor(dat$gene,levels = roworder)
                           if(show_cluster_rows){
                             row_ggtreeplot <- fviz_dend(row_clust, k = cluster_num[1],
                                                         k_colors = tree_color_rows,
                                                         color_labels_by_k = TRUE,
                                                         show_labels = FALSE,
                                                         labels_track_height = 0,
                                                         horiz = TRUE,
                                                         ggtheme = theme_classic())+
                               theme(title = element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.ticks = element_blank(),
                                     axis.line.x = element_blank())
                           }else{
                             row_ggtreeplot <- NULL
                           }
                         }else{

                           dat <- as.data.frame(scale_df)%>%
                             rownames_to_column(var = "gene")%>%
                             tidyr::gather(key = "cluster",value = "expression",-gene)
                           if(!is.null(levels_rows)){
                             dat$gene <- factor(dat$gene,levels = levels_rows)
                           }else{
                             dat$gene <- as.factor(dat$gene)
                           }

                           row_ggtreeplot <- NULL
                         }


                        if(cluster_cols){
                           cols_clust <-  hclust(dist(t(as.matrix(scale_df)),method = dist_method),method = hclust_method)
                           colorder <- cols_clust$labels[cols_clust$order]
                           dat$cluster <- factor(dat$cluster,levels = colorder)
                           if(show_cluster_cols){
                            col_ggtreeplot <- fviz_dend(cols_clust, k = cluster_num[2],
                                                        k_colors = tree_color_cols,
                                                        color_labels_by_k = TRUE,
                                                        show_labels = FALSE,
                                                        labels_track_height = 0,
                                                        ggtheme = theme_classic())+
                              theme(title = element_blank(),
                                    axis.text.y = element_blank(),
                                    axis.ticks = element_blank(),
                                    axis.line.x = element_blank())
                           }else{
                             col_ggtreeplot <- NULL
                           }
                         }else{
                           if(!is.null(levels_cols)){
                             dat$cluster <- factor(dat$cluster,levels = levels_cols)
                           }else{
                             dat$cluster <- as.factor(dat$cluster)
                           }

                           col_ggtreeplot <- NULL
                         }

  #step3.axis label

                          if(is.character(text_show_rows)){
                            text_rows <- as.character(sapply(levels(dat$gene),function(x){ifelse(!x%in%text_show_rows,NA,x)}))
                          }else{text_rows <- text_show_rows}
                          if(is.character(text_show_cols)){
                            text_cols <- as.character(sapply(levels(dat$cluster),function(x){ifelse(!x%in%text_show_cols,NA,x)}))
                          }else{text_cols <- text_show_cols}

  #step4.Draw the main body of the heatmap
                          ggdat <- dat
                          p <- ggplot(ggdat, aes(cluster, gene, fill= expression)) +
                              geom_tile() +
                              scale_fill_gradientn(colours = color)+
                              scale_y_discrete(position = text_position_rows,breaks=text_rows)+
                              scale_x_discrete(position = text_position_cols,breaks=text_cols)+
                              theme(axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    axis.ticks = element_blank(),
                                    panel.background = element_rect(fill = "white", colour = "white"),
                                    legend.key = element_rect(fill = "white", colour = "white"),
                                    axis.text.x = element_text(face = text_face_cols,
                                                               angle = text_angle_cols,
                                                               color = text_color_cols,
                                                               hjust = text_just_cols[1],
                                                               vjust = text_just_cols[2]),
                                    axis.text.y = element_text(face = text_face_rows,
                                                               angle = text_angle_rows,
                                                               color = text_color_rows,
                                                               hjust = text_just_rows[1],
                                                               vjust = text_just_rows[2]))+
                              labs(fill=legendName)

  #step5.draw your annotation
                         if(!is.null(annotation_rows)){
                           annotation_rows$none <- factor(rep(1,nrow(annotation_rows)))
                           annotation_rows <- rownames_to_column(annotation_rows,var = "none1")
                           for(i in 1:(ncol(annotation_rows)-2)){
                             rowlist <- list()
                             rowanno <- ggplot()+
                               geom_exec(geom_tile,data = annotation_rows,x="none1",y="none",fill=names(annotation_rows)[i+1])+
                               scale_fill_manual(values =annotation_color[names(annotation_color)==names(annotation_rows)[i+1]][[1]])+
                               theme_void()+coord_flip()+labs(fill=names(annotation_rows)[i+1])
                             p <-  p%>%insert_left(rowanno,width =annotation_width )
                           }
                         }else{
                           rowanno <- NULL
                           }
                         if(!is.null(annotation_cols)){
                           annotation_cols$none <- factor(rep(1,nrow(annotation_cols)))
                           annotation_cols <- rownames_to_column(annotation_cols,var = "none1")
                           for(i in 1:(ncol(annotation_cols)-2)){
                             collist <- list()
                             colanno <- ggplot()+
                               geom_exec(geom_tile,data = annotation_cols,x="none1",y="none",fill=names(annotation_cols)[i+1])+
                               scale_fill_manual(values = annotation_color[names(annotation_color)==names(annotation_cols)[i+1]][[1]])+
                               theme_void()+labs(fill=names(annotation_cols)[i+1])
                             p <-  p%>%insert_top(colanno,height =annotation_width )
                           }
                         }else{
                           colanno <- NULL
                         }

  #step6.Merge pictures


                         if(!is.null(row_ggtreeplot)){
                           p <- p%>%
                             insert_left(row_ggtreeplot,width = tree_height_rows)
                         }
                         if(!is.null(col_ggtreeplot)){
                           p <- p%>%insert_top(col_ggtreeplot,height = tree_height_cols)
                         }
                        return(p)
                       }

