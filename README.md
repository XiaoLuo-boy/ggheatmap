# ggheatmap
ggplot2 version of heatmap

# Install Package
` devtools::install_github("XiaoLuo-boy/ggheatmap")`

# Parameter Description
* **data**	input data(matrix or data.frame)
* **color**	the color of heatmap
* **legendName**	character,the title of heatmap legend
* **scale**	character,the way of scale data("none", "row" or "column")
* **cluster_rows**	whether rows should be clustered(TRUE of FALSE)
* **cluster_cols**	whether column should be clustered(TRUE of FALSE)
* **dist_method**	character,the method parameter of dist function. see dist
* **hclust_method**	character,the method parameter of hclust function, see hclust
* **text_angle_rows**	numeric,the angle of y-axis label
* **text_angle_cols**	numeric,the angle of x-axis label
* **text_color_rows**	character,the color of y-axis label
* **text_color_cols**	character,the color of x-axis label
* **text_face_rows**	character,the face of y-axis label, see element_text
* **text_face_cols**	character,the face of x-axis label, see element_text
* **text_just_rows**	a vector(hjust,vjust) for the hjust/vjust of y-axis label
* **text_just_cols**	a vector(hjust,vjust) for the hjust/vjust of x-axis label
* **text_show_rows**	a character you want to show for y-axis
* **text_show_cols**	a character you want to show for x-axis
* **text_position_rows**	character,the position of y-axis label("right" or "left")
* **text_position_cols**	character,the position of x-axis label("bottom" or "top")
* **annotation_cols**	a data.frame for column annotation
* **annotation_rows**	a data.frame for row annotation
* **annotation_color**	a list for annotation color
* **annotation_width**	a numeric for annotation width
* **show_cluster_cols**	whether show column cluster tree(TRUE of FALSE)
* **show_cluster_rows**	whether show row cluster tree(TRUE of FALSE)
* **cluster_num**	a numeric for cut cluster tree
* **tree_height_rows**	row cluster tree height
* **tree_height_cols**	column cluster tree height
* **tree_color_rows**	a character for row cluster tree color
* **tree_color_cols**	a character for column cluster tree color
* **levels_rows**	a character for y-axis label levels
* **levels_cols**	a character for x-axis label levels

# Usage
```
library(ggheatmap)
library(aplot)
set.seed(123)
df <- matrix(runif(600,0,10),ncol = 12)
colnames(df) <- paste("sample",1:12,sep = "")
rownames(df) <- sapply(1:50, function(x)paste(sample(LETTERS,3,replace = F),collapse = ""))
row_metaData <- data.frame(exprtype=sample(c("Up","Down"),50,replace = T),
                           genetype=sample(c("Metabolism","Immune","None"),50,replace = T))
rownames(row_metaData) <- rownames(df)
col_metaData <- data.frame(tissue=sample(c("Normal","Tumor"),12,replace = T),
                           risklevel=sample(c("High","Low"),12,replace = T))
rownames(col_metaData) <- colnames(df)
exprcol <- c("#EE0000FF","#008B45FF" )
names(exprcol) <- c("Up","Down")
genecol <- c("#EE7E30","#5D9AD3","#D0DFE6FF")
names(genecol) <- c("Metabolism","Immune","None")
tissuecol <- c("#98D352","#FF7F0E")
names(tissuecol) <- c("Normal","Tumor")
riskcol <- c("#EEA236FF","#46B8DAFF")
names(riskcol) <- c("High","Low")
col <- list(exprtype=exprcol,genetype=genecol,tissue=tissuecol,risklevel=riskcol)
ggheatmap<- ggheatmap(df,cluster_rows = T,cluster_cols = T,scale = "row",
          cluster_num = c(5,3),
          tree_color_rows = c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF"),
          tree_color_cols = c("#1F77B4FF","#FF7F0EFF","#2CA02CFF"),
          annotation_rows = row_metaData,
          annotation_cols = col_metaData,
          annotation_color = col
)
dat <- data.frame(marker=sample(c(1,NA),50,replace = T),
                  gene=rownames(df),
                  shape=sample(c("T","F"),50,replace = T))
p <- ggplot(dat,aes(x=1,y=gene,size=marker,color=shape,shape=shape))+
  geom_point()+theme_classic()+
  scale_color_manual(values = c("#D2691E","#1E87D2"))+
  theme(line = element_blank(),axis.text = element_blank(),axis.title = element_blank())+
  guides(size = FALSE)  
ggheatmap%>%insert_right(p,width = 0.1)
```
![ggheatmap](http://github.com/XiaoLuo-boy/ggheatmap/blob/main/ggheatmap.png)
