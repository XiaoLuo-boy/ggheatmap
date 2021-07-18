# ggheatmap
ggplot2 Version of Heatmap

## Install Package
* Method1.Download from Github
` devtools::install_github("XiaoLuo-boy/ggheatmap")`
* Method2.Download from CRAN
` install.packages("ggheatmap") `

## function list
* ggheatmap() Draw a heatmap
* ggheatmap_theme() Set the styles of the drawing elements of each component of the ggheatmap except cluster tree
* ggheatmap_plotlist() Display the basic elements of the ggheatmap

## ggheatmap() Parameter Description
* **data**	input data(matrix or data.frame)
* **color**	the color of heatmap
* **legendName**	character,the title of heatmap legend
* **scale**	character,the way of scale data("none", "row" or "column")
* **border** character, the colour of border
* **shape** character, the shape of cell("square", "circle" and "triangle").Default is NULL
* **cluster_rows**	whether rows should be clustered(TRUE of FALSE)
* **cluster_cols**	whether column should be clustered(TRUE of FALSE)
* **dist_method**	character,the method parameter of dist function. see dist
* **hclust_method**	character,the method parameter of hclust function, see hclust
* **text_show_rows**	a character you want to show for y-axis
* **text_show_cols**	a character you want to show for x-axis
* **text_position_rows**	character,the position of y-axis label("right" or "left")
* **text_position_cols**	character,the position of x-axis label("bottom" or "top")
* **annotation_cols**	a data.frame for column annotation
* **annotation_rows**	a data.frame for row annotation
* **annotation_color**	a list for annotation color
* **annotation_width**	a numeric for annotation width
* **annotation_position_rows** character,the position of column annotation("right" or "left")
* **annotation_position_cols** character,the position of row annotation("bottom" or "top")
* **show_cluster_cols**	whether show column cluster tree(TRUE of FALSE)
* **show_cluster_rows**	whether show row cluster tree(TRUE of FALSE)
* **cluster_num**	a numeric for cut cluster tree
* **tree_height_rows**	row cluster tree height
* **tree_height_cols**	column cluster tree height
* **tree_color_rows**	a character for row cluster tree color
* **tree_color_cols**	a character for column cluster tree color
* **tree_position_rows** character,the position of row cluster tree("right" or "left")
* **tree_position_cols** character,the position of column cluster tree("bottom" or "top")
* **levels_rows**	a character for y-axis label levels
* **levels_cols**	a character for x-axis label levels

## Usage
```
devtools::install_github("XiaoLuo-boy/ggheatmap")
install.packages("ggheatmap")
library(ggheatmap)
library(tidyr)
set.seed(123)
df <- matrix(runif(225,0,10),ncol = 15)
colnames(df) <- paste("sample",1:15,sep = "")
rownames(df) <- sapply(1:15, function(x)paste(sample(LETTERS,3,replace = F),collapse = ""))
df[1:4,1:4]


row_metaData <- data.frame(exprtype=sample(c("Up","Down"),15,replace = T),
                           genetype=sample(c("Metabolism","Immune","None"),15,replace = T))
rownames(row_metaData) <- rownames(df)
col_metaData <- data.frame(tissue=sample(c("Normal","Tumor"),15,replace = T),
                           risklevel=sample(c("High","Low"),15,replace = T))
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
text_rows <- sample(rownames(df),3)

p<- ggheatmap(df,cluster_rows = T,cluster_cols = T,scale = "row",
              text_show_rows = text_rows,
              cluster_num = c(3,3),
              tree_color_rows = c("#008B45FF","#631879FF","#008280FF"),
              tree_color_cols = c("#1F77B4FF","#FF7F0EFF","#2CA02CFF"),
              annotation_rows = row_metaData,
              annotation_cols = col_metaData,
              annotation_color = col
)
p
```
![](https://user-images.githubusercontent.com/65945397/126067499-2426bc94-be1b-485c-909c-f1f91d65d101.png)

Show your basic elements of the ggheatmap
```
ggheatmap_plotlist(p)
```
![](https://user-images.githubusercontent.com/65945397/126067537-9ccfa3b7-1c0a-4a91-a7fd-ff0a58a73d66.png)

Use ggheatmap_theme to make your figure more elegant
```
p%>%
  ggheatmap_theme(1:5,
                  theme =list(
                    theme(axis.text.x = element_text(angle = 90,face = "bold",size = 10),
                          axis.text.y = element_text(colour = "red",face = "bold")),
                    theme(legend.title = element_text(face = "bold")),
                    theme(legend.title = element_text(face = "bold")),
                    theme(legend.title = element_text(face = "bold")),
                    theme(legend.title = element_text(face = "bold"))
                    ))
```
![image](https://user-images.githubusercontent.com/65945397/126067628-0115a4ea-171f-4956-ae2a-9f4d8df402e0.png)

Set your border color
```
ggheatmap(df,cluster_rows = T,cluster_cols = T,scale = "row",
              text_show_rows = text_rows,
              border = "grey",
              cluster_num = c(3,3),
              tree_color_rows = c("#008B45FF","#631879FF","#008280FF"),
              tree_color_cols = c("#1F77B4FF","#FF7F0EFF","#2CA02CFF"),
              annotation_rows = row_metaData,
              annotation_cols = col_metaData,
              annotation_color = col
)%>%
  ggheatmap_theme(1,theme =list(theme(axis.text.x = element_text(angle = 90,face = "bold",size = 10),
                          axis.text.y = element_text(colour = "red",face = "bold"))))
```
![](https://user-images.githubusercontent.com/65945397/126067560-18309174-6b02-442d-a0a0-cdba0628b8ab.png)

Chage your heatmap cell shape
```
ggheatmap(df,cluster_rows = T,cluster_cols = T,scale = "row",
          text_show_rows = text_rows,
          border = "grey",
          shape = "circle",
          cluster_num = c(3,3),
          tree_color_rows = c("#008B45FF","#631879FF","#008280FF"),
          tree_color_cols = c("#1F77B4FF","#FF7F0EFF","#2CA02CFF"),
          annotation_rows = row_metaData,
          annotation_cols = col_metaData,
          annotation_color = col
)%>%
  ggheatmap_theme(1,theme =list(theme(axis.text.x = element_text(angle = 90,face = "bold",size = 10),
                                      axis.text.y = element_text(colour = "red",face = "bold"))))
```
![](https://user-images.githubusercontent.com/65945397/126067675-67a20361-6683-4c7d-8c7c-c94b0e30c43a.png)

Position
```
ggheatmap(df,cluster_rows = T,cluster_cols = T,scale = "row",
          text_show_rows = text_rows,
          border = "grey",
          cluster_num = c(3,3),
          tree_color_rows = c("#008B45FF","#631879FF","#008280FF"),
          tree_color_cols = c("#1F77B4FF","#FF7F0EFF","#2CA02CFF"),
          annotation_rows = row_metaData,
          annotation_cols = col_metaData,
          annotation_color = col,
          text_position_rows = "left",
          text_position_cols = "top",
          tree_position_rows = "right",
          tree_position_cols = "bottom",
          annotation_position_rows = "right",
          annotation_position_cols = "bottom"
          
)%>%
  ggheatmap_theme(1,theme =list(theme(axis.text.x = element_text(angle = 90,face = "bold",size = 10),
                                      axis.text.y = element_text(colour = "red",face = "bold"))))
```
![](https://user-images.githubusercontent.com/65945397/126067706-35e462c0-9736-4225-80a1-ff40d310e27e.png)
