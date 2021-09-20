library("pheatmap")
getwd()
setwd("C://Users//fatem//Downloads/Pheatmap/cleaning")
library(readxl)
y <- read_excel("export_dataframe_attosa_delfani.xlsx",sheet = "Sheet2")
head(y)
#change row name of the dataset


x = y[,2:5]
x#normalize dataset
#if we have zero among numbers of the dataset, we should write --> y = log2(y+1)
#x = log2(x)
rownames(x) <- y$Gene
#y$shobe
rownames(x)#save the heatmap
#pdf("heatmap.pdf")
#heatmap with correlation between columns and rows for showing relationship between them:


library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()                       #vector of font family names
##  [1] "Andale Mono"                  "AppleMyungjo"                
##  [3] "Arial Black"                  "Arial"                       
##  [5] "Arial Narrow"                 "Arial Rounded MT Bold"  
loadfonts()

#Italic fonts:
newnames <- lapply(
  rownames(x),
  function(x) bquote(italic(.(x))))
pheatmap(x,fontsize_row = 15, fontsize = 15, fontsize_col = 15,show_colnames = TRUE,
         cellwidth=45,cellheight=30,
         show_rownames = TRUE,angle_col=45,display_numbers=FALSE,legend= FALSE,
         cluster_rows = F,cluster_cols = F, clustering_distance_rows = "correlation",
         clustering_distance_cols = "correlation", family="Tahoma")
         #main = "test1")
         #,labels_row = as.expression(newnames))
#dev.off()
#getwd()
?pheatmap

