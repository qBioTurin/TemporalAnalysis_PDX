library(circlize) # https://jokergoo.github.io/circlize_book/book/advanced-usage-of-chorddiagram.html#organization-of-tracks
library(dplyr)
library(wesanderson)

load("./Results/AllClusters3w_p4.RData")

#######
dfList = list()

dfAll = ClusteringComplete %>% select(-ClusterPre) %>%
  tidyr::gather(key = "NumberCluster", value = "Clusters", -LongID,-ShortID) %>%
  na.omit()
#### comment if you want to keep the subclustering
dfAll$Clusters = gsub(dfAll$Clusters,pattern = "a|b|c|d|e",replacement = "")
####
dfAll$cl = paste0("cl",dfAll$NumberCluster,"_",dfAll$Clusters)

## color definition
CLall = unique(dfAll$cl)
colAll = rep("white",length(CLall))
names(colAll) = CLall
colAll[grep("cl5",CLall)] = wes_palette("FantasticFox1", n = length(grep("cl5",CLall)),type = "discrete")
colAll[grep("cl4",CLall)] = wes_palette("Darjeeling1", n = length(grep("cl4",CLall)),type = "discrete")
colAll[grep("cl3",CLall)] = wes_palette("GrandBudapest1", n = length(grep("cl3",CLall)),type = "discrete")

###
names(colAll) = unique(dfAll$cl)

combo = list(c("3","4"),c("4","5"))
for(j in 1:2){
  df = dfAll %>% dplyr::filter(NumberCluster %in% combo[[j]]) %>% select(LongID,cl)
  cl = sort(unique(df$cl))
  cl2 = cl[grep(x = cl,pattern = min(combo[[j]]))]
  cl1 = cl[grep(x = cl,pattern = max(combo[[j]]))]


  mat = matrix(0,nrow = length(cl1),ncol = length(cl2) )
  colnames(mat) =  cl2
  row.names(mat) =  cl1

  for(c in cl1){
    longId_c = df$LongID[df$cl == c]
    t = table(df$cl[df$LongID %in% longId_c])
    t = t[names(t) != c]
    mat[c,names(t)] = t
  }

  df = data.frame(from = rep(colnames(mat), each = nrow(mat)),
                  to = rep(rownames(mat), times = ncol(mat)),
                  value = as.vector(mat),
                  stringsAsFactors = FALSE)

  	df = df[order(df$from,decreasing = T),]


  dfList[[paste(j)]]$df = df
  dfList[[paste(j)]]$mat = mat
}

# par(mfrow = c(1, 2))
# first plot

pdf(paste0("Circos_3_4.pdf"),width = 6,height = 6)

mat = dfList$`1`$mat
df = dfList$`1`$df
nm = union(df[[1]], df[[2]])
group = structure(gsub("_(A|B|C|D)|(a|b|c|d|e)$","", nm), names = nm)
circos.clear()
circos.par(start.degree = -100,track.margin = c(mm_h(4), 0))
col = colAll[names(group)]

chordDiagram(df,
						 group = group, big.gap = 20, small.gap = 1,
             annotationTrack = "grid",
             grid.col = col,
             annotationTrackHeight = c(mm_h(3)),
             preAllocateTracks = list(
               track.height = mm_h(4),
               track.margin = c(mm_h(2), 0)
             ), scale = TRUE)

## Adding the %
circos.track(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  xplot = get.cell.meta.data("xplot")

  circos.lines(xlim, c(min(ylim), min(ylim)), lty = 3) # dotted line
  by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.2, 0.5)
  circos.lines(c(0,0),
               c(min(ylim), min(ylim)- 0.15), lty = 1)
  for(p in seq(by, 1, by = by)) {
    circos.text(p*(xlim[2] - xlim[1]) + xlim[1], min(ylim) - 0.6,
                paste0(p*100, "%"), cex = 0.6,
                adj = c(0.6, 0), niceFacing = TRUE )
    circos.lines(c(p*(xlim[2] - xlim[1]) + xlim[1],p*(xlim[2] - xlim[1]) + xlim[1]),
                 c(min(ylim), min(ylim)- 0.15), lty = 1)
  }
}, bg.border = NA)
circos.track(track.index = 2, panel.fun = function(x, y) {
 s.index = gsub("cl(3|4)_","", get.cell.meta.data("sector.index"))
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")

  circos.text(mean(xlim), mean(ylim),
  						s.index, cex = 0.6, niceFacing = TRUE)
}, bg.border = NA)

dev.off()
# highlight.sector(rownames(mat)[grep(rownames(mat),pattern = "cl4")], track.index = 1,
#                  col = "blue",
#                  text = "Cluster 4", cex = 0.8, text.col = "white", niceFacing = TRUE)
# highlight.sector(colnames(mat)[grep(colnames(mat),pattern = "cl3")], track.index = 1,
#                  col = "red",
#                  text = "Cluster 3", cex = 0.8, text.col = "white", niceFacing = TRUE)

# second plot

pdf(paste0("Circos_4_5.pdf"),width = 6,height = 6)

mat = dfList$`2`$mat
df = dfList$`2`$df
nm = union(df[[1]], df[[2]])

group = structure(gsub("_(A|B|C|D|E)|(a|b|c|d|e)$","", nm), names = nm)
circos.clear()
circos.par(start.degree = -105,track.margin = c(mm_h(4),0))
col = colAll[names(group)]
chordDiagram(df, group = group, big.gap = 20, small.gap = 1,
             annotationTrack = "grid",
             grid.col = col,
             annotationTrackHeight = c(mm_h(3)),
             preAllocateTracks = list(
               track.height = mm_h(4),
               track.margin = c(mm_h(2), 0)
             ), scale = TRUE)
circos.info()
## Adding the %
circos.track(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  xplot = get.cell.meta.data("xplot")

  circos.lines(xlim, c(min(ylim), min(ylim)), lty = 3) # dotted line
  by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.2, 0.5)
  circos.lines(c(0,0),
               c(min(ylim), min(ylim)- 0.15), lty = 1)
  for(p in seq(by, 1, by = by)) {
    circos.text(p*(xlim[2] - xlim[1]) + xlim[1], min(ylim) - 0.6,
                paste0(p*100, "%"), cex = 0.6,
                adj = c(0.6, 0), niceFacing = TRUE )
    circos.lines(c(p*(xlim[2] - xlim[1]) + xlim[1],p*(xlim[2] - xlim[1]) + xlim[1]),
                 c(min(ylim), min(ylim)- 0.15), lty = 1)
  }
}, bg.border = NA)
## Adding the cluster letter
circos.track(track.index = 2, panel.fun = function(x, y) {
  sector.index = gsub("cl(5|4)_","", get.cell.meta.data("sector.index"))
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  circos.text(mean(xlim), mean(ylim), sector.index, cex = 0.6, niceFacing = TRUE)
}, bg.border = NA)
## adding the second rect layer
# highlight.sector(rownames(mat)[grep(rownames(mat),pattern = "cl5")], track.index = 1,
#                  col = "green",
#                  text = "Cluster 5", cex = 0.8, text.col = "white", niceFacing = TRUE)
# highlight.sector(colnames(mat)[grep(colnames(mat),pattern = "cl4")], track.index = 1,
#                  col = "blue",
#                  text = "Cluster 4", cex = 0.8, text.col = "white", niceFacing = TRUE)

dev.off()
circos.clear()

##############################


