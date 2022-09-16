library(ggplot2)
library(dplyr)
library(wesanderson)

load("./Results/AllClusters_p4.RData")
load("./Data/AllTimeSeries.RData")


a = ClusteringComplete %>%
	select(-ShortID) %>% na.omit() %>%
	tidyr::gather(-LongID,value = "Clusters", key = "NumberOfClusters")
b = DataAllInfo.post.tillEndDate %>% select(LongID,Time,volume) %>% distinct()
c = merge(a,b,all.x = T)

M = do.call("rbind",MeanValues)

c$col = paste0(gsub(c$Clusters, pattern = "a|b|c|d|e",replacement = ""),c$NumberOfClusters)
CLall = unique(c$col)
colAll = rep("white",length(CLall))
names(colAll) = CLall
colAll[grep("5",CLall)] = wes_palette("FantasticFox1", n = length(grep("5",CLall)),type = "discrete")
colAll[grep("4",CLall)] = wes_palette("Darjeeling1", n = length(grep("4",CLall)),type = "discrete")
colAll[grep("3",CLall)] = wes_palette("GrandBudapest1", n = length(grep("3",CLall)),type = "discrete")


ggplot(c) +
	geom_line(aes(x  = Time, y = volume,group = LongID, col = Clusters)) +
	geom_line(data = M, aes(x  = Time, y = Means, group = Clusters),linetype = "dashed") +
	facet_wrap(NumberOfClusters ~ Clusters,scales = "free" )+
	theme_bw() +
	lims(x = c(0,25), y= c(0,10000))+
	labs(x = "Days", y = "Vol")+
	theme(legend.position = "none")

ggsave(filename = "LinesPlotSubClusters.pdf",width = 16,height = 7)

### Color tsne dependent

tsneCol = RColorBrewer::brewer.pal(9, "Paired")
c3 = c %>% filter(NumberOfClusters == "3")
names(tsneCol) = unique(c3$Clusters)
ggplot(c3) +
	geom_line(aes(x  = Time, y = volume,group = LongID, col = Clusters)) +
	geom_line(data = M %>% filter(NumberOfClusters == "3" & !Clusters %in% c("A","B")),
						aes(x  = Time, y = Means, group = Clusters),linetype = "dashed") +
	facet_wrap( ~ Clusters,nrow = 1)+
	scale_color_manual(values = tsneCol, breaks = names(tsneCol)) +
	theme_bw() +
	labs(x = "Days", y = "Vol")+
	theme(legend.position = "none")

ggsave(filename = "LinesPlotSubClustersTsneCol.pdf",width = 15,height = 2)


### Color dependent
ggplot(c) +
	geom_line(aes(x  = Time, y = volume,group = LongID, col = col)) +
	geom_line(data = M, aes(x  = Time, y = Means, group = Clusters),linetype = "dashed") +
	facet_wrap(NumberOfClusters ~ Clusters,scales = "free" )+
	scale_color_manual(values = colAll, breaks = names(colAll)) +
	theme_bw() +
	lims(x = c(0,25), y= c(0,10000))+
	labs(x = "Days", y = "Vol")+
	theme(legend.position = "none")

ggsave(filename = "LinesPlotSubClustersDiffCol.pdf",width = 16,height = 7)

ggplot(c %>% mutate(Clusters = gsub(Clusters, pattern = "a|b|c|d|e",replacement = ""))) +
	geom_line(aes(x  = Time, y = volume,group = LongID, col = col)) +
	geom_line(data = M %>% filter(Clusters %in% LETTERS[1:5]), aes(x  = Time, y = Means, group = Clusters),linetype = "dashed") +
	facet_wrap(NumberOfClusters ~ Clusters,scales = "free" )+
	scale_color_manual(values = colAll, breaks = names(colAll)) +
	theme_bw()+
	lims(x = c(0,25), y= c(0,10000))+
	labs(x = "Days", y = "Vol")+
	theme(legend.position = "none")

ggsave(filename = "LinesPlotClustersDiffCol.pdf",width = 10,height = 7)


