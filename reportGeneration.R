
##############################################################################
######## Run the entire analysis considering the Cetixumab treatment #########

rmarkdown::render("PDXanalysis.Rmd",
                  output_file ="PDX_Cetixumab.html",
                  params = list(p = 4) )


##############################################################################
######## Let start segregating the larger clusters into subclasses ###########


gpost3wA = c(5,5,3)
gpost3wB = c(3,4,4)
k = 1
for( g in 3:5){
  
  Gselected = gpost3wA[k]
  rmarkdown::render("Report/SingleCluster.Rmd",
                    output_file =paste0("3w_ClusterAfromG",g,".html"),
                    output_dir = "Cetixumab/Report/",
                    params = list(pathRdata = "Cetixumab/RData/",
                                  Gpost = g,
                                  Gselected=Gselected,
                                  ClusterName = "A",
                                  p=4) )
  
  Gselected = gpost3wB[k]
  rmarkdown::render("Report/SingleCluster.Rmd",
                    output_file =paste0("3w_ClusterBfromG",g,".html"),
                    output_dir = "Cetixumab/Report/",
                    params = list(pathRdata = "Cetixumab/RData",
                                  Gpost = g,
                                  Gselected=Gselected,
                                  ClusterName = "B",
                                  p=4) ) 
  
  k = k+1
}


## 6 weeks

gpost = c(5,3,5)
k = 1
for( g in 3:5){
  rmarkdown::render("Report/SingleCluster.Rmd",
                    output_file =paste0("6w_ClusterAfromG",g,"_NoCRC0456.html"),
                    output_dir = "Cetixumab/Report/p4/",
                    params = list(pathRdata = "Cetixumab/RData/p4",
                                  Cut="6wCRC0456",
                                  Gpost = g,
                                  Gselected=gpost[k],
                                  Analysis = "6w",
                                  ClusterName = "A",
                                  p=4) )
  k = k+1
}

rmarkdown::render("Report/3and6WeeksReport.Rmd",
                  output_file ="3and6WeeksReport.html",
                  output_dir = "Cetixumab/Report/p4/",
                  params = list(Exp="Cetixumab",
                                p = 4,
                                gpost3wA = gpost3wA,
                                gpost3wB = gpost3wB,
                                gpost6w = c(4,3,5)) )

