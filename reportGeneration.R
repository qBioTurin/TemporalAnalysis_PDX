


rmarkdown::render("Cetixumab.Rmd",
                  output_file ="CetixumabWithOneCRC0456.html",
                  params = list(Cut="CRC0456",
                                Analysis = "") )

rmarkdown::render("SingleCluster.Rmd",
                  output_file ="ClusterAfromG4_NoCRC0456.html",
                  params = list(Cut="CRC0456",
                                Gpost = 4,
                                Gselected=4) )

rmarkdown::render("SingleCluster.Rmd",
                  output_file ="ClusterAfromG5_NoCRC0456.html",
                  params = list(pathRdata = "Cetixumab/RData/DataCetuxi",
                                Cut="CRC0456",
                                Gpost = 5,
                                Gselected=4) )

### cetixumab 

rmarkdown::render("Report/Cetixumab.Rmd",
                  output_file ="Cetixumab/3w_CetixumabWithOneCRC0456.html",
                  output_dir = "Cetixumab/Report/p4/",
                  params = list(Cut="CRC0456",
                                Analysis = "3w",
                                p = 4) )

rmarkdown::render("Report/Cetixumab.Rmd",
                  output_file ="6w_CetixumabWithOneCRC0456.html",
                  output_dir = "Cetixumab/Report/p4/",
                  params = list(Cut="CRC0456",
                                Analysis = "6w",
                                p = 4) )

gpost3wA = c(5,5,3)
gpost3wB = c(3,4,4)
k = 1
for( g in 3:5){
  
  Gselected = gpost3wA[k]
  rmarkdown::render("Report/SingleCluster.Rmd",
                    output_file =paste0("3w_ClusterAfromG",g,"_NoCRC0456.html"),
                    output_dir = "Cetixumab/Report/p4/",
                    params = list(pathRdata = "Cetixumab/RData/p4",
                                  Cut="3wCRC0456",
                                  Gpost = g,
                                  Gselected=Gselected,
                                  Analysis = "3w",
                                  ClusterName = "A",
                                  p=4) )
  
    Gselected = gpost3wB[k]
    rmarkdown::render("Report/SingleCluster.Rmd",
                      output_file =paste0("3w_ClusterBfromG",g,"_NoCRC0456.html"),
                      output_dir = "Cetixumab/Report/p4/",
                      params = list(pathRdata = "Cetixumab/RData/p4",
                                    Cut="3wCRC0456",
                                    Gpost = g,
                                    Gselected=Gselected,
                                    Analysis = "3w",
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

