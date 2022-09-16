# Application of CONNECTOR to PDX models of metastatic colorectal cancer

The PDX curves were analyzed by CONNECTOR to achieve a widespread overview of the distribution of the individual PDX models after drug administration. When running CONNECTOR, based on the results from the model selection phase, the optimal value of the base spline was 4. To perform a through analysis we evaluated fDB and tight-
ness for a broad range of clusters numbers, starting with three, that corresponds to the number of clinical response classes. We chose to compare the results obtained with 3, 4 and 5 clusters, as the fDB index worsen for larger values.

We observed that by increasing the number of clusters, the curves that were separated were mainly those characterized by a sudden volume increase. Based on such analysis, the dataset was optimally described by
three primary clusters, namely A, B and C. We then assessed whether a higher resolution could be achieved by segregating the larger clusters into subclasses. We thus processed again, as independent datasets, the clusters composed by more than 200 curves (namely A and B). Through this, we obtained a final number of 9 clusters - where A and B are further split in Aa, Ab, Ac, Ad, Ae and Ba, Bb, Bc.


This repository is structured as follows:

1. **Data** folder stores the data regarding the PDX time series.
2. **Report** folder stores the Rmarkdown scripts to run the complete analysises described above.
3. **Rsults** folder stores the RData with the results obtained.
4. **Rfunctions** folder stores all the R scripts necessary to reproduce some step of the analysis.