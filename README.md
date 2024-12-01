# This repository contains the data and code for the paper "Understanding the spread of agriculture in the Western Mediterranean (6th-3rd millennia BC) with Machine Learning tools"

Instructions for running the R scripts for the manuscript "Understanding the spread of agriculture in the Western Mediterranean (6th-3rd millennia BC) with Machine Learning tools" Castiello, M.E., Russo, E., Martinez-Grau, H., Jesus, A., Prats, G., Antolin, F. 2024 Nat. Comm.


The repository contains: 

1. A data folder with the data compiled for the analysis. 

- OriginalData:
This folder contains the Wget path file to download the original paleoclimatic variables from Karger, Dirk Nikolaus; Nobis, Michael P.; Normand, Signe; Graham, Catherine H.; Zimmermann, Niklaus E. (2020). CHELSA-TraCE21k: Downscaled transient temperature and precipitation data since the last glacial maximum. EnviDat. https://doi.org/10.16904/envidat.211  
The data can be downloaded from https://envicloud.wsl.ch/#/?bucket=https%3A%2F%2Fos.zhdk.cloud.switch.ch%2Fchelsav1%2F&prefix=chelsa_trace%2F using the Wget path file envidatS3paths.txt (for further processing using script 01 save all the downloaded files in this folder).

- Sites:
This folder contains the csv file for the archaeological sites and radiocarbon dates associated with the study area.
The raw data can be downloaded from Martínez-Grau, Héctor, Morell-Rovira, Berta, & Antolín, Ferran. (2021). Radiocarbon Dates Associated to Neolithic Contexts (Ca. 5900 – 2000 Cal BC) from the Northwestern Mediterranean Arch to the High Rhine Area. In Journal of Open Archaeology Data (Vol. 9, Number 1, pp. 1–10). https://doi.org/10.5334/joad.72 

- Crops:
This folder contains the csv file with crops information (spatial distribution and name) associated with the archaeological sites in the study area. References for this data are found in the manuscript.

- cropped_data/Env_Vars:
This folder contains the tiff files of the environmental variables (DEM, Slope, TRI, Distance to lakes, Distance to rivers) without buffer. 

- cropped_data_Buffer/Env_Vars:
This folder contains the tiff files of the environmental variables (DEM, Slope, TRI, Distance to lakes, Distance to rivers) with buffer. 

- Coast_Line:
This folder contains the GIS shapefile of the coast line used for producing Figures 4 and 5 in the main manuscript.

- Hillshade:
This folder contains the GIS shapefile of the study area's hillshade used for producing Figures 4 and 5 in the main manuscript.

- studyarea:
This folder contains the GIS shapefile of the study area with and without a buffer (5 km) used for producing Figures 4 and 5 in the main manuscript.


2. A Scripts folder with all the R scripts for data processing, model preparation, as well as scripts for generating figures and other relevant output files. In order to reproduce the manuscript results they should be executed in the numbering order. 
