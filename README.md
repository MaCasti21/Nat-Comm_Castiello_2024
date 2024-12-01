#  Understanding the spread of agriculture in the Western Mediterranean (6th-3rd millennia BC) with Machine Learning tools

This repository contains the data and code for the paper: <br>
Castiello, M.E. et al. (2024) Understanding the spread of agriculture in the Western Mediterranean (6th-3rd millennia BC) with Machine Learning tools 

## Authors 
[![ORCID iD](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-0446-1301) Castiello, M.E. (mariaelena.castiello@unil.ch) <br>
Russo, E., Martinez-Grau, H., Jesus, A., Prats, G., Antolin, F.

## Abstract
The first Neolithic farmers arrived in the Western Mediterranean area from the East. They established settlements in coastal areas and over time migrated to new environments, adapting to changing ecological and climatic conditions. While farming practices and settlements in the Western Mediterranean differ greatly from those known in the Eastern Mediterranean and central Europe, the extent to which these differences are connected to the local environment and climate is unclear. Here, we tackle this question by compiling data and proxies at a superregional and multi-scale level, including archaeobotanical information, radiocarbon dates and paleoclimatic models, then applying a machine learning approach to investigate the impact of ecological and climatic constraints on the first Neolithic humans and crops. This approach facilitates calculating the pace of spread of farming in the Western Mediterranean area, modelling and estimating the potential areas suitable for settlement location, and discriminating distinct types of crop cultivation under changing climatic conditions that characterized the period 5900 – 2300 cal. BC. The results of this study shed light onto the past climate variability and its influence on human distribution in the Western Mediterranean area, but also discriminate sensitive parameters for successful agricultural practices.

## How to cite
If you use this code or data in your research, please cite the original project as: <br>


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
