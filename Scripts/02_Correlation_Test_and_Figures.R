###################################################################################
#                               CASTIELLO ET AL.                                  #
#                               ----------------                                  #
#           GENERIC SRIPT TO PERFORM SPEARMAN CORRELATION TEST ON                 #
#          PALEOCLIMATIC AND ENVIRONMENTAL DATA AND CREATE FIGURES                #
#                                                                                 #
#                             #  November 2024  #                                 #
#                                                                                 #
#                                                                                 #
###################################################################################


## READ LIBRARIES AND PREPARE FOLDER & FILES ##
#----------------------------------------------
library(raster)
library(corrplot)
library(dendextend)

# Set your working directory
setwd("YOUR WORKING DIRECTORY")


## PALEOCLIMATIC DATA ##
#-----------------------

# Create object with number of variables and vector to store them
nbio<-18 
cor.array<-array(,c(nbio,nbio))
ind.var<-rep(NA,nbio) 

# Create new folder for outputs
dir.create("./Correlation_results")

# Read all files in the tiff files containing the mean values over the entire study period for each paleoclimatic variable ("means" folder)
# Here we use raster files cropped to the exact extent of the study area (without buffer)
raster_files <- list.files("./data/cropped_data/means/", pattern = "tif", full.names = TRUE)


## CREATE A FUNCTION TO READ THE RASTER FILES ##
#-----------------------------------------------

# Create a function to open the raster files
myfun <- function(path){
  # Isolate the file name from the path
  file_name <- basename(tools::file_path_sans_ext(path))
  # Create an object with the raster you are working on
  clim <- raster(path)
  }
  
lapply(raster_files, myfun)->my.raster  
  

## CALCULATE SPATIAL CORRELATION AND CREATE CORRELATION MATRIX ##
#----------------------------------------------------------------

for (ibio in c(1:nbio)){
   for (ybio in c(1:nbio)){
      if ((ibio!=ybio)){
        # Calculate spatial correlation between 2 variables and pass value in corr.array, with corresponding order
        cor(as.vector(my.raster[[ibio]]),as.vector(my.raster[[ybio]]), use="complete.obs",method=c("spearman"))->cor.array[ibio,ybio]
        # If 2 variables are highly correlated, consider only the first 
        # as representative of a given cluster of variables with similar correlation
        print(ibio)
        }
      }
    }

for (ibio in c(1:nbio)){
   which(cor.array[ibio,]>=0.75,arr.ind=TRUE)->tmp.index
   if (is.na(ind.var[ibio])==TRUE){
      ind.var[c(ibio,tmp.index)]<-ibio
      }
   print(ibio)
   }


# Create labels for row and column names
labels <- list("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10", "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19")
colnames(cor.array) <- labels
rownames(cor.array) <- labels

# Print and save correlation matrix array as csv file
print(cor.array)
write.csv(cor.array, file = "./Correlation_results/Bio_correlation_matrix_spearman.csv")


# CREATE DENDROGRAM PLOT WITH CORRELATION CLUSTERS WITH A LIMIT AT r = 0.75 (distance = 0.25)
# -------------------------------------------------------------------------------------------

# Use values from cor.array to create dendrogram with correlation clusters with limit at r = 0.75
var.dist <- abs(as.dist(cor.array))
var.cluster <- hclust(1-var.dist)
dd <- as.dendrogram(var.cluster)

# Create Dendrogram plot and save as pdf file
dev.new()
  pdf(file = "./Correlation_results/Bio_Dendrogram_spearman.pdf", width = 4, height = 7, bg = "white")
  # Highlight the retained variables in black
  labels_colors(dd) <- c("black", "grey", "grey", "grey", "grey", "grey", "black", "grey", "grey", "grey", "black", "black", "black", "black", "grey", "black", "black", "black")
  par(mar=c(2,4,1,4))
  plot(dd, ylab="Distance", xlab="", sub=NA, cex=0.75, horiz=T)
  abline(v=0.25, lty=2, lwd=2)
dev.off()


## ENVIRONMENTAL DATA ##
#-----------------------

# Create object with number of variables and vector to store them
nenv <- 5 
cor.array <- array(,c(nenv,nenv))
ind.var <- rep(NA,nenv) 

# Read all files in the tiff files containing the mean values over the entire study period for each paleoclimatic variable ("means" folder)
# Here we use raster files cropped to the exact extent of the study area (without buffer)
raster_files <- list.files("./data/cropped_data/Env_Vars/", pattern = "tif", full.names = TRUE)


## CREATE A FUNCTION TO READ THE RASTER FILES ##
#-----------------------------------------------

# Create a function to open the raster files
myfun <- function(path){
  # Isolate the file name from the path
  file_name <- basename(tools::file_path_sans_ext(path))
  # Create an object with the raster you are working on
  clim <- raster(path)
}

lapply(raster_files, myfun)->my.raster  


## CALCULATE SPATIAL CORRELATION AND CREATE CORRELATION MATRIX ##
#----------------------------------------------------------------

for (ienv in c(1:nenv)){
  for (yenv in c(1:nenv)){
    if ((ienv!=yenv)){
      # Calculate spatial correlation between 2 variables and pass value in corr.array, with corresponding order
      cor(as.vector(my.raster[[ienv]]),as.vector(my.raster[[yenv]]), use="complete.obs",method=c("spearman"))->cor.array[ienv,yenv]
      # If 2 variables are highly correlated, consider only the first 
      # as representative of a given cluster of variables with similar correlation
      print(ienv)
    }
  }
}

for (ienv in c(1:nenv)){
  which(cor.array[ienv,]>=0.75,arr.ind=TRUE)->tmp.index
  if (is.na(ind.var[ienv])==TRUE){
    ind.var[c(ienv,tmp.index)]<-ienv
  }
  print(ienv)
}


# Create labels for row and column names
labels <- list(
  "DEM", 
  "DistLakes", 
  "DistRiv", 
  "Slope", 
  "TRI")
colnames(cor.array) <- labels
rownames(cor.array) <- labels

# Print and save correlation matrix array as csv file
print(cor.array)
write.csv(cor.array, file = "./Correlation_results/Env_correlation_matrix_spearman.csv")


# CREATE DENDROGRAM PLOT WITH CORRELATION CLUSTERS WITH A LIMIT AT r = 0.75 (distance = 0.25)
# -------------------------------------------------------------------------------------------

# Use values from cor.array to create dendrogram with correlation clusters with limit at r = 0.75
var.dist <- abs(as.dist(cor.array))
var.cluster <- hclust(1-var.dist)
dd <- as.dendrogram(var.cluster)

# Create Dendrogram plot and save as pdf file
dev.new()
pdf(file = "./Correlation_results/Env_Dendrogram_spearman.pdf", width = 4, height = 7, bg = "white")
# Highlight the retained variables in black
labels_colors(dd) <- c("black", "black", "black", "black", "grey")
par(mar=c(2,4,1,4))
plot(dd, ylab="Distance", xlab="", sub=NA, cex=0.75, horiz=T)
abline(v=0.25, lty=2, lwd=2)
dev.off()

graphics.off()

