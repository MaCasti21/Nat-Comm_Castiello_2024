###################################################################################
#                               CASTIELLO ET AL.                                  #
#                               ----------------                                  #
#         GENERIC SRIPT FOR RANDOM FOREST & MAXIMUM ENTROPY PREDICTION            #
#                WITH PALEO-CLIMATIC AND ENVIRONMENTAL VARIABLES                  #
#                                                                                 #
#                            #  November 2024  #                                  #
#                                                                                 #
#                                                                                 #
###################################################################################


###################################################################################
# Load PACKAGES AND LIBRARIES
###################################################################################

library(raster)
library(blockCV)
library(sf)
library(ggplot2)
library(maxnet)
library(plotROC)
library(randomForest)
library(base)
library(caret)
library(terra)

  
###################################################################################
# CREATE LISTS FOR PHASES
###################################################################################

phase <- c("Phase01", "Phase02", "Phase03", "Phase04")
phase_start <- c(5300, 4500, 3100, 2300)
phase_end <- c(5900, 5299, 4499, 3099)
            
###################################################################################
# IMPORT RASTER DATA
###################################################################################

setwd("YOUR WORKING DIRECTORY")

# Create new folder for outputs
dir.create("./RF")
  dir.create("./RF/Phase01")
  dir.create("./RF/Phase02")
  dir.create("./RF/Phase03")
  dir.create("./RF/Phase04")
  
dir.create("./MaxEnt")
  dir.create("./MaxEnt/Phase01")
  dir.create("./MaxEnt/Phase02")
  dir.create("./MaxEnt/Phase03")
  dir.create("./MaxEnt/Phase04")

dir.create("./Model_Results_tiffs")
  dir.create("./Model_Results_tiffs/ME")
  dir.create("./Model_Results_tiffs/RF")
  
# Read Study area
study_area <- vect("./data/studyarea/AgriChange_SA.shp")
study_area <- sf::st_as_sf(study_area)

# Read environmental data (constant)
DEM<-raster("./data/cropped_data_Buffer/Env_Vars/CHELSA_TraCE21k_DEM_MEAN-58to-23_V1.0_cropped_buf.tif")
Slope<-raster("./data/cropped_data_Buffer/Env_Vars/Slope_tot_cropped_buffer_0.tif")
D_Rivers<-raster("./data/cropped_data_Buffer/Env_Vars/RivDist_cropped_buffer.tif")
D_Lakes<-raster("./data/cropped_data_Buffer/Env_Vars/DistLakes_cropped_buffer.tif")

# Loop for reading data and creating a model for each of the 4 phases
# Make sure your data is organized in folders and named as resulting from script "01_Crop_files_and_Calculate_means"

for (i in 1:length(phase)){

# Read Paleoclimatic data
Bio02<-raster(paste0("./data/cropped_data_Buffer/means/", phase[[i]], "/Bio02_", phase[[i]], "_MEAN_buf.tif"))
Bio04<-raster(paste0("./data/cropped_data_Buffer/means/", phase[[i]], "/Bio04_", phase[[i]], "_MEAN_buf.tif"))
Bio05<-raster(paste0("./data/cropped_data_Buffer/means/", phase[[i]], "/Bio05_", phase[[i]], "_MEAN_buf.tif"))
Bio08<-raster(paste0("./data/cropped_data_Buffer/means/", phase[[i]], "/Bio08_", phase[[i]], "_MEAN_buf.tif"))
Bio09<-raster(paste0("./data/cropped_data_Buffer/means/", phase[[i]], "/Bio09_", phase[[i]], "_MEAN_buf.tif"))
Bio13<-raster(paste0("./data/cropped_data_Buffer/means/", phase[[i]], "/Bio13_", phase[[i]], "_MEAN_buf.tif"))
Bio15<-raster(paste0("./data/cropped_data_Buffer/means/", phase[[i]], "/Bio15_", phase[[i]], "_MEAN_buf.tif"))
Bio17<-raster(paste0("./data/cropped_data_Buffer/means/", phase[[i]], "/Bio17_", phase[[i]], "_MEAN_buf.tif"))
Bio19<-raster(paste0("./data/cropped_data_Buffer/means/", phase[[i]], "/Bio19_", phase[[i]], "_MEAN_buf.tif"))


###################################################################################
# CREATE A SINGLE RASTER BRICK AND DATA FRAME WITH ENVIRONMENTAL VARIABLES
###################################################################################

# Create an extent that includes all the data 
e<-extent(40.47486, 48.35819, 0.09986039, 13.97486)

# Create a raster with equal extent, number of rows and colums 
s<-raster(e, nrows=946, ncols=1665)

# Create raster brick
features<-brick(DEM, Slope, D_Rivers, D_Lakes, Bio02, Bio04, Bio05, Bio08, Bio09, Bio13, Bio15, Bio17, Bio19)

# Rename layers
names(features)[1]  <- "DEM"
names(features)[2]  <- "Slope"
names(features)[3]  <- "D_Rivers"
names(features)[4]  <- "D_Lakes"
names(features)[5]  <- "Bio02"
names(features)[6]  <- "Bio04"
names(features)[7]  <- "Bio05"
names(features)[8]  <- "Bio08"
names(features)[9]  <- "Bio09"
names(features)[10] <- "Bio13"
names(features)[11] <- "Bio15"
names(features)[12] <- "Bio17"
names(features)[13] <- "Bio19"

# Assign CRS (WGS84)
crs(features) <- CRS("+init=epsg:4326")


###################################################################################
# PREPARE PRESENCES DATASET
###################################################################################

# Import presence only dataset
PO <- read.table("./data/Sites/AllSites_BC.csv", header = TRUE, sep = ",")

# Rename columns
names(PO)[names(PO)=="X_muni"] <- "X"
names(PO)[names(PO)=="Y_muni"] <- "Y"
names(PO)[names(PO)=="meanBC"] <- "cal_BC"

# Add Pres_Abs column with only Pres
PO$Pres_Abs <- 1

# Remove points that are outside the study area
PO <- SpatialPointsDataFrame(PO[,c("X", "Y")],PO, proj4string=crs(features))
PO_feat <- raster::extract(features, PO, df=TRUE)
PO_bind <- cbind(PO, PO_feat)
PO_df <- as.data.frame(PO_bind)
PO_df <- na.omit(PO_df)
PO = PO_df[,1:7]

# Create Presence subset for Phase i 
df_pres <- subset(PO, PO$cal_BC >= phase_start[[i]] & PO$cal_BC <= phase_end[[i]])

# Create an object with the number of presences for Random Forest
pres_number <- nrow(df_pres)

###################################################################################
# PREPARE BACKGROUND DATASET FOR MAXENT
###################################################################################

set.seed(123)
# Choose cells that are not NA in the DEM
notna <- which(!is.na(values(DEM)))

# Grab same number of cells as number of presences
samp_bg <- sample(notna, 10000, replace = FALSE)

# And their location coordinates from dem
samplocs_bg <- xyFromCell(DEM, samp_bg)

# Convert to a dataframe with Pres_Abs defined as 0
df_bg <- as.data.frame(samplocs_bg,samp_bg)
names(df_bg) <- c('X', 'Y')
df_bg$Pres_Abs <- 0
df_bg$ID <- "Background"


###################################################################################
# PREPARE PSEUDO-ABSENCES DATASET FOR RANDOM FOREST
###################################################################################

set.seed(123)
# Choose cells that are not NA in the DEM
notna <- which(!is.na(values(DEM)))

# Grab same number of cells as number of presences
samp_abs <- sample(notna, pres_number, replace = FALSE)

# And their location coordinates from dem
samplocs_abs <- xyFromCell(DEM, samp_abs)

# Convert to a dataframe with Pres_Abs defined as 0
df_abs <- as.data.frame(samplocs_abs,samp_abs)
names(df_abs) <- c('X', 'Y')
df_abs$Pres_Abs <- 0
df_abs$ID <- "Pseudo_Abs"


###################################################################################
# COMBINE PRESENCES AND PSEUDO-ABSENCES OR BACKGROUND DATA IN ONE DATASET
###################################################################################

# Remove extra column (ID) (indicate which columns to keep)
df_pres = cbind(df_pres[,1], df_pres[,5:7])
names(df_pres)[1] <- "ID"

# Bind into a single data frame
random_PA <- rbind.data.frame(df_pres,df_abs)
# Bind into a single data frame
random_PBg <- rbind.data.frame(df_pres,df_bg)

# Shuffle the rows
set.seed(123)
shPA<-random_PA[sample(nrow(random_PA), nrow(random_PA)), ]
shPBg<-random_PBg[sample(nrow(random_PBg), nrow(random_PBg)), ]

# Create a spatial data frame
spdfPA <- SpatialPointsDataFrame(shPA[,c("X", "Y")], shPA, proj4string=crs(features))
spdfPBg <- SpatialPointsDataFrame(shPBg[,c("X", "Y")], shPBg, proj4string=crs(features))


###################################################################################
# CREATE A MATRIX WITH DATA AND VARIABLES -> MYDATA
###################################################################################

# Extract environmental variable and data
mydata_RF <- raster::extract(features, spdfPA, df=TRUE)
mydata_ME <- raster::extract(features, spdfPBg, df=TRUE)

# remove extra column (ID)
mydata_RF <- mydata_RF[,2:ncol(mydata_RF)]
mydata_ME <- mydata_ME[,2:ncol(mydata_ME)]

# Create a spatial data frame from mydata to be used in blockCV
spdf_mydata_ME <- cbind(shPBg, mydata_ME)
spdf_mydata_ME <- na.omit(spdf_mydata_ME)
spdf_mydata_ME <- SpatialPointsDataFrame(spdf_mydata_ME[,c("X", "Y")], spdf_mydata_ME, proj4string=crs(features))

# Include Pres_Abs
mydata_RF$Pres_Abs <- spdfPA$Pres_Abs
mydata_ME$Pres_Abs <- spdfPBg$Pres_Abs


###################################################################################
# CALCULATE SPATIAL AUTO CORRELATION
###################################################################################

# Calculate the spatial auto correlation range
sac <- spatialAutoRange(rasterLayer = features,
                        sampleNumber = 5000,
                        doParallel = TRUE,
                        showPlots = FALSE)

# Show spatial autocorrelation range summary
summary(sac)
 

###################################################################################
# SPATIAL BLOCK FOR RANDOM FOREST
###################################################################################

set.seed(123) 
# Spatial Block with 5 folds and block size of 170'000 m
sb_RF <- blockCV::spatialBlock(speciesData = spdfPA,
                            species = "Pres_Abs",
                            rasterLayer = features,
                            theRange = 170000, 
                            k = 5,
                            selection = "random",
                            iteration = 100,
                            biomod2Format = TRUE,
                            xOffset = 0,
                            yOffset = 0)


###################################################################################
# SPATIAL BLOCK FOR MAXENT
###################################################################################

set.seed(123) 
# Spatial Block with 5 folds and block size of 170'000 m
sb_ME <- blockCV::spatialBlock(speciesData = spdfPBg,
                               species = "Pres_Abs",
                               rasterLayer = features,
                               theRange = 170000, 
                               k = 5,
                               selection = "random",
                               iteration = 100,
                               biomod2Format = TRUE,
                               xOffset = 0,
                               yOffset = 0)


###################################################################################
# MaxEnt
###################################################################################

# Extract the folds in Spatial Block object created in the previous section
folds <- sb_ME$folds

# Create a data frame with a column called "prediction" with NA for subsequently storing the prediction values
testTable_ME <- spdf_mydata_ME@data
testTable_ME$pred <- NA

# MaxEnt model fit on training dataset and prediction of testing dataset: 

set.seed(123)

for(k in 1:length(folds)){
  trainSet_ME <- unlist(folds[[k]][1]) # extract the training set indices
  testSet_ME <- unlist(folds[[k]][2]) # extract the testing set indices
  
  # Create objects for MaxEnt model fit 
  p <- mydata_ME$Pres_Abs[trainSet_ME]
  data <- mydata_ME[trainSet_ME, 1:13]
  
  # MaxEnt model fit on training dataset
  maxent <- maxnet(p, data, maxnet.formula(p, data, classes = "default"), addsamplestobackground=TRUE)
                    #, regmult = 1, regfun = maxnet.default.regularization)
                    #maxnet.default.regularization(p, m)
                    #maxnet.formula(p, data, classes="default")
  
  # predict the test set
  testTable_ME[testSet_ME,"pred"] <- predict(maxent, mydata_ME[testSet_ME, ], type = "cloglog")  
  
}  


###################################################################################
# MODEL OUTPUTS
###################################################################################

# Create Response Plots
dev.new()
pdf(file = paste0("./MaxEnt/", phase[[i]], "/RespPlot_", phase[[i]], "_cloglog.pdf"),
     width = 15, height = 15, bg = "white")
plot(maxent, vars = names(maxent$samplemeans), common.scale = TRUE,
     type = "cloglog", ylab = "Prediction")
dev.off()

# Create and save Predictive map
predmap <- predict(features, maxent, clamp=FALSE, type="cloglog")
predmap <- mask(predmap, study_area) # Cut out buffer
writeRaster(predmap, paste0("./MaxEnt/", phase[[i]], "/MaxEnt_pred_", phase[[i]], ".tif"), overwrite=TRUE)
writeRaster(predmap, paste0("./Model_Results_tiffs/ME/MaxEnt_pred_", phase[[i]], ".tif"), overwrite=TRUE)


###################################################################################
# RANDOM FOREST 
###################################################################################

# Extract the folds in Spatial Block object created in the previous section
folds <- sb_RF$folds

# Create a data frame with a column called "pred" with NA for subsequently storing the prediction values
testTable_RF <- spdfPA@data
testTable_RF$pred <- NA

# Random Forest classification: 
set.seed(123)

for(k in 1:length(folds)){
  trainSet_RF <- unlist(folds[[k]][1]) # extract the training set indices
  testSet_RF <- unlist(folds[[k]][2]) # extract the testing set indices
  
  md_train <- mydata_RF[trainSet_RF, ]
  md_test <- mydata_RF[testSet_RF, ]
  md_train$Pres_Abs <- as.factor(md_train$Pres_Abs)
  md_test$Pres_Abs <- as.factor(md_test$Pres_Abs)
  
  # Apply RF algorithm on Pres_Abs values, using as data the points of mydata belonging to trainset subset 
  rf <- randomForest(Pres_Abs~., data=md_train, ntree=1000, mtry=4, na.action = na.omit, importance=TRUE) 
  
  # predict the test set
  testTable_RF[testSet_RF,"pred"] <- predict(rf, md_test, type = "prob") [,2]
}  


###################################################################################
# MODEL OUTPUTS 
###################################################################################

#--------------------------------------
# Create and save Prediction map
#--------------------------------------
pred <- predict(features, rf, type="prob", index=2, na.rm=FALSE)
pred <- mask(pred, study_area) # Cut out buffer
writeRaster(pred, paste0("./RF/", phase[[i]], "/RF_pred_", phase[[i]], ".tif"), overwrite=TRUE)
writeRaster(pred, paste0("./Model_Results_tiffs/RF/RF_pred_", phase[[i]], ".tif"), overwrite=TRUE)

#--------------------------------------
# Plot variable importance
#--------------------------------------

dev.new()  
pdf(file = paste0("./RF/", phase[[i]], "/VarImp_", phase[[i]], ".pdf"),
     width = 5, height = 9, bg = "white")
varImpPlot(rf, type=1, main = "")
dev.off()

#--------------------------------------
# Partial dependence plots
#--------------------------------------
dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_DEM_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, DEM, "1", main="DEM")
abline(h=0)
dev.off()

dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_Slope_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, Slope, "1", main="Slope")
abline(h=0)
dev.off()

dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_D_Lakes_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, D_Lakes, "1", main="Distance to Lakes")
abline(h=0)
dev.off()

dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_D_Rivers_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, D_Rivers, "1", main="Distance to Rivers")
abline(h=0)
dev.off()

dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_Bio02_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, Bio02, "1", main="Bio02")
abline(h=0)
dev.off()

dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_Bio04_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, Bio04, "1", main="Bio04")
abline(h=0)
dev.off()

dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_Bio05_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, Bio05, "1", main="Bio05")
abline(h=0)
dev.off()

dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_Bio08_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, Bio08, "1", main="Bio08")
abline(h=0)
dev.off()

dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_Bio09_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, Bio09, "1", main="Bio09")
abline(h=0)
dev.off()

dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_Bio13_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, Bio13, "1", main="Bio13")
abline(h=0)
dev.off()

dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_Bio15_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, Bio15, "1", main="Bio15")
abline(h=0)
dev.off()

dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_Bio17_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, Bio17, "1", main="Bio17")
abline(h=0)
dev.off()

dev.new()
pdf(file = paste0("./RF/", phase[[i]], "/PP_Bio19_", phase[[i]], ".pdf"), 
     width= 7, height = 7, bg = "white")
partialPlot(rf, md_train, Bio19, "1", main="Bio19")
abline(h=0)
dev.off()


###################################################################################
# MODEL PERFORMANCE FOR BOTH MODELS
###################################################################################

ggROC <- ggplot(testTable_ME[testSet_ME, ], aes(m=pred, d=Pres_Abs)) + geom_roc(n.cuts=0, color='red') +
  coord_equal() + geom_abline(intercept = 0, slope = 1) + theme_bw() +
  labs(x="False positive rate (1 - specificity)", y="True positive rate (sensitivity)")
auc_ME <- calc_auc(ggROC) [3]

ggROC <- ggplot(testTable_RF[testSet_RF, ], aes(m=pred, d=Pres_Abs)) + geom_roc(n.cuts=0, color='red') +
  coord_equal() + geom_abline(intercept = 0, slope = 1) + theme_bw() +
  labs(x="False positive rate (1 - specificity)", y="True positive rate (sensitivity)")
auc_RF <- calc_auc(ggROC) [3]

# ROC Curve for the test data set for both models:
ggROC <- ggplot(NULL, aes(m=pred, d=Pres_Abs)) +
  geom_roc(data = testTable_ME[testSet_ME, ], n.cuts=0, color='red') +
  geom_roc(data = testTable_RF[testSet_RF, ], n.cuts=0, color='blue') +
  coord_equal() + geom_abline(intercept = 0, slope = 1) + theme_bw() +
  labs(x="False positive rate (1 - specificity)", y="True positive rate (sensitivity)")
plot(ggROC + ggtitle(paste(phase[[i]], "- Testing dataset"), subtitle=paste0("AUC Maxent (red): ", signif(auc_ME, 4),"\n", "AUC RandomForest (blue): ", signif(auc_RF, 4))))
ggsave(path = paste0("./RF/", phase[[i]], "/"), filename = paste0("ROC_testing_data_ME_RF_", phase[[i]], ".pdf"), width = 4, height = 4)
ggsave(path = paste0("./MaxEnt/", phase[[i]], "/"), filename = paste0("ROC_testing_data_ME_RF_", phase[[i]], ".pdf"), width = 4, height = 4)


#--------------------------------------

# Close all devices and clear workspace
graphics.off()
rm(list= ls()[!(ls() %in% c('phase','phase_end','phase_start','study_area','DEM','Slope','D_Rivers','D_Lakes'))])

}

