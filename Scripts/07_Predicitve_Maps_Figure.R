###################################################################################
#                               CASTIELLO ET AL.                                  #
#                               ----------------                                  #
#                   GENERIC SRIPT TO CREATE PREDICTIVE MAPS                       #
#                                                                                 #
#                             #  November 2024  #                                 #
#                                                                                 #
#                                                                                 #
###################################################################################


## READ LIBRARIES, SET DIRECTORY AND CREATE OUTPUT FOLDER ##
#-----------------------------------------------------------

# Load libraries
library(raster)
library(terra)
library(RColorBrewer) 
library(tmap) 

# Set your working directory
setwd("YOUR WORKING DIRECTORY")

# Create new folder for outputs
dir.create("./RF_Maxent_Maps")

## READ AND PREPARE DATA ##
#--------------------------

# Load the maps and assemble them to a raster brick
raster_files <- list.files("./Model_Results_tiffs/RF", pattern = "tif", full.names = TRUE)
RF_tiff_maps <- lapply(raster_files, raster)
RF_tiff_maps <- brick(RF_tiff_maps)

raster_files <- list.files("./Model_Results_tiffs/ME", pattern = "tif", full.names = TRUE)
ME_tiff_maps <- lapply(raster_files, raster)
ME_tiff_maps <- brick(ME_tiff_maps)


# Load the coast line shapefile
coast_line <- vect("./data/Coast_Line/Europe_coastline_poly.shp")
coast_line <- sf::st_as_sf(coast_line)

# Load Hillshade and DEM raster
hillshade <- raster("./data/Hillshade/hillshade.tif")
DEM <- raster("./data/cropped_data_Buffer/Env_Vars/CHELSA_TraCE21k_DEM_MEAN-58to-23_V1.0_cropped_buf.tif")
# Crop the DEM to the right extent
DEM <- crop(DEM, RF_tiff_maps[[1]])

# Import presence only dataset
PO <- read.table("./data/Sites/AllSites_BC.csv", header = TRUE, sep = ",")

# Rename columns
names(PO)[names(PO)=="X_muni"] <- "X"
names(PO)[names(PO)=="Y_muni"] <- "Y"
names(PO)[names(PO)=="meanBC"] <- "cal_BC"

# Remove points that are outside the study area
PO <- SpatialPointsDataFrame(PO[,c("X", "Y")],PO, proj4string=crs(DEM))
PO_map <- raster::extract(DEM, PO, df=TRUE)
PO_bind <- cbind(PO, PO_map)
PO_df <- as.data.frame(PO_bind)
PO_df <- na.omit(PO_df)

# Create subset with points for each phase and create spatial points dataframes
PO_p1 <- subset(PO_df, PO_df$cal_BC >= 5300 & PO_df$cal_BC <= 5900)
PO_p1 <- SpatialPointsDataFrame(PO_p1[,c("X", "Y")],PO_p1, proj4string=crs(DEM))
PO_p2 <- subset(PO_df, PO_df$cal_BC >= 4500 & PO_df$cal_BC <= 5299)
PO_p2 <- SpatialPointsDataFrame(PO_p2[,c("X", "Y")],PO_p2, proj4string=crs(DEM))
PO_p3 <- subset(PO_df, PO_df$cal_BC >= 3100 & PO_df$cal_BC <= 4499)
PO_p3 <- SpatialPointsDataFrame(PO_p3[,c("X", "Y")],PO_p3, proj4string=crs(DEM))
PO_p4 <- subset(PO_df, PO_df$cal_BC >= 2300 & PO_df$cal_BC <= 3099)
PO_p4 <- SpatialPointsDataFrame(PO_p4[,c("X", "Y")],PO_p4, proj4string=crs(DEM))

# Create a list of spatial points data frames
POs <- list(PO_p1, PO_p2, PO_p3, PO_p4)


## PREPARE COLOR PALETTES, LABELS AND THEME ##
#---------------------------------------------

# Define color palette for hillshade
pal_greys <- hcl.colors(1000, "Grays")

# Create list for panel labels
Model_Phases <- c("a)                                                                               ", 
                  "b)                                                                               ", 
                  "c)                                                                               ", 
                  "d)                                                                               ")

# Create map theme
theme_tmaps <- tm_layout(
  frame=FALSE, # No frame
  frame.lwd = NA, # No frame
  
  panel.label.bg.color="white", # Change panel label to white
  panel.label.fontface="bold", # Bold panel label
  panel.label.height = 1,
  panel.label.size = 0.7,
  
  legend.outside = TRUE,
  legend.outside.position = "top",
  legend.outside.size = 0.2,
  legend.format = list(fun = function(x) {
    ifelse(x %in% c(0, 1), x, "")
  }),
  
  legend.text.size = 0.5,
  legend.title.size = 1,
  legend.title.fontface = "bold"
)


## CREATE AND SAVE MAPS PER PHASE AND MODEL ##
# --------------------------------------------

## RANDOM FOREST MAPS ##
# ----------------------

map1 <-  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, legend.show = FALSE, showNA = F) +
  
  tm_shape(coast_line) +
  tm_borders(col= "black") +
  
  tm_shape(RF_tiff_maps[[1]]) +
  tm_raster(style = "cont", n=20, palette="-PRGn", showNA = F,
            legend.show = FALSE,
            legend.is.portrait = FALSE,
            title = "Suitability",
            labels = c("LOW: 0", rep("", times = 4), "HIGH: 1")) +
  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, alpha = 0.2, legend.show = FALSE, showNA = F) +
  
  # tm_shape(PO_p1) +                                                               # Uncomment these lines to add the points
  # tm_symbols(shape = 23, size = 0.02, border.col = "black", col = "orange") +     # Uncomment these lines to add the points
  
  tm_graticules(lwd = 0.05,col="grey75",n.x=2,n.y=3,labels.size = 0.5) +
  
  tm_layout(panel.labels = Model_Phases[[1]]) +
  
  theme_tmaps

tmap_save(tm = map1, filename = "./RF_Maxent_Maps/Figure_X_RF_no_sites_phase1.tiff", width = 8, height = 7, units = "cm",dpi = 600)


map2 <-  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, legend.show = FALSE, showNA = F) +
  
  tm_shape(coast_line) +
  tm_borders(col= "black") +
  
  tm_shape(RF_tiff_maps[[2]]) +
  tm_raster(style = "cont", n=20, palette="-PRGn", showNA = F,
            legend.show = FALSE,
            legend.is.portrait = FALSE,
            title = "Suitability",
            labels = c("LOW: 0", rep("", times = 4), "HIGH: 1")) +
  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, alpha = 0.2, legend.show = FALSE, showNA = F) +
  
  # tm_shape(PO_p2) +                                                               # Uncomment these lines to add the points
  # tm_symbols(shape = 23, size = 0.02, border.col = "black", col = "orange") +     # Uncomment these lines to add the points
  
  tm_graticules(lwd = 0.05,col="grey75",n.x=2,n.y=3,labels.size = 0.5) +
  
  tm_layout(panel.labels = Model_Phases[[2]]) +
  
  theme_tmaps

tmap_save(tm = map2, filename = "./RF_Maxent_Maps/Figure_X_RF_no_sites_phase2.tiff", width = 8, height = 7, units = "cm",dpi = 600)


map3 <-  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, legend.show = FALSE, showNA = F) +
  
  tm_shape(coast_line) +
  tm_borders(col= "black") +
  
  tm_shape(RF_tiff_maps[[3]]) +
  tm_raster(style = "cont", n=20, palette="-PRGn", showNA = F,
            legend.show = FALSE,
            legend.is.portrait = FALSE,
            title = "Suitability",
            labels = c("LOW: 0", rep("", times = 4), "HIGH: 1")) +
  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, alpha = 0.2, legend.show = FALSE, showNA = F) +
  
  # tm_shape(PO_p3) +                                                               # Uncomment these lines to add the points
  # tm_symbols(shape = 23, size = 0.02, border.col = "black", col = "orange") +     # Uncomment these lines to add the points
  
  tm_graticules(lwd = 0.05,col="grey75",n.x=2,n.y=3,labels.size = 0.5) +
  
  tm_layout(panel.labels = Model_Phases[[3]]) +
  
  theme_tmaps

tmap_save(tm = map3, filename = "./RF_Maxent_Maps/Figure_X_RF_no_sites_phase3.tiff", width = 8, height = 7, units = "cm",dpi = 600)


map4 <-  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, legend.show = FALSE, showNA = F) +
  
  tm_shape(coast_line) +
  tm_borders(col= "black") +
  
  tm_shape(RF_tiff_maps[[4]]) +
  tm_raster(style = "cont", n=20, palette="-PRGn", showNA = F,
            legend.show = FALSE,
            legend.is.portrait = FALSE,
            title = "Suitability",
            labels = c("LOW: 0", rep("", times = 4), "HIGH: 1")) +
  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, alpha = 0.2, legend.show = FALSE, showNA = F) +
  
  # tm_shape(PO_p4) +                                                               # Uncomment these lines to add the points
  # tm_symbols(shape = 23, size = 0.02, border.col = "black", col = "orange") +     # Uncomment these lines to add the points
  
  tm_graticules(lwd = 0.05,col="grey75",n.x=2,n.y=3,labels.size = 0.5) +
  
  tm_layout(panel.labels = Model_Phases[[4]]) +
  
  theme_tmaps

tmap_save(tm = map4, filename = "./RF_Maxent_Maps/Figure_X_RF_no_sites_phase4.tiff", width = 8, height = 7, units = "cm",dpi = 600)

# Create separate legend
legend <-  
  tm_shape(RF_tiff_maps[[1]]) +
  tm_raster(style = "cont", n=20, palette="-PRGn", showNA = F,
            legend.show = TRUE,
            legend.is.portrait = FALSE,
            title = "Suitability",
            labels = c("LOW: 0", rep("", times = 4), "HIGH: 1")) +
  tm_layout(legend.only = TRUE) +
  theme_tmaps

tmap_save(tm = legend, filename = "./RF_Maxent_Maps/Figure_X_RF_sites_legend.tiff", width = 8, height = 7, units = "cm",dpi = 600)


## MAXENT MAPS ##
# ---------------

map1 <-  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, legend.show = FALSE, showNA = F) +
  
  tm_shape(coast_line) +
  tm_borders(col= "black") +
  
  tm_shape(ME_tiff_maps[[1]]) +
  tm_raster(style = "cont", n=20, palette="-PRGn", showNA = F,
            legend.show = FALSE,
            legend.is.portrait = FALSE,
            title = "Suitability",
            labels = c("LOW: 0", rep("", times = 4), "HIGH: 1")) +
  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, alpha = 0.2, legend.show = FALSE, showNA = F) +
  
  # tm_shape(PO_p1) +                                                               # Uncomment these lines to add the points
  # tm_symbols(shape = 23, size = 0.02, border.col = "black", col = "orange") +     # Uncomment these lines to add the points
  
  tm_graticules(lwd = 0.05,col="grey75",n.x=2,n.y=3,labels.size = 0.5) +
  
  tm_layout(panel.labels = Model_Phases[[1]]) +
  
  theme_tmaps

tmap_save(tm = map1, filename = "./RF_Maxent_Maps/Figure_X_ME_no_sites_phase1.tiff", width = 8, height = 7, units = "cm",dpi = 600)


map2 <-  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, legend.show = FALSE, showNA = F) +
  
  tm_shape(coast_line) +
  tm_borders(col= "black") +
  
  tm_shape(ME_tiff_maps[[2]]) +
  tm_raster(style = "cont", n=20, palette="-PRGn", showNA = F,
            legend.show = FALSE,
            legend.is.portrait = FALSE,
            title = "Suitability",
            labels = c("LOW: 0", rep("", times = 4), "HIGH: 1")) +
  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, alpha = 0.2, legend.show = FALSE, showNA = F) +
  
  # tm_shape(PO_p2) +                                                               # Uncomment these lines to add the points
  # tm_symbols(shape = 23, size = 0.02, border.col = "black", col = "orange") +     # Uncomment these lines to add the points
  
  tm_graticules(lwd = 0.05,col="grey75",n.x=2,n.y=3,labels.size = 0.5) +
  
  tm_layout(panel.labels = Model_Phases[[2]]) +
  
  theme_tmaps

tmap_save(tm = map2, filename = "./RF_Maxent_Maps/Figure_X_ME_no_sites_phase2.tiff", width = 8, height = 7, units = "cm",dpi = 600)


map3 <-  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, legend.show = FALSE, showNA = F) +
  
  tm_shape(coast_line) +
  tm_borders(col= "black") +
  
  tm_shape(ME_tiff_maps[[3]]) +
  tm_raster(style = "cont", n=20, palette="-PRGn", showNA = F,
            legend.show = FALSE,
            legend.is.portrait = FALSE,
            title = "Suitability",
            labels = c("LOW: 0", rep("", times = 4), "HIGH: 1")) +
  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, alpha = 0.2, legend.show = FALSE, showNA = F) +
  
  # tm_shape(PO_p3) +                                                               # Uncomment these lines to add the points
  # tm_symbols(shape = 23, size = 0.02, border.col = "black", col = "orange") +     # Uncomment these lines to add the points
  
  tm_graticules(lwd = 0.05,col="grey75",n.x=2,n.y=3,labels.size = 0.5) +
  
  tm_layout(panel.labels = Model_Phases[[3]]) +
  
  theme_tmaps

tmap_save(tm = map3, filename = "./RF_Maxent_Maps/Figure_X_ME_no_sites_phase3.tiff", width = 8, height = 7, units = "cm",dpi = 600)


map4 <-  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, legend.show = FALSE, showNA = F) +
  
  tm_shape(coast_line) +
  tm_borders(col= "black") +
  
  tm_shape(ME_tiff_maps[[4]]) +
  tm_raster(style = "cont", n=20, palette="-PRGn", showNA = F,
            legend.show = FALSE,
            legend.is.portrait = FALSE,
            title = "Suitability",
            labels = c("LOW: 0", rep("", times = 4), "HIGH: 1")) +
  
  tm_shape(hillshade) +
  tm_raster(style = "cont", n=10, palette=pal_greys, alpha = 0.2, legend.show = FALSE, showNA = F) +
  
  # tm_shape(PO_p4) +                                                               # Uncomment these lines to add the points
  # tm_symbols(shape = 23, size = 0.02, border.col = "black", col = "orange") +     # Uncomment these lines to add the points
  
  tm_graticules(lwd = 0.05,col="grey75",n.x=2,n.y=3,labels.size = 0.5) +
  
  tm_layout(panel.labels = Model_Phases[[4]]) +
  
  theme_tmaps

tmap_save(tm = map4, filename = "./RF_Maxent_Maps/Figure_X_ME_no_sites_phase4.tiff", width = 8, height = 7, units = "cm",dpi = 600)

