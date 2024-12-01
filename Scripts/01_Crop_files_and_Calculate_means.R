
# Load libraries
library(raster)
library(terra)
library(sf)

# Set your working directory
setwd("YOUR WORKING DIRECTORY")

# Create new folders for inputs and outputs
dir.create("./data")
dir.create("./data/OriginalData")
dir.create("./data/cropped_data_Buffer")

# Create input folders for cropped paleoclimatic data and means 
Folders <- c("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10", 
             "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19",
             "means")

Phases <- c("Phase01", "Phase02", "Phase03", "Phase04")

for (f in 1:length(Folders)){
  dir.create(paste0("./data/cropped_data_Buffer/", Folders[[f]]))
  for (p in 1:length(Phases)){
    dir.create(paste0("./data/cropped_data_Buffer/", Folders[[f]], "/", Phases[[p]]))
  }
}

# Load the Study area with buffer
# Here we use a study area with a buffer to avoid issues at the border of the study area. 
study_area <- vect("./data/studyarea/AgriChange_SA_Buffer.shp")
study_area <- sf::st_as_sf(study_area)

## CREATE FUNCTION AND APPLY TO ALL TIFF FILES IN DATA ##
#--------------------------------------------------------
# To execute this part of the script, locate all the tiff files you need to crop in the "OriginalData" folder in the "data" folder 
# in your working directory. Otherwise adapt paths accordingly. 

# Create a loop function to crop the files
myfun <- function(path){
  # Isolate the file name from the path
  file_name <- basename(tools::file_path_sans_ext(path))
  # Create an object with the raster you are working on
  clim <- raster(path)
  # Clip the raster on the study area and save with the appendix _cropped
  clim_clip <- crop(clim, study_area)
  clim_clip <- mask(clim_clip, study_area, filename=paste0("./data/cropped_data_Buffer/",file_name,"_cropped_buffer.tif"), overwrite=TRUE)
}

# Read all files in your data folder (make sure all the files you need to crop are in your OriginalData folder or adapt accordingly)
raster_files <- list.files("./data/OriginalData", pattern = "tif", full.names = TRUE)

# Apply your function to all files
lapply(raster_files, myfun)


## CALCULATE MEAN VALUES FROM RASTER FILES IN EACH BIO FOLDER FOR THE ENTIRE STUDY PERIOD ##
#-------------------------------------------------------------------------------------------
# Before executing this part of the script, make sure to classify all the tiff files cropped using the script above in the folders
# called "Bio01", "Bio02" etc. within your "data" folder. 

# Create a list of all paleoclimatic variables to be extracted (for all Bio variables)
Bio_vars_all <- c("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10",
                  "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19")

# For all "BioXX" folders: 
for (k in 1:length(Bio_vars_all)){
  
    # Read all files in the corresponding folder
    raster_cropped_files = list.files(paste0("./data/cropped_data_Buffer/", Bio_vars_all[[k]]), pattern="\\.tif$", full.names = TRUE)
    # Create a raster stack, calculate the mean value for each cell and write the new raster file in a folder called "means". 
    cropped_files_stack <- stack(raster_cropped_files)
    mean_cropped_files <- calc(cropped_files_stack, mean)
    raster::writeRaster(mean_cropped_files, filename=paste0("./data/cropped_data_Buffer/means/", Bio_vars_all[[k]], "_MEAN_buf.tif"), overwrite=FALSE)
}


## CALCULATE MEAN VALUES FROM RASTER FILES IN EACH BIO FOLDER FOR EACH PHASE ##
#------------------------------------------------------------------------------
# Before executing this part of the script, make sure to copy all the tiff files in each "BioXX" folder and classify them according 
# to their phase in the folders "Phase01", "Phase02", "Phase03" and "Phase04" within the "BioXX" folder. 

# Create a list of all paleoclimatic variables to be extracted (for all Bio variables; not necessary if script above has already been run)
Bio_vars_all <- c("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10",
                  "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19")

# Create list of Phases
Phases <- c("Phase01", "Phase02", "Phase03", "Phase04")

# For all "BioXX" folders: 
for (k in 1:length(Bio_vars_all)){
  # And for all "PhaseXX" folders: 
  for(i in 1:length(Phases)){
    
    # Read all files in the corresponding folder
    raster_cropped_files = list.files(paste0("./data/cropped_data_Buffer/", Bio_vars_all[[k]], "/", Phases[[i]]), pattern="\\.tif$", full.names = TRUE)
    # Create a raster stack, calculate the mean value for each cell and write the new raster file in a "PhaseXX" folder 
    # inside your "means" folder. 
    cropped_files_stack <- stack(raster_cropped_files)
    mean_cropped_files <- calc(cropped_files_stack, mean)
    raster::writeRaster(mean_cropped_files, filename=paste0("./data/cropped_data_Buffer/means/", Phases[[i]], "/", Bio_vars_all[[k]], "_", Phases[[i]], "_MEAN_buf.tif"), overwrite=FALSE)
  }
}



####################################################################################################################
## REPEAT THE WOHLE PROCEDURE CROPPING THE DATA TO THE STUDY AREA WITHOUT BUFFER FOR CALCULATING CORRELATION TEST ##
####################################################################################################################

# Create new folders for outputs
dir.create("./data/cropped_data")

# Create input folders for cropped paleoclimatic data and means 
Folders <- c("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10", 
             "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19",
             "means")
Phases <- c("Phase01", "Phase02", "Phase03", "Phase04")

for (f in 1:length(Folders)){
  dir.create(paste0("./data/cropped_data/", Folders[[f]]))
  for (p in 1:length(Phases)){
    dir.create(paste0("./data/cropped_data/", Folders[[f]], "/", Phases[[p]]))
  }
}

# Load the Study area without buffer
study_area <- vect("./data/studyarea/AgriChange_SA.shp")
study_area <- sf::st_as_sf(study_area)


## CREATE FUNCTION AND APPLY TO ALL TIFF FILES IN DATA ##
#--------------------------------------------------------
# To execute this part of the script, locate all the tiff files you need to crop in the "OriginalData" folder in the "data" folder 
# in your working directory. Otherwise adapt paths accordingly. 

# Create a loop function to crop the files
myfun <- function(path){
  # Isolate the file name from the path
  file_name <- basename(tools::file_path_sans_ext(path))
  # Create an object with the raster you are working on
  clim <- raster(path)
  # Clip the raster on the study area and save with the appendix _cropped
  clim_clip <- crop(clim, study_area)
  clim_clip <- mask(clim_clip, study_area, filename=paste0("./data/cropped_data/",file_name,"_cropped.tif"), overwrite=TRUE)
}

# Read all files in your data folder (make sure all the files you need to crop are in your OriginalData folder or adapt accordingly)
raster_files <- list.files("./data/OriginalData", pattern = "tif", full.names = TRUE)

# Apply your function to all files
lapply(raster_files, myfun)


## CALCULATE MEAN VALUES FROM RASTER FILES IN EACH BIO FOLDER FOR THE ENTIRE STUDY PERIOD ##
#-------------------------------------------------------------------------------------------
# Before executing this part of the script, make sure to classify all the tiff files cropped using the script above in the folders
# called "Bio01", "Bio02" etc. within your "data" folder. 

# Create a list of all paleoclimatic variables to be extracted (for all Bio variables)
Bio_vars_all <- c("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10",
                  "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19")

# For all "BioXX" folders: 
for (k in 1:length(Bio_vars_all)){
  
  # Read all files in the corresponding folder
  raster_cropped_files = list.files(paste0("./data/cropped_data/", Bio_vars_all[[k]]), pattern="\\.tif$", full.names = TRUE)
  # Create a raster stack, calculate the mean value for each cell and write the new raster file in a folder called "means". 
  cropped_files_stack <- stack(raster_cropped_files)
  mean_cropped_files <- calc(cropped_files_stack, mean)
  raster::writeRaster(mean_cropped_files, filename=paste0("./data/cropped_data/means/", Bio_vars_all[[k]], "_MEAN.tif"), overwrite=FALSE)
}


## CALCULATE MEAN VALUES FROM RASTER FILES IN EACH BIO FOLDER FOR EACH PHASE ##
#------------------------------------------------------------------------------
# Before executing this part of the script, make sure to copy all the tiff files in each "BioXX" folder and classify them according 
# to their phase in the folders "Phase01", "Phase02", "Phase03" and "Phase04" within the "BioXX" folder. 

# Create a list of all paleoclimatic variables to be extracted (for all Bio variables; not necessary if script above has already been run)
Bio_vars_all <- c("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10",
                  "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19")

# Create list of Phases
Phases <- c("Phase01", "Phase02", "Phase03", "Phase04")

# For all "BioXX" folders: 
for (k in 1:length(Bio_vars_all)){
  # And for all "PhaseXX" folders: 
  for(i in 1:length(Phases)){
    
    # Read all files in the corresponding folder
    raster_cropped_files = list.files(paste0("./data/cropped_data/", Bio_vars_all[[k]], "/", Phases[[i]]), pattern="\\.tif$", full.names = TRUE)
    # Create a raster stack, calculate the mean value for each cell and write the new raster file in a "PhaseXX" folder 
    # inside your "means" folder. 
    cropped_files_stack <- stack(raster_cropped_files)
    mean_cropped_files <- calc(cropped_files_stack, mean)
    raster::writeRaster(mean_cropped_files, filename=paste0("./data/cropped_data/means/", Phases[[i]], "/", Bio_vars_all[[k]], "_", Phases[[i]], "_MEAN.tif"), overwrite=FALSE)
  }
}
