
# Load libraries
library(raster)
library(terra)
library(ggplot2)
library(viridis)
library(stringr)
library(ggalluvial)

# Set your working directory
setwd("YOUR WORKING DIRECTORY")

# Create new folder for outputs
dir.create("./Sites_Plots")

# Create a list of all paleoclimatic variables to be extracted (for all Bio variables)
Bio_vars_all <- c("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10",
                  "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19")

# Create a list of all variables to be extracted (for all variables)
All_vars <- c("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10", 
              "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19",
              "DEM", "Slope", "TRI", "D_Lakes", "D_Rivers")

# Create a list of all phases to be extracted (for all phases)
All_phases <- c("Phase01", "Phase02", "Phase03", "Phase04")


## CREATE A DATAFRAME WITH ALL SITES ##
#--------------------------------------

# Read the site table
# Make sure you saved the site dataset (csv) in a folder "Sites" within your "data" folder.
Sites <- read.table("./data/Sites/AllSites_BC.csv", header = TRUE, sep = ",")

# Round down mean BC values with 0.5, so that e.g. 3499.5 still belongs to 100-year step 3400-3500
Sites$meanBC <- floor(Sites$meanBC)

# Create a Column containing the name of the corresponding layer in the raster stack
Sites$layername <- paste0("Years_",str_sub(Sites$meanBC,0,-3),"00")


## CREATE A DATAFRAME WITH THE VALUE OF THE CLIMATIC VARIABLES CORRESPONDING TO THE DATE OF EACH SITE ##
#-------------------------------------------------------------------------------------------------------

# Prepare site_stats table with the columns of the Sites table
site_stats <- Sites

# Loop to extract Bio variable data from all bio variables
for (k in 1:length(Bio_vars_all)){
  
  # Read the data into a list
  rastFiles = list.files(paste0("./data/cropped_data_Buffer/", Bio_vars_all[[k]]), pattern="\\.tif$", full.names = TRUE)
  
  # Create a raster stack with all 36 variable tif files
  bio_variables <- stack(rastFiles)
  
  # Rename the columns to match the index "layername" in the Sites dataframe
  names(bio_variables) <-  paste0("Years_", seq(2300, 5800, by=100))
  
  # Extract value from the layer from the stack that is called equally as the value in column layername 
  # (get the value of the variable for the correct date)
  Bio_values <- raster::extract(bio_variables, cbind(Sites$X_muni, Sites$Y_muni))[
    cbind(1:nrow(Sites),match(Sites$layername, names(bio_variables)))
  ]
  
  # Turn the extracted Bio values into a data frame, rename the new column accordingly and bind with the site_stats table
  Bio_values <- as.data.frame(Bio_values)
  names(Bio_values)[1] <- Bio_vars_all[[k]]
  site_stats <- cbind(site_stats, Bio_values)

}

# Create column with phases
site_stats$Phase <- NA
site_stats$Phase[site_stats$meanBC >= 5300 & site_stats$meanBC <= 5900] <- "Phase01"
site_stats$Phase[site_stats$meanBC >= 4500 & site_stats$meanBC <= 5299] <- "Phase02"
site_stats$Phase[site_stats$meanBC >= 3100 & site_stats$meanBC <= 4499] <- "Phase03"
site_stats$Phase[site_stats$meanBC >= 2300 & site_stats$meanBC <= 3099] <- "Phase04"


## READ ENVIRONMENTAL DATA AND EXTRACT THEIR CORRESPONDING VALUES TO THE DATAFRAME ##
#------------------------------------------------------------------------------------

# Read the environmental data
DEM      <- raster("./data/cropped_data_Buffer/Env_Vars/CHELSA_TraCE21k_DEM_MEAN-58to-23_V1.0_cropped_buf.tif")
Slope    <- raster("./data/cropped_data_Buffer/Env_Vars/Slope_tot_cropped_buffer.tif")
TRI      <- raster("./data/cropped_data_Buffer/Env_Vars/TRI_cropped_buffer.tif")
D_lakes  <- raster("./data/cropped_data_Buffer/Env_Vars/DistLakes_cropped_buffer.tif")
D_rivers <- raster("./data/cropped_data_Buffer/Env_Vars/RivDist_cropped_buffer.tif")

# Create a Slope raster with 0 instead of NA's to avoid data loss by the sea
Slope[is.na(Slope[])] <- 0
study_area <- vect("./data/studyarea/AgriChange_SA_Buffer.shp")
study_area <- sf::st_as_sf(study_area)
Slope <- crop(Slope, study_area)
Slope <- mask(Slope, study_area, filename="./data/cropped_data_Buffer/Env_Vars/Slope_tot_cropped_buffer_0.tif", overwrite=TRUE)

# Unite the variables in a single stack
env_variables <- stack(DEM, Slope, TRI, D_lakes, D_rivers)

# Extract value of the environmental variables from the layers in the stack 
Env_values <- raster::extract(env_variables, cbind(Sites$X_muni, Sites$Y_muni))

# Bind with the table containing the values of the sites and the climatic variables
site_stats <- cbind(site_stats, Env_values)

# Rename columns to match All_vars
names(site_stats)[names(site_stats)=="CHELSA_TraCE21k_DEM_MEAN.58to.23_V1.0_cropped_buf"] <- "DEM"
names(site_stats)[names(site_stats)=="Slope_tot_cropped_buffer_0"] <- "Slope"
names(site_stats)[names(site_stats)=="TRI_cropped_buffer"] <- "TRI"
names(site_stats)[names(site_stats)=="DistLakes_cropped_buffer"] <- "D_Lakes"
names(site_stats)[names(site_stats)=="RivDist_cropped_buffer"] <- "D_Rivers"

# Replace NA's in TRI with 0 to avoid data loss
site_stats$TRI[is.na(site_stats$TRI)] <- 0

# Omit NA's to erase sites outside the study area or the study period
site_stats <- na.omit(site_stats)

## PLOT THE DISTRIBUTION OF THE SITES ON THE VARIABLE VALUES FOR EACH CLIMATIC AND ENVIRONMENTAL VARIABLE ##
#-----------------------------------------------------------------------------------------------------------

# Create a list of Variable names to be used in the plots
Var_names <- c("Annual Mean Temperature",
               "Mean Diurnal Temperature Range",
               "Temperature Seasonality",
               "Maximal Temperature of Warmest Month",
               "Minimal Temperature of Coldest Month",
               "Temperature Annual Range",
               "Mean Temperature of Wettest Quarter",
               "Mean Temperature of Driest Quarter",
               "Mean Temperature of Warmest Quarter", 
               "Mean Temperature of Coldest Quarter",
               "Annual Precipitation",
               "Precipitation of Wettest Month", 
               "Precipitation of Driest Month", 
               "Precipitation Seasonality", 
               "Precipitation of Wettest Quarter",
               "Precipitation of Driest Quarter",
               "Precipitation of Warmest Quarter", 
               "Precipitation of Coldest Quarter",
               "Altitude",
               "Slope",
               "Terrain Ruggedness Index",
               "Distance to Lakes",
               "Distance to Rivers")

# Create Object with X-Axis labels (units) to be used in the plots
Units <- c("Temperature [°C]",
           "Temperature [°C]",
           "Standard Deviation [°C * 100]",
           "Temperature [°C]",
           "Temperature [°C]",
           "Temperature [°C]",
           "Temperature [°C]",
           "Temperature [°C]",
           "Temperature [°C]",
           "Temperature [°C]",
           "kg m-2 year-1",
           "kg m-2 year-1",
           "kg m-2 year-1",
           "Coefficient of Varation [%]",
           "kg m-2 year-1",
           "kg m-2 year-1",
           "kg m-2 year-1",
           "kg m-2 year-1",
           "Meters Above Sea Level", 
           "Degrees [°]",
           "TRI [m]",
           "Distance [m]",
           "Distance [m]")


# Create theme to be used for the plots
theme_plots <- function(...) {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 7),
      plot.subtitle = element_text(hjust = 0.5, size = 5),
      strip.text = element_text(size = 5),
      axis.title = element_text(size = 5),
      axis.text.x = element_text(angle=45, hjust=1, size = 4),
      axis.text.y = element_text(hjust=1, size = 4),
      axis.ticks.x = element_line(color = "grey", linewidth = 0.1),
      axis.ticks.length = unit(2, "pt"),
      legend.key.size = unit(7, 'pt'),
      legend.title = element_text(size = 5),
      legend.text = element_text(size = 5),
      legend.position="bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey", linewidth = 0.075),
      panel.grid.minor = element_blank(),
      legend.margin = ggplot2::margin(0,0,0,0),
      ...
    ) 
}

# Avoid scientific notation
options(scipen=1000000)

# Create combined histogram & boxplot per phase and per site type for each variable
for (k in 1:length(All_vars)){
  
  p <-  ggplot(site_stats, aes_string(x=All_vars[[k]])) +
    geom_histogram(aes(y = (..count..)/sum(..count..), fill=TypeSite), alpha=0.8) + coord_flip() +
    scale_fill_manual(values = viridis(n=6)) +
    scale_y_continuous(labels=scales::percent, breaks = seq(0, 1, by = 0.05)) +
    geom_boxplot(width = 0.005, linewidth = 0.1, outlier.shape = NA, alpha = 0.5, position= position_nudge(y=-0.005)) +
    labs(
      title=paste0("Site Distribution over ",Var_names[[k]]),             # Delete this line to create figure without title
      x=Units[[k]],
      fill="Site type:") +
    facet_grid(.~ Phase) +
    theme_plots() +                                                       
    theme(axis.line.x = element_line(color = "grey", linewidth = 0.075),
          axis.title.x = element_blank()) +                               # Add legend.position="none" to create figure without legend
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))
  p
  
  ggsave(path="./Sites_Plots/", filename=paste0(All_vars[[k]],"_SiteType_distribution_per_Variable_and_Phase_light.pdf"), bg="white", width=4, height=3)
  
}


## CREATE A ALLUVIUM CHART TO SHOW DISTRIBUTION OF SITE TYPES PER PHASE ##
#-------------------------------------------------------------------------

# Prepare aggregated dataframe
agg <- site_stats
agg["Freq"] <- 1
agg <- aggregate(agg[c("Freq")], by=agg[c("Phase", "TypeSite")], FUN = sum)

# Prepare labels dataframe with hjust 1 (aligned right) for left axis (TypeSites) and 0 (aligned left) for second axis (Phases)
data_labs <- tibble::tibble(group = c(rep("TypeSite", 6), rep("Phase", 4)))
data_labs$vjustName = c(rep(-0.5, 10))
data_labs$vjustNbr = c(rep(1, 10))
data_labs$nudge_x = c(rep(0, 3),-0.12,0.13,rep(0, 5))

# Create chart and save it
p <- ggplot(agg, aes(y = Freq, axis1 = TypeSite, axis2 = Phase)) +
  geom_alluvium(aes(fill = TypeSite), width = 1/6) +
  geom_stratum(linewidth = 0.2, fill = "black", alpha = 0.2, width = 1/6) +
  geom_text(size = 1.5, stat = "stratum", aes(label = after_stat(stratum))
            , vjust=data_labs$vjustName, nudge_x=data_labs$nudge_x) +
  geom_text(size = 1.5, stat = "stratum", aes(label = paste0("n=",after_stat(count)))
            , vjust=data_labs$vjustNbr, nudge_x=data_labs$nudge_x) +
  
  scale_fill_manual(values = viridis(n=6)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        plot.margin = ggplot2::margin(0,0,0,0))
p

ggsave(filename="./Sites_Plots/Type_Phase_Alluvium.pdf", bg="white", width=3, height=3)

