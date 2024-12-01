###################################################################################
#                               CASTIELLO ET AL.                                  #
#                               ----------------                                  #
#      GENERIC SRIPT TO CREATE PALEOCLIMATIC VARIABLE EVOLUTION PLOTS             #
#                                                                                 #
#                             #  November 2024  #                                 #
#                                                                                 #
#                                                                                 #
###################################################################################


## READ LIBRARIES, DATA AND SET DIRECTORY ##
#-------------------------------------------

# Load libraries
library(raster)
library(ggplot2)

# Set your working directory
setwd("YOUR WORKING DIRECTORY")

# Create new folder for outputs
dir.create("./Variable_Plots")


## PREPARE LOOP VARIABLE, LABELS AND PLOT THEME ##
#-------------------------------------------------

# Create a list of all paleoclimatic variables to be extracted 
Bio_vars_all <- c("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10", 
                  "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19")

# Create a list of x axis tick labels
years_labels <- c("2400-2301","2500-2401","2600-2501","2700-2601","2800-2701","2900-2801","3000-2901",
                  "3100-3001","3200-3101","3300-3201","3400-3301","3500-3401","3600-3501","3700-3601","3800-3701","3900-3801","4000-3902",
                  "4100-4001","4200-4101","4300-4201","4400-4301","4500-4401","4600-4501","4700-4601","4800-4701","4900-4801","5000-4902",
                  "5100-5001","5200-5101","5300-5201","5400-5301","5500-5401","5600-5501","5700-5601","5800-5701","5900-5801")

# Create a list of Variable names to be used in the plots
Bio_names <- c("Annual Mean Temperature",
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
               "Precipitation of Coldest Quarter")

# Create a list of Variable names to be used in the plots as subtitles
Bio_names_sub <- c("",
               "Annual mean of all the monthly diurnal temperature ranges \nEach monthly diurnal range is the difference between that month's max. and min. temperature",
               "Standard deviation of the monthly mean temperatures * 100",
               "",
               "",
               "Difference between the maximum temperature of the warmest period \nand the minimum temperature of the coldest period",
               "",
               "",
               "", 
               "",
               "Sum of all the monthly precipitation estimates",
               "", 
               "", 
               "Standard deviation of the monthly precipitation estimates expressed \nas a percentage of themean of those estimates", 
               "",
               "",
               "", 
               "")

# Create a list with X-Axis labels (units) to be used in the plots
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
           "kg m-2 year-1")

# Create a light plot theme to be used for the plots
theme_plots <- function(...) {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 15),
      axis.title = element_text(size = 15),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      ...
    ) 
}


## CREATE THE BIO VARIABLE PLOTS FOR TEMPERATURE VARIABLES (Bio01 to Bio11, red) ##
#----------------------------------------------------------------------------------

# Create a "years" object with a sequence from 2300 to 5800 with steps of 100
years <- seq(2300,5800,100)

# For Bio01 to Bio11
for (k in 1:10){
  
  # Read the data into a list
  rastFiles = list.files(paste0("./data/cropped_data/", Bio_vars_all[[k]]), pattern="\\.tif$", full.names = TRUE)

  # Read in all the tiff files in rastFiles (all tiffs from each Bio variable), calculate their means for each tif and 
  # return the mean values in a vector
  rastMeans = sapply(rastFiles, function(filename) {
    r = raster(filename)
    return(cellStats(r,'mean', na.rm=TRUE))
  })
  
  # Create a dataframe with the mean values
  Bio_df <-as.data.frame(rastMeans)
  
  #Add the column with years to the dataframe with the mean values
  Bio_df$years<-years
  
  
  # Create the plot and save it
  p <-ggplot(Bio_df, aes(x=years, y=rastMeans)) +
    # Add dotted lines for phases
    geom_vline(xintercept = 5300, linetype="longdash", color = "grey", size=0.5) +
    geom_vline(xintercept = 4500, linetype="longdash", color = "grey", size=0.5) +
    geom_vline(xintercept = 3100, linetype="longdash", color = "grey", size=0.5) +
    # Add smoothed trend line with loess tendency
    geom_smooth(linetype="dotted", span=0.3, colour="black", size=0.5, fill="#CC3300", alpha=0.2) + #336699
    # Add points at means values
    geom_point(shape=23, size=2.5, fill="#CC3300", alpha=0.6) + #003366
    # Add Plot title and axis labels
    labs(title=paste0(Bio_names[[k]], " 5900-2300 BC"), 
         subtitle=paste0(Bio_names_sub[[k]], "\n"),
         x="\nYears BC",
         y=paste0(Units[[k]], "\n")) +
    # Invert x axis to show from oldest to most recent
    scale_x_reverse(breaks=seq(2300, 5800, 100), labels=years_labels) +
    # Add Plot theme
    theme_plots()
  
  # Create the plot
  p
  
  # Save the Plot
  ggsave(path="./Variable_Plots/", filename=paste0(Bio_vars_all[[k]],"_Variable_Plot.pdf"), bg="white", width=9, height=7)
  
}


## CREATE THE BIO VARIABLE PLOTS FOR PRECIPITATION VARIABLES (Bio12 to Bio19, blue) ##
#-------------------------------------------------------------------------------------

# Create a "years" object with a sequence from 2300 to 5800 with steps of 100 
# (not necessary if script for temperature variables already run)
years <- seq(2300,5800,100)

# For Bio12 to Bio19
for (k in 11:length(Bio_vars_all)){
  
  # Read the data into a list
  rastFiles = list.files(paste0("./data/cropped_data/", Bio_vars_all[[k]]), pattern="\\.tif$", full.names = TRUE)
  
  # Read in all the tiff files in rastFiles (all tiffs from each Bio variable), calculate their means for each tif and 
  # return the means in a vector
  rastMeans = sapply(rastFiles, function(filename) {
    r = raster(filename)
    return(cellStats(r,'mean', na.rm=TRUE))
  })
  
  # Create a dataframe with the mean values
  Bio_df <-as.data.frame(rastMeans)
  
  #Add the column with years to the dataframe with the mean values 
  Bio_df$years<-years
  
  
  # Create the plot and save it
  p <-ggplot(Bio_df, aes(x=years, y=rastMeans)) +
    # Add dotted lines for phases
    geom_vline(xintercept = 5300, linetype="longdash", color = "grey", size=0.5) +
    geom_vline(xintercept = 4500, linetype="longdash", color = "grey", size=0.5) +
    geom_vline(xintercept = 3100, linetype="longdash", color = "grey", size=0.5) +
    # Add smoothed trend line with loess tendency
    geom_smooth(linetype="dotted", span=0.3, colour="black", size=0.5, fill="#336699", alpha=0.2) + 
    # Add points at means values
    geom_point(shape=23, size=2.5, fill="#003366", alpha=0.6) + 
    # Add Plot title and axis labels
    labs(title=paste0(Bio_names[[k]], " 5900-2300 BC"), 
         subtitle=paste0(Bio_names_sub[[k]], "\n"),
         x="\nYears BC",
         y=paste0(Units[[k]], "\n")) +
    # Invert x axis to show from oldest to most recent
    scale_x_reverse(breaks=seq(2300, 5800, 100), labels=years_labels) +
    # Add Plot theme
    theme_plots()
  
  # Create  the plot
  p
  
  # Save the Plot
  ggsave(path="./Variable_Plots/", filename=paste0(Bio_vars_all[[k]],"_Variable_Plot.pdf"), bg="white", width=9, height=7)
  
}

