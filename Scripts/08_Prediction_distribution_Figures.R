###################################################################################
#                                 CASTIELLO ET AL.                                #
#                                 ----------------                                #
#           GENERIC SRIPT FOR PLOTTING MAXENT AND RF PREDICTIONS AGAINST          #
#                  PALEO-CLIMATIC AND ENVIRONMENTAL VARIABLES                     #
#                                                                                 #
#                               #  November 2024  #                               #
#                                                                                 #
#                                                                                 #
###################################################################################


###################################################################################
# Load PACKAGES AND LIBRARIES
###################################################################################

library(raster)
library(ggplot2)
library(gghalves)
library(viridis)
library(patchwork)
library(dplyr)

###################################################################################
# SET WORKING DIRECTORY AND CREATE FOLDER FOR OUTPUTS
###################################################################################

setwd("YOUR WORKING DIRECTORY")

dir.create("./Prediction_Plots")

###################################################################################
# CREATE LISTS FOR VARIABLES AND MODEL
###################################################################################

# Create a list of all variables to be extracted (for all variables)
All_vars <- c("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10", 
              "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19",
              "DEM", "Slope", "TRI", "D_Lakes", "D_Rivers")

# Create a liste for reading each model
model <- c("RF", "MaxEnt")

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

# Create a list of subtitles to be used in the plots
Var_names_sub <- c("",
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
                   "",
                   "",
                   "",
                   "",
                   "",
                   "")

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

# Create a label for each phase (Y-axis labels)
phase_label <- c("Phase01\n5300-5900 cal. BC", "Phase02\n4500-5299 cal. BC", "Phase03\n3100-4499 cal. BC", "Phase04\n2300-3099 cal. BC")

# Define Phase colors for plots
phase_colors <- c("Phase04"="#fde725",
                  "Phase03"="#5ec962",
                  "Phase02"="#20a486",
                  "Phase01"="#31688e")

# Create list for colors
model_colors <- c("RF"="#c8e020",
                  "MaxEnt"="#440154")

# Create theme to be used for the plots
theme_plots <- function(...) {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      plot.subtitle = element_text(hjust = 0.5, size = 8),
      axis.text.x = element_text(angle=45, hjust=1),
      axis.title.y = element_text(hjust = 0.5, size = 8),
      axis.line.x = element_line(colour = 'black', size=0.3, linetype='solid'),
      axis.line.y = element_line(colour = 'black', size=0.3, linetype='solid'),
      axis.ticks.x = element_line(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      ...
    ) 
}

###################################################################################
# IMPORT RASTER DATA
###################################################################################

# Read environmental data (constant)
DEM <- raster("./data/cropped_data_Buffer/Env_Vars/CHELSA_TraCE21k_DEM_MEAN-58to-23_V1.0_cropped_buf.tif")
Slope <- raster("./data/cropped_data_Buffer/Env_Vars/Slope_tot_cropped_buffer_0.tif")
TRI <- raster("./data/cropped_data_Buffer/Env_Vars/TRI_cropped_buffer.tif")
D_Lakes <- raster("./data/cropped_data_Buffer/Env_Vars/DistLakes_cropped_buffer.tif")
D_Rivers <- raster("./data/cropped_data_Buffer/Env_Vars/RivDist_cropped_buffer.tif")

env_vars <- list(DEM, Slope, TRI, D_Rivers, D_Lakes)

# Loop for each model (RF and MaxEnt)
for (m in 1:length(model)){
# Read modeled probability map for each phase
Pred_map_phase01 <- raster(paste0("./", model[[m]], "/Phase01/", model[[m]], "_pred_Phase01.tif"))
Pred_map_phase02 <- raster(paste0("./", model[[m]], "/Phase02/", model[[m]], "_pred_Phase02.tif"))
Pred_map_phase03 <- raster(paste0("./", model[[m]], "/Phase03/", model[[m]], "_pred_Phase03.tif"))
Pred_map_phase04 <- raster(paste0("./", model[[m]], "/Phase04/", model[[m]], "_pred_Phase04.tif"))

# Loop for all paleoclimatic variables
for (v in 1:1){   # Replace with for (v in 1:18){ for looping through all paleoclimatic variables
# Read paleoclimatic data
var_phase01 <- raster(paste0("./data/cropped_data_Buffer/means/Phase01/", All_vars[[v]], "_Phase01_MEAN_buf.tif"))
var_phase02 <- raster(paste0("./data/cropped_data_Buffer/means/Phase02/", All_vars[[v]], "_Phase02_MEAN_buf.tif"))
var_phase03 <- raster(paste0("./data/cropped_data_Buffer/means/Phase03/", All_vars[[v]], "_Phase03_MEAN_buf.tif"))
var_phase04 <- raster(paste0("./data/cropped_data_Buffer/means/Phase04/", All_vars[[v]], "_Phase04_MEAN_buf.tif"))


##################################################################################
# CREATE A RASTER BRICK AND DATA FRAME WITH VARIABLES AND PREDICTION
##################################################################################

# Create raster brick for each phase
raster_data01 <- brick(var_phase01, Pred_map_phase01)
raster_data02 <- brick(var_phase02, Pred_map_phase02)
raster_data03 <- brick(var_phase03, Pred_map_phase03)
raster_data04 <- brick(var_phase04, Pred_map_phase04)

# Extract raster bricks as dataframe, delete NAs and select only modeled presences
df_data01 <- as.data.frame(raster_data01)
df_data01 <- na.omit(df_data01)
names(df_data01)[1] <- All_vars[[v]]
names(df_data01)[2] <- "Prediction"
df_pres01 <- subset(df_data01, df_data01$Prediction >= 0.75)
df_pres01$Phase <- "Phase01"

df_data02 <- as.data.frame(raster_data02)
df_data02 <- na.omit(df_data02)
names(df_data02)[1] <- All_vars[[v]]
names(df_data02)[2] <- "Prediction"
df_pres02 <- subset(df_data02, df_data02$Prediction >= 0.75)
df_pres02$Phase <- "Phase02"

df_data03 <- as.data.frame(raster_data03)
df_data03 <- na.omit(df_data03)
names(df_data03)[1] <- All_vars[[v]]
names(df_data03)[2] <- "Prediction"
df_pres03 <- subset(df_data03, df_data03$Prediction >= 0.75)
df_pres03$Phase <- "Phase03"

df_data04 <- as.data.frame(raster_data04)
df_data04 <- na.omit(df_data04)
names(df_data04)[1] <- All_vars[[v]]
names(df_data04)[2] <- "Prediction"
df_pres04 <- subset(df_data04, df_data04$Prediction >= 0.75)
df_pres04$Phase <- "Phase04"

# Bind all 4 phases into a single data frame
df_pres <- rbind(df_pres01, df_pres02, df_pres03, df_pres04)

# Create object to insert n label with amount of points
n_labels <- c(count(df_pres01), count(df_pres02), count(df_pres03), count(df_pres04))

##################################################################################
# CREATE AND SAVE THE PLOTS
##################################################################################

# Create and save the distribution plots for each paleoclimatic variable
g <- ggplot(data = df_pres, aes_string(y=All_vars[[v]], x = "Phase", fill = "Phase")) +
  geom_half_violin(position = position_nudge(x = .1, y = 0), side = "r", alpha = 0.6) + 
  scale_fill_manual(values = phase_colors) +
  geom_point(aes(color=Phase), position = position_jitter(width = .05), size = .05, alpha = 0.08) +
  scale_color_manual(values = phase_colors) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.3) +
  stat_summary(fun.y=median, colour="black", geom="text", size=3, vjust=2.2, aes(label=round(..y.., digits=1))) +
  expand_limits(x = 5.25) +
  coord_flip() +
  labs(title=paste0("Distribution of predicted presences (probability >= 0.75) using ", model[[m]], "\nover ",Var_names[[v]], " (", All_vars[[v]], ")"),
       subtitle=paste0(Var_names_sub[[v]], "\n"),
       x="",
       y=paste0(Units[[v]])) +
  scale_x_discrete(labels= paste0(phase_label, "\n", "n = ", n_labels)) +
  scale_y_continuous(limits = c(-10, 18), breaks = seq(-10,15,5)) + # Only for Bio01, comment of creating plots for all variables
  theme_plots() 
g

ggsave(path="./Prediction_Plots/", filename=paste0(All_vars[[v]], "_", model[[m]], "_PredPres_distribution.pdf"), bg="white", width=8, height=6)

# Close all devices
graphics.off()

# Close Bio variables loop
}

# Loop for all environmental variables
for (v in 19:19){      # Replace with for (v in 19:length(All_vars)){ for looping through all environmental variables

# Create raster brick for each phase
raster_data01 <- brick(env_vars[[v-18]], Pred_map_phase01)
raster_data02 <- brick(env_vars[[v-18]], Pred_map_phase02)
raster_data03 <- brick(env_vars[[v-18]], Pred_map_phase03)
raster_data04 <- brick(env_vars[[v-18]], Pred_map_phase04)

# Extract raster bricks as dataframe, delete NAs and select only modeled presences
df_data01 <- as.data.frame(raster_data01)
df_data01 <- na.omit(df_data01)
names(df_data01)[1] <- All_vars[[v]]
names(df_data01)[2] <- "Prediction"
df_pres01 <- subset(df_data01, df_data01$Prediction >= 0.75)
df_pres01$Phase <- "Phase01"

df_data02 <- as.data.frame(raster_data02)
df_data02 <- na.omit(df_data02)
names(df_data02)[1] <- All_vars[[v]]
names(df_data02)[2] <- "Prediction"
df_pres02 <- subset(df_data02, df_data02$Prediction >= 0.75)
df_pres02$Phase <- "Phase02"

df_data03 <- as.data.frame(raster_data03)
df_data03 <- na.omit(df_data03)
names(df_data03)[1] <- All_vars[[v]]
names(df_data03)[2] <- "Prediction"
df_pres03 <- subset(df_data03, df_data03$Prediction >= 0.75)
df_pres03$Phase <- "Phase03"

df_data04 <- as.data.frame(raster_data04)
df_data04 <- na.omit(df_data04)
names(df_data04)[1] <- All_vars[[v]]
names(df_data04)[2] <- "Prediction"
df_pres04 <- subset(df_data04, df_data04$Prediction >= 0.75)
df_pres04$Phase <- "Phase04"

# Bind all 4 phases into a single data frame
df_pres <- rbind(df_pres01, df_pres02, df_pres03, df_pres04)

# Create and save the distribution plots for each environmental variable
g <- ggplot(data = df_pres, aes_string(y=All_vars[[v]], x = "Phase", fill = "Phase")) +
  geom_half_violin(position = position_nudge(x = .1, y = 0), side = "r", alpha = 0.6) +
  scale_fill_manual(values = phase_colors) +
  geom_point(aes(color=Phase), position = position_jitter(width = .05), size = .05, alpha = 0.08) +
  scale_color_manual(values = phase_colors) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.3) +
  stat_summary(fun.y=median, colour="black", geom="text", size=3, vjust=2.2, aes(label=round(..y.., digits=1))) +
  expand_limits(x = 5.25) +
  coord_flip() +
  labs(title=paste0("Distribution of predicted presences (probability >= 0.75) using ", model[[m]], "\nover ",Var_names[[v]], " (", All_vars[[v]], ")"),
       subtitle=paste0(Var_names_sub[[v]], "\n"),
       x="",
       y=paste0(Units[[v]])) +
  scale_x_discrete(labels= paste0(phase_label, "\n", "n = ", n_labels)) +
  scale_y_continuous(limits = c(0, 4500)) + # Only for DEM, comment of creating plots for all variables
  theme_plots() 
g

# ggsave(path="./Prediction_Plots/", filename=paste0(All_vars[[v]], "_", model[[m]], "_PredPres_distribution.tiff"), bg="white", dpi=200, width=17, height=13, units="cm")
ggsave(path="./Prediction_Plots/", filename=paste0(All_vars[[v]], "_", model[[m]], "_PredPres_distribution.pdf"), bg="white", width=8, height=6)

# Close environmental data loop
}

# Close model type (rf or maxent) loop
}


##################################################################################
# CREATE BOXPLOTS FOR ALL VARIABLES PER PHASE
##################################################################################

# Create theme to be used for the plots
theme_plots_box <- function(...) {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 7),
      axis.text.x = element_text(hjust = 0.5, size = 5),
      axis.text.y = element_text(size = 5),
      axis.title.y = element_text(size = 7),
      axis.title.x = element_blank(),
      axis.line.x = element_line(colour = 'black', size=0.2, linetype='solid'),
      axis.line.y = element_line(colour = 'black', size=0.2, linetype='solid'),
      axis.ticks.x = element_line(),
      axis.ticks.y = element_line(),
      panel.grid.major.y = element_line(colour = 'grey', size=0.07, linetype='solid'),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 7),
      ...
    ) 
}


# Prepare a dataframe with values per model and phase
Pred_stats = data.frame()

# Loop for each model (RF and MaxEnt)
for (m in 1:length(model)){
  # Loop for each phase
  for (p in 1:length(phase)){
    for (v in 1:18){
      
      # Read modeled probability map 
      Pred_map <- raster(paste0("./", model[[m]], "/", phase[[p]], "/", model[[m]], "_pred_", phase[[p]], ".tif"))
      
      # Read Bio variables
      Bio_var <- raster(paste0("./data/cropped_data_Buffer/means/", phase[[p]], "/", All_vars[[v]], "_", phase[[p]], "_MEAN_buf.tif"))

      matrix <- brick(Pred_map, Bio_var)
      matrix_df <- na.omit(as.data.frame(matrix))
      names(matrix_df) <- c("prob", "values")
      matrix_df$model <- model[[m]]
      matrix_df$phase <- phase[[p]]
      matrix_df$variable <- All_vars[[v]]
      matrix_df <- subset(matrix_df, prob >= 0.75)
      
      Pred_stats <- rbind(Pred_stats, matrix_df)
    }
    
    for (e in 1:length(env_vars)){
      Env_var <- env_vars[[e]]
      matrix <- brick(Pred_map, Env_var)
      matrix_df <- na.omit(as.data.frame(matrix))
      names(matrix_df) <- c("prob", "values")
      matrix_df$model <- model[[m]]
      matrix_df$phase <- phase[[p]]
      matrix_df$variable <- All_vars[[18+e]]
      matrix_df <- subset(matrix_df, prob >= 0.75)
      Pred_stats <- rbind(Pred_stats, matrix_df)
    }
  }  
}  


# Create each plot separately and wrap them together for all variables
#---------------------------------------------------------------------

# Create a list to store the plots
plots <- list()

# Avoid scientific notation
options(scipen=1000000)

# Loop through all variables and add plot to list
for (v in 1:length(All_vars)){
  # Subset data for selected variable
  Pred_stats_sub <- subset(Pred_stats, Pred_stats$variable == All_vars[[v]])
  
  # Create and add the distribution plot
  p <- ggplot(data = Pred_stats_sub, aes(x=phase, y=values, fill = model)) +
    scale_fill_manual(values = model_colors) +
    geom_boxplot(width=0.3, linewidth = 0.1, outlier.shape = NA, alpha = 0.6, position = position_dodge(0.5), show.legend = FALSE) +
    labs(title = stringr::str_wrap(paste0(Var_names[[v]], " (", All_vars[[v]], ")"), width=30), 
         x ="",
         y = Units[[v]]) +
    theme_plots_box()
  
  plots[[paste0("plot_", v)]] <- p  
  
}

# Create a plot to extract legend
p <- ggplot(data = Pred_stats_sub, aes(x=phase, y=values, fill = model)) +
  scale_fill_manual(values = model_colors, name = "Model", labels = c("Maximum Entropy", "Random Forest")) +
  geom_boxplot(width=0.3, linewidth = 0.1, outlier.shape = NA, alpha = 0.6, position = position_dodge(0.5)) +
  theme_plots_box()

# Extract legend and add to list
legend <- cowplot::get_legend(p)
plots[[paste0("plot_", length(All_vars)+1)]] <- legend 

# Create and save the wrapped plot
dev.new()
pdf(file = "./Prediction_Plots/All_vars_PredPres_distribution.pdf", 
     width= 9, height = 13.5, bg = "white")
wrap_plots(plots, ncol = 4, nrow = 6)
dev.off()

# Close all graphics
graphics.off()  
