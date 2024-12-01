###################################################################################
#                               CASTIELLO ET AL.                                  #
#                               ----------------                                  #
#         GENERIC SRIPT TO EXTRACT PALEOCLIMATIC AND ENVIRONMENTAL DATA AT        #
#               SITE LOCATION AND CREATE CROP DISTRIBUTION FIGURES                #
#                                                                                 #
#                             #  November 2024  #                                 #
#                                                                                 #
#                                                                                 #
###################################################################################


## LOAD LIBRARIES, SET DIRECTORY AND PREPARE LISTS ##
#----------------------------------------------------

# Load libraries
library(raster)
library(ggplot2)
library(stringr)
library(dplyr)
library(ggridges)
library(hrbrthemes)
library(tidyr)

# Set your working directory
setwd("YOUR WORKING DIRECTORY")

# Create new folder for outputs
dir.create("./Crops_Plots")

# Create a list of all paleoclimatic variables to be extracted (for all Bio variables)
Bio_vars_all <- c("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10",
                  "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19")

# Create a list of all variables to be extracted (for all variables)
All_vars <- c("Bio01", "Bio02", "Bio04", "Bio05", "Bio06", "Bio07", "Bio08", "Bio09", "Bio10", 
              "Bio11", "Bio12", "Bio13", "Bio14", "Bio15", "Bio16", "Bio17", "Bio18", "Bio19",
              "DEM", "Slope", "TRI", "D_Lakes", "D_Rivers")

# Create a list of all crops to be extracted (for all variables)
All_Crops <- c("Barley",
               "Naked wheat",
               "Einkorn",
               "Emmer",
               "Timopheev's wheat",
               "Lentil",
               "Pea",
               "Faba bean",
               "Flax",
               "Poppy",
               "Apple pear",
               "Hazel",
               "Oak")

# Create a list of all phases to be extracted (for all phases)
All_phases <- c("Phase01", "Phase02", "Phase03", "Phase04")

## CREATE A DATAFRAME WITH THE CROP DATASET ##
#---------------------------------------------

# Read in the crops
Crops <- read.table("./data/Crops/AllCrops_BC.csv", header = TRUE, sep = ",")

# Round down mean BC values with 0.5, so that e.g. 3499.5 still belongs to 100-year step 3400-3500
Crops$meanBC <- floor(Crops$mean_cal_BC)

# Create a Column containing the name of the corresponding layer in the raster stack
Crops$layername <- paste0("Years_",str_sub(Crops$mean_cal_BC,0,-3),"00")


## CREATE A DATAFRAME WITH THE VALUE OF THE CLIMATIC VARIABLES CORRESPONDING TO THE DATE OF EACH CROP ##
#-------------------------------------------------------------------------------------------------------

# Prepare crops_stats table with the columns of the Crops table
crops_stats <- Crops

# Loop to extract Bio variable data from all bio variables
for (k in 1:length(Bio_vars_all)){
  
  # Read the data into a list
  rastFiles = list.files(paste0("./data/cropped_data_Buffer/", Bio_vars_all[[k]]), pattern="\\.tif$", full.names = TRUE)
  
  # Create a raster stack with all 36 variable tif files
  bio_variables <- stack(rastFiles)
  
  # Rename the columns to match the index "layername" in the Crops dataframe
  names(bio_variables) <-  paste0("Years_", seq(2300, 5800, by=100))
  
  # Extract value from the layer from the stack that is called equally as the value in column layername 
  # (get the value of the variable for the correct date)
  Bio_values <- raster::extract(bio_variables, cbind(Crops$Longitude, Crops$Latitude))[
    cbind(1:nrow(Crops),match(Crops$layername, names(bio_variables)))
  ]
  
  # Turn the extracted Bio values into a data frame, rename the new column accordingly and bind with the crops_stats table
  Bio_values <- as.data.frame(Bio_values)
  names(Bio_values)[1] <- Bio_vars_all[[k]]
  crops_stats <- cbind(crops_stats, Bio_values)

}

# Create column with phases
crops_stats$Phase <- NA
crops_stats$Phase[crops_stats$meanBC >= 5300 & crops_stats$meanBC <= 5900] <- "Phase01"
crops_stats$Phase[crops_stats$meanBC >= 4500 & crops_stats$meanBC <= 5299] <- "Phase02"
crops_stats$Phase[crops_stats$meanBC >= 3100 & crops_stats$meanBC <= 4499] <- "Phase03"
crops_stats$Phase[crops_stats$meanBC >= 2300 & crops_stats$meanBC <= 3099] <- "Phase04"


## READ ENVIRONMENTAL DATA AND EXTRACT THEIR CORRESPONDING VALUES TO THE DATAFRAME ##
#------------------------------------------------------------------------------------

# Read the environmental data
DEM      <- raster("./data/cropped_data_Buffer/Env_Vars/CHELSA_TraCE21k_DEM_MEAN-58to-23_V1.0_cropped_buf.tif")
Slope    <- raster("./data/cropped_data_Buffer/Env_Vars/Slope_tot_cropped_buffer_0.tif")
TRI      <- raster("./data/cropped_data_Buffer/Env_Vars/TRI_cropped_buffer.tif")
D_lakes  <- raster("./data/cropped_data_Buffer/Env_Vars/DistLakes_cropped_buffer.tif")
D_rivers <- raster("./data/cropped_data_Buffer/Env_Vars/RivDist_cropped_buffer.tif")

# Unite the variables in a single stack
env_variables <- stack(DEM, Slope, TRI, D_lakes, D_rivers)

# Extract value of the environmental variables from the layers in the stack 
Env_values <- raster::extract(env_variables, cbind(Crops$Longitude, Crops$Latitude))

# Bind with the table containing the values of the crops and the climatic variables
crops_stats <- cbind(crops_stats, Env_values)

# Rename columns to match All_vars
names(crops_stats)[names(crops_stats)=="CHELSA_TraCE21k_DEM_MEAN.58to.23_V1.0_cropped_buf"] <- "DEM"
names(crops_stats)[names(crops_stats)=="Slope_tot_cropped_buffer_0"] <- "Slope"
names(crops_stats)[names(crops_stats)=="TRI_cropped_buffer"] <- "TRI"
names(crops_stats)[names(crops_stats)=="DistLakes_cropped_buffer"] <- "D_Lakes"
names(crops_stats)[names(crops_stats)=="RivDist_cropped_buffer"] <- "D_Rivers"

# Replace NA's in TRI woth 0 to avoid data loss
crops_stats$TRI[is.na(crops_stats$TRI)] <- 0

# Delete points outside the study period and area
crops_stats <- na.omit(crops_stats)


## CREATE LISTS AND THEME FOR FIGURES ##
#---------------------------------------

# Create a list with the labels for the phase groups
Phase_names <- c(`Phase01` = "Phase 01\n(5900-5300 cal. BC)",
                 `Phase02` = "Phase 02\n(5299-4500 cal. BC)",
                 `Phase03` = "Phase 03\n(4499-3100 cal. BC)",
                 `Phase04` = "Phase 04\n(3099-2300 cal. BC)"
                  )

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

# Create units for the 2D density plots
Units_facet <- c("[°C]",
                 "[°C]",
                 "[Standard Deviation in°C * 100]",
                 "[°C]",
                 "[°C]",
                 "[°C]",
                 "[°C]",
                 "[°C]",
                 "[°C]",
                 "[°C]",
                 "[kg m-2 year-1]",
                 "[kg m-2 year-1]",
                 "[kg m-2 year-1]",
                 "[Coefficient of Varation in %]",
                 "[kg m-2 year-1]",
                 "[kg m-2 year-1]",
                 "[kg m-2 year-1]",
                 "[kg m-2 year-1]",
                 "[m.a.s.l.]", 
                 "[°]",
                 "[m]",
                 "[m]",
                 "[m]")

# Create list for colors
crop_colors <- c("Oak"="#fde725",
                 "Hazel"="#c8e020",
                 "Apple pear"="#90d743",
                 "Poppy"="#5ec962",
                 "Flax"="#35b779",
                 "Faba bean"="#20a486",
                 "Pea"="#21918c",
                 "Lentil"="#287c8e",
                 "Timopheev's wheat"="#31688e",
                 "Emmer"="#3b528b",
                 "Einkorn"="#443983",
                 "Naked wheat"="#481f70",
                 "Barley"="#440154")

# Create list for Crops and/or y axis order
crop_order <- factor(crops_stats$Crop, level = c("Oak",
                                                 "Hazel",
                                                 "Apple pear",
                                                 "Poppy",
                                                 "Flax",
                                                 "Faba bean",
                                                 "Pea",
                                                 "Lentil",
                                                 "Timopheev's wheat",
                                                 "Emmer",
                                                 "Einkorn",
                                                 "Naked wheat",
                                                 "Barley"))


# Create theme without axis, etc... to be used for the plots
theme_plots_box <- function(...) {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 7),
      plot.subtitle = element_text(hjust = 0.5, size = 6),
      strip.text = element_text(hjust = 0.5, size = 7, face = "bold"),
      axis.title = element_text(size = 7),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle=0, hjust=0.5, size = 6),
      axis.text.y = element_text(hjust=1, size = 6),
      axis.ticks.x = element_blank(),
      axis.ticks.length = unit(2, "pt"),
      legend.position="none",
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey", linewidth = 0.075),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.4, "cm"),
      ...
    ) 
}


# Avoid scientific notation
options(scipen=1000000)


# CREATE BOXPLOTS FOR CROP DISTRIBUTION PER VARIABLE AND PHASE
#-------------------------------------------------------------

# Reorganize dataframe for faceted boxplots
stats_var = data.frame()

for (p in 1:length(All_phases)){
  for (v in 1:length(All_vars)){
    sub <- crops_stats[c("Crop", "Phase", All_vars[[v]])]
    sub_sub <- subset(sub, sub$Phase==All_phases[[p]])
    sub_sub$values <- sub_sub[, All_vars[[v]]]
    sub_sub$variable <- All_vars[[v]]
    sub_sub <- sub_sub[c("Crop", "Phase", "variable", "values")]
    stats_var <- rbind(stats_var, sub_sub)
  }
}

# Create labels for facet titles
variable_labs <- Var_names
names(variable_labs) <- All_vars

for (v in 1:length(All_vars)){
  
  # Create a subset with only the data for one variable
  stats_var_sub <- subset(stats_var, stats_var$variable == All_vars[[v]])
  # Define the right order for the Crops
  stats_var_sub$Crop <- factor(stats_var_sub$Crop, levels = c("Barley",
                                                              "Naked wheat",
                                                              "Einkorn",
                                                              "Emmer",
                                                              "Timopheev's wheat",
                                                              "Lentil",
                                                              "Pea",
                                                              "Faba bean",
                                                              "Flax",
                                                              "Poppy",
                                                              "Apple pear",
                                                              "Hazel",
                                                              "Oak"))
  
  
  g <- ggplot(data = stats_var_sub, aes(x=Phase, y=values, fill = Crop)) +
    geom_boxplot(width=0.18, linewidth = 0.1, outlier.shape = NA, alpha = 0.5, position=position_dodge(0)) + #standard ggplot colors "#F8766D" "#7CAE00" "#00BFC4" "#C77CFF"
    scale_fill_manual(values = crop_colors) +
    stat_summary(fun.y=median, colour="black", geom="text", size=1.3, vjust=0.5, hjust=0, position = position_nudge(x=0.15) , aes(label=round(..y.., digits=1))) +
    facet_wrap(~Crop, labeller = labeller(variable = variable_labs), scales = "free_x") + 
    scale_x_discrete(limits = c("Phase01", "Phase02", "Phase03", "Phase04"), expand = expansion(mult = c(0.1, 0.23))) +
    labs(y=Units[[v]]) +    
    theme_plots_box() 
  
  g  
  
  ggsave(filename=paste0("./Crops_Plots/Crop_distribution_over_", All_vars[[v]], ".pdf"), bg="white", width=9, height=11)

  }

# CREATE 2D KERNEL DENSITY PLOTS FOR VARIABLE PAIRS
#--------------------------------------------------

# Create theme without axis, etc... to be used for the plots
theme_plots_2D <- function(...) {
  theme_minimal() +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      plot.title = element_text(hjust=0, size=10),
      strip.text.x = element_text(size = 9), 
      
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.5, "cm"),
      
      axis.title.x = element_text(hjust=0, size = 9),
      axis.text.x = element_text(hjust=0.5, size = 8),
      
      axis.title.y = element_text(hjust=0.5, size = 9),
      axis.text.y = element_text(hjust=0.5, size = 8),
      
      legend.position = c(0.63, 0.08), 
      legend.direction = ("horizontal"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8))
}

# Create the color palette
mycolors <- colorRampPalette(colors = c("#FFFFFF", "#08306B"), space = "rgb")(10)

# Create two lists to pair the variables
var1 <- c(1,  10)  # 1 being Bio01, 10 = Bio11; You can pair any two variables by adding them to the lists
var2 <- c(11, 18)  # 11 = Bio12, 18 = Bio19; You can pair any two variables by adding them to the lists

# Make 2D density plots for the variable pairs and save them
for (k in 1:length(var1)){
  
  p <-ggplot(crops_stats, aes_string(y = All_vars[[var1[[k]]]], x=All_vars[[var2[[k]]]])) +
    geom_density_2d_filled(contour_var = "ndensity") + 
    geom_point(size=0.1, colour="#08306B", alpha=0.2) +
    facet_wrap(~factor(Crop, levels=rev(c("Oak", 
                                          "Hazel", 
                                          "Apple pear", 
                                          "Poppy",  
                                          "Flax", 
                                          "Faba bean", 
                                          "Pea", 
                                          "Lentil", 
                                          "Timopheev's wheat", 
                                          "Emmer", 
                                          "Einkorn", 
                                          "Naked wheat", 
                                          "Barley")))) +
    scale_fill_manual(values = mycolors) +
    labs(title=paste0("Crop Distribution over ",Var_names[[var1[[k]]]], " and ", Var_names[[var2[[k]]]]), 
         y=paste0(Var_names[[var1[[k]]]], " ", Units_facet[[var1[[k]]]], "\n"), 
         x=paste0("\n", Var_names[[var2[[k]]]], " ", Units_facet[[var2[[k]]]])) +
    theme_plots_2D() 
  
  p
  
  ggsave(path="./Crops_Plots/", filename=paste0(All_vars[[var1[[k]]]], "_", All_vars[[var2[[k]]]],"_Crop_distribution_2d.pdf"), bg="white", width=7, height=9.5)
  
}


# CREATE DENSITY RIDGE PLOTS FOR ALL CROPS
#-----------------------------------------

# Create list for bandwiths
Bwidth <- c(1,     # Bio01
            0.5,   # Bio02
            30,    # Bio04
            1,     # Bio05
            1,     # Bio06
            1,     # Bio07
            1,     # Bio08
            1,     # Bio09
            1,     # Bio10
            1,     # Bio11
            100,   # Bio12
            10,    # Bio13
            10,    # Bio14
            4,     # Bio15
            50,    # Bio16
            50,    # Bio17
            50,    # Bio18
            50,    # Bio19
            100,   # DEM
            1,     # Slope
            10000, # D_Lakes
            10000) # D_Rivers

# Create theme without axis, etc... to be used for the plots
theme_plots <- function(...) {
  theme_minimal() +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 9),
      axis.title.x = element_text(hjust = 1, size = 10),
      panel.grid.minor = element_blank(),
      legend.position="none",
      panel.spacing = unit(0.5, "cm"),
      strip.text.x = element_text(size = 9))
}

# Create and save the plot
p <-ggplot(crops_stats) +
  geom_density_ridges(aes_string(y = crop_order, x="mean_cal_BC", fill="Crop"), alpha=0.6, bandwidth = 200) +
  geom_vline(xintercept = 5300, linetype="longdash", color = "grey", size=0.5) +
  geom_vline(xintercept = 4500, linetype="longdash", color = "grey", size=0.5) +
  geom_vline(xintercept = 3100, linetype="longdash", color = "grey", size=0.5) +
  labs(title=paste0("Crop Distribution over time"), 
       subtitle=paste0("Subdivision in 4 phases\n"),
       x=paste0("Years cal. BC"), 
       y="") +
  scale_x_reverse() +
  scale_fill_manual(values = crop_colors) +
  scale_color_manual(values = crop_colors) +
  theme_plots() +
  theme(panel.grid.major = element_blank())
p

# ggsave(path="./Crops_Plots/", filename="Crop_distribution_Phases.tiff", bg="white", dpi=300, width=17, height=23, units="cm")
ggsave(path="./Crops_Plots/", filename="Crop_distribution_Phases.pdf", bg="white", width=7, height=9.5)


# CREATE BARPLOT WITH NUMBER OF CROP PER PHASE
#---------------------------------------------

# Group Crop dataset by to Phases and Crops and count the number of occurrences
crops_phases <- Crops %>%
                group_by(Phase, Crop) %>%
                summarise(total_count=n()) %>%
                # Complete with a row with a count value of 0 where there are no occurrences
                ungroup() %>%
                complete(Phase, Crop, fill = list(total_count = 0))

# Create a list with the labels for the phase groups
Phase_names_bar <- c(`Phase01` = "Phase 01\n(5900-5300 cal. BC)\nn = 70",
                     `Phase02` = "Phase 02\n(5299-4500 cal. BC)\nn = 200",
                     `Phase03` = "Phase 03\n(4499-3100 cal. BC)\nn = 492",
                     `Phase04` = "Phase 04\n(3099-2300 cal. BC)\nn = 81"
                      )

# Define the right order for the Crops
crops_phases$Crop <- factor(crops_phases$Crop, levels = c("Oak", 
                                                          "Hazel", 
                                                          "Apple pear", 
                                                          "Poppy",  
                                                          "Flax", 
                                                          "Faba bean", 
                                                          "Pea", 
                                                          "Lentil", 
                                                          "Timopheev's wheat", 
                                                          "Emmer", 
                                                          "Einkorn", 
                                                          "Naked wheat", 
                                                          "Barley"))

# Create theme without axis, etc... to be used for the plots
theme_plots <- function(...) {
  theme_minimal() +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      plot.title = element_text(hjust = 0.5, size = 13),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 8.5),
      panel.grid.minor = element_blank(),
      legend.position="none",
      panel.spacing = unit(0.5, "cm"),
      strip.text = element_text(size = 10))
}

# Create and save the bar plot
p <- ggplot(crops_phases, aes(y=Crop, x=total_count, fill=Crop)) + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values = crop_colors) +
  facet_wrap(Phase ~ ., nrow = 4, strip.position = "left", labeller = as_labeller(Phase_names_bar)) +
  geom_text(aes(label = total_count), size=2.8, hjust=-0.5) +
  scale_x_continuous(expand = c(0.01, 0)) +
  expand_limits(x = c(0, 100)) +
  theme_plots() +
  theme(strip.placement = "outside",
        strip.text.y = element_text(hjust = 0.5),
        axis.line.y = element_line(color="black", size=1),
        axis.text.y = element_text(),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        legend.position="none") +
  labs(y="",
       x="") +
  guides(fill=guide_legend(nrow=3,byrow=TRUE, reverse = TRUE))
p

ggsave(path="./Crops_Plots/", filename="Crop_Number_Phases.pdf", bg="white", width=6.2, height=9.5)

