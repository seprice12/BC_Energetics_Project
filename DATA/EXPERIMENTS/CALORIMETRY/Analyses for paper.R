--------------------------------------------------------------------------------
  # CCLME bioenergetics stats
  # Matt Savoca and Samuel Price
  # Started on: 4/19/23
--------------------------------------------------------------------------------
  
library(lme4)
library(lmerTest)
library(tidyverse)
library(modelsummary)



combustion_df <- read.csv("EnergyDensity.csv") %>% 
  mutate(species = as.character(SPECIES),
         organization = as.character(ORGANIZATION),
         haul = as.factor(HAUL_NO), 
         date = mdy(DATE),
         month_num = as.numeric(substr(date, 6, 7)),
         month_name = case_when(month_num == 04~"April", 
                                month_num == 05~"May",
                                month_num == 06~"June",
                                month_num == 07~"July",
                                month_num == 08~"August",
                                month_num == 09~"September"),
         month_name = fct_relevel(month_name, "April", "May", "June", "July", "August", "September"),
         jdate = yday(mdy(DATE)),
         location = as.factor(LOCATION),
         lat = as.numeric(LAT),
         long = -1*(as.numeric(LONG)),
         depth = as.numeric(BOTTOM_DEPTH.m.),
         sample_num = as.factor(SAMPLE_NUM),
         length = as.numeric(LENGTH.mm.),
         num_individuals = as.numeric(NUM_INDIVIDUALS),
         WW = as.numeric(WET_WEIGHT.g.),
         DW = as.numeric(DRY_WEIGHT.g.),
         afdw = as.numeric(AFDW.g.),
         water_content = as.numeric(WATER_CONTENT..mass.),
         water_content_corrected = as.numeric(WATER_CONTENT..mass.AFDW.),
         wet_dry_ratio = as.numeric(WET.DRY),
         wet_dry_ratio_corrected = as.numeric(WET.DRY.AFDW.),
         subsample_weight = as.numeric(SUBSAMPLE_WEIGHT.g.),
         gross_heat = as.numeric(GROSS_HEAT.J.Kg.),
         ED_dry = as.numeric(ENERGY_DENSITY.kJ.g.DRY.),
         ED_wet = as.numeric(ENERGY_DENSITY.kJ.g.WET.),
         ED_wet_corrected = as.numeric(ENERGY_DENSITY.kJ.g.WET.AFDW.),
         notes = as.character(NOTES)
  ) 



Prey_Ew_summ <- combustion_df %>% 
  group_by(SPECIES) %>% 
  summarise(
            En_dens_25 = quantile(ENERGY_DENSITY.kJ.g.WET., 0.25, na.rm = T),
            En_dens_med = median(ENERGY_DENSITY.kJ.g.WET., na.rm = T),
            En_dens_75 = quantile(ENERGY_DENSITY.kJ.g.WET., 0.75, na.rm = T),
            Med_AFDW = median(AFDW.g., na.rm = T),
            Dry_wt = median(DRY_WEIGHT.g., na.rm = T),
            AFDW_prop = 1- (Med_AFDW/Dry_wt)
            )

#GLMM for Ew and ED by species----

combustion_df_for_analy <- combustion_df %>% 
   mutate(EW_sp = ifelse(SPECIES == "E. mordax", "anch", SPECIES))

Ew_glmm <- lmer(ENERGY_DENSITY.kJ.g.WET. ~ EW_sp + (1|month_name) + (1|haul), 
                data = 
                  #filter(combustion_df, SPECIES %in% c("E. mordax", "S. sagax"))
                  combustion_df_for_analy
                )
summary(Ew_glmm)
  

ED_glmm <- lmer(ENERGY_DENSITY.kJ.g.WET.AFDW. ~ EW_sp + (1|month_name) + (1|haul), 
                data = 
                  #filter(combustion_df, SPECIES %in% c("E. mordax", "S. sagax"))
                  combustion_df_for_analy
)
summary(ED_glmm)




#GLMM for Ew within species by size (length and weight)----

Ew_length_glmm <- lmer(ENERGY_DENSITY.kJ.g.WET. ~ LENGTH.mm. + (1|month_name), 
                       data = 
                      #filter(combustion_df, SPECIES == "E. pacifica/ T. spinifera")
                       #filter(combustion_df, SPECIES == "E. mordax")
                       #filter(combustion_df, SPECIES == "S. sagax")
                       filter(combustion_df, SPECIES == "D. opalescens")
)
summary(Ew_length_glmm)



Ew_wt_glmm <- lmer(ENERGY_DENSITY.kJ.g.WET. ~ (WW/num_individuals) + (1|month_name), 
                       data = 
                      #filter(combustion_df, SPECIES == "E. pacifica/ T. spinifera")
                      #filter(combustion_df, SPECIES == "E. mordax")
                       #filter(combustion_df, SPECIES == "S. sagax")
                       filter(combustion_df, SPECIES == "D. opalescens")
)
summary(Ew_wt_glmm)





# Define the species levels to loop through
species_levels <- c("E. pacifica/ T. spinifera", "E. mordax", "S. sagax", "D. opalescens")

# Group the data by species and split into separate data frames
grouped_data <- combustion_df %>% 
  filter(SPECIES %in% species_levels) %>% 
  group_split(SPECIES)

# Create a list to store model summaries
model_summaries <- list()

# Loop through each group and fit the GLMM model
for (i in seq_along(grouped_data)) {
  # Extract the data for the current species
  species_data <- grouped_data[[i]]
  
  # Fit the GLMM model
  model <- lmer(ENERGY_DENSITY.kJ.g.WET. ~ WET_WEIGHT.g. + (1|month_name),
                data = species_data)
  
  # Extract and save the summary of the model to the list
  model_summaries[[species_levels[i]]] <- summary(model)
}

# Print the model summaries
for (species in species_levels) {
  cat("Summary for species:", species, "\n")
  print(model_summaries[[species]])
  cat("\n")
}







#LMs by month for species----

Month_glmm <- lm(ENERGY_DENSITY.kJ.g.WET. ~ as.factor(month_num), 
                data = 
                  filter(combustion_df, SPECIES == "E. mordax")
)
summary(Month_glmm)










# Junk code below here


Ew_length_glmm <- lmer(ENERGY_DENSITY.kJ.g.WET. ~ LENGTH.mm. + (1|month_name), 
                       data = 
                         filter(combustion_df, SPECIES == "E. pacifica/ T. spinifera")
)
summary(Ew_length_glmm)


Ew_length_glmm <- lmer(ENERGY_DENSITY.kJ.g.WET. ~ LENGTH.mm. + (1|month_name), 
                       data = 
                         filter(combustion_df, SPECIES == "E. mordax")
)
summary(Ew_length_glmm)

Ew_glmm_modsumm <- modelsummary(Ew_glmm, gof_omit = ".*",
                                statistic = c("conf.int",
                                              "s.e. = {std.error}", 
                                              "t = {statistic}",
                                              "p = {p.value}"))

  