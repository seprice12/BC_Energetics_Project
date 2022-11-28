#### Libraries ####
library(tidyverse)
library(dplyr)
library(grid)
library(gridExtra)
library(lattice)
library(lubridate)
library(scales)


#### Load in Data ####

# Drying Curves

drying_df <- read.csv("DATA/EXPERIMENTS/DRYING CURVES/DRYING_CURVES_WEIGHTS.csv") 
drying_krill <- read.csv("DATA/EXPERIMENTS/DRYING CURVES/DRYING_CURVES_WEIGHTS - KRILL.csv") %>% 
  mutate(time = as.numeric(TIME.h.))
drying_anchovy <- read.csv("DATA/EXPERIMENTS/DRYING CURVES/DRYING_CURVES_WEIGHTS - ANCHOVY.csv")
drying_sardine <- read.csv("DATA/EXPERIMENTS/DRYING CURVES/DRYING_CURVES_WEIGHTS - SARDINE.csv")
drying_squid <- read.csv("DATA/EXPERIMENTS/DRYING CURVES/DRYING_CURVES_WEIGHTS - SQUID.csv")

# Combustion Experiments
combustion_df <- read.csv("DATA/EXPERIMENTS/CALORIMETRY/EnergyDensity.csv") %>% 
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

# Ash-Free Dry Weight Experiments
AFDW_df <- read.csv("DATA/EXPERIMENTS/CALORIMETRY/SPECIES_EXPERIMENTS(AFDW).csv") %>%
  mutate(species = as.character(SPECIES),
         organization = as.character(ORGANIZATION),
         haul = as.factor(HAUL_NO),
         date = mdy(DATE),
         jdate = yday(mdy(DATE)),
         location = as.factor(LOCATION),
         sample_num = as.factor(SAMPLE_NO),
         num_individuals = as.numeric(NUM_INDIVIDUALS),
         length = as.numeric(LENGTH.mm.),
         WW = as.numeric(WET_WEIGHT.g.),
         DW = as.numeric(DRY_WEIGHT.g.), 
         water_content = as.numeric(WATER_CONTENT..mass.),
         ash_weight = as.numeric(ASH_WEIGHT.g.),
         ash_weight_percent = as.numeric(ASH_WEIGHT..WW.),
         afdw = as.numeric(AFDW.g.),
         mean_afdw = as.numeric(AVERAGE_AFDW.g.),
         mean_ash_weight = as.numeric(AVERAGE_ASH_WEIGHT..WW.),
         water_content_corrected = as.numeric(WATER_CONTENT..mass.AFDW.)
  )


#### Figure 3 - Drying Curves ####
# Krill
drying_curve_krill <- ggplot(data = drying_krill, aes(x = TIME.h., y = WEIGHT.g.)) +
  geom_point() +
  geom_smooth(color="#F53D3F") +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "italic"),
        plot.title.position = "panel",
        plot.margin= unit(c(1, 1, 10, 10), "pt")) +
  scale_x_continuous(limits = c(0, 72), breaks = c(0, 24, 48, 72)) +
  ggtitle("E. pacifica/ T. spinifera")

drying_curve_krill

# Anchovy
drying_curve_anchovy <- ggplot(data = drying_anchovy, aes(x = TIME.h., y = WEIGHT.g.)) +
  geom_point() +
  geom_smooth(color="#B0B7F5") +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "italic"),
        plot.title.position = "panel",
        plot.margin= unit(c(1, 1, 10, 10), "pt")) +
  scale_x_continuous(limits = c(0, 120), breaks = c(0, 24, 48, 72, 96, 120)) +
  ggtitle("E. mordax")

drying_curve_anchovy

# Sardine
drying_curve_sardine <- ggplot(data = drying_sardine, aes(x = TIME.h., y = WEIGHT.g.)) +
  geom_point() +
  geom_smooth(color = "#F5CF4A") +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "italic"),
        plot.title.position = "panel",
        plot.margin= unit(c(1, 1, 10, 10), "pt")) +
  scale_x_continuous(limits = c(0, 120), breaks = c(0, 24, 48, 72, 96, 120)) +
  ggtitle("S. sagax")

drying_curve_sardine

# Squid
drying_curve_squid <- ggplot(data = drying_squid, aes(x = TIME.h., y = WEIGHT.g.)) +
  geom_point() +
  geom_smooth(color="#62F58B") +
  scale_x_continuous(limits = c(0, 96), breaks = c(0, 24, 48, 72, 96)) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "italic"),
        plot.title.position = "panel",
        plot.margin= unit(c(1, 1, 10, 10), "pt")) +
  ggtitle("D. opalescens")

drying_curve_squid

# Group graphs into single panel
grid.arrange(drying_curve_krill,
             drying_curve_squid,
             drying_curve_anchovy,
             drying_curve_sardine,
             ncol = 1,
             left = textGrob("Weight (grams)", rot = 90, gp=gpar(fontsize=15, fontfamily="Helvetica")),
             bottom = textGrob("Time (hours)", gp=gpar(fontsize=15, fontfamily="Helvetica"))
)

# facet_wrap attempt

#drying_curves <- ggplot(data = drying_df, aes(x = TIME.h., y = WEIGHT.g.)) +
#  geom_point() +
#  geom_smooth() +
#  labs(x = "Time (hours)", y = "Weight (grams)") +
#  facet_wrap(.~SPECIES) +
#  scale_x_continuous(limits = c(0, 120), breaks = c(0, 24, 48, 72, 96, 120)) +
#  theme_bw(base_size = 12) +
#  theme(legend.position = "none",
#        strip.text = element_text(face = "italic"))

#  drying_curves

#### Figure 4 - Summary Statistics ####
summary_plot_uncorrected_facet<- ggplot(combustion_df, aes(x=SPECIES, y=ENERGY_DENSITY.kJ.g.WET.)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.2) +
  facet_grid(~factor(SPECIES, levels = c("E. mordax", "S. sagax", "D. opalescens", "E. pacifica/ T. spinifera")), scales = "free_x") +
  labs(y = "Energy Density (kJ/g, Wet Weight)") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_blank(),
        strip.text = element_text(face = "italic"),
        axis.ticks.x = element_blank(),
        axis.title.x.bottom = element_blank()) 

summary_plot_uncorrected_facet

summary_plot_corrected_facet <- ggplot(combustion_df, aes(x=SPECIES, y=ENERGY_DENSITY.kJ.g.WET.AFDW.)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.2) +
  facet_grid(~factor(SPECIES, levels = c("E. mordax", "S. sagax", "D. opalescens", "E. pacifica/ T. spinifera")), scales = "free_x") +
  labs(y = "Energy Density (kJ/g, Wet Weight, AFDW Corrected)") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_blank(),
        strip.text = element_text(face = "italic"),
        axis.ticks.x = element_blank(),
        axis.title.x.bottom = element_blank())

summary_plot_corrected_facet

grid.arrange(summary_plot_uncorrected_facet, summary_plot_corrected_facet,
             ncol=1)


#### Figure 5 - Energy Density vs. Variable Plots ####


# ED vs DW and AFDW

ED_DW <- ggplot(combustion_df, aes(x = log10(DW), y = log10(ENERGY_DENSITY.kJ.g.WET.))) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(.~SPECIES, scales = "free_x") +
  labs(x = "Log10 % Dry Weight", y = "Log10 ED") +
  theme_bw()

ED_DW    

ED_AFDW <- ggplot(combustion_df, aes(x = log10(afdw), y = ED_wet_corrected)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(.~SPECIES, scales = "free_x") +
  labs(x = "Log10(AFDW)", y = "Log10 ED") +
  theme_bw()

ED_AFDW      

# ED vs Length

EDvsLength_uncorrected <- ggplot(combustion_df, aes(x=LENGTH.mm., y=ENERGY_DENSITY.kJ.g.WET.)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_grid(~factor(SPECIES, levels = c("E. mordax", "S. sagax", "D. opalescens", "E. pacifica/ T. spinifera")), scales = "free_x") +
  labs(x = "Average Length (mm)", y = "Energy Density (kJ/g, WW)") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_blank(),
        strip.text = element_text(face = "italic"),
        axis.title.x.bottom = element_blank())

EDvsLength_uncorrected

EDvsLength_corrected <- ggplot(combustion_df, aes(x=LENGTH.mm., y=ENERGY_DENSITY.kJ.g.WET.AFDW.)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_grid(~factor(SPECIES, levels = c("E. mordax", "S. sagax", "D. opalescens", "E. pacifica/ T. spinifera")), scales = "free_x") +
  labs(x = "Length (mm)", y = "Energy Density (kJ/g, WW, AFDW Corrected)") +
  theme_bw(base_size = 14) +
  theme(strip.text = element_text(face = "italic"))

EDvsLength_corrected

grid.arrange(EDvsLength_uncorrected, EDvsLength_corrected, ncol=1)

# ED vs Weight
EDvsWeight_uncorrected <- ggplot(combustion_df, aes(x=WET_WEIGHT.g., y=ENERGY_DENSITY.kJ.g.WET.)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_grid(~factor(SPECIES, levels = c("E. mordax", "S. sagax", "D. opalescens", "E. pacifica/ T. spinifera")), scales = "free_x") +
  labs(x = "Wet Weight (g)", y = "Energy Density (kJ/g, WW)") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_blank(),
        strip.text = element_text(face = "italic"),
        axis.title.x.bottom = element_blank())

EDvsWeight_uncorrected

EDvsWeight_corrected <- ggplot(combustion_df, aes(x=WET_WEIGHT.g., y=ENERGY_DENSITY.kJ.g.WET.AFDW.)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_grid(~factor(SPECIES, levels = c("E. mordax", "S. sagax", "D. opalescens", "E. pacifica/ T. spinifera")), scales = "free_x") +
  labs(x = "Wet Weight (g)", y = "Energy Density (kJ/g, WW, AFDW Corrected)") +
  theme_bw(base_size = 14) +
  theme(strip.text = element_text(face = "italic"))

EDvsWeight_corrected

grid.arrange(EDvsWeight_uncorrected, EDvsWeight_corrected, ncol=1)

# ED vs AFDW #
EDvsAFDW_corrected <- ggplot(combustion_df, aes(x=AFDW.g., y=ENERGY_DENSITY.kJ.g.WET.AFDW.)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(.~SPECIES, scales = "free_x") +
  labs(x = "Ash-free Dry Weight (g)", y = "Energy Density (kJ/g, WW, AFDW Corrected)") +
  theme_bw(base_size = 14) +
  theme(strip.text = element_text(face = "italic"))

EDvsAFDW_corrected

# Energy Density Over Time # 

# Krill
krill_dates <- c('4/29/2021', '5/1/2021', '5/5/2021',
                 '5/11/2021', '5/22/2021', '5/24/2021',
                 '5/30/2021', '7/11/2022')

EDvsTIME_krill <- ggplot(filter(combustion_df, SPECIES == "E. pacifica/ T. spinifera"),
                         aes(x = month_name, y = ED_wet)) +
  geom_boxplot(width=.75) + 
  theme_bw(base_size = 10) + 
  theme(plot.title = element_text(face = "italic"),
        plot.title.position = "panel",
        axis.title.y = element_blank(),
        axis.title.x.bottom = element_blank()) +
  ggtitle("E. pacifica/ T. spinifera")


EDvsTIME_krill

# Anchovy
EDvsTIME_anchovy <- ggplot(filter(combustion_df, SPECIES == "E. mordax"),
                           aes(x = month_name, y = ED_wet)) +
  geom_boxplot() + 
  theme_bw(base_size = 10) + 
  theme(legend.position = "right",
        legend.title = element_text(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(.5, 'cm'),
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(face = "italic"),
        plot.title.position = "panel",
        axis.title.y = element_blank(),
        axis.title.x.bottom = element_blank()) +
  ggtitle("E. mordax")

EDvsTIME_anchovy

# Sardine
EDvsTIME_sardine <- ggplot(filter(combustion_df, SPECIES == "S. sagax"),
                           aes(x = month_name, y = ED_wet)) +
  geom_boxplot() + 
  theme_bw(base_size = 10) + 
  theme(legend.position = "right",
        legend.title = element_text(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(.5, 'cm'),
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(face = "italic"),
        plot.title.position = "panel",
        axis.title.y = element_blank(),
        axis.title.x.bottom = element_blank()) +
  ggtitle("S. sagax")

EDvsTIME_sardine

# Squid
EDvsTIME_squid <- ggplot(filter(combustion_df, SPECIES == "D. opalescens"),
                         aes(x = month_name, y = ENERGY_DENSITY.kJ.g.WET.)) +
  geom_boxplot() + 
  theme_bw(base_size = 10) + 
  theme(legend.position = "right",
        legend.title = element_text(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(.5, 'cm'),
        legend.key.width = unit(1, 'cm'),
        plot.title = element_text(face = "italic"),
        plot.title.position = "panel",
        axis.title.y = element_blank(),
        axis.title.x.bottom = element_blank()) +
  ggtitle("D. opalescens")

EDvsTIME_squid

# Grid 

grid.arrange(EDvsTIME_krill, EDvsTIME_anchovy, EDvsTIME_sardine, EDvsTIME_squid, ncol = 1)

ggsave("EDvsMonthBox.pdf", width = 4, height = 4)

# Facet Wrap Version

EDvsTIME_facet <- ggplot(filter(combustion_df, month_name !="NA"), aes(x = month_name, y = ENERGY_DENSITY.kJ.g.WET.)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(alpha=0.4, width = 0.2) +
  facet_wrap(SPECIES~., scales = "free_x") +
  theme_bw(base_size = 10) + 
  theme(plot.title = element_text(face = "italic"),
        plot.title.position = "panel",
        axis.title.y = element_blank(),
        axis.title.x.bottom = element_blank(),
        strip.text = element_text(face ="italic"),
        axis.text.x = element_text(angle = -45)) +
  labs(y = "Energy Density (kJ/g, Wet Weight")

EDvsTIME_facet

ggsave("EDvsMonthBox.pdf", width = 4, height = 4)


## Energy Density by julian date 
EDvsJulian <- ggplot(combustion_df, aes(x = month, y = ED_wet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(.~SPECIES, scales = "free_x")

EDvsJulian

## ED vs Month

#EDvsT_krill_v2 <- ggplot(filter(combustion_df, SPECIES == "E. pacifica/ T. spinifera"),
#aes(x = as.factor()))



