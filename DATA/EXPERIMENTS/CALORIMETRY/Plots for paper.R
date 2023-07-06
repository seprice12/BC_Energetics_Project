#################################################################
  # CCLME prey bioenergetics plots
  # Matt Savoca and Samuel Price
  # Started on: 4/27/23
#################################################################
  
  
#### Load Libraries ####
library(ggpubr)
library(maps)
# library("ggOceanMaps")
# library(ggOceanMapsData)
library(tidyverse)
library(grid)
library(gridExtra)
library(lattice)
library(scales)
library(patchwork)


pal = c("E. pacifica/ T. spinifera" = "#F53D3F",
        "E. mordax" = "#B0B7F5",
        "S. sagax" = "#F5CF4A",
        "D. opalescens" = "#62F58B")



#### Load Data ####

# Drying Curves

drying_df <- read.csv("DATA/EXPERIMENTS/DRYING CURVES/DRYING_CURVES_WEIGHTS.csv") 
drying_krill <- read.csv("DATA/EXPERIMENTS/DRYING CURVES/DRYING_CURVES_WEIGHTS - KRILL.csv") %>% 
  mutate(time = as.numeric(TIME.h.))
drying_anchovy <- read.csv("DATA/EXPERIMENTS/DRYING CURVES/DRYING_CURVES_WEIGHTS - ANCHOVY.csv")
drying_sardine <- read.csv("DATA/EXPERIMENTS/DRYING CURVES/DRYING_CURVES_WEIGHTS - SARDINE.csv")
drying_squid <- read.csv("DATA/EXPERIMENTS/DRYING CURVES/DRYING_CURVES_WEIGHTS - SQUID.csv")

# Combustion Experiments
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

# Ash-Free Dry Weight Experiments
AFDW_df <- read.csv("SPECIES_EXPERIMENTS(AFDW).csv") %>%
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


abbr_binom = function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}





# Figure 1 - Map of Sample Collection Sites---- 


combustion_df_krillmap <- combustion_df %>% 
  filter(SPECIES=="E. pacifica/ T. spinifera")

KRILL_map <- basemap(limits = c(-126, -120, 34.5, 44.5), 
                     land.col = "burlywood",
                     # bathymetry = TRUE, 
                     # bathy.style = "contour_grey",
                     rotate = TRUE) + 
  geom_point(data = combustion_df_krillmap, 
             aes(x = LONG, y = LAT, color = SPECIES),
             alpha = 0.5) +
  annotation_scale(location = "bl") + 
  annotate("text", x = c(-123.2, -122.25, -121.4), y = c(40.44, 38, 36.65), 
           label = c("Cape Mendocino","San\nFrancisco","Monterey\nBay"), 
           size = 3) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  labs(color = "Species",
       x = "",
       y = "") +
  theme(legend.position="none")
KRILL_map

# Still need to work on getting this sizing right 
#ggsave("SPrice_KrillMap.pdf", width = 4, height = 8)




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




#### Figure 4 - Summary Statistics ----
summary_plot_uncorrected_facet<- ggplot(combustion_df, aes(x=SPECIES, y=ENERGY_DENSITY.kJ.g.WET.)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = SPECIES), size = 2, alpha = 0.4, width = 0.25) +
  facet_grid(~factor(SPECIES, levels = c("E. mordax", "S. sagax", "D. opalescens", "E. pacifica/ T. spinifera")), scales = "free_x") +
  labs(y = "Energy Density (kJ/g, ww)") +
  ylim(2,10) +
  theme_bw(base_size = 18) +
  scale_color_manual(values = pal) + 
  theme(axis.text.x = element_blank(),
        strip.text = element_text(face = "italic"),
        axis.ticks.x = element_blank(),
        axis.title.x.bottom = element_blank()) +
  guides(color = "none") 
summary_plot_uncorrected_facet

SPUF_boxplot_data <- layer_data(summary_plot_uncorrected_facet, 1) %>% 
  mutate(IQR = upper-lower) %>% 
  select(1:5, 27)
View(SPUF_boxplot_data)

summary_plot_uncorrected_facet

summary_plot_corrected_facet <- ggplot(combustion_df, aes(x=SPECIES, y=ENERGY_DENSITY.kJ.g.WET.AFDW.)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = SPECIES), alpha = 0.4, width = 0.25) +
  facet_grid(~factor(SPECIES, levels = c("E. mordax", "S. sagax", "D. opalescens", "E. pacifica/ T. spinifera")), scales = "free_x") +
  scale_color_manual(values = pal) +
  labs(y = "Energy Density (kJ/g, ww, AFDW corr)") +
  ylim(2,10) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_blank(),
        strip.text = element_text(face = "italic"),
        axis.ticks.x = element_blank(),
        axis.title.x.bottom = element_blank())  +
  guides(color = "none") 

summary_plot_corrected_facet


# ED_plot_comb = grid.arrange(summary_plot_uncorrected_facet, summary_plot_corrected_facet,
#              ncol=1)


Fig_4_EDplot <- ggarrange(summary_plot_uncorrected_facet, summary_plot_corrected_facet, 
                             labels = c("A", "B"), 
                             font.label = list(size = 18),
                             legend = "none",
                             ncol = 1, nrow = 2)
Fig_4_EDplot



ggsave("Fig. 4_ED_plot_comb.pdf", Fig_4_EDplot, width = 13, height = 11)


#### Figure 5 - Energy Density with sp by size ----


# ED vs Length

EDvsLength_uncorrected <- ggplot(combustion_df, aes(x=LENGTH.mm., y=ENERGY_DENSITY.kJ.g.WET.)) +
  geom_point(aes(color = SPECIES)) +
  geom_smooth(method = "lm") + 
  facet_grid(~factor(SPECIES, levels = c("E. mordax", "S. sagax", "D. opalescens", "E. pacifica/ T. spinifera")), scales = "free_x") +
  labs(x = "Average Length (mm)", y = "Energy Density (kJ/g, ww)") +
  scale_color_manual(values = pal) +
  theme_bw(base_size = 18) +
  theme(
    #axis.text.x = element_blank(),
    strip.text = element_text(face = "italic"),
    #axis.title.x.bottom = element_blank()
  ) +
  guides(color = "none") 

EDvsLength_uncorrected


# EDvsLength_corrected <- ggplot(combustion_df, aes(x=LENGTH.mm., y=ENERGY_DENSITY.kJ.g.WET.AFDW.)) +
#   geom_point() +
#   geom_smooth(method = "lm") + 
#   facet_grid(~factor(SPECIES, levels = c("E. mordax", "S. sagax", "D. opalescens", "E. pacifica/ T. spinifera")), scales = "free_x") +
#   labs(x = "Length (mm)", y = "Energy Density (kJ/g, WW, AFDW Corrected)") +
#   theme_bw(base_size = 14) +
#   theme(strip.text = element_text(face = "italic"))
# 
# EDvsLength_corrected
# 
# grid.arrange(EDvsLength_uncorrected, EDvsLength_corrected, ncol=1)

# ED vs Weight
EDvsWt_uncorrected <- ggplot(combustion_df, 
                                 aes(x=WW/num_individuals, y=ENERGY_DENSITY.kJ.g.WET.)) +
  geom_point(aes(color = SPECIES)) +
  geom_smooth(method = "lm") + 
  facet_grid(~factor(SPECIES, levels = c("E. mordax", "S. sagax", "D. opalescens", "E. pacifica/ T. spinifera")), scales = "free_x") +
  labs(x = "Wet Weight (g)", y = "Energy Density (kJ/g, ww)") +
  scale_color_manual(values = pal) +
  theme_bw(base_size = 18) +
  theme(
    #axis.text.x = element_blank(),
    strip.text = element_text(face = "italic"),
    #axis.title.x.bottom = element_blank()
  ) +
  guides(color = "none") 

EDvsWt_uncorrected

#ggsave("EDvsWeight_uncorrected.pdf")

# EDvsWeight_corrected <- ggplot(combustion_df, aes(x=WW/num_individuals, y=ENERGY_DENSITY.kJ.g.WET.AFDW.)) +
#   geom_point(aes(color = SPECIES)) +
#   geom_smooth(method = "lm") + 
#   facet_grid(~factor(SPECIES, levels = c("E. mordax", "S. sagax", "D. opalescens", "E. pacifica/ T. spinifera")), scales = "free_x") +
#   labs(x = "Wet Weight (g)", y = "Energy Density (kJ/g, ww, AFDW Corrected)") +
#   scale_color_manual(values = pal) +
#   theme_bw(base_size = 16) +
#   theme(strip.text = element_text(face = "italic"))
# 
# EDvsWeight_corrected
# 
# grid.arrange(EDvsWeight_uncorrected, EDvsWeight_corrected, ncol=1)


Fig_5_EDvsMorphoPlot <- ggarrange(EDvsLength_uncorrected, EDvsWt_uncorrected, 
                          labels = c("A", "B"), 
                          font.label = list(size = 20),
                          legend = "none",
                          ncol = 1, nrow = 2)
Fig_5_EDvsMorphoPlot

ggsave("Fig. 5_EDvsMorphoPlot.pdf", Fig_5_EDvsMorphoPlot, width = 13, height = 11)


### Figure 6 - Energy Density within sp by month ---- 

# Krill
krill_dates <- c('4/29/2021', '5/1/2021', '5/5/2021',
                 '5/11/2021', '5/22/2021', '5/24/2021',
                 '5/30/2021', '7/11/2022')


EDvsTIME_facet <- ggplot(filter(combustion_df, month_name !="NA"), aes(x = month_name, y = ENERGY_DENSITY.kJ.g.WET.)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(color = SPECIES),
              alpha=0.4, width = 0.2) +
  facet_wrap(SPECIES~., scales = "free_x") +
  labs(y = "Energy Density (kJ/g, ww)") +
  scale_color_manual(values = pal) +
  theme_bw(base_size = 22) + 
  theme(plot.title = element_text(face = "italic"),
        plot.title.position = "panel",
        axis.title.x.bottom = element_blank(),
        strip.text = element_text(face ="italic"),
        axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 0.25))  +
          guides(color = "none") 

EDvsTIME_facet

ggsave("Fig. 6_EDvsTIME_facet.pdf", EDvsTIME_facet, width = 13, height = 11)

