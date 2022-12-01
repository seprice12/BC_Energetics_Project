# Map of Sample Collection Sites 


#### Libraries ####
library("ggmap")
library("ggpubr")
library("maps")
install.packages("ggOceanMapsData", repos = c("https://mikkovihtakari.github.io/drat", "https://cloud.r-project.org"))
library("ggOceanMaps")
library(ggOceanMapsData)

abbr_binom = function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}

# pal_whale_mp <- c("M. novaeangliae" = "gray30", 
#                   "B. physalus" = "chocolate3", 
#                   "B. musculus" = "dodgerblue2")


#Load data----

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



#Load whale location (tag on) data
whale_tagon <- read_csv("alldata_CAwhales.csv") %>% 
  mutate(SpeciesCode = substr(deployID, 1, 2),
         Species = case_when(SpeciesCode %in% c("bw", "Bm") ~ "Balaenoptera musculus",
                             SpeciesCode %in% c("bp", "Bp") ~ "Balaenoptera physalus",
                             SpeciesCode %in% c("mn", "Mn") ~ "Megaptera novaeangliae"))


#Manually add NorCal plastic sampling locations
Choy_sam_loc <- data.frame(
  lon = c(-121.82,-122.05), 
  lat = c(36.8,36.7),
  depth = "depth"
)


BoxKash_sam_loc <- data.frame(
  lon = c(-122.9,-122.6,-123.4,
          -121.83595,-122.0129,-122.3055,-122.7199333), 
  lat = c(37.45,37.65,37.98,
          36.73223333,36.95426667, 36.39216667, 35.75838333),
  depth = "surface"
)



#Manually add SoCal plastic sampling locations
LattinDoyle_sam_loc <- data.frame(
  lon = c(-118.5), 
  lat = c(33.9),
  depth = "surface"
)



#Generate maps----

#NorCal Map           
NorCal_map <- basemap(limits = c(-123.5, -121, 35.75, 38.25), 
                      land.col = "burlywood",
                      # bathymetry = TRUE, 
                      # bathy.style = "contour_grey",
                      rotate = TRUE) + 
  geom_point(data = whale_tagon, 
             aes(x = longitude, y = latitude, color = abbr_binom(Species)),
             alpha = 0.5) +
  scale_colour_manual(values = pal_whale_mp,
                      guide =
                        guide_legend(label.theme = element_text(face = "italic", size = 10))) +
  geom_point(data = Choy_sam_loc,
             aes(x = lon, y = lat),
             shape = 17, color = "firebrick3") +
  
  geom_point(data = BoxKash_sam_loc,
             aes(x = lon, y = lat),
             shape = 2, color = "firebrick3") +
  annotation_scale(location = "br") + 
  annotate("text", x = c(-122.35,-121.78), y = c(37.725,36.6), 
           label = c("San\nFrancisco\nbay area","Monterey"), 
           size = 3) +
  #  annotation_north_arrow(location = "tr", which_north = "true") +
  labs(color = "Species",
       x = "Longitude",
       y = "Latitude")
NorCal_map



#SoCal Map   
SoCal_map <- basemap(limits = c(-121, -116.5, 32, 35), 
                     land.col = "burlywood",
                     # bathymetry = TRUE, 
                     # bathy.style = "contour_grey",
                     rotate = TRUE) + 
  geom_point(data = whale_tagon, 
             aes(x = longitude, y = latitude, color = abbr_binom(Species)),
             alpha = 0.5) +
  scale_colour_manual(values = pal_whale_mp,
                      guide =
                        guide_legend(label.theme = element_text(face = "italic", size = 10))) +
  geom_point(data = LattinDoyle_sam_loc,
             aes(x = lon, y = lat),
             shape = 2, color = "firebrick3") +
  annotation_scale(location = "br") + 
  annotate("text", x = c(-119.75,-118.2, -117.1), y = c(34.55,33.9, 32.85), 
           label = c("Santa\nBarbara","Los\nAngeles","San\nDiego"), 
           size = 3) +
  #  annotation_north_arrow(location = "tr", which_north = "true") +
  labs(color = "Species",
       x = "Longitude",
       y = "Latitude")
SoCal_map



#Combined map fig
whale_MP_map <- ggarrange(NorCal_map, SoCal_map,
                          labels = c("A","B"), 
                          widths = c(0.9, 1.1),
                          common.legend = TRUE,
                          ncol = 2, nrow = 1)
whale_MP_map
