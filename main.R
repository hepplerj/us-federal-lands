# U.S. Federal Lands
# 2016-01-06 | Jason A. Heppler

# maps
library("rgdal")
library("maptools")
library("ggplot2")
library("plyr")
library("ggmap")
library("RColorBrewer")
library("ggthemes")

# data
library("tidyr")
library("dplyr")
library("stringr")
library("car")

ifelse(!dir.exists(file.path("figures")), dir.create(file.path("figures")), FALSE)
stringsAsFactors = FALSE

###--------------------------------------------------
### Shapefile prep (read, convert projection, and create DF)
###--------------------------------------------------

wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

fedlands.shp <- readOGR("/research-data/federal-data/fedlanp010g.shp/", "fedlanp010g")
fedlands.shp <- spTransform(fedlands.shp, CRS(wgs.84))
fedlands.shp@data$id <- rownames(fedlands.shp@data)
fedlands <- fortify(fedlands.shp)
fedlands.df <- join(fedlands, fedlands.shp@data, by="id")

# Tribal lands
tribal.shp <- readOGR("/research-data/tribal-lands/", "tl_2012_us_aiannh")
tribal.shp <- spTransform(tribal.shp, CRS(wgs.84))
tribal.shp@data$id <- rownames(tribal.shp@data)
tribal <- fortify(tribal.shp)
tribal.df <- join(tribal, tribal.shp@data, by="id")

###--------------------------------------------------
### Clean up the dataframe
###--------------------------------------------------

# Simplify the FEATURE1 category to more discernable features
fedlands.df$FEATURE1 %>% unique()
fedlands.df$landtype <- recode(fedlands.df$FEATURE1, "c('AF Guard', 'Air Force', 'Army', 'Army Guard', 'Coast Guard', 'Marine Corps', 'Navy',  'Test Site') 
                               = 'Military Lands'")
fedlands.df$landtype <- recode(fedlands.df$landtype, "c('Land', 'National Laboratory', 'Research Natural Area', 'MWAA')  
                               = 'Other'")
fedlands.df$landtype <- recode(fedlands.df$landtype, "c('National Wilderness Area', 'National Preserve', 'National Historic Reserve',  'National Wilderness Area', 'National Seashore', 'National Scenic Area', 'National Grassland',  'National Natural Landmark', 'National Conservation Area', 'National Lakeshore', 'Wild and Scenic River') 
                               = 'Wilderness Area or Preserve'")
fedlands.df$landtype <- recode(fedlands.df$landtype, "c('National Wildlife Refuge', 'National Fish Hatchery', 'National Game Preserve', 'Waterfowl Production Area', 'Wildlife Management Area', 'Coordinated Area')
                               = 'Wildlife Refuges'")
fedlands.df$landtype <- recode(fedlands.df$landtype, "c('National Forest') 
                               = 'National Forest'")
fedlands.df$landtype <- recode(fedlands.df$landtype, "c('National Park', 'National Battlefield', 'National Cemetery', 'National Historic Park', 
                               'National Memorial', 'National Military Park', 'National Historic Reserve', 'National Monument', 'National Recreation Area', 'National Parkway')
                               = 'National Park'")
fedlands.df$landtype <- recode(fedlands.df$landtype, "c('Indian Reservation')  = 'Tribal'",
                      levels = c("Military Lands", "Public Domain Land", "Wilderness Area or Preserve", "Wildlife Refuges", "National Park", 
                                 "National Forest", "Lake", "Tribal", "Other"))

# Check
#fedlands.df$landtype %>% unique()

###--------------------------------------------------
### Map: U.S.
###--------------------------------------------------

map <- get_map(location="40.9639, -110.8095", zoom=5, color = "bw")
us.map <- ggmap(map) +
  geom_polygon(data=fedlands.df, 
               aes(map_id = id, x=long, y=lat, group=group, fill=landtype),
               size=.01,
               color="white",
               alpha=.7) +
  geom_polygon(data=tribal.df,
               aes(map_id = id, x=long, y=lat, group=group),
               fill="#a65628",
               alpha=0.7,
               color="white",
               size=.01)

# Theme and presentation
federallands.all <- us.map + 
  theme_map() +
  ggtitle("Western Federal Lands") +
  scale_fill_brewer("Land Type or Purpose", palette="Set1") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "right"
  ) 

# PNG and PDF
ggsave("figures/federal-lands.png", federallands.all, height=8, width=12, dpi=300)
pdf(file="figures/federal-lands.pdf", height = 9, width = 10)

###--------------------------------------------------
### Map: Oregon
###--------------------------------------------------

oregon <- get_map(location="Oregon", zoom=6, color = "bw")
oregon.map <- ggmap(oregon) +
  geom_polygon(data=fedlands.df, 
               aes(map_id = id, x=long, y=lat, group=group, fill=landtype),
               size=.01,
               color="white",
               alpha=.7) +
  geom_polygon(data=tribal.df,
               aes(map_id = id, x=long, y=lat, group=group),
               fill="#ff7f00",
               alpha=0.7,
               color="white",
               size=.01)

# Theme and presentation
oregon.all <- oregon.map + 
  theme_map() +
  ggtitle("Oregon's Federal Lands") +
  scale_fill_brewer("Land Type or Purpose", palette="Set1") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "right"
  ) 

# PNG and PDF
ggsave("figures/oregon-lands.png", oregon.all, height=8, width=12, dpi=300)
pdf(file="figures/oregon-lands.pdf", height = 9, width = 10)

###--------------------------------------------------
### Agency
###--------------------------------------------------

fedlands.df$administrator <- recode(fedlands.df$ADMIN1,
                                "c('GSA', 'HHS', 'DOT', 'NASA', 'MWAA', 'DOL', 'DOJ', 'DOC', 'BIA', 'USDA', 'VA') = 'Other'; 'BLM' = 'Bureau of Land Management'; 'BOR' = 'Bureau of Reclamation'; 'DOD' = 'Dept of Defense'; 'DOE' = 'Dept of Energy'; 'NPS' = 'Park Service'; 'FS' = 'Forest Service'; 'FWS' = 'Fish and Wildlife Service'; 'TVA' = 'Tenn. Valley Authority'",
                                levels = c("Bureau of Land Management", "Forest Service",
                                           "Dept of Defense", "Park Service", "Bureau of Reclamation",
                                           "Dept of Energy", "Fish and Wildlife Service",
                                           "Tenn. Valley Authority", "Other"))


admin.map <- ggmap(map) +
  geom_polygon(data=fedlands.df, 
               aes(map_id = id, x=long, y=lat, group=group, fill=administrator),
               size=.01,
               color="white",
               alpha=.7) +
  geom_polygon(data=tribal.df,
             aes(map_id = id, x=long, y=lat, group=group),
             fill="#999999",
             alpha=0.7,
             color="white",
             size=.01)

# Theme and presentation
admin.all <- admin.map + 
  theme_map() +
  ggtitle("Federal Land Administration") +
  scale_fill_brewer("Administration", palette="Set1") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "right"
  ) 

# PNG and PDF
ggsave("figures/admin-lands.png", admin.all, height=8, width=12, dpi=300)
pdf(file="figures/admin-lands.pdf", height = 9, width = 10)