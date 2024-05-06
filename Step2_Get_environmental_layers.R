###

#Step 2 obtain environmental covariates

#Code to obtain environmental covariates using MARSPEC and COPERNICUS repositories

#Tania Valdivia Carrillo

###

#Install packages

list.of.packages=c("sdmpredictors","leaflet", "recipes","raster", "ncdf4","ggplot2","viridis","RColorBrewer",
                   "ggsn","rnaturalearth","rnaturalearthdata","kableExtra","biomod2","knitr","readr",
                   "magrittr","taxize","stars","dplyr","sf","scico","patchwork","stringr",
                   "stringi","vegan","tidyr", "librarian","sp","plyr","rgdal","mgcv","gratia")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = T)

library(librarian)

librarian::shelf(list.of.packages)
source('custom_functions.R')

#remotes::install_github("michaeldorman/geobgu", force = TRUE)
library("geobgu")
###
#setwd("~/Library/CloudStorage/GoogleDrive-tania.valdiviac@gmail.com/My Drive/2.2023/05_MURI_Module_3_Tania/Documents/Manuscript_eDNA-occurrenceSDM/Github/")
-------------------------------------------
  #Retrieve Environmental Data, 
  #this is provitional environmental data not from the sampling dates, but means from 1994 to 2009
------------------------------------------

# Extent of area of interest California Current
# latmax <- 48.5416666716337204
# latmin <-37.7916653901338577
# lonmax <- -122.5416692097981866
# lonmin <- -126.6250025431315152

# Extent of area of interest WA cost
latmax <- 50.0416
latmin <- 32.375
lonmax <- -115.7917
lonmin <- -130.2083

# Create a spatial bounding box for the area of interest using the 'sf' package:
# create a matrix:
bb <- cbind(c(lonmin,lonmax,lonmax,lonmin,lonmin),
            c(latmin,latmin,latmax,latmax,latmin)) %>%
  # put that matrix into a list, because that's what `st_polygon()` needs
  list() %>%
  # Make the matrix a 'simple features' polygon:
  sf::st_polygon() %>%
  # and let's make it a simple feature column and give it information about the projection:
  sf::st_sfc(crs="+proj=longlat +datum=WGS84") %>%
  # finally, let's put the sfc in a simple features data.frame in the variable `geometry`:
  sf::st_sf(name="Study Site",geometry=.)

--------------------------------------
  # Download environmental layers
  --------------------------------------
  
#Select envirnnomental variables. More infor see https://bio-oracle.org/code.php, http://marspec.org/
datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)
layers_all <- list_layers(datasets)
#write.csv(layers, "Environmental_layers_metadata.csv")

# Create directory and the list of environmental variables you need
#dir.create("downloaded_predictors/Environmental_Seascape/", showWarnings = FALSE)
#Which variables?

#Static variables
layers <- c("MS_bathy_5m","MS_biogeo05_dist_shore_5m","MS_biogeo06_bathy_slope_5m")

# Download marine layers to directory. 

sdmpredictors::load_layers(layers, datadir = "./downloaded_predictors/", rasterstack = TRUE)

####envCov_USA_surface_083_082019

bathy_083<-raster('./downloaded_predictors/MS_bathy_5m_lonlat.tif')
dist_shore_083<-raster('./downloaded_predictors/MS_biogeo05_dist_shore_5m_lonlat.tif')
slope_083<-raster('./downloaded_predictors/MS_biogeo06_bathy_slope_5m_lonlat.tif')

#Manually obtain from env layers from COPERNICUS and load them
#surface_083_082019
ESV_surface_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/ESV.tif')
epi_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/epi.tif')
lmeso_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/lmeso.tif')
umeso_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/umeso.tif')
zooc_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/zooc.tif')
zeu_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/zeu.tif')
npp_surface_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/npp.tif')
NWV_surface_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/NWV.tif')
OML_surface_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/OML.tif')
pelagicL_depth_surface_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/pelagicL_depth.tif')
SSH_surface_083_082019<-raster('./downloaded_predictors/SSH.tif')
SWS_surface_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/SWS.tif')
SWT_surface_083_082019<-raster('./downloaded_predictors/SWT.tif')

#Extract latitude and longitude
lat_083 <- lon_083 <- SWT_surface_083_082019
xy_083 <- coordinates(SWT_surface_083_082019)
lon_083[] <- xy_083[, 1]
lat_083[] <- xy_083[, 2]
plot(lat_083)

#Create a raster brick
envCov_USA_surface_083_082019<-raster::brick(bathy_083,slope_083,dist_shore_083,ESV_surface_083_082019,npp_surface_083_082019,
                                             NWV_surface_083_082019,OML_surface_083_082019,pelagicL_depth_surface_083_082019,
                                             SSH_surface_083_082019,SWS_surface_083_082019,epi_surface_083_022019,lmeso_surface_083_022019,
                                             umeso_surface_083_022019,zooc_surface_083_022019,zeu_surface_083_022019,
                                             SWT_surface_083_082019,lon_083,lat_083)

names(envCov_USA_surface_083_082019)
names(envCov_USA_surface_083_082019)[4]<-"ESV"
names(envCov_USA_surface_083_082019)[5]<-"npp"
names(envCov_USA_surface_083_082019)[6]<-"NWV"
names(envCov_USA_surface_083_082019)[8]<-"pelagicL_depth"
names(envCov_USA_surface_083_082019)[10]<-"SWS"
names(envCov_USA_surface_083_082019)[11]<-"epi"
names(envCov_USA_surface_083_082019)[12]<-"lmeso"
names(envCov_USA_surface_083_082019)[13]<-"umeso"
names(envCov_USA_surface_083_082019)[14]<-"zooc"
names(envCov_USA_surface_083_082019)[15]<-"zeu"
names(envCov_USA_surface_083_082019)[16]<-"SWT"
names(envCov_USA_surface_083_082019)[17]<-"lon"
names(envCov_USA_surface_083_082019)[18]<-"lat"
plot(envCov_USA_surface_083_082019)

#Mask to WA coast
WA_short<- readOGR("./downloaded_predictors/shp/Mask_WA_Coast_short_B.shp")

bathy_spol_083_082019 <- raster::mask(bathy_083, WA_short, updateNA=TRUE) 
dist_shore_spol_083_082019 <- raster::mask(dist_shore_083, WA_short,updateNA=TRUE) 
slope_spol_083_082019 <- raster::mask(slope_083, WA_short,updateNA=TRUE) 
lon_spol_083_082019 <- raster::mask(lon_083, WA_short,updateNA=TRUE) 
lat_spol_083_082019 <- raster::mask(lat_083, WA_short,updateNA=TRUE)
SWT_surface_spol_083_082019 <- raster::mask(SWT_surface_083_082019, WA_short,updateNA=TRUE)
SSH_surface_spol_083_082019 <- raster::mask(SSH_surface_083_082019, WA_short,updateNA=TRUE)

envCov_spol_surface_083_082019 <- raster::mask(envCov_USA_surface_083_082019, WA_short,updateNA=TRUE)
plot(envCov_spol_surface_083_082019)
writeRaster(envCov_USA_surface_083_082019,"./downloaded_predictors/envCov_USA_surface_083_082019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)
writeRaster(envCov_spol_surface_083_082019,"./downloaded_predictors/envCov_spol_surface_083_082019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)

envCov_spol_surface_083_082019_df <- as.data.frame(envCov_spol_surface_083_082019, xy = TRUE)


(p <- ggplot() +
    geom_raster(data = envCov_spol_surface_083_082019_df, aes(x = x, y = y, fill = lat)) +
    scale_fill_viridis_c() +
    coord_quickmap() +
    theme(legend.key.width = unit(1, "cm"), 
          legend.key.height = unit(1, "cm"), 
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          plot.title = element_text(size = 18))+
    labs(title = "Bathymetry"))
#ggsave(".png", plot = p, width = 8, height = 6, units = "in", dpi = 300)

#Extract the environmental information on the metadata file

#Extract env_info per coordinate
#coordinates(coord) <-c("lon", "lat")
md.taxa.long.formated<-read.csv("./dataframes/md_taxa_long_formated.csv") #ordered by latitude 
coord <- md.taxa.long.formated[,c(24,23)] # Order must be lon lat
# coord.dec<-SpatialPoints(coord, proj4string=CRS("+proj=longlat"))
# coord.dec.df<-as.data.frame(coord)
# coords <- st_as_sf(coord, coords = c("lon", "lat"))
sites.names<-md.taxa.long.formated$sampleID_station

envCov_surface_083_df<-raster::extract(envCov_spol_surface_083_082019,coord,df=TRUE)

md.taxa.long.formated.env083<-bind_cols(md.taxa.long.formated,envCov_surface_083_df)

write.csv(envCov_surface_083_df, "./dataframes/envCov_surface_083_df.csv")
write.csv(md.taxa.long.formated.env083, "./dataframes/md_taxa_long_formated_env083.csv")


