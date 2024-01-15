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
layers <- list_layers(datasets)
#write.csv(layers, "Environmental_layers_metadata.csv")

# Create directory and the list of environmental variables you need
#dir.create("downloaded_predictors/Environmental_Seascape/", showWarnings = FALSE)
#Which variables?

layers <- c("MS_bathy_5m","MS_biogeo05_dist_shore_5m",)

#sstmean<-("BO_sstmean")
# Download marine layers to directory. 

bathy <- sdmpredictors::load_layers(layers, datadir = "downloaded_predictors/CMEMs/August2019/USA-Coast/", rasterstack = TRUE)

#Load env layers, you have to unzip the files 

#Work with netCDF layers
#https://rpubs.com/boyerag/297592
nc_data <- nc_open('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/depth/nc/cmems_mod_glo_phy_my_0.083_P1M-m_1697395376722.nc')
{
  sink('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/depth/nc/cmems_mod_glo_phy_my_0.083_P1M-m_1697395376722_metadata.txt')
  print(nc_data)
  sink()
}


####envCov_USA_surface_025_082019
bathy_025<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/3_biooracle_0-025/bathy.tif')
dist_shore_025<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/3_biooracle_0-025/dist_shore.tif')
slope_025<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/3_biooracle_0-025/slope.tif')

#surface_025_082019
chl_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/chl.tif')
ESV_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/ESV.tif')
epi_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/epi.tif')
Fe_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/Fe.tif')
lmeso_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/lmeso.tif')
no3_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/no3.tif')
npp_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/npp.tif')
nppv_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/nppv.tif')
NW_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/NW.tif')
NWV_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/NWV.tif')
o2_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/o2.tif')
OML_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/OML.tif')
pelagicL_depth_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/pelagicL_depth.tif')
phyc_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/phyc.tif')
po4_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/po4.tif')
si_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/si.tif')
spco2_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/spco2.tif')
SSH_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/SSH.tif')
SWS_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/SWS.tif')
SWT_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/SWT.tif')
umeso_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/umeso.tif')
zeu_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/zeu.tif')
zooc_surface_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/surface/tif/zooc.tif')

lat_025 <- lon_025 <- zooc_surface_025_082019
xy_025 <- coordinates(zooc_surface_025_082019)
lon_025[] <- xy_025[, 1]
lat_025[] <- xy_025[, 2]
plot(lat_025)

envCov_USA_surface_025_082019<-raster::brick(bathy_025,slope_025,dist_shore_025,epi_surface_025_082019,
                                             chl_surface_025_082019,ESV_surface_025_082019,Fe_surface_025_082019,
                                             lmeso_surface_025_082019,no3_surface_025_082019,npp_surface_025_082019,
                                             nppv_surface_025_082019,NW_surface_025_082019,NWV_surface_025_082019,
                                            o2_surface_025_082019,OML_surface_025_082019,pelagicL_depth_surface_025_082019,
                                            phyc_surface_025_082019,po4_surface_025_082019,si_surface_025_082019,
                                            spco2_surface_025_082019,SSH_surface_025_082019,SWS_surface_025_082019,
                                            SWT_surface_025_082019,umeso_surface_025_082019,zeu_surface_025_082019,
                                            zooc_surface_025_082019, lat_025, lon_025)

names(envCov_USA_surface_025_082019)
names(envCov_USA_surface_025_082019)[4]<-"epi"
names(envCov_USA_surface_025_082019)[8]<-"lmeso"
names(envCov_USA_surface_025_082019)[10]<-"npp"
names(envCov_USA_surface_025_082019)[16]<-"pelagicL_depth"
names(envCov_USA_surface_025_082019)[24]<-"umeso"
names(envCov_USA_surface_025_082019)[25]<-"zeu"
names(envCov_USA_surface_025_082019)[26]<-"zooc"
names(envCov_USA_surface_025_082019)[27]<-"lat"
names(envCov_USA_surface_025_082019)[28]<-"lon"

plot(envCov_USA_surface_025_082019)

#depth_025_082019
chl_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/chl.tif')
ESV_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/ESV.tif')
Fe_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/Fe.tif')
no3_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/no3.tif')
nppv_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/nppv.tif')
NWV_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/NWV.tif')
o2_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/o2.tif')
OML_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/OML.tif')
phyc_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/phyc.tif')
po4_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/po4.tif')
si_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/si.tif')
spco2_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/spco2.tif')
SSH_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/SSH.tif')
SWS_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/SWS.tif')
SWT_depth_025_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/1_August/depth/tif/SWT.tif')

envCov_USA_depth_025_082019<-raster::brick(bathy_025,slope_025,dist_shore_025,chl_depth_025_082019,ESV_depth_025_082019,
                                           Fe_depth_025_082019,no3_depth_025_082019,nppv_depth_025_082019,NWV_depth_025_082019,
                                          o2_depth_025_082019,OML_depth_025_082019,phyc_depth_025_082019,po4_depth_025_082019,
                                           si_depth_025_082019,spco2_depth_025_082019,SSH_depth_025_082019,SWS_depth_025_082019,
                                           SWT_depth_025_082019, lat_025, lon_025)
names(envCov_USA_depth_025_082019)
names(envCov_USA_depth_025_082019)[19]<-"lat"
names(envCov_USA_depth_025_082019)[20]<-"lon"

plot(envCov_USA_depth_025_082019)

#surface_025_022019
chl_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/chl.tif')
ESV_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/ESV.tif')
epi_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/epi.tif')
Fe_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/Fe.tif')
lmeso_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/lmeso.tif')
no3_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/no3.tif')
npp_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/npp.tif')
nppv_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/nppv.tif')
NW_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/NW.tif')
NWV_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/NWV.tif')
o2_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/o2.tif')
OML_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/OML.tif')
pelagicL_depth_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/pelagicL_depth.tif')
phyc_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/phyc.tif')
po4_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/po4.tif')
si_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/si.tif')
spco2_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/spco2.tif')
SSH_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/SSH.tif')
SWS_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/SWS.tif')
SWT_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/SWT.tif')
umeso_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/umeso.tif')
zeu_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/zeu.tif')
zooc_surface_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/surface/tif/zooc.tif')

envCov_USA_surface_025_022019<-raster::brick(bathy_025,slope_025,dist_shore_025,epi_surface_025_022019,chl_surface_025_022019,ESV_surface_025_022019,Fe_surface_025_022019,
                                             lmeso_surface_025_022019,no3_surface_025_022019,npp_surface_025_022019,nppv_surface_025_022019,NW_surface_025_022019,NWV_surface_025_022019,
                                             o2_surface_025_022019,OML_surface_025_022019,pelagicL_depth_surface_025_022019,phyc_surface_025_022019,po4_surface_025_022019,si_surface_025_022019,
                                             spco2_surface_025_022019,SSH_surface_025_022019,SWS_surface_025_022019,SWT_surface_025_022019,umeso_surface_025_022019,zeu_surface_025_022019,
                                             zooc_surface_025_022019,lat_025, lon_025)
names(envCov_USA_surface_025_022019)
names(envCov_USA_surface_025_022019)[4]<-"epi"
names(envCov_USA_surface_025_022019)[8]<-"lmeso"
names(envCov_USA_surface_025_022019)[10]<-"npp"
names(envCov_USA_surface_025_022019)[16]<-"pelagicL_depth"
names(envCov_USA_surface_025_022019)[24]<-"umeso"
names(envCov_USA_surface_025_022019)[25]<-"zeu"
names(envCov_USA_surface_025_022019)[26]<-"zooc"
names(envCov_USA_surface_025_022019)[27]<-"lat"
names(envCov_USA_surface_025_022019)[28]<-"lon"

plot(envCov_USA_surface_025_022019)

#depth_025_022019
chl_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/chl.tif')
ESV_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/ESV.tif')
Fe_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/Fe.tif')
no3_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/no3.tif')
nppv_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/nppv.tif')
NWV_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/NWV.tif')
o2_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/o2.tif')
OML_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/OML.tif')
phyc_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/phyc.tif')
po4_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/po4.tif')
si_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/si.tif')
spco2_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/spco2.tif')
SSH_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/SSH.tif')
SWS_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/SWS.tif')
SWT_depth_025_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-25/2_February/depth/tif/SWT.tif')

envCov_USA_depth_025_022019<-raster::brick(bathy_025,slope_025,dist_shore_025,chl_depth_025_022019,ESV_depth_025_022019,
                                           Fe_depth_025_022019,no3_depth_025_022019,nppv_depth_025_022019,NWV_depth_025_022019,
                                           o2_depth_025_022019,OML_depth_025_022019,phyc_depth_025_022019,po4_depth_025_022019,
                                           si_depth_025_022019,spco2_depth_025_022019,SSH_depth_025_022019,SWS_depth_025_022019,
                                           SWT_depth_025_022019, lat_025, lon_025)

names(envCov_USA_depth_025_022019)
names(envCov_USA_depth_025_022019)[19]<-"lat"
names(envCov_USA_depth_025_022019)[20]<-"lon"

plot(envCov_USA_depth_025_022019)

####envCov_USA_surface_083_082019

bathy_083<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/3_biooracle_0-0083/bathy.tif')
dist_shore_083<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/3_biooracle_0-0083/dist_shore.tif')
slope_083<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/3_biooracle_0-0083/slope.tif')

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
SSH_surface_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/SSH.tif')
SWS_surface_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/SWS.tif')
SWT_surface_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/surface/tif/SWT.tif')

lat_083 <- lon_083 <- SWT_surface_083_082019
xy_083 <- coordinates(SWT_surface_083_082019)
lon_083[] <- xy_083[, 1]
lat_083[] <- xy_083[, 2]
plot(lat_083)

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

#depth_083_082019
ESV_depth_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/depth/tif/ESV.tif')
NWV_depth_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/depth/tif/NWV.tif')
OML_depth_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/depth/tif/OML.tif')
SSH_depth_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/depth/tif/SSH.tif')
SWS_depth_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/depth/tif/SWS.tif')
SWT_depth_083_082019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/1_August/depth/tif/SWT.tif')

envCov_USA_depth_083_082019<-raster::brick(bathy_083,slope_083,dist_shore_083,ESV_depth_083_082019,
                                           NWV_depth_083_082019,OML_depth_083_082019,SSH_depth_083_082019,SWS_depth_083_082019,
                                           SWT_depth_083_082019,lon_083,lat_083)
names(envCov_USA_depth_083_082019)
names(envCov_USA_depth_083_082019)[4]<-"ESV"
names(envCov_USA_depth_083_082019)[5]<-"NWV"
names(envCov_USA_depth_083_082019)[8]<-"SWS"
names(envCov_USA_depth_083_082019)[9]<-"SWT"
names(envCov_USA_depth_083_082019)[10]<-"lon"
names(envCov_USA_depth_083_082019)[11]<-"lat"
plot(envCov_USA_depth_083_082019)

#surface_083_022019
ESV_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/surface/tif/ESV.tif')
epi_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/surface/tif/epi.tif')
lmeso_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/surface/tif/lmeso.tif')
npp_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/surface/tif/npp.tif')
NWV_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/surface/tif/NWV.tif')
OML_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/surface/tif/OML.tif')
SSH_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/surface/tif/SSH.tif')
SWS_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/surface/tif/SWS.tif')
SWT_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/surface/tif/SWT.tif')
umeso_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/surface/tif/umeso.tif')
zeu_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/surface/tif/zeu.tif')
zooc_surface_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/surface/tif/zooc.tif')

envCov_USA_surface_083_022019<-raster::brick(bathy_083,slope_083,dist_shore_083,epi_surface_083_022019,ESV_surface_083_022019,
                                             lmeso_surface_083_022019,npp_surface_083_022019,NWV_surface_083_022019,
                                             OML_surface_083_022019,SSH_surface_083_022019,SWS_surface_083_022019,
                                             umeso_surface_083_022019,zeu_surface_083_022019,
                                             zooc_surface_083_022019,SWT_surface_083_022019,lon_083,lat_083)
names(envCov_USA_surface_083_022019)
names(envCov_USA_surface_083_022019)[4]<-"epi"
names(envCov_USA_surface_083_022019)[6]<-"lmeso"
names(envCov_USA_surface_083_022019)[7]<-"npp"
names(envCov_USA_surface_083_022019)[12]<-"umeso"
names(envCov_USA_surface_083_022019)[13]<-"zeu"
names(envCov_USA_surface_083_022019)[14]<-"zooc"
names(envCov_USA_surface_083_022019)[15]<-"SWT"
names(envCov_USA_surface_083_022019)[16]<-"lon"
names(envCov_USA_surface_083_022019)[17]<-"lat"
plot(envCov_USA_surface_083_022019)

#depth_083_022019
ESV_depth_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/depth/tif/ESV.tif')
NWV_depth_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/depth/tif/NWV.tif')
OML_depth_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/depth/tif/OML.tif')
SSH_depth_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/depth/tif/SSH.tif')
SWS_depth_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/depth/tif/SWS.tif')
SWT_depth_083_022019<-raster('./downloaded_predictors/CMEMs/USA-Coast/0-0083/2_February/depth/tif/SWT.tif')

envCov_USA_depth_083_022019<-raster::brick(bathy_083,slope_083,dist_shore_083,ESV_depth_083_022019,
                                           NWV_depth_083_022019,OML_depth_083_022019,SSH_depth_083_022019,SWS_depth_083_022019,
                                           SWT_depth_083_022019,lon_083,lat_083)

names(envCov_USA_depth_083_022019)
names(envCov_USA_depth_083_022019)[4]<-"ESV"
names(envCov_USA_depth_083_022019)[5]<-"NWV"
names(envCov_USA_depth_083_022019)[8]<-"SWS"
names(envCov_USA_depth_083_022019)[9]<-"SWT"
names(envCov_USA_depth_083_022019)[10]<-"lon"
names(envCov_USA_depth_083_022019)[11]<-"lat"
plot(envCov_USA_depth_083_022019)


#save as dataframe

envCov_USA_surface_025_082019_df <- as.data.frame(envCov_USA_surface_025_082019, xy = TRUE)
envCov_USA_depth_025_082019_df <- as.data.frame(envCov_USA_depth_025_082019, xy = TRUE)
envCov_USA_surface_025_022019_df <- as.data.frame(envCov_USA_surface_025_022019, xy = TRUE)
envCov_USA_depth_025_022019_df <- as.data.frame(envCov_USA_depth_025_022019, xy = TRUE)

envCov_USA_surface_083_082019_df <- as.data.frame(envCov_USA_surface_083_082019, xy = TRUE)
envCov_USA_depth_083_082019_df <- as.data.frame(envCov_USA_depth_083_082019, xy = TRUE)
envCov_USA_surface_083_022019_df <- as.data.frame(envCov_USA_surface_083_022019, xy = TRUE)
envCov_USA_depth_083_022019_df <- as.data.frame(envCov_USA_depth_083_022019, xy = TRUE)

# Extent of area of interest WA cost
latmax <- 49.20833
latmin <- 45.70833
lonmax <- -123.0417
lonmin <- -127.2083

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

myshp <- readOGR("./Interpolations/Sampling_polygon.shp")

ESV_surface_083_082019_polygon<-raster::mask(envCov_WA_surface_025_082019, myshp)
ESV_surface_083_082019_polygon_WA<- raster::crop(ESV_surface_083_082019_polygon, extent(bb))
plot(ESV_surface_083_082019_polygon_WA)

#Save WA polygon
envCov_WA_surface_025_082019 <- raster::crop(envCov_USA_surface_025_082019, extent(bb))
envCov_WA_depth_025_082019 <- raster::crop(envCov_USA_depth_025_082019, raster::extent(lonmin, lonmax, latmin, latmax))
envCov_WA_surface_025_022019 <- raster::crop(envCov_USA_surface_025_022019, raster::extent(lonmin, lonmax, latmin, latmax))
envCov_WA_depth_025_022019 <- raster::crop(envCov_USA_depth_025_022019, raster::extent(lonmin, lonmax, latmin, latmax))

envCov_WA_surface_083_082019 <- raster::crop(envCov_USA_surface_083_082019, raster::extent(lonmin, lonmax, latmin, latmax))
envCov_WA_depth_083_082019 <- raster::crop(envCov_USA_depth_083_082019, raster::extent(lonmin, lonmax, latmin, latmax))
envCov_WA_surface_083_022019 <- raster::crop(envCov_USA_surface_083_022019, raster::extent(lonmin, lonmax, latmin, latmax))
envCov_WA_depth_083_022019 <- raster::crop(envCov_USA_depth_083_022019, raster::extent(lonmin, lonmax, latmin, latmax))

envCov_WA_surface_025_082019_df <- as.data.frame(envCov_WA_surface_025_082019, xy = TRUE)
envCov_WA_depth_025_082019_df <- as.data.frame(envCov_WA_depth_025_082019, xy = TRUE)
envCov_WA_surface_025_022019_df <- as.data.frame(envCov_WA_surface_025_022019, xy = TRUE)
envCov_WA_depth_025_022019_df <- as.data.frame(envCov_WA_depth_025_022019, xy = TRUE)

envCov_WA_surface_083_082019_df <- as.data.frame(envCov_WA_surface_083_082019, xy = TRUE)
envCov_WA_depth_083_082019_df <- as.data.frame(envCov_WA_depth_083_082019, xy = TRUE)
envCov_WA_surface_083_022019_df <- as.data.frame(envCov_WA_surface_083_022019, xy = TRUE)
envCov_WA_depth_083_022019_df <- as.data.frame(envCov_WA_depth_083_022019, xy = TRUE)

#Save samplng polygon
envCov_spol_surface_025_082019 <- raster::mask(envCov_WA_surface_025_082019, myshp)
envCov_spol_depth_025_082019 <- raster::mask(envCov_WA_depth_025_082019, myshp)
envCov_spol_surface_025_022019 <- raster::mask(envCov_WA_surface_025_022019, myshp)
envCov_spol_depth_025_022019 <- raster::mask(envCov_WA_depth_025_022019, myshp)

envCov_spol_surface_083_082019 <- raster::mask(envCov_WA_surface_083_082019, myshp)
envCov_spol_depth_083_082019 <- raster::mask(envCov_WA_depth_083_082019, myshp)
envCov_spol_surface_083_022019 <- raster::mask(envCov_WA_surface_083_022019, myshp)
envCov_spol_depth_083_022019 <- raster::mask(envCov_WA_depth_083_022019, myshp)

envCov_WA_surface_025_082019_df <- as.data.frame(envCov_WA_surface_025_082019, xy = TRUE)
envCov_WA_depth_025_082019_df <- as.data.frame(envCov_WA_depth_025_082019, xy = TRUE)
envCov_WA_surface_025_022019_df <- as.data.frame(envCov_WA_surface_025_022019, xy = TRUE)
envCov_WA_depth_025_022019_df <- as.data.frame(envCov_WA_depth_025_022019, xy = TRUE)

envCov_WA_surface_083_082019_df <- as.data.frame(envCov_WA_surface_083_082019, xy = TRUE)
envCov_WA_depth_083_082019_df <- as.data.frame(envCov_WA_depth_083_082019, xy = TRUE)
envCov_WA_surface_083_022019_df <- as.data.frame(envCov_WA_surface_083_022019, xy = TRUE)
envCov_WA_depth_083_022019_df <- as.data.frame(envCov_WA_depth_083_022019, xy = TRUE)

#Save stacks
writeRaster(envCov_USA_surface_025_082019,"./downloaded_predictors/CMEMs/custom_stacks//USA-Coast/0-25/envCov_USA_surface_025_082019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)
writeRaster(envCov_USA_depth_025_082019,"./downloaded_predictors/CMEMs/custom_stacks/USA-Coast/0-25/envCov_USA_depth_025_082019.tif",options="INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(envCov_USA_surface_025_022019,"./downloaded_predictors/CMEMs/custom_stacks/USA-Coast/0-25/envCov_USA_surface_025_022019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)
writeRaster(envCov_USA_depth_025_022019,"./downloaded_predictors/CMEMs/custom_stacks/USA-Coast/0-25/envCov_USA_depth_025_022019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)

writeRaster(envCov_USA_surface_083_082019,"./downloaded_predictors/CMEMs/custom_stacks/USA-Coast/0-083/envCov_USA_surface_083_082019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)
writeRaster(envCov_USA_depth_083_082019,"./downloaded_predictors/CMEMs/custom_stacks/USA-Coast/0-083/envCov_USA_depth_083_082019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)
writeRaster(envCov_USA_surface_083_022019,"./downloaded_predictors/CMEMs/custom_stacks/USA-Coast/0-083/envCov_USA_surface_083_022019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)
writeRaster(envCov_USA_depth_083_022019,"./downloaded_predictors/CMEMs/custom_stacks/USA-Coast/0-083/envCov_USA_depth_083_022019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)

#Save stacks
writeRaster(envCov_WA_surface_025_082019,"./downloaded_predictors/CMEMs/custom_stacks/WA/0-25/envCov_WA_surface_025_082019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)
writeRaster(envCov_WA_depth_025_082019,"./downloaded_predictors/CMEMs/custom_stacks/WA/0-25/envCov_WA_depth_025_082019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)
writeRaster(envCov_WA_surface_025_022019,"./downloaded_predictors/CMEMs/custom_stacks/WA/0-25/envCov_WA_surface_025_022019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)
writeRaster(envCov_WA_depth_025_022019,"./downloaded_predictors/CMEMs/custom_stacks/WA/0-25/envCov_WA_depth_025_022019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)

writeRaster(envCov_WA_surface_083_082019,"./downloaded_predictors/CMEMs/custom_stacks/WA/0-083/envCov_WA_surface_083_082019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)
writeRaster(envCov_WA_depth_083_082019,"./downloaded_predictors/CMEMs/custom_stacks/WA/0-083/envCov_WA_depth_083_082019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)
writeRaster(envCov_WA_surface_083_022019,"./downloaded_predictors/CMEMs/custom_stacks/WA/0-083/envCov_WA_surface_083_022019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)
writeRaster(envCov_WA_depth_083_022019,"./downloaded_predictors/CMEMs/custom_stacks/WA/0-083/envCov_WA_depth_083_022019.tif", options="INTERLEAVE=BAND",overwrite = TRUE)

#https://rpubs.com/boyerag/297592

(p <- ggplot() +
    geom_raster(data = envCov_WA_depth_083_082019_df, aes(x = x, y = y, fill = lat)) +
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


envCov_surface_025_df<-raster::extract(envCov_USA_surface_025_082019,coord,df=TRUE)
envCov_surface_083_df<-raster::extract(envCov_USA_surface_083_082019,coord,df=TRUE)

write.csv(envCov_surface_025_df, "./dataframes/envCov_surface_025_df.csv")
write.csv(envCov_surface_083_df, "./dataframes/envCov_surface_083_df.csv")

md.taxa.long.formated.env025<-read.csv("./dataframes/md_taxa_long_formated_env025.csv")
md.taxa.long.formated.env083<-read.csv("./dataframes/md_taxa_long_formated_env083.csv")

