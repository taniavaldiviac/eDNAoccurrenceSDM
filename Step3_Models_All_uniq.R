list.of.packages=c("readr","dplyr","ggplot2", "here", "raster", "mgcv","gratia", "librarian", "lme4", "maps","sf", "car", "usdm", "mgcv.helper","dplyr", 
                   "dsm", "Distance", "knitr", "captioner", "ggplot2", "rgdal","maptools", "tweedie","stringr","fuzzySim")
detach("plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = T)
library(librarian)
librarian::shelf(list.of.packages)


md.taxa.long.formated.env083.uniq <- md.taxa.long.formated.env083 %>% 
  separate(sampleID_station, into = c("sampleID", "station"), sep = "_", extra = "merge", remove = FALSE) %>% 
  group_by(station, species) %>%
  mutate(presence_uniq = sum(Presence, na.rm = TRUE)) %>%
  slice(1) %>% 
  mutate(presence_uniq = ifelse(presence_uniq >= 1, 1, 0)) %>%
  relocate(presence_uniq, .before = Presence) %>% 
  ungroup() 


md.taxa.long.formated.env083.uniq.envS<-as.data.frame(md.taxa.long.formated.env083.uniq[1:40])

#Extract env_info per coordinate
#coordinates(coord) <-c("lon", "lat")

coord <- md.taxa.long.formated.env083.uniq.envS[,c(26,25)] # Order must be lon lat
coord.dec<-SpatialPoints(coord, proj4string=CRS("+proj=longlat"))
coord.dec.df<-as.data.frame(coord)
coords <- st_as_sf(coord, coords = c("lon", "lat"))
sites.names<-md.taxa.long.formated.env083.uniq$sampleID_station

envCov_uniq_083_df<-raster::extract(envCov_USA_surface_083_082019,coord,df=TRUE)
envCov_uniq_083_df<-envCov_uniq_083_df[1:17]

md.taxa.long.formated.env083.uniq.envS<-bind_cols(md.taxa.long.formated.env083.uniq.envS,envCov_uniq_083_df)
  
write.csv(Grgr.unique_083_F, "~/Maxent/occurrences/Grgr_eDNA.csv")
Laob.unique_083_F <- md.taxa.long.formated.env083.uniq.envS  %>% 
  filter(str_detect(species, "Lagenorhynchus obliquidens")) 

Meno.unique_083_F <- md.taxa.long.formated.env083.uniq.envS  %>% 
  filter(str_detect(species, "Megaptera novaeangliae")) 

Grgr.unique_083_F <- md.taxa.long.formated.env083.uniq.envS  %>% 
  filter(str_detect(species, "Grampus griseus")) 

Libo.unique_083_F <- md.taxa.long.formated.env083.uniq.envS  %>% 
  filter(str_detect(species, "Lissodelphis borealis")) 

Laob.unique_083_F <- Laob.unique_083_F  %>% 
  dplyr::select(c(40,26,25,42:57))

Meno.unique_083_F <- Meno.unique_083_F  %>% 
  dplyr::select(c(40,26,25,42:57)) 

Grgr.unique_083_F <- Grgr.unique_083_F  %>% 
  dplyr::select(c(40,26,25,42:57)) 

Libo.unique_083_F <- Libo.unique_083_F  %>% 
  dplyr::select(c(40,26,25,42:57)) 

##Surface Presence Map
color_mapping <- c("0" = "white", "1" = "#ffce00", "2" = "#ff9a00", "3" = "#ff5a00", "4"="#ff0000", "5"="#940000")
Laob <- ggplot() +
  geom_tile(data = envCov_WA_surface_083_082019_df,
            aes(x = x, y = y, fill = bathy)) +
  geom_point(data = Laob.unique_083_F,
             aes(x = lon, y = lat, color = as.factor(Presence)),size=3) +
  scale_fill_continuous(type = "gradient") +
  scale_color_manual(values = color_mapping) +  
  labs(title = "Pacific white-sided dolphin eDNA detections")

Laob + theme(legend.position="right", legend.box = "vertical",plot.title = element_text(size = 12, face = "bold"),
                legend.title=element_text(size=7), 
                legend.text=element_text(size=7),
                legend.spacing.y = unit(0.5, 'cm'))

Meno <- ggplot() +
  geom_tile(data = envCov_WA_surface_083_082019_df,
            aes(x = x, y = y, fill = bathy)) +
  geom_point(data = Meno.unique_083_F,
             aes(x = lon, y = lat, color = as.factor(Presence)),size=4) +
  scale_fill_continuous(type = "gradient") +
  scale_color_manual(values = color_mapping) +  
  labs(title = "Humpback whale eDNA detections")

Meno + theme(legend.position="right", legend.box = "vertical",plot.title = element_text(size = 12, face = "bold"),
             legend.title=element_text(size=7), 
             legend.text=element_text(size=7),
             legend.spacing.y = unit(0.5, 'cm'))

Grgr <- ggplot() +
  geom_tile(data = envCov_WA_surface_083_082019_df,
            aes(x = x, y = y, fill = bathy)) +
  geom_point(data = Grgr.unique_083_F,
             aes(x = lon, y = lat, color = as.factor(Presence)),size=4) +
  scale_fill_continuous(type = "gradient") +
  scale_color_manual(values = color_mapping) +  
  labs(title = "Risso's dolphin eDNA detections")

Grgr + theme(legend.position="right", legend.box = "vertical",plot.title = element_text(size = 12, face = "bold"),
             legend.title=element_text(size=7), 
             legend.text=element_text(size=7),
             legend.spacing.y = unit(0.5, 'cm'))

### I estimated the distance to the 200m isobath in Qgis so load the df again

Laob.unique_083_dist<-read.csv("~/Maxent/occurrences/Laob.unique_083_dist.csv")


# ENVIRONMENTAL DATA

data<-Laob.unique_083_dist[,c(1:14,17:20)]

FDR(data=data,sp.cols = 1,var.cols = 2:ncol(data),family='binomial')

corSelect(data,sp.cols = 1,var.cols = 2:ncol(data), cor.thresh = 0.7)

# Multicolinealidad
multicol(data)

# Variable correlation Heatmap
Z <- as.data.frame(data)
Z<-na.omit(Z)
cor_matrix <- cor(Z)
library(reshape2)
cor_matrix_melted <- melt(cor_matrix)
ggplot(data = cor_matrix_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "light blue", high = "red", na.value = "black") +
  geom_text(aes(label = round(value, 1)), vjust = 1, size = 3) +  # Smaller font size (adjust size as needed)
  labs(title = "Correlation Heatmap Environmental data") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplots
# Create a grid of boxplots for multiple variables
response_variables <- c("lat", "lon", "bathy", "dist_shore", "slope","ESV", 
                        "npp", "NWV", "pelagicL_depth", "OML","SSH","SWS",
                        "epi","lmeso","umeso","zooc","zeu","SWT","dist_200")

# Define a function to create individual plots
create_boxplot <- function(variable) {
  ggplot(Laob.unique_083_dist) +
    aes(x = as.factor(Presence), y = .data[[variable]]) +
    geom_boxplot(fill = "#4682B4") +
    labs(y = variable, x = "eDNA presence") +
    theme_bw()+
    theme(axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7)
          )
}

# Create a list of individual plots
plots_list <- lapply(response_variables, create_boxplot)

# Combine the list of plots into a grid
library(grid)
library(gridExtra)

grid.text("Pacific white-sided dolphin", x = 0.1, y = 0.95, gp = gpar(fontsize = 16, fontface = "bold"))
grid.text("eDNA detections", x = 0.1, y = 0.92, gp = gpar(fontsize = 14))
grid.arrange(grobs = plots_list, ncol = 4)

view(formula_str)

#### MODEL SELECTION -- 

# FULL MODEL Lagenorhynchus obliquidens (Laob.unique_083_F)

set.seed(123)
Full.gam<-gam(Presence~s(lon,bs="ts")+
                s(lat,bs="ts")+
                s(bathy,bs=	"ts") +
                s(slope,bs=	"ts") +
                s(dist_shore,bs=	"ts") +
                s(SSH, bs="ts") +
                s(SWT,bs=	"ts"),
              data = Laob.unique_083_F, family = binomial, method="REML",na.action = "na.fail")

dd.dregde.fm2 <- dredge(Full.gam, rank = "AIC", extra="ICOMP", m.lim = c(2,3),
                        subset = c(!("s(SWT, bs = \"ts\")" & "s(SSH, bs = \"ts\")") &   
                                     !("s(SWT, bs = \"ts\")" & "s(dist_shore, bs = \"ts\")") &
                                     !("s(SSH, bs = \"ts\")" & "s(dist_shore, bs = \"ts\")")&
                                     !("s(bathy, bs = \"ts\")" & "s(dist_shore, bs = \"ts\")")))


(dd.subset.fm2<-subset(dd.dregde.fm2, delta < 2))

par(mar = c(3,5,6,4))
plot(dd.subset.fm2, labAsExpr = TRUE)

(dd.subset.fm2[1])
confset.95p.fm2 <- get.models(dd.subset.fm2, cumsum(weight) <= .95)
summary(confset.95p.fm2[[1]])

# BEST MODEL

best.model.Laob.AIC <- gam(Presence~ s(slope,bs="ts")+s(lon,bs="ts")+s(SWT,bs="ts"), data = Laob.unique_083_F, family =binomial , method="REML")
summary(best.model.Laob.AIC)
gam.check(best.model.Laob.AIC)
draw(best.model.Laob.AIC)
summary(best.model.Laob.AIC)
AIC(best.model.Laob.AIC)
auc(Laob.unique_083_F$Presence, predict(best.model.Laob.AIC, type = "response"))

#Predict_1 To predict to a raster stack
predictions =  predict(envCov_spol_surface_083_082019, model = best.model.Laob.AIC, type = "response")
predictions[is.na(predictions[])] <- 0
spplot(predictions, colorkey = list(space = "left") ,scales = list(draw = TRUE))

#Predict_2 To predict to a dataframe
Laob.predictions_df<-envCov_spol_surface_083_082019_df
Laob.predictions_df$prediction =  predict(best.model.Laob.AIC,newdata =envCov_spol_surface_083_082019_df, type = "response")

#dataframe to raster
Laob.predictions_r<- Laob.predictions_df[,c(1,2,21)] %>% 
  filter(!is.na(prediction))
dfr <- rasterFromXYZ(Laob.predictions_r) 
writeRaster(dfr,"~/Maxent/GAM_predictions/Laob_GAM_Jan17.asc",overwrite=TRUE)

#load USA shape
USA_sf<-st_read("/Users/taniavc/Library/CloudStorage/GoogleDrive-tania.valdiviac@gmail.com/My Drive/SIG/Layers/USA.shp")
USA_map <- st_as_sf(USA_sf)
wa_or_ca_sf <- USA_sf %>% filter(STATE_NAME %in% c("Washington","Oregon", "California"))
wa_or_ca_sf <- st_transform(wa_or_ca_sf, crs = st_crs(4326))
plot_coords <- st_bbox(wa_or_ca_sf)

#Plot map
map<- ggplot() +
    geom_tile(data = Laob.predictions_r,
            aes(x = x, y = y, fill = prediction)) +
  geom_point(data = Laob.unique_083_F,
             aes(x = lon, y = lat, color = as.factor(Presence)), size = 2) +
  geom_sf(data = wa_or_ca_sf, fill = "darkgray", color = "darkgray") +
  scale_fill_viridis() +
  scale_color_manual(values = color_mapping) +
  labs(title = "Pacific white-sided dolphin eDNA distribution") +
  coord_sf(xlim = c(-127, -123), 
           ylim = c(46.4, 48.5))

map <- map + labs(fill = "eDNA presence prob",color="Species presence")

map+theme(legend.position="right", legend.box = "vertical",plot.title = element_text(size = 12, face = "bold"),
                              legend.title=element_text(size=7), 
                              legend.text=element_text(size=7),
                              legend.spacing.y = unit(0.5, 'cm'),
          legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

