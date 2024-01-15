

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

#### MODEL SELECTION -- Laob surface Laob.unique_083_F
Laob.surface.gam<-gam(Presence~s(lon,lat,bs="ds")+
                      # s(bathy,bs=	"ts",k=3) +
                       s(dist_shore,bs=	"ts",k=3) +
                      # s(OML,bs=	"ts",k=3) +
                      # s(npp,bs=	"ts",k=3) +
                      # s(ESV,bs=	"ts",k=3) +
                      # s(NWV,bs=	"ts",k=3) +
                      # s(pelagicL_depth,bs=	"ts",k=3) +
                      # s(SSH, bs="ts", k=3) +
                      # s(zeu,bs=	"ts",k=3) +
                        s(epi,bs=	"ts",k=3) +
                       # s(zooc,bs=	"ts",k=3) +
                        s(SWT,bs=	"ts",k=3),
                        data = Laob.unique_083_F, family = binomial, method="REML",na.action = "na.fail")

dd.dregde.fm2 <- dredge(Laob.surface.gam, rank = "AIC", extra="ICOMP", m.lim = c(2,5))

(dd.subset.fm2<-subset(dd.dregde.fm2, delta < 2))

par(mar = c(3,5,6,4))
plot(dd.subset.fm2, labAsExpr = TRUE)

(dd.subset.fm2[1])
confset.95p.fm2 <- get.models(dd.subset.fm2, cumsum(weight) <= .95)
summary(confset.95p.fm2[[1]])

#BEST MODEL
best.model.Laob.AIC <- gam(Presence~ s(dist_shore,bs=	"ts",k=3)+
                             s(SWT, bs = "ts",k=3),
                           data = Laob.unique_083_F, family =binomial , method="REML")

predictions =  predict(envCov_WA_surface_083_082019, model = best.model.Laob.AIC, type = "response")
predictions[is.na(predictions[])] <- 0 
spplot(predictions, colorkey = list(space = "left") ,scales = list(draw = TRUE))

k.check(best.model.Laob.AIC)
draw(best.model.Laob.AIC)
summary(best.model.Laob.AIC)
AIC(best.model.Laob.AIC)
auc(Laob.test.surface_083_R$Presence, predict(best.model.Laob.surface.AIC, type = "response"))

# gam_eval <- pa_evaluate(p = Laob.unique_083_F[Laob.unique_083_F$Presence == 1, ],
#                        a= Laob.unique_083_F[Laob.unique_083_F$Presence == 0, ],
#                        model = best.model.Laob.surface.AIC,
#                         type = "response")
writeRaster(predictions,"~/Maxent/GAM_predictions/Laob_GAM_distshore_SWT.asc",overwrite=TRUE)
write.csv(Laob.unique_083_F,"~/Maxent/Laob_presence.csv")

##############
#GBIF data for WA
# Set the seed for the random-number generator to ensure results are similar
set.seed(20210707)

Laob_WA_uniq_PA<-read.csv("~/Maxent/occurrences/Laob/presence_absence/Laob_uniq.csv")
coord <- Laob_WA_uniq_PA[,c(2,3)]%>% 
  rename(lon=longitude)%>% 
  rename(lat=latitude)# Order must be lon lat
enviro_df<-raster::extract(envCov_WA_surface_083_082019,coord,df=TRUE) 
Laob_WA_uniq_PA_env<-bind_cols(enviro_df,Laob_WA_uniq_PA) %>% 
  dplyr::select(-c(umeso, lmeso)) %>% 
  #na.omit() %>% 
  rename(lonB=lon)%>% 
  rename(latB=lat) %>% 
  rename(lon=longitude)%>% 
  rename(lat=latitude)
write.csv(Laob_WA_uniq_PA_env,"~/Maxent/occurrences/Laob/presence_absence/Laob_WA_uniq_PA_env.csv")


Laob_WA_uniq_PA_B<-read.csv("~/Maxent/occurrences/Laob/presence_absence/Laob_uniq_B.csv")
coord <- Laob_WA_uniq_PA_B[,c(2,3)]%>% 
  rename(lon=longitude)%>% 
  rename(lat=latitude)# Order must be lon lat
enviro_df<-raster::extract(envCov_WA_surface_083_082019,coord,df=TRUE) 
Laob_WA_uniq_PA_B_env<-bind_cols(enviro_df,Laob_WA_uniq_PA_B) %>% 
  dplyr::select(-c(umeso, lmeso)) %>% 
  na.omit() %>% 
  rename(lonB=lon)%>% 
  rename(latB=lat) %>% 
  rename(lon=longitude)%>% 
  rename(lat=latitude)
write.csv(Laob_WA_uniq_PA_B_env,"~/Maxent/occurrences/Laob/presence_absence/Laob_WA_uniq_PA_B_env.csv")


Laob_WA_GBIF_25_PA<-read.csv("~/Maxent/occurrences/Laob/presence_absence/Laob_WA_GBIF_25_PA.csv")
coord <- Laob_WA_GBIF_25_PA[,c(2,3)]%>% 
  rename(lon=longitude)%>% 
  rename(lat=latitude)# Order must be lon lat
enviro_df<-raster::extract(envCov_WA_surface_083_082019,coord,df=TRUE) 
Laob_WA_GBIF_25_PA_env<-bind_cols(enviro_df,Laob_WA_GBIF_25_PA) %>% 
  dplyr::select(-c(umeso, lmeso)) %>% 
  na.omit() %>% 
  rename(lonB=lon)%>% 
  rename(latB=lat) %>% 
  rename(lon=longitude)%>% 
  rename(lat=latitude)
write.csv(Laob_WA_GBIF_25_PA_env,"~/Maxent/occurrences/Laob/presence_absence/Laob_WA_GBIF_25_PA_env.csv")

Laob_WA_GBIF_25_PA_B<-read.csv("~/Maxent/occurrences/Laob/presence_absence/Laob_WA_GBIF_25_PA_B.csv")
coord <- Laob_WA_GBIF_25_PA_B[,c(2,3)]%>% 
  rename(lon=longitude)%>% 
  rename(lat=latitude)# Order must be lon lat
enviro_df<-raster::extract(envCov_WA_surface_083_082019,coord,df=TRUE) 
Laob_WA_GBIF_25_PA_B_env<-bind_cols(enviro_df,Laob_WA_GBIF_25_PA_B) %>% 
  dplyr::select(-c(umeso, lmeso)) %>% 
  na.omit() %>% 
  rename(lonB=lon)%>% 
  rename(latB=lat) %>% 
  rename(lon=longitude)%>% 
  rename(lat=latitude)
write.csv(Laob_WA_GBIF_25_PA_B_env,"~/Maxent/occurrences/Laob/presence_absence/Laob_WA_GBIF_25_PA_B_env.csv")


Laob.GBIF.WA.gam<-gam(Presence~ s(lon,lat,bs="ds")+
                         s(bathy) +
                         s(dist_shore,bs=	"ts",k=3) +
                        s(slope)+
                        # s(OML,bs=	"ts") +
                         s(npp) +
                        # s(ESV,bs=	"ts") +
                        # s(NWV,bs=	"ts") +
                        # s(pelagicL_depth) +
                        # s(SSH, bs="ts") +
                        # s(zeu,bs=	"ts") +
                      #  s(epi,bs=	"ts") +
                        # s(zooc,bs=	"ts") +
                        s(SWT),
                      data = Laob_WA_GBIF_100_PA_env, family = binomial, method="REML",na.action = "na.fail")
summary(Laob.GBIF.WA.gam)
dd.dregde <- dredge(Laob.GBIF.WA.gam, rank = "AIC", extra="ICOMP", m.lim = c(2,4))
(dd.subset.<-subset(dd.dregde, delta < 2))

par(mar = c(3,5,6,4))
plot(dd.subset., labAsExpr = TRUE)

(dd.subset.[1])
confset.95p <- get.models(dd.subset., cumsum(weight) <= .95)
summary(confset.95p[[1]])

#BEST MODEL

Laob_WA_uniq_PA_env
Laob_WA_uniq_PA_B_env
Laob_WA_GBIF_25_PA_env
Laob_WA_GBIF_25_PA_B_env
Laob_WA_GBIF_50_PA_env

best.model.Laob.GBIF.AIC <- gam(Presence~  s(lon, lat, bs = "ds") + s(npp) + s(slope),
                           data = Laob_WA_GBIF_100_PA_env, family =binomial , method="REML")

predictions =  predict(envCov_WA_surface_083_082019, model = best.model.Laob.GBIF.AIC, type = "response")
predictions[is.na(predictions[])] <- 0 
spplot(predictions, colorkey = list(space = "left") ,scales = list(draw = TRUE))


k.check(best.model.Laob.GBIF.AIC)
draw(best.model.Laob.GBIF.AIC)
plot(best.model.Laob.GBIF.AIC)
summary(best.model.Laob.GBIF.AIC)
AIC(best.model.Laob.GBIF.AIC)
auc(best.model.Laob.GBIF.AIC$Presence, predict(best.model.Laob.GBIF.AIC, type = "response"))

(gam_eval <- pa_evaluate(p = Laob_WA_GBIF_100_PA_env[Laob_WA_GBIF_100_PA_env$Presence == 1, ],
                        a= Laob_WA_GBIF_100_PA_env[Laob_WA_GBIF_100_PA_env$Presence == 0, ],
                        model = best.model.Laob.GBIF.AIC,
                        type = "response"))

writeRaster(predictions,"~/Maxent/GAM_predictions/Laob_GAM_distshore_SWT.asc",overwrite=TRUE)
write.csv(Laob.unique_083_F,"~/Maxent/Laob_presence.csv")
