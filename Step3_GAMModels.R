###

#Step3: GAM models using eDNA-occurence data 

#Tania Valdivia Carrillo

###

list.of.packages=c("readr","dplyr","ggplot2", "here", "raster", "mgcv","gratia", "librarian", "lme4", "maps","sf", "car", "usdm", "mgcv.helper","dplyr", 
                   "dsm", "pROC","Distance", "knitr", "ggplot2", "rgdal","maptools", "tweedie","stringr","fuzzySim","MuMIn")
detach("plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = T)
library(librarian)
librarian::shelf(list.of.packages)

### SETWD
#Set the working directory to the main folder where this repository (eDNAoccurrenceSDM) is located on your disk.

md.taxa.long.formated.env083.uniq<-read.csv("./dataframes/md.taxa.long.formated.env083.uniq.csv")

Laob.unique_083_F_A <- md.taxa.long.formated.env083.uniq  %>% 
  filter(str_detect(species, "Lagenorhynchus obliquidens")) 

Meno.unique_083_F_A <- md.taxa.long.formated.env083.uniq  %>% 
  filter(str_detect(species, "Megaptera novaeangliae")) 

Grgr.unique_083_F_A <- md.taxa.long.formated.env083.uniq  %>% 
  filter(str_detect(species, "Grampus griseus")) 

#write.csv(binded, "./dataframes/binded_eDNA.csv")

Laob.unique_083_F <- Laob.unique_083_F_A  %>% 
  dplyr::select(c(40,26,25,41:56))

Meno.unique_083_F <- Meno.unique_083_F_A  %>% 
  dplyr::select(c(40,26,25,41:56)) 

Grgr.unique_083_F <- Grgr.unique_083_F_A  %>% 
  dplyr::select(c(40,26,25,41:56)) 

#### MODEL SELECTION 

# PACIFIC WHITE-SIDED DOLPHIN

set.seed(123)
Full.gam<-gam(Presence~s(lon,bs="ts")+
               s(lat,bs="ts")+
                s(bathy,bs=	"ts") +
                s(slope,bs=	"ts") +
                s(dist_shore,bs="ts")+
                s(SWT,bs=	"ts"),
              data = Laob.unique_083_F, family = binomial, method="REML",na.action = "na.fail")

dd.dregde.fm2 <- dredge(Full.gam, rank = "AIC", extra="ICOMP", m.lim = c(2,3),
                        subset = c(!("s(dist_shore, bs = \"ts\")" & "s(bathy, bs = \"ts\")")))

(dd.subset.fm2<-subset(dd.dregde.fm2, delta < 2))
dredge_results <- data.frame(dd.dregde.fm2)

plot(dd.subset.fm2, labAsExpr = TRUE)

(dd.subset.fm2[1])

confset.95p.fm2 <- get.models(dd.subset.fm2, cumsum(weight) <= .95)
summary(confset.95p.fm2[[1]])

write.csv(dredge_results, file = "../SupplementaryMaterial/Laob_GAMresults.csv", row.names = FALSE)

# BEST MODEL
set.seed(123)
best.model.Laob.AIC <- gam(Presence~ s(lon, bs = "ts") + s(dist_shore, bs = "ts"), 
                           data = Laob.unique_083_F, family =binomial, method="REML")
summary(best.model.Laob.AIC)
gam.check(best.model.Laob.AIC)
plot(best.model.Laob.AIC)

qqnorm(residuals(best.model.Laob.AIC))
qqline(residuals(best.model.Laob.AIC))

(draw<-draw(best.model.Laob.AIC, residuals=TRUE))
ggsave("./images/best.model.Laob.AIC.jpg", plot = draw, width = 7, height = 5, units = "in", dpi = 300)
AIC(best.model.Laob.AIC)
auc(Laob.unique_083_F$Presence, predict(best.model.Laob.AIC, type = "response"))

#Predict_1 To predict to a raster stack
predictions =  terra::predict(envCov_spol_surface_083_082019, model = best.model.Laob.AIC, type = "response")
predictions[is.na(predictions[])] <- 0
spplot(predictions, colorkey = list(space = "left") ,scales = list(draw = TRUE))

#Predict_2 To predict to a dataframe
Laob.predictions_df<-envCov_spol_surface_083_082019_df
Laob.predictions_df$prediction =  predict(best.model.Laob.AIC,newdata =envCov_spol_surface_083_082019_df, type = "response")

#dataframe to raster
Laob.predictions_r<- Laob.predictions_df[,c(1,2,21)] %>% 
  filter(!is.na(prediction))
dfr <- rasterFromXYZ(Laob.predictions_r) 
#dfr.disaggregate <- disaggregate(dfr, fact=10)
#res(dfr.disaggregate)

writeRaster(dfr,"~/Maxent/GAM_predictions/Laob_GAM_Jan17.asc",overwrite=TRUE)

##HUMPBACK WHALE

Full.gam<-gam(Presence~s(lon,bs="ts")+
                s(lat,bs="ts")+
                s(bathy,bs=	"ts") +
                s(slope,bs=	"ts") +
                s(dist_shore,bs=	"ts")+
                s(SWT,bs=	"ts"),
              data = Meno.unique_083_F, family = binomial, method="REML",na.action = "na.fail")

dd.dregde.fm2 <- dredge(Full.gam, rank = "AIC", extra="ICOMP", m.lim = c(2,3),
                        subset = c(!("s(dist_shore, bs = \"ts\")" & "s(bathy, bs = \"ts\")")))

(dd.subset.fm2<-subset(dd.dregde.fm2, delta < 2))
dredge_results <- data.frame(dd.dregde.fm2)

plot(dd.subset.fm2, labAsExpr = TRUE)

(dd.subset.fm2[1])

confset.95p.fm2 <- get.models(dd.subset.fm2, cumsum(weight) <= .95)
summary(confset.95p.fm2[[1]])

write.csv(dredge_results, file = "../SupplementaryMaterial/Meno_GAMresults.csv", row.names = FALSE)

#BEST MODEL
best.model.Meno.AIC <- gam(Presence~ s(bathy, bs = "ts",k=9) + s(SWT, bs = "ts"),
                           data = Meno.unique_083_F, family =binomial , method="REML")
summary(best.model.Meno.AIC)
gam.check(best.model.Meno.AIC)
(draw<-draw(best.model.Meno.AIC, residuals=TRUE))
ggsave("./images/best.model.Meno.AIC.jpg", plot = draw, width = 7, height = 5, units = "in", dpi = 300)
summary(best.model.Meno.AIC)
AIC(best.model.Meno.AIC)
auc(Meno.unique_083_F$Presence, predict(best.model.Meno.AIC, type = "response"))

#Predict_1 To predict to a raster stack
predictions =  terra::predict(envCov_spol_surface_083_082019, model = best.model.Meno.AIC, type = "response")
predictions[is.na(predictions[])] <- 0
spplot(predictions, colorkey = list(space = "left") ,scales = list(draw = TRUE))

#Predict_2 To predict to a dataframe
Meno.predictions_df<-envCov_spol_surface_083_082019_df
Meno.predictions_df$prediction =  predict(best.model.Meno.AIC,newdata =envCov_spol_surface_083_082019_df, type = "response")

#dataframe to raster
Meno.predictions_r<- Meno.predictions_df[,c(1,2,21)] %>% 
  filter(!is.na(prediction))
dfr <- rasterFromXYZ(Meno.predictions_r) 
writeRaster(dfr,"~/Maxent/GAM_predictions/Meno_GAM_Jan17.asc",overwrite=TRUE)

##RISSO'S DOLPHIN

Full.gam<-gam(Presence~s(lon,bs="ts")+
                s(lat,bs="ts")+
                s(bathy,bs=	"ts") +
                s(slope,bs=	"ts") +
                s(dist_shore,bs=	"ts") +
                s(SWT,bs=	"ts"),
              data = Grgr.unique_083_F, family = binomial, method="REML",na.action = "na.fail")

dd.dregde.fm2 <- dredge(Full.gam, rank = "AIC", extra="ICOMP", m.lim = c(2,3),
                        subset = c(!("s(dist_shore, bs = \"ts\")" & "s(bathy, bs = \"ts\")")))

(dd.subset.fm2<-subset(dd.dregde.fm2, delta < 2))
dredge_results <- data.frame(dd.dregde.fm2)

plot(dd.subset.fm2, labAsExpr = TRUE)

(dd.subset.fm2[1])

confset.95p.fm2 <- get.models(dd.subset.fm2, cumsum(weight) <= .95)
summary(confset.95p.fm2[[1]])

write.csv(dredge_results, file = "../SupplementaryMaterial/Grgr_GAMresults.csv", row.names = FALSE)

#BEST MODEL
best.model.Grgr.AIC <- gam(Presence~ s(lon, bs = "ts") + s(slope, bs = "ts"),
                           data = Grgr.unique_083_F, family =binomial , method="REML")
summary(best.model.Grgr.AIC)
gam.check(best.model.Grgr.AIC)
(draw<-draw(best.model.Grgr.AIC,residuals=TRUE))
ggsave("./images/best.model.Grgr.AIC.jpg", plot = draw, width = 7, height = 5, units = "in", dpi = 300)
summary(best.model.Grgr.AIC)
AIC(best.model.Grgr.AIC)
auc(Grgr.unique_083_F$Presence, predict(best.model.Grgr.AIC, type = "response"))

#Predict_1 To predict to a raster stack
predictions =  terra::predict(envCov_spol_surface_083_082019, model = best.model.Grgr.AIC, type = "response")
predictions[is.na(predictions[])] <- 0
spplot(predictions, colorkey = list(space = "left") ,scales = list(draw = TRUE))

#Predict_2 To predict to a dataframe
Grgr.predictions_df<-envCov_spol_surface_083_082019_df
Grgr.predictions_df$prediction =  predict(best.model.Grgr.AIC,newdata =envCov_spol_surface_083_082019_df, type = "response")

#dataframe to raster
Grgr.predictions_r<- Grgr.predictions_df[,c(1,2,21)] %>% 
  filter(!is.na(prediction))
dfr <- rasterFromXYZ(Grgr.predictions_r) 
writeRaster(dfr,"~/Maxent/GAM_predictions/Grgr_GAM_Jan17.asc",overwrite=TRUE)

