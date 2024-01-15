
list.of.packages=c("readr","dplyr","ggplot2", "here", "raster", "mgcv","gratia", "librarian", "lme4", "maps","sf", "car", "usdm", "mgcv.helper","dplyr", 
                   "dsm", "Distance", "knitr", "captioner", "ggplot2", "rgdal","maptools", "tweedie","stringr","fuzzySim")
detach("plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = T)
library(librarian)
librarian::shelf(list.of.packages)


strings_to_remove.Laob<- c("52193-1485-d1_75-5",
                           "52193-1472-d5_75-6",
                           "52193-1510-d1_75-3",
                           "52193-1536-d1_77-1",
                           "52193-1542-d1_77-2",
                           "52193-1561-d1_77-5",
                           "52193-1662-d1_80-1",
                           "52193-1836-d1_83-5",
                           "52193-1843-d1_85-15",
                           "52193-1867-d5_85-13",
                           "52193-1879-d1_85-11")

strings_to_remove.Meno<- c("52193-1472-d1_75-6",
                           "52193-1485-d1_75-5",
                           "52193-1510-d1_75-3",
                           "52193-1536-d1_77-1",
                           "52193-1542-d1_77-2",
                           "52193-1561-d1_77-5",
                           "52193-1662-d1_80-1",
                           "52193-1828-d1_83-6",
                           "52193-1836-d1_83-5",
                           "52193-1843-d1_85-15",
                           "52193-1867-d1_85-13",
                           "52193-1879-d1_85-11")

strings_to_remove.Grgr<- c("52193-1472-d1_75-6",
                           "52193-1485-d1_75-5",
                           "52193-1510-d1_75-3",
                           "52193-1536-d1_77-1",
                           "52193-1542-d1_77-2",
                           "52193-1561-d1_77-5",
                           "52193-1662-d1_80-1",
                           "52193-1828-d1_83-6",
                           "52193-1836-d1_83-5",
                           "52193-1843-d1_85-15",
                           "52193-1867-d1_85-13",
                           "52193-1879-d1_85-11")

strings_to_remove.Libo<- c("52193-1472-d1_75-6",
                           "52193-1485-d1_75-5",
                           "52193-1510-d1_75-3",
                           "52193-1536-d1_77-1",
                           "52193-1542-d1_77-2",
                           "52193-1561-d1_77-5",
                           "52193-1662-d1_80-1",
                           "52193-1828-d1_83-6",
                           "52193-1836-d1_83-5",
                           "52193-1843-d1_85-15",
                           "52193-1867-d1_85-13",
                           "52193-1879-d1_85-11")


Laob.test_083_F <- md.taxa.long.formated.env083  %>% 
  filter(str_detect(species, "Lagenorhynchus obliquidens")) %>% 
  filter(!sampleID_station %in% strings_to_remove.Laob)

Meno.test_083_F <- md.taxa.long.formated.env083  %>% 
  filter(str_detect(species, "Megaptera novaeangliae")) %>% 
  filter(!sampleID_station %in% strings_to_remove.Grgr)

Grgr.test_083_F <- md.taxa.long.formated.env083  %>% 
  filter(str_detect(species, "Grampus griseus")) %>% 
  filter(!sampleID_station %in% strings_to_remove.Grgr)

Libo.test_083_F <- md.taxa.long.formated.env083  %>% 
  filter(str_detect(species, "Lissodelphis borealis")) %>% 
  filter(!sampleID_station %in% strings_to_remove.Libo)

Laob.test_083_R <- Laob.test_083_F  %>% 
  dplyr::select(c(37,32,23,24,26,38:53)) 

Meno.test_083_R <- Meno.test_083_F  %>% 
  dplyr::select(c(37,32,23,24,26,38:53)) 

Grgr.test_083_R <- Grgr.test_083_F  %>% 
  dplyr::select(c(37,32,23,24,26,38:53)) 

Libo.test_083_R <- Libo.test_083_F  %>% 
  dplyr::select(c(37,32,23,24,26,38:53)) 

#Surface data
`Laob.test.surface_083_R` <- Laob.test_083_R %>%
  filter(str_detect(depth_class, "surface \\(0 m\\)")) %>% 
  dplyr::select(-depth_class)

Meno.test.surface_083_R <- Meno.test_083_R %>%
  filter(str_detect(depth_class, "surface \\(0 m\\)")) %>% 
  dplyr::select(-depth_class)

Grgr.test.surface_083_R <- Grgr.test_083_R %>%
  filter(str_detect(depth_class, "surface \\(0 m\\)")) %>% 
  dplyr::select(-depth_class)

Libo.test.surface_083_R <- Libo.test_083_R %>%
  filter(str_detect(depth_class, "surface \\(0 m\\)")) %>% 
  dplyr::select(-depth_class)

#Depth data
Laob.test.depth_083_R <- Laob.test_083_R %>%
  filter(str_detect(depth_class, "depth \\(50 m\\)"))%>% 
  dplyr::select(-depth_class)

Meno.test.depth_083_R <- Meno.test_083_R %>%
  filter(str_detect(depth_class, "depth \\(50 m\\)"))%>% 
  dplyr::select(-depth_class)

Grgr.test.depth_083_R <- Grgr.test_083_R %>%
  filter(str_detect(depth_class, "depth \\(50 m\\)"))%>% 
  dplyr::select(-depth_class)

Libo.test.depth_083_R <- Libo.test_083_R %>%
  filter(str_detect(depth_class, "depth \\(50 m\\)"))%>% 
  dplyr::select(-depth_class)

##Surface Presence Map
color_mapping <- c("0" = "white", "1" = "#ffce00", "2" = "#ff9a00", "3" = "#ff5a00", "4"="#ff0000", "5"="#940000")
surface <- ggplot() +
  geom_tile(data = envCov_WA_surface_083_082019_df,
            aes(x = x, y = y, fill = bathy)) +
  geom_point(data = Laob.test.surface_083_R,
             aes(x = lon, y = lat, color = as.factor(Presence)),size=4) +
  scale_fill_continuous(type = "gradient") +
  scale_color_manual(values = color_mapping) +  
  labs(title = "Pacific white-sided dolphin eDNA detections (surface)")

surface + theme(legend.position="right", legend.box = "vertical",plot.title = element_text(size = 12, face = "bold"),
                              legend.title=element_text(size=7), 
                              legend.text=element_text(size=7),
                              legend.spacing.y = unit(0.5, 'cm'))
##Depth Presence Map
depth <- ggplot() +
  geom_tile(data = envCov_WA_surface_083_082019_df,
            aes(x = x, y = y, fill = bathy)) +
  geom_point(data = Laob.test.depth_083_R,
             aes(x = lon, y = lat, color = as.factor(Presence)),size=4) +
  scale_fill_continuous(type = "gradient") +
  scale_color_manual(values = color_mapping) +  
  labs(title = "Pacific white-sided dolphin eDNA detections (depth)")
depth + theme(legend.position="right", legend.box = "vertical",plot.title = element_text(size = 12, face = "bold"),
                            legend.title=element_text(size=7), 
                            legend.text=element_text(size=7),
                            legend.spacing.y = unit(0.5, 'cm'))

#False discovery rate:Calculate the false discovery rate (type I error) under repeated 
#testing and determine which variables to select and to exclude from multivariate analysis.
data<-Laob.test.surface_083_R[,c(1,2,3,5,6,7,8,9,10,11,13,14,15,18,19,20)]
FDR(data=data,sp.cols = 1,var.cols = 3:ncol(data),family='binomial')
corSelect(data,sp.cols = 1,var.cols = 3:ncol(data), cor.thresh = 0.7)

#Variable correlation
Z <- as.data.frame(data)
Z<-na.omit(Z)
cor_matrix <- cor(Z)
library(reshape2)
cor_matrix_melted <- melt(cor_matrix)
ggplot(data = cor_matrix_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "light blue", high = "red", na.value = "black") +
  geom_text(aes(label = round(value, 1)), vjust = 1, size = 3) +  # Smaller font size (adjust size as needed)
  labs(title = "Correlation Heatmap surface data") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Multicolinealidad
multicol(data)

model00	=	gam(Presence~	1	,data = Meno.test.surface_083_R, family = binomial, method="REML")																				
model01	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")														
model02	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")														
model03	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")														
model04	=	gam(Presence~	s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")														
model05	=	gam(Presence~	s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")														
model06	=	gam(Presence~	s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")														
model07	=	gam(Presence~	s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")														
model08	=	gam(Presence~	ti(lon,lat, k=3)	+ s(bathy,bs=	"tp"	,k=	3	),data = Meno.test.surface_083_R, family = binomial, method="REML")	
model09	=	gam(Presence~	ti(lon,lat,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model10	=	gam(Presence~	ti(lon,lat,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model11	=	gam(Presence~	ti(lon,lat,k=3)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model12	=	gam(Presence~	ti(lon,lat,k=3)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model13	=	gam(Presence~	ti(lon,lat,k=3)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model14	=	gam(Presence~	ti(lon,lat,k=3)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model15	=	gam(Presence~	ti(lon,lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	slope	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model16	=	gam(Presence~	ti(lon,lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model17	=	gam(Presence~	ti(lon,lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"ts"		)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model18	=	gam(Presence~	ti(lon,lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model19	=	gam(Presence~	ti(lon,lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"ts"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						

model20 = gam(Presence~	ti(lon,lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"ad"	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
draw(model08)
gam.check(model08)
model21	=	gam(Presence~	ti(lon,lat,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model22	=	gam(Presence~	ti(lon,lat,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model23	=	gam(Presence~	ti(lon,lat,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model24	=	gam(Presence~	ti(lon,lat,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model25	=	gam(Presence~	ti(lon,lat,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model26	=	gam(Presence~	ti(lon,lat,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model27	=	gam(Presence~	ti(lon,lat,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model28	=	gam(Presence~	ti(lon,lat,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model29	=	gam(Presence~	ti(lon,lat,k=3)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model30	=	gam(Presence~	ti(lon,lat,k=3)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model31	=	gam(Presence~	ti(lon,lat,k=3)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model32	=	gam(Presence~	ti(lon,lat,k=3)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model33	=	gam(Presence~	ti(lon,lat,k=3)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model34	=	gam(Presence~	ti(lon,lat,k=3)	+s(	zooc	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model35	=	gam(Presence~	s(lon,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model36	=	gam(Presence~	s(lon,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model37	=	gam(Presence~	s(lon,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model38	=	gam(Presence~	s(lon,k=3)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model39	=	gam(Presence~	s(lon,k=3)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model40	=	gam(Presence~	s(lon,k=3)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model41	=	gam(Presence~	s(lon,k=3)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model42	=	gam(Presence~	s(lon,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	slope	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model43	=	gam(Presence~	s(lon,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model44	=	gam(Presence~	s(lon,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model45	=	gam(Presence~	s(lon,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model46	=	gam(Presence~	s(lon,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model47	=	gam(Presence~	s(lon,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model48	=	gam(Presence~	s(lon,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model49	=	gam(Presence~	s(lon,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model50	=	gam(Presence~	s(lon,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model51	=	gam(Presence~	s(lon,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model52	=	gam(Presence~	s(lon,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model53	=	gam(Presence~	s(lon,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model54	=	gam(Presence~	s(lon,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model55	=	gam(Presence~	s(lon,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model56	=	gam(Presence~	s(lon,k=3)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model57	=	gam(Presence~	s(lon,k=3)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model58	=	gam(Presence~	s(lon,k=3)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model59	=	gam(Presence~	s(lon,k=3)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model60	=	gam(Presence~	s(lon,k=3)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model61	=	gam(Presence~	s(lon,k=3)	+s(	zooc	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model62	=	gam(Presence~	s(lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model63	=	gam(Presence~	s(lat,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model64	=	gam(Presence~	s(lat,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model65	=	gam(Presence~	s(lat,k=3)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model66	=	gam(Presence~	s(lat,k=3)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model67	=	gam(Presence~	s(lat,k=3)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model68	=	gam(Presence~	s(lat,k=3)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")													
model69	=	gam(Presence~	s(lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	slope	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model70	=	gam(Presence~	s(lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model71	=	gam(Presence~	s(lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model72	=	gam(Presence~	s(lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model73	=	gam(Presence~	s(lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model74	=	gam(Presence~	s(lat,k=3)	+s(	bathy	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model75	=	gam(Presence~	s(lat,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model76	=	gam(Presence~	s(lat,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model77	=	gam(Presence~	s(lat,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model78	=	gam(Presence~	s(lat,k=3)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model79	=	gam(Presence~	s(lat,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model80	=	gam(Presence~	s(lat,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model81	=	gam(Presence~	s(lat,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model82	=	gam(Presence~	s(lat,k=3)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model83	=	gam(Presence~	s(lat,k=3)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model84	=	gam(Presence~	s(lat,k=3)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model85	=	gam(Presence~	s(lat,k=3)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model86	=	gam(Presence~	s(lat,k=3)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model87	=	gam(Presence~	s(lat,k=3)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model88	=	gam(Presence~	s(lat,k=3)	+s(	zooc	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")						
model89	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model90	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model91	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model92	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	slope	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model93	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model94	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model95	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model96	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model97	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	slope	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model98	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model99	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model100	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model101	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model102	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model103	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")							
model104	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model105	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model106	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model107	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model108	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model109	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model110	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model111	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	slope	,bs=	"tp"	,k=	3	)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model112	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model113	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model114	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model115	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model116	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model117	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model118	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model119	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model120	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model121	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model122	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model123	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model124	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model125	=	gam(Presence~	s(	bathy	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model126	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model127	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model128	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model129	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model130	=	gam(Presence~	s(	dist_shore	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model131	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model132	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model133	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model134	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model135	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model136	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model137	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model138	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model139	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	+s(	epi	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model140	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	+s(	npp	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model141	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")
model142	=	gam(Presence~	s(	slope	,bs=	"tp"	,k=	3	)	+s(	zooc	,bs=	"tp"	,k=	3	)	+s(	SWT	,bs=	"tp"	,k=	3	)	,data = Meno.test.surface_083_R, family = binomial, method="REML")

CAICF(model00,
    model01,
    model02,
    model03,
    model04,
    model05,
    model06,
    model07,
    model08,
    model09,
    model10,
    model11,
    model12,
    model13,
    model14,
    model15,
    model16,
    model17,
    model18,
    model19,
    model20,
    model21,
    model22,
    model23,
    model24,
    model25,
    model26,
    model27,
    model28,
    model29,
    model30,
    model31,
    model32,
    model33,
    model34,
    model35,
    model36,
    model37,
    model38,
    model39,
    model40,
    model41,
    model42,
    model43,
    model44,
    model45,
    model46,
    model47,
    model48,
    model49,
    model50,
    model51,
    model52,
    model53,
    model54,
    model55,
    model56,
    model57,
    model58,
    model59,
    model60,
    model61,
    model62,
    model63,
    model64,
    model65,
    model66,
    model67,
    model68,
    model69,
    model70,
    model71,
    model72,
    model73,
    model74,
    model75,
    model76,
    model77,
    model78,
    model79,
    model80,
    model81,
    model82,
    model83,
    model84,
    model85,
    model86,
    model87,
    model88,
    model89,
    model90,
    model91,
    model92,
    model93,
    model94,
    model95,
    model96,
    model97,
    model98,
    model99,
    model100,
    model101,
    model102,
    model103,
    model104,
    model105,
    model106,
    model107,
    model108,
    model109,
    model110,
    model111,
    model112,
    model113,
    model114,
    model115,
    model116,
    model117,
    model118,
    model119,
    model120,
    model121,
    model122,
    model123,
    model124,
    model125,
    model126,
    model127,
    model128,
    model129,
    model130,
    model131,
    model132,
    model133,
    model134,
    model135,
    model136,
    model137,
    model138,
    model139,
    model140,
    model141,
    model142, REML = NULL)

anova(model00,
    model01,
    model02,
    model03,
    model04,
    model05,
    model06,
    model07,
    model08,
    model09,
    model10,
    model11,
    model12,
    model13,
    model14,
    model15,
    model16,
    model17,
    model18,
    model19,
    model20,
    model21,
    model22,
    model23,
    model24,
    model25,
    model26,
    model27,
    model28,
    model29,
    model30,
    model31,
    model32,
    model33,
    model34,
    model35,
    model36,
    model37,
    model38,
    model39,
    model40,
    model41,
    model42,
    model43,
    model44,
    model45,
    model46,
    model47,
    model48,
    model49,
    model50,
    model51,
    model52,
    model53,
    model54,
    model55,
    model56,
    model57,
    model58,
    model59,
    model60,
    model61,
    model62,
    model63,
    model64,
    model65,
    model66,
    model67,
    model68,
    model69,
    model70,
    model71,
    model72,
    model73,
    model74,
    model75,
    model76,
    model77,
    model78,
    model79,
    model80,
    model81,
    model82,
    model83,
    model84,
    model85,
    model86,
    model87,
    model88,
    model89,
    model90,
    model91,
    model92,
    model93,
    model94,
    model95,
    model96,
    model97,
    model98,
    model99,
    model100,
    model101,
    model102,
    model103,
    model104,
    model105,
    model106,
    model107,
    model108,
    model109,
    model110,
    model111,
    model112,
    model113,
    model114,
    model115,
    model116,
    model117,
    model118,
    model119,
    model120,
    model121,
    model122,
    model123,
    model124,
    model125,
    model126,
    model127,
    model128,
    model129,
    model130,
    model131,
    model132,
    model133,
    model134,
    model135,
    model136,
    model137,
    model138,
    model139,
    model140,
    model141,
    model142,test = "Chisq")

predictions =  predict(envCov_spol_surface_083_082019, model = model106, type = "response")
spplot(predictions, colorkey = list(space = "left") ,scales = list(draw = TRUE))
summary(model106)
cor(Laob.test.surface_083_R$bathy,Laob.test.surface_083_R$dist_shore)
