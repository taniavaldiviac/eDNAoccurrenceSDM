###
#2023 MURI Project
#Module 3: 3D Spatial distribution of marine mammals 
###

##Install packages
list.of.packages=c("sdmpredictors","leaflet", "recipes","raster", "ncdf4","ggplot2","viridis","RColorBrewer",
                   "ggsn","rnaturalearth","rnaturalearthdata","kableExtra","biomod2","knitr","readr",
                   "magrittr","taxize","stars","dplyr","sf","scico","patchwork","stringr",
                   "stringi","vegan","tidyr", "librarian","sp","rgdal","mgcv","gratia","janitor",'tibble')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = T)

library(librarian)

librarian::shelf(list.of.packages)
source('custom_functions.R')

#remotes::install_github("michaeldorman/geobgu", force = TRUE)
library("geobgu")

###
#We are going to create a megadatabase with the information of all the runs 304 306-308
###

md_306 <- read.csv(paste0("~/Library/CloudStorage/GoogleDrive-tania.valdiviac@gmail.com/My Drive/2.2023/05_MURI_Module_3_Tania/Pipeline/306/muri_metabarcoding/final_data/csv_output/metadata_merged.csv"),header = TRUE, sep=",")
md_306 <- md_306 %>% 
  filter(!str_detect(Sample_ID, "DL-positive-control-d1-1")) %>% 
  filter(!str_detect(Sample_ID, "DL-NTC-d1-1")) %>% 
  separate(Sample_ID, into = c("barcode", "project", "sample","dilution","replicate"), sep = "-", remove = FALSE) %>%
  unite(col = "sampleID", project, sample, dilution, sep = "-") %>% 
  select(-X,-barcode,-Sample_Name, -Description,-replicate)
taxon_table_306A <- read.csv(paste0("~/Library/CloudStorage/GoogleDrive-tania.valdiviac@gmail.com/My Drive/2.2023/05_MURI_Module_3_Tania/Pipeline/306/muri_metabarcoding/final_data/csv_output/taxon_table.csv"))
taxon_table_306 <- taxon_table_306A %>% 
  dplyr::select(-1) %>% 
  rename_with(~"Sample_ID", 1) %>% 
  filter(!str_detect(Sample_ID, "Sus scrofa")) %>% 
  filter(!str_detect(Sample_ID, "Bos taurus")) %>% 
 # drop_na(Sample_ID) %>% 
  t %>% 
  row_to_names(row_number = 1) %>% 
  as.data.frame() %>%
  rename_with(~paste0(.x, "_r1"), 1:7) %>% 
  tibble::rownames_to_column(var = "Sample_ID") %>% 
  mutate(Sample_ID = gsub("\\.", "-", Sample_ID)) %>% 
  filter(!str_detect(Sample_ID, "DL-positive-control-d1-1")) %>% 
  separate(Sample_ID, into = c("barcode", "project", "sample","dilution","replicate"), sep = "-", remove = FALSE)%>%
  tidyr::unite(col = "sampleID", project, sample, dilution, sep = "-") %>% 
  dplyr::select(-barcode,-replicate)
md.taxa_306<-full_join(md_306, taxon_table_306, by="sampleID", suffix = c(".x", ".y"))%>% 
  dplyr::select(-Sample_ID.y)  

taxon_table_307 <- read.csv(paste0("~/Library/CloudStorage/GoogleDrive-tania.valdiviac@gmail.com/My Drive/2.2023/05_MURI_Module_3_Tania/Pipeline/307/muri_metabarcoding/final_data/csv_output/taxon_table.csv"))
taxon_table_307 <- taxon_table_307 %>% 
  select(-1) %>% 
  rename_with(~"Sample_ID", 1) %>% 
  filter(!str_detect(Sample_ID, "Sus scrofa")) %>% 
  filter(!str_detect(Sample_ID, "Bos taurus")) %>% 
  drop_na(Sample_ID) %>% 
  t %>% 
  row_to_names(row_number = 1) %>% 
  as.data.frame() %>%
  rename_with(~paste0(.x, "_r2"), 1:5) %>% 
  rownames_to_column(var = "Sample_ID") %>% 
  mutate(Sample_ID = gsub("\\.", "-", Sample_ID)) %>% 
  filter(!str_detect(Sample_ID, "DL-positive-control-d1-2")) %>% 
  separate(Sample_ID, into = c("barcode", "project", "sample","dilution","replicate"), sep = "-", remove = FALSE) %>%
  unite(col = "sampleID", project, sample, dilution, sep = "-") %>% 
  select(-barcode,-replicate)

taxon_table_308_3 <- read.csv(paste0("~/Library/CloudStorage/GoogleDrive-tania.valdiviac@gmail.com/My Drive/2.2023/05_MURI_Module_3_Tania/Pipeline/308/muri_metabarcoding/final_data/csv_output/taxon_table.csv"))
taxon_table_308_3 <- taxon_table_308_3 %>% 
  select(-1) %>% 
  rename_with(~"Sample_ID", 1) %>% 
  filter(!str_detect(Sample_ID, "Sus scrofa")) %>% 
  filter(!str_detect(Sample_ID, "Bos taurus")) %>% 
  filter(!str_detect(Sample_ID, "Oncorhynchus masou")) %>% 
  t %>% 
  row_to_names(row_number = 1) %>% 
  as.data.frame() %>%
  rownames_to_column(var = "Sample_ID") %>% 
  mutate(Sample_ID = gsub("\\.", "-", Sample_ID)) %>% 
  filter(!str_detect(Sample_ID, "DL-positive-control-d1-3")) %>% 
  separate(Sample_ID, into = c("barcode", "project", "sample","dilution","replicate"), sep = "-", remove = FALSE) %>%
  unite(col = "sampleID", project, sample, dilution, sep = "-") %>% 
  select(-barcode)

taxon_table_308_4 <- taxon_table_308_3 %>% 
  filter(grepl("4", replicate)) %>% 
  rename_with(~paste0(.x, "_r4"), 4:10) %>% 
  select(-replicate)

taxon_table_308_5 <- taxon_table_308_3 %>% 
  filter(grepl("5", replicate)) %>% 
  rename_with(~paste0(.x, "_r5"), 4:10)%>% 
  select(-replicate)

taxon_table_308_3 <- taxon_table_308_3 %>% 
  filter(grepl("3", replicate)) %>% 
  rename_with(~paste0(.x, "_r3"), 4:10)%>% 
  select(-replicate)

#MERGE DF FROM DIFFERENT RUNS

md.taxa<-full_join(md.taxa_306, taxon_table_307, by="sampleID", suffix = c(".x", ".y"))%>% 
  select(-Sample_ID)
md.taxa<-full_join(md.taxa, taxon_table_308_3, by="sampleID", suffix = c(".x", ".y"))%>% 
  select(-Sample_ID)

md.taxa[] <- lapply(md.taxa, function(x) replace(x, is.na(x), 0))

md.taxa<-full_join(md.taxa, taxon_table_308_4, by="sampleID", suffix = c(".x", ".y"))%>% 
  select(-Sample_ID)

md.taxa<-full_join(md.taxa, taxon_table_308_5, by="sampleID", suffix = c(".x", ".y"))%>% 
  select(-Sample_ID) %>% 
  mutate_at(vars(34:66), as.numeric)

#DATA IN LONG FORMAT 
md.taxa.long <- md.taxa %>% 
  pivot_longer(cols=34:length(md.taxa), names_to = "species_rep", values_to = "read_count") %>% 
  separate(species_rep, into = c("genus","species_rep"), sep = " ") %>% 
  separate(species_rep, into = c("species","rep"), sep = "_") %>% 
  unite(col = "species", genus:species, sep = " ") %>% 
  mutate(rep = str_remove(rep, "r")) %>%
  filter(!is.na(read_count)) %>% 
  mutate(rep = as.numeric(rep)) %>% 
  group_by(sampleID, species, read_count) %>% 
  mutate(occurrence = case_when(read_count > 0 ~ 1, TRUE ~ 0)) %>% 
  ungroup() %>% 
  group_by(sampleID, species) %>% 
  mutate(sum_occurrence=sum(occurrence)) %>% 
  ungroup() %>% 
  group_by(sampleID) %>%
  mutate(max_rep = max(as.numeric(rep))) %>%
  mutate(prob_detection = sum_occurrence/max_rep) %>% 
  ungroup() %>% 
  group_by(sampleID, species) %>% 
  slice_head() %>% 
  ungroup() %>% 
  select(-rep,-read_count,-occurrence) %>% 
  relocate(station, .after = sampleID) %>% 
  unite(col = "sampleID_station", sampleID:station, sep = "_")

#DATA IN LONG FORMAT 
md.taxa.rep<-read.csv("md_taxa_rep.csv",header = TRUE,sep = ",")

md.taxa.long.rep <- md.taxa.rep %>% 
  pivot_longer(cols=35:length(md.taxa.rep), names_to = "species_rep", values_to = "read_count") %>% 
  separate(species_rep, into = c("genus","species","rep"), sep = "_") %>% 
  #separate(species_rep, into = c("species","rep"), sep = "//-//") %>% 
  unite(col = "species", genus:species, sep = " ") %>% 
  mutate(rep = str_remove(rep, "r")) %>%
  filter(!is.na(read_count)) %>% 
  #mutate(rep = as.numeric(rep)) %>% 
  #group_by(sampleID, species, read_count) %>% 
  mutate(Presence = case_when(read_count > 0 ~ 1, TRUE ~ 0)) 
  
#SUMMARY
summary <-md.taxa.long %>% 
  dplyr::group_by(species) %>% 
  dplyr::mutate(total_detections = sum(sum_occurrence)) %>% 
  dplyr::select(-c(1:33, 35:37)) %>% 
  slice_head()


