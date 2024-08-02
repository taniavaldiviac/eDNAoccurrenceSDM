# A GAM approach for describing species distribution using eDNA-occurrence data

The repository contains R code that can be utilized to process data from our investigation, which aimed to leverage eDNA metabarcoding occurrence data to model eDNA species distribution using generalized additive models (GAMs). The code is divided into three steps aimed at formatting metadata files, extracting environmental information from environmental layers to each sampling location, and modeling the spatial distribution of eDNA of different marine mammals using GAMs. 

The input ASV data used for this paper can be accessed in one of two ways:

1. MMARINeDNA team members can access the data from MiSeq plates 306, 307, and 308 on the MURI Google Drive folder ~\Module 3 - West Coast eDNA cetaceans & prey\06 M3 Processed Data.

2. Anyone else interested in these data should download the fastq files from NCBI (Accession #s: XXXX) and run them through the [metabarcoding QAQC pipeline](https://github.com/MMARINeDNA/metabarcoding_QAQC_pipeline). Using the pre-loaded default parameters will regenerate the same AVS tables used in this paper. Detailed information on how to use the pipeline, which parameters are used, and the effect of varying those parameters can be found on the [pipeline Wiki](https://github.com/MMARINeDNA/metabarcoding_QAQC_pipeline/wiki).

The processing steps are divided into three parts:

1. **Step 1**: Aimed at getting data from metadata, mainly data wrangling, to have a formatted data frame with sampling location and species presence-absence data from eDNA metabarcoding of three sequencing replicates.

2. **Step 2**: This step aims to extract environmental information for each sampling site using environmental layers obtained from MARSPEC and COPERNICUS. Note: An extra step was performed in QGIS to homogenize the extent of environmental layers using the shapefile (./downloaded_predictors/shp/). Processed layers are contained in the downloaded_predictors and are ready to be used in Step 2. 

3. **Step 3**: This step is intended to perform Generalized Additive Models (GAM) (using the mgcv package) and model selection (MuMIn) for the most frequently detected species in our metabarcoding dataset.

