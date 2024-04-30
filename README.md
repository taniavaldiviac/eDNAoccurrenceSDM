# A GAM approach for describing species distribution using eDNA-occurrence data

The repository contains R code that can be utilized to acquire environmental layers, metadata processing from Hake sampling, and evaluate the spatial distribution of eDNA of different marine mammals using GAMs. 

The input ASV data used for this paper can be accessed in one of two ways:

**1** - MMARINeDNA team members can access the data from MiSeq plates 306, 307, and 308 on the MURI Google Drive folder ~\Module 3 - West Coast eDNA cetaceans & prey\06 M3 Processed Data.

**2** - Anyone else interested in these data should download the fastq files from NCBI (Accession #s: XXXX) and run them through the [metabarcoding QAQC pipeline](https://github.com/MMARINeDNA/metabarcoding_QAQC_pipeline). Using the pre-loaded default parameters will regenerate the same AVS tables used in this paper. Detailed information on how to use the pipeline, which parameters are used, and the effect of varying those parameters, can be found on the [pipeline Wiki](https://github.com/MMARINeDNA/metabarcoding_QAQC_pipeline/wiki).

The processing steps are divided into three parts.

**Step 1**: Get data from metadata, mainly data wrangling, to have a formatted data frame.

**Step 2**: Obtain environmental data layers and use them to extract information to sampling sites.

**Step 3**: Generalized Additive Models (GAM) and model selection for the species being modeled. This process involves using statistical techniques to determine the most appropriate model for each species.

