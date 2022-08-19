# This file runs all of the scripts that analyze the data and generate the statistics and figures
# for the paper. 

# This script assumes that your working directory is the same folder in which it is contained. 

# Load all packages and utility functions
source("R/source_all.R")

###################################################################################################
# Analyze raw data
###################################################################################################

# Clean the metadata
source("R/clean_metadata.R")

# Clean the MSD data
source("R/clean_msd_data.R")

# Run the models
source("R/model_data.R")

# Perform the meta-analysis
source("R/clean_other_studies.R", encoding = "UTF-8")
source("R/meta_analysis.R", encoding = "UTF-8")

###################################################################################################
# Make figures and statistics
###################################################################################################

# Generate figures
source("R/figures.R")

# Generate supplement
source("R/generate_supplement.R", encoding = "UTF-8")

# Generate demographics table
rmarkdown::render("R/tables.Rmd", output_file = "tables.html", quiet = TRUE)

# Generate supplemental figures
rmarkdown::render("R/plot_each_analyte_meta-analysis_separately.Rmd", 
    output_file = "plot_each_analyte_meta-analysis_separately.pdf", quiet = TRUE)