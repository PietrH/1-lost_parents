# script to look into Database of Vascular Plants of Canada (VASCAN) gbif
# dataset, to find some examples of hybrid forlumas



# load libraries ----------------------------------------------------------

library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.5
library(data.table) # Extension of `data.frame`, CRAN v1.14.0
library(stringi) # Character String Processing Facilities, CRAN v1.5.3


# load input data ---------------------------------------------------------

vascan <- fread(file.path("data","dwca-vascan-v37.9","taxon.txt"), encoding = "UTF-8")


# get list of hybrid names ------------------------------------------------

vascan %>% 
  pull(scientificName) %>% 
  # unique() %>% 
  stri_extract_all(regex = '^.*×.+') %>% 
  .[!is.na(.)] %>% 
  as.character(.) %>% 
  writeLines(file.path("data","vascan_hybrid_names.txt"))
  


# filter dataframe on hybrids only ----------------------------------------

vascan[stri_detect(scientificName, regex = '^.*×.+'), ] %>%
  View()
