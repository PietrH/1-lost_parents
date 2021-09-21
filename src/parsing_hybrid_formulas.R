# script to take a hybrid formula and to parse out the parents, and get id's for all 3


# load libraries ----------------------------------------------------------

library(data.table)
library(dplyr)
library(stringi)
library(purrr)
library(tidyr)

# load input data ---------------------------------------------------------

# loading the test sets of hybrid names

vascan_names <- readLines(file.path("data","vascan_hybrid_names.txt"))

# parsing -----------------------------------------------------------------

# nothospecies means a hybrid (nothovar, nothogenus)

## split on a delimitor ----------------------------------------------------

# TODO something to check which delimiter was likely used for a string from a
# list of known delimitors

delimitors <- c("×")

delimitor <- "×"

is_hybrid_formula <- function(taxon_name,hybrid_delimitor) {
  
  parts <- taxon_name %>% stri_split_fixed(hybrid_delimitor)


## check if it's a hybrid formula ------------------------------------------

# if the first part is longer than one word, it's probably a hybrid formula, if
# it's just one word, it's a hybrid name

  
hybrid_formula <- 
  length(
  stri_split_boundaries(str = trimws(parts[[1]][1]),
                        type = "word")[[1]]) > 1
return(hybrid_formula)

}

# for(i in 1:50){get_parents(vascan_names[i],"×")}

# filter out hybrid formulas only -----------------------------------------

hybrid_formulas <- 
  vascan_names[map_lgl(vascan_names,is_hybrid_formula,"×")]


# get parents from a formula ----------------------------------------------

get_parents <- function(taxon_name,delimitor){
  
  parents <- 
    taxon_name %>% 
    stri_split_fixed(delimitor) %>% 
    unlist()
  # return a dataframe with gbif taxonomic backbone matches for the parents
  map_dfr(parents,rgbif::name_backbone) 
  
}



map(hybrid_formulas,get_parents,delimitor)




get_parents_pivoted <- function(hybrid_formula,delimitor) {
  
  parents <- get_parents(hybrid_formula,delimitor) %>%
    filter(!is.na(scientificName)) %>% 
    mutate(.,parent = letters[1:nrow(.)])
  
  spec <- parents %>% 
    build_wider_spec(.,names_from = parent, values_from = names(.))
  
  
  # get_parents(hybrid_formula,delimitor) %>% mutate(parent = c("A","B")) %>%
  parents %>%
    pivot_wider_spec(spec) %>% 
    mutate(hybrid_formula = hybrid_formula) %>% 
    select(hybrid_formula,
              starts_with("usageKey"),
              starts_with("scientificName"),
              starts_with("rank"),
              starts_with("confidence")
    )
}


hybrids_and_parents <- map_dfr(hybrid_formulas,get_parents_pivoted,delimitor) 

