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


# parts <- vascan_names[1] %>% stri_split_fixed(delimitor)


## check if it's a hybrid formula ------------------------------------------

# for(i in seq(parts[[1]])) {
#   length(
#     stri_split_boundaries(str = parts[[1]][i],
#                           type = "word")[[1]]) 
# }

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

spec <- get_parents(hybrid_formulas[33],delimitor) %>% mutate(parent = c("A","B")) %>% 
  build_wider_spec(.,names_from = parent, values_from = names(.))


get_parents_pivoted <- function(hybrid_formula,delimitor) {
  
  get_parents(hybrid_formula,delimitor) %>% mutate(parent = c("A","B")) %>% 
    pivot_wider_spec(spec) %>% 
    transmute(hybrid_formula = hybrid_formula,
              usageKey_A,
              usageKey_B,
              scientificName_A,
              scientificName_B,
              rank_A,
              rank_B,
              confidence_A,
              confidence_B,
    )
}


# get_parents(hybrid_formulas[33],delimitor) %>% mutate(parent = c("A","B")) %>% 
#   pivot_wider_spec(spec) %>% 
#   transmute(hybrid_formula = hybrid_formulas[33],
#             usageKey_A,
#             usageKey_B,
#             scientificName_A,
#             scientificName_B,
#             rank_A,
#             rank_B,
#             confidence_A,
#             confidence_B,
#             )


map_dfr(hybrid_formulas[1:3],get_parents,delimitor) %>% 
  tidyr::pivot_wider(names_from = hybrid_formula) %>% 
  View()
