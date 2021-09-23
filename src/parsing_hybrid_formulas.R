# script to take a hybrid formula and to parse out the parents, and get id's for all 3


# load libraries ----------------------------------------------------------

library(data.table) # Extension of `data.frame`
library(dplyr) # A Grammar of Data Manipulation
library(stringi) # Character String Processing Facilities
library(purrr) # Functional Programming Tools
library(tidyr) # Tidy Messy Data

# load input data ---------------------------------------------------------

# loading the test sets of hybrid names

vascan_names <- readLines(file.path("data", "vascan_hybrid_names.txt"))
gbif_names <- fread("data/pierre-andres_gbif_hybrid1.tsv", encoding = "UTF-8",select = "V2")$V2
quentin_gbif_occ <- readr::read_lines("data/hybridnamesUniq2.txt")

# parsing -----------------------------------------------------------------

# nothospecies means a hybrid (nothovar, nothogenus)

## split on a delimiter ----------------------------------------------------

# TODO something to check which delimiter was likely used for a string from a
# list of known delimiters

delimiters <- c("×"," X "," x ")

delimiter <- ' x | X | × | ×'

is_hybrid_formula <- function(taxon_name, hybrid_delimiter = ' x | X | × | ×') {
  
  # check length of input (currently one at a time please)
  if (length(taxon_name) > 1) {
    stop(sprintf("taxon_name has length %i, should be only 1", length(taxon_name)))
  }
  
  ## taxon_name cleaner code
  
  taxon_name_clean <- 
    stringr::str_remove_all(taxon_name,"\\?") %>% 
    trimws()
  
  
  # parts <- taxon_name %>% stri_split_fixed(hybrid_delimiter)

  
  # parts <- stringr::str_split_fixed(taxon_name_clean,pattern = hybrid_delimiter, n = Inf)
  parts <- stringr::str_split_regex(taxon_name_clean,pattern = hybrid_delimiter, n = Inf) %>% 
    unlist

  ## check if it's a hybrid formula ------------------------------------------

  # if the first part is longer than one word, it's probably a hybrid formula, if
  # it's just one word, it's a hybrid name

  # parts[[1]] %>% 
  #   trimws() %>% 
  #   stringi::stri_split_boundaries(type = "word") %>% 
  #   unlist() %>% 
  #   stri_replace_all_fixed(pattern = " ", replacement = "") %>% 
  #   stri_remove_empty()

  # hybrid_formula <-
  #   length(
  #     stri_split_boundaries(
  #       str = trimws(parts[[1]][1]),
  #       type = "word"
  #     )[[1]]
  #   ) > 1
  # return(hybrid_formula)
  
  
  # return(dim(parts)[[2]] > 1)
  return(stringi::stri_count_words(parts[[1]]) > 1)
}


# filter out hybrid formulas only -----------------------------------------

hybrid_formulas <- gbif_names[purrr::map_lgl(gbif_names,is_hybrid_formula," x")] %>% 
  unique

hybrid_formulas <-
  vascan_names[map_lgl(vascan_names, is_hybrid_formula, "×")]


# get parents from a formula ----------------------------------------------

get_parents <- function(taxon_name, delimiter) {
  parents <-
    taxon_name %>%
    # stri_split_fixed(delimiter) %>%
    stri_split_regex(delimiter) %>%
    unlist()
  # return a dataframe with gbif taxonomic backbone matches for the parents
  
  if(any(stri_count(parents,fixed = " ") < 1)){
    genus <-
      stri_split_boundaries(parents[1], type = "word") %>% unlist %>% .[[1]]
    
    parents <- c(parents[1],paste(genus,parents[2:length(parents)]))
  }

# if the taxon names don't have a space, assume they are of the same genus as the first one

  map_dfr(parents, rgbif::name_backbone)
  
}


# test function
# map(hybrid_formulas, get_parents, delimiter)

get_parents_pivoted <- function(hybrid_formula, delimiter) {
  parents <- get_parents(hybrid_formula, delimiter) %>%
    # filter(!is.na(scientificName)) %>%
    mutate(., parent = letters[1:nrow(.)])

  spec <- parents %>%
    build_wider_spec(., names_from = parent, values_from = names(.))


  # get_parents(hybrid_formula,delimiter) %>% mutate(parent = c("A","B")) %>%
  parents %>%
    pivot_wider_spec(spec) %>%
    mutate(hybrid_formula = hybrid_formula) %>%
    select(
      hybrid_formula,
      starts_with("usageKey"),
      starts_with("scientificName"),
      starts_with("rank"),
      starts_with("confidence")
    )
}


# create dataframe with the hybrid formulas united with their parents
hybrids_and_parents <-
  map_dfr(hybrid_formulas,
          get_parents_pivoted,
          # delimiter
          " x "
          )



# write output ------------------------------------------------------------

fwrite(hybrids_and_parents,
       file.path("data",
                 paste0(
                   format(Sys.time(),
                          "%F_%H-%M"),
                   "_vascan_hybrids_parents",
                   ".csv")
                 )
       )
