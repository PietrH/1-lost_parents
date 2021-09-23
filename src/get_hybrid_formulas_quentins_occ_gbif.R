# string to take hybrid taxon names Quentin provided based on a very large GBIF
# occurrence dump and detect hybrid formulas, also match the input strings to
# the gbif backbone where possible

# depends on parsing_hybrid_formulas.R


# load libraries ----------------------------------------------------------
library(furrr)
library(dplyr)
library(stringi)
library(tidyr)

# set futures plan --------------------------------------------------------

plan("multisession",workers = 8)

# load input --------------------------------------------------------------


quentin_gbif_occ <- readr::read_lines("data/hybridnamesUniq2.txt")


# match to gbif and detect hybrid formulas --------------------------------



# just detect the hybrid formulas

# hybrids_parsed <-
#   tibble(mixed = quentin_gbif_occ) %>%
#   mutate(is_hybrid_formula = purrr::map_chr(mixed, is_hybrid_formula, ' x | X | × | ×'))

# add gbif backbone information
tictoc::tic()
hybrids_parsed <- 
  furrr::future_map_dfr(
    # sample(quentin_gbif_occ,5000),
    quentin_gbif_occ,
               function(x) {
                 rgbif::name_backbone(x) %>% 
                   mutate(input_strings = x)
               },
               .options = furrr_options(seed = NULL)
                 ) %>% 
  mutate(is_hybrid_formula = furrr::future_map_lgl(input_strings, is_hybrid_formula, ' x | X | × | ×')) %>% 
  mutate(has_brackets = stringi::stri_detect_regex(input_strings,"\\(|\\)")) %>% 
  select(input_strings,is_hybrid_formula,has_brackets,tidyselect::everything())


tictoc::toc()


# get parents for non bracket non formula ---------------------------------

hybrids_parsed_parents <- 
  hybrids_parsed %>%
  filter(is_hybrid_formula) %>%
  filter(!has_brackets) %>%
  pull(input_strings) %>% 
  future_map_dfr(get_parents_pivoted,' x | X | × | ×',.options = furrr_options(seed = NULL))


# hybrids_and_parents %>% full_join(hybrids_parsed,by = c( "hybrid_formula" = "input_strings")) %>% View()

full_join(
  hybrids_parsed,
  hybrids_parsed_parents,
  by = c("input_strings" = "hybrid_formula"),
  suffix = c(".hybrids", ".parents")
) %>% 
  data.table::fwrite(file.path("data",
                   paste0(
                     format(Sys.time(),
                            "%F_%H-%M"),
                     "_hybridnamesUniq2_parents",
                     ".csv")
  ))
