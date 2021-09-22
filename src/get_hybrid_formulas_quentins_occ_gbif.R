# string to take hybrid taxon names Quentin provided based on a very large GBIF
# occurrence dump and detect hybrid formulas, also match the input strings to
# the gbif backbone where possible

# depends on parsing_hybrid_formulas.R


# load libraries ----------------------------------------------------------
library(furrr)



# set futures plan --------------------------------------------------------

plan("multisession",workers = 10)

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
  mutate(is_hybrid_formula = purrr::map_lgl(input_strings, is_hybrid_formula, ' x | X | × | ×')) %>% 
  mutate(has_brackets = stringi::stri_detect_regex(input_strings,"\\(|\\)")) %>% 
  select(input_strings,is_hybrid_formula,has_brackets,tidyselect::everything())


tictoc::toc()


# get parents for non bracket non formula ---------------------------------

hybrids_parsed %>%
  filter(is_hybrid_formula) %>%
  filter(!has_brackets) %>%
  nrow()
