# script to get hybrid species from gbif species api (checklists)


# load libraries ----------------------------------------------------------

library(httr) # Tools for Working with URLs and HTTP, CRAN v1.4.2
library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.5
library(furrr) # Apply Mapping Functions in Parallel using Futures, CRAN v0.2.2

# set variables -----------------------------------------------------------

limit <- 1000
offset <- 0
out <- tibble()
# parse query url ---------------------------------------------------------

for (offset in seq(0, 40000, by = limit)) {
  print(offset)

  url <-
    sprintf(
      "https://api.gbif.org/v1/species/search?name_type=HYBRID&limit=%i&offset=%i",
      limit,
      offset
    )

  response <- httr::GET(url)

  # only taking the first 25 colums of every response
  parsed <- httr::content(response, as = "parsed")

  # parsed_columns <- lapply(parsed$results,function(x) x[1:20])
  parsed_columns <-
    lapply(parsed$results, function(x) {
      x[c(
        "key",
        "accepted",
        "kingdom",
        "scientificName",
        "speciesKey",
        "taxonID"
      )]
    })
  # binded <- data.table::rbindlist(parsed$results, fill = TRUE)

  # data.frame(t(sapply(parsed$results,c)))
  #
  # View(parsed$results)



  out <- bind_rows(out, data.table::rbindlist(parsed_columns, fill = TRUE)) %>%
    janitor::remove_empty(which = "cols")

  message(sprintf("we have %i rows in our out with offset %i", nrow(out), offset))
}

setDT(out)

data.table::setkey(out, speciesKey)


# matching to gbif backbone locally ---------------------------------------

#
# backbone <- fread(
#   "data/Taxon.tsv",
#   encoding = "UTF-8",
#   # nrows = 1000,
#   select = c("taxonID", "kingdom"),
#   key = taxonID
# )
#
# backbone[out, by = c()] %>% glimpse
#
# dplyr::inner_join(out, backbone, by = c("speciesKey" = "taxonID")) %>% glimpse()
#

# using the taxonomic backbone --------------------------------------------

plan(multisession, workers = 8)

hybrids_matched <-
  filter(out, kingdom == "Plantae" | is.na(kingdom)) %>%
  pull(scientificName) %>%
  # sample(600) %>%
  future_map_dfr(function(x) {
    stringr::str_remove_all(x, pattern = "_x") %>%
      rgbif::name_backbone()
  },
  .options = furrr_options(seed = NULL)
  )

# create csv output -------------------------------------------------------


hybrids_matched %>% data.table::fwrite(file.path(
  "data",
  paste0(
    format(
      Sys.time(),
      "%F_%H-%M"
    ),
    "_gbif_checklist_hybrids",
    ".csv"
  )
))

