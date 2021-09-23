# mapping hybrids parents ranges


# load libraries ----------------------------------------------------------
library(leaflet)
library(raster)
# library(mapview)
library(rangemap)

# set an example ----------------------------------------------------------

hybrids_and_parents <- 
  full_join(
  hybrids_parsed,
  hybrids_parsed_parents,
  by = c("input_strings" = "hybrid_formula"),
  suffix = c(".hybrids", ".parents")
) %>% filter(is_hybrid_formula) %>% 
  filter(!is.na(usageKey_a)) %>% 
  filter(!is.na(usageKey_b)) %>% 
  filter(!is.na(usageKey))


# parent_a <- hybrids_and_parents[6,"usageKey_a"] %>% as.character()
# parent_b <- hybrids_and_parents[6,"usageKey_b"] %>% as.character()

# hybrid <-
#   hybrids_and_parents[6, "hybrid_formula"] %>%
#   rgbif::name_backbone() %>%
#   pull(usageKey) %>%
#   as.character()

parent_a <- pull(slice_sample(hybrids_and_parents, n = 1), usageKey_a) %>% as.character()
parent_b <- pull(slice_sample(hybrids_and_parents, n = 1), usageKey_b) %>% as.character()
hybrid <- pull(slice_sample(hybrids_and_parents, n = 1), usageKey) %>% as.character()



# make a plot -------------------------------------------------------------



rgbif::map_fetch(source="density",taxonKey=parent_a, style = "gbif-violet") %>% plot()



# using leaflet -----------------------------------------------------------
# https://data-blog.gbif.org/post/gbif-maps-api-using-r-and-leaflet/



taxon_key = parent_a <- hybrids_and_parents[6,"usageKey_a"]

map_taxon <- function(taxon_key) {
  


# create style raster layer 
projection = '3857' # projection code
style = 'style=osm-bright' # map style
tileRaster = paste0('https://tile.gbif.org/',projection,'/omt/{z}/{x}/{y}@1x.png?',style)
# create our polygons layer 
prefix = 'https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?'
polygons = 'style=fire.point' # ploygon styles 
taxonKey_query = sprintf('taxonKey=%i',taxon_key) 
tilePolygons = paste0(prefix,polygons,'&',taxonKey_query)
# plot the styled map
leaflet() %>%
  setView(lng = 5.4265362, lat = 43.4200248, zoom = 01) %>%
  addTiles(urlTemplate = tileRaster) %>%
  addTiles(urlTemplate = tilePolygons)  

}


# styles available include classic.point purpleYellow.point fire.point
# glacier.point purpleHeat.point and more, see
# https://www.gbif.org/developer/maps

create_taxon_tiles <-
  function(taxon_key, style = sample(c(
    "classic.point",
    "purpleYellow.point",
    "fire.point",
    "glacier.point"
  ),
  1)) {
    
  prefix = 'https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?'
  polygons = sprintf('style=%s',style) # ploygon styles 
  taxonKey_query = sprintf('taxonKey=%s',taxon_key) 
  tilePolygons = paste0(prefix,polygons,'&',taxonKey_query)
  
  # return(addTiles(urlTemplate = tilePolygons))
  return(tilePolygons)
}


# create leaflet ----------------------------------------------------------
projection = '3857' # projection code
style = 'style=osm-bright' # map style, see https://tile.gbif.org/ui/
tileRaster = paste0('https://tile.gbif.org/',projection,'/omt/{z}/{x}/{y}@1x.png?',style)

leaflet() %>%
  setView(lng = 5.4265362, lat = 43.4200248, zoom = 01) %>%
  addTiles(urlTemplate = tileRaster) %>%
  addTiles(urlTemplate = create_taxon_tiles(parent_a,style = "blue.marker")) %>%
  addTiles(urlTemplate = create_taxon_tiles(parent_b, style = "blue.marker")) %>%
  addTiles(urlTemplate = create_taxon_tiles(hybrid, style = "orange.marker")) # %>% addLegend()



# using rangemapping ------------------------------------------------------

# trying convex hulls

get_occ <- function(taxon_key) {
  transmute(rgbif::occ_data(taxonKey = taxon_key)$data, name = scientificName, longitude = decimalLongitude, latitude = decimalLatitude)
}
