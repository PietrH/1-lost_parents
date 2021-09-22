# mapping hybrids parents ranges


# load libraries ----------------------------------------------------------
library(leaflet)
library(raster)
library(mapview)

# set an example ----------------------------------------------------------




parent_a <- hybrids_and_parents[6,"usageKey_a"]
parent_b <- hybrids_and_parents[6,"usageKey_b"]

hybrid <- hybrids_and_parents[6,"hybrid_formula"] %>% rgbif::name_backbone() %>% pull(usageKey)


# make a plot -------------------------------------------------------------



rgbif::map_fetch(source="density",taxonKey=parent_a, style = "gbif-violet") %>% plot()



# using leaflet -----------------------------------------------------------
# https://data-blog.gbif.org/post/gbif-maps-api-using-r-and-leaflet/



taxon_key = parent_a <- hybrids_and_parents[6,"usageKey_a"] %>% as.integer

map_taxon <- function(taxon_key) {
  


# create style raster layer 
projection = '3857' # projection code
style = 'style=osm-bright' # map style
tileRaster = paste0('https://tile.gbif.org/',projection,'/omt/{z}/{x}/{y}@1x.png?',style)
# create our polygons layer 
prefix = 'https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?'
polygons = 'style=fire.point' # ploygon styles 
taxonKey_query = sprintf('taxonKey=%i',taxon_key) # taxonKey of Odonata (dragonflies and damselflies)
tilePolygons = paste0(prefix,polygons,'&',taxonKey_query)
# plot the styled map
leaflet() %>%
  setView(lng = 5.4265362, lat = 43.4200248, zoom = 01) %>%
  addTiles(urlTemplate = tileRaster) %>%
  addTiles(urlTemplate = tilePolygons)  

}