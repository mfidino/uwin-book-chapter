
library(sf)
library(FedData)
library(raster)
library(dplyr)


dat <- read_sf("D:/GIS/cb_2018_us_cbsa_500k")


dat$centeroids <- dat %>% group_by(NAME) %>% 
  st_geometry() %>% 
  st_centroid()

# get just the metro areas in the contiguous US

dat <- dat[-grep(", AK|, HI|, PR", dat$NAME),]


# to store the summarised data
prop_list <- vector("list", length = nrow(d2))

pb <- txtProgressBar(max = nrow(d2))
for(i in 1:nrow(dat)){
setTxtProgressBar(pb, i)
 tmp <- dat[i,]
  
  tmp <- sf::st_transform(
  tmp,
  crs = 4326
)

my_polygon <- FedData::polygon_from_extent(
  extent(
    as.numeric(sf::st_bbox(tmp)[c(1,3,2,4)] + c(-.05,-.05,.05,.05))
  ),
  proj4string = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
)


my_raster <- get_nlcd(my_polygon, label = as.character(tmp$GEOID))

n <- prod(dim(my_raster))
vals <- values(my_raster)
vals <- tabulate(vals, 95)
prop_list[[i]] <- data.frame(
  city = tmp$NAME,
  water = vals[11]/n,
  urban_open = vals[21]/n,
  urban = sum(vals[22:24])/n,
  forest = sum(vals[41:43])/n,
  shrub = sum(vals[51:52])/n,
  ag = sum(vals[81:82])/n
)
}

# combine it
prop_df <- do.call("rbind", prop_list)

# and then join it back to the grand dataset
dat <- inner_join(
  dat,
  prop_df,
  by = c("NAME" = "city")
)
# save it
saveRDS(dat, "./data/municipal_with_covars.RDS")

# make a shapefile for contig US

cont <- read_sf(
  "D:/GIS/continents"
)
cont <- cont[cont$COUNTRY == "USA",]

cont <- cont[-grep("Alaska|Hawaii|United States Virgin Islands|Puerto Rico|water/agua/d'eau", cont$NAME),]

cont <- summarise(cont)
cont <- sf::st_make_valid(cont)
cont <- st_transform(
  cont,
  crs = st_crs(dat)
)
saveRDS(cont, "./data/contig_US.RDS")


