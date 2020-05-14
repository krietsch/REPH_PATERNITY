#========================================================================================================================
# EXAMPLE MAP
#========================================================================================================================

sapply( c('data.table', 'magrittr', 'sdb','sdbvis', 'sf', 'ggplot2', 'maptools', 'rgdal', 'auksRuak', 'foreach'),
        require, character.only = TRUE)

# Projection
PROJ   = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

#------------------------------------------------------------------------------------------------------------------------
# FUNCTIONS
#------------------------------------------------------------------------------------------------------------------------

# theme for map
theme_classic_b = function (base_size = 11, base_family = '', base_line_size = base_size/22, 
                            base_rect_size = base_size/22) 
{
  theme_grey(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(panel.background = element_blank(), 
          panel.grid = element_line(color = 'grey70', size = 0.3), 
          panel.ontop = TRUE, 
          rect = element_blank(), 
          panel.grid.minor = element_line(size = rel(0.5)),            
          panel.border = element_rect(colour = 'black', fill = NA, size = 1),
          strip.background = element_rect(fill = 'grey85', colour = 'grey20'), 
          legend.key = element_rect(fill = 'white', colour = NA), complete = TRUE)
}

# transform data in table to create kml
d_plot = function(x){
  ds = data.table(id = x$tagID,
                  datetime_ = x$datetime_,
                  lat = x$lat,
                  lon = x$lon)
  ds
  
}

#------------------------------------------------------------------------------------------------------------------------
# LOAD AND TRANSFORM DATA
#------------------------------------------------------------------------------------------------------------------------

# get raw data
con = dbcon('jkrietsch', db = 'ARGOS') 
d  = dbq(con, 'select * FROM 2019_EUDO')
DBI::dbDisconnect(con)

# change names
setnames(d, c('latitude', 'longitude', 'locationDate'), c('lat', 'lon', 'datetime_'))
d[, datetime_ := as.POSIXct(datetime_)]

# subset tracks on migration
d = d[tagID == 66809 | tagID == 66813]


ggplot(data = d) +
  geom_point(aes(lon, lat, color = as.character(tagID)))

# peak at the data
world_map = map_data('world')
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill ='lightgray', colour = 'white') +
  geom_point(data = d, aes(lon, lat, color = as.character(tagID)), show.legend = FALSE)

d$lon %>% min
d$lon %>% max
d$lat %>% min
d$lat %>% max

# add point id 
d[, point_id := seq_len(.N), by = tagID] 

# create kml's
kml(dat = d_plot(d[tagID == 66809]), file = paste0('./REPORTS/ID_', 66809,'_TRACK.kml'), scale = 0.5)
kml(dat = d_plot(d[tagID == 66813]), file = paste0('./REPORTS/ID_', 66813,'_TRACK.kml'), scale = 0.5)






# load  data
d = readRDS('./DATA/crw_predictions_15min.rds')  %>% data.table

# subset original
do = d[locType == 'o', .(tagID, datetime_, lon = lon_raw, lat = lat_raw, locType)]

# subset predicted
dp = d[locType == 'p', .(tagID, datetime_, lon, lat, locType)]

# peak at the data
world_map = map_data('world')
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill='lightgray', colour = 'white') +
  geom_point(data = do, aes(lon, lat, color = as.character(tagID)), show.legend = FALSE)

# change projection to polar
spTransform.DT(do, lat = 'lat', lon = 'lon', to = PROJ)
spTransform.DT(dp, lat = 'lat', lon = 'lon', to = PROJ)

# high resolution coast line in laea
polar_region = readOGR(dsn = './DATA/source_data.db' , layer='polar_region') %>% st_as_sfc

# Places in Alaksa
da = data.table(name = c('Utqiagvik', 'Nome'), 
                lat = c(71.321174, 64.500958),
                lon = c(-156.666726, -165.401924) )
spTransform.DT(da, lat = 'lat', lon = 'lon', to = PROJ)

dm = data.table(name = c('Alaska'), 
                lat = c(69.212033),
                lon = c(-160.121766) )
spTransform.DT(dm, lat = 'lat', lon = 'lon', to = PROJ)

#------------------------------------------------------------------------------------------------------------------------
# PLOT EXAMPLE
#------------------------------------------------------------------------------------------------------------------------

# subset individual
dos = do[tagID == 169917]
dps = dp[tagID == 169917]

# crop map with individual
rs = st_as_sf(dps[!is.na(lon), .(lon, lat)], coords = c('lon','lat'), crs = PROJ)
rs_extent = rs %>% st_bbox(crs = PROJ) %>% st_as_sfc %>% st_buffer(100000) %>% st_bbox(crs = PROJ) %>% st_as_sfc %>% st_geometry
bb = st_bbox(rs_extent) %>% data.table
lpc = st_intersection(polar_region, rs_extent)

# map
p = 
  ggplot() + 
  geom_sf(data = rs_extent, color = 'black', fill = '#D7E7FF') +
  geom_sf(data = lpc, fill = 'grey95') +
  geom_sf(data = rs_extent, color = 'black', fill = NA) +
  geom_path(data = dps, aes(lon, lat), color = 'firebrick3', size = 1.2, alpha = 0.8) +
  geom_point(data = dos, aes(lon, lat), color = 'black', alpha = 0.3) +
  geom_point(data = da, aes(lon, lat), color = 'grey50', size = 3, shape = 15, stroke = 2) +
  geom_text(data = da, aes(lon, lat,label = name), hjust = 0.5, vjust = -1.5, color = 'grey20', size = 6) +
  geom_text(data = dm, aes(lon, lat,label = name), hjust = 0.4, vjust = 2, color = 'grey20', size = 15) +
  ggspatial::annotation_scale(aes(location = 'br'), text_cex = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab('Longitude') + ylab('Latitude') +
  theme_classic_b(base_size = 20)
p

# tiff('./REPORTS/1REPH_track.tiff', width = 800, height = 800)
# p
# dev.off()
