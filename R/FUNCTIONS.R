#' Create base map (Function adjusted from package auksRuak: https://github.com/krietsch/auksRuak)
#'
#' Use open street map data to create a basemap with the extent of the data
#'
#' @param DT         Name of the data.table
#' @param lat        Name of the column with latitude (as.character)
#' @param lon        Name of the column with longitude (as.character)
#' @param buffer     Buffer around the data
#' @param sc_dist    Distance of the scale
#' @param squared    If true buffer is created around the centre and outline squared
#' @param projection Projection of the data (default is equal area with centre Barrow)
#'
#' @return           bm, a ggplot2 base map
#' @export
#'
#' @import           data.table
#' @importFrom       sf st_as_sf st_transform st_join st_buffer st_intersects st_geometry st_intersection st_bbox st_as_sfc st_crs
#' @importFrom       magrittr %>%
#' @importFrom       ggplot2 ggplot geom_sf coord_sf aes theme element_line element_rect element_blank unit
#' @importFrom       ggspatial annotation_scale
#'
#' @examples
#' # create table with two points
#' DT = data.table(name = c('NARL', 'Utqiagvik'),
#'                 lat  = c(71.320854, 71.290246),
#'                 lon  = c(-156.648210, -156.788622))
#'
#' # change projection
#' st_transform_DT(DT)
#'
#' # create base map
#' bm = create_bm(DT)
#' bm

create_bm = function(DT, lat = 'lat', lon = 'lon', buffer = 1000, sc_dist, squared = FALSE, 
                     projection = paste0('+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0',
                                         ' +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')){
  
  if(nrow(DT) > 0) {
    
    setnames(DT, c(lat, lon), c('lat', 'lon'))
    
    if(squared == TRUE){
      center_lon = min(DT$lon) + (max(DT$lon) - min(DT$lon))/2
      center_lat = min(DT$lat) + (max(DT$lat) - min(DT$lat))/2
      center_point = st_point(x = c(center_lon, center_lat)) %>% st_sfc(crs = projection)
      rs_extent = center_point %>% st_buffer(buffer) %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_geometry
      rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
      bb = st_bbox(rs_extent) %>% data.table
      
    } else {
        
      st_d = st_as_sf(DT[!is.na(lon), .(lon, lat)], coords = c('lon','lat'), crs = projection)
      rs_extent = st_d %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_buffer(buffer) %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_geometry
      rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
      bb = st_bbox(rs_extent) %>% data.table
    }

    # crop data
    land      = st_intersection(osm_land, rs_extent)
    lakes     = st_intersection(osm_lakes, rs_extent)
    rivers    = st_intersection(osm_rivers, rs_extent)
    roads     = st_intersection(osm_roads, rs_extent)
    buildings = st_intersection(osm_buildings, rs_extent)
    
    bm =
      ggplot() +
      geom_sf(data = land, fill = 'grey92', colour = 'grey80') +
      geom_sf(data = lakes[lakes$fclass == 'water', ], fill = 'grey85', colour = 'grey80') +
      geom_sf(data = roads, color = 'grey60') +
      geom_sf(data = buildings, fill = 'grey50', color = 'grey50', size = 0.3) +
      coord_sf(expand = FALSE, xlim = c(bb$.[1], bb$.[3]), ylim = c(bb$.[2], bb$.[4])) +
      ggspatial::annotation_scale(aes(location = 'bl'), text_cex = 0.8, height = unit(0.2, 'cm'),
                                  pad_x = unit(0.9, 'cm'), pad_y = unit(0.2, 'cm')) +
      ggspatial::annotation_north_arrow(aes(location = 'bl'), which_north = "true",
                                        height = unit(0.6, "cm"), width = unit(0.4, "cm"),
                                        style = ggspatial::north_arrow_orienteering(text_size = 6, fill = c("black", "black")),
                                        pad_x = unit(0.2, 'cm'), pad_y = unit(0.25, 'cm')) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            panel.grid.minor = element_line(colour = "transparent"),
            panel.background = element_rect(fill = 'white'), 
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(fill = NA, colour = "black"),
            axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
            axis.title = element_blank(), plot.margin = unit(c(0, 0, -0.2, -0.2), "lines"),
            legend.background = element_rect(fill = 'transparent'), legend.key = element_blank(),
            legend.key.size = unit(0.5, 'lines')
            )
    
    setnames(DT, c('lat', 'lon'), c(lat, lon))
    
    bm
    
  }
}

