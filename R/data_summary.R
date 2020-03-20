#========================================================================================================================
# Nest data summary
#========================================================================================================================

# Summary
# 1. Captures on and off plot
# 2.

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dc = dbq(con, 'select * FROM CAPTURES')
# d = dbq(con, 'select * FROM NESTS')
# dp = dbq(con, 'select * FROM PATERNITY')
# dw = dbq(con, 'select * FROM SNOW_SURVEY')
DBI::dbDisconnect(con)

#------------------------------------------------------------------------------------------------------------------------
# 1. Captures on and off plot
#------------------------------------------------------------------------------------------------------------------------

# Change projection
ds = dc[is.na(lon)] # seperate data without position
dc = dc[!is.na(lon)]
st_transform_DT(dc)

point_over_poly_DT(dc, poly = study_site, buffer = 10)
setnames(dc, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := NA]
dc = rbind(dc, ds)

# check data
bm = create_bm(dc[study_site == TRUE], buffer = 100)

bm +
  geom_sf(data = study_site, fill = 'grey95') +
  geom_point(data = dc, aes(lon, lat, color = study_site))















