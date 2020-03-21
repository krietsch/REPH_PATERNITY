#========================================================================================================================
# Nest data summary
#========================================================================================================================

# Summary
# 1. Captures on and off plot
# 2. Incubation length & initiation date method

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dc = dbq(con, 'select * FROM CAPTURES')
d = dbq(con, 'select * FROM NESTS')
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

point_over_poly_DT(dc, poly = study_site, buffer = 60) # buffer including birds followed flying off plot
setnames(dc, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := FALSE]
dc = rbind(dc, ds)

# check data
bm = create_bm(dc[study_site == TRUE], buffer = 500)

bm +
  geom_point(data = dc, aes(lon, lat, color = study_site))

# any without metal band?
dc[is.na(ID)]

# assign first capture
dc[, caught_time := as.POSIXct(caught_time)]
setorder(dc, year_, caught_time)
dc[, capture_id := seq_len(.N), by = ID]
dc[, .N, capture_id]

# banded each year on and off plot by us
ds = dc[external == 0 & capture_id == 1 & year_ > 2016]

ds[, .N, .(year_, study_site)]

#------------------------------------------------------------------------------------------------------------------------
# 2. Incubation length & initiation date method
#------------------------------------------------------------------------------------------------------------------------

# assign datetime format
d[, found_datetime := as.POSIXct(found_datetime)]
d[, collected_datetime := as.POSIXct(collected_datetime)]
d[, initiation := as.POSIXct(initiation)]
d[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, est_hatching_datetime := as.POSIXct(est_hatching_datetime)]
d[, hatching_datetime := as.POSIXct(hatching_datetime)]

# incubation period in incubator
d[, found_incomplete := initial_clutch_size < clutch_size]
d[, inc_period := difftime(hatching_datetime, c(initiation + clutch_size * 86400 - 86400), units = 'days') %>% as.numeric]

ds = d[!is.na(inc_period) & found_incomplete == TRUE]
mean(ds[external == 0]$inc_period, na.rm = TRUE)
nrow(ds[external == 0])

# not in incubator
mean(ds[external == 1]$inc_period, na.rm = TRUE)
nrow(ds[external == 1])

ggplot(data = ds[external == 0]) +
  geom_point(aes(y = inc_period, x = initiation_y, color = as.character(year_))) +
  geom_smooth(aes(y = inc_period, x = initiation_y), method = 'lm')

ggplot(data = ds[external == 1]) +
  geom_point(aes(y = inc_period, x = initiation_y, color = as.character(year_))) +
  geom_smooth(aes(y = inc_period, x = initiation_y), method = 'lm')









