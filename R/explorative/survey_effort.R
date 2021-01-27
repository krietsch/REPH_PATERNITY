#========================================================================================================================
# Survey effort analysis
#========================================================================================================================

# Summary
# 1. Assign locations in the study area & calculate distance and time between points
# 2. Survey effort NARL study site
# 3. Survey effort by rapid survey plots

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak', 'viridis', 'anytime', 'lubridate', 'windR'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dr = dbq(con, 'select * FROM RESIGHTINGS')
dp = dbq(con, 'select * FROM GPS_POINTS')
d = dbq(con, 'select * FROM GPS_TRACKS')
DBI::dbDisconnect(con)

# merge with points
# dp[, type := 'point']
# dt[, type := 'track']
# d = rbind(dp, dt, use.names = TRUE, fill = TRUE)

# Change projection
st_transform_DT(d)

# Functions
# distance between points
dst = function (x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

#------------------------------------------------------------------------------------------------------------------------
# 1. Assign locations in the study area & calculate distance and time between points
#------------------------------------------------------------------------------------------------------------------------

point_over_poly_DT(d, poly = study_site, buffer = 10)
setnames(d, 'poly_overlap', 'study_site')

# points over BARC
BARC = data.table(lat = 71.324971,
                  lon = -156.667436)
st_transform_DT(BARC)
st_BARC = st_as_sf(BARC, coords = c('lon','lat'), crs = PROJ) 

point_over_poly_DT(d, poly = st_BARC, buffer = 30)
setnames(d, 'poly_overlap', 'BARC')

# distance to next point
setorder(d, year_, gps_id, datetime_)
d[, ':=' (lat2 = shift(lat, type = 'lead'), 
          lon2 = shift(lon, type = 'lead')), 
  by = .(gps_id, seg_id)]
d[, distance := dst(lat, lon, lat2, lon2), by = .(gps_id, seg_id)]    

# time to next point
d[, datetime_ := anytime(datetime_, asUTC = TRUE, tz = 'America/Anchorage')]
d[, datetime_2 := shift(datetime_, type = 'lead'), by = .(gps_id, seg_id)]    
d[, time_next := difftime(datetime_2, datetime_, units = 'mins') %>% as.numeric] 

# exclude when GPS was in BARC
d[, datetime_15min := round_date(datetime_, '15 minutes')]

dss = d[BARC == TRUE, .N, by = .(datetime_15min, BARC, gps_id)]

d = merge(d, dss[, .(datetime_15min, gps_id, N_BARC = N)], by = c('datetime_15min', 'gps_id'), all.x = TRUE)
d[is.na(N_BARC), N_BARC := 0]
d[, BARC_5 := N_BARC > 4] 

#------------------------------------------------------------------------------------------------------------------------
# 2. Survey effort NARL study site
#------------------------------------------------------------------------------------------------------------------------

# subset relevant data
dAll = copy(d)
d = d[study_site == TRUE & BARC_5 == FALSE & BARC == FALSE]

# time on plot
d[, year_ := year(datetime_)]
d[, date_ := as.Date(as.POSIXct(as.character(datetime_)))]
d[, datetime_2 := shift(datetime_, type = 'lead'), by = .(gps_id, seg_id)]    
d[, time_next := difftime(datetime_2, datetime_, units = 'mins') %>% as.numeric] 
d[, time_next_15min := time_next < 15]

# exclude error
d = d[!(date_ == as.Date('2019-07-04') & gps_id == 5)] # error

# distance by 15 min
d[, dist_15 := sum(distance, na.rm = TRUE), by = .(gps_id, datetime_15min)]
d = d[dist_15 > 0]

# get bouts of data 
d[, bout := bCounter(time_next_15min), by = .(year_, gps_id)]

# by bout
d[, bout_start := min(datetime_), by = .(year_, date_, gps_id, bout)]
d[, bout_end := max(datetime_), by = .(year_, date_, gps_id, bout)]
d[, time_bout := difftime(bout_end, bout_start, units = 'hours') %>% as.numeric, by = .(year_, date_, gps_id, bout)]

# unique by day
du = unique(d, by = c('year_', 'date_', 'gps_id', 'bout'))

# sum for each day and gps
ds = du[, .(sum_time_bout = sum(time_bout)), by = .(year_, date_, gps_id)]
ds[sum_time_bout > 10]


ggplot(data = ds[year_ == 2018]) +
  geom_point(aes(date_, sum_time_bout, color = as.character(gps_id))) +
  geom_smooth(aes(date_, sum_time_bout))

ggplot(data = ds[year_ == 2019]) +
  geom_point(aes(date_, sum_time_bout, color = as.character(gps_id))) +
  geom_smooth(aes(date_, sum_time_bout))

ds = du[, .(sum_time_bout = sum(time_bout)), by = .(year_, date_)]
ds[, date_y := as.Date(format(date_, format = '%m-%d'), format = '%m-%d')]

ggplot(data = ds) +
  geom_point(aes(date_y, sum_time_bout, color = as.character(year_))) +
  geom_smooth(aes(date_y, sum_time_bout, color = as.character(year_))) +
  theme_classic(base_size = 18)

dse = copy(ds)

ggplot(data = dse) +
  geom_point(aes(date_y, sum_time_bout, color = as.character(year_))) +
  geom_smooth(aes(date_y, sum_time_bout, color = as.character(year_))) +
  theme_classic(base_size = 18)

#------------------------------------------------------------------------------------------------------------------------
# 3. Compare with observations
#------------------------------------------------------------------------------------------------------------------------

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW') 
d  = dbq(con, 'select * FROM RESIGHTINGS')
dc = dbq(con, 'select * FROM CAPTURES')
dn = dbq(con, 'select * FROM NESTS')
dp = dbq(con, 'select * FROM PATERNITY')
DBI::dbDisconnect(con)

# Change projection
ds = dn[is.na(lon)] # separate data without position
dn = dn[!is.na(lon)]
st_transform_DT(dn)
st_transform_DT(d)


# Assign locations in the study area 
point_over_poly_DT(d, poly = study_site, buffer = 10)
setnames(d, 'poly_overlap', 'study_site')

point_over_poly_DT(dn, poly = study_site, buffer = 10)
setnames(dn, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := NA]
dn = rbind(dn, ds)

# datetime
d[, datetime_ := as.POSIXct(datetime_)]
d[, datetime_y := as.POSIXct(format(datetime_, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, year_ := year(datetime_)]
dn[, initiation := as.POSIXct(initiation)]

# ID_year
d[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]

# unique observations
d[, date_ := as.POSIXct(format(datetime_, format = '%y-%m-%d'), format = '%y-%m-%d')]
du = unique(d[!is.na(ID)], by = c('date_', 'ID_year'))
du = du[!is.na(ID)]

ds = du[, .(sum_obs = .N), by = .(year_, date_)]
ds[, date_y := as.Date(format(date_, format = '%m-%d'), format = '%m-%d')]

ggplot(data = ds) +
  geom_point(aes(date_y, sum_obs, color = as.character(year_))) +
  geom_smooth(aes(date_y, sum_obs, color = as.character(year_))) +
  theme_classic(base_size = 18)

ggplot() +
  geom_point(data = ds, aes(date_y, sum_obs, color = as.character(year_))) +
  geom_smooth(data = ds, aes(date_y, sum_obs, color = as.character(year_))) +
  geom_point(data = dse, aes(date_y, sum_time_bout, color = as.character(year_))) +
  geom_smooth(data = dse, aes(date_y, sum_time_bout, color = as.character(year_))) +
  theme_classic(base_size = 18)

ggplot() +
  geom_point(data = ds, aes(date_y, sum_obs), color = 'red') +
  geom_smooth(data = ds, aes(date_y, sum_obs), color = 'red') +
  geom_point(data = dse, aes(date_y, sum_time_bout), color = 'black') +
  geom_smooth(data = dse, aes(date_y, sum_time_bout), color = 'black') +
  facet_grid(.~year_) +
  theme_classic(base_size = 18)

# ggsave('./REPORTS/FIGURES/working_effort_vs_observations.tiff', plot = last_plot(),  width = 300, height = 177, units = c('mm'), dpi = 'print')


ds = du[, .(sum_obs = .N), by = .(year_, date_, sex)]
ds[, date_y := as.Date(format(date_, format = '%m-%d'), format = '%m-%d')]


ggplot() +
  geom_point(data = ds[sex == 'M'], aes(date_y, sum_obs), color = 'blue') +
  geom_smooth(data = ds[sex == 'M'], aes(date_y, sum_obs), color = 'blue') +
  geom_point(data = ds[sex == 'F'], aes(date_y, sum_obs), color = 'red') +
  geom_smooth(data = ds[sex == 'F'], aes(date_y, sum_obs), color = 'red') +
  geom_point(data = dse, aes(date_y, sum_time_bout), color = 'black') +
  geom_smooth(data = dse, aes(date_y, sum_time_bout), color = 'black') +
  geom_vline(xintercept = as.Date(format(as.Date('2019-06-28'), format = '%m-%d'), format = '%m-%d')) +
  geom_hline(yintercept = 35) +
  facet_grid(.~year_) +
  theme_classic(base_size = 18)

# ggsave('./REPORTS/FIGURES/working_effort_vs_observations.tiff', plot = last_plot(),  width = 300, height = 177, units = c('mm'), dpi = 'print')

