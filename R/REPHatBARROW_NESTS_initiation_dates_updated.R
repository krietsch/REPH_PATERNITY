#=========================================================================================================================
# REPHatBARROW NESTS correct incubation period to 19 days
#=========================================================================================================================

### Summary
# 1. Correct external data 
# 2. Correct our data

# settings, con
options(stringsAsFactors = FALSE)
sapply( c('data.table', 'sdb','readxl','foreach', 'xlsx', 'anytime', 'magrittr', 'DBI'),
        require, character.only = TRUE)

con = dbcon('jkrietsch', db = 'REPHatBARROW')  


#-------------------------------------------------------------------------------------------------------------------------
# 1. Correct external data 
#-------------------------------------------------------------------------------------------------------------------------

# External data
d = read.xlsx('/ds/raw_data_kemp/FIELD/Barrow/2019/DATA/RAW_DATA/EXTERNAL_DATA/REPH_barrow_nests_2003_2019_updated_2021_1_20.xlsx',
              sheetName = 'Sheet1') %>% as.data.table

d = d[!is.na(year)]

# replace all '-' & '.' with NA
for (col in d[, names(.SD)[lapply(.SD, class) %in% c("character", "factor")]]) {
  set(d, d[get(col) %in% c('-', '.'), which = TRUE], col, NA_character_)
}

d = d[!is.na(year)]
names(d)

d[, nest := toupper(nestID)]

setnames(d, c('year',  'm_band',  'f_band',    'lat_corrected', 'long_corrected', 'number_egg_find',   'max_egg_number'), 
         c('year_', 'male_id', 'female_id', 'lat',           'lon',            'initial_clutch_size', 'clutch_size') )

setnames(d, c('last_day_checked',      'hatch_date',        'initiation_date', 'initiation_date_corrected', 'problem'),
         c('last_checked_datetime', 'hatching_datetime', 'inidate',         'initiation',                'comments'))

# transform excel time
require(openxlsx)
d[, date_found := convertToDateTime(date_found, origin = "1900-01-01")]
d[, est_hatch_date := convertToDateTime(est_hatch_date, origin = "1900-01-01")]
d[, last_checked_datetime := convertToDateTime(last_checked_datetime, origin = "1900-01-01")]
d[, hatching_datetime := convertToDateTime(hatching_datetime, origin = "1900-01-01")]
d[, inidate := convertToDateTime(inidate, origin = "1900-01-01")]
d[, initiation := convertToDateTime(initiation, origin = "1900-01-01")]
d[, initiation := as.POSIXct(initiation) + 3600*12]

# merge data
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# load nest data
dn = dbq(con, "select * FROM NESTS")
dn[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

dn = merge(dn, d[, .(nestID, initiation_updated = as.character(initiation))], by = 'nestID', all.x = TRUE)

# subset external data 
dn = dn[external == 1 & !is.na(initiation_updated)]

# check where things changed
dn = dn[!is.na(initiation_updated) & initiation != initiation_updated]
dn[, diff := difftime(initiation, initiation_updated, units = 'days') %>% as.numeric]
dn[, .N, diff]

# update db
dt = dn[, .(pk, initiation_updated = as.character(initiation_updated))]

# save new values from d in a temp table
dbWriteTable(con, 'temp', dt , row.names = FALSE)

# update target table based on values in temp table
dbExecute(con, "update NESTS n, temp t set n.initiation = t.initiation_updated where n.pk = t.pk")
dbExecute(con,"drop table temp")

#-------------------------------------------------------------------------------------------------------------------------
# 2. Correct our data
#-------------------------------------------------------------------------------------------------------------------------

d = dbq(con, "select * FROM NESTS")
d = d[external == 0]
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

d[nestID == 'R231_19']

# calculate incubation period based on known initiation and hatching
d[, found_incomplete := ifelse(initial_clutch_size < clutch_size, TRUE, FALSE)]
d[is.na(found_incomplete), found_incomplete := FALSE]

# calculate incubation period
d[, initiation := as.POSIXct(initiation)]
d[, start_incubation := initiation + clutch_size*3600*24 - 3600*24]
d[is.na(hatching_datetime), based_on_est_hdt := TRUE]
d[is.na(hatching_datetime), hatching_datetime := est_hatching_datetime]
d[, incubation_period := difftime(hatching_datetime, start_incubation, units = 'days') %>% as.numeric]

hist(d$incubation_period)
hist(d[is.na(collected_datetime)]$incubation_period)

d[is.na(collected_datetime) & incubation_period  > 19, .(nestID, initial_clutch_size, clutch_size, found_incomplete, initiation, incubation_period)]


# know initiation
ds = d[is.na(collected_datetime) & !is.na(initiation), .(nestID, plot, initial_clutch_size, clutch_size, start_incubation, found_incomplete, initiation, 
                                                         est_hatching_datetime, hatching_datetime, incubation_period, pk)]

ds[, est_hatching_datetime := as.POSIXct(est_hatching_datetime)]
ds[found_incomplete == TRUE, est_hatching_datetime := initiation + 3600*24*19 + clutch_size*3600*24 - 3600*24]
ds[, start_incubation := initiation + clutch_size*3600*24 - 3600*24]
ds[, incubation_period := difftime(est_hatching_datetime, start_incubation, units = 'days') %>% as.numeric]
ds


# update db
dt = ds[found_incomplete == TRUE, .(pk, est_hatching_datetime_updated = as.character(est_hatching_datetime))]

# save new values from d in a temp table
dbWriteTable(con, 'temp', dt , row.names = FALSE)

# update target table based on values in temp table
dbExecute(con, "update NESTS n, temp t set n.est_hatching_datetime = t.est_hatching_datetime_updated where n.pk = t.pk")
dbExecute(con,"drop table temp")

# known hatching
ds = d[is.na(collected_datetime) & !is.na(initiation), .(nestID, plot, initial_clutch_size, clutch_size, start_incubation, found_incomplete, initiation, 
                                                         est_hatching_datetime, hatching_datetime, incubation_period, pk)]
ds = ds[incubation_period == 17.2]
ds[, hatching_datetime := as.POSIXct(hatching_datetime)]
ds[, initiation := hatching_datetime - 3600*24*19 - clutch_size*3600*24 + 3600*24]
ds[, start_incubation := initiation + clutch_size*3600*24 - 3600*24]
ds[, incubation_period := difftime(hatching_datetime, start_incubation, units = 'days') %>% as.numeric]
ds

# update db
dt = ds[, .(pk, initiation_updated = as.character(initiation))]

# save new values from d in a temp table
dbWriteTable(con, 'temp', dt , row.names = FALSE)

# update target table based on values in temp table
dbExecute(con, "update NESTS n, temp t set n.initiation = t.initiation_updated where n.pk = t.pk")
dbExecute(con,"drop table temp")


