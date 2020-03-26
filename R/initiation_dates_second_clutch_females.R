#========================================================================================================================
# Females second clutch initiation date adjusted 
#========================================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')
DBI::dbDisconnect(con)

# bring everything in the right format
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
d[, initiation := as.POSIXct(initiation)]
d[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, initiation_doy := yday(initiation)]
d[, found_datetime := as.POSIXct(found_datetime)]
d[, collected_datetime := as.POSIXct(collected_datetime)]
d[, last_checked_datetime := as.POSIXct(last_checked_datetime)]
d[, hatching_datetime := as.POSIXct(hatching_datetime)]
d[, nest_state_date := as.POSIXct(nest_state_date)]
d[, YEAR_ := factor(year_)]
d[, complete := initiation + clutch_size * 86400 - 86400]
d[, complete_y := initiation_y + clutch_size * 86400 - 86400]

setorder(d, year_, initiation)

# general check for datetimes mistakes
d[, diff_initiation_nest_state := difftime(nest_state_date, initiation, units = 'days') %>% as.numeric]
hist(d$diff_initiation_nest_state)
d[diff_initiation_nest_state < 0, .(nestID, initiation, nest_state, diff_initiation_nest_state)]

# first and second clutches by females
d[, N_female_clutch := .N, by = female_id]
d[, female_clutch := seq_len(.N), by = .(female_id, year_)]
d[is.na(female_id), female_clutch := 1]
d[!is.na(female_id), N_female_clutch := max(female_clutch), by = .(female_id, year_)]
d[is.na(female_id), N_female_clutch := 1]
d[, .N, by = .(year_, female_clutch)]
d[, .N, by = .(female_clutch, external)]

# females
ID2c = d[female_clutch == 2]$female_id
dx = d[female_id %in% ID2c & female_clutch < 3]
dx[, .N, by = .(female_clutch)]
dx[, female_clutch := as.factor(female_clutch)]

# renesting with same female?
dr = merge(dx[female_clutch == 1, .(year_, nestID1 = nestID, initiation1 = initiation, clutch_size1 = clutch_size, complete, female_id, m1 = male_id)], 
           dx[female_clutch == 2, .(year_, nestID2 = nestID, initiation2 = initiation, clutch_size2 = clutch_size, female_id, m2 = male_id)], 
           by = c('year_', 'female_id'), all = TRUE)

dr[, same_male := m1 == m2]
setorder(dr, female_id)
dr

dr[, laying_1_2 := difftime(initiation2, complete, units = 'days') ]
dr




