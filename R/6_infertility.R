#========================================================================================================================
# Paternity analysis
#========================================================================================================================

# Summary
# 0. Prepare data for analysis
# 1. Captures on and off plot

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak', 'arm'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')
de = dbq(con, 'select * FROM EGGS')
dp = dbq(con, 'select * FROM PATERNITY')
dms = dbq(con, 'select * FROM MICROSATS')
DBI::dbDisconnect(con)

#------------------------------------------------------------------------------------------------------------------------
# 0. Prepare data for analysis
#------------------------------------------------------------------------------------------------------------------------

# Change projection
ds = d[is.na(lon)] # seperate data without position
d = d[!is.na(lon)]
st_transform_DT(d)

# Assign locations in the study area 
point_over_poly_DT(d, poly = study_site, buffer = 10)
setnames(d, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := FALSE]
d[male_id == 175189876, study_site := FALSE] # exclude one male from 2006
d = rbind(d, ds)

# check data
# bm = create_bm(d[study_site == TRUE], buffer = 500)
# bm +
#   geom_point(data = d, aes(lon, lat, color = study_site))

# nestID
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
de[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# assign datetime format
d[, found_datetime := as.POSIXct(found_datetime)]
d[, collected_datetime := as.POSIXct(collected_datetime)]
d[, initiation := as.POSIXct(initiation)]
d[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, initiation_doy := yday(initiation)]
d[, est_hatching_datetime := as.POSIXct(est_hatching_datetime)]
d[, hatching_datetime := as.POSIXct(hatching_datetime)]
d[, nest_state_date := as.POSIXct(nest_state_date)]
setorder(d, year_, initiation)
d[, YEAR_ := factor(year_)]

#------------------------------------------------------------------------------------------------------------------------
# 1. Infertility in eggs
#------------------------------------------------------------------------------------------------------------------------

# check homozygosity of undeveloped eggs 
de[undeveloped == 1]$ID

# subset undeveloped eggs
dm = dms[ID %in% de[undeveloped == 1]$ID]
de = de[ID %in% de[undeveloped == 1]$ID]

# merge with father microsats
de[, .(ID, nestID)]
unique(de[, .(ID, nestID)], by = 'ID')

dm = merge(dm, unique(de[, .(ID, nestID)], by = 'ID'), by = 'ID', all.x = TRUE)
dm = merge(dm, dp[, .(IDchick, IDmother, IDfather)], by.x  = 'ID', by.y = 'IDchick', all.x = TRUE)
dm[, IDfather := as.character(IDfather)]
dm = merge(dm, unique(dms[, .(ID, marker, af = a, bf = b)], by = 'ID'), by.x  = c('IDfather', 'marker'), by.y = c('ID', 'marker'), all.x = TRUE)

# what could have come from the father?
dm[, a_pot_father := a == af | a == bf]
dm[, b_pot_father := b == af | b == bf]

dm[, pot_father := a_pot_father == TRUE | a_pot_father == TRUE]

dm[, homozygot := a == b]

dp[IDchick == 'R207_3_19']


dm[ID == 'R404_3_19']

ds = dm[!is.na(homozygot), .N, .(ID, homozygot)]
setorder(ds, homozygot)

dm[is.na(a) & is.na(b), .N, by = ID]

ds = dm[, .N, .(ID, pot_father)]

setorder(ds, pot_father)
ds


