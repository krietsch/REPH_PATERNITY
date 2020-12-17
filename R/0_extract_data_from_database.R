#========================================================================================================================
# Extract all for the parentage study relevant data & prepare them for analysis
#========================================================================================================================

# Summary
# 0. Prepare data for analysis

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'sf', 'auksRuak', 'ggplot2'),
        function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE)))

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

#------------------------------------------------------------------------------------------------------------------------
# 1. Captures
#------------------------------------------------------------------------------------------------------------------------

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM CAPTURES')
DBI::dbDisconnect(con)

# Change projection
ds = d[is.na(lon)] # seperate data without position
d = d[!is.na(lon)]
st_transform_DT(d)

point_over_poly_DT(d, poly = study_site, buffer = 60) # buffer including birds followed flying off plot
setnames(d, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := FALSE]
d = rbind(d, ds)

# exclude birds caught before intensive study in this site
d[year_ < 2017, study_site := FALSE]

# assign data type
d[study_site == TRUE, data_type := 'study_site']
d[study_site == FALSE & external == 0, data_type := 'own_off_site']
d[external == 1, data_type := 'survey_plot']


# check data
bm = create_bm(d[study_site == TRUE], buffer = 500)
bm +
  geom_point(data = d, aes(lon, lat, color = study_site))

# any without metal band?
d[is.na(ID)]

# exclude chick
d = d[!is.na(sex_observed)]

# assign first capture
d[, caught_time := as.POSIXct(caught_time)]
setorder(d, year_, caught_time)
d[, capture_id := seq_len(.N), by = ID]
d[, .N, capture_id]

# banded each year on and off plot by us
ds = d[external == 0 & capture_id == 1 & year_ > 2016]

# subset years relevant for this study 
d = d[year_ %in% c(2003:2006, 2014, 2017:2019)]




d = d[, .(external, data_type, year_, ID, UL, UR, LL, LR, sex_observed, lat, lon, caught_time, genotyped)]


#------------------------------------------------------------------------------------------------------------------------
# 2. Nests
#------------------------------------------------------------------------------------------------------------------------





#------------------------------------------------------------------------------------------------------------------------
# 3. Behavioural observations
#------------------------------------------------------------------------------------------------------------------------





#------------------------------------------------------------------------------------------------------------------------
# 4. Paternity
#------------------------------------------------------------------------------------------------------------------------

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM PATERNITY')
DBI::dbDisconnect(con)

# nestID
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# subset nests with paternity data for at least one offspring
d[!is.na(EPY), parentage := 1]
d[, offspring_sampled := sum(parentage, na.rm = TRUE), by = nestID]
d = d[offspring_sampled > 0]

# select columns of interest
d = d[, .(year_, nestID, IDchick, IDmother, IDfather, EPY, comment)]

# save data
saveRDS(d, './DATA/PATERNITY.rds')



