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
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

# subset years relevant for this study 
d = d[year_ %in% c(2003:2006, 2014, 2017:2019)]

# Change projection
d[, lat_dec := lat]
d[, lon_dec := lon]

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
d[comments %like% 'clutch_removal_exp', data_type := 'clutch_removal_exp']
d = d[!is.na(data_type)]

# any without metal band?
d[is.na(ID)]

# exclude chick
d = d[!is.na(sex_observed)]

# assign first capture
d[, caught_time := as.POSIXct(caught_time)]
setorder(d, year_, caught_time)
d[, capture_id := seq_len(.N), by = ID]
d[, .N, capture_id]

# subset only adults that were genotyped from long-term monitoring data
dg[, genotyped := TRUE]
d[, ID := as.character(ID)]
d = merge(d, dg[, .(ID, genotyped, sex)], by = 'ID', all.x = TRUE)
d[is.na(genotyped), genotyped := FALSE]
d = d[!(genotyped == FALSE & external == 1)]

# check data
bm = create_bm(d[study_site == TRUE], buffer = 500)
bm +
  geom_point(data = d, aes(lon, lat, color = study_site))

bm = create_bm(d, buffer = 500)
bm +
  geom_point(data = d, aes(lon, lat, color = data_type))

# subset data relevant for this study
d = d[, .(external, data_type, year_, ID, UL, UR, LL, LR, sex, lat = lat_dec, lon = lon_dec, caught_time, dead)]

# save data
# write.table(d, './DATA/CAPTURES.txt', quote = TRUE, sep = '\t', row.names = FALSE)

#------------------------------------------------------------------------------------------------------------------------
# 2. Nests
#------------------------------------------------------------------------------------------------------------------------

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')
dp = dbq(con, 'select * FROM PATERNITY')
DBI::dbDisconnect(con)

# subset years relevant for this study 
d = d[year_ %in% c(2003:2006, 2014, 2017:2019)]

# Change projection
d[, lat_dec := lat]
d[, lon_dec := lon]

ds = d[is.na(lon)] # seperate data without position
d = d[!is.na(lon)]
st_transform_DT(d)

point_over_poly_DT(d, poly = study_site, buffer = 0)
setnames(d, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := FALSE]
d[male_id == 175189876, study_site := FALSE] # exclude one male from 2006 (not intensive study)
d = rbind(d, ds)

# nestID
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dp[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# nests with paternity data
dp = dp[!is.na(EPY)]
# EPY father 
dp[EPY == 1, .N, by = nestID] # more than 1 EPY father? No
dp[EPY == 1, EPY_father := IDfather]
dps = dp[, .(EPY = sum(EPY, na.rm = TRUE), N = .N, EPY_father = min(EPY_father, na.rm = TRUE)), by = .(nestID, year_)]
dps[, anyEPY := ifelse(EPY > 0, 1, 0)]
dps[is.infinite(EPY_father), EPY_father := NA]
dps[!is.na(EPY_father)] %>% nrow # known EPY fathers

# merge with nests
d = merge(d, dps[, .(nestID, N_parentage = N, N_EPY = EPY, anyEPY, EPY_father)], by = 'nestID', all.x = TRUE)

# assign all nests with parentage data
d[, parentage := ifelse(!is.na(N_parentage), TRUE, FALSE)]
d[, any_parentage_year := any(parentage == TRUE), by = year_]

# data sets and data available
d[study_site == TRUE, data_type := 'study_site']
d[parentage == TRUE & study_site == FALSE & external == 0, data_type := 'own_off_site']

# plots with parentage data, total nests in years with data
plot_R = d[parentage == TRUE & study_site == FALSE & external == 1 & plot %like% 'brw']$plot %>% unique
year_R = d[parentage == TRUE & study_site == FALSE & external == 1 & plot %like% 'brw']$year_ %>% unique
d[plot %in% plot_R & year_ %in% year_R, data_type := 'survey_plot']
d[parentage == TRUE & external == 1 & !(plot %like% 'brw') & year_ %in% year_R, data_type := 'clutch_removal_exp']

d[, .N, data_type]

# check data
bm = create_bm(d[study_site == TRUE], buffer = 500)
bm +
  geom_point(data = d, aes(lon, lat, color = study_site))

bm = create_bm(d, buffer = 500)
bm +
  geom_point(data = d, aes(lon, lat, color = data_type))








# subset data relevant for this study
d = d[, .(external, data_type, year_, nestID, male_id, female_id, male_assigned, male_field, female_assigned, 
          female_field, found_datetime, collected_datetime, last_checked_datetime, lat = lat_dec, lon = lon_dec, anyEPY, N_parentage, N_EPY)]

# save data
write.table(d, './DATA/NESTS.txt', quote = TRUE, sep = '\t', row.names = FALSE)



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
write.table(d, './DATA/PATERNITY.txt', quote = TRUE, sep = '\t', row.names = FALSE)



