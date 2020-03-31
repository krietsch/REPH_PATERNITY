#========================================================================================================================
# Paternity analysis
#========================================================================================================================

# Summary
# 0. Prepare data for analysis


# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'viridis'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')
dp = dbq(con, 'select * FROM PATERNITY')
dw = dbq(con, 'select * FROM SNOW_SURVEY')
DBI::dbDisconnect(con)

# Change projection
ds = d[is.na(lon)] # seperate data without position
d = d[!is.na(lon)]
st_transform_DT(d)

#------------------------------------------------------------------------------------------------------------------------
# 0. Prepare data for analysis
#------------------------------------------------------------------------------------------------------------------------

# Assign locations in the study area 
point_over_poly_DT(d, poly = study_site, buffer = 10)
setnames(d, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := NA]
d = rbind(d, ds)

# check data
# ggplot() + 
#   geom_sf(data = study_site, fill = 'grey95') +
#   geom_point(data = d, aes(lon, lat, color = study_site))


# bring everything in the right format
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
d[, initiation := as.POSIXct(initiation)]
d[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, initiation_doy := yday(initiation)]
d[, YEAR_ := factor(year_)]
d[, complete := initiation + clutch_size * 86400 - 86400]
d[, complete_y := initiation_y + clutch_size * 86400 - 86400]

setorder(d, year_, initiation)
d[, female_id := factor(female_id, levels = unique(female_id[order(initiation)]))]
d[, male_id := factor(male_id, levels = unique(male_id[order(initiation)]))]
d[, nestID := factor(nestID, levels = unique(nestID[order(initiation)]))]

# add parentage data
dp[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dp = dp[!is.na(EPY)]

# assign external data
dp[nestID %in% d[external == 0]$nestID, external := 0]
dp[is.na(external), external := 1]

# EPY father 
dp[EPY == 1, .N, by = nestID] # more than 1 EPY father known? No
dp[EPY == 1, EPY_father := IDfather]

# nests with data
dps = dp[, .(EPY = sum(EPY), N = .N, EPY_father = min(EPY_father, na.rm = TRUE)), by = .(nestID, year_, external)]
dps[, anyEPY := ifelse(EPY > 0, 1, 0)]
dps[is.infinite(EPY_father), EPY_father := NA]
dps[!is.na(EPY_father)] %>% nrow # known EPY fathers

# merge with nests
d = merge(d, dps[, .(nestID, N_EPY = N, Eggs_EPY = EPY, anyEPY, EPY_father)], by = 'nestID', all.x = TRUE)

# N eggs with and without EPP
d[, Eggs_no_EPY := N_EPY - Eggs_EPY]

# assign all nests with parentage data
d[, parentage := ifelse(!is.na(N_EPY), TRUE, FALSE)]
d[, any_parentage_year := any(parentage == TRUE), by = year_]

# assign nests on plot (Rick's plot)
d[external == 1, study_site := ifelse(plot %like% 'brw', TRUE, FALSE)]
d[plot == 'brwfwl', study_site := FALSE]

#------------------------------------------------------------------------------------------------------------------------
# 1. Nests data avaiable
#------------------------------------------------------------------------------------------------------------------------

# exclude off-plot nests without parentage data
d = d[!(study_site == FALSE & parentage == FALSE)]

# our data
ds  = d[external == 0, .N, by = .(year_, study_site)]
ds2 = d[external == 0 & parentage == TRUE, .(N_parentage = .N), by = .(year_, study_site)]
ds3 = d[external == 0 & anyEPY == TRUE, .(N_EPY = .N), by = .(year_, study_site)]
ds = merge(ds, ds2, by = c('year_', 'study_site'), all.x = TRUE)
ds = merge(ds, ds3, by = c('year_', 'study_site'), all.x = TRUE)

setorder(ds, year_, -study_site)
ds

# Rick's data
ds =  d[external == 1, .N, by = .(year_, study_site)]
ds2 = d[external == 1 & parentage == TRUE, .(N_parentage = .N), by = .(year_, study_site)]
ds3 = d[external == 1 & anyEPY == TRUE, .(N_EPY = .N), by = .(year_, study_site)]
ds = merge(ds, ds2, by = c('year_', 'study_site'))
ds = merge(ds, ds3, by = c('year_', 'study_site'), all.x = TRUE)

setorder(ds, year_, -study_site)
ds

# total number of nests with parentage data and number with full clutch
ds = d[parentage == TRUE]
ds %>% nrow

# males assigned
ds[!is.na(male_id)] %>% nrow
ds[!is.na(female_id) & female_assigned != 3] %>% nrow
ds[!is.na(female_id) & female_assigned == 3] %>% nrow

# N nests full clutch sampled
ds[, full_clutch_sampled := clutch_size == N_EPY]
ds[, .N, full_clutch_sampled]
ds[full_clutch_sampled == TRUE]  %>% nrow
ds[full_clutch_sampled == TRUE]  %>% nrow / ds %>% nrow * 100 # percent of all
mean(ds$clutch_size, na.rm = TRUE)
sd(ds$clutch_size, na.rm = TRUE)
min(ds$clutch_size, na.rm = TRUE)
max(ds$clutch_size, na.rm = TRUE)

# N with EPY
ds[anyEPY == TRUE] %>% nrow
dp[EPY == 1] %>% unique(., by = 'nestID') %>% nrow
dp[EPY == 1 & !is.na(IDfather)] %>% nrow # fathers that could be assigned

dp[, N_EPY := sum(EPY, na.rm = TRUE), by = nestID]
dp[N_EPY > 1] %>% unique(., by = 'nestID') %>% nrow
dp[N_EPY > 2]

setorder(d, year_, initiation)

# first and second clutches by females
d[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]
d[, N_female_clutch := .N, by = female_id]
d[, female_clutch := seq_len(.N), by = female_id_year]
d[is.na(female_id), female_clutch := 1]
d[!is.na(female_id), N_female_clutch := max(female_clutch), by = female_id_year]
d[is.na(female_id), N_female_clutch := 1]
d[, .N, by = .(year_, female_clutch)]
d[, .N, by = .(female_clutch, external)]

# males renesting
d[, male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
d[, N_male_clutch := .N, by = male_id]
d[, male_clutch := seq_len(.N), by = male_id_year]
d[is.na(male_id), male_clutch := 1]
d[!is.na(male_id), N_male_clutch := max(male_clutch), by = male_id_year]
d[is.na(male_id), N_male_clutch := 1]
d[, .N, by = .(year_, male_clutch)]
d[, .N, by = .(male_clutch, external)]

# polyandrous clutches (second clutch with different partner)
ID2c = d[female_clutch == 2]$female_id_year
dx = d[female_id_year %in% ID2c]
dx[, .N, by = .(female_clutch, anyEPY)]
dx[, female_clutch := as.factor(female_clutch)]

dr = merge(dx[female_clutch == 1, .(year1 = year_, nestID1 = nestID, female_id_year, m1 = male_id, anyEPY1 = anyEPY, 
                                    mc1 = male_clutch, ss1 = study_site)], 
           dx[female_clutch == 2, .(year2 = year_, nestID2 = nestID, female_id_year, m2 = male_id, anyEPY2 = anyEPY, 
                                    mc2 = male_clutch, ss2 = study_site)], 
           by = 'female_id_year', all = TRUE)

dr[, same_male := m1 == m2]
dr[is.na(same_male), same_male := FALSE]
dr[, both_study_site := ss1 == ss2]
setorder(dr, female_id_year)
dr

# polyandrous females
dr = dr[same_male == FALSE, .(female_id_year, polyandrous = TRUE, polyandry_study_site = both_study_site)]
d = merge(d, dr, by = 'female_id_year', all.x = TRUE)

#------------------------------------------------------------------------------------------------------------------------
# 2. Extra-pair young by nest on plots
#------------------------------------------------------------------------------------------------------------------------





















