#========================================================================================================================
# Paternity analysis
#========================================================================================================================

# Summary
# 0. Prepare data for analysis
# 1. Captures on and off plot
# 2. Incubation length & initiation date method
# 3. Rate of polyandry & renesting on plot
# 4. Paternity data
# 5. Paternity between study site and external
# 6. Paternity between years 
# 7. Paternity polyandrous clutches & renesting 
# 8. Timing of second and third clutches
# 9. Paternity frequency within the season 

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak', 'patchwork', 'multcomp', 'viridis', 'car'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')
dc = dbq(con, 'select * FROM CAPTURES')
dp = dbq(con, 'select * FROM PATERNITY')
dg = dbq(con, 'select * FROM SEX')
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
dp[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

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

# nests with paternity data
dp = dp[!is.na(EPY)]
# EPY father 
dp[EPY == 1, .N, by = nestID] # more than 1 EPY father? No
dp[EPY == 1, EPY_father := IDfather]
dps = dp[, .(EPY = sum(EPY), N = .N, EPY_father = min(EPY_father, na.rm = TRUE)), by = .(nestID, year_)]
dps[, anyEPY := ifelse(EPY > 0, 1, 0)]
dps[is.infinite(EPY_father), EPY_father := NA]
dps[!is.na(EPY_father)] %>% nrow # known EPY fathers

# merge with nests
d = merge(d, dps[, .(nestID, N_parentage = N, N_EPY = EPY, anyEPY, EPY_father)], by = 'nestID', all.x = TRUE)

# assign all nests with parentage data
d[, parentage := ifelse(!is.na(N_parentage), TRUE, FALSE)]
d[, any_parentage_year := any(parentage == TRUE), by = year_]

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

# exclude birds caught before intensive study in this site
dc[year_ < 2017, study_site := FALSE]

# check data
# bm = create_bm(dc[study_site == TRUE], buffer = 500)
# bm +
#   geom_point(data = dc, aes(lon, lat, color = study_site))

# any without metal band?
dc[is.na(ID)]

# assign first capture
dc[, caught_time := as.POSIXct(caught_time)]
setorder(dc, year_, caught_time)
dc[, capture_id := seq_len(.N), by = ID]
dc[, .N, capture_id]

# banded each year on and off plot by us
ds = dc[external == 0 & capture_id == 1 & year_ > 2016]

ds[, .N, .(year_)]
ds[, .N, .(year_, study_site)]

# capture after nest found?
dc_first = dc[, .(first_caught = min(caught_time, na.rm = TRUE)), by = ID]
ds = merge(d, dc_first, by.x = 'male_id', by.y = 'ID', all.x = TRUE)
ds[, diff_found_caught := difftime(found_datetime, first_caught, units = 'hours') %>% as.numeric]
ds = ds[study_site == TRUE]
ds[, found_before_caught := diff_found_caught < 0]
ds[, .N, found_before_caught]

# How many birds were genotyped each year?
dg[, genotyped := TRUE]

dc[, ID := as.character(ID)]
dc = merge(dc, dg[, .(ID, genotyped)], by = 'ID', all.x = TRUE)

dc = dc[genotyped == TRUE]
dc = dc[!is.na(sex_observed)] # exclude chicks
dc = dc[year_ %in% c(2003:2006, 2014, 2017:2019)]

# assign data type
dc[study_site == TRUE, data_type := 'study_site']
dc[study_site == FALSE & external == 0, data_type := 'own_off_site']
dc[external == 1, data_type := 'survey_plot']


dcs = dc[, .N, by = .(year_, sex_observed, data_type)]

dcs_m = dcs[sex_observed == 'M']
dcs_f = dcs[sex_observed == 'F']

dcs = merge(dcs_m[, .(year_, data_type, N_males = N)], dcs_f[, .(year_, data_type, N_females = N)], by = c('year_', 'data_type'), all.x = TRUE)

# 33 males caught in renesting experiment 2018 and 20 females
dcs[year_ == 2018 & data_type == 'survey_plot', N_males := N_males - 33]
dcs[year_ == 2018 & data_type == 'survey_plot', N_females := N_females - 20]
dcs = rbind(dcs, data.table(year_ = 2018, data_type = 'clutch_removal_exp', N_males = 33, N_females = 20))
dcs[is.na(N_females), N_females := 0]

setorder(dcs, -year_, data_type)
dcs






#------------------------------------------------------------------------------------------------------------------------
# 4. Paternity data
#------------------------------------------------------------------------------------------------------------------------

# data sets and data available
d[study_site == TRUE, data_type := 'study_site']
d[parentage == TRUE & study_site == FALSE & external == 0, data_type := 'own_off_site']

# plots with parentage data, total nests in years with data
plot_R = d[parentage == TRUE & study_site == FALSE & external == 1 & plot %like% 'brw']$plot %>% unique
year_R = d[parentage == TRUE & study_site == FALSE & external == 1 & plot %like% 'brw']$year_ %>% unique
d[plot %in% plot_R & year_ %in% year_R, data_type := 'survey_plot']
d[parentage == TRUE & external == 1 & !(plot %like% 'brw') & year_ %in% year_R, data_type := 'clutch_removal_exp']

ds = d[!is.na(data_type), .(N_nests = .N), by =  data_type]
ds2 = d[parentage == TRUE, .(N_parentage = .N), by =  data_type]
ds = merge(ds, ds2, by = c('data_type'), all.x = TRUE)
ds[, N_parentage := paste0(round(N_parentage / N_nests * 100, 0), '% (', N_parentage, '/', N_nests, ')')]
ds

# position of nests with parentage data
# bm = create_bm(d)
# bm + geom_point(data = d[parentage == TRUE], aes(lon, lat, color = study_site))
# bm + geom_point(data = d[parentage == TRUE & !is.na(data_type)], aes(lon, lat, color = data_type))
# bm + geom_point(data = d[parentage == TRUE], aes(lon, lat, color = YEAR_))
# bm + geom_point(data = d[parentage == TRUE], aes(lon, lat, color = as.character(anyEPY)))

# split in intense study and other data types
ds = d[, .(N_nests = .N), by = .(year_, data_type)]
ds2 = d[parentage == TRUE, .(N_parentage = .N), by = .(year_, data_type)]
ds3 = d[anyEPY == TRUE, .(N_EPY = .N), by = .(year_, data_type)]
ds4 = d[, .(N_eggs = sum(N_parentage, na.rm = TRUE), N_eggs_EPY = sum(N_EPY, na.rm = TRUE)), by = .(year_, data_type)]
ds = merge(ds, ds2, by = c('year_', 'data_type'), all.x = TRUE)
ds = merge(ds, ds3, by = c('year_', 'data_type'), all.x = TRUE)
ds = merge(ds, ds4, by = c('year_', 'data_type'), all.x = TRUE)
ds[is.na(ds)] = 0
ds = ds[N_parentage != 0]
ds[, EPY_nests_per := round(N_EPY / N_parentage * 100, 1)]
ds[, EPY_eggs_per  := round(N_eggs_EPY / N_eggs * 100, 1)]
ds[, EPY_nests_N   := paste0(N_EPY, '/', N_parentage)]
ds[, EPY_eggs_N    := paste0(N_eggs_EPY, '/', N_eggs)]

# merge with adults 
ds = merge(ds, dcs, by = c('year_', 'data_type'), all.x = TRUE)


# subset and rename table
ds[data_type == 'study_site', data_type := 'intensive study']
ds[data_type == 'own_off_site', data_type := 'outside plot']
ds[data_type == 'survey_plot', data_type := 'long-term monitoring']
ds[data_type == 'clutch_removal_exp', data_type := 'renesting experiment']

ds = ds[, .(Year = year_, `Data type` = data_type, `% EPY` = EPY_eggs_per, `N EPY` = EPY_eggs_N, `% nests with EPY` = EPY_nests_per, 
            `N nests with EPY` = EPY_nests_N, `N males genotyped` = N_males, `N females genotyped` = N_females)]
setorder(ds,  -Year, `Data type`)
ds

# openxlsx::write.xlsx(ds, './REPORTS/EPY_frequency/EPY_summary_table_all_data_types.xlsx')
