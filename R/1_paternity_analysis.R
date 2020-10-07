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
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak', 'patchwork', 'multcomp'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')
dc = dbq(con, 'select * FROM CAPTURES')
dp = dbq(con, 'select * FROM PATERNITY')
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

#------------------------------------------------------------------------------------------------------------------------
# 2. Incubation length & initiation date method
#------------------------------------------------------------------------------------------------------------------------

# incubation period in incubator
d[, found_incomplete := initial_clutch_size < clutch_size]
d[, inc_period := difftime(hatching_datetime, c(initiation + clutch_size * 3600*24 - 3600*24), units = 'days') %>% as.numeric]

ds = d[!is.na(inc_period) & found_incomplete == TRUE]
mean(ds[external == 0]$inc_period, na.rm = TRUE) # 17.2 days is based on eggs
nrow(ds[external == 0]) 

# not in incubator
mean(ds[external == 1]$inc_period, na.rm = TRUE)
nrow(ds[external == 1])
mean(ds[external == 1 & inc_period < 25]$inc_period, na.rm = TRUE) # excluding outliers

# plot
# ggplot(data = ds[external == 0]) +
#   geom_point(aes(y = inc_period, x = initiation_y, color = as.character(year_))) +
#   geom_smooth(aes(y = inc_period, x = initiation_y), method = 'lm')
# 
# ggplot(data = ds[external == 1 & inc_period < 25]) +
#   geom_point(aes(y = inc_period, x = initiation_y, color = as.character(year_))) +
#   geom_smooth(aes(y = inc_period, x = initiation_y), method = 'lm')
# 
# ggplot(data = ds[inc_period < 25]) +
#   geom_point(aes(y = inc_period, x = initiation_y, color = as.character(year_))) +
#   geom_smooth(aes(y = inc_period, x = initiation_y), method = 'lm')
# 
# # comparision natural vs. incubator
# ggplot(data = ds) +
#   geom_boxplot(aes(y = inc_period, x = as.character(external))) 
# 
# ggplot(data = ds[year_ > 2016]) +
#   geom_boxplot(aes(y = inc_period, x = as.character(external))) 

# initiation date methods
d[found_incomplete == TRUE, initiation_method := 'found_incomplete']
d[is.na(initiation_method) & !is.na(hatching_datetime), initiation_method := 'hatching_datetime']
d[is.na(initiation_method) & !is.na(est_hatching_datetime), initiation_method := 'est_hatching_datetime']
d[is.na(initiation_method) & !is.na(initiation), initiation_method := 'est_hatching_datetime']
d[is.na(initiation_method), initiation_method := 'none']

# check NA
d[study_site == TRUE & initiation_method == 'none', .(year_, nest, initiation_y)]

# d[is.na(initiation_method) & !is.na(initiation), initiation_method := 'found_incomplete']

ds = d[study_site == TRUE & parentage == TRUE, .N, initiation_method]
ds[, N_nests := nrow(d[study_site == TRUE & parentage == TRUE])]
ds[, initiation_method_percent := N / N_nests * 100]
ds

# age when found
ds = d[study_site == TRUE]
ds[, complete := initiation + clutch_size*86400]

ds[, age_found := difftime(found_datetime, initiation, units = 'days') %>% as.numeric]
ds[, age_found_complete := difftime(found_datetime, complete, units = 'days') %>% as.numeric]
ds[age_found < 5] %>% nrow/ 174
ds[age_found_complete < 3] %>%  nrow/ 174

#------------------------------------------------------------------------------------------------------------------------
# 3. Rate of polyandry & renesting
#------------------------------------------------------------------------------------------------------------------------

setorder(d, year_, initiation)

# first and second clutches by females
d[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]
d[, N_female_clutch := .N, by = female_id]
d[, female_clutch := seq_len(.N), by = .(female_id, year_)]
d[is.na(female_id), female_clutch := 1]
d[!is.na(female_id), N_female_clutch := max(female_clutch), by = .(female_id, year_)]
d[is.na(female_id), N_female_clutch := 1]
d[, .N, by = .(year_, female_clutch)]
d[, .N, by = .(female_clutch, external)]

# polyandrous clutches (second clutch with different partner)
ID2c = d[female_clutch == 2]$female_id_year
dx = d[female_id_year %in% ID2c]

dr = merge(dx[female_clutch == 1, .(year1 = year_, nestID1 = nestID, female_id_year, m1 = male_id, anyEPY1 = anyEPY, 
                                    ss1 = study_site, initiation1 = initiation)], 
           dx[female_clutch == 2, .(year2 = year_, nestID2 = nestID, female_id_year, m2 = male_id, anyEPY2 = anyEPY, 
                                    ss2 = study_site, initiation2 = initiation)],  
           by = 'female_id_year', all = TRUE)

dr[, same_male := m1 == m2]
dr[is.na(same_male), same_male := FALSE]
dr[, both_study_site := ss1 == ss2]
dr[, diff_initiation := difftime(initiation2, initiation1, units = 'days') %>% as.numeric]
setorder(dr, female_id_year)
dr

# polyandrous females
dr = dr[same_male == FALSE, .(female_id_year, polyandrous = TRUE, polyandry_study_site = both_study_site)]
d = merge(d, dr, by = 'female_id_year', all.x = TRUE)

setorder(d, year_, initiation)

# males renesting
d[, male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
d[, N_male_clutch := .N, by = male_id]
d[, male_clutch := seq_len(.N), by = male_id_year]
d[is.na(male_id), male_clutch := 1]
d[!is.na(male_id), N_male_clutch := max(male_clutch), by = male_id_year]
d[is.na(male_id), N_male_clutch := 1]
d[, .N, by = .(year_, male_clutch)]
d[, .N, by = .(male_clutch, external)]

# males renesting with same or different partner
ID2c = d[male_clutch == 2]$male_id_year
dx = d[male_id_year %in% ID2c]

dr = merge(dx[male_clutch == 1, .(year1 = year_, nestID1 = nestID, male_id_year, f1 = female_id, anyEPY1 = anyEPY, 
                                  mfc1 = male_clutch, ss1 = study_site, initiation1 = initiation)], 
           dx[male_clutch == 2, .(year2 = year_, nestID2 = nestID, male_id_year, f2 = female_id, anyEPY2 = anyEPY, 
                                  fc2 = male_clutch, ss2 = study_site, initiation2= initiation)],  
           by = 'male_id_year', all = TRUE)

dr[, same_female := f1 == f2]
dr[is.na(same_female), same_female := FALSE]
dr[, both_study_site := ss1 == ss2]
dr[, diff_initiation := difftime(initiation2, initiation1, units = 'days') %>% as.numeric]
setorder(dr, male_id_year)
dr

# renesting males
dr = dr[, .(male_id_year, renesting_male = TRUE, same_female, renesting_study_site = both_study_site)]
d = merge(d, dr, by = 'male_id_year', all.x = TRUE)

### unique ID's
d[is.na(male_id), male_id_year := NA]
d[, male_id_year_NA := male_id_year]
d[is.na(male_id), male_id_year_NA := paste0(seq_len(.N), '_', substr(year_, 3,4)), by = male_id_year_NA]

d[is.na(female_id), female_id_year := NA]
d[, female_id_year_NA := female_id_year]
d[is.na(female_id), female_id_year_NA := paste0(seq_len(.N), '_', substr(year_, 3,4)), by = female_id_year_NA]

# subset study site
ds = d[external == 0 & study_site == TRUE]
dss = ds[, .(N_nests = .N), by = year_]
setorder(dss, year_)

# unassigned males and females
ds[is.na(male_id)]
ds[is.na(female_id)]

# males
dsm = unique(ds, by = 'male_id_year_NA')
dsm[, .N, by = male_assigned]
dssm = dsm[, .(unique_males = .N), by = year_]

# renesting males
dsmr = dsm[renesting_male == TRUE, .(renesting_males = .N), by = year_]
dsmr_NARL = dsm[renesting_study_site == TRUE, .(renesting_males_NARL = .N), by = year_]

# females
dsf = unique(ds, by = 'female_id_year_NA')
dsf[, .N, by = female_assigned]
dssf = dsf[, .(unique_females = .N), by = year_]

# polyandrous females
dsfp = dsf[polyandrous == TRUE, .(polyandrous_females = .N), by = year_]
dsfp_NARL = dsf[polyandry_study_site == TRUE, .(polyandrous_females_NARL = .N), by = year_]

# combine data
dss = merge(dss, dssm, by = 'year_', all.x = TRUE)
dss = merge(dss, dsmr, by = 'year_', all.x = TRUE)
dss = merge(dss, dsmr_NARL, by = 'year_', all.x = TRUE)
dss = merge(dss, dssf, by = 'year_', all.x = TRUE)
dss = merge(dss, dsfp, by = 'year_', all.x = TRUE)
dss = merge(dss, dsfp_NARL, by = 'year_', all.x = TRUE)
dss[is.na(dss)] = 0

# percent renesting / polyandrous
dss[, renesting_males_per          := round(renesting_males / unique_males * 100, 1)]
dss[, renesting_males_NARL_per     := round(renesting_males_NARL / unique_males * 100, 1)]
dss[, polyandrous_females_per      := round(polyandrous_females / unique_females * 100, 1)]
dss[, polyandrous_females_NARL_per := round(polyandrous_females_NARL / unique_females * 100, 1)]

# add EPY on plot
dsp = ds[!is.na(N_parentage), .(N_parentage = .N), by = year_]
dspn = ds[!is.na(N_parentage) & anyEPY == TRUE, .(N_anyEPY = .N), by = year_]
dsp = merge(dsp, dspn, by = 'year_')
dsp[, EPY_nests_per := round(N_anyEPY / N_parentage * 100, 1)]
dsp[, EPY := paste0(EPY_nests_per, '% (', N_anyEPY, '/', N_parentage, ')')]
dss = merge(dss, dsp, by = 'year_')

# plot EPP, polyandry and renesting
ds = rbindlist(list(dss[, .(year_, x = EPY_nests_per, n = N_anyEPY, N = N_parentage, type = 'EPP', study_site = TRUE)],
                    dss[, .(year_, x = polyandrous_females_NARL_per, n = polyandrous_females_NARL, N = unique_females, type = 'polyandry', study_site = TRUE)],
                    dss[, .(year_, x = renesting_males_NARL_per, n = renesting_males_NARL, N = unique_males,  type = 'renesting', study_site = TRUE)],
                    dss[, .(year_, x = EPY_nests_per, n = N_anyEPY, N = N_parentage, type = 'EPP', study_site = FALSE)],
                    dss[, .(year_, x = polyandrous_females_per, n = polyandrous_females, N = unique_females,  type = 'polyandry', study_site = FALSE)],
                    dss[, .(year_, x = renesting_males_per, n = renesting_males, N = unique_males,  type = 'renesting', study_site = FALSE)]))

ds[, year_ := as.factor(year_)]
ds[, sample_size := paste0(n, '/', N)]
ds[year_ != '2017' & type != 'EPP',sample_size := paste0(sample_size, '*')]

p = 
ggplot(data = ds[study_site == FALSE], aes(year_, x, fill = type, label = sample_size)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.7) +
  geom_text(position = position_dodge(width = 0.7), size = 6, vjust = -0.5) +
  scale_fill_manual(values = c('grey50','#95D840FF', '#33638DFF'), labels = c('nests with EPP', 'polyandrous females', 'renesting males')) +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
  xlab('Year') + ylab('Percentage of') + 
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.2, 0.9), legend.title = element_blank())
p

# png(paste0('./REPORTS/FIGURES/EPY_polyandry_renesting.png'), width = 700, height = 600)
# p
# dev.off()

dss[, renesting_males          := paste0(renesting_males_per, '% (', renesting_males, '/', unique_males, ')')]
dss[, renesting_males_NARL     := paste0(renesting_males_NARL_per, '% (', renesting_males_NARL, '/', unique_males, ')')]
dss[, polyandrous_females      := paste0(polyandrous_females_per, '% (', polyandrous_females, '/', unique_females, ')')]
dss[, polyandrous_females_NARL := paste0(polyandrous_females_NARL_per, '% (', polyandrous_females_NARL, '/', unique_females, ')')]

dss[, c('unique_males', 'unique_females', 'N_parentage', 'N_anyEPY') := NULL]
dss



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
bm = create_bm(d)
bm + geom_point(data = d[parentage == TRUE], aes(lon, lat, color = study_site))
bm + geom_point(data = d[parentage == TRUE & !is.na(data_type)], aes(lon, lat, color = data_type))
bm + geom_point(data = d[parentage == TRUE], aes(lon, lat, color = YEAR_))
bm + geom_point(data = d[parentage == TRUE], aes(lon, lat, color = as.character(anyEPY)))

# split in four data types
ds = d[parentage == TRUE, .(N_parentage = .N), by =  data_type]
ds = d[data_type == 'clutch_removal_exp']

ds = d[parentage == TRUE & year_ == 2018]
ggplot(data = ds) +
  geom_boxplot(aes(data_type, initiation))
ds[, .N, data_type]




# split in year only
ds = d[, .(N_nests = .N), by = .(year_)]
ds2 = d[parentage == TRUE, .(N_parentage = .N), by = .(year_)]
ds3 = d[anyEPY == TRUE, .(N_EPY = .N), by = .(year_)]
ds4 = d[, .(N_eggs = sum(N_parentage, na.rm = TRUE), N_eggs_EPY = sum(N_EPY, na.rm = TRUE)), by = .(year_)]
ds = merge(ds, ds2, by = c('year_'), all.x = TRUE)
ds = merge(ds, ds3, by = c('year_'), all.x = TRUE)
ds = merge(ds, ds4, by = c('year_'), all.x = TRUE)
ds[is.na(ds)] = 0
ds = ds[N_parentage != 0]
ds[, EPY_nests_ := round(N_EPY / N_parentage * 100, 1)]
ds[, EPY_eggs_  := round(N_eggs_EPY / N_eggs * 100, 1)]
ds[, EPY_nests := paste0(round(N_EPY / N_parentage * 100, 1), '% (', N_EPY, '/', N_parentage, ')')]
ds[, EPY_eggs  := paste0(round(N_eggs_EPY / N_eggs * 100, 1), '% (', N_eggs_EPY, '/', N_eggs, ')')]

# plot 
ds[, sample_size := paste0(N_EPY, '/', N_parentage)]
ds[, sample_size_eggs := paste0(N_eggs_EPY, '/', N_eggs)]
ds[, year_ := as.factor(year_)]

p1 = 
ggplot(data = ds, aes(year_, EPY_nests_, label = sample_size)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.7, fill = 'grey50') +
  geom_text(position = position_dodge(width = 0.7), size = 6, vjust = -0.5) +
  scale_y_continuous(limits = c(0, 22), expand = c(0, 0)) +
  xlab('Year') + ylab('Percentage of nests with EPY') + 
  theme_classic(base_size = 20)

p2 = 
ggplot(data = ds, aes(year_, EPY_eggs_, label = sample_size_eggs)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.7, fill = 'grey50') +
  geom_text(position = position_dodge(width = 0.7), size = 6, vjust = -0.5) +
  scale_y_continuous(limits = c(0, 11), expand = c(0, 0)) +
  xlab('Year') + ylab('Percentage of EPY') + 
  theme_classic(base_size = 20)

p1 + p2 + plot_layout(nrow = 2)

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
ds[, EPY_nests_ := round(N_EPY / N_parentage * 100, 1)]
ds[, EPY_eggs_  := round(N_eggs_EPY / N_eggs * 100, 1)]
ds[, EPY_nests := paste0(round(N_EPY / N_parentage * 100, 1), '% (', N_EPY, '/', N_parentage, ')')]
ds[, EPY_eggs  := paste0(round(N_eggs_EPY / N_eggs * 100, 1), '% (', N_eggs_EPY, '/', N_eggs, ')')]

# plot 
ds[, sample_size := paste0(N_EPY, '/', N_parentage)]
ds[, year_ := as.factor(year_)]

ggplot(data = ds, aes(year_, EPY_nests_, fill = data_type, label = sample_size)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.7) 

p = 
  ggplot(data = ds[study_site == FALSE], aes(year_, x, fill = type, label = sample_size)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.7) +
  geom_text(position = position_dodge(width = 0.7), size = 6, vjust = -0.5) +
  scale_fill_manual(values = c('grey50','#95D840FF', '#33638DFF'), labels = c('nests with EPP', 'polyandrous females', 'renesting males')) +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
  xlab('Year') + ylab('Percentage of') + 
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.2, 0.9), legend.title = element_blank())
p




ds[, c('N_parentage', 'N_EPY', 'N_eggs', 'N_eggs_EPY') := NULL]
setorder(ds, year_, -data_type)
ds

# split in NARL study site and everything else
ds = d[, .(N_nests = .N), by =  study_site]
ds2 = d[parentage == TRUE, .(N_parentage = .N), by =  study_site]
ds3 = d[anyEPY == TRUE, .(N_EPY = .N), by =  study_site]
ds4 = d[, .(N_eggs = sum(N_parentage, na.rm = TRUE), N_eggs_EPY = sum(N_EPY, na.rm = TRUE)), by =  study_site]
ds = merge(ds, ds2, by = c('study_site'), all.x = TRUE)
ds = merge(ds, ds3, by = c('study_site'), all.x = TRUE)
ds = merge(ds, ds4, by = c('study_site'), all.x = TRUE)
ds[is.na(ds)] = 0
ds = ds[N_parentage != 0]
ds[, EPY_nests := paste0(round(N_EPY / N_parentage * 100, 0), '% (', N_EPY, '/', N_parentage, ')')]
ds[, EPY_eggs  := paste0(round(N_eggs_EPY / N_eggs * 100, 0), '% (', N_eggs_EPY, '/', N_eggs, ')')]
ds[, c('N_parentage', 'N_EPY', 'N_eggs', 'N_eggs_EPY') := NULL]
setorder(ds, -study_site)
ds[, year_ := 'total']
dt = copy(ds)

# split in NARL study site and everything else by year
ds = d[, .(N_nests = .N), by = .(year_, study_site)]
ds2 = d[parentage == TRUE, .(N_parentage = .N), by = .(year_, study_site)]
ds3 = d[anyEPY == TRUE, .(N_EPY = .N), by = .(year_, study_site)]
ds4 = d[, .(N_eggs = sum(N_parentage, na.rm = TRUE), N_eggs_EPY = sum(N_EPY, na.rm = TRUE)), by = .(year_, study_site)]
ds = merge(ds, ds2, by = c('year_', 'study_site'), all.x = TRUE)
ds = merge(ds, ds3, by = c('year_', 'study_site'), all.x = TRUE)
ds = merge(ds, ds4, by = c('year_', 'study_site'), all.x = TRUE)
ds[is.na(ds)] = 0
ds = ds[N_parentage != 0]
ds[, EPY_nests := paste0(round(N_EPY / N_parentage * 100, 0), '% (', N_EPY, '/', N_parentage, ')')]
ds[, EPY_eggs  := paste0(round(N_eggs_EPY / N_eggs * 100, 0), '% (', N_eggs_EPY, '/', N_eggs, ')')]
# mean
ds[study_site == TRUE, N_EPY / N_parentage * 100]  %>% mean %>% round(., 0)
ds[study_site == FALSE, N_EPY / N_parentage * 100] %>% mean %>% round(., 0)
ds[study_site == TRUE, N_eggs_EPY / N_eggs * 100]  %>% mean %>% round(., 0)
ds[study_site == FALSE, N_eggs_EPY / N_eggs * 100] %>% mean %>% round(., 0)

ds[, c('N_parentage', 'N_EPY', 'N_eggs', 'N_eggs_EPY') := NULL]
setorder(ds, -study_site, year_)

# bind with total
ds[, year_ := as.character(year_)]
ds = rbind(ds, dt, use.names = TRUE)

# data type
ds[study_site == TRUE, data_type := 'study site']
ds[study_site == FALSE, data_type := 'external']

ds = ds[, .(year_, data_type, EPY_nests, EPY_eggs)]
ds

# openxlsx::write.xlsx(ds, './REPORTS/EPY_frequency/EPY_summary_table.xlsx')

# N nests full clutch sampled
ds = d[parentage == TRUE]
ds[, full_clutch_sampled := clutch_size == N_parentage]
ds[is.na(full_clutch_sampled), full_clutch_sampled := FALSE]
ds[, .N, full_clutch_sampled]
ds[full_clutch_sampled == TRUE]  %>% nrow
ds[full_clutch_sampled == TRUE]  %>% nrow / ds %>% nrow * 100 # percent of all

# seperated in external and not
ds[study_site == TRUE & full_clutch_sampled == TRUE]  %>% nrow
ds[study_site == TRUE & full_clutch_sampled == TRUE]  %>% nrow / ds[study_site == TRUE] %>% nrow * 100 # percent of all

# why not?
ds[study_site == TRUE & full_clutch_sampled == FALSE] # twice no DNA extracted, once egg gone before sampled

ds[study_site == FALSE & parentage == TRUE]  %>% nrow
ds[study_site == FALSE & full_clutch_sampled == TRUE]  %>% nrow
ds[study_site == FALSE & full_clutch_sampled == TRUE]  %>% nrow / ds[study_site == FALSE] %>% nrow * 100 # percent of all

mean(ds$clutch_size, na.rm = TRUE)
sd(ds$clutch_size, na.rm = TRUE)
min(ds$clutch_size, na.rm = TRUE)
max(ds$clutch_size, na.rm = TRUE)

# repeatability of EPY by male_id?
ds = d[N_male_clutch > 1 & !is.na(anyEPY)]
ds[, N_nests_anyEPY := sum(anyEPY), by = male_id]
ds[, .N, by = N_nests_anyEPY] # no male with multiple nests with EPY
unique(ds$male_id) %>% length

# repeatability of EPY by male_id?
ds = d[N_female_clutch > 1 & !is.na(anyEPY)]
ds[, N_nests_anyEPY := sum(anyEPY), by = female_id]
ds[, .N, by = N_nests_anyEPY] # no female with multiple nests with EPY
unique(ds$female_id) %>% length

# how often was the male assigned in the field in nests with parentage data?
d[parentage == TRUE] %>% nrow
d[parentage == TRUE & male_field == 1] %>% nrow
d[parentage == TRUE & male_field == 1] %>% nrow / d[parentage == TRUE] %>% nrow
d[parentage == TRUE & female_field == 1] %>% nrow
d[parentage == TRUE & female_field == 1] %>% nrow / d[parentage == TRUE] %>% nrow
d[parentage == TRUE & female_assigned == 2] %>% nrow / d[parentage == TRUE] %>% nrow

d[parentage == TRUE & female_assigned == 2 & study_site == TRUE] %>% nrow / d[parentage == TRUE & study_site == TRUE] %>% nrow
d[parentage == TRUE & female_assigned == 2 & study_site == FALSE] %>% nrow / d[parentage == TRUE & study_site == FALSE] %>% nrow

d[parentage == TRUE & study_site == TRUE & female_field == 1] %>% nrow / d[parentage == TRUE & study_site == TRUE] %>% nrow

#------------------------------------------------------------------------------------------------------------------------
# 5. Paternity between study site and external
#------------------------------------------------------------------------------------------------------------------------
 
# subset nests with parentage study site
ds = d[parentage == TRUE & study_site == TRUE]

# by nests
fm = glm(anyEPY ~ 1, data = ds, family = binomial)
summary(fm)

confint(fm) %>% binomial()$linkinv()

# by eggs
fm = glm(cbind(N_EPY, N_parentage) ~ study_site, data = ds, family = binomial)
summary(fm)

confint(fm) %>% binomial()$linkinv()


# subset nests with parentage external
ds = d[parentage == TRUE & study_site == FALSE]

# by nests
fm = glm(anyEPY ~ 1, data = ds, family = binomial)
summary(fm)

confint(fm) %>% binomial()$linkinv()

# by eggs
fm = glm(cbind(N_EPY, N_parentage) ~ study_site, data = ds, family = binomial)
summary(fm)

confint(fm) %>% binomial()$linkinv()

# parentage between study site and external
ds = d[parentage == TRUE]

# by nests
fm = glm(anyEPY ~ study_site, data = ds, family = binomial)
summary(fm)
Anova(fm)

# by eggs
fm = glm(cbind(N_EPY, N_parentage) ~ study_site, data = ds, family = binomial)
summary(fm)
Anova(fm)


#------------------------------------------------------------------------------------------------------------------------
# 6. Paternity between years 
#------------------------------------------------------------------------------------------------------------------------

# subset nests with parentage
ds = d[parentage == TRUE]

# reverse factor, otherwise intercept 2003 (no EPY)
ds[, YEAR_ := factor(YEAR_, levels = rev(sort(unique(ds$YEAR_))))]

fm = glm(anyEPY ~ YEAR_, data = ds, family = binomial)
summary(fm)
Anova(fm)

# correlation between EPP and polyandry?
cor.test(x = c(3, 5, 9), y = c(3, 6, 14)) # just study site
cor.test(x = c(3, 5, 9), y = c(6, 8, 13)) # all data


#------------------------------------------------------------------------------------------------------------------------
# 7. Paternity polyandrous clutches & renesting 
#------------------------------------------------------------------------------------------------------------------------

# polyandrous clutches (second clutch with different partner)
ID2c = d[female_clutch == 2]$female_id_year
dx = d[female_id_year %in% ID2c]

dr = merge(dx[female_clutch == 1, .(year1 = year_, nestID1 = nestID, female_id_year, m1 = male_id, anyEPY1 = anyEPY, 
                                    ss1 = study_site, initiation1 = initiation)], 
           dx[female_clutch == 2, .(year2 = year_, nestID2 = nestID, female_id_year, m2 = male_id, anyEPY2 = anyEPY, 
                                    EPY_father2 = EPY_father, ss2 = study_site, initiation2 = initiation)], 
           by = 'female_id_year', all = TRUE)

dr[, same_male := m1 == m2]
dr[is.na(same_male), same_male := FALSE]
dr[, both_study_site := ss1 == ss2]
dr[, diff_initiation := difftime(initiation2, initiation1, units = 'days') %>% as.numeric]
setorder(dr, female_id_year)

dr[, mean(diff_initiation)]
dr[, min(diff_initiation)]
dr[, max(diff_initiation)]

dr[anyEPY2 == 1]
dr[anyEPY2 == 1, mean(diff_initiation)]
dr[anyEPY2 == 1, min(diff_initiation)]
dr[anyEPY2 == 1, max(diff_initiation)]

dr = dr[same_male == FALSE]
dr[, sperm_storage := m1 == EPY_father2]

dr[anyEPY2 == 1, .(female_id_year, m1, EPY_father2, initiation1, initiation2, diff_initiation)]

ggplot(data = dr) +
  geom_boxplot(aes(x = factor(anyEPY2), y = diff_initiation))

# difference in initiation dates
dr[anyEPY2 == 0]$diff_initiation %>% median
dr[anyEPY2 == 0]$diff_initiation %>% min
dr[anyEPY2 == 0]$diff_initiation %>% max
dr[sperm_storage == TRUE]$diff_initiation
dr[sperm_storage == FALSE]$diff_initiation


dsp = dr[, .(type = 'polyandrous', N_nests = nrow(dr), EPY_1nest = sum(anyEPY1), EPY_2nest = sum(anyEPY2))]


# fisher test to see if EPY in polyandrous clutches different to known first clutches
matrix(c(3, 0, 11, 18), ncol = 2) %>% fisher.test()

# fisher test to see if EPY in polyandrous clutches different to known first clutches & monogamous renesting
matrix(c(3, 0, 11, 24), ncol = 2) %>% fisher.test() 

# sperm storage?
ds = dr[anyEPY2 == TRUE]
ds[, .N, sperm_storage]

# third male was already incubating at this time
d[male_id_year == '270170235_19', .(initiation, clutch_size, nest_state_date)]

# males renesting with same or different partner
ID2c = d[male_clutch == 2]$male_id_year
dx = d[male_id_year %in% ID2c]

dr = merge(dx[male_clutch == 1, .(year1 = year_, nestID1 = nestID, male_id_year, f1 = female_id, anyEPY1 = anyEPY, 
                                  mfc1 = male_clutch, ss1 = study_site, initiation1 = initiation, nest_state_date1 = nest_state_date)], 
           dx[male_clutch == 2, .(year2 = year_, nestID2 = nestID, male_id_year, f2 = female_id, anyEPY2 = anyEPY, 
                                  fc2 = male_clutch, ss2 = study_site, initiation2 = initiation)], 
           by = 'male_id_year', all = TRUE)

dr[, same_female := f1 == f2]
dr[is.na(same_female), same_female := FALSE]
dr[, both_study_site := ss1 == ss2]
dr[, diff_initiation := difftime(initiation2, initiation1, units = 'days') %>% as.numeric]
dr[, diff_nest_state := difftime(initiation2, nest_state_date1, units = 'days') %>% as.numeric]
setorder(dr, same_female)
dr = dr[!is.na(anyEPY1)]
dr = dr[!is.na(anyEPY2)]

ggplot(data = dr) +
  geom_boxplot(aes(x = factor(anyEPY2), y = diff_initiation))

ggplot(data = dr) +
  geom_boxplot(aes(x = factor(same_female), y = diff_nest_state))

# difference in initiation dates
dr$diff_nest_state %>% length
dr$diff_nest_state
dr$diff_nest_state %>% median
dr$diff_nest_state %>% min
dr$diff_nest_state %>% max

dr[anyEPY2 == 1]$diff_nest_state

dr[same_female == TRUE]$diff_nest_state %>% length
dr[same_female == TRUE]$diff_nest_state
dr[same_female == TRUE]$diff_nest_state %>% median
dr[same_female == TRUE]$diff_nest_state %>% min
dr[same_female == TRUE]$diff_nest_state %>% max

dr[same_female == FALSE]$diff_nest_state %>% length
dr[same_female == FALSE]$diff_nest_state
dr[same_female == FALSE]$diff_nest_state %>% median
dr[same_female == FALSE]$diff_nest_state %>% min
dr[same_female == FALSE]$diff_nest_state %>% max

dsr = dr[, .(type = 'renesting', N_nests = nrow(dr), EPY_1nest = sum(anyEPY1), EPY_2nest = sum(anyEPY2))]

dsr1 = dr[same_female == TRUE, .(type = 'renesting_same_partner', N_nests = nrow(dr[same_female == TRUE]), 
                                 EPY_1nest = sum(anyEPY1), EPY_2nest = sum(anyEPY2))]
dsr2 = dr[same_female == FALSE, .(type = 'renesting_different_partner', N_nests = nrow(dr[same_female == FALSE]), 
                                  EPY_1nest = sum(anyEPY1), EPY_2nest = sum(anyEPY2))]

ds = rbindlist(list(dsp, dsr, dsr1, dsr2))

ds[, EPY_first_nest := paste0(round(EPY_1nest / N_nests * 100, 0), '% (', EPY_1nest, '/', N_nests, ')')]
ds[, EPY_second_nest := paste0(round(EPY_2nest / N_nests * 100, 0), '% (', EPY_2nest, '/', N_nests, ')')]
ds[, c('N_nests', 'EPY_1nest', 'EPY_2nest') := NULL]

ds

#------------------------------------------------------------------------------------------------------------------------
# 8. Timing of second and third clutches
#------------------------------------------------------------------------------------------------------------------------

# assign clutch identity
d[female_clutch == 3, clutch_identity := 'third']
d[female_clutch == 2, clutch_identity := 'second']
d[female_clutch == 1 & N_female_clutch > 1, clutch_identity := 'first']
d[female_clutch == 1 & N_female_clutch < 2, clutch_identity := 'one']
d[clutch_identity == 'one' & anyEPY == FALSE, clutch_identity := 'one_noEPY']
d[clutch_identity == 'one' & anyEPY == TRUE, clutch_identity := 'one_EPY']
d[is.na(clutch_identity)]
d[, .N, by = clutch_identity]

# factor order
d[, clutch_identity := factor(clutch_identity, levels = c('one_noEPY', 'one_EPY', 'first', 'second', 'third'))]

ds1 = d[study_site == TRUE]
ds2 = d[study_site == FALSE & clutch_identity %in% c('first', 'second', 'third')]
ds = rbind(ds1, ds2)
ds = ds[!is.na(initiation)]
ds = ds[!is.na(anyEPY)]
ds[, anyEPY := as.character(anyEPY)]


ds[, mean_initiation := mean(initiation, na.rm = TRUE), by = year_]
ds[, initiation_st := difftime(initiation, mean_initiation, units = 'days') %>% as.numeric]


p = 
  ggplot(data = ds) +
    geom_boxplot(aes(clutch_identity, initiation_st), fill = 'grey85', outlier.alpha = 0) +
    geom_jitter(aes(clutch_identity, initiation_st, fill = anyEPY), width = 0.3, height = 0, shape = 21, size = 3) +
    scale_fill_manual(values = c('white', 'black'), name = 'any EPY', labels = c('no', 'yes')) +
    scale_x_discrete(labels = c('without EPY', 'with EPY', 'first', 'second', 'third')) +
    xlab('one known clutch          multiple known clutches    ') + ylab('clutch initiation standardized') +
    theme_classic(base_size = 24)
p


# png(paste0('./REPORTS/FIGURES/EPY_timing_multiple_clutches_study_site.png'), width = 800, height = 800)
# p
# dev.off()


# timing of polyandrous females
ds1 = d[study_site == TRUE]
ds2 = d[study_site == FALSE & clutch_identity %in% c('first', 'second', 'third')]
dst = rbind(ds1, ds2)
dst = dst[!is.na(initiation)]
dst[clutch_identity == 'one_noEPY', clutch_identity := 'one']
dst[clutch_identity == 'one_EPY', clutch_identity := 'one']
dst[, mean_initiation := mean(initiation, na.rm = TRUE), by = year_]
dst[, initiation_st := difftime(initiation, mean_initiation, units = 'days') %>% as.numeric]


fm = aov(initiation_st ~ clutch_identity, data = dst)
summary(fm)

glht(fm, linfct = mcp(clutch_identity = c('one - first = 0'))) %>% summary()
glht(fm, linfct = mcp(clutch_identity = c('one - second = 0'))) %>% summary()

#------------------------------------------------------------------------------------------------------------------------
# all from 2017-2019
ds = d[year_ > 2016]
ds = ds[!is.na(initiation)]
ds = ds[!is.na(anyEPY)]
ds[, anyEPY := as.character(anyEPY)]


ds[, mean_initiation := mean(initiation, na.rm = TRUE), by = year_]
ds[, initiation_st := difftime(initiation, mean_initiation, units = 'days') %>% as.numeric]

p = 
  ggplot(data = ds) +
    geom_boxplot(aes(clutch_identity, initiation_st), fill = 'grey85', outlier.alpha = 0) +
    geom_jitter(aes(clutch_identity, initiation_st, fill = anyEPY), width = 0.3, height = 0, shape = 21, size = 3) +
    scale_fill_manual(values = c('white', 'black'), name = 'any EPY', labels = c('no', 'yes')) +
    scale_x_discrete(labels = c('without EPY', 'with EPY', 'first', 'second', 'third')) +
    xlab('one known clutch          multiple known clutches    ') + ylab('clutch initiation standardized') +
    theme_classic(base_size = 24)
p


# png(paste0('./REPORTS/FIGURES/EPY_timing_multiple_clutches_all.png'), width = 800, height = 800)
# p
# dev.off()


#------------------------------------------------------------------------------------------------------------------------
# all 
ds = d
ds = ds[!is.na(initiation)]
ds = ds[!is.na(anyEPY)]
ds[, anyEPY := as.character(anyEPY)]

ds[, mean_initiation := mean(initiation, na.rm = TRUE), by = year_]
ds[, initiation_st := difftime(initiation, mean_initiation, units = 'days') %>% as.numeric]

ds[, .N, by = anyEPY]
dss = data.table(anyEPY = c('0', '1'),
                 sample_size = c('295', '37'))

p = 
  ggplot(data = ds) +
  geom_boxplot(aes(anyEPY, initiation_st), fill = 'grey85', outlier.alpha = 0) +
  geom_jitter(aes(anyEPY, initiation_st, fill = anyEPY), width = 0.3, height = 0, shape = 21, size = 3, show.legend = FALSE) +
  scale_fill_manual(values = c('white', 'black')) +
  scale_x_discrete(labels = c('without EPY', 'with EPY')) +
  xlab('clutches') + ylab('') +
  geom_text(data = dss, aes(anyEPY, Inf, label = sample_size), vjust = 1, size = 6) +
  theme_classic(base_size = 24)
p


# png(paste0('./REPORTS/FIGURES/EPY_timing_multiple_clutches_all.png'), width = 800, height = 800)
# p
# dev.off()

ds = ds[clutch_identity %in% c('first', 'second', 'third')]
ds[is.na(renesting_male), renesting_male := FALSE]

ds[, any_renesting := any(renesting_male == TRUE), by = female_id_year_NA]
ds[any_renesting == FALSE, next_clutch := 'polyandrous']
ds[any_renesting == TRUE, next_clutch := 'renesting']

# add first and second clutch of females with three clutches again (otherwise linetype does not work)
ds[clutch_identity == 'third']
dss = ds[clutch_identity %in% c('first', 'second') & female_id %in% c(270170935, 19222)]
dss[, female_id_year_NA := paste0(female_id_year_NA, '2')]

ds = rbind(ds, dss)


theme_classic_edit = function (base_size = 11, base_family = "", base_line_size = base_size/22, 
                               base_rect_size = base_size/22, lp = c(0.8, 0.2)) 
{
  theme_bw(base_size = base_size, base_family = base_family, 
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = rel(1)), legend.key = element_blank(), 
          strip.background = element_rect(fill = "white", colour = "black", size = rel(2)), complete = TRUE,
          legend.position = lp)
}

dss = data.table(clutch_identity = c('first', 'second', 'third'),
                 sample_size = c('0/15', '3/15', '0/2'))



p1 = 
  ggplot(data = ds) +
  geom_boxplot(aes(clutch_identity, initiation_st), fill = 'grey85', outlier.alpha = 0) +
  geom_line(aes(clutch_identity, initiation_st, group = female_id_year_NA, linetype = next_clutch)) +
  geom_point(aes(clutch_identity, initiation_st, fill = anyEPY), shape = 21, size = 3) +
  scale_fill_manual(values = c('white', 'black'), name = 'any EPY', labels = c('no', 'yes')) +
  scale_linetype_manual(values = c('solid', 'dotted'), name = 'next clutch') +
  scale_x_discrete(labels = c('first', 'second', 'third')) +
  xlab('clutch') + ylab('clutch initiation standardized') +
  geom_text(data = dss, aes(clutch_identity, Inf, label = sample_size), vjust = 1, size = 6) +
  theme_classic_edit(base_size = 24, lp = c(0.85, 0.2))
p1


p1 + p


# png(paste0('./REPORTS/FIGURES/EPY_timing.png'), width = 1000, height = 800)
# p1 + p + plot_layout(ncol = 2, widths = c(2 , 1))
# dev.off()


#------------------------------------------------------------------------------------------------------------------------
# 9. Paternity frequency within the season 
#------------------------------------------------------------------------------------------------------------------------

# all 
ds = d
ds = ds[!is.na(initiation)]
ds = ds[!is.na(anyEPY)]

ds[, mean_initiation := mean(initiation, na.rm = TRUE), by = year_]
ds[, initiation_st := difftime(initiation, mean_initiation, units = 'days') %>% as.numeric]

fm = glm(anyEPY ~ initiation_st, data = ds, family = binomial)
summary(fm)
Anova(fm)

# for each year single
ds$year_ %>% unique

ds[, .N, year_]

dss = ds[year_ == 2004]
fm = glm(anyEPY ~ initiation_st, data = dss, family = binomial)
summary(fm)

dss = ds[year_ == 2005]
fm = glm(anyEPY ~ initiation_st, data = dss, family = binomial)
summary(fm)

dss = ds[year_ == 2014]
fm = glm(anyEPY ~ initiation_st, data = dss, family = binomial)
summary(fm)

dss = ds[year_ == 2017]
fm = glm(anyEPY ~ initiation_st, data = dss, family = binomial)
summary(fm)

dss = ds[year_ == 2018]
fm = glm(anyEPY ~ initiation_st, data = dss, family = binomial)
summary(fm)

dss = ds[year_ == 2019]
fm = glm(anyEPY ~ initiation_st, data = dss, family = binomial)
summary(fm)


# load Dale et al. data
dale = read.csv2('./DATA/Dale_EPP.csv') %>% data.table

# adjust initiation date between populations
diff_to_Dale = mean(dale$initiation_doy, na.rm = TRUE) - mean(ds$initiation_doy, na.rm = TRUE)
dale[, initiation_doy := initiation_doy - diff_to_Dale]

fm = glm(anyEPY ~ initiation_doy, data = dale, family = binomial)
summary(fm)
Anova(fm)
