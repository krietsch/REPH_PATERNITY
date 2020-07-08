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
# 8. Paternity frequency within the season 
# 9. Paternity frequency between years

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak', 'arm'),
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

ds[, .N, .(year_, study_site)]

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

# check NA
d[external == 0 & is.na(initiation_method), .(year_, nest, initiation_y)]

# d[is.na(initiation_method) & !is.na(initiation), initiation_method := 'found_incomplete']

ds = d[external == 1 & parentage == TRUE, .N, initiation_method]
ds[, N_nests := nrow(d[external == 1 & parentage == TRUE])]
ds[, initiation_method_percent := N / N_nests * 100]
ds


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
dss[, renesting_males          := paste0(round(renesting_males / unique_males * 100, 0), '% (', renesting_males, '/', unique_males, ')')]
dss[, renesting_males_NARL     := paste0(round(renesting_males_NARL / unique_males * 100, 0), '% (', renesting_males_NARL, '/', unique_males, ')')]
dss[, polyandrous_females      := paste0(round(polyandrous_females / unique_females * 100, 0), '% (', polyandrous_females, '/', unique_females, ')')]
dss[, polyandrous_females_NARL := paste0(round(polyandrous_females_NARL / unique_females * 100, 0), '% (', polyandrous_females_NARL, '/', unique_females, ')')]
dss[, c('unique_males', 'unique_females') := NULL]

# add EPY on plot
dsp = ds[!is.na(N_parentage), .(N_parentage = .N), by = year_]
dspn = ds[!is.na(N_parentage) & anyEPY == TRUE, .(N_anyEPY = .N), by = year_]
dsp = merge(dsp, dspn, by = 'year_')
dsp[, EPY := paste0(round(N_anyEPY / N_parentage * 100, 0), '% (', N_anyEPY, '/', N_parentage, ')')]
dsp[, c('N_parentage', 'N_anyEPY') := NULL]
dss = merge(dss, dsp, by = 'year_')

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

d[parentage == TRUE & study_site == TRUE & female_field == 1] %>% nrow / d[parentage == TRUE & study_site == TRUE] %>% nrow

#------------------------------------------------------------------------------------------------------------------------
# 5. Paternity between study site and external
#------------------------------------------------------------------------------------------------------------------------

# subset nests with parentage
ds = d[parentage == TRUE]

# by nests
fm = glm(anyEPY ~ study_site, data = ds, family = binomial)
summary(fm)

# calculate credibility intervals
nd = data.frame(study_site = c(TRUE, FALSE))
nd = droplevels(nd)
xmat = model.matrix(~study_site, data = nd)
nd$fit = plogis(xmat %*% coef(fm))
nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

# plot 
ggplot(data = nd) +
  geom_point(aes(x = study_site, y = fit*100)) +
  geom_errorbar(aes(x = study_site, ymin = lower*100, ymax = upper*100), width = .1, position = position_dodge(width = 0.5)) +
  labs(y = 'Percent nests with EPY', x = 'Study site') +
  ylim(min = 0, max = 20) +
  theme_classic(base_size = 16)

# intervals study site vs. external
fit_mean = fitmat[nd$study_site == TRUE, ] 
quantile(fit_mean, probs = c(0.025, 0.5, 0.975)) %>% round(., 3) * 100

fit_mean = fitmat[nd$study_site == FALSE, ] 
quantile(fit_mean, probs = c(0.025, 0.5, 0.975)) %>% round(., 3) * 100

# by eggs
fm = glm(cbind(N_EPY, N_parentage) ~ study_site, data = ds, family = binomial)
summary(fm)

# calculate credibility intervals
nd = data.frame(study_site = c(TRUE, FALSE))
nd = droplevels(nd)
xmat = model.matrix(~study_site, data = nd)
nd$fit = plogis(xmat %*% coef(fm))
nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

# plot 
ggplot(data = nd) +
  geom_point(aes(x = study_site, y = fit*100)) +
  geom_errorbar(aes(x = study_site, ymin = lower*100, ymax = upper*100), width = .1, position = position_dodge(width = 0.5)) +
  labs(y = 'Percent nests with EPY', x = 'Study site') +
  ylim(min = 0, max = 20) +
  theme_classic(base_size = 16)

# intervals study site vs. external
fit_mean = fitmat[nd$study_site == TRUE, ] 
quantile(fit_mean, probs = c(0.025, 0.5, 0.975)) %>% round(., 3) * 100

fit_mean = fitmat[nd$study_site == FALSE, ] 
quantile(fit_mean, probs = c(0.025, 0.5, 0.975)) %>% round(., 3) * 100


#------------------------------------------------------------------------------------------------------------------------
# 6. Paternity between years 
#------------------------------------------------------------------------------------------------------------------------

# subset nests with parentage
ds = d[parentage == TRUE]

# reverse factor, otherwise intercept 2003 (no EPY)
ds[, YEAR_ := factor(YEAR_, levels = rev(sort(unique(ds$YEAR_))))]

fm = glm(anyEPY ~ YEAR_ + study_site, data = ds, family = binomial)
summary(fm)

# calculate credibility intervals
nd0 = data.frame(YEAR_ = rep(unique(ds[study_site == TRUE]$YEAR_)), study_site = TRUE)
nd1 = data.frame(YEAR_ = rep(unique(ds[study_site == FALSE]$YEAR_)), study_site = FALSE)
nd = (rbind(nd0, nd1))
xmat = model.matrix(~YEAR_ + study_site, data = nd)
nd$fit = plogis(xmat %*% coef(fm))
nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

# correct year with 0 EPY in sample
qb2003 = qbeta(p = c(0.025, 0.5, 0.975), 1, 13)
nd$fit[nd$YEAR_ == 2003] = qb2003[2]
nd$lower[nd$YEAR_ == 2003] = qb2003[1]
nd$upper[nd$YEAR_ == 2003] = qb2003[3]

nd = data.table(nd)
nd[, YEAR_ := factor(YEAR_, levels = sort(unique(ds$YEAR_)))]


ggplot(data = nd) +
  geom_point(aes(x = YEAR_, y = fit*100, group = as.factor(study_site), color = as.factor(study_site)), 
             position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(x = YEAR_, ymin = lower*100, ymax = upper*100, group = as.factor(study_site)), 
                width = .1, position = position_dodge(width = 0.5)) +
  scale_color_manual(name = 'Study site', values = c('firebrick3', 'dodgerblue2')) +
  theme_classic(base_size = 24) + labs(x = 'Year', y = 'Percent nests with EPY')

# calculate estimate mean year
fit_mean = fitmat[nd$study_site == TRUE, ] %>% colMeans
quantile(fit_mean, probs = c(0.025, 0.5, 0.975))
mean(fit_mean)

fit_mean = fitmat[nd$study_site == FALSE, ] %>% colMeans
quantile(fit_mean, probs = c(0.025, 0.5, 0.975))
mean(fit_mean)

### differences if in seperate models?
#------------------------------------------------------------------------------------------------------------------------

# subset nests with parentage and study site
ds = d[parentage == TRUE & study_site == TRUE]

fm = glm(anyEPY ~ YEAR_, data = ds, family = binomial)
summary(fm)

# calculate credibility intervals
nd = data.frame(YEAR_ = rep(unique(ds$YEAR_)))
nd = droplevels(nd)
xmat = model.matrix(~YEAR_, data = nd)
nd$fit = plogis(xmat %*% coef(fm))
nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

# plot 
ggplot(data = nd) +
  geom_point(aes(x = YEAR_, y = fit*100)) +
  geom_errorbar(aes(x = YEAR_, ymin = lower*100, ymax = upper*100), width = .1, position = position_dodge(width = 0.5)) +
  labs(y = 'Percent nests with EPY', x = 'Years') +
  theme_classic(base_size = 16)

# intervals external
quantile(fitmat, probs = c(0.025, 0.5, 0.975))

# copy for plot
nd_on = copy(data.table(nd))

# subset nests with parentage and external
ds = d[parentage == TRUE & study_site == FALSE]

fm = glm(anyEPY ~ YEAR_, data = ds, family = binomial)
summary(fm)

# calculate credibility intervals
nd = data.frame(YEAR_ = rep(unique(ds$YEAR_)))
nd = droplevels(nd)
xmat = model.matrix(~YEAR_, data = nd)
nd$fit = plogis(xmat %*% coef(fm))
nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

# correct year with 0 EPY in sample
qb2003 = qbeta(p = c(0.025, 0.5, 0.975), 1, 13)
nd$fit[nd$YEAR_ == 2003] = qb2003[2]
nd$lower[nd$YEAR_ == 2003] = qb2003[1]
nd$upper[nd$YEAR_ == 2003] = qb2003[3]

# plot 
ggplot(data = nd) +
  geom_point(aes(x = YEAR_, y = fit*100)) +
  geom_errorbar(aes(x = YEAR_, ymin = lower*100, ymax = upper*100), width = .1, position = position_dodge(width = 0.5)) +
  labs(y = 'Percent nests with EPY', x = 'Years') +
  theme_classic(base_size = 16)

# intervals external
quantile(fitmat, probs = c(0.025, 0.5, 0.975))

# copy for plot
nd_off = copy(data.table(nd))

# merge
nd_on[, study_site := TRUE]
nd_off[, study_site := FALSE]
nd = rbind(nd_on, nd_off)
nd[, YEAR_ := factor(YEAR_, levels = sort(unique(ds$YEAR_)))]

# plot for both seperated models
p = 
ggplot(data = nd) +
  geom_point(aes(x = YEAR_, y = fit*100, group = as.factor(study_site), color = as.factor(study_site)), 
             position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(x = YEAR_, ymin = lower*100, ymax = upper*100, group = as.factor(study_site)), 
                width = .1, position = position_dodge(width = 0.5)) +
  scale_color_manual(name = 'Study site', values = c('firebrick3', 'dodgerblue2')) +
  theme_classic(base_size = 24) + labs(x = 'Year', y = 'Percent nests with EPY')
p

png(paste0('./REPORTS/FIGURES/EPY_frequency_years.png'), width = 1200, height = 800)
p
dev.off()


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
dr[anyEPY2 == 1]$diff_nest_state

dr[anyEPY2 == 0]$diff_nest_state %>% length
dr[anyEPY2 == 0]$diff_nest_state
dr[anyEPY2 == 0]$diff_nest_state %>% median
dr[anyEPY2 == 0]$diff_nest_state %>% min
dr[anyEPY2 == 0]$diff_nest_state %>% max

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
# 8. Paternity frequency within the season 
#------------------------------------------------------------------------------------------------------------------------

# subset nests with parentage, exclude year without any EPY
ds = d[parentage == TRUE & YEAR_ != '2003']
ds[, YEAR_ := factor(YEAR_)]

fm = glm(anyEPY ~ initiation_doy * YEAR_, data = ds, family = binomial)
summary(fm)

rs = seq(min(ds$initiation_doy, na.rm = TRUE), 
         max(ds$initiation_doy, na.rm = TRUE), 1)
years = unique(ds$YEAR_)
nd = data.table(YEAR_ = rep(years, each = length(rs)),
                initiation_doy = rep(rs, length(years)))
dx = ds[, .(min_year_ = min(initiation_doy, na.rm = TRUE), max_year = max(initiation_doy, na.rm = TRUE)), by = YEAR_]
nd = merge(nd, dx, all.x = TRUE, by = 'YEAR_')
nd[, initiation_in_year := initiation_doy > min_year_ & initiation_doy < max_year]
nd = nd[initiation_in_year == TRUE]
xmat = model.matrix(~initiation_doy * YEAR_, data = nd)
nd$fit = plogis(xmat %*% coef(fm))
nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

ggplot(data = nd) +
  geom_line(aes(x = initiation_doy, y = fit*100, group = YEAR_, color = YEAR_)) +
  geom_ribbon(aes(x = initiation_doy, ymin = lower*100, ymax = upper*100, fill = YEAR_, color = NULL), alpha = .15) +
  theme_classic(base_size = 24) + labs(x = 'Day of the year', y = 'Percent nests with EPY')

nd1 = copy(data.table(nd))

# load Dale et al. data
dale = read.csv2('./DATA/Dale_EPP.csv') %>% data.table

# adjust initiation date between populations
diff_to_Dale = mean(dale$initiation_doy, na.rm = TRUE) - mean(ds$initiation_doy, na.rm = TRUE)
dale[, initiation_doy := initiation_doy - diff_to_Dale]

fm = glm(anyEPY ~ initiation_doy, data = dale, family = binomial)
summary(fm)

# calculate credibility intervals
rs = seq(min(dale$initiation_doy, na.rm = TRUE), 
         max(dale$initiation_doy, na.rm = TRUE), 1)
nd = data.table(initiation_doy = rs)
xmat = model.matrix(~initiation_doy, data = nd)
nd$fit = plogis(xmat %*% coef(fm))
nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

# plot of Dale data
plot(nd$initiation_doy, nd$fit)
ggplot(data = nd) +
  geom_line(aes(x = initiation_doy, y = fit*100)) +
  geom_ribbon(aes(x = initiation_doy, ymin = lower*100, ymax = upper*100), alpha = .15) +
  theme_classic(base_size = 24) + labs(x = 'Year', y = 'Percent nests with EPY')

nd2 = copy(data.table(nd))

# merge data
nd2[, YEAR_ := '1993 (Dale et al.)']

nd = rbind(nd2[, .(YEAR_, initiation_doy, fit, lower, upper)], nd1[, .(YEAR_, initiation_doy, fit, lower, upper)])

# ggplot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(7)

# plot of our data and Dale et al. together
p = 
ggplot(data = nd) +
  geom_line(aes(x = initiation_doy, y = fit*100, group = YEAR_, color = YEAR_)) +
  geom_ribbon(aes(x = initiation_doy, ymin = lower*100, ymax = upper*100, fill = YEAR_, color = NULL), alpha = .15) +
  scale_color_manual(values = c('black', cols), name = 'Year') +
  scale_fill_manual(values = c('black', cols), name = 'Year') + 
  theme_classic(base_size = 24) + labs(x = 'Day of the year', y = 'Percent nests with EPY')
p

png(paste0('./REPORTS/FIGURES/EPY_frequency_nest_initiation.png'), width = 1200, height = 800)
p
dev.off()


#------------------------------------------------------------------------------------------------------------------------
### Sample timing and EPY frequency
ds = d[!is.na(initiation_y)]
ds[, anyEPY_year := any(parentage == TRUE), by = year_]

dsAll = ds[anyEPY_year == TRUE]
dsAll[, type := 'all nests']

dsEPY = ds[parentage == TRUE]
dsEPY[, type := ifelse(anyEPY == 1, 'with EPY', 'without EPY')]

ds = rbind(dsAll, dsEPY)
ds[, type := factor(type, levels = c('with EPY','without EPY', 'all nests'))]

ggplot(ds[year_ > 2016]) +
  geom_boxplot(aes(type, initiation_y, color = type)) +
  labs(x = '', y = 'Initiation date') +
  coord_flip() + facet_grid(year_ ~ .) +
  theme_classic(base_size = 24)

ggplot(ds) +
  geom_boxplot(aes(type, initiation_y, color = type)) +
  labs(x = '', y = 'Initiation date') +
  coord_flip() + facet_grid(year_ ~ .) +
  theme_classic(base_size = 24)

ggplot(dsAll) +
  geom_boxplot(aes(parentage, initiation_y, color = parentage)) +
  labs(x = '', y = 'Initiation date') +
  coord_flip() + facet_grid(year_ ~ .) +
  theme_classic(base_size = 24)


#------------------------------------------------------------------------------------------------------------------------
# 9. Paternity frequency between years
#------------------------------------------------------------------------------------------------------------------------

require(effects)

# seson length and number of nests based on 3 plots monitored in each year
dss = d[plot %in% c('brw1', 'brw2', 'brw3'), .(mean_year_initiation = mean(initiation_doy, na.rm = TRUE),
                                               q25_initiation = quantile(initiation_doy, probs = 0.25, na.rm = TRUE),
                                               q75_initiation = quantile(initiation_doy, probs = 0.75, na.rm = TRUE),
                                               N = .N), by = YEAR_]
dss[, season_length := q75_initiation - q25_initiation]

# merge to all data
d = merge(d, dss, by = 'YEAR_', all.x = TRUE)

# Effect of seson lenght or number of nests, only using Rick's data for season lenght and N nests, but EPY from all
dx = d[!is.na(anyEPY)]
setkey(dx, anyEPY, YEAR_)
dx = dx[CJ(anyEPY, YEAR_, unique = TRUE), .(N_anyEPY = .N, N_nests = N[c(1)], season_length = season_length[c(1)]), by = .EACHI]

dx = merge(dx[anyEPY == 1, .(YEAR_, N_anyEPY1 = N_anyEPY)], dx[anyEPY == 0, .(YEAR_, N_anyEPY0 = N_anyEPY, N_nests, season_length)], by = 'YEAR_')
dx[, percentEPY := N_anyEPY1 / N_anyEPY0 * 100]

# Higher EPY frequency in longer seasons?
ggplot(data = dx) +
  geom_point(aes(x = season_length, y = percentEPY)) +
  geom_smooth(aes(x = season_length, y = percentEPY), method = 'lm', color = 'black') + 
  theme_classic()

fm = glm(cbind(N_anyEPY1, N_anyEPY0) ~ season_length, data = dx, family = binomial)

summary(fm)
plot(allEffects(fm))


# Higher frequency of EPY depending on amount of nests?
ggplot(data = dx) +
  geom_point(aes(x = N_nests, y = percentEPY)) +
  geom_smooth(aes(x = N_nests, y = percentEPY), method = 'lm', color = 'black') + 
  theme_classic()

fm = glm(cbind(N_anyEPY1, N_anyEPY0) ~ N_nests, data = dx, family = binomial)

summary(fm)
plot(allEffects(fm))



