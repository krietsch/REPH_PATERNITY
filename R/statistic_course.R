#========================================================================================================================
# Statistic course - Paternity analysis
#========================================================================================================================
# Packages
sapply( c('data.table', 'sdb', 'magrittr', 'ggplot2', 'sf', 'foreach', 'auksRuak', 'viridis'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')
d = dbq(con, 'select * FROM NESTS')
dp = dbq(con, 'select * FROM PATERNITY')
DBI::dbDisconnect(con)

# d = read.csv2('../NESTS.csv') %>% data.table
# dp = read.csv2('../PATERNITY.csv') %>% data.table

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

# check data
# ggplot() + 
#   geom_sf(data = study_site, fill = 'grey95') +
#   geom_point(data = d, aes(lon, lat, color = study_site))

# merge with data without position
ds[, study_site := NA]
d = rbind(d, ds)

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

# Parentage data
dp[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dp = dp[!is.na(EPY)]

# assign external data
dp[nestID %in% d[external == 0]$nestID, external := 0]
dp[is.na(external), external := 1]

# EPY father 
dp[EPY == 1, .N, by = nestID] # more than 1 EPY father? No
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

# feamles nesting on multiple years
ds = unique(d[!is.na(female_id)], by = 'female_id_year')
ds[, female_clutch_years := seq_len(.N), by = female_id]
ds[, .N, by = .(female_clutch_years)]

# males renesting
d[, male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
d[, N_male_clutch := .N, by = male_id]
d[, male_clutch := seq_len(.N), by = male_id_year]
d[is.na(male_id), male_clutch := 1]
d[!is.na(male_id), N_male_clutch := max(male_clutch), by = male_id_year]
d[is.na(male_id), N_male_clutch := 1]
d[, .N, by = .(year_, male_clutch)]
d[, .N, by = .(male_clutch, external)]

# males nesting on multiple years
ds = unique(d[!is.na(male_id)], by = 'male_id_year')
ds[, male_clutch_years := seq_len(.N), by = male_id]
ds[, .N, by = .(male_clutch_years)]
ds[, .N, by = .(male_clutch_years, external)]

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

# sapply( c('lme4', 'effects', 'multcomp', 'glmmTMB'),
#         require, character.only = TRUE)

# percent EPY per nest in plots / year 
ds = d[study_site == TRUE, .N, .(external, year_)]
ds2 = d[study_site == TRUE & parentage == TRUE, .(N_parentage = .N), .(external, year_)]
ds3 = d[study_site == TRUE & anyEPY == TRUE, .(N_EPY = .N), .(external, year_)]

ds = merge(ds, ds2, by = c('external', 'year_'), all.x = TRUE)
ds = merge(ds, ds3, by = c('external', 'year_'), all.x = TRUE)

ds = ds[!is.na(N_parentage)]
ds[is.na(N_EPY), N_EPY := 0]
ds[, percent_EPY := round(N_EPY / N_parentage * 100, 1)]
ds

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

# differences in mean initiation external vs. our data
dx = d[year_ > 2016 & study_site == TRUE]
dx[, external := factor(external)]

# Use all 
ds = d[, .(external, study_site, plot, year_, YEAR_, nestID, male_id, female_id, anyEPY, Eggs_EPY, Eggs_no_EPY, parentage,
           any_parentage_year, initiation, initiation_y, initiation_doy, female_clutch, N_female_clutch, male_clutch,
           N_male_clutch, nest_state)]

ds[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]

dss = ds[plot %in% c('brw1', 'brw2', 'brw3'), .(mean_year_initiation = mean(initiation_doy, na.rm = TRUE),
                                                q25_initiation = quantile(initiation_doy, probs = 0.25, na.rm = TRUE),
                                                q75_initiation = quantile(initiation_doy, probs = 0.75, na.rm = TRUE),
                                                N = .N), by = YEAR_]
dss[, season_length := q75_initiation - q25_initiation]

# merge to all data
ds = merge(ds, dss, by = 'YEAR_', all.x = TRUE)
ds[, initiation_standardized := initiation_doy - mean_year_initiation]
ds = ds[parentage == TRUE]

ggplot(data = ds) +
  geom_boxplot(aes(x = YEAR_, y = initiation_doy)) +
  theme_classic()

ggplot(data = dss) +
  geom_point(aes(x = q25_initiation, y = season_length)) +
  geom_smooth(aes(x = q25_initiation, y = season_length), method = 'lm', color = 'black') + 
  theme_classic()

fm = lm(season_length ~ q25_initiation, data = dss)
summary(fm)

# data set split
ds[, external := factor(external)]
ds[, NARL_site := external == 0 & study_site == TRUE]
ds[, .N, by = .(YEAR_, NARL_site)]

# 
ds[, .(sum(Eggs_no_EPY), sum(Eggs_EPY)), NARL_site]

# data available
dx %>% nrow
dx[, .N, YEAR_]
Nm = dx[, .(N = .N), by = male_id]
Nm[, .N, by = N]
Nf = dx[, .(N = .N), by = female_id]
Nf[, .N, by = N]

# EPY with SE
fm = glmer(anyEPY ~ 1 +  ( 1 | YEAR_), data = ds, family = binomial)

summary(fm)
confint(fm)[2, ] %>% (binomial()$linkinv)

fm = glm(anyEPY ~ 1, data = ds, family = binomial)
summary(fm)

# own data
fm = glm(anyEPY ~ 1, data = ds[external == 0], family = binomial)
summary(fm)

# external
fm = glm(anyEPY ~ 1, data = ds[external == 1], family = binomial)
summary(fm)

# difference between external or not?
fm = glmer(anyEPY ~ 1 +  ( 1 | YEAR_), data = ds, family = binomial)
fm1 = glmer(anyEPY ~ external +  ( 1 | YEAR_), data = ds, family = binomial)

summary(fm1)
AIC(fm, fm1) # no difference, can combine data

# difference between years?
ds[,YEAR_ := factor(YEAR_, levels = unique(ds$YEAR_))]

ds[, external := factor(external)]

fm = glm(anyEPY ~ external + YEAR_, data = ds, family = binomial)

summary(fm)
# plot(allEffects(fm))
# summary(glht(fm, mcp(YEAR_ = 'Tukey')))



# gelman & hill buch - citation for sim
require(arm)



fm = glm(anyEPY ~ YEAR_, data = ds[NARL_site == TRUE], family = binomial)
summary(fm)

nd = data.frame(YEAR_ = unique(ds[NARL_site == TRUE]$YEAR_))
nd = droplevels(nd)

xmat = model.matrix(~YEAR_, data = nd)

nd$fit = plogis(xmat %*% coef(fm))

nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

qb2003 = qbeta(p = c(0.025, 0.5, 0.975), 1, 14)

nd$fit[nd$YEAR_ == 2003] = qb2003[2]
nd$lower[nd$YEAR_ == 2003] = qb2003[1]
nd$upper[nd$YEAR_ == 2003] = qb2003[3]

ggplot(data = nd) +
  geom_point(aes(x = YEAR_, y = fit)) +
  geom_errorbar(aes(x = YEAR_, ymin = lower, ymax = upper), width = .1, position = position_dodge(width = 0.5))



fit_mean = fitmat %>% colMeans
quantile(fit_mean, probs = c(0.025, 0.5, 0.975))



# both in one model

fm = glm(anyEPY ~ YEAR_ + external, data = ds, family = binomial)
summary(fm)

nd0 = data.frame(YEAR_ = rep(unique(ds[external == 0]$YEAR_)), external = '0')
nd1 = data.frame(YEAR_ = rep(unique(ds[external == 1]$YEAR_)), external = '1')
nd = (rbind(nd0, nd1))

xmat = model.matrix(~YEAR_ + external, data = nd)

nd$fit = plogis(xmat %*% coef(fm))

nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

qb2003 = qbeta(p = c(0.025, 0.5, 0.975), 1, 13)

nd$fit[nd$YEAR_ == 2003] = qb2003[2]
nd$lower[nd$YEAR_ == 2003] = qb2003[1]
nd$upper[nd$YEAR_ == 2003] = qb2003[3]

ggplot(data = nd) +
  geom_point(aes(x = YEAR_, y = fit, group = as.factor(external), color = as.factor(external)), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(x = YEAR_, ymin = lower, ymax = upper, group = as.factor(external)), 
                width = .1, position = position_dodge(width = 0.5)) 



# differnt assigned 
ds[, YEAR_ := factor(YEAR_, levels = rev(unique(ds$YEAR_)))]

fm = glm(anyEPY ~ YEAR_ + NARL_site, data = ds, family = binomial)
summary(fm)

nd0 = data.frame(YEAR_ = rep(unique(ds[NARL_site == TRUE]$YEAR_)), NARL_site = TRUE)
nd1 = data.frame(YEAR_ = rep(unique(ds[NARL_site == FALSE]$YEAR_)), NARL_site = FALSE)
nd = (rbind(nd0, nd1))

xmat = model.matrix(~YEAR_ + NARL_site, data = nd)

nd$fit = plogis(xmat %*% coef(fm))

nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

qb2003 = qbeta(p = c(0.025, 0.5, 0.975), 1, 13)

nd$fit[nd$YEAR_ == 2003] = qb2003[2]
nd$lower[nd$YEAR_ == 2003] = qb2003[1]
nd$upper[nd$YEAR_ == 2003] = qb2003[3]

nd = data.table(nd)
nd[, YEAR_ := factor(YEAR_, levels = unique(ds$YEAR_))]


ggplot(data = nd) +
  geom_point(aes(x = YEAR_, y = fit, group = as.factor(NARL_site), color = as.factor(NARL_site)), 
             position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(x = YEAR_, ymin = lower, ymax = upper, group = as.factor(NARL_site)), 
                width = .1, position = position_dodge(width = 0.5)) +
  scale_color_manual(name = 'Own data', values = c('firebrick3', 'dodgerblue2')) +
  theme_classic(base_size = 24) + labs(x = 'Year', y = 'Extra-pair young / nest')




# differnt assigned and interaction!
ds[, YEAR_ := factor(YEAR_, levels = rev(unique(ds$YEAR_)))]

fm = glm(anyEPY ~ YEAR_ * NARL_site, data = ds, family = binomial)
summary(fm)


nd0 = data.frame(YEAR_ = rep(unique(ds[NARL_site == TRUE]$YEAR_)), NARL_site = TRUE)
nd1 = data.frame(YEAR_ = rep(unique(ds[NARL_site == FALSE]$YEAR_)), NARL_site = FALSE)
nd = (rbind(nd0, nd1))
setorder(nd, NARL_site, YEAR_)

xmat = model.matrix(~YEAR_ * NARL_site, data = nd)


nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
fitmat[nd$YEAR_ == 2003, ] = rbeta(nsim, 1, 14) # calculate confidence interval for year with 13 nests without EPY

nd$fit = plogis(xmat %*% coef(fm))
qb2003 = qbeta(p = c(0.025, 0.5, 0.975), 1, 14)
nd$fit[nd$YEAR_ == 2003] = qb2003[2]
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

nd = nd %>% data.table

nd[1, fit := plogis(sum(xmat[1,]*coef(fm), na.rm = TRUE))]
nd[2, fit := plogis(sum(xmat[2,]*coef(fm), na.rm = TRUE))]
nd[3, fit := plogis(sum(xmat[3,]*coef(fm), na.rm = TRUE))]
nd[4, fit := plogis(sum(xmat[4,]*coef(fm), na.rm = TRUE))]
nd[5, fit := plogis(sum(xmat[5,]*coef(fm), na.rm = TRUE))]
nd[6, fit := plogis(sum(xmat[6,]*coef(fm), na.rm = TRUE))]
nd[7, fit := plogis(sum(xmat[7,]*coef(fm), na.rm = TRUE))]
nd[9, fit := plogis(sum(xmat[9,]*coef(fm), na.rm = TRUE))]
nd[10, fit := plogis(sum(xmat[10,]*coef(fm), na.rm = TRUE))]
nd[11, fit := plogis(sum(xmat[11,]*coef(fm), na.rm = TRUE))]


ggplot(data = nd) +
  geom_point(aes(x = YEAR_, y = fit, group = as.factor(NARL_site), color = as.factor(NARL_site)), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(x = YEAR_, ymin = lower, ymax = upper, group = as.factor(NARL_site)), 
                width = .1, position = position_dodge(width = 0.5)) 


# calculate single EPP value
fit_mean = fitmat[nd$NARL_site == TRUE, ] %>% colMeans
quantile(fit_mean, probs = c(0.025, 0.5, 0.975))

fit_mean = fitmat[nd$NARL_site == FALSE, ] %>% colMeans
quantile(fit_mean, probs = c(0.025, 0.5, 0.975))


# different assigned no year_ differences

fm = glm(anyEPY ~ NARL_site, data = ds, family = binomial)
summary(fm)

nd = data.frame(NARL_site = c(TRUE, FALSE))
nd = droplevels(nd)

xmat = model.matrix(~NARL_site, data = nd)

nd$fit = plogis(xmat %*% coef(fm))

nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

ggplot(data = nd) +
  geom_point(aes(x = NARL_site, y = fit)) +
  geom_errorbar(aes(x = NARL_site, ymin = lower, ymax = upper), width = .1, position = position_dodge(width = 0.5))

fit_mean = fitmat[nd$NARL_site == TRUE, ] 
quantile(fit_mean, probs = c(0.025, 0.5, 0.975))

fit_mean = fitmat[nd$NARL_site == FALSE, ] 
quantile(fit_mean, probs = c(0.025, 0.5, 0.975))


dx = d[study_site == TRUE & external == 0 & !is.na(anyEPY)]
dx[, .N, by = anyEPY]

dx = d[!(study_site == TRUE & external == 0) & !is.na(anyEPY)]
dx[, .N, by = anyEPY]


# calculate relationshipt between polyandry and EPP
dx = d[study_site == TRUE & external == 0 & !is.na(anyEPY)]
d1 = dx[, .(N_nests_sampled = .N), by = year_]
d2 = dx[anyEPY == TRUE, .(N_nests_EPY = .N), by = year_]

dq1 = merge(d1, d2, by = 'year_')

dx = d[study_site == TRUE & external == 0 & !is.na(female_id)]
d1 = dx[, .(N_females = unique(female_id) %>% length), by = year_]
d2 = dx[polyandrous == TRUE, .(polyandrous = unique(female_id) %>% length), by = year_]

dq2 = merge(d1, d2, by = 'year_')

dq = merge(dq1, dq2, by = 'year_')

# calculate confidence intervalls for EPY
# dq[, c('EPY_lwr', 'EPY_median', 'EPY_upr') := as.list(qbeta(p = c(0.025, 0.5, 0.975), 1+N_nests_EPY, 1+N_nests_sampled))]

# e.g. confidence interval for one EPPi n 33 nests
qbeta(p = c(0.025, 0.5, 0.975), 1+3, 1+10)


dq[, EPY_lwr    := qbeta(p = c(0.025), 1+N_nests_EPY, 1+N_nests_sampled)]
dq[, EPY_median := qbeta(p = c(0.500), 1+N_nests_EPY, 1+N_nests_sampled)]
dq[, EPY_upr    := qbeta(p = c(0.975), 1+N_nests_EPY, 1+N_nests_sampled)]

# calculate confidence intervalls for polyandry
dq[, pa_lwr    := qbeta(p = c(0.025), 1+polyandrous, 1+N_females)]
dq[, pa_median := qbeta(p = c(0.500), 1+polyandrous, 1+N_females)]
dq[, pa_upr    := qbeta(p = c(0.975), 1+polyandrous, 1+N_females)]

ggplot(data = dq) +
  geom_point(aes(x = pa_median, y = EPY_median)) +
  geom_errorbar(aes(x = pa_median, ymin = EPY_lwr, ymax = EPY_upr), width = .01) +
  geom_errorbarh(aes(y = EPY_median, xmin = pa_lwr, xmax = pa_upr), height = .01) +
  geom_point(aes(x = 1, y = 0.27), color = 'firebrick3') +
  geom_errorbar(aes(x = 1, ymin = 0.08, ymax = 0.50), width = .01, color = 'firebrick3') +
  geom_point(aes(x = 0, y = 0.1), color = 'firebrick3') +
  geom_errorbar(aes(x = 0, ymin = 0.02, ymax = 0.38), width = .01, color = 'firebrick3') +
  geom_point(aes(x = -0.1, y = 0.12), color = 'dodgerblue') +
  geom_errorbar(aes(x = -0.1, ymin = 0.07, ymax = 0.18), width = .01, color = 'dodgerblue') +
  geom_point(aes(x = 0.06, y = 0.10), color = 'dodgerblue') +
  geom_errorbar(aes(x = 0.06, ymin = 0.06, ymax = 0.14), width = .01, color = 'dodgerblue') +
  geom_errorbarh(aes(y = 0.10, xmin = 0.03, xmax = 0.10), height = .01, color = 'dodgerblue') +
  # REPH Dale et al. 
  geom_point(aes(x = -0.15, y = 0.26), color = 'darkgreen') +
  geom_errorbar(aes(x = -0.15, ymin = 0.12, ymax = 0.45), width = .01, color = 'darkgreen') +
  # RNPH general
  geom_point(aes(x = 0.14, y = 0.07), color = 'darkgreen') +
  geom_errorbar(aes(x = 0.14, ymin = 0.02, ymax = 0.14), width = .01, color = 'darkgreen') +
  geom_errorbarh(aes(y = 0.07, xmin = 0.06, xmax = 0.24), height = .01, color = 'darkgreen') +
  # RNPH polyandrous
  geom_point(aes(x = 1.01, y = 0.35), color = 'darkgreen') +
  geom_errorbar(aes(x = 1.01, ymin = 0.12, ymax = 0.65), width = .01, color = 'darkgreen') +
  # RNPH first clutches
  geom_point(aes(x = 0.01, y = 0.03), color = 'darkgreen') +
  geom_errorbar(aes(x = 0.01, ymin = 0, ymax = 0.13), width = .01, color = 'darkgreen') +
  theme_classic(base_size = 16) + labs(y = 'Extra-pair young / nest', x = 'Proportion polyandrous females') 



qbeta(p = c(0.025, 0.5, 0.975), 1+3, 1+10)
qbeta(p = c(0.025, 0.5, 0.975), 1+1, 1+10)
d[!is.na(anyEPY) & external == 1, .N, by = anyEPY]
qbeta(p = c(0.025, 0.5, 0.975), 1+16, 1+125)
d[!is.na(anyEPY) & external == 0 & study_site == TRUE, .N, by = anyEPY]
qbeta(p = c(0.025, 0.5, 0.975), 1+10, 1+165)

# dale et al 6 EPY in 18 nests
qbeta(p = c(0.025, 0.5, 0.975), 1+6, 1+18)

# schamel RNPH 4 in 63 nests with EPY 
qbeta(p = c(0.025, 0.5, 0.975), 1+4, 1+63) # EPP
qbeta(p = c(0.025, 0.5, 0.975), 1+7, 1+47) # polyandry
qbeta(p = c(0.025, 0.5, 0.975), 1+3, 1+6)  # polyandrous females second
qbeta(p = c(0.025, 0.5, 0.975), 1+0, 1+25) # polyandrous females first



# polyandry all data
qbeta(p = c(0.025, 0.5, 0.975), 1+17, 1+158)

fm = lm(EPP ~ log(polyandry), data = dt)
summary(fm)

ggplot(data = dt) +
  geom_point(aes(x = log(polyandry), y = EPP)) 

nd = data.table(polyandry = seq(0, 1, 0.1),
                EPP = seq(0, 1, 0.1))
pr = predict(fm, newdata = nd, type = 'response', interval = "prediction")
pr = cbind(nd, pr)

ggplot(data = pr) +
  geom_line(aes(x = polyandry, y = fit)) +
  geom_ribbon(aes(x = polyandry, ymin = lwr, ymax = upr), alpha = .15)



# both in one model, different assigned based on eggs
fm = glm(cbind(Eggs_EPY, Eggs_no_EPY) ~ YEAR_ + NARL_site, data = ds, family = binomial)
summary(fm)

nd0 = data.frame(YEAR_ = rep(unique(ds[NARL_site == TRUE]$YEAR_)), NARL_site = TRUE)
nd1 = data.frame(YEAR_ = rep(unique(ds[NARL_site == FALSE]$YEAR_)), NARL_site = FALSE)
nd = (rbind(nd0, nd1))

xmat = model.matrix(~YEAR_ + NARL_site, data = nd)

nd$fit = plogis(xmat %*% coef(fm))

nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])

nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

fit_mean = fitmat[nd$NARL_site == FALSE, ] %>% colMeans
plogis(quantile(fit_mean, probs = c(0.025, 0.5, 0.975)))


# calculate confidence interval for year with 13 nests without EPY
qb2003 = qbeta(p = c(0.025, 0.5, 0.975), 1, 14)
nd$fit[nd$YEAR_ == 2003] = qb2003[2]
nd$lower[nd$YEAR_ == 2003] = qb2003[1]
nd$upper[nd$YEAR_ == 2003] = qb2003[3]

ggplot(data = nd) +
  geom_point(aes(x = YEAR_, y = fit, group = as.factor(NARL_site), color = as.factor(NARL_site)), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(x = YEAR_, ymin = lower, ymax = upper, group = as.factor(NARL_site)), 
                width = .1, position = position_dodge(width = 0.5)) 





### year and initiation_doy
# add Dale data
dale = read.csv2('./DATA/Dale_EPP.csv') %>% data.table

ds[, mean(initiation_doy, na.rm = TRUE)]
dale[, mean(initiation_doy, na.rm = TRUE)]

dale[, initiation_doy := initiation_doy - 23.5665]

fm = glm(anyEPY ~ initiation_doy, data = dale, family = binomial)
summary(fm)

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


plot(nd$initiation_doy, nd$fit)
ggplot(data = nd) +
  geom_line(aes(x = initiation_doy, y = fit)) +
  geom_ribbon(aes(x = initiation_doy, ymin = lower, ymax = upper), alpha = .15) 

nd2 = copy(nd)




ds_03 = ds[YEAR_ != '2003']
ds_03[, YEAR_ := factor(YEAR_)]



fm = glm(anyEPY ~ initiation_doy * YEAR_, data = ds_03, family = binomial)
summary(fm)

rs = seq(min(ds_03$initiation_doy, na.rm = TRUE), 
         max(ds_03$initiation_doy, na.rm = TRUE), 1)
years = unique(ds_03$YEAR_)

nd = data.table(YEAR_ = rep(years, each = length(rs)),
                initiation_doy = rep(rs, length(years)))

dx = ds_03[, .(min_year_ = min(initiation_doy, na.rm = TRUE), max_year = max(initiation_doy, na.rm = TRUE)), by = YEAR_]

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
  geom_line(data = nd2, aes(x = initiation_doy, y = fit)) +
  geom_ribbon(data = nd2, aes(x = initiation_doy, ymin = lower, ymax = upper), alpha = .15) +
  geom_line(aes(x = initiation_doy, y = fit, group = YEAR_, 
                color = YEAR_)) +
  geom_ribbon(aes(x = initiation_doy, ymin = lower, ymax = upper, fill = YEAR_, color = NULL), alpha = .15) +
  theme_classic(base_size = 24) + labs(x = 'Day of the year', y = 'Extra-pair young / nest')





### NARL_site and initiation_doy
fm = glm(anyEPY ~ initiation_doy * NARL_site, data = ds_03, family = binomial)
summary(fm)

rs = seq(min(ds_03$initiation_doy, na.rm = TRUE), 
         max(ds_03$initiation_doy, na.rm = TRUE), 1)
NARL_site = unique(ds_03$NARL_site)

nd = data.table(NARL_site = rep(NARL_site, each = length(rs)),
                initiation_doy = rep(rs, length(years)))

dx = ds_03[, .(min_year_ = min(initiation_doy, na.rm = TRUE), max_year = max(initiation_doy, na.rm = TRUE)), by = NARL_site]

nd = merge(nd, dx, all.x = TRUE, by = 'NARL_site')
nd[, initiation_in_year := initiation_doy > min_year_ & initiation_doy < max_year]
nd = nd[initiation_in_year == TRUE]



xmat = model.matrix(~initiation_doy * NARL_site, data = nd)

nd$fit = plogis(xmat %*% coef(fm))

nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

ggplot(data = nd) +
  geom_line(aes(x = initiation_doy, y = fit, group = NARL_site, color = NARL_site)) +
  geom_ribbon(aes(x = initiation_doy, ymin = lower, ymax = upper, fill = NARL_site, color = NULL), alpha = .15) 



### year and initiation_standardized
fm = glm(anyEPY ~ initiation_standardized * YEAR_, data = ds_03, family = binomial)
summary(fm)

rs = seq(min(ds_03$initiation_standardized, na.rm = TRUE), 
         max(ds_03$initiation_standardized, na.rm = TRUE), 1)
YEAR_ = unique(ds_03$YEAR_)

nd = data.table(YEAR_ = rep(YEAR_, each = length(rs)),
                initiation_standardized = rep(rs, length(years)))

dx = ds_03[, .(min_year_ = min(initiation_standardized, na.rm = TRUE), 
            max_year = max(initiation_standardized, na.rm = TRUE)), by = YEAR_]

nd = merge(nd, dx, all.x = TRUE, by = 'YEAR_')
nd[, initiation_in_year := initiation_standardized > min_year_ & initiation_standardized < max_year]
nd = nd[initiation_in_year == TRUE]



xmat = model.matrix(~initiation_standardized * YEAR_, data = nd)

nd$fit = plogis(xmat %*% coef(fm))

nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)


ggplot(data = nd) +
  geom_line(aes(x = initiation_standardized, y = fit, group = YEAR_, color = YEAR_)) +
  geom_ribbon(aes(x = initiation_standardized, ymin = lower, ymax = upper, fill = YEAR_, color = NULL), alpha = .15) 

ggplot(data = nd[YEAR_ == '2017' | YEAR_ == '2018' | YEAR_ == '2019']) +
  geom_line(aes(x = initiation_standardized, y = fit, group = YEAR_, color = YEAR_)) +
  geom_ribbon(aes(x = initiation_standardized, ymin = lower, ymax = upper, fill = YEAR_, color = NULL), alpha = .15) 


### NARL_site and initiation_standardized
fm = glm(anyEPY ~ initiation_standardized + NARL_site, data = ds, family = binomial)
summary(fm)

rs = seq(min(ds$initiation_standardized, na.rm = TRUE), 
         max(ds$initiation_standardized, na.rm = TRUE), 1)
NARL_site = unique(ds$NARL_site)

nd = data.table(NARL_site = rep(NARL_site, each = length(rs)),
                initiation_standardized = rep(rs, length(years)))

dx = ds[, .(min_year_ = min(initiation_standardized, na.rm = TRUE), 
            max_year = max(initiation_standardized, na.rm = TRUE)), by = NARL_site]

nd = merge(nd, dx, all.x = TRUE, by = 'NARL_site')
nd[, initiation_in_year := initiation_standardized > min_year_ & initiation_standardized < max_year]
nd = nd[initiation_in_year == TRUE]



xmat = model.matrix(~initiation_standardized + NARL_site, data = nd)

nd$fit = plogis(xmat %*% coef(fm))

nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)


# calculate confidence interval for year with 13 nests without EPY
qb2003 = qbeta(p = c(0.025, 0.5, 0.975), 1, 14)
nd$fit[nd$YEAR_ == 2003] = qb2003[2]
nd$lower[nd$YEAR_ == 2003] = qb2003[1]
nd$upper[nd$YEAR_ == 2003] = qb2003[3]


ggplot(data = nd) +
  geom_line(aes(x = initiation_standardized, y = fit, group = NARL_site, color = NARL_site)) +
  geom_ribbon(aes(x = initiation_standardized, ymin = lower, ymax = upper, fill = NARL_site, color = NULL), alpha = .15) 

### only initiation_standardized
fm = glm(anyEPY ~ initiation_standardized, data = ds, family = binomial)
summary(fm)

rs = seq(min(ds$initiation_standardized, na.rm = TRUE), 
         max(ds$initiation_standardized, na.rm = TRUE), 1)


nd = data.table(initiation_standardized = rep(rs, length(years)))

dx = ds[, .(min_year_ = min(initiation_standardized, na.rm = TRUE), 
            max_year = max(initiation_standardized, na.rm = TRUE))]

nd = cbind(nd, dx)
nd[, initiation_in_year := initiation_standardized > min_year_ & initiation_standardized < max_year]
nd = nd[initiation_in_year == TRUE]

xmat = model.matrix(~initiation_standardized, data = nd)

nd$fit = plogis(xmat %*% coef(fm))

nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

ggplot(data = nd) +
  geom_line(aes(x = initiation_standardized, y = fit)) +
  geom_ribbon(aes(x = initiation_standardized, ymin = lower, ymax = upper, color = NULL), alpha = .15) 




######### second clutches
ID2c = d[polyandrous == TRUE]$female_id_year
dx = ds[female_id_year %in% ID2c]
dx[female_clutch]

fm = glm(anyEPY ~ YEAR_, data = ds[external == 1], family = binomial)
summary(fm)

nd = data.frame(YEAR_ = unique(ds[external == 1]$YEAR_))
nd = droplevels(nd)

xmat = model.matrix(~YEAR_, data = nd)

nd$fit = plogis(xmat %*% coef(fm))

nsim = 5000
bsim = sim(fm, n.sim = nsim)

fitmat = matrix(ncol = nsim, nrow = nrow(nd))

for(i in 1:nsim) fitmat[, i] = plogis(xmat %*% bsim@coef[i, ])
nd$lower = apply(fitmat, 1, quantile, probs = 0.025)
nd$upper = apply(fitmat, 1, quantile, probs = 0.975)

qb2003 = qbeta(p = c(0.025, 0.5, 0.975), 1, 14)

nd$fit[nd$YEAR_ == 2003] = qb2003[2]
nd$lower[nd$YEAR_ == 2003] = qb2003[1]
nd$upper[nd$YEAR_ == 2003] = qb2003[3]

ggplot(data = nd) +
  geom_point(aes(x = YEAR_, y = fit)) +
  geom_errorbar(aes(x = YEAR_, ymin = lower, ymax = upper), width = .1, position = position_dodge(width = 0.5))





# per egg
fm = glm(cbind(Eggs_EPY, Eggs_no_EPY) ~ YEAR_, data = ds, family = binomial)

summary(fm)
plot(allEffects(fm))

pr = predict(fm, newdata = data.frame(YEAR_ = unique(ds$YEAR_)),type = 'response', se.fit = TRUE)
plot(pr$fit)

tb = unique(ds$YEAR_)



fm = glm(anyEPY ~ initiation_standardized, data = ds, family = binomial)
summary(fm)
plot(allEffects(fm))

fm = glm(cbind(Eggs_EPY, Eggs_no_EPY) ~ initiation_standardized, data = ds, family = binomial)
summary(fm)
plot(allEffects(fm))

# plot data
d1 = dx[!is.na(anyEPY)]
d1[anyEPY == 1, data := 'EPY']
d1[anyEPY == 0, data := 'noEPY']

d2 = copy(dx)
d2[, data := 'all']

dx = rbind(d1, d2)

ggplot(dx) +
  geom_boxplot(aes(data, initiation_y, color = data)) +
  labs(x = 'Type', y = 'Initiation date') +
  coord_flip() + facet_grid(YEAR_ ~ .) +
  theme_classic(base_size = 24)

#------------------------------------------------------------------------------------------------------------------------
# 3. Higher rates of EPP in second clutches?
#------------------------------------------------------------------------------------------------------------------------

# Higher EPY frequency in known second clutches?
# females
ID2c = ds[female_clutch == 2]$female_id
dx = ds[female_id %in% ID2c & female_clutch < 3]
dx[, .N, by = .(female_clutch, anyEPY)]
dx[, female_clutch := as.factor(female_clutch)]

# renesting with same female?
dr = merge(dx[female_clutch == 1, .(year_, nestID, female_id, m1 = male_id, anyEPY1 = anyEPY, mc1 = male_clutch)], 
           dx[female_clutch == 2, .(year_, female_id, m2 = male_id, anyEPY2 = anyEPY, mc2 = male_clutch)], 
           by = c('year_', 'female_id'), all = TRUE)

dr[, same_male := m1 == m2]
setorder(dr, female_id)
dr

ID2c = dr[same_male == FALSE | is.na(same_male)]$female_id
dx = dx[female_id %in% ID2c]
dx
dx[, .N, .(female_clutch, anyEPY)]

fm = glm(anyEPY ~ female_clutch, data = dx, family = binomial(link = 'logit'))

summary(fm)
plot(allEffects(fm))
-2.197 %>% (binomial()$linkinv)
(-2.197 + 1.350) %>% (binomial()$linkinv)
confint(fm)[1, ] %>% (binomial()$linkinv)
confint(fm)[2, ] %>% (binomial()$linkinv)

(-5.1118569 -0.9316549)  %>% (binomial()$linkinv)
(-0.5255978 +4.4764374)  %>% (binomial()$linkinv)

1.266 %>% (binomial()$linkinv)

# sperm storage?
ds[female_clutch == 2 & anyEPY == TRUE]

# con = dbcon('jkrietsch', db = 'REPHatBARROW')  
# dp = dbq(con, 'select * FROM PATERNITY')
# DBI::dbDisconnect(con)

dp[IDmother == 19222 & EPY == 1]$IDfather == ds[female_id == 19222 & female_clutch == 1]$male_id
dp[IDmother == 270170901 & EPY == 1]$IDfather == ds[female_id == 270170901 & female_clutch == 1]$male_id
dp[IDmother == 273145109 & EPY == 1]$IDfather == ds[female_id == 273145109 & female_clutch == 1]$male_id
# True in 2/3 cases where EPY
dp[IDfather == 270170235]

# males
ID2c = ds[male_clutch == 2]$male_id
dx = ds[male_id %in% ID2c & N_male_clutch > 1]
dx[, .N, by = .(male_clutch, anyEPY)]
dx[, male_clutch := as.factor(male_clutch)]

# renesting with same female?
dr = merge(dx[male_clutch == 1, .(year_, nestID, male_id, f1 = female_id, anyEPY1 = anyEPY, fc1 = female_clutch, init1 = initiation, nest_state)], 
           dx[male_clutch == 2, .(year_, male_id, f2 = female_id, anyEPY2 = anyEPY, fc2 = female_clutch, init2 = initiation)], 
           by = c('year_', 'male_id'), all = TRUE)

dr[, same_female := f1 == f2]
setorder(dr, male_id)
dr

fm = glm(anyEPY ~ male_clutch, data = dx, family = binomial)

summary(fm)
plot(allEffects(fm))

-1.2528 %>% (binomial()$linkinv)
(-1.2528 -0.8267) %>% (binomial()$linkinv)

confint(fm)[1, ] %>% (binomial()$linkinv)

1.266 %>% (binomial()$linkinv)

# plot with estimates
dd = data.table(name = c('allEPY', '1female', '2female', '1male', '2male'),
                n = 1:5, 
                estimate = c(0.11369911, 0.1000202, 0.3000626, 0.2222158, 0.111105),
                lower = c(0.08063429, 0.005988803, 0.09, 0.04, 0.01),
                upper = c(0.15799242, 0.371544221, 0.62, 0.45, 0.65))

ggplot(data = dd) +
  geom_point(aes(x = n, y = estimate)) +
  geom_errorbar(aes(x = n, ymin = lower, ymax = upper), width=.1) +
  ylim(0, 1) + 
  scale_x_discrete(name = 'Type', limits = c('all', '1female', '2female', '1male', '2male')) +
  theme_classic(base_size = 24)


# Higher EPY frequency later in the season?
dx = ds[!is.na(initiation) & !is.na(anyEPY)]

fm = glmer(anyEPY ~ initiation_standardized + (1 | YEAR_), data = dx, family = binomial(link = 'logit'))

summary(fm)
plot(allEffects(fm))

fm = glm(anyEPY ~ initiation_standardized + YEAR_, data = dx, family = binomial(link = 'logit'))

summary(fm)
plot(allEffects(fm))


fm = glm(anyEPY ~ initiation_doy + YEAR_, data = dx, family = binomial(link = 'logit'))

summary(fm)
plot(allEffects(fm))





ggplot(data = dx) + 
  geom_boxplot(aes(x = as.character(anyEPY), y = initiation_standardized)) +
  theme_classic()

# EPY in first nest around Barrow
dx = ds[!is.na(initiation) & !is.na(anyEPY) & year_ > 2016]

ggplot(dx) +
  geom_boxplot(aes(plot, initiation_y, color = plot)) +
  labs(x = 'Type', y = 'Initiation date') +
  coord_flip() + facet_grid(YEAR_ ~ .) +
  theme_classic(base_size = 24)

dx = ds[plot == 'kaleak']
dx[, .N, anyEPY]

ggplot(dx) +
  geom_boxplot(aes(as.character(anyEPY), initiation_y, color = as.character(anyEPY))) +
  labs(x = 'Type', y = 'Initiation date') +
  theme_classic(base_size = 24)

setorder(dx, initiation)
dx[, .(initiation, anyEPY)]


# Effect of seson lenght or number of nests, only using Rick's data for season lenght and N nests, but EPY from all
dx = ds[!is.na(anyEPY)]
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




















