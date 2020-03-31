#========================================================================================================================
# Nest data summary & Parentage analysis
#========================================================================================================================

### TO DO
# 1. Nests data avaiable
# - clear definition which data to use (exclude nests never in study site)
# - How many second clutches of females, males?
# - timing of second clutches and renesting (include Schamel & Tracy)
# - initiation dates for how many?
# - Initiation / seson length  explained by snow cover

# 2 EPP data avaiable
# - N nest with at least 1 ID assigned
# - N EPP fathers assigned (known)
# - EPP per year
# - EPP explained by length of the season?

# 3. Anything know about EPP fathers?
# - seen together?
# - seen for how long?
# - N interactions difference EPP females / EPP fathers
# - copulation data? anything useful to say?



# Summary
# 1. Assign locations in the study area
# 2. Data summary
# 3. Extra-pair young by nest
# 4. Timing of clutch initiation / comparision Schamel & Tracy
# 5. Timing of clutch initiation and snow cover
# 6. Nest info by ID
# 7. Maps of nests

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

# check data
# ggplot() + 
#   geom_sf(data = study_site, fill = 'grey95') +
#   geom_point(data = d, aes(lon, lat, color = study_site))

# merge with data without position
ds[, study_site := NA]
d = rbind(d, ds)

# bring everything in the right format
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
d[, initiation := anytime(initiation)]
d[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, initiation_doy := yday(initiation)]
d[, YEAR_ := factor(year_)]
d[, complete := initiation_y + clutch_size * 86400 - 86400]

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
d = merge(d, dps[, .(nestID, N_parentage = N, N_EPY = EPY, anyEPY, EPY_father)], by = 'nestID', all.x = TRUE)

# assign all nests with parentage data
d[, parentage := ifelse(!is.na(N_parentage), TRUE, FALSE)]
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
ds3 = d[external == 0 & anyEPY == TRUE, .(N_parentage = .N), by = .(year_, study_site)]
ds = merge(ds, ds2, by = c('year_', 'study_site'), all.x = TRUE)
ds = merge(ds, ds3, by = c('year_', 'study_site'), all.x = TRUE)

setorder(ds, year_, -study_site)
ds

# Rick's data
ds =  d[external == 1, .N, by = .(year_, study_site)]
ds2 = d[external == 1 & parentage == TRUE, .(N_parentage = .N), by = .(year_, study_site)]
ds3 = d[external == 1 & anyEPY == TRUE, .(N_parentage = .N), by = .(year_, study_site)]
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
ds[, full_clutch_sampled := clutch_size == N_parentage]
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


# from which plots do we have parentage data
ds = d[external == 1 & parentage == TRUE]
ds[, .N, by = plot]

# position of nests
bm = create_bm(d)

bm +
  geom_point(data = d, aes(lon, lat, color = plot))

# nest for which we have parentage data
bm +
  geom_point(data = d[parentage == TRUE], aes(lon, lat, color = as.character(year_)))

setorder(d, year_, initiation)

# initiation date by category
d[, external_study_site := paste0(external, study_site)]

ggplot(d[year_ > 2016]) +
  geom_boxplot(aes(external_study_site, initiation_y, color = external_study_site)) +
  labs(x = 'Extra-pair young', y = 'Initiation date') +
  coord_flip() + facet_grid(year_ ~ .) +
  theme_classic(base_size = 24)

ggplot(d[year_ > 2016 & parentage == TRUE]) +
  geom_boxplot(aes(as.character(anyEPY), initiation_y, color = as.character(anyEPY))) +
  labs(x = 'Extra-pair young', y = 'Initiation date') +
  coord_flip() + facet_grid(year_ ~ .) +
  theme_classic(base_size = 24)

# first and second clutches by females
d[, N_female_clutch := .N, by = female_id]
d[, female_clutch := seq_len(.N), by = .(female_id, year_)]
d[is.na(female_id), female_clutch := 1]
d[!is.na(female_id), N_female_clutch := max(female_clutch), by = .(female_id, year_)]
d[is.na(female_id), N_female_clutch := 1]
d[, .N, by = .(year_, female_clutch)]
d[, .N, by = .(female_clutch, external)]

# feamles nesting on multiple years
ds = unique(d[!is.na(female_id)], by = c('female_id', 'year_'))
ds[, female_clutch_years := seq_len(.N), by = female_id]
ds[, .N, by = .(female_clutch_years)]

# males renesting
d[, N_male_clutch := .N, by = male_id]
d[, male_clutch := seq_len(.N), by = .(male_id, year_)]
d[is.na(male_id), male_clutch := 1]
d[!is.na(male_id), N_male_clutch := max(male_clutch), by = .(male_id, year_)]
d[is.na(male_id), N_male_clutch := 1]
d[, .N, by = .(year_, male_clutch)]
d[, .N, by = .(male_clutch, external)]

# males nesting on multiple years
ds = unique(d[!is.na(male_id)], by = c('male_id', 'year_'))
ds[, male_clutch_years := seq_len(.N), by = male_id]
ds[, .N, by = .(male_clutch_years)]
ds[, .N, by = .(male_clutch_years, external)]

#------------------------------------------------------------------------------------------------------------------------
# 2. Extra-pair young by nest on plots
#------------------------------------------------------------------------------------------------------------------------

sapply( c('lme4', 'effects', 'multcomp', 'glmmTMB'),
        require, character.only = TRUE)

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

ggplot(dx) +
  geom_boxplot(aes(as.character(external), initiation_y, color = as.character(external)), notch = TRUE) +
  coord_flip() + facet_grid(year_ ~ .) +
  theme_classic(base_size = 24)


# dx = d[plot %in% c('brw1', 'brw2', 'brw3')]
# ggplot(dx) +
#   geom_boxplot(aes(as.character(plot), initiation_y, color = as.character(plot))) +
#   coord_flip() + facet_grid(year_ ~ .) +
#   theme_classic(base_size = 24)

ggplot(d) +
  geom_boxplot(aes(as.character(plot), initiation_y, color = as.character(plot))) +
  theme_classic(base_size = 24)

dx = d[study_site == TRUE]
fm = lmer(initiation_doy ~ plot +  (1 | YEAR_), data = dx)

glht(fm) %>% summary
plot(allEffects(fm))

# Use all 
ds = d[, .(external, study_site, plot, year_, YEAR_, nestID, male_id, female_id, anyEPY, N_parentage, N_EPY, parentage, any_parentage_year, 
           initiation, initiation_y, initiation_doy, female_clutch, N_female_clutch, male_clutch, N_male_clutch, nest_state)]

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

# EPY frequency 
ds[, external := factor(external)]

# data available
dx %>% nrow
dx[, .N, YEAR_]
Nm = dx[, .(N = .N), by = male_id]
Nm[, .N, by = N]
Nf = dx[, .(N = .N), by = female_id]
Nf[, .N, by = N]

# EPY with SE
fm = glmmTMB(anyEPY ~ 1 +  ( 1 | YEAR_), data = ds, family = binomial)
# fm = glmer(anyEPY ~ 1 +  ( 1 | YEAR_), data = ds, family = binomial)

mean(ds$anyEPY)
sd(ds$anyEPY)

summary(fm)
confint(fm)[1, ] %>% (binomial()$linkinv)
-2.054 %>% (binomial()$linkinv)
0.194 %>% (binomial()$linkinv)
-10.58 %>% (binomial()$linkinv)

fm = glm(anyEPY ~ 1, data = ds, family = binomial)
summary(fm)
-2.0505 %>% (binomial()$linkinv)

# own data
fm = glm(anyEPY ~ 1, data = ds[external == 0], family = binomial)
summary(fm)
-2.1493 %>% (binomial()$linkinv)

# external
fm = glm(anyEPY ~ 1, data = ds[external == 1], family = binomial)
summary(fm)
-1.9188 %>% (binomial()$linkinv)


# difference between external or not?
fm1 = glmmTMB(anyEPY ~ external +  ( 1 | YEAR_), data = ds, family = binomial)

summary(fm1)
AIC(fm, fm1) # no difference, can combine data

# difference between years?
fm1 = glm(anyEPY ~ YEAR_, data = ds, family = binomial)

summary(fm1)
plot(allEffects(fm1))
summary(glht(fm1, mcp(YEAR_ = 'Tukey')))

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
# 3. Higher rates if EPP in second clutches?
#------------------------------------------------------------------------------------------------------------------------

# Higher EPY frequency in known second clutches?
# females
ID2c = ds[female_clutch == 2]$female_id
dx = ds[female_id %in% ID2c & female_clutch < 3]
dx[, .N, by = .(female_clutch, anyEPY)]
dx[, female_clutch := as.factor(female_clutch)]

# renesting with same female?
dr = merge(dx[female_clutch == 1, .(year_, nestID, female_id, m1 = male_id, anyEPY1 = anyEPY, N_parentage1 =  N_parentage, 
                                    N_EPY1 = N_EPY, mc1 = male_clutch)], 
           dx[female_clutch == 2, .(year_, female_id, m2 = male_id, anyEPY2 = anyEPY, N_parentage2 =  N_parentage, 
                                    N_EPY2 = N_EPY, mc2 = male_clutch)], 
           by = c('year_', 'female_id'), all = TRUE)

dr[, same_male := m1 == m2]
setorder(dr, female_id)
dr

dr[, .(sum(N_parentage1), sum(N_EPY1))] # fist clutch
dr[, .(sum(N_parentage2), sum(N_EPY2))] # second clutch


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
dr = merge(dx[male_clutch == 1, .(year_, nestID, male_id, f1 = female_id, anyEPY1 = anyEPY, fc1 = female_clutch, N_parentage1 =  N_parentage, 
                                  N_EPY1 = N_EPY, init1 = initiation, nest_state)], 
           dx[male_clutch == 2, .(year_, male_id, f2 = female_id, anyEPY2 = anyEPY, fc2 = female_clutch, N_parentage2 =  N_parentage, 
                                  N_EPY2 = N_EPY, init2 = initiation)], 
           by = c('year_', 'male_id'), all = TRUE)

dr[, same_female := f1 == f2]
setorder(dr, male_id)
dr

dr[, .(sum(N_parentage1), sum(N_EPY1))] # fist clutch
dr[same_female == TRUE, .(sum(N_parentage2), sum(N_EPY2))] # second clutch


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
                                    
fm = glmmTMB(anyEPY ~ initiation_standardized + (1 | YEAR_), data = dx, family = binomial(link = 'logit'))

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





