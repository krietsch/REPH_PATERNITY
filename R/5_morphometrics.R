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
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak', 'arm', 'effects', 'multcomp'),
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

#------------------------------------------------------------------------------------------------------------------------
# 1. Tarsus and bill length extra-pair fathers
#------------------------------------------------------------------------------------------------------------------------

dcu = dc[, .(tarsus = mean(tarsus, na.rm = TRUE), culmen = mean(culmen, na.rm = TRUE), 
             weight = mean(weight, na.rm = TRUE), min_year = min(year_, na.rm = TRUE)), by = ID]
dcu[tarsus > 25, tarsus := NA]

d = merge(d, dcu, by.x = 'male_id', by.y = 'ID', all.x = TRUE)
d = merge(d, dcu[, .(ID, tarsus_EPY_father = tarsus, culmen_EPY_father = culmen, weight_EPY_father = weight, min_year_EPY_father = min_year)], 
          by.x = 'EPY_father', by.y = 'ID', all.x = TRUE)

ds = d[!is.na(tarsus) & !is.na(!culmen)]
ds[, YEAR_ := factor(YEAR_, levels = rev(sort(unique(ds$YEAR_))))]

# ds = ds[year_ > 2016]

ggplot(ds) + 
  geom_density(aes(x = tarsus), size = 1.3) +
  theme_bw(base_size = 13) 

ggplot(ds[!is.na(anyEPY)]) + 
  geom_density(aes(x = tarsus, color = as.character(anyEPY)), size = 1.3) +
  scale_color_manual(values = c('firebrick', 'dodgerblue')) +
  ylab('') +
  theme_bw(base_size = 13) 


ggplot(ds) +
  geom_point(aes(initiation_y, tarsus, color = YEAR_)) +
  geom_smooth(aes(initiation_y, tarsus, group = YEAR_), method = 'lm') +
  theme_classic(base_size = 18)

ggplot(ds) +
  geom_point(aes(initiation_y, tarsus, color = YEAR_)) +
  geom_smooth(aes(initiation_y, tarsus), method = 'lm') +
  theme_classic(base_size = 18)

hist(ds$tarsus)

fm = lm(initiation_doy ~ tarsus, data = ds)
summary(fm)

plot(allEffects(fm))

fm = lmer(initiation_doy ~ tarsus + (1 | YEAR_), data = ds)
summary(fm)

plot(allEffects(fm))
glht(fm) %>% summary

fm = lm(initiation_doy ~ culmen, data = ds)
summary(fm)

plot(allEffects(fm))


fm = lmer(initiation_doy ~ culmen + (1 | YEAR_), data = ds)
summary(fm)

plot(allEffects(fm))
glht(fm) %>% summary


dss1 = ds[!is.na(anyEPY), .(ID = male_id, tarsus, culmen, weight, anyEPY)]
dss2 = ds[!is.na(EPY_father), .(ID = EPY_father, tarsus = tarsus_EPY_father, culmen = culmen_EPY_father, 
                                weight = weight_EPY_father, anyEPY = 2)]

dss = rbind(dss1, dss2)

ggplot(dss[anyEPY > 0]) + 
  geom_density(aes(x = tarsus, color = as.character(anyEPY)), size = 1.3) +
  scale_color_manual(values = c('firebrick', 'dodgerblue')) +
  ylab('') +
  theme_bw(base_size = 13) 

dss[anyEPY == 2 & tarsus > 24]

ggplot(dss) +
  geom_boxplot(aes(as.character(anyEPY), tarsus), notch = TRUE)


ggplot(dss[anyEPY > 0]) + 
  geom_density(aes(x = weight, color = as.character(anyEPY)), size = 1.3) +
  scale_color_manual(values = c('firebrick', 'dodgerblue')) +
  ylab('') +
  theme_bw(base_size = 13) 


dss = ds[!is.na(EPY_father)]


t.test(ds[anyEPY == 1]$tarsus, dss$tarsus_EPY_father)

t.test(dss$tarsus, dss$tarsus_EPY_father)
t.test(dss$tarsus, dss$tarsus_EPY_father, paired = TRUE)


t.test(dss$culmen, dss$culmen_EPY_father)
t.test(dss$culmen, dss$culmen_EPY_father, paired = TRUE)




#------------------------------------------------------------------------------------------------------------------------
# 2. Age of extra-pair fathers
#------------------------------------------------------------------------------------------------------------------------

ds = d[!is.na(EPY_father)]
ds[, age_social_father := year_ - min_year]
ds[, age_EPY_father := year_ - min_year_EPY_father]

t.test(ds$age_social_father, ds$age_EPY_father, paired = TRUE)

dps[!is.na(EPY_father)]$EPY_father

ds = d[male_id %in% dps[!is.na(EPY_father)]$EPY_father]

merge(ds, dcu)


ds[, .N, by = male_id]




