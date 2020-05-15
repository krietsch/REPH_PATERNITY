#========================================================================================================================
# Interactions and copulations
#========================================================================================================================

# Summary
# 0. Prepare data for analysis
# 1. Interactions anaylsis
# 2. Interactions summary
# 3. Plots by ID with interactions
# 4. Copulations

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'viridis', 'asnipe', 'igraph'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW') 
d  = dbq(con, 'select * FROM RESIGHTINGS')
dc = dbq(con, 'select * FROM CAPTURES')
dn = dbq(con, 'select * FROM NESTS')
dp = dbq(con, 'select * FROM PATERNITY')
DBI::dbDisconnect(con)

# Change projection
ds = dn[is.na(lon)] # separate data without position
dn = dn[!is.na(lon)]
st_transform_DT(dn)
st_transform_DT(d)

#------------------------------------------------------------------------------------------------------------------------
# 0. Prepare data for analysis
#------------------------------------------------------------------------------------------------------------------------

# Assign locations in the study area 
point_over_poly_DT(d, poly = study_site, buffer = 10)
setnames(d, 'poly_overlap', 'study_site')

point_over_poly_DT(dn, poly = study_site, buffer = 10)
setnames(dn, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := NA]
dn = rbind(dn, ds)

# datetime
d[, datetime_ := as.POSIXct(datetime_)]
d[, datetime_y := as.POSIXct(format(datetime_, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, year_ := year(datetime_)]
dn[, initiation := as.POSIXct(initiation)]

# nestID
dn[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dp[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# ID_year
d[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]
dc[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]
dn[!is.na(male_id), male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
dn[!is.na(female_id), female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]
dp[!is.na(IDfather), IDfather_year := paste0(IDfather, '_', substr(year_, 3,4 ))]
dp[!is.na(IDmother), IDmother_year := paste0(IDmother, '_', substr(year_, 3,4 ))]

# unique nest information by ID
dm = dn[, .(year_, nestID, ID_year = male_id_year, study_site, initiation)]
df = dn[, .(year_, nestID, ID_year = female_id_year, study_site, initiation)]

dnID = rbind(dm, df)
dnID = dnID[!is.na(ID_year) & year_ > 2000]

# any nest in study site?
dnID[, any_nest := TRUE]
dnID[, any_nest_study_site := any(study_site == TRUE), by = ID_year]
dnID[!is.na(ID_year) & !is.na(initiation), first_initiation := min(initiation, na.rm = TRUE), by = ID_year]
dnID[, N_clutches := .N, by = ID_year]
dnID = unique(dnID, by = 'ID_year')

# table with pairs with nest
dnp1 = dn[!is.na(male_id) & !is.na(female_id), .(ID1 = male_id_year, ID2 = female_id_year, nestID, initiation)]
dnp2 = dnp1[, .(ID1 = ID2, ID2 = ID1, nestID, initiation)]
dnp = rbind(dnp1, dnp2)
dnp[, nest_together := 1]
dnp[!is.na(initiation), first_initiation_together := min(initiation, na.rm = TRUE), by = .(ID1, ID2)]
dnp = unique(dnp, by = c('ID1', 'ID2'))

# how many nest partners?
dnp[, N_partners := .N, by = ID1]
dnp[, .N, by = N_partners]
setorder(dnp, initiation)
dnp[, partner := seq_len(.N), by = ID1]

# first & second partner partner
ds = dnp[partner == 1]
dnp = merge(dnp, ds[, .(ID1, ID1_1st_partner = ID2)], by = 'ID1', all.x = TRUE)
ds = dnp[partner == 2]
dnp = merge(dnp, ds[, .(ID1, ID1_2nd_partner = ID2)], by = 'ID1', all.x = TRUE)
dnp[, c('N_partners', 'partner') := NULL]

# individuals seen at least once in study site
d[, seen_in_study_site := any(study_site == TRUE), by = ID_year]

# first and last seen &  tenure time
d[!is.na(ID), first_obs := min(datetime_, na.rm = TRUE), by = ID_year]
d[!is.na(ID), last_obs  := max(datetime_, na.rm = TRUE), by = ID_year]
d[!is.na(ID), tenure := as.numeric(difftime(last_obs, first_obs, units = 'days')), by = ID_year]

# number of observations per year
d[!is.na(ID), N_obs := .N, by = ID_year]

# number of copulations
d[, .N, by = cop]
d[, copAS := ifelse(!is.na(cop), 1, 0), by = 1:nrow(d)]
d[!is.na(ID), N_cop := sum(copAS, na.rm = TRUE), by = ID_year]

# on how many days seen?
d[, date_ := as.POSIXct(format(datetime_, format = '%y-%m-%d'), format = '%y-%m-%d')]
du = unique(d[!is.na(ID)], by = c('date_', 'ID_year'))
du = du[, .(N_obs_days = .N), by = ID_year]
d = merge(d, du[, .(ID_year, N_obs_days)], by = 'ID_year', all.x = TRUE)

# exclude NOBA and birds never seen in study site
d = d[!is.na(ID)]
d = d[seen_in_study_site == TRUE]

# ID's per obs_id
d[, N := .N, by = obs_id]

# check if sex in resightings fits captures
d_sex = unique(d[!is.na(ID)], by = 'ID')
dc_sex = unique(dc, by = 'ID')

dx = merge(d_sex[, .(ID, sex)], dc_sex[, .(ID, sex_observed)], by = 'ID', all.x = TRUE)
dx[, identical(sex, sex_observed)]
dx[is.na(sex_observed)]

drs = unique(d, by = 'ID_year')
drs = drs[, .(ID_year, sex)]

# females that had EPY in a clutch
dpm = dp[, any_EPY := any(EPY == 1), IDmother_year] %>% unique(., by = 'IDmother_year')
dpm = dpm[, .(year_, ID_year = IDmother_year, any_EPY)]

# males that sired EPY
dpf = dp[, any_EPY := any(EPY == 1), IDfather_year] %>% unique(., by = 'IDfather_year')
dpf = dpf[, .(year_, ID_year = IDfather_year, any_EPY)]

dpmf = rbind(dpm, dpf)
dpmf = dpmf[!is.na(ID_year)]

# EPY together
dpEPY1 = dp[EPY == 1 & !is.na(IDmother) & !is.na(IDfather), .(ID1 = IDfather_year, ID2 = IDmother_year)]
dpEPY2 = dpEPY1[, .(ID1 = ID2, ID2 = ID1)]
dpEPY = rbind(dpEPY1, dpEPY2)
dpEPY[, EPY_together := 1]
dpEPY = unique(dpEPY, by = c('ID1', 'ID2'))

#------------------------------------------------------------------------------------------------------------------------
# 1. Interactions anaylsis
#------------------------------------------------------------------------------------------------------------------------

# split data in obs_id with only one individual
d1 = d[N == 1, .(obs_id, ID1 = ID_year, ID2 = NA)]
d2 = d[N > 1]

# reshape data in long format
d2 = d2[, data.table(t(combn(ID_year, 2))), obs_id]
setnames(d2, c('obs_id', 'ID1', 'ID2'))

# have everything also in the ID
dup = d2[, .(obs_id, ID1 = ID2, ID2 = ID1)]
d2 = rbind(d2, dup)

di = rbind(d1, d2)

di = merge(di, d[, .(obs_id, ID_year, ID2sex = sex, ID2copAS = copAS)], 
           by.x = c('obs_id', 'ID2'), by.y = c('obs_id', 'ID_year'), all.x = TRUE)

di = merge(di, d[, .(obs_id, ID_year, ID1sex = sex, ID1copAS = copAS, author, year_, datetime_, 
                     datetime_y, date_, lat, lon, seen_in_study_site, N, N_obs, N_obs_days, N_cop, first_obs, last_obs)], 
           by.x = c('obs_id', 'ID1'), by.y = c('obs_id', 'ID_year'), all.x = TRUE)

# any nest? 
di = merge(di, dnID[, .(ID_year, any_nest, any_nest_study_site, first_initiation, N_clutches)], by.x = 'ID1', by.y = 'ID_year', all.x = TRUE)
di[is.na(any_nest), any_nest := FALSE]
di[is.na(any_nest_study_site), any_nest_study_site := FALSE]

# any nest together?
di = merge(di, dnp[, .(ID1, ID2, nestID, initiation, nest_together, first_initiation_together)], by = c('ID1', 'ID2'), all.x = TRUE)
di[is.na(nest_together), nest_together := 0]

# merge partner to all
di = merge(di, dnp[, .(ID1, ID1_1st_partner, ID1_2nd_partner)], by = 'ID1', all.x = TRUE)

# any EPY?
di = merge(di, dpmf[, .(ID_year, any_EPY)], by.x = 'ID1', by.y = 'ID_year', all.x = TRUE)

# EPY together?
di = merge(di, dpEPY, by = c('ID1', 'ID2'), all.x = TRUE)
di[is.na(EPY_together), EPY_together := 0]

# type of interactions
di[, interaction_ := ifelse(!is.na(ID2), 1, 0), by = 1:nrow(di)]
di[, any_interaction := any(interaction_ == 1), by = ID1]
di[, same_sex := ifelse(ID1sex == ID2sex, 1, 0), by = 1:nrow(di)]
di[, any_same_sex := any(same_sex == 1), by = ID1]
di[, any_opp_sex := any(same_sex == 0), by = ID1]
di[, copAS := ifelse(ID1copAS == 1 & ID2copAS == 1, 1, 0), by = 1:nrow(di)]
di[, N_cop_ID := sum(copAS, na.rm = TRUE), by = ID1]

# number of interactions
di[, N_interactions := sum(interaction_, na.rm = TRUE), by = ID1]

# number of unique interactions
diu = unique(di, by = c('ID1', 'ID2'))
diu[, N_interactions_unique := sum(interaction_, na.rm = TRUE), by = ID1]
diu = unique(diu, by = 'ID1')
di = merge(di, diu[, .(ID1, N_interactions_unique)], by = 'ID1', all.x = TRUE)

# number of unique copulation partner
diu = unique(di, by = c('ID1', 'ID2'))
diu = diu[copAS == 1]
diu[, N_copAS_unique := sum(copAS, na.rm = TRUE), by = ID1]
diu = unique(diu, by = 'ID1')
di = merge(di, diu[, .(ID1, N_copAS_unique)], by = 'ID1', all.x = TRUE)

# first and last time seen together
di[!is.na(ID1) & !is.na(ID2), first_obs_together := min(datetime_, na.rm = TRUE), by = .(ID1, ID2)]
di[!is.na(ID1) & !is.na(ID2), last_obs_together  := max(datetime_, na.rm = TRUE), by = .(ID1, ID2)]
di[!is.na(ID1) & !is.na(ID2), tenure_together := as.numeric(difftime(last_obs_together, first_obs_together, units = 'days')), by = .(ID1, ID2)]

# first and last time seen after nest initation 
di[any_nest == TRUE, first_obs_first_initiation := as.numeric(difftime(first_obs, first_initiation, units = 'days')), by = ID1]
di[any_nest == TRUE, last_obs_first_initiation := as.numeric(difftime(last_obs, first_initiation, units = 'days')), by = ID1]

# first_paired assuming paired at least a day before initiation
di[nest_together == 1, first_obs_together_cor_initiation := dplyr::if_else(first_obs_together < c(first_initiation_together - 86400), 
                                                                           first_obs_together, c(first_initiation_together - 86400)), by = ID1] 

# last_paired assuming paired at least 3 days after initiation (during egg laying)
di[nest_together == 1, last_obs_together_cor_initiation := dplyr::if_else(last_obs_together > c(first_initiation_together + 3*86400), 
                                                                          last_obs_together, c(first_initiation_together + 3*86400)), by = ID1]

# tenure together using also initiation data
di[!is.na(ID1) & !is.na(ID2), tenure_together_cor_initiation := as.numeric(difftime(last_obs_together_cor_initiation, 
                                                                                    first_obs_together_cor_initiation, units = 'days')), by = .(ID1, ID2)]

# seen any other birds of opposite sex while tenure together? 
ds = unique(di[ID2 == ID1_1st_partner], by = 'ID1')
di = merge(di, ds[, .(ID1, first_obs_1st_partner = first_obs_together_cor_initiation, 
                      last_obs_1st_partner = last_obs_together_cor_initiation)], by = 'ID1', all.x = TRUE)

di[, paired_1st_partner := datetime_ >= first_obs_1st_partner & datetime_ <= last_obs_1st_partner]
di[, not_1st_partner_opp_sex := same_sex == 0 & ID2 != ID1_1st_partner]
di[, contact_other_than_1st_partner_while_paired := paired_1st_partner == TRUE & not_1st_partner_opp_sex == TRUE]
di[, copulation_other_than_1st_partner_while_paired := paired_1st_partner == TRUE & not_1st_partner_opp_sex == TRUE & ID1copAS == 1]

di[contact_other_than_1st_partner_while_paired == TRUE, .N, by = ID1]
di[copulation_other_than_1st_partner_while_paired == TRUE, .N, by = ID1]

# copulation with other than first partner? 
di[ID1copAS == 1 & ID2copAS == 1 & ID2 != ID1_1st_partner, copAS_not_1st_partner := TRUE]
di[ID1copAS == 1 & ID2copAS == 1 & ID2 == ID1_2nd_partner, copAS_2nd_partner := TRUE]

# timing of this copulation
di[copAS_not_1st_partner == TRUE, copEPC_timing := difftime(datetime_, first_initiation, units = 'days') %>% as.numeric]
di[copAS_not_1st_partner == TRUE, copEPC_first := min(copEPC_timing, na.rm = TRUE), by = .(ID1, ID2)]
di[copAS_not_1st_partner == TRUE, copEPC_last := max(copEPC_timing, na.rm = TRUE), by = .(ID1, ID2)]

# second partner?
unique(di[copAS_not_1st_partner == TRUE, .(ID1, ID2, copAS_2nd_partner)], by = 'ID1')

# seen with anybody except first partner? 
di[ID2 != ID1_1st_partner & same_sex == 0, seen_with_other_than_1st_partner := TRUE]
di[ID2 == ID1_2nd_partner, seen_with_2nd_partner := TRUE]


unique(di[seen_with_other_than_1st_partner == TRUE, .(ID1, ID2, seen_with_2nd_partner)], by = c('ID1', 'ID2'))












#------------------------------------------------------------------------------------------------------------------------
# 2. Interactions summary
#------------------------------------------------------------------------------------------------------------------------

# unique ID's by year
ds = unique(di[N_obs_days > 1], by = 'ID1')
ds[, .N, by = year_]

### unique interactions
ds = unique(di[!is.na(ID2) & N_obs_days > 1], by = c('ID1', 'ID2'))

# unique interactions
dss = ds[, .N, by = ID1]

dn = dss[, .(N_int = .N), by = N]
setorder(dn, N)
dn

# no interactions
dso = unique(di[any_interaction == FALSE & N_obs_days > 1], by = 'ID1')
dss = dso[, .N, by = ID1]

dno = dss[, .(N_int = .N), by = N]
dno[, N := 0]
dn = rbind(dno, dn)

# unique opposite sex interactions
dss = ds[same_sex == 0, .N, by = ID1]

dn2 = dss[, .(N_oppo_sex_int = .N), by = N]
setorder(dn2, N)
dn2

# no opposite interactions
dso = unique(di[any_opp_sex == FALSE & N_obs_days > 1 | is.na(any_opp_sex) & N_obs_days > 1], by = 'ID1')
dss = dso[, .N, by = ID1]

dno = dss[, .(N_oppo_sex_int = .N), by = N]
dno[, N := 0]
dn2 = rbind(dno, dn2)

# unique same sex interactions
dss = ds[same_sex == 1, .N, by = ID1]

dn3 = dss[, .(N_same_sex_int = .N), by = N]
setorder(dn3, N)
dn3

# no same sex interactions
dso = unique(di[any_same_sex == FALSE & N_obs_days > 1 | is.na(any_same_sex) & N_obs_days > 1], by = 'ID1')
dss = dso[, .N, by = ID1]

dno = dss[, .(N_same_sex_int = .N), by = N]
dno[, N := 0]
dn3 = rbind(dno, dn3)

# merge
dn = merge(dn, dn2, by = 'N', all.x = TRUE)
dn = merge(dn, dn3, by = 'N', all.x = TRUE)
dn

### unique copulations total
ds = unique(di[ID1copAS == 1 & ID2copAS == 1 & !is.na(ID2)], by = c('ID1', 'ID2'))
dss = ds[, .N, by = ID1]

dn = dss[, .(N_cop = .N), by = N]
setorder(dn, N)
dn

# females 
dss = ds[ID1sex == 'F', .N, by = ID1]

dn2 = dss[, .(N_cop_F = .N), by = N]
setorder(dn2, N)
dn2

# males 
dss = ds[ID1sex == 'M', .N, by = ID1]

dn3 = dss[, .(N_cop_M = .N), by = N]
setorder(dn3, N)
dn3

# merge
dn = merge(dn, dn2, by = 'N', all.x = TRUE)
dn = merge(dn, dn3, by = 'N', all.x = TRUE)
dn

# Extra-1st partner copulations timing

ds = unique(di[!is.na(ID2)], by = c('ID1', 'ID2'))

hist(ds[copAS_not_1st_partner == TRUE]$copEPC_first)
hist(ds[copAS_not_1st_partner == TRUE]$copEPC_last)

ds[copAS_not_1st_partner == TRUE & ID1copAS == 1 & ID2copAS == 1, .(ID1, ID1sex, ID2, ID1_2nd_partner, datetime_, 
                                                                    first_initiation, copEPC_timing, copEPC_first, copEPC_last)]


############## copulations clost to first initiation 


di[contact_other_than_1st_partner_while_paired == TRUE & copulation_other_than_1st_partner_while_paired == FALSE]

setorder(di, datetime_)

di[ID1 == '273145091_19']

 diu = unique(di, by = 'ID1')

# tenure together 
ggplot(data = diu[nest_together == 1]) +
  geom_boxplot(aes(factor(year_), tenure_together_cor_initiation))

ggplot(data = diu[nest_together == 1]) +
  geom_boxplot(aes(factor(year_), tenure_together))

# any other opposite sex met during time together?


di[1,]$first_initiation_together - 86400

ds = unique(di[nest_together == 1], by = c('ID1', 'ID2'))
ds[, start_paired]


di[interaction_ == 1, interaction_while_paired := any(datetime_ < first_obs_together )]



diu[first_obs_first_initiation > 20]



bm = create_bm(di[ID1 == '273145369_19']) 

bm +
  geom_point(data = di[ID1 == '273145369_19'], aes(lon, lat))


ggplot(data = di[seen_in_study_site == TRUE]) +
  geom_point(aes(lon, lat, color = as.character(nest_together)))

ggplot(data = di[seen_in_study_site == TRUE & tenure_together > 1]) +
  geom_point(aes(lon, lat, color = as.character(nest_together)))


ggplot(data = diu[seen_in_study_site == TRUE]) +
  geom_boxplot(aes(any_nest_study_site, first_obs_first_initiation))

ggplot(data = diu[seen_in_study_site == TRUE]) +
  geom_boxplot(aes(any_nest_study_site, last_obs_first_initiation, color = ID1sex))

ggplot(data = diu[ID1sex == 'F']) +
  geom_boxplot(aes(any_nest_study_site, first_obs_first_initiation, color = as.factor(year_)))

ggplot(data = diu[ID1sex == 'F']) +
  geom_boxplot(aes(any_nest_study_site, last_obs_first_initiation, color = as.factor(year_)))

ggplot(data = diu) +
  geom_boxplot(aes(factor(year_), first_obs_first_initiation, color = ID1sex))


ggplot(data = diu[ID1sex == 'F']) +
  geom_boxplot(aes(factor(year_), last_obs_first_initiation, color = factor(N_clutches)))


diu[ID1sex == 'F' & last_obs_first_initiation > 10, .N, by = N_clutches]



ggplot(data = diu[any_nest_study_site == TRUE]) +
  geom_boxplot(aes(factor(year_), last_obs_first_initiation, color = ID1sex))


hist(diu[tenure_together > 1]$tenure_together)

diu[last_obs_first_initiation > 10 & ID1sex == 'F' & year_ == 2018, .(year_, ID1, last_obs_first_initiation)]

di[last_obs_first_initiation > 20 & ID1sex == 'F' & year_ == 2017]

ggplot(data = diu) +
  geom_boxplot(aes(any_nest, tenure_together))

ggplot(data = diu) +
  geom_boxplot(aes(as.character(nest_together), tenure_together))

diu[, .N, by = nest_together]


hist(di$N_obs)
hist(di$N_obs_days)

hist(diu$N_interactions_unique)
hist(diu$N_copAS_unique)

ggplot(data = diu) +
  geom_boxplot(aes(any_EPY, N_interactions_unique))

ggplot(data = diu) +
  geom_boxplot(aes(any_nest, N_interactions_unique))

ggplot(data = diu) +
  geom_boxplot(aes(any_EPY, N_copAS_unique))

ggplot(data = diu) +
  geom_boxplot(aes(any_nest, N_copAS_unique))

# distribution plots
cdplot(as.factor(interaction_) ~ datetime_y, data = di)
cdplot(as.factor(interaction_) ~ datetime_y, data = di[year_ == 2017])
cdplot(as.factor(interaction_) ~ datetime_y, data = di[year_ == 2018])
cdplot(as.factor(interaction_) ~ datetime_y, data = di[year_ == 2019])
cdplot(as.factor(same_sex) ~ datetime_y, data = di[interaction_ == TRUE])
cdplot(as.factor(copAS) ~ datetime_y, data = di)
cdplot(as.factor(copAS) ~ datetime_y, data = di[year_ == 2017])
cdplot(as.factor(copAS) ~ datetime_y, data = di[year_ == 2018])
cdplot(as.factor(copAS) ~ datetime_y, data = di[year_ == 2019])


# png(paste0('./REPORTS/INTERACTIONS/Copulations.png'), width = 900, height = 600)
# cdplot(as.factor(copAS) ~ datetime_y, data = di)
# dev.off()


# unique obs_id per day
ds = unique(di, by = c('obs_id'))

ggplot(data = ds) +
  geom_density(aes(datetime_y, color = as.character(year_))) +
  theme_classic()

# unique ID seen per day
ds = unique(di, by = c('ID1', 'date_'))

ggplot(data = ds) +
  geom_density(aes(datetime_y, color = as.character(year_))) +
  theme_classic()

ggplot(data = ds) +
  geom_density(aes(datetime_y, color = as.character(any_nest))) +
  theme_classic() + 
  facet_grid(.~year_)

ggplot(data = ds) +
  geom_density(aes(datetime_y, color = as.character(any_EPY))) +
  theme_classic() + 
  facet_grid(.~year_)

ggplot(data = ds) +
  geom_density(aes(datetime_y, color = as.character(ID1sex))) +
  theme_classic() + 
  facet_grid(.~year_)

# unique interaction events
ds = unique(di, by = c('obs_id'))

cdplot(as.factor(interaction_) ~ datetime_y, data = ds)
cdplot(as.factor(same_sex) ~ datetime_y, data = ds[interaction_ == TRUE])

ggplot(data = ds) +
  geom_density(aes(datetime_y, color = as.character(year_))) +
  theme_classic()

# unique copulation events
ds = unique(di[copAS == 1], by = 'obs_id')

ggplot(data = ds) +
  geom_density(aes(datetime_y, color = as.character(year_))) +
  theme_classic()

# N ID copulated with & N cop observed
setorder(di, datetime_y)
ds = di[copAS == 1]
ds = unique(ds, by = c('ID1', 'ID2'))
ds[, N_cop_partners := .N, by = ID1]


ggplot(data = ds) +
  geom_density(aes(datetime_y, color = as.character(N_cop_partners))) +
  theme_classic() + 
  facet_grid(.~year_)


# first copulation 
ds[, first_cop := min(datetime_y, na.rm = TRUE), by = ID1]
ds[, cop_after_first_partner  := datetime_y > first_cop, by = ID1]

cdplot(as.factor(cop_after_first_partner) ~ datetime_y, data = ds)
cdplot(as.factor(cop_after_first_partner) ~ datetime_y, data = ds[year_ == 2017])
cdplot(as.factor(cop_after_first_partner) ~ datetime_y, data = ds[year_ == 2018])
cdplot(as.factor(cop_after_first_partner) ~ datetime_y, data = ds[year_ == 2019])


# png(paste0('./REPORTS/INTERACTIONS/Copulations_after_first_partner.png'), width = 900, height = 600)
# cdplot(as.factor(cop_after_first_partner) ~ datetime_y, data = ds)
# dev.off()


ds = unique(ds, by = 'ID1')
ds[N_cop > 1, .N, by = ID1sex]

ds[, .N, by = N_cop_partners]
15 / 172 * 100

ds[N_cop > 1, .N, by = N_cop_partners]
15 / 81 * 100

ds[N_cop > 1 & ID1sex == 'F', .N, by = N_cop_partners]
9 / 42 * 100

ds[N_cop > 1 & ID1sex == 'M', .N, by = N_cop_partners]
6 / 39 * 100

ggplot(data = ds) +
  geom_boxplot(aes(x = as.character(N_cop_partners), y = N_cop))

ggplot(data = ds) +
  geom_boxplot(aes(x = as.character(N_cop_partners), y = N_cop_ID))

ggplot(data = ds[N_cop > 1]) +
  geom_boxplot(aes(x = as.character(N_cop_partners), y = N_cop))

# statistic
sapply( c('lme4', 'effects', 'multcomp', 'glmmTMB'),
        require, character.only = TRUE)

ds = unique(di, by = c('ID1', 'ID2'))
ds[, .N, any_EPY]

fm = glmmTMB(interaction_ ~ any_EPY + (1 | year_) +  (1 | ID1) + (1 | nest_together), data = di, family = binomial)

summary(fm)
plot(allEffects(fm))

#------------------------------------------------------------------------------------------------------------------------
# 3. Plots by ID with interactions
#------------------------------------------------------------------------------------------------------------------------

# clutch compler
dn[, nest_state_date := as.POSIXct(nest_state_date)]

nIDs = dn[!is.na(male_id) & !is.na(female_id)]$nestID %>% unique

i = 'R901_17'

dsn = dn[nestID == i]

dsm = di[ID1 == dsn$male_id_year]
dsf = di[ID1 == dsn$female_id_year]

dsn_ic = data.table(nestID = dsn$nestID,
                    datetime_ = c(dsn$initiation, dsn$nest_state_date))

ggplot() +
  geom_line(data = dsn_ic, aes(x = datetime_, y = 1)) +
  geom_point(data = dsm, aes(x = datetime_, y = 2), color = 'dodgerblue4') +
  geom_point(data = dsm, aes(x = datetime_, y = 1.5, color = not_1st_partner_opp_sex)) +
  geom_point(data = dsf, aes(x = datetime_, y = 3), color = 'firebrick2') +
  geom_point(data = dsf, aes(x = datetime_, y = 2.5, color = not_1st_partner_opp_sex)) +
  theme_classic(base_size = 18)






#------------------------------------------------------------------------------------------------------------------------
# 4. Copulations
#------------------------------------------------------------------------------------------------------------------------

d[!is.na(cop), .N, cop]
d[, copAS := ifelse(!is.na(cop), TRUE, FALSE)]

ds = d[!is.na(cop), .N, by = datetime_y]

ds = d[!is.na(cop)]

ggplot(data = ds) +
  geom_bar(aes(datetime_y, fill = study_site), position = 'stack') +
  facet_grid(year_ ~ .)
  
ds = d[!is.na(cop)]

dx = ds[, .N, obs_id]
dx[, .N, N]

# exclude NOBA and birds never seen in study site
ds = d[!is.na(cop) & !is.na(ID)]
ds[, ID_year := paste0(ID, '_', substr(year_, 3, 4))]
ds[, N := .N, obs_id]
ds = ds[N > 1]

# create matrix with observation ID by individual
gbi = get_group_by_individual(ds[, .(ID_year, obs_id)], data_format = 'individuals')

# calculate a network
netw = get_network(gbi, data_format = 'GBI')

# plot network
pn = graph.adjacency(netw, mode = 'undirected',weighted = TRUE, diag = FALSE)
plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = 4, edge.color = 'black')

# assign sex
ID_netw = unique(ds$ID_year)
dcn = d[ID_year %in% ID_netw, .(ID_year, sex)]
dcn = unique(dcn)
dcn[, ID_year := as.character(ID_year)]

ID_pn = data.table(ID_year = V(pn)$name)
ID_pn[, order := 1:nrow(ID_pn)]
ID_pn = merge(ID_pn, dcn, by = 'ID_year')
setorder(ID_pn, order)

V(pn)$sex = ID_pn$sex

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = 4, edge.color = 'black',
     vertex.color = c('red', 'blue')[1+(V(pn)$sex == 'M')])

# point size by number of observations
ds_N = ds[, .N, by = ID_year]
ds_N[, ID := as.character(ID_year)]

ID_pn = merge(ID_pn, ds_N, by = 'ID_year')
setorder(ID_pn, order)

V(pn)$size = as.numeric(ID_pn$N) %>% log

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = V(pn)$size+2, edge.color = 'black',
     vertex.color = c('red', 'blue')[1+(V(pn)$sex == 'M')])

# point color by nest or not
load('./DATA/ID_nest_information.RData') # load nest info by ID created in 1_nests_&_parentage

dnID[, any_nest := TRUE]
dnID[, ID_year := paste0(ID, '_', substr(year_, 3, 4))]

ID_pn = merge(ID_pn, dnID[, .(ID_year, any_nest, N_clutch, any_EPY)], by = 'ID_year', all.x = TRUE)
ID_pn[is.na(any_nest), any_nest := FALSE]
ID_pn[is.na(N_clutch), N_clutch := 1]
ID_pn[is.na(any_EPY), any_EPY := NA]
setorder(ID_pn, order)

V(pn)$any_nest = ID_pn$any_nest
V(pn)$N_clutch = ID_pn$N_clutch
V(pn)$any_EPY = ID_pn$any_EPY

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = V(pn)$size+2, edge.color = 'black',
     vertex.color = c('red', 'green')[1+(V(pn)$any_nest == TRUE)])


plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = V(pn)$size+2, edge.color = 'black',
     vertex.color = c('red', 'green')[1+(V(pn)$any_EPY == TRUE)], vertex.shape = c('circle', 'square')[1+(V(pn)$sex == 'F')])






# pair-wise table
dw = as.table(netw) %>% data.table
setnames(dw, c('ID', 'ID2', 'association'))

# exclude duplicated associations
dw = dw[ID < ID2]
dw = dw[association != 0]

# connect with sex 
dcu[, ID := as.character(ID)]
dw = merge(dw, dcu[, .(ID, sex_observed)], by.x = 'ID2', by.y = 'ID')
setnames(dw, 'sex_observed', 'sexID2')

dw = merge(dw, dcu[, .(ID, sex_observed)], by = 'ID')
setnames(dw, 'sex_observed', 'sexID')

# make all males to ID
dm = dw[sexID == 'M']
dm2 = dw[sexID2 == 'M']
setnames(dm2, c('ID', 'ID2', 'sexID', 'sexID2'), c('ID2', 'ID', 'sexID2', 'sexID'))
dw = rbind(dm, dm2, use.names = TRUE)

# 
dw[ID == 270170245]
dw[ID2 == 270170556]

unique(dw$ID) %>% length
unique(dw$ID2) %>% length

10/81*100

dw[, N_ID := .N, ID]
dw[, N_ID2 := .N, ID2]


dx = dw[, .N, ID2]
dx[, .N, N]


ggplot(data = dw) +
  geom_boxplot()






