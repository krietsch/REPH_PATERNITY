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
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'foreach', 'auksRuak', 'patchwork', 'cowplot'),
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

# first REPH and first copulation
d[, min(datetime_), by = .(year_, study_site)]
d[!is.na(cop), min(datetime_), by = year_]
d[!is.na(cop), max(datetime_), by = year_]
d[!is.na(cop)]

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
dm = dn[, .(year_, nestID, ID_year = male_id_year, study_site, initiation, nest_state_date)]
df = dn[, .(year_, nestID, ID_year = female_id_year, study_site, initiation, nest_state_date)]

dnID = rbind(dm, df)
dnID = dnID[!is.na(ID_year) & year_ > 2000]

# any nest in study site?
dnID[, any_nest := TRUE]
dnID[, any_nest_study_site := any(study_site == TRUE), by = ID_year]
dnID[!is.na(ID_year) & !is.na(initiation), first_initiation := min(initiation, na.rm = TRUE), by = ID_year]
dnID[!is.na(ID_year) & !is.na(initiation) & initiation != first_initiation, 
     second_initiation := min(initiation, na.rm = TRUE), by = ID_year]
dnID[, second_initiation := min(second_initiation, na.rm = TRUE), by = ID_year]
dnID[!is.na(ID_year) & !is.na(nest_state_date), first_nest_state_date := min(nest_state_date, na.rm = TRUE), by = ID_year]
dnID[!is.na(ID_year) & !is.na(nest_state_date) & nest_state_date != first_nest_state_date, 
     second_nest_state_date := min(nest_state_date, na.rm = TRUE), by = ID_year]
dnID[, second_nest_state_date := min(second_nest_state_date, na.rm = TRUE), by = ID_year]
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
di = merge(di, dnID[, .(ID_year, any_nest, any_nest_study_site, first_initiation, second_initiation, N_clutches)], 
           by.x = 'ID1', by.y = 'ID_year', all.x = TRUE)
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
di[copulation_other_than_1st_partner_while_paired == TRUE, .N, by = .(ID1, ID1sex)]

# copulation with other than first partner? 
di[ID1copAS == 1 & ID2copAS == 1 & ID2 != ID1_1st_partner, copAS_not_1st_partner := TRUE]
di[ID1copAS == 1 & ID2copAS == 1 & ID2 == ID1_2nd_partner, copAS_2nd_partner := TRUE]
di[ID1copAS == 1 & ID2copAS == 1 & ID2 != ID1_2nd_partner, copAS_not_2nd_partner := TRUE]

# timing of this copulation
di[copAS_not_1st_partner == TRUE, copEPC_timing := difftime(datetime_, first_initiation, units = 'days') %>% as.numeric]
di[copAS_not_1st_partner == TRUE, copEPC_first := min(copEPC_timing, na.rm = TRUE), by = .(ID1, ID2)]
di[copAS_not_1st_partner == TRUE, copEPC_last := max(copEPC_timing, na.rm = TRUE), by = .(ID1, ID2)]

# second partner?
unique(di[copAS_not_1st_partner == TRUE, .(ID1, ID2, copAS_2nd_partner)], by = 'ID1')

# seen with anybody except first partner? 
di[ID2 != ID1_1st_partner & same_sex == 0, seen_with_other_than_1st_partner := TRUE]
di[ID2 == ID1_2nd_partner, seen_with_2nd_partner := TRUE]

di[ID2 != ID1_2nd_partner & same_sex == 0, seen_with_other_than_2nd_partner := TRUE]

unique(di[seen_with_other_than_1st_partner == TRUE, .(ID1, ID2, seen_with_2nd_partner)], by = c('ID1', 'ID2'))

di[, diff_obs_1st_initiation := difftime(datetime_, first_initiation, units = 'days') %>% as.numeric %>% round(., 0)]
di[, diff_obs_2nd_initiation := difftime(datetime_, second_initiation, units = 'days') %>% as.numeric %>% round(., 0)]

# turn inf values in NA
invisible(lapply(names(di),function(.name) set(di, which(is.infinite(di[[.name]])), j = .name,value = NA)))

# unique by day
ds = unique(di, by = c('ID1', 'ID2', 'date_'))

t_margin = 0

#------------------------------------------------------------------------------------------------------------------------

# plot settings
bs = 11 # basesize
ls = 3 # label size
lsa = 5 # label size annotation
vline = 0.7 # size of vertical line
width_ = 1
grey_ = 'grey75'
bar_line = 'grey20'
bar_line_thickness = 0.1
margin_ = unit(c(0, 8, 2, 0), "pt")
vjust_ = 1.7 # vjust of text
vjust_label = 1.2

# colors in legend scale
c_active = '#A6D854'
c_failed = '#D53E4F'
c_previous = '#2B83BA'
c_next = '#FDAE61'

# colors in legend scale
# c_active = '#95D840FF'
# c_failed = '#33638DFF'
# c_previous = 'orange'
# c_next = 'firebrick3'

### interactions

# within pair
ds1 = ds[ID2 == ID1_1st_partner & ID1sex == 'M', .(ID1, ID2, diff_obs_initiation = diff_obs_1st_initiation, type = '1st', datetime_)]
ds2 = ds[ID2 == ID1_2nd_partner & ID1sex == 'M', .(ID1, ID2, diff_obs_initiation = diff_obs_2nd_initiation, type = '2nd', datetime_)]
dss = rbind(ds1, ds2)
sample_size1 = paste0('N = ', nrow(dss))

# color for birds with more than one clutch 
dss = merge(dss, dnID[, .(ID_year, N_clutches)], by.x = 'ID1', by.y = 'ID_year', all.x = TRUE)
dnpu = unique(dnp, by = 'ID1')

dss = merge(dss, dnpu[, .(ID1, ID1_1st_partner)], by = 'ID1', all.x = TRUE)


dss[N_clutches == 2 & ID1_1st_partner == ID2, int_with_first := TRUE]
dss[int_with_first == TRUE, type := '2nd_same']

# within pair interactions while clutch active?
da = dss[diff_obs_initiation >= 0]

da = merge(da, dn[, .(male_id_year, female_id_year, nest_state_date)], 
           by.x = c('ID1', 'ID2'), by.y = c('male_id_year', 'female_id_year'), all.x = TRUE)

da = unique(da, by = c('ID1', 'ID2', 'datetime_'))

da[, nest_still_active := difftime(nest_state_date, datetime_, units = 'days') %>% as.numeric]

da[nest_still_active > 0, nest_still_active_factor := TRUE]
da[nest_still_active < 0, nest_still_active_factor := FALSE]

dss = merge(dss, da[, .(ID1, ID2, datetime_, nest_still_active_factor)], by = c('ID1', 'ID2', 'datetime_'), all.x = TRUE)

dss[diff_obs_initiation >= 0 & nest_still_active_factor == TRUE, type2 := 'active nest']
dss[diff_obs_initiation >= 0 & nest_still_active_factor == FALSE, type2 := 'failed nest']

dss[is.na(type2), type2 := 'aabefore']

# dss = merge(dss, da[, .(ID1, ID2, datetime_, nest_still_active)], by = c('ID1', 'ID2', 'datetime_'), all.x = TRUE)
# 
# dss[diff_obs_initiation > 0 & type != '2nd_same', type2 := 'renesting']
# dss[diff_obs_initiation > 0 & type == '2nd_same', type2 := 'active nest']
# dss[is.na(type2), type2 := 'paired']

p1 = 
  ggplot(data = dss) +
  geom_bar(aes(diff_obs_initiation, fill = type2), width = width_, color = bar_line, size = bar_line_thickness) +
  geom_vline(aes(xintercept = 3), linetype = 'dotted', size = vline) + 
  scale_x_continuous(limits = c(-13, 23), labels = NULL, expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 54), labels = c('', '','20', '','40', ''), expand = c(0, 0)) +
  scale_fill_manual(values = c(grey_, c_active, c_failed)) +
  xlab('') + ylab('') +
  geom_text(aes(-9.5, Inf, label = sample_size1), vjust = vjust_, size = ls) +
  geom_text(aes(22, Inf, label = 'a'), vjust = vjust_label, size = lsa) +
  theme_classic(base_size = bs) +
  theme(legend.position = 'none', plot.margin = margin_, axis.title.x = element_blank(), axis.text.x=element_blank()) # legend.position = c(0.9, 0.9), legend.title = element_blank()

p1 


# N interactions before clutch complete 
dss[diff_obs_initiation < 4] %>% nrow
dss[type2 == 'active nest' & diff_obs_initiation > 3] %>% nrow
dss %>% nrow


# male 
ds1 = ds[seen_with_other_than_1st_partner == TRUE & same_sex == 0 & ID1sex == 'M', 
         .(ID1, ID2, diff_obs_initiation = diff_obs_1st_initiation, type = '1st', datetime_)]
ds2 = ds[seen_with_other_than_2nd_partner == TRUE & same_sex == 0 & ID1sex == 'M', 
         .(ID1, ID2, diff_obs_initiation = diff_obs_2nd_initiation, type = '2nd', datetime_)]
dss = rbind(ds1, ds2)
sample_size3 = paste0('N = ', nrow(dss))

# 1st interacting with second partner
dnpu = unique(dnp, by = 'ID1')
dss = merge(dss, dnpu[, .(ID1, ID1_1st_partner, ID1_2nd_partner)], by = 'ID1', all.x = TRUE)

dss[type == '1st' & ID2 == ID1_2nd_partner, type := 'next_partner']

# interaction with previous partner
dss[type == '2nd' & ID2 == ID1_1st_partner, type := 'previous_partner']

# factor order
dss[, type := factor(type, levels = c('1st', '2nd', 'previous_partner', 'next_partner'))]

# within pair interactions while clutch active?
da = dss[diff_obs_initiation >= 0]

da = merge(da, dn[, .(male_id_year, female_id_year, nest_state_date)], 
           by.x = c('ID1', 'ID1_1st_partner'), by.y = c('male_id_year', 'female_id_year'), all.x = TRUE)

da = unique(da, by = c('ID1', 'ID2', 'datetime_'))

da[, nest_still_active := difftime(nest_state_date, datetime_, units = 'days') %>% as.numeric]
da[nest_still_active > 0, nest_still_active_factor := TRUE]
da[nest_still_active < 0, nest_still_active_factor := FALSE]

dss = merge(dss, da[, .(ID1, ID2, datetime_, nest_still_active_factor)], by = c('ID1', 'ID2', 'datetime_'), all.x = TRUE)


dss[diff_obs_initiation >= 0 & nest_still_active_factor == TRUE, type2 := 'active nest']
dss[diff_obs_initiation >= 0 & nest_still_active_factor == FALSE, type2 := 'failed nest']
dss[is.na(type2), type2 := 'aabefore']

p2 = 
  ggplot(data = dss) +
  geom_bar(aes(diff_obs_initiation, fill = type2, width = width_), width = width_, color = bar_line, size = bar_line_thickness) +
  geom_vline(aes(xintercept = 3), linetype = 'dotted', size = vline) + 
  scale_x_continuous(limits = c(-13, 23), labels = NULL, expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 22), labels = c('', '','10', '','20'), expand = c(0, 0)) +
  scale_fill_manual(values = c(grey_, c_active, c_failed)) +
  xlab('') + ylab('Number of male-female interactions') + 
  geom_text(aes(-9.5, Inf, label = sample_size3), vjust = vjust_, size = ls) +
  geom_text(aes(22, Inf, label = 'c'), vjust = vjust_label, size = lsa) +
  theme_classic(base_size = bs) +
  theme(legend.position = 'none', plot.margin = margin_, axis.title.x = element_blank(), axis.text.x=element_blank())
p2



# N interactions while active clutch
dss[type2 == 'active nest'] %>% nrow
dss %>% nrow



# female 
ds1 = ds[seen_with_other_than_1st_partner == TRUE & same_sex == 0 & ID1sex == 'F', 
         .(ID1, ID2, diff_obs_initiation = diff_obs_1st_initiation, type = '1st', datetime_)]
ds2 = ds[seen_with_other_than_2nd_partner == TRUE & same_sex == 0 & ID1sex == 'F', 
         .(ID1, ID2, diff_obs_initiation = diff_obs_2nd_initiation, type = '2nd', datetime_)]
dss = rbind(ds1, ds2)
sample_size2 = paste0('N = ', nrow(dss))

# 1st interacting with second partner
dnpu = unique(dnp, by = 'ID1')
dss = merge(dss, dnpu[, .(ID1, ID1_1st_partner, ID1_2nd_partner)], by = 'ID1', all.x = TRUE)

dss[type == '1st' & ID2 == ID1_2nd_partner, type := 'next partner']

# interaction with previous partner
dss[type == '2nd' & ID2 == ID1_1st_partner, type := 'previous partner']

# additional ask which males had an active nest
da = dss

da = merge(da, dnID[, .(ID_year, first_initiation, second_initiation, first_nest_state_date, second_nest_state_date)], 
           by.x = c('ID2'), by.y = c('ID_year'), all.x = TRUE)

da = unique(da, by = c('ID1', 'ID2', 'datetime_'))

# nest active?
da[, active_nest1 := datetime_%between% c(first_initiation, first_nest_state_date), by = 1:nrow(da)]
da[is.na(second_initiation), active_nest1 := NA]
da[, active_nest2 := datetime_%between% c(second_initiation, second_nest_state_date), by = 1:nrow(da)]
da[is.na(second_initiation), active_nest2 := NA]
da[, active_nest := any(active_nest1 == TRUE | active_nest2 == TRUE), by = 1:nrow(da)]
da[is.na(first_initiation), no_nest := TRUE]

dss = merge(dss, da[, .(ID1, ID2, datetime_, active_nest, no_nest)], by = c('ID1', 'ID2', 'datetime_'), all.x = TRUE)
dss[type %in% c('1st', '2nd') & active_nest == TRUE, type := 'active nest']
# dss[diff_obs_initiation > 0 & type %in% c('1st', '2nd') & active_nest == FALSE, type := 'not active nest']
# dss[diff_obs_initiation > 0 & type %in% c('1st', '2nd') & no_nest == TRUE, type := 'no nest']

# factor order
dss[!(type %in% c('previous partner', 'next partner', 'active nest')), type := 'unknown']
dss[, type := factor(type, levels = c('unknown', 'active nest', 'previous partner', 'next partner'))]

p3 = 
  ggplot(data = dss) +
  geom_bar(aes(diff_obs_initiation, fill = type), width = width_, color = bar_line, size = bar_line_thickness) +
  geom_vline(aes(xintercept = 3), linetype = 'dotted', size = vline) + 
  scale_x_continuous(limits = c(-13, 23), expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 22), labels = c('0', '','10', '','20'), expand = c(0, 0)) +
  scale_fill_manual(values = c(grey_, c_active, c_previous, c_next)) + 
  xlab('Day relative to clutch initiation (= 0)') + ylab('') +
  geom_text(aes(-9.5, Inf, label = sample_size2), vjust = vjust_, size = ls) +
  geom_text(aes(22, Inf, label = 'e'), vjust = vjust_label, size = lsa) +
  theme_classic(base_size = bs) +
  theme(legend.position = 'none', plot.background = element_rect(fill = 'transparent'),
        plot.margin = margin_) # legend.position = c(0.8, 0.9), legend.title = element_blank()
p3


# N interactin with next partner while laying
dss[type == 'next partner' & diff_obs_initiation < 4]$ID1 %>% unique %>% length

# N females interacting with male with active nest
dss[type == 'active nest' & diff_obs_initiation > -1 & diff_obs_initiation < 4]$ID1 %>% unique %>% length

# Unknown status males that females interacted with during laying
dss[type == 'unknown' & diff_obs_initiation > -1 & diff_obs_initiation < 4]$ID2 %>% unique %>% length
dss[diff_obs_initiation > -1 & diff_obs_initiation < 4]$ID2 %>% unique %>% length

# # additional ask which males had an active nest
# da = dss[diff_obs_initiation > 0]
# 
# da = merge(da, dnID[, .(ID_year, first_initiation, second_initiation, first_nest_state_date, second_nest_state_date)], 
#            by.x = c('ID2'), by.y = c('ID_year'), all.x = TRUE)
# 
# da = unique(da, by = c('ID1', 'ID2', 'datetime_'))
# 
# # nest active?
# da[, active_nest1 := datetime_%between% c(first_initiation, first_nest_state_date), by = 1:nrow(da)]
# da[is.na(second_initiation), active_nest1 := NA]
# da[, active_nest2 := datetime_%between% c(second_initiation, second_nest_state_date), by = 1:nrow(da)]
# da[is.na(second_initiation), active_nest2 := NA]
# da[, active_nest := any(active_nest1 == TRUE | active_nest2 == TRUE), by = 1:nrow(da)]
# da[is.na(first_initiation), no_nest := TRUE]
# 
# dss = merge(dss, da[, .(ID1, ID2, datetime_, active_nest, no_nest)], by = c('ID1', 'ID2', 'datetime_'), all.x = TRUE)
# 
# dss[diff_obs_initiation > 0 & type %in% c('1st', '2nd') & active_nest == TRUE, type := 'active nest']
# dss[diff_obs_initiation > 0 & type %in% c('1st', '2nd') & no_nest == TRUE, type := 'no nest']
# dss[diff_obs_initiation > 0 & type %in% c('1st', '2nd') & active_nest == FALSE, type := 'not active nest']
# 
# 
# p3b =
#   ggplot(data = dss) +
#   geom_bar(aes(diff_obs_initiation, fill = type), width = width_, color = bar_line, size = bar_line_thickness) +
#   geom_vline(aes(xintercept = 3), linetype = 'dotted', size = 1.2) +
#   scale_x_continuous(limits = c(-13, 23), expand = c(0.02, 0.02)) +
#   scale_y_continuous(limits = c(0, 22), labels = c('0', '','10', '','20'), expand = c(0, 0)) +
#   scale_fill_manual(values = c(grey_, 'black', 'orange', 'orangered2', 'green', 'blue', 'yellow')) +
#   xlab('Day relative to clutch initiation') + ylab('') +
#   geom_text(aes(-10, Inf, label = sample_size2), vjust = 1, size = ls) +
#   geom_text(aes(17, Inf, label = 'females with\nextra-pair males'), vjust = 1, size = 6) +
#   theme_classic(base_size = bs) +
#   theme(panel.spacing = unit(0, "cm"), plot.margin = margin_,
#         axis.title.x = element_blank()) # legend.position = c(0.8, 0.9), legend.title = element_blank()
# p3b
# 
# dss[type == '1st']


#------------------------------------------------------------------------------------------------------------------------
### copulations
ds = unique(di[ID1copAS == 1 & ID2copAS == 1 & !is.na(ID2)], by = c('ID1', 'ID2', 'date_'))

# within pair
ds1 = ds[ID2 == ID1_1st_partner & ID1copAS == 1 & ID1sex == 'M', 
         .(ID1, ID2, diff_obs_initiation = diff_obs_1st_initiation, type = '1st', datetime_)]
ds2 = ds[ID2 == ID1_2nd_partner & ID1copAS == 1 & ID1sex == 'M', 
         .(ID1, ID2, diff_obs_initiation = diff_obs_2nd_initiation, type = '2nd'), datetime_]
dss = rbind(ds1, ds2)
sample_size4 = paste0('N = ', nrow(dss))


dss = merge(dss, dnID[, .(ID_year, N_clutches)], by.x = 'ID1', by.y = 'ID_year', all.x = TRUE)
dnpu = unique(dnp, by = 'ID1')

dss = merge(dss, dnpu[, .(ID1, ID1_1st_partner)], by = 'ID1', all.x = TRUE)


dss[N_clutches == 2 & ID1_1st_partner == ID2, int_with_first := TRUE]
dss[int_with_first == TRUE, type := '2nd_same']

dss[diff_obs_initiation > 4 & type != '2nd_same']

# within pair interactions while clutch active?
da = dss[diff_obs_initiation >= 0]

da = merge(da, dn[, .(male_id_year, female_id_year, nest_state_date)], 
           by.x = c('ID1', 'ID2'), by.y = c('male_id_year', 'female_id_year'), all.x = TRUE)

da = unique(da, by = c('ID1', 'ID2', 'datetime_'))

da[, nest_still_active := difftime(nest_state_date, datetime_, units = 'days') %>% as.numeric]

da[nest_still_active > 0, nest_still_active_factor := TRUE]
da[nest_still_active < 0, nest_still_active_factor := FALSE]

dss = merge(dss, da[, .(ID1, ID2, datetime_, nest_still_active_factor)], by = c('ID1', 'ID2', 'datetime_'), all.x = TRUE)

dss[diff_obs_initiation >= 0 & nest_still_active_factor == TRUE, type2 := 'active nest']
dss[diff_obs_initiation >= 0 & nest_still_active_factor == FALSE, type2 := 'failed nest']

dss[is.na(type2), type2 := 'aabefore']

dss[diff_obs_initiation > 2]


p4 = 
  ggplot(data = dss) +
  geom_bar(aes(diff_obs_initiation, fill = type2), width = width_, color = bar_line, size = bar_line_thickness) +
  geom_vline(aes(xintercept = 3), linetype = 'dotted', size = vline) + 
  scale_x_continuous(limits = c(-13, 23), labels = NULL, expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 27), breaks = c(0, 5, 10, 15, 20, 25), labels = c('', '', '10', '', '20', ''), expand = c(0, 0)) +
  scale_fill_manual(values = c(grey_, c_active, c_failed)) +
  xlab('') + ylab('') +
  geom_text(aes(-10, Inf, label = sample_size4), vjust = vjust_, size = ls) +
  geom_text(aes(22, Inf, label = 'b'), vjust = vjust_label, size = lsa) +
  theme_classic(base_size = bs) +
  theme(legend.position = 'none', plot.margin = margin_, axis.title.x = element_blank(), axis.text.x=element_blank())
p4 

# N interactions before clutch complete 
dss[diff_obs_initiation < 4] %>% nrow
dss[type2 == 'active nest' & diff_obs_initiation > 3] %>% nrow
dss %>% nrow

dss[type2 == 'active nest' & diff_obs_initiation > 3]

# male 
ds1 = ds[copAS_not_1st_partner == TRUE & ID1sex == 'M', 
         .(ID1, ID2, diff_obs_initiation = diff_obs_1st_initiation, type = '1st', datetime_)]
ds2 = ds[copAS_not_2nd_partner == TRUE & ID1sex == 'M', 
         .(ID1, ID2, diff_obs_initiation = diff_obs_2nd_initiation, type = '2nd', datetime_)]
dss = rbind(ds1, ds2)
sample_size6 = paste0('N = ', nrow(dss))

# 1st interacting with second partner
dnpu = unique(dnp, by = 'ID1')
dss = merge(dss, dnpu[, .(ID1, ID1_1st_partner, ID1_2nd_partner)], by = 'ID1', all.x = TRUE)

dss[type == '1st' & ID2 == ID1_2nd_partner, type := 'next_partner']

# interaction with previous partner
dss[type == '2nd' & ID2 == ID1_1st_partner, type := 'previous_partner']

# within pair interactions while clutch active?
da = dss[diff_obs_initiation >= 0]

da = merge(da, dn[, .(male_id_year, female_id_year, nest_state_date)], 
           by.x = c('ID1', 'ID1_1st_partner'), by.y = c('male_id_year', 'female_id_year'), all.x = TRUE)

da = unique(da, by = c('ID1', 'ID2', 'datetime_'))

da[, nest_still_active := difftime(nest_state_date, datetime_, units = 'days') %>% as.numeric]
da[nest_still_active > 0, nest_still_active_factor := TRUE]
da[nest_still_active < 0, nest_still_active_factor := FALSE]

dss = merge(dss, da[, .(ID1, ID2, datetime_, nest_still_active_factor)], by = c('ID1', 'ID2', 'datetime_'), all.x = TRUE)


dss[diff_obs_initiation >= 0 & nest_still_active_factor == TRUE, type2 := 'active nest']
dss[diff_obs_initiation >= 0 & nest_still_active_factor == FALSE, type2 := 'failed nest']
dss[is.na(type2), type2 := 'aabefore']

dss[, .N, type2]
dss[diff_obs_initiation < 0]

p5 = 
  ggplot(data = dss) +
  geom_bar(aes(diff_obs_initiation, fill = type2), width = width_, color = bar_line, size = bar_line_thickness) +
  geom_vline(aes(xintercept = 3), linetype = 'dotted', size = vline) + 
  scale_x_continuous(limits = c(-13, 23), labels = NULL, expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 11), breaks = c(0, 2.5, 5, 7.5, 10), labels = c('', '', '5', '', '10'), expand = c(0, 0)) +
  scale_fill_manual(values = c(grey_, c_active, c_failed)) +
  xlab('') + ylab('Number of copulation attempts') +
  geom_text(aes(-10, Inf, label = sample_size6), vjust = vjust_, size = ls) +
  geom_text(aes(22, Inf, label = 'd'), vjust = vjust_label, size = lsa) +
  theme_classic(base_size = bs) +
  theme(legend.position = 'none', plot.margin = margin_, axis.title.x = element_blank(), axis.text.x=element_blank())
p5


# female 
ds1 = ds[copAS_not_1st_partner == TRUE & ID1sex == 'F', 
         .(ID1, ID2, diff_obs_initiation = diff_obs_1st_initiation, type = '1st', datetime_)]
ds2 = ds[copAS_not_2nd_partner == TRUE & ID1sex == 'F', 
         .(ID1, ID2, diff_obs_initiation = diff_obs_2nd_initiation, type = '2nd', datetime_)]
dss = rbind(ds1, ds2)
sample_size5 = paste0('N = ', nrow(dss))

# 1st interacting with second partner
dnpu = unique(dnp, by = 'ID1')
dss = merge(dss, dnpu[, .(ID1, ID1_1st_partner, ID1_2nd_partner)], by = 'ID1', all.x = TRUE)

dss[type == '1st' & ID2 == ID1_2nd_partner, type := 'next partner']

# interaction with previous partner
dss[type == '2nd' & ID2 == ID1_1st_partner, type := 'previous partner']

dss[diff_obs_initiation > 0 & type == 'previous partner']

# additional ask which males had an active nest
da = dss

da = merge(da, dnID[, .(ID_year, first_initiation, second_initiation, first_nest_state_date, second_nest_state_date)], 
           by.x = c('ID2'), by.y = c('ID_year'), all.x = TRUE)

da = unique(da, by = c('ID1', 'ID2', 'datetime_'))

# nest active?
da[, active_nest1 := datetime_%between% c(first_initiation, first_nest_state_date), by = 1:nrow(da)]
da[is.na(second_initiation), active_nest1 := NA]
da[, active_nest2 := datetime_%between% c(second_initiation, second_nest_state_date), by = 1:nrow(da)]
da[is.na(second_initiation), active_nest2 := NA]
da[, active_nest := any(active_nest1 == TRUE | active_nest2 == TRUE), by = 1:nrow(da)]
da[is.na(first_initiation), no_nest := TRUE]

dss = merge(dss, da[, .(ID1, ID2, datetime_, active_nest, no_nest)], by = c('ID1', 'ID2', 'datetime_'), all.x = TRUE)
dss[diff_obs_initiation > 0 & type %in% c('1st', '2nd') & active_nest == TRUE, type := 'active nest']
# dss[diff_obs_initiation > 0 & type %in% c('1st', '2nd') & active_nest == FALSE, type := 'not active nest']
# dss[diff_obs_initiation > 0 & type %in% c('1st', '2nd') & no_nest == TRUE, type := 'no nest']

# factor order
dss[!(type %in% c('previous partner', 'next partner', 'active nest')), type := 'unknown']
dss[, type := factor(type, levels = c('unknown', 'active nest', 'previous partner', 'next partner'))]

dss[type == 'active nest']


p6 = 
  ggplot(data = dss) +
  geom_bar(aes(diff_obs_initiation, fill = type), width = width_, color = bar_line, size = bar_line_thickness) +
  geom_vline(aes(xintercept = 3), linetype = 'dotted', size = vline) + 
  scale_x_continuous(limits = c(-13, 23), expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 11), breaks = c(0, 2.5, 5, 7.5, 10), labels = c('0', '', '5', '', '10'), expand = c(0, 0)) +
  scale_fill_manual(values = c(grey_, c_active, c_previous, c_next)) + 
  xlab('Day relative to clutch initiation (= 0)') + ylab('') +
  geom_text(aes(-10, Inf, label = sample_size5), vjust = vjust_, size = ls) +
  geom_text(aes(22, Inf, label = 'f'), vjust = vjust_label, size = lsa) +
  theme_classic(base_size = bs) +
  theme(legend.position = 'none', plot.background = element_rect(fill = 'transparent'), 
        plot.margin = margin_)
p6

# N interactin with next partner while laying
dss[type == 'next partner' & diff_obs_initiation < 4]$ID1 %>% unique %>% length



# legend
ds = data.table(type = c('with active nest', 'with failed nest', 'next partner', 'previous partner'),
                N = c(rep(1, 4)))

ds[, type := factor(type, levels =c('with active nest', 'with failed nest', 'previous partner', 'next partner'))]

pl = 
ggplot(data = ds) +
  geom_bar(aes(N, fill = type)) +
  scale_fill_manual(name = 'Male', values = c(c_active, c_failed, c_previous, c_next)) + 
  theme_classic(base_size = bs) +
  theme(legend.position = 'top', legend.key.width = unit(0.4, 'cm'), legend.key.height = unit(0.4, 'cm'))

legend = ggpubr::get_legend(pl)
plgg = ggpubr::as_ggplot(legend)


# plot 
# patchwork <- (plot_spacer() + p1 + p4) / (plot_spacer() + p2 + p5) / (plot_spacer() + p3 + p6) / (plot_spacer() + plgg)
# 
# patchwork[[1]] <- patchwork[[1]] + plot_layout(tag_level = 'new') + plot_layout(widths = c(1, 2, 2))
# patchwork[[2]] <- patchwork[[2]] + plot_layout(tag_level = 'new') + plot_layout(widths = c(1, 2, 2))
# patchwork[[3]] <- patchwork[[3]] + plot_layout(tag_level = 'new') + plot_layout(widths = c(1, 2, 2))
# patchwork[[4]] <- patchwork[[4]] + plot_layout(tag_level = 'new') + plot_layout(widths = c(0.5, 2))
# patchwork + plot_layout(heights = c(3, 2, 2, 0.2)) 
# 
# 
# ggsave('./REPORTS/FIGURES/Figure3_.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')


# include icon

pi1 = 
  ggdraw() +
  draw_image('./DATA/reph_icon1.tif') +
  geom_text(aes(0.5, 0.5, label = 'Within-pair'), vjust = 3, size = 4) 
pi1

pi2 = 
  ggdraw() +
  draw_image('./DATA/reph_icon2.tif') +
  geom_text(aes(0.5, 0.5, label = 'Extra-pair'), vjust = 3, size = 4) 
pi2

pi3 = 
  ggdraw() +
  draw_image('./DATA/reph_icon3.tif') +
  geom_text(aes(0.5, 0.5, label = 'Extra-pair'), vjust = 3, size = 4) 
pi3


# plot with icon
patchwork <- (pi1 + p1 + p4) / (pi2 + p2 + p5) / (pi3 + p3 + p6) / (plot_spacer() + plgg)

patchwork[[1]] <- patchwork[[1]] + plot_layout(widths = c(1, 2, 2))
patchwork[[2]] <- patchwork[[2]] + plot_layout(widths = c(1, 2, 2))
patchwork[[3]] <- patchwork[[3]] + plot_layout(widths = c(1, 2, 2))
patchwork[[4]] <- patchwork[[4]] + plot_layout(widths = c(0.5, 2))
patchwork + plot_layout(heights = c(3, 2, 2, 0.2)) 


ggsave('./REPORTS/FIGURES/Figure3_icon_new.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')












