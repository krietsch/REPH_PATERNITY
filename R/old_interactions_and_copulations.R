# Summary
# 1. Assign locations in the study area
# 2. Data summary

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'viridis', 'asnipe', 'igraph'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW') 
d = dbq(con, 'select * FROM RESIGHTINGS')
dc = dbq(con, 'select * FROM CAPTURES')
dn = dbq(con, 'select * FROM NESTS')
dp = dbq(con, 'select * FROM PATERNITY')
DBI::dbDisconnect(con)

# load nest info by ID created in 1_nests_&_parentage
load('./DATA/ID_nest_information.RData') 
dnID[, any_nest := TRUE]

# Change projection
ds = dn[is.na(lon)] # seperate data without position
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

# dnID adjustmensts
dn[, male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
dn[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]

# table with pairs with nest
dnp1 = dn[!is.na(male_id) & !is.na(female_id), .(ID1 = male_id_year, ID2 = female_id_year)]
dnp2 = dnp1[, .(ID1 = ID2, ID2 = ID1)]
dnp = rbind(dnp1, dnp2)
dnp[, nest_together := 1]
dnp = unique(dnp, by = c('ID1', 'ID2'))

# datetime
d[, datetime_ := anytime(datetime_)]
d[, datetime_y := as.POSIXct(format(datetime_, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, year_ := year(datetime_)]

# assign categories of interest
d[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]

# individuals seen at least once in study site
d[, seen_in_study_site := any(study_site == TRUE), by = .(year_, sex)]

# exclude NOBA and birds never seen in study site
d = d[!is.na(ID)]
d = d[seen_in_study_site == TRUE]

# first and last seen &  tenure time
d[, first_obs := min(datetime_, na.rm = TRUE), by = .(year_, ID)]
d[, last_obs  := max(datetime_, na.rm = TRUE), by = .(year_, ID)]
d[, tenure := as.numeric(difftime(last_obs, first_obs, units = 'days')), by = .(year_, ID)]

# number of observations per year
d[, N_obs := .N, by = .(year_, ID)]

# number of copulations
d[, .N, by = cop]
d[, copAS := ifelse(!is.na(cop), 1, 0), by = 1:nrow(d)]
d[, N_cop := sum(copAS, na.rm = TRUE), by = .(year_, ID)]

# on how many days seen?
d[, date_ := as.Date(datetime_)]
du = unique(d, by = c('date_', 'ID_year'))
du = du[, .(N_obs_days = .N), by = ID_year]
d = merge(d, du[, .(ID_year, N_obs_days)], by = 'ID_year', all.x = TRUE)

# ID's per obs_id
d[, N := .N, by = obs_id]

# check if sex in resightings fits captures
d_sex = unique(d, by = 'ID')
dc_sex = unique(dc, by = 'ID')

dx = merge(d_sex[, .(ID, sex)], dc_sex[, .(ID, sex_observed)], by = 'ID', all.x = TRUE)
dx[, identical(sex, sex_observed)]

drs = unique(d, by = 'ID_year')
drs = drs[, .(ID_year, sex)]

# dnID adjustmensts
dnID[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]
dnID[, pID_year := paste0(pID, '_', substr(year_, 3,4 ))]

# females that had EPY in a clutch
dp[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dp[, IDfather_year := paste0(IDfather, '_', substr(year_, 3,4 ))]
dp[, IDmother_year := paste0(IDmother, '_', substr(year_, 3,4 ))]
dpm = dp[, anyEPY := any(EPY == 1), nestID] %>% unique(., by = 'IDmother')
dpm = dpm[, .(year_, IDmother, anyEPY)]
dpm[, ID_year := paste0(IDmother, '_', substr(year_, 3,4 ))]

# males that sired EPY
dpf = dp[EPY == 1, .(year_, IDfather)]
dpf[, ID_year := paste0(IDfather, '_', substr(year_, 3,4 ))]
dpf[, siredEPY := 1]

# EPY together
dpEPY1 = dp[EPY == 1 & !is.na(IDmother) & !is.na(IDfather), .(ID1 = IDfather_year, ID2 = IDmother_year)]
dpEPY2 = dpEPY1[, .(ID1 = ID2, ID2 = ID1)]
dpEPY = rbind(dpEPY1, dpEPY2)
dpEPY[, EPY_together := 1]
dpEPY = unique(dpEPY, by = c('ID1', 'ID2'))

#------------------------------------------------------------------------------------------------------------------------
# 1. Interactions
#------------------------------------------------------------------------------------------------------------------------

ds = d[, .(ID, ID_year, obs_id)]
unique(ds$ID_year) %>% length

# create matrix with observation ID_year by individual
gbi = get_group_by_individual(ds[, .(ID_year, obs_id)], data_format = 'individuals')

# calculate a network
netw = get_network(gbi, data_format = 'GBI')

# plot network
pn = graph.adjacency(netw, mode = 'undirected',weighted = TRUE, diag = FALSE)
plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = 4, edge.color = 'black')

# assign sex
ID_netw = unique(ds$ID_year)
dcn = drs[ID_year %in% ID_netw, .(ID_year, sex)]
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
ID_pn = merge(ID_pn, dnID[, .(ID_year, any_nest, N_clutch)], by = 'ID_year', all.x = TRUE)
ID_pn[is.na(any_nest), any_nest := FALSE]
ID_pn[is.na(N_clutch), N_clutch := 1]
setorder(ID_pn, order)

V(pn)$any_nest = ID_pn$any_nest
V(pn)$N_clutch = ID_pn$N_clutch

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = V(pn)$size+2, edge.color = 'black',
     vertex.color = c('red', 'green')[1+(V(pn)$any_nest == TRUE)])

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = V(pn)$size+2, edge.color = 'black',
     vertex.color = c('red', 'green')[1+(V(pn)$any_nest == TRUE)], vertex.shape = c('circle', 'square')[1+(V(pn)$sex == 'F')])

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = V(pn)$size+2, edge.color = 'black',
     vertex.color = c('red', 'green')[1+(V(pn)$any_nest == TRUE)], vertex.shape = c('circle', 'square')[1+(V(pn)$N_clutch == 2)])

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = V(pn)$size+2, edge.color = 'black', edge.color = 'black',
     vertex.color = c('red', 'blue')[1+(V(pn)$sex == 'M')], vertex.shape = c('circle', 'square')[1+(V(pn)$N_clutch == 2)])


# pair-wise table
dw = as.table(netw) %>% data.table
setnames(dw, c('ID', 'ID2', 'association'))

# exclude duplicated associations
# dw = dw[ID < ID2]
dw = dw[association != 0]

# merge with nest data
dw = merge(dw, dn[, .(male_id_year, female_id_year, pair1 = TRUE)], by.x = c('ID', 'ID2'), by.y = c('male_id_year', 'female_id_year'), all.x = TRUE)
dw = merge(dw, dn[, .(male_id_year, female_id_year, pair2 = TRUE)], by.x = c('ID', 'ID2'), by.y = c('female_id_year', 'male_id_year'), all.x = TRUE)
dw[pair1 == TRUE | pair2 == TRUE, pair := TRUE]
dw[is.na(pair), pair := FALSE]
dw[, pair1 := NULL]
dw[, pair2 := NULL]

# paired ID's
dw[, any_pair := any(pair == TRUE), by = ID]
dw[pair == TRUE, partner := 'nest_partner']
dw[pair == FALSE & any_pair == TRUE, partner := 'nest_other_contacts']
dw[is.na(partner), partner := 'unknown']

# assign sex
ID_netw = unique(dw$ID)
dcn = drs[ID_year %in% ID_netw, .(ID_year, sex)]
dcn = unique(dcn)
dcn[, ID_year := as.character(ID_year)]

dw = merge(dw, dcn[, .(ID_year, IDsex = sex)], by.x = 'ID', by.y = 'ID_year', all.x = TRUE)
dw = merge(dw, dcn[, .(ID_year, ID2sex = sex)], by.x = 'ID2', by.y = 'ID_year', all.x = TRUE)

# assign breeding
dw = merge(dw, dnID[, .(ID_year, IDany_nest = any_nest, IDN_clutch = N_clutch)], by.x = 'ID', by.y = 'ID_year', all.x = TRUE)
dw[is.na(IDany_nest), IDany_nest := FALSE]
dw[is.na(IDN_clutch), IDN_clutch := FALSE]

# assign females with any EPY or none
dw = merge(dw, dpm[, .(ID_year, anyEPY)], by.x = 'ID', by.y = 'ID_year', all.x = TRUE)

# assign males that sired EPY
dw = merge(dw, dpf[, .(ID_year, siredEPY)], by.x = 'ID', by.y = 'ID_year', all.x = TRUE)

# days a ID was seen
dID = unique(d, by = 'ID_year')
dw = merge(dw, dID[, .(ID_year, first_obs, last_obs, tenure, N_obs, N_obs_days)], by.x = 'ID', by.y = 'ID_year', all.x = TRUE)

dw[, first_obs_y := as.POSIXct(format(first_obs, format = '%m-%d h:m:s'), format = '%m-%d h:m:s')]
dw[, last_obs_y := as.POSIXct(format(last_obs, format = '%m-%d h:m:s'), format = '%m-%d h:m:s')]

# number of ID's interacted with
dw[, N_interactions := .N, by = ID]
dw[, N_interactions_per_obs_day := N_interactions / N_obs_days]

# same sex?
dw[, same_sex := IDsex == ID2sex]


ds = unique(dw, by = 'ID')
ggplot(data = ds) +
  geom_point(aes(tenure, N_interactions))

ggplot(data = ds) +
  geom_point(aes(N_obs_days, N_obs))

ggplot(data = ds) +
  geom_point(aes(N_interactions_per_obs_day, N_interactions, color = anyEPY))

ggplot(data = ds) +
  geom_boxplot(aes(x = anyEPY, y = N_interactions_per_obs_day))

ggplot(data = ds) +
  geom_boxplot(aes(x = anyEPY, y = N_obs))

ggplot(data = ds) +
  geom_boxplot(aes(IDany_nest, N_interactions_per_obs_day, color = IDsex))

ggplot(data = ds[same_sex == FALSE]) +
  geom_boxplot(aes(IDsex, N_interactions_per_obs_day, color = IDsex))

fm = lm(N_interactions_per_obs_day ~ IDsex, data = ds)
summary(fm)

ds[same_sex == FALSE, .(mean_interactions_per_obs_day = mean(N_interactions_per_obs_day), 
                        sd_interactions_per_obs_day = sd(N_interactions_per_obs_day)), by = IDsex]

ds[same_sex == FALSE, .(mean_interactions_per_obs_day = mean(N_interactions_per_obs_day), 
                        sd_interactions_per_obs_day = sd(N_interactions_per_obs_day)), by = anyEPY]

ds[same_sex == FALSE, .(mean_interactions_per_obs_day = mean(N_interactions_per_obs_day), 
                        sd_interactions_per_obs_day = sd(N_interactions_per_obs_day)), by = siredEPY]

dx = ds[same_sex == FALSE, .N, by = .(N_interactions, IDsex)]
setorder(dx, N_interactions) 

p = 
ggplot(data = dx) +
  geom_bar(aes(x = N_interactions, y = N, group = IDsex, fill = IDsex), alpha = 0.6, size = 1.3, stat = "identity", position = 'dodge') +
  scale_x_discrete(limits = c(1:10)) +
  theme_classic(base_size = 24)
p

# png(paste0('./REPORTS/INTERACTIONS/N_interactionsn.png'), width = 900, height = 600)
# p
# dev.off()


setorder(d, first_obs)
lev = c(as.character(unique(d$ID)))
d[, ID_ := factor(ID, levels = lev)]

ggplot(data = d[N_obs > 30]) +
  geom_point(aes(x = datetime_y, y = ID_))


ggplot(data = d) +
  geom_point(aes(x = N_obs_days, y = N_obs, color = sex))

ggplot(data = d) +
  geom_point(aes(x = N_obs_days, y = tenure, color = sex))







ggplot(data = d) +
  geom_point(aes(x = N_obs_days, y = N_obs, color = as.character(year_)))

ggplot(data = ds) +
  geom_point(aes(first_obs_y, N_interactions))



ggplot(data = d) +
  geom_density(aes(x = first_obs_y, color = year_))

ggplot(data = ds) +
  geom_density(aes(x = first_obs_y, color = tenure))

ds[, long_tenure := tenure > median(tenure, na.rm = TRUE)]

ggplot(data = ds) +
  geom_density(aes(x = first_obs_y, color = long_tenure))


dw[siredEPY == TRUE]

dp[IDfather == 270170235 & EPY == 1]

dw[ID == '273145109_19']


ds = unique(dw, by = 'ID')

ggplot(data = ds) +
  geom_histogram(aes(N_interactions, fill = anyEPY))

ggplot(data = ds) +
  geom_histogram(aes(N_interactions, fill = siredEPY))

ggplot(data = ds) +
  geom_boxplot(aes(x = anyEPY, y = N_interactions))

# plot association index pair vs. unknown
ggplot() +
  geom_violin(data = dw, aes(pair, association)) +
  theme_classic(base_size = 24)

ggplot() +
  geom_violin(data = dw, aes(partner, association)) +
  theme_classic(base_size = 24)

#------------------------------------------------------------------------------------------------------------------------
# 2. Interactions statistic
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
                     datetime_y, date_, seen_in_study_site, N, N_obs, N_obs_days, N_cop)], 
           by.x = c('obs_id', 'ID1'), by.y = c('obs_id', 'ID_year'), all.x = TRUE)

# any nest? any EPY?
di = merge(di, dnID[, .(ID_year, any_nest, anyEPY)], by.x = 'ID1', by.y = 'ID_year', all.x = TRUE)

# any nest together?
di = merge(di, dnp, by = c('ID1', 'ID2'), all.x = TRUE)
di[is.na(nest_together), nest_together := 0]

# EPY together?
di = merge(di, dpEPY, by = c('ID1', 'ID2'), all.x = TRUE)
di[is.na(EPY_together), EPY_together := 0]

# type of interactions
di[, interaction_ := ifelse(!is.na(ID2), 1, 0), by = 1:nrow(di)]
di[, same_sex := ifelse(ID1sex == ID2sex, 1, 0), by = 1:nrow(di)]
di[, copAS := ifelse(ID1copAS == 1 & ID2copAS == 1, 1, 0), by = 1:nrow(di)]
di[, N_cop_ID := sum(copAS, na.rm = TRUE), by = ID1]

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
  geom_density(aes(datetime_y, color = as.character(anyEPY))) +
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
ds[, .N, anyEPY]

fm = glmmTMB(interaction_ ~ anyEPY + (1 | year_) +  (1 | ID1) + (1 | nest_together), data = di, family = binomial)

summary(fm)
plot(allEffects(fm))




#------------------------------------------------------------------------------------------------------------------------
# 2. Copulations
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

ID_pn = merge(ID_pn, dnID[, .(ID_year, any_nest, N_clutch, anyEPY)], by = 'ID_year', all.x = TRUE)
ID_pn[is.na(any_nest), any_nest := FALSE]
ID_pn[is.na(N_clutch), N_clutch := 1]
ID_pn[is.na(anyEPY), anyEPY := NA]
setorder(ID_pn, order)

V(pn)$any_nest = ID_pn$any_nest
V(pn)$N_clutch = ID_pn$N_clutch
V(pn)$anyEPY = ID_pn$anyEPY

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = V(pn)$size+2, edge.color = 'black',
     vertex.color = c('red', 'green')[1+(V(pn)$any_nest == TRUE)])


plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = V(pn)$size+2, edge.color = 'black',
     vertex.color = c('red', 'green')[1+(V(pn)$anyEPY == TRUE)], vertex.shape = c('circle', 'square')[1+(V(pn)$sex == 'F')])






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






