#========================================================================================================================
# R script of data analysis & figures for
# Extra-pair paternity in a sequentially polyandrous shorebird: 
# limited evidence for the sperm-storage hypothesis
#========================================================================================================================

### Description
# This script contains all steps to get from the data to the presented results 
# and figures presented in this study. 
# The order follows the appearance in the manuscript. 
# Data were extracted from our database (see script) and are in the DATA folder
# Each section in the summary below can be run independently. 

### Summary
# METHODS 
# Study species, study site and general procedures - Table 1
# Field procedures
# Parentage analysis
#
# RESULTS
# Frequency of extra-pair paternity, social polyandry and renesting - Figure 2a
# Extra-pair paternity and clutch order
# Extra-pair paternity and breeding phenology
# Characteristics of the extra-pair sires
# Frequency and timing of copulations and other male-female interactions

# Packages
sapply(c('data.table', 'magrittr', 'sf', 'auksRuak', 'ggplot2', 'ggnewscale', 'car', 'emmeans'),
       function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE)))

# auksRuak can be found at https://github.com/krietsch/auksRuak 
# (includes study site polygon and functions to create maps)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Figure 2 settings
bs = 11 # basesize
ps = 1 # point size
ls = 3 # label size
lsa = 5 # label size annotation
vjust_ = 1.5 # vjust of text
vjust_label = 1
hjust_ = 1.5 # hjust of text

#========================================================================================================================
# METHODS
#========================================================================================================================
#------------------------------------------------------------------------------------------------------------------------
# Study species, study site and general procedures
#------------------------------------------------------------------------------------------------------------------------

# Load data
d = read.table('./DATA/NESTS.txt', sep = '\t', header = TRUE) %>% data.table
dc = read.table('./DATA/CAPTURES.txt', sep = '\t', header = TRUE) %>% data.table

# Intensive study site size
st_area(study_site) %>% as.numeric/ 1000000 # in km²

# N nests with parentage
ds = d[!is.na(data_type), .(N_nests = .N), by =  data_type]
ds2 = d[parentage == TRUE, .(N_parentage = .N), by =  data_type]
ds = merge(ds, ds2, by = c('data_type'), all.x = TRUE)
ds[, N_parentage := paste0(round(N_parentage / N_nests * 100, 0), '% (', N_parentage, '/', N_nests, ')')]
ds

### Frequency of extra-pair paternity for each year and data source - Table 1

# Assign first capture
dc[, caught_time := as.POSIXct(caught_time)]
dc[, first_cap := caught_time == min(caught_time), by = ID]
dc = dc[first_cap == TRUE]

# N by category
dcs = dc[, .N, by = .(year_, sex, data_type)]
dcs_m = dcs[sex == 'M']
dcs_f = dcs[sex == 'F']
dcs = merge(dcs_m[, .(year_, data_type, N_males = N)], dcs_f[, .(year_, data_type, N_females = N)], by = c('year_', 'data_type'), all.x = TRUE)
dcs[is.na(N_females), N_females := 0]
setorder(dcs, -year_, data_type)
dcs

# Data overview
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

# Merge with captures
ds = merge(ds, dcs, by = c('year_', 'data_type'), all.x = TRUE)

# subset and rename table
ds[data_type == 'study_site', data_type := 'intensive study']
ds[data_type == 'own_off_site', data_type := 'outside plot']
ds[data_type == 'survey_plot', data_type := 'long-term monitoring']
ds[data_type == 'clutch_removal_exp', data_type := 'renesting experiment']

ds = ds[, .(Year = year_, `Data type` = data_type, `% EPY` = EPY_eggs_per, `N EPY` = EPY_eggs_N, `% nests with EPY` = EPY_nests_per, 
            `N nests with EPY` = EPY_nests_N, `N adult males genotyped` = N_males, `N adult females genotyped` = N_females)]
setorder(ds,  -Year, `Data type`)
ds

# save table
# openxlsx::write.xlsx(ds, './REPORTS/EPY_frequency/TableS1.xlsx')

#------------------------------------------------------------------------------------------------------------------------
# Field procedures
#------------------------------------------------------------------------------------------------------------------------

# Load data
d = read.table('./DATA/NESTS.txt', sep = '\t', header = TRUE) %>% data.table
dc = read.table('./DATA/CAPTURES.txt', sep = '\t', header = TRUE) %>% data.table

# Assign first capture
dc[, caught_time := as.POSIXct(caught_time)]
setorder(dc, year_, caught_time)
dc[, capture_id := seq_len(.N), by = ID]
dc[, .N, capture_id]

# Banded each year in intensive study site
ds = dc[data_type == 'study_site' & capture_id == 1]
ds[, .N, .(year_)]

# Plot capture locations
st_transform_DT(ds)
bm = create_bm(ds, buffer = 500)
bm +
  geom_sf(data = study_site, color = 'red') +
  coord_sf(expand = FALSE) +
  geom_point(data = ds, aes(lon, lat, color = as.character(year_)), alpha = 0.3)

# Nest age when found
d[, initiation := as.POSIXct(initiation)]
d[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
ds = d[data_type == 'study_site' ]
ds[, complete := initiation + clutch_size*86400]

ds[, age_found := difftime(found_datetime, initiation, units = 'days') %>% as.numeric]
ds[, age_found_complete := difftime(found_datetime, complete, units = 'days') %>% as.numeric]
ds[age_found_complete < 1] %>%  nrow/ ds %>% nrow
ds[age_found_complete < 5] %>%  nrow/ ds %>% nrow

# Number of males and females unbanded when clutch found
d = merge(d, dc[capture_id == 1, .(ID, caught_time_male = caught_time)], by.x = 'male_id', by.y = 'ID', all.x = TRUE)
d[, diff_found_caugth_male := difftime(found_datetime, caught_time_male, units = 'days') %>% as.numeric]
d = merge(d, dc[capture_id == 1, .(ID, caught_time_female = caught_time)], by.x = 'female_id', by.y = 'ID', all.x = TRUE)
d[, diff_found_caugth_female := difftime(found_datetime, caught_time_female, units = 'days') %>% as.numeric]

ds = d[data_type == 'study_site']
ds[diff_found_caugth_male < 0 | is.na(diff_found_caugth_male)] %>% nrow /ds %>% nrow * 100 # males
ds[diff_found_caugth_female < 0 | is.na(diff_found_caugth_female)] %>% nrow /ds %>% nrow * 100 # females

# Mean incubation period of collected clutches with known initiation date
ds = d[!is.na(collected_datetime) & initiation_method == 'found_incomplete' & !is.na(hatching_datetime)]
ds[, initiation := as.POSIXct(initiation)]
ds[, start_incubation := initiation + clutch_size*3600*24 - 3600*24]
ds[is.na(hatching_datetime), based_on_est_hdt := TRUE]
ds[, incubation_period := difftime(hatching_datetime, start_incubation, units = 'days') %>% as.numeric]

ds$incubation_period %>% length
ds$incubation_period %>% mean
ds$incubation_period %>% sd
ds$incubation_period %>% min
ds$incubation_period %>% max

# Initiation date method
ds = d[data_type == 'study_site' & parentage == TRUE, .N, initiation_method]
ds[, N_nests := nrow(d[data_type == 'study_site' & parentage == TRUE])]
ds[, initiation_method_percent := N / N_nests * 100]
ds

#------------------------------------------------------------------------------------------------------------------------
# Parentage analysis
#------------------------------------------------------------------------------------------------------------------------

# Load data
d = read.table('./DATA/NESTS.txt', sep = '\t', header = TRUE) %>% data.table
dc = read.table('./DATA/CAPTURES.txt', sep = '\t', header = TRUE) %>% data.table
dp = read.table('./DATA/PATERNITY.txt', sep = '\t', header = TRUE) %>% data.table

# Clutch size of nests with genotype
ds = d[parentage == TRUE]
ds[, mean(clutch_size, na.rm = TRUE)]
ds[, sd(clutch_size, na.rm = TRUE)]
ds[, min(clutch_size, na.rm = TRUE)]
ds[, max(clutch_size, na.rm = TRUE)]

# N offspring sampled overall
ds[clutch_size == N_parentage] %>% nrow
ds[clutch_size == N_parentage] %>% nrow / ds %>% nrow *100

# N offspring sampled intensive study site
ds = d[parentage == TRUE & data_type == 'study_site']
ds[clutch_size == N_parentage] %>% nrow
ds[clutch_size == N_parentage] %>% nrow / ds %>% nrow *100

# Undeveloped eggs
dp = merge(dp, d[, .(nestID, data_type)], by = 'nestID', all.x = TRUE)

# collected clutches within study site
dp[data_type == 'study_site']$nestID %>% unique %>% length 

# undeveloped eggs in study site
ds = dp[data_type == 'study_site']

# nests
ds$nestID %>% unique %>% length # all
ds[undeveloped == 1]$nestID %>% unique %>% length # nests with undeveloped eggs
ds[undeveloped == 1]$nestID %>% unique %>% length / ds$nestID %>% unique %>% length * 100

# eggs
ds %>% nrow # all
ds[undeveloped == 1] %>% nrow # undeveloped eggs
ds[undeveloped == 1] %>% nrow  / ds %>% nrow  * 100

# undeveloped and DNA extracted
ds[undeveloped == 1] %>% nrow # all
ds[undeveloped == 1 & !is.na(EPY)] %>% nrow # undeveloped eggs
ds[undeveloped == 1 & !is.na(EPY)] %>% nrow / ds[undeveloped == 1] %>% nrow  * 100

# total potential unfertile eggs
ds[undeveloped == 1 & is.na(EPY)] %>% nrow 
ds[undeveloped == 1 & is.na(EPY)] %>% nrow / ds %>% nrow * 100

# Number of mothers and fathers assigned ################################################################################ TODO
dp[!is.na(EPY) & !is.na(IDmother)]

#========================================================================================================================
# RESULTS
#========================================================================================================================
#------------------------------------------------------------------------------------------------------------------------
# Frequency of extra-pair paternity, social polyandry and renesting - Figure 2a
#------------------------------------------------------------------------------------------------------------------------

# load data
d = read.table('./DATA/NESTS.txt', sep = '\t',header = TRUE) %>% data.table
dp = read.table('./DATA/PATERNITY.txt', sep = '\t',header = TRUE) %>% data.table

# EPY in nests
d[parentage == TRUE & anyEPY == 1, .N]
d[parentage == TRUE, .N]
d[parentage == TRUE & anyEPY == 1, .N] / d[parentage == TRUE, .N] * 100 

# EPY in eggs
d[parentage == TRUE, sum(N_EPY)]
d[parentage == TRUE, sum(N_parentage)]
d[parentage == TRUE, sum(N_EPY)] /d[parentage == TRUE, sum(N_parentage)] * 100

# EPY difference between years?
ds = d[parentage == TRUE & data_type == 'study_site']
ds[, YEAR_ := as.character(year_)]

fm = glm(anyEPY ~ YEAR_, data = ds, family = binomial)
summary(fm)
Anova(fm)

# Post-hoc test
marginal = emmeans(fm, ~ YEAR_)
pairs(marginal)

# EPY difference between study sites?
ds = d[parentage == TRUE]
ds[, YEAR_ := as.character(year_)]
ds[, data_type2 := ifelse(data_type == 'study_site', 'study_site', 'other_sources')]

fm = glm(anyEPY ~ data_type2, data = ds, family = binomial)
summary(fm)
Anova(fm)

# Number of EPY in each clutch
d[anyEPY == 1] %>% nrow
d[anyEPY == 1, .N, N_EPY]
d[anyEPY == 1, .N, clutch_size]

# Any nests with multiple EPP sires
dp[, N_EPY := sum(EPY, na.rm = TRUE), by = nestID]
ds = unique(dp, by = c('nestID', 'IDfather'))
ds[, N_fathers := .N, by = nestID]
ds[, .N, by = N_fathers] # nestID == R409_19 had one identified and one unknown EPY sire 

### Rates of polyandry and renesting - Figure 2a -----

# unique ID's by year
d[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]
d[, male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]

### unique ID's
d[is.na(male_id), male_id_year := NA]
d[, male_id_year_NA := male_id_year]
d[is.na(male_id), male_id_year_NA := paste0(seq_len(.N), '_', substr(year_, 3,4)), by = male_id_year_NA]

d[is.na(female_id), female_id_year := NA]
d[, female_id_year_NA := female_id_year]
d[is.na(female_id), female_id_year_NA := paste0(seq_len(.N), '_', substr(year_, 3,4)), by = female_id_year_NA]

# subset study site
ds = d[external == 0 & data_type == 'study_site']
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
dsmr_NARL = dsm[renesting_study_site == TRUE, .(renesting_study_site = .N), by = year_]

# females
dsf = unique(ds, by = 'female_id_year_NA')
dsf[, .N, by = female_assigned]
dssf = dsf[, .(unique_females = .N), by = year_]

# polyandrous females
dsfp = dsf[polyandrous == TRUE, .(polyandrous_females = .N), by = year_]
dsfp_NARL = dsf[polyandry_study_site == TRUE, .(polyandrous_study_site = .N), by = year_]

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
dss[, renesting_males_NARL_per     := round(renesting_study_site / unique_males * 100, 1)]
dss[, polyandrous_females_per      := round(polyandrous_females / unique_females * 100, 1)]
dss[, polyandrous_females_NARL_per := round(polyandrous_study_site / unique_females * 100, 1)]

# add EPY on plot
dspe = ds[!is.na(N_parentage), .(N_parentage_sum = sum(N_parentage), N_EPY = sum(N_EPY)), by = year_]
dspe[, EPY_per := round(N_EPY / N_parentage_sum * 100, 1)]
dss = merge(dss, dspe, by = 'year_')

dsp = ds[!is.na(N_parentage), .(N_parentage = .N), by = year_]
dspn = ds[!is.na(N_parentage) & anyEPY == TRUE, .(N_anyEPY = .N), by = year_]
dsp = merge(dsp, dspn, by = 'year_')
dsp[, EPY_nests_per := round(N_anyEPY / N_parentage * 100, 1)]
dsp[, EPY := paste0(EPY_nests_per, '% (', N_anyEPY, '/', N_parentage, ')')]
dss = merge(dss, dsp, by = 'year_')

# plot EPP, polyandry and renesting
ds = rbindlist(list(dss[, .(year_, x = EPY_per, n = N_EPY, N = N_parentage_sum, type = 'EPY', study_site = TRUE)],
                    dss[, .(year_, x = EPY_nests_per, n = N_anyEPY, N = N_parentage, type = 'EPP', study_site = TRUE)],
                    dss[, .(year_, x = polyandrous_females_NARL_per, n = polyandrous_study_site, N = unique_females, type = 'polyandry', study_site = TRUE)],
                    dss[, .(year_, x = renesting_males_NARL_per, n = renesting_study_site, N = unique_males,  type = 'renesting', study_site = TRUE)],
                    dss[, .(year_, x = EPY_per, n = N_EPY, N = N_parentage_sum, type = 'EPY', study_site = FALSE)],
                    dss[, .(year_, x = EPY_nests_per, n = N_anyEPY, N = N_parentage, type = 'EPP', study_site = FALSE)],
                    dss[, .(year_, x = polyandrous_females_per, n = polyandrous_females, N = unique_females,  type = 'polyandry', study_site = FALSE)],
                    dss[, .(year_, x = renesting_males_per, n = renesting_males, N = unique_males,  type = 'renesting', study_site = FALSE)]))

ds[, year_ := as.factor(year_)]
ds[, sample_size := as.character(N)]
ds[year_ == '2019' & type %in% c('polyandry', 'renesting'), sample_size := paste0(sample_size, '*')]

ds[, type := factor(type, levels = c('EPY', 'EPP', 'polyandry', 'renesting'))]

p1 = 
  ggplot(data = ds[study_site == FALSE], aes(year_, x, fill = type, label = sample_size)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.9) +
  geom_text(position = position_dodge(width = 0.9), size = ls, vjust = -0.5, hjust = 0.5) + # text horizontal
  # geom_text(position = position_dodge(width = 0.9), size = ls, hjust = -0.1, angle = 90) +
  # scale_fill_manual(values = c('grey85', 'grey50','firebrick4', '#33638DFF'), 
  #                   labels = c('EPY', 'nests with EPY', 'polyandrous females', 'renesting males')) +
  scale_fill_grey(labels = c('EPY', 'nests with EPY', 'polyandrous females', 'renesting males')) +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0, 16), expand = c(0, 0)) +
  geom_text(aes(Inf, Inf, label = 'b'), vjust = vjust_label, hjust = hjust_,  size = lsa) +
  xlab('Year') + ylab('Percentage') + 
  theme_classic(base_size = bs) +
  theme(legend.position = c(0.285, 0.82), legend.title = element_blank(), legend.background = element_rect(fill = alpha('white', 0)))
p1


ds[, sample_size_N := paste0(n, '/', N)]
ds[year_ == '2019' & type %in% c('polyandry', 'renesting'), sample_size_N := paste0(sample_size_N, '*')]

#  Figure 2a
p1 = 
  ggplot(data = ds[study_site == FALSE], aes(year_, x, fill = type, label = sample_size_N)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.9) +
  geom_text(position = position_dodge(width = 0.9), size = lsa, hjust = -0.1, angle = 90) +
  # scale_fill_manual(values = c('grey85', 'grey50','firebrick4', '#33638DFF'), 
  #                   labels = c('EPY', 'nests with EPY', 'polyandrous females', 'renesting males')) +
  scale_fill_grey(labels = c('EPY', 'nests with EPY', 'polyandrous females', 'renesting males')) +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0, 16), expand = c(0, 0)) +
  geom_text(aes(Inf, Inf, label = 'a'), vjust = vjust_label, hjust = hjust_,  size = lsa) +
  xlab('Year') + ylab('Percentage') + 
  theme_classic(base_size = bs) +
  theme(legend.position = c(0.285, 0.82), legend.title = element_blank(), legend.background = element_rect(fill = alpha('white', 0)))
p1



### Clutch initiation between years - Figure 2b -----
d[, initiation := as.POSIXct(initiation)]
d[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]

# Subset data from intensive study site with initiation date
ds = d[data_type == 'study_site' & !is.na(initiation_y)]
ds[, initiation_date := as.Date(initiation)]

# Nests at peak
y = ds[, .N, by = .(year_, initiation_date)]
y[, max(N), by = year_]

# Sample size
dss = ds[, .(median = median(initiation_y), q25 = quantile(initiation_y, probs = c(0.25)), 
             q75 = quantile(initiation_y, probs = c(0.75)), .N, max = max(initiation_y)), by = year_]

#  Figure 2b
p2 = 
  ggplot(data = ds) +
  geom_violin(aes(as.character(year_), initiation_y), show.legend = FALSE, fill = 'grey85') +
  geom_point(data = dss, aes(as.character(year_), median), size = 2) +
  geom_linerange(data = dss, aes(x = as.character(year_), ymin = q75, ymax = q25), size = 0.5) +
  geom_point(data = ds[year_ == 2017 & initiation_y < as.POSIXct('2020-06-16')], aes(as.character(year_), initiation_y), 
             shape = 4, size = 1, stroke = 1.3) +
  geom_text(data = dss, aes(as.character(year_), as.POSIXct('2021-07-02 11:03:00'), label = N), vjust = 0, size = ls) +
  scale_y_datetime(breaks = c(as.POSIXct(c('2021-06-07', '2021-06-14', '2021-06-21', '2021-06-28'))), 
                   labels = c('7', '14', '21', '28')) +
  geom_text(aes(Inf, as.POSIXct(c('2021-07-02')), label = 'b'), vjust = vjust_label - 1.3, hjust = hjust_,  size = lsa) +
  xlab('Year') + ylab('Clutch initiation date (June)') + 
  theme_classic(base_size = bs)
p2

# Length of the clutch initiation period 
ds = d[data_type == 'study_site' & !is.na(initiation_y)]
ds = ds[!(year_ == 2017 & initiation_y < as.POSIXct('2021-06-16'))] # exclude to outliers in 2017
dss = ds[, .(min = min(initiation_y), max = max(initiation_y)), by = year_]
dss[, season_length := difftime(max, min, units = 'days') %>% as.numeric]
dss

dss[, polyandry := c(2.9, 5.4, 8.8)]
dss[, N_nests := c(35, 39, 100)]
dss[, N_birds := c(138, 203, 319)]

# EPY difference between years due to season
ds = d[parentage == TRUE & data_type == 'study_site']
ds[, YEAR_ := as.character(year_)]

ds = merge(ds, dss[, .(year_, season_length, polyandry, N_nests, N_birds)], by = 'year_', all.x = TRUE)

fm = glm(anyEPY ~ season_length, data = ds, family = binomial)
summary(fm)
Anova(fm)

fm = glm(anyEPY ~ polyandry, data = ds, family = binomial)
summary(fm)
Anova(fm)

fm = glm(anyEPY ~ N_nests, data = ds, family = binomial)
summary(fm)
Anova(fm)

fm = glm(anyEPY ~ N_birds, data = ds, family = binomial)
summary(fm)
Anova(fm)

#------------------------------------------------------------------------------------------------------------------------
# Extra-pair paternity and clutch order
#------------------------------------------------------------------------------------------------------------------------

# load data
d = read.table('./DATA/NESTS.txt', sep = '\t',header = TRUE) %>% data.table
dp = read.table('./DATA/PATERNITY.txt', sep = '\t',header = TRUE) %>% data.table

# format
d[, initiation := as.POSIXct(initiation)]
d[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]

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

# subset all multiple clutches or in study site
ds = d[!is.na(initiation)]
ds = ds[data_type == 'study_site' & !(clutch_identity %in% c('first', 'second', 'third')) | clutch_identity %in% c('first', 'second', 'third')]
ds[, anyEPY := as.character(anyEPY)]

ds[, mean_initiation := mean(initiation, na.rm = TRUE), by = year_]
ds[, initiation_st := difftime(initiation, mean_initiation, units = 'days') %>% as.numeric]

ds = ds[!is.na(anyEPY)]

ds[is.na(renesting_male), renesting_male := FALSE]
ds[, any_renesting := any(renesting_male == TRUE), by = female_id_year]
ds[any_renesting == FALSE, next_clutch := 'polyandrous']
ds[any_renesting == TRUE, next_clutch := 'renesting']

# add first and second clutch of females with three clutches again (otherwise linetype does not work)
ds[clutch_identity == 'third']
dss = ds[female_id %in% c(270170935, 19222)]
dss[, female_id_year_NA := paste0(female_id_year, '2')]

ds = rbind(ds, dss)
ds[!(clutch_identity %in% c('first', 'second', 'third')), clutch_identity := 'single']

# correct females with three clutches
ds = ds[!(female_id_year_NA %in% c('270170935_19', '19222_19') & clutch_identity == 'third')]
ds[female_id_year_NA %in% c('270170935_19', '19222_19'), next_clutch := 'polyandrous']

# factor order
ds[, clutch_identity := factor(clutch_identity, levels = c('single', 'first', 'second', 'third'))]
ds[, anyEPY := factor(anyEPY, levels = c('0', '1'))]


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

ds[anyEPY == '1', .N, clutch_identity]
ds[, .N, clutch_identity]

ds[clutch_identity == 'first' & next_clutch == 'polyandrous']
ds[clutch_identity == 'first' & next_clutch == 'renesting']

dss = data.table(clutch_identity = c('single', 'first', 'second', 'third'),
                 sample_size = c('138', '15', '15', '2'))

p3 = 
  ggplot() +
  geom_hline(yintercept = 0, color = 'grey70') +
  geom_boxplot(data = ds, aes(clutch_identity, initiation_st), fill = 'grey85', outlier.alpha = 0) +
  geom_line(data = ds, aes(clutch_identity, initiation_st, group = female_id_year_NA, linetype = next_clutch), size = 0.3) +
  geom_point(data = ds[clutch_identity != 'single' & anyEPY == '0'], aes(clutch_identity, initiation_st, fill = anyEPY),
             shape = 21, size = 1) +
  geom_point(data = ds[clutch_identity != 'single' & anyEPY == '1'], aes(clutch_identity, initiation_st, fill = anyEPY),
             shape = 21, size = 1) +
  geom_jitter(data = ds[clutch_identity == 'single'], aes(clutch_identity, initiation_st, fill = anyEPY), 
              shape = 21, size = ps, height = 0, width = 0.3) +
  scale_fill_manual(values = c('white', 'black'), name = NULL, labels = c('no EPY', 'EPY'), guide = FALSE) +
  scale_linetype_manual(values = c('solid', 'dotted'), name = NULL) +
  scale_x_discrete(labels = c('Single', 'First', 'Second', 'Third')) +
  scale_y_continuous(breaks = c(-14, -7, 0, 7, 14), limits = c(-18, 18), expand = c(0, 0)) +
  geom_text(aes(Inf, Inf, label = 'c'), vjust = vjust_label, hjust = hjust_,  size = lsa) +
  xlab('Clutch type') + ylab('Clutch initiation date (standardized)') +
  geom_text(data = dss, aes(clutch_identity, Inf, label = sample_size), vjust = vjust_, size = ls) +
  theme_classic_edit(base_size = bs, lp = c(0.85, 0.08)) +
  theme(legend.background = element_rect(fill = alpha('white', 0)),
        legend.key.size = unit(0.5, 'lines'))
p3


# some descriptive statisic about the timing

# exclude double again
ds = ds[!(female_id_year_NA %in% c('270170935_19', '19222_19'))]
ds[, .N, clutch_identity]

# initiation first clutch compared to single clutch
ds[clutch_identity == 'single', mean(initiation_st)] - ds[clutch_identity == 'first', mean(initiation_st)] 

# second clutches compared to single
ds[clutch_identity == 'single', mean(initiation_st)] - ds[clutch_identity == 'second', mean(initiation_st)] 
ds[clutch_identity == 'single', mean(initiation_st)] - ds[clutch_identity == 'second' & polyandrous == TRUE, mean(initiation_st)] 
ds[clutch_identity == 'single', mean(initiation_st)] - ds[clutch_identity == 'second' & polyandrous != TRUE, mean(initiation_st)] 

ds[clutch_identity == 'third', .(initiation_st)] 


#------------------------------------------------------------------------------------------------------------------------
# Extra-pair paternity and breeding phenology
#------------------------------------------------------------------------------------------------------------------------

# load data
d = read.table('./DATA/NESTS.txt', sep = '\t',header = TRUE) %>% data.table
dp = read.table('./DATA/PATERNITY.txt', sep = '\t',header = TRUE) %>% data.table

# subset all multiple clutches or in study site
ds = d[!is.na(initiation) & !is.na(anyEPY)]
ds = ds[, .(year_, nestID, initiation_y, anyEPY, data_type)]
ds[data_type == 'study_site', data_type := 'Intensive study']
ds[data_type != 'Intensive study', data_type := 'Other data']

# load Dale et al. data
dale = read.csv2('./DATA/Dale_EPP.csv') %>% data.table
dale[, initiation_y := as.POSIXct(as.Date(initiation_doy, origin = '1993-01-01'))]

ds = rbind(ds, dale[, .(year_ = YEAR_, nestID, initiation_y, anyEPY, data_type = 'Dale et al.')])


ds[, mean_initiation := mean(initiation_y, na.rm = TRUE), by = year_]
ds[, initiation_st := difftime(initiation_y, mean_initiation, units = 'days') %>% as.numeric]
ds[, anyEPY := as.character(anyEPY)]
ds[, study_site := as.character(study_site)]

dss = ds[, .(median = median(initiation_st), q25 = quantile(initiation_st, probs = c(0.25)), 
             q75 = quantile(initiation_st, probs = c(0.75)), .N), by = .(data_type, anyEPY)]

dss2 = data.table(data_type = c(rep('Intensive study', 2), rep('Other data', 2), rep('Dale et al.', 2)),
                  sample_size = c('16', '149', '21', '146', '6', '12'),
                  anyEPY = c('1', '0', '1', '0', '1', '0'))

# factor order
ds[, data_type := factor(data_type, levels = c('Intensive study', 'Other data', 'Dale et al.'))]
ds[, anyEPY := factor(anyEPY, levels = c('1', '0'))]

dss2[, anyEPY := factor(anyEPY, levels = c('1', '0'))]
dss2[, data_type := factor(data_type, levels = c('Intensive study', 'Other data', 'Dale et al.'))]


p4 = 
  ggplot(data = ds, aes(data_type, initiation_st, fill = anyEPY)) +
  geom_hline(yintercept = 0, color = 'grey70') +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.alpha = 0, show.legend = FALSE) +
  scale_fill_manual(values = c('#818181', 'white'), labels = c('0', '1')) +
  new_scale_fill() +
  geom_point(data = ds, aes(data_type, initiation_st, fill = anyEPY), shape = 21, size = ps,
             position = position_jitterdodge(jitter.width = 0.6, jitter.height = 0, dodge.width = 0.9)) +
  scale_fill_manual(values = c('black', 'white'), name = NULL, labels = c('EPY', 'no EPY')) +
  scale_y_continuous(breaks = c(-14, -7, 0, 7, 14), limits = c(-18, 18), expand = c(0, 0)) +
  geom_text(data = dss2, aes(data_type, Inf, group = anyEPY, label = sample_size), position = position_dodge(width = 0.9), vjust = vjust_, size = ls) +
  geom_text(aes(Inf, Inf, label = 'd'), vjust = vjust_label, hjust = hjust_,  size = lsa) +
  xlab('Data source') + ylab('Clutch initiation date (standardized)') + 
  theme_classic_edit(base_size = bs, lp = c(0.89, 0.08)) +
  theme(legend.background = element_rect(fill = alpha('white', 0)), 
        legend.key.size = unit(0.5, 'lines'))


p4

# statistics
fm = glm(anyEPY ~ initiation_st, data = ds[data_type == 'Intensive study'], family = binomial)
summary(fm)

fm = glm(anyEPY ~ initiation_st, data = ds[data_type == 'Other data'], family = binomial)
summary(fm)


# save Figure 2
p1 + p2 + p3 + p4 + plot_layout(ncol = 2, nrow = 2) 

ggsave('./REPORTS/FIGURES/Figure2.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')


#------------------------------------------------------------------------------------------------------------------------
# Characteristics of the extra-pair sires
#------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------
# Frequency and timing of copulations and other male-female interactions
#------------------------------------------------------------------------------------------------------------------------

















