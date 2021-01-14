#========================================================================================================================
# Extract all for the parentage study relevant data & prepare them for analysis
#========================================================================================================================

# Summary
# 0. Prepare data for analysis

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'sf', 'auksRuak'),
        function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE)))

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

#------------------------------------------------------------------------------------------------------------------------
# METHODS
#------------------------------------------------------------------------------------------------------------------------

# Load data
d = read.table('./DATA/NESTS.txt', sep = '\t',header = TRUE) %>% data.table
dc = read.table('./DATA/CAPTURES.txt', sep = '\t',header = TRUE) %>% data.table

# Intensive study site
st_area(study_site) %>% as.numeric/ 1000000 # in km²

# N nests with parentage
ds = d[!is.na(data_type), .(N_nests = .N), by =  data_type]
ds2 = d[parentage == TRUE, .(N_parentage = .N), by =  data_type]
ds = merge(ds, ds2, by = c('data_type'), all.x = TRUE)
ds[, N_parentage := paste0(round(N_parentage / N_nests * 100, 0), '% (', N_parentage, '/', N_nests, ')')]
ds

#------------------------------------------------------------------------------------------------------------------------
# Table 1 - Samples available 
#------------------------------------------------------------------------------------------------------------------------

# captures
# assign first capture
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
            `N nests with EPY` = EPY_nests_N, `N adult males genotyped` = N_males, `N adult females genotyped` = N_females)]
setorder(ds,  -Year, `Data type`)
ds


# openxlsx::write.xlsx(ds, './REPORTS/EPY_frequency/TableS1.xlsx')


# Number of captures 

# load data
d = read.table('./DATA/CAPTURES.txt', sep = '\t',header = TRUE) %>% data.table

# assign first capture
dc[, caught_time := as.POSIXct(caught_time)]
setorder(dc, year_, caught_time)
dc[, capture_id := seq_len(.N), by = ID]
dc[, .N, capture_id]

# banded each year on and off plot by us
ds = dc[external == 0 & capture_id == 1 & year_ > 2016]

ds[, .N, .(year_)]


bm = create_bm(d, buffer = 500)
bm +
  geom_sf(data = study_site, color = 'red') +
  coord_sf(expand = FALSE) +
  geom_point(data = d, aes(lon, lat, color = data_type))


# Initiation date method


ds = d[data_type == 'intensive_study' & parentage == TRUE, .N, initiation_method]
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






### Parentage analysis 

# mean clutch size

# N offspring sampled

# undeveloped eggs





#------------------------------------------------------------------------------------------------------------------------
# RESULTS
#------------------------------------------------------------------------------------------------------------------------

# load data
d = read.table('./DATA/NESTS.txt', sep = '\t',header = TRUE) %>% data.table
dp = read.table('./DATA/PATERNITY.txt', sep = '\t',header = TRUE) %>% data.table

### Frequency of extra-pair paternity, social polyandry and renesting -----

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

# Any nests with multiple EPP sires
dp[, N_EPY := sum(EPY, na.rm = TRUE), by = nestID]
dp[N_EPY > 1] # nestID == R409_19 had one identified and one unknown EPY sire 




#------------------------------------------------------------------------------------------------------------------------
# Figure 2a
#------------------------------------------------------------------------------------------------------------------------

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


ggplot(data = ds[study_site == FALSE], aes(type, x, fill = year_, label = sample_size)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.9) +
  geom_text(position = position_dodge(width = 0.9), size = ls, vjust = -0.5, hjust = 0.5) +
  scale_fill_grey(start = 0.8, end = 0.3, labels = c('2017', '2018', '2019')) +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0, 16), expand = c(0, 0)) +
  geom_text(aes(Inf, Inf, label = 'b'), vjust = vjust_label, hjust = hjust_,  size = lsa) +
  xlab('Type') + ylab('Percentage') + 
  theme_classic(base_size = bs) +
  theme(legend.position = c(0.285, 0.82), legend.title = element_blank(), legend.background = element_rect(fill = alpha('white', 0)))




# Figure 2b

# rates pf renesting

# correlation EPY ~ season lenght or polyandry


### Extra-pair paternity and clutch order -----



