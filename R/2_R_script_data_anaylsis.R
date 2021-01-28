#' ---
#' title: R script for data analysis and figures
#' subtitle: "Extra-pair paternity in a sequentially polyandrous shorebird; limited evidence for the sperm-storage hypothesis"  
#' author: Johannes Krietsch
#' output:
#'    html_document:
#'      toc: true
#'      highlight: tango
#' run: 
#'     R("""
#'          library(knitr)
#'          library(rmarkdown)
#'          opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
#'      """)
#' ---


#' ## DESCRIPTION
#' This script contains all steps to get from the data to the presented results
#' and figures presented in this study.
#' The order follows the appearance in the manuscript.
#' Data were extracted from our database (see script) and are in the DATA folder.
#' Outputs are written to the FIGURES or TABLES folder.
#' Each section in the summary below can be run independently.

# Line to run to create html output
# rmarkdown::render('./R/2_R_script_data_anaylsis.R', output_dir = './REPORTS')

# Packages
sapply(c('data.table', 'magrittr', 'sf', 'auksRuak', 'ggplot2', 'ggnewscale', 'car', 'emmeans', 'knitr', 
         'patchwork', 'cowplot', 'ggpubr'),
       function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE)))

#' auksRuak can be found at [github](https://github.com/krietsch/auksRuak)
#' (includes study site polygon and functions to create maps)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Functions
source('./R/0_functions.R')

# Settings 
options(warn = -1) # Suppress warnings

# Figure adjustments
bs = 11 # base size
ps = 1 # point size
ls = 3 # label size
lsa = 5 # label size annotation
vjust_ = 1.5 # vjust of text
vjust_label = 1
hjust_ = 1.5 # hjust of text

#==============================================================================================================
#' ## METHODS
#==============================================================================================================
#--------------------------------------------------------------------------------------------------------------
#' ### Study species, study site and general procedures
#--------------------------------------------------------------------------------------------------------------

# Load data
d = read.table('./DATA/NESTS.txt', sep = '\t', header = TRUE) %>% data.table

# Intensive study site size
st_area(study_site) %>% as.numeric/ 1000000 # in km²

# N nests with parentage
ds = d[!is.na(data_type), .(N_nests = .N), by =  data_type]
ds2 = d[parentage == TRUE, .(N_parentage = .N), by =  data_type]
ds = merge(ds, ds2, by = c('data_type'), all.x = TRUE)
ds[, N_parentage := paste0(round(N_parentage / N_nests * 100, 0), '% (', N_parentage, '/', N_nests, ')')]
kable(ds)

# Map of nests with parentage
ds = d[parentage == TRUE]

# change projection
st_transform_DT(ds)

ds[, data_type := factor(data_type, levels = c('study_site', 'own_off_site', 'survey_plot', 'clutch_removal_exp'))]


bm = create_bm(ds, buffer = 6000,  squared = TRUE)
bm +
  geom_point(data = ds, aes(lon, lat, shape = data_type, fill = data_type, size = data_type)) +
  scale_shape_manual(name = '', values = c(21, 21, 24, 24), 
                     labels = c('Intensive study', 'Outside plot', 'Long-term monitoring', 'Renesting experiment')) +
  scale_size_manual(name = '', values = c(rep(0.8, 4)), 
                    labels = c('Intensive study', 'Outside plot', 'Long-term monitoring', 'Renesting experiment')) +
  scale_fill_manual(name = '', values = c('white', 'black', 'white', 'black'), 
                    labels = c('Intensive study', 'Outside plot', 'Long-term monitoring', 'Renesting experiment')) +
  theme(legend.position = c(0.8, 0.95))


# ggsave('./REPORTS/FIGURES/Map_nests_with_parentage.tiff', plot = last_plot(),  width = 85, height = 85, units = c('mm'), dpi = 'print')

#--------------------------------------------------------------------------------------------------------------
#' ### Field procedures
#--------------------------------------------------------------------------------------------------------------

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
  geom_point(data = ds, aes(lon, lat), color = 'black', alpha = 0.3)

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
d = merge(d, dc[capture_id == 1, .(ID, caught_time_male = caught_time)], by.x = 'male_id', 
          by.y = 'ID', all.x = TRUE)
d[, diff_found_caugth_male := difftime(found_datetime, caught_time_male, units = 'days') %>% as.numeric]
d = merge(d, dc[capture_id == 1, .(ID, caught_time_female = caught_time)], by.x = 'female_id', 
          by.y = 'ID', all.x = TRUE)
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
kable(ds)

#--------------------------------------------------------------------------------------------------------------
#' ### Parentage analysis and molecular sexing
#--------------------------------------------------------------------------------------------------------------

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
ds[clutch_size == N_parentage] %>% nrow / ds %>% nrow * 100

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

# total potential infertile eggs
ds[undeveloped == 1 & is.na(EPY)] %>% nrow 
ds[undeveloped == 1 & is.na(EPY)] %>% nrow / ds %>% nrow * 100

# Number of mothers assigned
ds[IDmother > 100000]$nestID %>% unique %>% length # >100000 to exclude uncaught birds
ds[IDmother > 100000]$nestID %>% unique %>% length / ds$nestID %>% unique %>% length * 100

# Number of fathers assigned
ds[IDfather > 100000] %>% nrow # >100000 to exclude uncaught birds
ds[IDfather > 100000] %>% nrow / ds[!is.na(EPY)] %>% nrow * 100

# Other sources 
ds = dp[data_type != 'study_site']

# Number of mothers assigned
ds[IDmother > 100000]$nestID %>% unique %>% length # >100000 to exclude uncaught birds
ds[IDmother > 100000]$nestID %>% unique %>% length / ds$nestID %>% unique %>% length * 100

# Number of fathers assigned
ds[IDfather > 100000] %>% nrow # >100000 to exclude uncaught birds
ds[IDfather > 100000] %>% nrow / ds[!is.na(EPY)] %>% nrow * 100

#==============================================================================================================
#' ## RESULTS
#==============================================================================================================
#--------------------------------------------------------------------------------------------------------------
#' ### Frequency of extra-pair paternity, social polyandry and renesting
#--------------------------------------------------------------------------------------------------------------

# load data
d = read.table('./DATA/NESTS.txt', sep = '\t',header = TRUE) %>% data.table
dc = read.table('./DATA/CAPTURES.txt', sep = '\t', header = TRUE) %>% data.table
dp = read.table('./DATA/PATERNITY.txt', sep = '\t',header = TRUE) %>% data.table

# EPY in nests
d[parentage == TRUE & anyEPY == 1, .N]
d[parentage == TRUE, .N]
d[parentage == TRUE & anyEPY == 1, .N] / d[parentage == TRUE, .N] * 100

# EPY in eggs
d[parentage == TRUE, sum(N_EPY)]
d[parentage == TRUE, sum(N_parentage)]
d[parentage == TRUE, sum(N_EPY)] /d[parentage == TRUE, sum(N_parentage)] * 100

#' #### Frequency of extra-pair paternity for each year and data source

# Assign first capture
dc[, caught_time := as.POSIXct(caught_time)]
dc[, first_cap := caught_time == min(caught_time), by = ID]
dc = dc[first_cap == TRUE]

# N by category
dcs = dc[, .N, by = .(year_, sex, data_type)]
dcs_m = dcs[sex == 'M']
dcs_f = dcs[sex == 'F']
dcs = merge(dcs_m[, .(year_, data_type, N_males = N)], dcs_f[, .(year_, data_type, N_females = N)], 
            by = c('year_', 'data_type'), all.x = TRUE)
dcs[is.na(N_females), N_females := 0]
setorder(dcs, -year_, data_type)

# Data overview
ds = d[, .(N_nests = .N), by = .(year_, data_type)]
ds2 = d[parentage == TRUE, .(N_parentage = .N), by = .(year_, data_type)]
ds3 = d[anyEPY == TRUE, .(N_EPY = .N), by = .(year_, data_type)]
ds4 = d[, .(N_eggs = sum(N_parentage, na.rm = TRUE), N_eggs_EPY = sum(N_EPY, na.rm = TRUE)), 
        by = .(year_, data_type)]
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

ds = ds[, .(Year = year_, `Data type` = data_type, `% EPY` = EPY_eggs_per, `N EPY` = EPY_eggs_N, 
            `% nests with EPY` = EPY_nests_per, 
            `N nests with EPY` = EPY_nests_N, `N adult males genotyped` = N_males, 
            `N adult females genotyped` = N_females)]
setorder(ds,  -Year, `Data type`)
kable(ds)

# save table
# openxlsx::write.xlsx(ds, './REPORTS/TABLES/EPY_frequency.xlsx')


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

#' #### Rates of polyandry and renesting

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
# ds[is.na(male_id)]
# ds[is.na(female_id)]

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
ds = rbindlist(list(dss[, .(year_, x = EPY_per, n = N_EPY, N = N_parentage_sum, 
                            type = 'EPY', study_site = TRUE)],
                    dss[, .(year_, x = EPY_nests_per, n = N_anyEPY, N = N_parentage, 
                            type = 'EPP', study_site = TRUE)],
                    dss[, .(year_, x = polyandrous_females_NARL_per, n = polyandrous_study_site, 
                            N = unique_females, type = 'polyandry', study_site = TRUE)],
                    dss[, .(year_, x = renesting_males_NARL_per, n = renesting_study_site, 
                            N = unique_males,  type = 'renesting', study_site = TRUE)],
                    dss[, .(year_, x = EPY_per, n = N_EPY, N = N_parentage_sum, 
                            type = 'EPY', study_site = FALSE)],
                    dss[, .(year_, x = EPY_nests_per, n = N_anyEPY, N = N_parentage, 
                            type = 'EPP', study_site = FALSE)],
                    dss[, .(year_, x = polyandrous_females_per, n = polyandrous_females,
                            N = unique_females,  type = 'polyandry', study_site = FALSE)],
                    dss[, .(year_, x = renesting_males_per, n = renesting_males, 
                            N = unique_males,  type = 'renesting', study_site = FALSE)]))

ds[, year_ := as.factor(year_)]
ds[, sample_size := as.character(N)]
ds[year_ == '2019' & type %in% c('polyandry', 'renesting'), sample_size := paste0(sample_size, '*')]

ds[, type := factor(type, levels = c('EPY', 'EPP', 'polyandry', 'renesting'))]
kable(ds)

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
  theme(legend.position = c(0.285, 0.82), legend.title = element_blank(), 
        legend.background = element_rect(fill = alpha('white', 0)))
p1


ds[, sample_size_N := paste0(n, '/', N)]
ds[year_ == '2019' & type %in% c('polyandry', 'renesting'), sample_size_N := paste0(sample_size_N, '*')]


p1 =
  ggplot(data = ds[study_site == FALSE], aes(year_, x, fill = type, label = sample_size_N)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.9) +
  geom_text(position = position_dodge(width = 0.9), size = ls, hjust = -0.1, angle = 90) +
  # scale_fill_manual(values = c('grey85', 'grey50','firebrick4', '#33638DFF'),
  #                   labels = c('EPY', 'nests with EPY', 'polyandrous females', 'renesting males')) +
  scale_fill_grey(labels = c('EPY', 'nests with EPY', 'polyandrous females', 'renesting males')) +
  scale_y_continuous(breaks = c(0, 5, 10, 15), limits = c(0, 16), expand = c(0, 0)) +
  geom_text(aes(Inf, Inf, label = 'a'), vjust = vjust_label, hjust = hjust_,  size = lsa) +
  xlab('Year') + ylab('Percentage') +
  theme_classic(base_size = bs) +
  theme(legend.position = c(0.285, 0.82), legend.title = element_blank(), 
        legend.background = element_rect(fill = alpha('white', 0)))
p1



#' #### Clutch initiation between years
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

# current year
y = format(Sys.Date(), "%Y")


p2 =
  ggplot(data = ds) +
  geom_violin(aes(as.character(year_), initiation_y), show.legend = FALSE, fill = 'grey85') +
  geom_point(data = dss, aes(as.character(year_), median), size = 2) +
  geom_linerange(data = dss, aes(x = as.character(year_), ymin = q75, ymax = q25), size = 0.5) +
  geom_point(data = ds[year_ == 2017 & initiation_y < as.POSIXct(paste0(y, '-06-16'))], 
             aes(as.character(year_), initiation_y),
             shape = 4, size = 1, stroke = 1.3) +
  geom_text(data = dss, aes(as.character(year_), as.POSIXct(paste0(y, '-07-02 11:03:00')), 
                            label = N), vjust = 0, size = ls) +
  scale_y_datetime(breaks = c(as.POSIXct(c(paste0(y, '-06-07'), paste0(y, '-06-14'),
                                           paste0(y, '-06-21'), paste0(y, '-06-28')))),
                   labels = c('7', '14', '21', '28')) +
  geom_text(aes(Inf, as.POSIXct(c(paste0(y, '-07-02'))), label = 'b'), vjust = vjust_label - 1.3, 
            hjust = hjust_,  size = lsa) +
  xlab('Year') + ylab('Clutch initiation date (June)') +
  theme_classic(base_size = bs)
p2

# Length of the clutch initiation period
ds = d[data_type == 'study_site' & !is.na(initiation_y)]
ds = ds[!(year_ == 2017 & initiation_y < as.POSIXct(paste0(y, '-06-16')))] # exclude to outliers in 2017
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

#--------------------------------------------------------------------------------------------------------------
#' ### Extra-pair paternity and clutch order
#--------------------------------------------------------------------------------------------------------------

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

# summary
ds[anyEPY == '1', .N, clutch_identity]
ds[, .N, clutch_identity]

ds[clutch_identity == 'first' & next_clutch == 'polyandrous', .(nestID, anyEPY)]
ds[clutch_identity == 'first' & next_clutch == 'renesting', .(nestID, anyEPY)]


# add first and second clutch of females with three clutches again (otherwise linetype does not work)
dss = ds[female_id %in% ds[clutch_identity == 'third']$female_id]
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


# fisher test to see if EPY in polyandrous clutches different to known first clutches & monogamous renesting
matrix(c(3, 0, 11, 21), ncol = 2) %>% fisher.test() 

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


#--------------------------------------------------------------------------------------------------------------
#' ### Extra-pair paternity and breeding phenology
#--------------------------------------------------------------------------------------------------------------

# load data
d = read.table('./DATA/NESTS.txt', sep = '\t',header = TRUE) %>% data.table
dp = read.table('./DATA/PATERNITY.txt', sep = '\t',header = TRUE) %>% data.table

# 
d[, initiation := as.POSIXct(initiation)]
d[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]

# subset all multiple clutches or in study site
ds = d[!is.na(initiation) & !is.na(anyEPY)]
ds = ds[, .(year_, nestID, initiation_y, anyEPY, data_type)]
ds[data_type == 'study_site', data_type := 'Intensive study']
ds[data_type != 'Intensive study', data_type := 'Other data']

# load Dale et al. data
dale = read.csv2('./DATA/Dale_et_al_1999_REPH_EPP.csv') %>% data.table
dale[, initiation_y := as.POSIXct(as.Date(initiation_doy, origin = '1993-01-01'))]

ds = rbind(ds, dale[, .(year_ = year_, nestID, initiation_y, anyEPY, data_type = 'Dale et al.')])


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
  geom_text(data = dss2, aes(data_type, Inf, group = anyEPY, label = sample_size), 
            position = position_dodge(width = 0.9), vjust = vjust_, size = ls) +
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


# save
p1 + p2 + p3 + p4 + plot_layout(ncol = 2, nrow = 2)

# ggsave('./REPORTS/FIGURES/Frequency_EPP_and_timing.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')



#--------------------------------------------------------------------------------------------------------------
#' ### Characteristics of the extra-pair sires
#--------------------------------------------------------------------------------------------------------------

# load data
d = read.table('./DATA/NESTS.txt', sep = '\t',header = TRUE) %>% data.table
dp = read.table('./DATA/PATERNITY.txt', sep = '\t',header = TRUE) %>% data.table

# change projection
d = d[parentage == TRUE]
st_transform_DT(d)

# ID_year
d[, male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
d[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]

d[, initiation := as.POSIXct(initiation)]
d[, complete := initiation + clutch_size * 86400 - 86400]

# merge with nests for data type
dp = merge(dp, d[, .(nestID, data_type)], by = 'nestID', all.x = TRUE)
dp[, father_identified := !is.na(IDfather)]
ds = dp[EPY == 1]

# Fathers identified
ds[father_identified == TRUE] %>% nrow
ds[father_identified == TRUE] %>% nrow / dp$IDfather %>% length * 100 # total EPY
ds$nestID %>% unique %>% length # clutches with EPY

# Fathers identified within intensive study & other data sources
ds[father_identified == TRUE & data_type == 'study_site'] %>% nrow
ds[data_type == 'study_site'] %>% nrow
ds[father_identified == TRUE & data_type == 'study_site'] %>% nrow / ds[data_type == 'study_site'] %>% nrow * 100

ds[father_identified == TRUE & data_type != 'study_site'] %>% nrow
ds[data_type != 'study_site'] %>% nrow
ds[father_identified == TRUE & data_type != 'study_site'] %>% nrow / ds[data_type == 'study_site'] %>% nrow * 100

# Merge social nest with nest with EPY
dp[, IDfather_year := paste0(IDfather, '_', substr(year_, 3,4 ))]
dp[, IDmother_year := paste0(IDmother, '_', substr(year_, 3,4 ))]

# subset know EPY fathers
ds = dp[father_identified == TRUE & EPY == 1, .(year_, nestID, IDmother, IDmother_year, IDfather, IDfather_year, study_site)]

# merge with nests where EPY sired
ds = merge(ds, d[nestID %in% ds$nestID, .(nestID, cuckold_male = male_id, initiation, clutch_size,
                                          complete, nest_state, nest_state_date, lat, lon)], by = 'nestID')

# merge with nests of EPY_father
ds = merge(ds, d[male_id_year %in% ds$IDfather_year, .(nestID_social = nestID, female_partner = female_id_year,
                                                       initiation_soc = initiation, clutch_size_soc = clutch_size,
                                                       complete_soc = complete, nest_state_soc = nest_state,
                                                       nest_state_date_soc = nest_state_date, lat_soc = lat,
                                                       lon_soc = lon, male_id_year)],
           by.x = 'IDfather_year', by.y = 'male_id_year', all = TRUE)

setorder(ds, IDfather_year)

# female was social female before?
ds[!is.na(IDmother), same_female := female_partner == IDmother_year]
ds[same_female == TRUE, .(nestID, IDfather_year, IDmother, nestID_social, female_partner)]

# timing of EPY nest
ds = ds[!(nestID_social == 'R320_19' & IDfather_year == '270170938_19' )] # second nest of the male (irrelevant for this analysis)
ds[, diff_EPY_to_social_nest := difftime(initiation_soc, initiation, units = 'days') %>% as.numeric]

# calculate distance between nests
ds[, dist_nests := sqrt(sum((c(lat, lon) - c(lat_soc, lon_soc))^2)) , by = 1:nrow(ds)]

# exclude males not breeding
ds[is.na(diff_EPY_to_social_nest)] %>% nrow
ds = ds[!is.na(diff_EPY_to_social_nest)]

# Format table
ds[same_female == TRUE, social_female_before := 'Yes']
ds[same_female == FALSE | is.na(same_female), social_female_before := 'No']
setorder(ds, -social_female_before, diff_EPY_to_social_nest, -dist_nests)
ds[, ID := 1:nrow(ds)]

ds = ds[, .(ID, social_female_before, difference_in_initiation = round(diff_EPY_to_social_nest, 0),
            distance_between_nests = round(dist_nests, 0))]
kable(ds)

# openxlsx::write.xlsx(ds, './REPORTS/TABLES/EPY_fathers_table.xlsx')

# difference in initiation
ds$difference_in_initiation %>% mean
ds$difference_in_initiation %>% min
ds$difference_in_initiation %>% max

# distance between nests
ds$distance_between_nests %>% median
ds$distance_between_nests %>% min
ds$distance_between_nests %>% max

# Sources of EPP within study site
8/17 * 100 # identified
1/17 * 100 # previous social male
5/17 * 100 # not previous social male
11/17 * 100 # unknown

#--------------------------------------------------------------------------------------------------------------
#' ### Timing of within- and extra-pair male-female interactions
#--------------------------------------------------------------------------------------------------------------

# load data
di = read.table('./DATA/OBSERVATIONS.txt', sep = '\t',header = TRUE) %>% data.table
dn = read.table('./DATA/NESTS.txt', sep = '\t',header = TRUE) %>% data.table
dn[, initiation := as.POSIXct(initiation)]
dn[, nest_state_date := as.POSIXct(nest_state_date)]

# datetime
di[, datetime_ := as.POSIXct(datetime_)]
di[, date_ := as.POSIXct(format(datetime_, format = '%y-%m-%d'), format = '%y-%m-%d')]

# unique by day
ds = unique(di, by = c('ID1', 'ID2', 'date_'))

# unique nest information by ID
dn[!is.na(male_id), male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
dn[!is.na(female_id), female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]


dm = dn[, .(year_, nestID, ID_year = male_id_year, study_site, initiation, nest_state_date)]
df = dn[, .(year_, nestID, ID_year = female_id_year, study_site, initiation, nest_state_date)]

dnID = rbind(dm, df)
dnID = dnID[!is.na(ID_year)]
dnID[!is.na(ID_year) & !is.na(initiation), first_initiation := min(initiation, na.rm = TRUE), by = ID_year]
dnID[!is.na(ID_year) & !is.na(initiation) & initiation != first_initiation,
     second_initiation := min(initiation, na.rm = TRUE), by = ID_year]
dnID[, second_initiation := min(second_initiation, na.rm = TRUE), by = ID_year]
dnID[!is.na(ID_year) & !is.na(nest_state_date), first_nest_state_date := min(nest_state_date, na.rm = TRUE),
     by = ID_year]
dnID[!is.na(ID_year) & !is.na(nest_state_date) & nest_state_date != first_nest_state_date,
     second_nest_state_date := min(nest_state_date, na.rm = TRUE), by = ID_year]
dnID[, second_nest_state_date := min(second_nest_state_date, na.rm = TRUE), by = ID_year]

dnID[, N_clutches := .N, by = ID_year]
dnID = unique(dnID, by = 'ID_year')


# plot settings
t_margin = 0
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
ds1 = ds[ID2 == ID1_1st_partner & ID1sex == 'M', .(ID1, ID2, diff_obs_initiation = diff_obs_1st_initiation,
                                                   type = '1st', datetime_)]
ds2 = ds[ID2 == ID1_2nd_partner & ID1sex == 'M', .(ID1, ID2, diff_obs_initiation = diff_obs_2nd_initiation,
                                                   type = '2nd', datetime_)]
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

dss = merge(dss, da[, .(ID1, ID2, datetime_, nest_still_active_factor)],
            by = c('ID1', 'ID2', 'datetime_'), all.x = TRUE)

dss[diff_obs_initiation >= 0 & nest_still_active_factor == TRUE, type2 := 'active nest']
dss[diff_obs_initiation >= 0 & nest_still_active_factor == FALSE, type2 := 'failed nest']

dss[is.na(type2), type2 := 'aabefore']


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
  theme(legend.position = 'none', plot.margin = margin_, axis.title.x = element_blank(),
        axis.text.x=element_blank()) # legend.position = c(0.9, 0.9), legend.title = element_blank()

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

dss = merge(dss, da[, .(ID1, ID2, datetime_, nest_still_active_factor)],
            by = c('ID1', 'ID2', 'datetime_'), all.x = TRUE)


dss[diff_obs_initiation >= 0 & nest_still_active_factor == TRUE, type2 := 'active nest']
dss[diff_obs_initiation >= 0 & nest_still_active_factor == FALSE, type2 := 'failed nest']
dss[is.na(type2), type2 := 'aabefore']

p2 =
  ggplot(data = dss) +
  geom_bar(aes(diff_obs_initiation, fill = type2, width = width_), width = width_,
           color = bar_line, size = bar_line_thickness) +
  geom_vline(aes(xintercept = 3), linetype = 'dotted', size = vline) +
  scale_x_continuous(limits = c(-13, 23), labels = NULL, expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 22), labels = c('', '','10', '','20'), expand = c(0, 0)) +
  scale_fill_manual(values = c(grey_, c_active, c_failed)) +
  xlab('') + ylab('Number of male-female interactions') +
  geom_text(aes(-9.5, Inf, label = sample_size3), vjust = vjust_, size = ls) +
  geom_text(aes(22, Inf, label = 'c'), vjust = vjust_label, size = lsa) +
  theme_classic(base_size = bs) +
  theme(legend.position = 'none', plot.margin = margin_, axis.title.x = element_blank(),
        axis.text.x=element_blank())
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


#--------------------------------------------------------------------------------------------------------------
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

dss = merge(dss, da[, .(ID1, ID2, datetime_, nest_still_active_factor)],
            by = c('ID1', 'ID2', 'datetime_'), all.x = TRUE)

dss[diff_obs_initiation >= 0 & nest_still_active_factor == TRUE, type2 := 'active nest']
dss[diff_obs_initiation >= 0 & nest_still_active_factor == FALSE, type2 := 'failed nest']

dss[is.na(type2), type2 := 'aabefore']

dss[diff_obs_initiation > 2]


p4 =
  ggplot(data = dss) +
  geom_bar(aes(diff_obs_initiation, fill = type2), width = width_, color = bar_line, size = bar_line_thickness) +
  geom_vline(aes(xintercept = 3), linetype = 'dotted', size = vline) +
  scale_x_continuous(limits = c(-13, 23), labels = NULL, expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 27), breaks = c(0, 5, 10, 15, 20, 25),
                     labels = c('', '', '10', '', '20', ''), expand = c(0, 0)) +
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

dss = merge(dss, da[, .(ID1, ID2, datetime_, nest_still_active_factor)],
            by = c('ID1', 'ID2', 'datetime_'), all.x = TRUE)


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
  scale_y_continuous(limits = c(0, 11), breaks = c(0, 2.5, 5, 7.5, 10), labels = c('', '', '5', '', '10'),
                     expand = c(0, 0)) +
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

# factor order
dss[!(type %in% c('previous partner', 'next partner', 'active nest')), type := 'unknown']
dss[, type := factor(type, levels = c('unknown', 'active nest', 'previous partner', 'next partner'))]

dss[type == 'active nest']


p6 =
  ggplot(data = dss) +
  geom_bar(aes(diff_obs_initiation, fill = type), width = width_, color = bar_line, size = bar_line_thickness) +
  geom_vline(aes(xintercept = 3), linetype = 'dotted', size = vline) +
  scale_x_continuous(limits = c(-13, 23), expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 11), breaks = c(0, 2.5, 5, 7.5, 10),
                     labels = c('0', '', '5', '', '10'), expand = c(0, 0)) +
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

legend = get_legend(pl)
plgg = as_ggplot(legend)

# include icon
pi1 =
  ggdraw() +
  draw_image('./DATA/ILLUSTRATIONS/reph_icon1.tif') +
  geom_text(aes(0.5, 0.5, label = 'Within-pair'), vjust = 3, size = 4)

pi2 =
  ggdraw() +
  draw_image('./DATA/ILLUSTRATIONS/reph_icon2.tif') +
  geom_text(aes(0.5, 0.5, label = 'Extra-pair'), vjust = 3, size = 4)

pi3 =
  ggdraw() +
  draw_image('./DATA/ILLUSTRATIONS/reph_icon3.tif') +
  geom_text(aes(0.5, 0.5, label = 'Extra-pair'), vjust = 3, size = 4)

# plot with icon
patchwork <- (pi1 + p1 + p4) / (pi2 + p2 + p5) / (pi3 + p3 + p6) / (plot_spacer() + plgg)

patchwork[[1]] <- patchwork[[1]] + plot_layout(widths = c(1, 2, 2))
patchwork[[2]] <- patchwork[[2]] + plot_layout(widths = c(1, 2, 2))
patchwork[[3]] <- patchwork[[3]] + plot_layout(widths = c(1, 2, 2))
patchwork[[4]] <- patchwork[[4]] + plot_layout(widths = c(0.5, 2))
patchwork + plot_layout(heights = c(3, 2, 2, 0.2))


# ggsave('./REPORTS/FIGURES/Interactions.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')


#==============================================================================================================
#' ## R PACKAGES 
#==============================================================================================================

sessionInfo()

sapply(c('data.table', 'magrittr', 'sf', 'auksRuak', 'ggplot2', 'ggnewscale', 'car', 'emmeans', 'knitr', 
         'patchwork', 'cowplot', 'ggpubr'),
       function(x) suppressPackageStartupMessages(citation(x)))




