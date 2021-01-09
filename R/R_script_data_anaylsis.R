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

# load data
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


### Frequency of extra-pair paternity, social polyandry and renesting -----

# EPY in nests and eggs

# EPY difference between years?

# Figure 2a

# Number of EPY in each clutch

# rates of social polyandry

# Figure 2b

# rates pf renesting

# correlation EPY ~ season lenght or polyandry


### Extra-pair paternity and clutch order -----



