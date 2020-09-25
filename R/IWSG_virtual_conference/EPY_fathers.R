#========================================================================================================================
# WHat is know about EPY fathers and mothers?
#========================================================================================================================

# Summary
# 0. Prepare data for analysis
# 1. Known EPY fathers
# 2. Breeding status and timing of EPY fathers

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak', 'foreach'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')
dc = dbq(con, 'select * FROM CAPTURES')
dp = dbq(con, 'select * FROM PATERNITY')
dr = dbq(con, 'select * FROM RESIGHTINGS')
DBI::dbDisconnect(con)

#------------------------------------------------------------------------------------------------------------------------
# 0. Prepare data for analysis
#------------------------------------------------------------------------------------------------------------------------

# Change projection
ds = d[is.na(lon)] # seperate data without position
d = d[!is.na(lon)]
st_transform_DT(d)
st_transform_DT(dr)

# Assign locations in the study area 
point_over_poly_DT(d, poly = study_site, buffer = 10)
setnames(d, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := FALSE]
d[male_id == 175189876, study_site := FALSE] # exclude one male from 2006
d = rbind(d, ds)

# nestID
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dp[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# assign datetime format
d[, found_datetime := as.POSIXct(found_datetime)]
d[, collected_datetime := as.POSIXct(collected_datetime)]
d[, initiation := as.POSIXct(initiation)]
d[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, initiation_doy := yday(initiation)]
d[, complete := initiation + clutch_size * 86400 - 86400]
d[, complete_y := initiation_y + clutch_size * 86400 - 86400]
d[, est_hatching_datetime := as.POSIXct(est_hatching_datetime)]
d[, hatching_datetime := as.POSIXct(hatching_datetime)]
setorder(d, year_, initiation)
d[, YEAR_ := factor(year_)]
dr[, datetime_ := as.POSIXct(datetime_)]
dr[, datetime_y := as.POSIXct(format(datetime_, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
dr[, year_ := year(datetime_)]

# ID_year
d[, male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
d[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]
dp[, IDfather_year := paste0(IDfather, '_', substr(year_, 3,4 ))]
dp[, IDmother_year := paste0(IDmother, '_', substr(year_, 3,4 ))]
dc[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]
dr[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]

# nests with paternity data
dp = dp[!is.na(EPY)]
# EPY father 
dp[EPY == 1, .N, by = nestID] # more than 1 EPY father? No
dp[EPY == 1, EPY_father := IDfather]
dps = dp[, .(EPY = sum(EPY), N = .N, EPY_father = min(EPY_father, na.rm = TRUE)), by = .(nestID, year_)]
dps[, anyEPY := ifelse(EPY > 0, 1, 0)]
dps[is.infinite(EPY_father), EPY_father := NA]
dps[!is.na(EPY_father)] %>% nrow # known EPY fathers

# assign study_site data
dp[nestID %in% d[study_site == TRUE]$nestID, study_site := TRUE]
dp[is.na(study_site), study_site := FALSE]

# merge with nests
d = merge(d, dps[, .(nestID, N_parentage = N, N_EPY = EPY, anyEPY, EPY_father)], by = 'nestID', all.x = TRUE)

# assign all nests with parentage data
d[, parentage := ifelse(!is.na(N_parentage), TRUE, FALSE)]
d[, any_parentage_year := any(parentage == TRUE), by = year_]

# first and second clutches by females
d[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]
d[, N_female_clutch := .N, by = female_id]
d[, female_clutch := seq_len(.N), by = .(female_id, year_)]
d[is.na(female_id), female_clutch := 1]
d[!is.na(female_id), N_female_clutch := max(female_clutch), by = .(female_id, year_)]
d[is.na(female_id), N_female_clutch := 1]

#------------------------------------------------------------------------------------------------------------------------
# 1. Known EPY fathers
#------------------------------------------------------------------------------------------------------------------------

# double check N nests & eggs with EPY
dps = dp[EPY == 1]
dps[, IDfather_identified := !is.na(IDfather)]

# nests with EPY
unique(dps$nestID) %>% length

# eggs with EPY
nrow(dps)

# nests with multiple EPY
dps[, N_EPY := .N, by = nestID]
dpu = unique(dps, by = 'nestID')
dpu[, .N, by = N_EPY]

# How many different EPY fathers?
dps[N_EPY > 1, .(nestID, EPY_father, IDfather_identified)]
# only one case known with two different fathers: R409_19

# How many EPY fathers identified?
ds1 = dps[, .(N_EPY_eggs = .N), by = study_site]
ds2 = dps[IDfather_identified == TRUE, .(EPY_father_identified = .N), by = study_site]

dss = merge(ds1, ds2, by = 'study_site')
dss[, EPY_father_identified := paste0(round(EPY_father_identified / N_EPY_eggs * 100, 0), '% (', EPY_father_identified, '/', N_EPY_eggs, ')')]
dss

#------------------------------------------------------------------------------------------------------------------------
# 2. Breeding status and timing of EPY fathers
#------------------------------------------------------------------------------------------------------------------------

# any tagged EPY fathers?
dc[ID %in% dps$IDfather & !is.na(gps_tag), .(year_, ID, gps_tag)]
dc[ID %in% dps$IDmother & !is.na(gps_tag), .(year_, ID, gps_tag)]

# subset know EPY fathers
ds = dps[IDfather_identified == TRUE, .(year_, nestID, IDmother, IDmother_year, IDfather, IDfather_year, site_EPY_father = study_site)]

# merge with nests where EPY sired
ds = merge(ds, d[nestID %in% ds$nestID, .(nestID, cuckold_male = male_id, initiation, clutch_size, 
                                          complete, nest_state, nest_state_date, lat, lon, study_site)], by = 'nestID')

# merge with nests of EPY_father
ds = merge(ds, d[male_id_year %in% ds$IDfather_year, .(nestID_social = nestID, female_partner = female_id_year, initiation_soc = initiation, 
                                                       clutch_size_soc = clutch_size, complete_soc = complete, nest_state_soc = nest_state, 
                                                       nest_state_date_soc = nest_state_date, lat_soc = lat, lon_soc = lon, male_id_year)], 
           by.x = 'IDfather_year', by.y = 'male_id_year', all = TRUE)

setorder(ds, IDfather_year)

# breeding
dpu = unique(ds, by = 'IDfather_year')
ds1 = dpu[!is.na(nestID), .(breeding = .N), by = study_site]
dss = merge(dss, ds1, by = 'study_site')
dss[, breeding := paste0(round(breeding / N_EPY_eggs * 100, 0), '% (', breeding, '/', N_EPY_eggs, ')')]

# other two males where breeding the year before
d[male_id %in% dpu[is.na(nestID_social)]$IDfather]
dr[ID == 270170094 & year_ == 2019]
dr[ID == 270170262 & year_ == 2018]

# female was social female before?
ds[!is.na(IDmother), same_female := female_partner == IDmother_year] 
ds[same_female == TRUE, .(nestID, IDfather_year, IDmother, nestID_social, female_partner)]

ds1 = ds[same_female == TRUE, .(social_female_before = .N), by = study_site]
dss = merge(dss, ds1, by = 'study_site')
dss[, social_female_before := paste0(round(social_female_before / N_EPY_eggs * 100, 0), '% (', social_female_before, '/', N_EPY_eggs, ')')]

# timing of EPY nest
ds = ds[!(nestID_social == 'R320_19' & IDfather_year == '270170938_19' )] # second nest of the male (irrelevant for this analysis)
ds[, while_initiation := initiation > initiation_soc & initiation < complete_soc]
ds[, while_incubation := initiation > complete_soc & initiation < nest_state_date_soc]
ds[, while_breeding   := initiation > initiation_soc & initiation < nest_state_date_soc]
ds[, diff_EPY_to_social_nest := difftime(initiation_soc, initiation, units = 'days') %>% as.numeric]
ds[, diff_EPY_to_social_nest_complete := difftime(complete_soc, initiation, units = 'days') %>% as.numeric]
ds[, diff_EPY_to_social_nest_state := difftime(nest_state_date, initiation, units = 'days') %>% as.numeric]

ds[, .(IDfather_year, initiation_soc, nest_state_date_soc, nest_state_soc, initiation, diff_EPY_to_social_nest, 
       same_female, while_initiation, while_incubation)]

setorder(ds, year_, initiation)

ggplot(data = ds) +
  geom_histogram(aes(x = round(diff_EPY_to_social_nest, 0))) +
  theme_classic(base_size = 18)

ddiff = ds[, .(IDfather_year, site_EPY_father, initiation, initiation_soc, diff_EPY_to_social_nest, same_female)]
ddiff[, diff_EPY_to_social_nest := as.integer(round(diff_EPY_to_social_nest, 0))]

ddiff = ddiff[!is.na(diff_EPY_to_social_nest)]
ddiff[is.na(same_female), same_female2 := 'unknown']
ddiff[same_female == TRUE, same_female2 := 'previous partner']
ddiff[same_female == FALSE, same_female2 := 'not previous partner']

p = 
ggplot(data = ddiff) +
  geom_bar(aes(x = round(diff_EPY_to_social_nest, 0), fill = same_female2), width = 0.9) +
  scale_x_continuous(limits = c(-11, 2), n.breaks = 9) +
  scale_y_continuous(limits = c(0, 2), labels = c('0', '','1', '','2')) +
  scale_fill_manual(values = c('#95D840FF', '#33638DFF', 'grey50')) +
  xlab('Difference initiation date (EPY fathered, social father)') + ylab('') + 
  theme_classic(base_size = 20) + theme(legend.title = element_blank(), legend.position = 'none')
p 


pgrey = 
  ggplot(data = ddiff) +
  geom_bar(aes(x = round(diff_EPY_to_social_nest, 0), fill = same_female2), width = 0.9) +
  scale_x_continuous(limits = c(-11, 2), n.breaks = 9) +
  scale_y_continuous(limits = c(0, 2), labels = c('0', '','1', '','2')) +
  scale_fill_manual(values = c('grey50', 'grey50', 'grey50')) +
  xlab('Difference initiation date (EPY fathered, social father)') + ylab('') + 
  theme_classic(base_size = 20) + theme(legend.title = element_blank(), legend.position = 'none')
pgrey 



png(paste0('./REPORTS/FIGURES/EPY_fathers_social.png'), width = 600, height = 500)
p
dev.off()


png(paste0('./REPORTS/FIGURES/EPY_fathers_social_grey.png'), width = 600, height = 500)
pgrey
dev.off()

ds$diff_EPY_to_social_nest
ds$diff_EPY_to_social_nest %>% length
ds$diff_EPY_to_social_nest %>% mean
ds$diff_EPY_to_social_nest %>% min
ds$diff_EPY_to_social_nest %>% max

ds$diff_EPY_to_social_nest_complete

ds[, .(nestID, initiation, initiation_soc, diff_EPY_to_social_nest, complete_soc, diff_EPY_to_social_nest_complete, same_female)]


# female was not social female before?
ds[is.na(same_female), same_female := FALSE] # can be excluded with initiation dates
ds1 = ds[same_female == FALSE, .(not_social_female_before = .N), by = study_site]
dss = merge(dss, ds1, by = 'study_site', all.x = TRUE)
dss[is.na(dss)] = 0
dss[, not_social_female_before := paste0(round(not_social_female_before / N_EPY_eggs * 100, 0), '% (', not_social_female_before, '/', N_EPY_eggs, ')')]

dss[, N_EPY_eggs := NULL]
dss

#------------------------------------------------------------------------------------------------------------------------
# 3. Distance between EPY nest and social nest and maps
#------------------------------------------------------------------------------------------------------------------------

# calculate distance between nests
ds[, dist_nests := sqrt(sum((c(lat, lon) - c(lat_soc, lon_soc))^2)) , by = 1:nrow(ds)]

#------------------------------------------------------------------------------------------------------------------------
# 4. Summary table & tarsus and bill length extra-pair fathers
#------------------------------------------------------------------------------------------------------------------------


dsd = ds[, .(year = year_, IDfather, cuckold_male, same_female, nest_initiation = as.Date(initiation), study_site,
             diff_EPY_to_social_nest = round(diff_EPY_to_social_nest, 0)*-1, while_breeding, dist_nests = round(dist_nests, 0))]
dsd

dcu = dc[, .(tarsus = mean(tarsus, na.rm = TRUE), culmen = mean(culmen, na.rm = TRUE), 
             weight = mean(weight, na.rm = TRUE), min_year = min(year_, na.rm = TRUE)), by = ID]
dcu[tarsus > 25, tarsus := NA]

dsd = merge(dsd, dcu[, .(ID, tarsus, culmen, weight, min_year)], by.x = 'cuckold_male', by.y = 'ID', all.x = TRUE)
dsd = merge(dsd, dcu[, .(ID, tarsus_EPY_father = tarsus, culmen_EPY_father = culmen, weight_EPY_father = weight, 
                         min_year_EPY_father = min_year)], by.x = 'IDfather', by.y = 'ID', all.x = TRUE)


dsd[, diff_tarsus := tarsus_EPY_father - tarsus]
dsd[, diff_culmen := culmen_EPY_father - culmen]
dsd[, diff_age := min_year - min_year_EPY_father]
ds = copy(dsd)

dsd = dsd[, .(IDfather, same_female, diff_EPY_to_social_nest, while_breeding, dist_nests, diff_tarsus, 
              diff_culmen, diff_age, study_site)]

setorder(dsd, -same_female, -while_breeding, na.last = TRUE)
dsd








