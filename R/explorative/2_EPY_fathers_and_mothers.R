#========================================================================================================================
# WHat is know about EPY fathers and mothers?
#========================================================================================================================

# Summary
# 0. Prepare data for analysis
# 1. Known EPY fathers
# 2. Breeding status and timing of EPY fathers
# 3. Distance between EPY nest and social nest and maps

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

# known fathers
dp[, father_identified := !is.na(IDfather)]
dp[, .N, by = .(father_identified, study_site)]
dp[, .N, by = .( study_site)]
587/600*100 # fathers assigned in study site
553/582*100 # fathers assigned out of study site

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

dps[study_site == TRUE, .(EPY_father_identified = .N), by = IDfather_identified]
dps[study_site == TRUE, .(year_, nest, IDfather, EPY_father)]

# possible sources of EPP
# one known previous male 
1/17*100
11/17*100

# 5 previous social male
5/17*100

# 2 unknown breeding history
2/17*100

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
ds = dps[IDfather_identified == TRUE, .(year_, nestID, IDmother, IDmother_year, IDfather, IDfather_year, study_site)]

# merge with nests where EPY sired
ds = merge(ds, d[nestID %in% ds$nestID, .(nestID, cuckold_male = male_id, initiation, clutch_size, 
                                          complete, nest_state, nest_state_date, lat, lon)], by = 'nestID')

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

ds[, .(IDfather_year, initiation_soc, nest_state_date_soc, nest_state_soc, initiation, diff_EPY_to_social_nest, 
       same_female, while_initiation, while_incubation)]

setorder(ds, year_, initiation)

ggplot(data = ds) +
  geom_histogram(aes(x = round(diff_EPY_to_social_nest, 0))) +
  theme_classic(base_size = 18)

ds$diff_EPY_to_social_nest
ds$diff_EPY_to_social_nest %>% length
ds$diff_EPY_to_social_nest %>% mean
ds$diff_EPY_to_social_nest %>% min
ds$diff_EPY_to_social_nest %>% max

ds$diff_EPY_to_social_nest_complete

ds[, .(nestID, nestID_social, initiation, initiation_soc, diff_EPY_to_social_nest, complete_soc, diff_EPY_to_social_nest_complete, same_female)]

ds[, .(IDfather, nestID, initiation, nestID_social, initiation_soc, diff_EPY_to_social_nest, complete_soc, diff_EPY_to_social_nest_complete, same_female)]

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

ggplot(data = ds) +
  geom_histogram(aes(x = round(dist_nests, 0))) +
  theme_classic(base_size = 18)

ds$dist_nests
mean(ds$dist_nests)
median(ds$dist_nests)
min(ds$dist_nests)
max(ds$dist_nests)

setorder(d, initiation)

# create base maps
bm1 = create_bm(d[study_site == TRUE], buffer = 500)
bm2 = create_bm(d, buffer = 500)
bm3 = create_bm(d[study_site == TRUE], buffer = 1000)


# nests with EPY and without
p = 
  bm1 +
  geom_point(data = d[external == 0], aes(lon, lat, color = as.character(anyEPY)), size = 2)
p

# only EPY nests
p = 
  bm1 +
  geom_point(data = d[external == 0 & anyEPY == TRUE], aes(lon, lat), size = 2)
p

# map of sightings of female and EPP father with nests
IDs = dpu$IDfather_year
IDs = dr[ID_year %in% IDs]$ID_year %>% unique # subset only males that where seen in the year of EPY
# i = IDs[2]

# foreach(i = IDs) %do% {
#   
#   dm = dr[ID_year == i]
#   dmn = d[male_id_year == i]
#   dpf = dp[IDfather_year == i & EPY == 1]$IDmother_year
#   dpfn = d[female_id_year == dpf & anyEPY == TRUE]
#   df = dr[ID_year == dpf]
#   
#   dextent_r = rbind(dm, df)
#   dextent_n = rbind(dmn, dpfn)
#   dextent = rbind(dextent_r[, .(lat, lon)], dextent_n[, .(lat, lon)])
#   
#   bm = create_bm(dextent, buffer = 200)
#   
#   p =
#     bm +
#     geom_point(data = dmn, aes(lon, lat), color = 'dodgerblue4', size = 6, alpha = 0.5) +
#     geom_point(data = dpfn, aes(lon, lat), color = 'firebrick2', size = 4, alpha = 0.5) +
#     geom_point(data = dm, aes(lon, lat), color = 'dodgerblue4', size = 2, alpha = 0.5) +
#     geom_point(data = df, aes(lon, lat), color = 'firebrick2', size = 2, alpha = 0.5) +
#     ggtitle(paste('EPY father ', i, ' and female ', dpf))
#   
#   png(paste0('./REPORTS/EPY_nest_maps/', i,'.png'), width = 600, height = 600)
#   print(p)
#   dev.off()
#   
# }


#------------------------------------------------------------------------------------------------------------------------
# 4. Summary table & tarsus and bill length extra-pair fathers
#------------------------------------------------------------------------------------------------------------------------


dsd = ds[, .(year = year_, IDfather, cuckold_male, same_female, nest_initiation = as.Date(initiation),
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
              diff_culmen, diff_age)]

setorder(dsd, -same_female, -while_breeding, na.last = TRUE)
dsd

# openxlsx::write.xlsx(dsd, './REPORTS/EPY_frequency/EPY_fathers_table.xlsx')

dsd$diff_EPY_to_social_nest %>% mean(., na.rm = TRUE)
dsd$dist_nests %>% median(., na.rm = TRUE)
dsd$diff_tarsus %>% mean(., na.rm = TRUE)
dsd$diff_culmen %>% mean(., na.rm = TRUE)
dsd$diff_age %>% mean(., na.rm = TRUE)


# t-tests
ds = ds[!is.na(tarsus) & !is.na(!culmen)]
t.test(ds$tarsus, ds$tarsus_EPY_father, paired = TRUE)
t.test(ds$culmen, ds$culmen_EPY_father, paired = TRUE)
t.test(ds$min_year, ds$min_year_EPY_father, paired = TRUE)

#------------------------------------------------------------------------------------------------------------------------
# 5. Distance betweem female nests
#------------------------------------------------------------------------------------------------------------------------


# polyandrous clutches (second clutch with different partner)
ID2c = d[female_clutch == 2]$female_id_year
dx = d[female_id_year %in% ID2c]

dr = merge(dx[female_clutch == 1, .(year1 = year_, nestID1 = nestID, female_id_year, m1 = male_id, anyEPY1 = anyEPY, 
                                    ss1 = study_site, initiation1 = initiation, lat, lon)], 
           dx[female_clutch == 2, .(year2 = year_, nestID2 = nestID, female_id_year, m2 = male_id, anyEPY2 = anyEPY, 
                                    EPY_father2 = EPY_father, ss2 = study_site, initiation2 = initiation, lat2 = lat, lon2 = lon)], 
           by = 'female_id_year', all = TRUE)

dr[, same_male := m1 == m2]
dr[is.na(same_male), same_male := FALSE]
dr[, both_study_site := ss1 == ss2]
dr[, diff_initiation := difftime(initiation2, initiation1, units = 'days') %>% as.numeric]
setorder(dr, female_id_year)

# distance between nests
dr[, dist_nests := sqrt(sum((c(lat, lon) - c(lat2, lon2))^2)) , by = 1:nrow(dr)]








