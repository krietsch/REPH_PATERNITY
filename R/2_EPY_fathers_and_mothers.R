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

# female was social female before?
ds[!is.na(IDmother), same_female := female_partner == IDmother_year] 
ds[same_female == TRUE, .(nestID, IDfather_year, IDmother, nestID_social, female_partner)]

ds1 = ds[same_female == TRUE, .(social_female_before = .N), by = study_site]
dss = merge(dss, ds1, by = 'study_site')
dss[, social_female_before := paste0(round(social_female_before / N_EPY_eggs * 100, 0), '% (', social_female_before, '/', N_EPY_eggs, ')')]

# timing of EPY nest
ds = ds[nestID_social != 'R320_19'] # second nest of the male (irrelevant for this analysis)
ds[, while_initiation := initiation > initiation_soc & initiation < complete_soc]
ds[, while_incubation := initiation > complete_soc & initiation < nest_state_date_soc]
ds[, while_breeding   := initiation > initiation_soc & initiation < nest_state_date_soc]
ds[, diff_EPY_to_social_nest := difftime(initiation, initiation_soc, units = 'days') %>% as.numeric]

ds[, .(IDfather_year, initiation_soc, nest_state_date_soc, nest_state_soc, initiation, diff_EPY_to_social_nest, 
       same_female, while_initiation, while_incubation)]

ggplot(data = ds) +
  geom_histogram(aes(x = round(diff_EPY_to_social_nest, 0))) +
  theme_classic(base_size = 18)

mean(ds$diff_EPY_to_social_nest)
min(ds$diff_EPY_to_social_nest)
max(ds$diff_EPY_to_social_nest)

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

foreach(i = IDs) %do% {
  
  dm = dr[ID_year == i]
  dmn = d[male_id_year == i]
  dpf = dp[IDfather_year == i & EPY == 1]$IDmother_year
  dpfn = d[female_id_year == dpf & anyEPY == TRUE]
  df = dr[ID_year == dpf]
  
  dextent_r = rbind(dm, df)
  dextent_n = rbind(dmn, dpfn)
  dextent = rbind(dextent_r[, .(lat, lon)], dextent_n[, .(lat, lon)])
  
  bm = create_bm(dextent, buffer = 200)
  
  p =
    bm +
    geom_point(data = dmn, aes(lon, lat), color = 'dodgerblue4', size = 6, alpha = 0.5) +
    geom_point(data = dpfn, aes(lon, lat), color = 'firebrick2', size = 4, alpha = 0.5) +
    geom_point(data = dm, aes(lon, lat), color = 'dodgerblue4', size = 2, alpha = 0.5) +
    geom_point(data = df, aes(lon, lat), color = 'firebrick2', size = 2, alpha = 0.5) +
    ggtitle(paste('EPY father ', i, ' and female ', dpf))
  
  png(paste0('./REPORTS/EPY_nest_maps/', i,'.png'), width = 600, height = 600)
  print(p)
  dev.off()
  
}







