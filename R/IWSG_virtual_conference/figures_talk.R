#========================================================================================================================
# Figures for talk
#========================================================================================================================

# Summary
# 0. Prepare data for analysis
# 1. Captures on and off plot
# 2. Incubation length & initiation date method
# 3. Rate of polyandry & renesting on plot
# 4. Paternity data
# 5. Paternity between study site and external
# 6. Paternity between years 
# 7. Paternity polyandrous clutches & renesting 
# 8. Timing of second and third clutches
# 9. Paternity frequency within the season 

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak', 'patchwork', 'multcomp'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')
dc = dbq(con, 'select * FROM CAPTURES')
dp = dbq(con, 'select * FROM PATERNITY')
DBI::dbDisconnect(con)

#------------------------------------------------------------------------------------------------------------------------
# 0. Prepare data for analysis
#------------------------------------------------------------------------------------------------------------------------

# Change projection
ds = d[is.na(lon)] # seperate data without position
d = d[!is.na(lon)]
st_transform_DT(d)

# Assign locations in the study area 
point_over_poly_DT(d, poly = study_site, buffer = 10)
setnames(d, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := FALSE]
d[male_id == 175189876, study_site := FALSE] # exclude one male from 2006
d = rbind(d, ds)

# check data
# bm = create_bm(d[study_site == TRUE], buffer = 500)
# bm +
#   geom_point(data = d, aes(lon, lat, color = study_site))

# nestID
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dp[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# assign datetime format
d[, found_datetime := as.POSIXct(found_datetime)]
d[, collected_datetime := as.POSIXct(collected_datetime)]
d[, initiation := as.POSIXct(initiation)]
d[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, initiation_doy := yday(initiation)]
d[, est_hatching_datetime := as.POSIXct(est_hatching_datetime)]
d[, hatching_datetime := as.POSIXct(hatching_datetime)]
d[, nest_state_date := as.POSIXct(nest_state_date)]
setorder(d, year_, initiation)
d[, YEAR_ := factor(year_)]

# nests with paternity data
dp = dp[!is.na(EPY)]
# EPY father 
dp[EPY == 1, .N, by = nestID] # more than 1 EPY father? No
dp[EPY == 1, EPY_father := IDfather]
dps = dp[, .(EPY = sum(EPY), N = .N, EPY_father = min(EPY_father, na.rm = TRUE)), by = .(nestID, year_)]
dps[, anyEPY := ifelse(EPY > 0, 1, 0)]
dps[is.infinite(EPY_father), EPY_father := NA]
dps[!is.na(EPY_father)] %>% nrow # known EPY fathers

# merge with nests
d = merge(d, dps[, .(nestID, N_parentage = N, N_EPY = EPY, anyEPY, EPY_father)], by = 'nestID', all.x = TRUE)

# assign all nests with parentage data
d[, parentage := ifelse(!is.na(N_parentage), TRUE, FALSE)]
d[, any_parentage_year := any(parentage == TRUE), by = year_]

#------------------------------------------------------------------------------------------------------------------------
# 6. Paternity between years 
#------------------------------------------------------------------------------------------------------------------------

# subset nests with parentage
ds = d[parentage == TRUE]

ggplot(data = ds) +
  geom_bar(aes(YEAR_), stat="count")


# split in NARL study site and everything else by year
ds = d[, .(N_nests = .N), by = .(year_, study_site)]
ds2 = d[parentage == TRUE, .(N_parentage = .N), by = .(year_, study_site)]
ds3 = d[anyEPY == TRUE, .(N_EPY = .N), by = .(year_, study_site)]
ds4 = d[, .(N_eggs = sum(N_parentage, na.rm = TRUE), N_eggs_EPY = sum(N_EPY, na.rm = TRUE)), by = .(year_, study_site)]
ds = merge(ds, ds2, by = c('year_', 'study_site'), all.x = TRUE)
ds = merge(ds, ds3, by = c('year_', 'study_site'), all.x = TRUE)
ds = merge(ds, ds4, by = c('year_', 'study_site'), all.x = TRUE)
ds[is.na(ds)] = 0
ds = ds[N_parentage != 0]
ds[, EPY_nests := paste0(round(N_EPY / N_parentage * 100, 0), '% (', N_EPY, '/', N_parentage, ')')]
ds[, EPY_eggs  := paste0(round(N_eggs_EPY / N_eggs * 100, 0), '% (', N_eggs_EPY, '/', N_eggs, ')')]

ds[, EPY_nests := round(N_EPY / N_parentage * 100, 0)]

ggplot(data = ds) +
  geom_bar(aes(factor(year_), EPY_nests, fill = study_site, group = study_site), stat="identity", position = "dodge")


dss = data.table(YEAR_ = c('2017', '2018', '2019'), 
                 type  = c(rep('epp', 3), rep('polyandry', 3)), 
                 percent = c(3, 6, 14, 3, 5, 7))
         

ggplot(data = dss) +
  geom_bar(aes(YEAR_, percent, fill = type, group = type), stat="identity", position = "dodge")



dss = data.table(data = c('SPSA', 'EUDO', 'WIPH', 'RNPH', 'REPH_Dale', 'REPH_ext', 'REPH_study'), 
                 N_nests = c(34, 22, 17, 63, 18, 169, 165),
                 N_EPY = c(7, 2, 0, 4, 6, 21, 16))

dss[, EPY_nests_per := round(N_EPY / N_nests * 100, 0)]
dss[, data := factor(data, levels = c('SPSA', 'EUDO', 'WIPH', 'RNPH', 'REPH_Dale', 'REPH_ext', 'REPH_study'))]
dss[, sample_size := paste0(N_EPY, '/', N_nests)]

p = 
ggplot(data = dss) +
  geom_bar(aes(data, EPY_nests_per), stat = 'identity', position = 'dodge', width = 0.5) +
  xlab('') + ylab('Nests with EPP (%)') +
  geom_text(data = dss, aes(data, EPY_nests_per + 2, label = sample_size), vjust = 1, size = 6) +
  theme_classic(base_size = 18)
p










