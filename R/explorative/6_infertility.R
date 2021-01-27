#========================================================================================================================
# Infertility analysis
#========================================================================================================================

# Summary
# 0. Prepare data for analysis
# 1. Infertility in eggs
# 2. Percentage of collected eggs

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak', 'arm'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')
de = dbq(con, 'select * FROM EGGS')
dp = dbq(con, 'select * FROM PATERNITY')
dms = dbq(con, 'select * FROM MICROSATS')
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
de[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

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

# collected after incubation
d[, complete := initiation + clutch_size * 86400]
d[, diff_collected_complete := difftime(collected_datetime, complete, units = 'days') %>% as.numeric]

setorder(d, year_, initiation)

# first and second clutches by females
d[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]
d[, N_female_clutch := .N, by = female_id]
d[, female_clutch := seq_len(.N), by = .(female_id, year_)]
d[is.na(female_id), female_clutch := 1]
d[!is.na(female_id), N_female_clutch := max(female_clutch), by = .(female_id, year_)]
d[is.na(female_id), N_female_clutch := 1]
d[, .N, by = .(year_, female_clutch)]
d[, .N, by = .(female_clutch, external)]

d[, female_id := as.character(female_id)]
#------------------------------------------------------------------------------------------------------------------------
# 1. Infertility in eggs
#------------------------------------------------------------------------------------------------------------------------

# check homozygosity of undeveloped eggs 
de[undeveloped == 1]$ID
de[undeveloped == 1]$ID %>% length

# subset undeveloped eggs
dm = dms[ID %in% de[undeveloped == 1]$ID]
de = de[ID %in% de[undeveloped == 1]$ID]

# merge with father microsats
dm = merge(dm, unique(de[, .(ID, nestID, external)], by = 'ID'), by = 'ID', all.x = TRUE)
dm = merge(dm, dp[, .(IDchick, IDmother, IDfather)], by.x  = 'ID', by.y = 'IDchick', all.x = TRUE)
dm[, IDfather := as.character(IDfather)]
dm[, IDmother := as.character(IDmother)]
dm = merge(dm, unique(dms[, .(ID, marker, af = a, bf = b)], by = c('ID', 'marker')), 
           by.x  = c('IDfather', 'marker'), by.y = c('ID', 'marker'), all.x = TRUE)
dm = merge(dm, unique(dms[, .(ID, marker, am = a, bm = b)], by = c('ID', 'marker')), 
           by.x  = c('IDmother', 'marker'), by.y = c('ID', 'marker'), all.x = TRUE)

# which chicks were undeveloped and have no genotype
dm[is.na(a) & !is.na(b)]
dm[is.na(b) & !is.na(a)]

dm[, aNA := ifelse(is.na(a), 0, 1)]
dm[, N_genotype := sum(aNA), by = ID]

# homozygot and heterozygot
dm[, homozygot := ifelse(a == b, 1, 0)]
dm[, heterozygot := ifelse(a == b, 0, 1)]

dm[, N_homo := sum(homozygot, na.rm = TRUE), by = ID]
dm[, N_hetero := sum(heterozygot, na.rm = TRUE), by = ID]

# what could have come from the father?
dm[, a_pot_father := ifelse(a == af | a == bf, 1, 0)]
dm[, b_pot_father := ifelse(b == af | b == bf, 1, 0)]
dm[, pot_father := ifelse(a_pot_father == 1 | a_pot_father == 1, 1, 0)]
dm[, N_pot_father := sum(pot_father, na.rm = TRUE), by = ID]

# what could have come from the mother?
dm[, a_pot_mother := ifelse(a == am | a == bm, 1, 0)]
dm[, b_pot_mother := ifelse(b == am | b == bm, 1, 0)]
dm[, pot_mother := ifelse(a_pot_mother == 1 | a_pot_mother == 1, 1, 0)]
dm[, N_pot_mother := sum(pot_mother, na.rm = TRUE), by = ID]


ds = unique(dm[, .(ID, nestID, IDfather, IDmother, N_genotype, N_homo, N_hetero, N_pot_father, N_pot_mother)], by = 'ID')
ds[, per_hetero := round(N_hetero / N_genotype * 100, 2)]

# only include our data
ds = ds[!(ID %like% 'REPH')]

# assign potenially infertile clutches
ds[, pot_infertile := N_genotype < 2]

setorder(ds, per_hetero)
ds

# check female clutch
ds = merge(ds, d[, .(nestID, female_clutch, N_female_clutch, initiation, diff_collected_complete)], by.x = 'nestID', by.y = 'nestID', all.x = TRUE)

# known next clutch
ds[, known_next_clutch := female_clutch < N_female_clutch]

setorder(ds, -known_next_clutch, female_clutch, IDmother)
ds

#------------------------------------------------------------------------------------------------------------------------
# 2. Percentage of collected eggs
#------------------------------------------------------------------------------------------------------------------------

d = d[external == 0 & !is.na(collected_datetime)]

# undeveloped eggs 
ds %>% nrow

# undeveloped eggs in N eggs
ds %>% nrow / sum(d$clutch_size) * 100

# undeveloped eggs in N clutches
ds$nestID %>% unique %>% length
d %>% nrow
ds$nestID %>% unique %>% length / d %>% nrow * 100

# undeveloped eggs potentially infertile 
ds[pot_infertile == TRUE] %>% nrow

# undeveloped eggs in N eggs
ds[pot_infertile == TRUE] %>% nrow / sum(d$clutch_size) * 100

# known_next_clutch and infertile egg in clutch before
# no egg infertile in first clutch of female which laid another clutch afterwards

setorder(ds, IDmother)
ds

hist(ds$diff_collected_complete)

ds[IDmother == '19222']$diff_collected_complete

ds$diff_collected_complete %>% median
ds$diff_collected_complete %>% min
ds$diff_collected_complete %>% max
