# Check spatial sigtings of EPP fathers in relation to mothers

# Summary

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


# check data
# ggplot() + 
#   geom_sf(data = study_site, fill = 'grey95') +
#   geom_point(data = d, aes(lon, lat, color = study_site))

# merge with data without position
ds[, study_site := NA]
dn = rbind(dn, ds)

# bring everything in the right format
dn[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dn[, initiation := anytime(initiation)]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
dn[, initiation_doy := yday(initiation)]
dn[, YEAR_ := factor(year_)]
dn[, complete := initiation_y + clutch_size * 86400]

setorder(dn, year_, initiation)
dn[, female_id := factor(female_id, levels = unique(female_id[order(initiation)]))]
dn[, male_id := factor(male_id, levels = unique(male_id[order(initiation)]))]
dn[, nestID := factor(nestID, levels = unique(nestID[order(initiation)]))]

# Parentage data
dp[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dp = dp[!is.na(EPY)]

# assign external data
dp[nestID %in% dn[external == 0]$nestID, external := 0]
dp[is.na(external), external := 1]

# EPY father 
dp[EPY == 1, .N, by = nestID] # more than 1 EPY father? No
dp[EPY == 1, EPY_father := IDfather]

# nests with data
dps = dp[, .(EPY = sum(EPY), N = .N, EPY_father = min(EPY_father, na.rm = TRUE)), by = .(nestID, year_, external)]
dps[, anyEPY := ifelse(EPY > 0, 1, 0)]
dps[is.infinite(EPY_father), EPY_father := NA]
dps[!is.na(EPY_father)] %>% nrow # known EPY fathers

# merge with nests
dn = merge(dn, dps[, .(nestID, N_EPY = N, anyEPY, EPY_father)], by = 'nestID', all.x = TRUE)

# assign all nests with parentage data
dn[, parentage := ifelse(!is.na(N_EPY), TRUE, FALSE)]
dn[, any_parentage_year := any(parentage == TRUE), by = year_]

# load nest info by ID created in 1_nests_&_parentage
load('./DATA/ID_nest_information.RData')

# change format
dnID[, year_ := as.factor(year_)]
d[, ID := as.factor(ID)]
d[, year_ := as.factor(year(datetime_))]

# merge data 
d = merge(d, dnID, by = c('year_', 'ID'), all.x = TRUE)

# fill NA
d[is.na(any_nest_study_site), any_nest_study_site := FALSE]

d[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]
dn[, male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
dn[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]
dp[, IDfather_year := paste0(IDfather, '_', substr(year_, 3,4 ))]
dp[, IDmother_year := paste0(IDmother, '_', substr(year_, 3,4 ))]

#------------------------------------------------------------------------------------------------------------------------
# 1. What's known aboubt EPY fathers?
#------------------------------------------------------------------------------------------------------------------------

dps = dp[EPY == 1 & !is.na(IDfather)]

ds = d[ID_year %in% dps$IDfather_year] 
dss = unique(ds, by = 'ID_year')

dps[!(IDfather %in% dss$ID)]
# 241159604 male on Rick's plots (breeding)

dss[any_nest_study_site == TRUE]
# all known EPY fathers where breeding in the study site

# any tagged EPY fathers?
dc[ID %in% dps$IDfather & !is.na(gps_tag), .(year_, ID, gps_tag)]
dc[ID %in% dps$IDmother & !is.na(gps_tag), .(year_, ID, gps_tag)]

# timing of EPF in relation to own nest initiation
dnEPP = dn[male_id_year %in% dss$ID_year]

#------------------------------------------------------------------------------------------------------------------------
# 2. Map of nests with EPP
#------------------------------------------------------------------------------------------------------------------------

setorder(dn, initiation)

# create base maps
bm1 = create_bm(dn[study_site == TRUE], buffer = 500)
bm2 = create_bm(dn, buffer = 500)
bm3 = create_bm(dn[study_site == TRUE], buffer = 1000)


# nests and secend nests within / between years
p = 
  bm1 +
  geom_point(data = dn[external == 0], aes(lon, lat, color = as.character(anyEPY)), size = 2)
p


# map of sightings of female and EPP father with nests
IDs = dss$ID_year
i = dss$ID_year[2]

foreach(i = IDs) %do% {
  
  dm = d[ID_year == i]
  dmn = dn[male_id_year == i]
  dpf = dp[IDfather_year == i & EPY == 1]$IDmother_year
  dpfn = dn[female_id_year == dpf]
  df = d[ID_year == dpf]
  
  dextent = rbind(dm, df)
  
  bm = create_bm(dextent, buffer= 200)
  
  p =
  bm +
    geom_point(data = dmn, aes(lon, lat), color = 'blue', alpha = 0.5, size = 6) +
    geom_point(data = dpfn, aes(lon, lat), color = 'red', alpha = 0.5, size = 4) +
    geom_point(data = dm, aes(lon, lat), color = 'blue', size = 2) +
    geom_point(data = df, aes(lon, lat), color = 'red', size = 2) +
    ggtitle(paste('EPY father ', i, ' and female ', dpf))
  
  png(paste0('./REPORTS/EPP_FATHERS/', i,'.png'), width = 600, height = 600)
  print(p)
  dev.off()
  
}
  

# two males not seen in the year of EPP
i = 270170094
i = 270170262


dm = d[ID == i]
dmn = dn[male_id == i]
dpf = dp[IDfather == i & EPY == 1]$IDmother_year
dpfn = dn[female_id_year == dpf]
df = d[ID_year == dpf]

dextent = rbind(dm, df)
bm = create_bm(dextent, buffer= 200)

p = 
bm +
  geom_point(data = dmn, aes(lon, lat), color = 'blue', alpha = 0.5, size = 6) +
  geom_point(data = dpfn, aes(lon, lat), color = 'red', alpha = 0.5, size = 4) +
  geom_point(data = dm, aes(lon, lat), color = 'blue', size = 2) +
  geom_point(data = df, aes(lon, lat), color = 'red', size = 2) +
  ggtitle(paste('EPY father (year before) ', i, ' and female ', dpf))


png(paste0('./REPORTS/EPP_FATHERS/', i,'.png'), width = 600, height = 600)
print(p)
dev.off()













