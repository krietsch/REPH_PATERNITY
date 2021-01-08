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












# load data
d = read.table('./DATA/CAPTURES.txt', sep = '\t',header = TRUE) %>% data.table




bm = create_bm(d, buffer = 500)
bm +
  geom_sf(data = study_site, color = 'red') +
  coord_sf(expand = FALSE) +
  geom_point(data = d, aes(lon, lat, color = data_type))


# Number of captures 






# banded each year on and off plot by us
ds = d[external == 0 & capture_id == 1 & year_ > 2016]

# Initiation date method


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



