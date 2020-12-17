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
# 1. Captures
#------------------------------------------------------------------------------------------------------------------------




#------------------------------------------------------------------------------------------------------------------------
# 2. Nests
#------------------------------------------------------------------------------------------------------------------------





#------------------------------------------------------------------------------------------------------------------------
# 3. Behavioural observations
#------------------------------------------------------------------------------------------------------------------------





#------------------------------------------------------------------------------------------------------------------------
# 4. Paternity
#------------------------------------------------------------------------------------------------------------------------

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM PATERNITY')
DBI::dbDisconnect(con)

# nestID
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# subset nests with paternity data for at least one offspring
d[!is.na(EPY), parentage := 1]
d[, offspring_sampled := sum(parentage, na.rm = TRUE), by = nestID]
d = d[offspring_sampled > 0]

# select columns of interest
d = d[, .(year_, nestID, IDchick, IDmother, IDfather, EPY, comment)]

# save data
saveRDS(d, './DATA/PATERNITY.rds')



