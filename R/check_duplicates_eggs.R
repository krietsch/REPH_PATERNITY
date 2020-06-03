#========================================================================================================================
# Egg ID duplicates
#========================================================================================================================

# Summary
# 0. Prepare data for analysis
# 1. Captures on and off plot

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

de[ID == 'REPH485_4_18']

duplicated(de, by = 'ID')








