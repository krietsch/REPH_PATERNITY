#========================================================================================================================
# Egg ID duplicates
#========================================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'DBI'),
        require, character.only = TRUE)
# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM EGGS')

#------------------------------------------------------------------------------------------------------------------------
# 0. Prepare data for analysis
#------------------------------------------------------------------------------------------------------------------------

d[, dup := duplicated(d, by = 'ID')]
ds = d[dup == TRUE]
ds

# Rick's nests
d = d[nest %like% 'REPH']
d[, ID2 := paste0(nest, '_', egg_id, '_', substr(year_, 3,4 ))]

# all with wrong name
d = d[ID != ID2]

# save data to DB
dt = d[, .(pk, ID2)]

# save new values from d in a temp table
dbWriteTable(con, 'temp', dt , row.names = FALSE)

# update target table based on values in temp table
dbExecute(con, "update EGGS e, temp t set e.ID = t.ID2 where e.pk = t.pk")
dbExecute(con,"drop table temp")

dbDisconnect(con)



