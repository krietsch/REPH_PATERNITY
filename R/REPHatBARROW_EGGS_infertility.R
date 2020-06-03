#=========================================================================================================================
# REPHatBARROW EGGS infertility
#=========================================================================================================================

# settings, con
options(stringsAsFactors = FALSE)
sapply( c('data.table', 'sdb','readxl','foreach', 'wadeR', 'ggplot2'),
        require, character.only = TRUE)

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
f = dbq(con, 'select * FROM EGGS WHERE FALSE') %>% names

d = dbq(con, 'select * FROM EGGS')

du = read_xlsx("/ds/raw_data_kemp/LAB/Alaska Barrow/Alaska 2019/REPH/Unhatched_chicks.xlsx", 
              sheet = 'Sheet1') %>% data.table

#-------------------------------------------------------------------------------------------------------------------------
# add column for undeveloped eggs
#-------------------------------------------------------------------------------------------------------------------------

du[, ID := paste0(ID, '_', substr(year, 3,4 ))]
du[, fate_new := 'u']

d = merge(d, du[, .(ID, fate_new, development)], by.x = 'sample_name', by.y = 'ID', all.x = TRUE)

# check (should be 0 rows)
d[!is.na(fate_new) & fate != fate_new]

# undeveloped
d[development == 0, undeveloped := 1]
d[is.na(undeveloped), undeveloped := 0]

d[, .N, undeveloped]


# save data to DB
dt = d[, .(pk, undeveloped)]

# save new values from d in a temp table
dbWriteTable(con, 'temp', dt , row.names = FALSE)

# update target table based on values in temp table
dbExecute(con, "update EGGS e, temp t set e.undeveloped = t.undeveloped where e.pk = t.pk")
dbExecute(con,"drop table temp")


