#=========================================================================================================================
# REPHatBARROW NESTS add column for assigned in field 
#=========================================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'DBI'),
        require, character.only = TRUE)

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dc = dbq(con, 'select * FROM CAPTURES')
d17 = dbq(con, "select * FROM FIELD_2017_REPHatBARROW.NESTS where nest like 'R%' ")
d18 = dbq(con, "select * FROM FIELD_2018_REPHatBARROW.NESTS where nest like 'R%' ")
d19 = dbq(con, "select * FROM FIELD_2019_REPHatBARROW.NESTS where nest like 'R%' ")
DBI::dbDisconnect(con)

#------------------------------------------------------------------------------------------------------------------------
# Parents assigned in field? 
#------------------------------------------------------------------------------------------------------------------------

# rbind field data 
d17 = d17[, .(year_ = 2017, nest, male_id, m_UL = NA, m_UR = NA, m_LL, m_LR, female_id, f_UL = NA, f_UR = NA, f_LL, f_LR)]
d18 = d18[, .(year_ = 2018, nest, male_id, m_UL, m_UR, m_LL, m_LR, female_id, f_UL, f_UR, f_LL, f_LR)]
d19 = d19[, .(year_ = 2019, nest, male_id, m_UL, m_UR, m_LL, m_LR, female_id, f_UL, f_UR, f_LL, f_LR)]

d = rbindlist(list(d17, d18, d19))

# nestID
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dn[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# ID
d[male_id %in% c(70001:71000), male_id := as.integer(paste0(2701, male_id))]
d[male_id %in% c(45001:46000), male_id := as.integer(paste0(2731, male_id))]
d[female_id %in% c(70001:71000), female_id := as.integer(paste0(2701, female_id))]
d[female_id %in% c(45001:46000), female_id := as.integer(paste0(2731, female_id))]

# confirmed male or female id?
d[, male_id_min := as.integer(min(male_id, na.rm = TRUE)), by = nestID]
d[, male_id_max := as.integer(max(male_id, na.rm = TRUE)), by = nestID]
identical(d$male_id_min, d$male_id_max) # check for unique ID

d[, male_id := as.integer(min(male_id, na.rm = TRUE)), by = nestID]

d[, female_id_min := as.integer(min(female_id, na.rm = TRUE)), by = nestID]
d[, female_id_max := as.integer(max(female_id, na.rm = TRUE)), by = nestID]
identical(d$female_id_min, d$female_id_max) # check for unique ID

d[, female_id := as.integer(min(female_id, na.rm = TRUE)), by = nestID]

# merge with ID from color combo
dc[, COMBO := paste0(UL, '-', UR, '-', LL, '-', LR)]
dc = dc[!is.na(COMBO)]
dc = unique(dc, by = 'COMBO')

# COMBO field data
# 2017
d[year_ == 2017 & !is.na(m_LL), m_UL := 'M']
d[year_ == 2017 & !is.na(m_LL), m_UR := 'Y']
d[year_ == 2017 & !is.na(f_LL), f_UL := 'M']
d[year_ == 2017 & !is.na(f_LL), f_UR := 'Y']

d[!is.na(m_LL), m_COMBO := paste0(m_UL, '-', m_UR, '-', m_LL, '-', m_LR)]
d[!is.na(f_LL), f_COMBO := paste0(f_UL, '-', f_UR, '-', f_LL, '-', f_LR)]

d = merge(d, dc[, .(maleID = ID, COMBO)], by.x = 'm_COMBO', by.y = 'COMBO', all.x = TRUE)
d = merge(d, dc[, .(femaleID = ID, COMBO)], by.x = 'f_COMBO', by.y = 'COMBO', all.x = TRUE)

# confirmed male or female id?
d[, maleID_min := as.integer(min(maleID, na.rm = TRUE)), by = nestID]
d[, maleID_max := as.integer(max(maleID, na.rm = TRUE)), by = nestID]
identical(d$maleID_min, d$maleID_max) # check for unique ID

# d[, m_identical := identical(maleID_min, maleID_max), by = 1:nrow(d)]
# d[!is.na(male_id_min) & m_identical == FALSE]

d[, maleID := as.integer(min(maleID, na.rm = TRUE)), by = nestID]

d[, femaleID_min := as.integer(min(femaleID, na.rm = TRUE)), by = nestID]
d[, femaleID_max := as.integer(max(femaleID, na.rm = TRUE)), by = nestID]
identical(d$femaleID_min, d$femaleID_max) # check for unique ID

d[, femaleID := as.integer(min(femaleID, na.rm = TRUE)), by = nestID]

# fill not confirmed ID's 
d[is.na(male_id) & !is.na(maleID), m_not_confirmed := TRUE]
d[is.na(male_id) & !is.na(maleID), male_id := maleID]

d[is.na(female_id) & !is.na(femaleID), f_not_confirmed := TRUE]
d[is.na(female_id) & !is.na(femaleID), female_id := femaleID]

# unique
d = unique(d, by = 'nestID')

# not confirmed?
d[m_not_confirmed == TRUE]
d[f_not_confirmed == TRUE]

# merge with nests 
dn = merge(dn[external == 0], d[, .(nestID, male_id_seen = male_id, female_id_seen = female_id, 
                                    m_COMBO, f_COMBO)], by = 'nestID', all.x = TRUE)

dn[, m_identical := identical(male_id, male_id_seen), by = 1:nrow(dn)]
dn[!is.na(male_id) & m_identical == FALSE, .(nestID, male_id, male_id_seen, m_COMBO)]
# checked: all mistakes that were corrected later

dn[male_id > 111111111, male_field := 1]
dn[is.na(male_field), male_field := 0]

dn[, f_identical := identical(female_id, female_id_seen), by = 1:nrow(dn)]
dn[is.na(female_id_seen), f_identical := FALSE]
dn[!is.na(female_id_seen) & f_identical == FALSE, .(nestID, female_id, female_id_seen, f_COMBO)]

dn[f_identical == TRUE, female_field := 1]
dn[is.na(female_field), female_field := 0]

# update db
dm = dn[, .(male_field, female_field, pk)]

con = dbcon('jkrietsch', db = 'REPHatBARROW')  

# save new values from d in a temp table
dbWriteTable(con, 'temp', dm , row.names = FALSE)

# update target table based on values in temp table
dbExecute(con, "update NESTS n, temp t set n.male_field = t.male_field where n.pk = t.pk")
dbExecute(con, "update NESTS n, temp t set n.female_field = t.female_field where n.pk = t.pk")
dbExecute(con,"drop table temp")

dbDisconnect(con)

#------------------------------------------------------------------------------------------------------------------------
# Parents assigned in field? Rick's data
#------------------------------------------------------------------------------------------------------------------------

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')

dn = dn[external != 0]

dn[!is.na(male_id), male_field := 1]
dn[is.na(male_field), male_field := 0]

dn[!is.na(female_id), female_field := 1]
dn[is.na(female_field), female_field := 0]

# update db
dm = dn[, .(male_field, female_field, pk)]

# save new values from d in a temp table
dbWriteTable(con, 'temp', dm , row.names = FALSE)

# update target table based on values in temp table
dbExecute(con, "update NESTS n, temp t set n.male_field = t.male_field where n.pk = t.pk")
dbExecute(con, "update NESTS n, temp t set n.female_field = t.female_field where n.pk = t.pk")
dbExecute(con,"drop table temp")
