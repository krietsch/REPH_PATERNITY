#========================================================================================================================
# Compare reproductive skew in REPH and PESA
#========================================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'sf', 'auksRuak', 'ggplot2'),
        function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE)))

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dR = dbq(con, 'select * FROM PATERNITY')
DBI::dbDisconnect(con)

con = dbcon('jkrietsch', db = 'PESAatBARROW')  
dP = dbq(con, 'select * FROM PATERNITY')
DBI::dbDisconnect(con)

# merge data
dR[, species := 'REPH']
dP[, species := 'PESA']

d = rbind(dR[, .(year_, nest, IDchick, IDmother, IDfather, species)], 
          dP[, .(year_, nest, IDchick, IDmother, IDfather, species)])

# ID by year
d[, IDmother_year := paste0(IDmother, '_', substr(year_, 3,4 ))]
d[, IDfather_year := paste0(IDfather, '_', substr(year_, 3,4 ))]

# offspring sired by females
df = d[!is.na(IDmother), .(N_offspring = .N, sex = 'F'), by = .(IDmother_year, species)]
dm = d[!is.na(IDfather), .(N_offspring = .N, sex = 'M'), by = .(IDfather_year, species)]

ggplot(data = dm) +
  geom_density(aes(N_offspring, color = species))

ggplot(data = df) +
  geom_density(aes(N_offspring, color = species))

df[species == 'REPH' & N_offspring > 4] %>% nrow / df[species == 'REPH'] %>% nrow * 100

dm[species == 'PESA' & N_offspring > 4] %>% nrow / df[species == 'PESA'] %>% nrow * 100

df[species == 'REPH' & N_offspring > 7] %>% nrow / df[species == 'REPH'] %>% nrow * 100
df[species == 'PESA' & N_offspring > 7] %>% nrow / df[species == 'PESA'] %>% nrow * 100

dm[species == 'PESA' & N_offspring > 7] %>% nrow / df[species == 'PESA'] %>% nrow * 100




ds = unique(d, by = c('IDmother_year', 'IDfather_year'))

dss = ds[, .N, by = .(IDmother_year, species)]
dss = ds[, .N, by = .(IDfather_year, species)]

ggplot(data = dss) +
  geom_density(aes(N, color = species))











