#========================================================================================================================
# Meta analysis - Paternity in polyandrous species
#========================================================================================================================
# Packages
sapply( c('data.table', 'magrittr', 'ggplot2', 'auksRuak', 'viridis'),
        require, character.only = TRUE)

# Data
d = read.delim('./DATA/EPP_Shorebirds.csv', sep = ';', dec = '.', header = TRUE) %>% data.table

# polyandry between species
ds = d[type == 'total']

ds = ds[, .(social_mating_system = simple_social_maiting_system[c(1)], N_nests = sum(n_broods), nests_EPY = sum(n_epbroods)), by = scinam]
ds[, percent_nests_EPY := nests_EPY / N_nests * 100]

dss = ds[, .(N_nests = sum(N_nests), nests_EPY = sum(nests_EPY)), by = social_mating_system]
dss[, percent_nests_EPY := nests_EPY / N_nests * 100]
dss[, social_mating_system := factor(social_mating_system, levels = c('monogamy', 'sequential polyandry', 'simultaneous polyandry', 'polygyny'))]
ds[, social_mating_system := factor(social_mating_system, levels = c('monogamy', 'sequential polyandry', 'simultaneous polyandry', 'polygyny'))]

dss[, EPY_lwr    := qbeta(p = c(0.025), 1+nests_EPY, 1+N_nests) * 100]
dss[, EPY_median := qbeta(p = c(0.500), 1+nests_EPY, 1+N_nests) * 100]
dss[, EPY_upr    := qbeta(p = c(0.975), 1+nests_EPY, 1+N_nests) * 100]

ggplot() +
  geom_point(data = dss, aes(x = social_mating_system, y = percent_nests_EPY, size = N_nests)) +
  geom_errorbar(data = dss, aes(x = social_mating_system, ymin = EPY_lwr, ymax = EPY_upr, group = social_mating_system), 
                width = .01, position = position_dodge(width = 0.5)) +
  geom_point(data = ds, aes(x = social_mating_system, y = percent_nests_EPY, size = N_nests, color = scinam)) +
  theme_classic(base_size = 16) + labs(y = 'Percent nests with EPY', x = 'Species') 

setorder(ds, -percent_nests_EPY)
ds


require(ggpol)
ggplot() +
  # geom_point(data = dss, aes(x = social_mating_system, y = percent_nests_EPY, size = N_nests)) +
  geom_point(data = ds, aes(x = social_mating_system, y = percent_nests_EPY, size = N_nests, color = scinam), 
             position = position_dodge(width = 0.3)) +
  geom_boxjitter(data = ds, aes(x = social_mating_system, y = percent_nests_EPY, group = social_mating_system), jitter.size = 2,
                 outlier.color = NA, jitter.shape = 21, jitter.color = NA, errorbar.draw = FALSE) +
  theme_classic(base_size = 16) + labs(y = 'Percent nests with EPY', x = 'Species') 




# Data
d = read.delim('./DATA/EPP_Shorebirds.csv', sep = ';', dec = '.', header = TRUE) %>% data.table

# polyandry between species
ds = d[type == 'total']

ds[, scinam := tolower(scinam)]

ds = ds[, .(social_mating_system = simple_social_maiting_system[c(1)], N_nests = sum(n_broods), nests_EPY = sum(n_epbroods)), by = scinam]
ds[, percent_nests_EPY := nests_EPY / N_nests * 100]
ds[social_mating_system == 'sequential polyandry' | social_mating_system == 'simultaneous polyandry', social_mating_system := 'polyandry']


dss = ds[, .(N_nests = sum(N_nests), nests_EPY = sum(nests_EPY)), by = social_mating_system]
dss[, percent_nests_EPY := nests_EPY / N_nests * 100]
dss[, social_mating_system := factor(social_mating_system, levels = c('monogamy', 'polyandry', 'polygyny'))]
ds[, social_mating_system := factor(social_mating_system, levels = c('monogamy', 'polyandry', 'polygyny'))]

dss[, EPY_lwr    := qbeta(p = c(0.025), 1+nests_EPY, 1+N_nests) * 100]
dss[, EPY_median := qbeta(p = c(0.500), 1+nests_EPY, 1+N_nests) * 100]
dss[, EPY_upr    := qbeta(p = c(0.975), 1+nests_EPY, 1+N_nests) * 100]

p = 
ggplot() +
  geom_point(data = ds, aes(x = social_mating_system, y = percent_nests_EPY, size = N_nests, color = scinam)) +
  geom_point(data = dss, aes(x = social_mating_system, y = percent_nests_EPY), shape = 3, size = 8) +
  theme_classic(base_size = 24) + labs(y = 'Percent nests with EPY', x = 'Mating system') 
p

# png(paste0('./REPORTS/FIGURES/EPY_frequency_shorebirds.png'), width = 1200, height = 800)
# p
# dev.off()



setorder(ds, -percent_nests_EPY)
ds


require(ggpol)
ggplot() +
  # geom_point(data = dss, aes(x = social_mating_system, y = percent_nests_EPY, size = N_nests)) +
  geom_point(data = ds, aes(x = social_mating_system, y = percent_nests_EPY, size = N_nests, color = scinam), 
             position = position_dodge(width = 0.3)) +
  geom_boxjitter(data = ds, aes(x = social_mating_system, y = percent_nests_EPY, group = social_mating_system), jitter.size = 2,
                 outlier.color = NA, jitter.shape = 21, jitter.color = NA, errorbar.draw = FALSE) +
  theme_classic(base_size = 16) + labs(y = 'Percent nests with EPY', x = 'Species') 



