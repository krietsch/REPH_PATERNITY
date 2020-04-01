#========================================================================================================================
# Meta analysis - Paternity in polyandrous species
#========================================================================================================================
# Packages
sapply( c('data.table', 'magrittr', 'ggplot2', 'auksRuak', 'viridis'),
        require, character.only = TRUE)

# Data
d = read.table('./DATA/Meta_analysis_EPP.csv', sep = ';', dec = '.', header = TRUE) %>% data.table

#------------------------------------------------------------------------------------------------------------------------
# 0. Prepare data for analysis
#------------------------------------------------------------------------------------------------------------------------

# nests
d[, EPY_lwr    := qbeta(p = c(0.025), 1+nests_EPY, 1+N_nests) * 100]
d[, EPY_median := qbeta(p = c(0.500), 1+nests_EPY, 1+N_nests) * 100]
d[, EPY_upr    := qbeta(p = c(0.975), 1+nests_EPY, 1+N_nests) * 100]

# eggs
d[, EPY_eggs_lwr    := qbeta(p = c(0.025), 1+eggs_EPY, 1+N_eggs) * 100]
d[, EPY_eggs_median := qbeta(p = c(0.500), 1+eggs_EPY, 1+N_eggs) * 100]
d[, EPY_eggs_upr    := qbeta(p = c(0.975), 1+eggs_EPY, 1+N_eggs) * 100]

# calculate confidence intervalls for polyandry
d[, pa_lwr    := qbeta(p = c(0.025), 1+N_polyandrous_females, 1+N_females)]
d[, pa_median := qbeta(p = c(0.500), 1+N_polyandrous_females, 1+N_females)]
d[, pa_upr    := qbeta(p = c(0.975), 1+N_polyandrous_females, 1+N_females)]

# factor order
d[, species_short := factor(species_short, levels = c('OYCA', 'PUSA', 'COSA', 'RIPL', 'SEPL', 'WESA', 'KEPL', 'WIPH', 
                                                      'RNPH', 'REPH', 'EUDO', 'SPSA', 'CCJA', 'WAJA', 'BLCO', 'RUFF', 'BBSA', 'GRSN'))]
d[, type := factor(type, levels = c('mean', 'single_year_only', 'single_year', 'monogamous', 'renesting', 'polyandrous'))]

# in percent
d[, percent_nests_EPY := as.numeric(percent_nests_EPY) * 100]
d[, percent_eggs_EPY := as.numeric(percent_eggs_EPY) * 100]

#------------------------------------------------------------------------------------------------------------------------
# 1. Plots per nest
#------------------------------------------------------------------------------------------------------------------------

# polyandry between species
ds = d[type == 'mean' | type == 'single_year_only']

ggplot(data = ds) +
  geom_point(aes(x = species_short, y = EPY_median, group = source, color = social_mating_system), 
             shape = 95, size = 10, position = position_dodge(width = 0.5)) +
  geom_point(aes(x = species_short, y = percent_nests_EPY, group = source, size = N_nests, color = social_mating_system), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(x = species_short, ymin = EPY_lwr, ymax = EPY_upr, group = source, color = social_mating_system), 
                width = .01, position = position_dodge(width = 0.5)) +
  scale_size_continuous(guides(name = 'N nests'), breaks = c(10, 25, 50, 75, 100)) +
  theme_classic(base_size = 16) + labs(y = 'Percent nests with EPY', x = 'Species') 


dss = ds[, .(N_nests = sum(N_nests), nests_EPY = sum(nests_EPY)), by = social_mating_system]
dss[, percent_nests_EPY := nests_EPY / N_nests * 100]
dss[, social_mating_system := factor(social_mating_system, levels = c('monogamy', 'sequential polyandry', 'simultaneous polyandry', 'Lekking'))]
ds[, social_mating_system := factor(social_mating_system, levels = c('monogamy', 'sequential polyandry', 'simultaneous polyandry', 'Lekking'))]

dss[, EPY_lwr    := qbeta(p = c(0.025), 1+nests_EPY, 1+N_nests) * 100]
dss[, EPY_median := qbeta(p = c(0.500), 1+nests_EPY, 1+N_nests) * 100]
dss[, EPY_upr    := qbeta(p = c(0.975), 1+nests_EPY, 1+N_nests) * 100]

ggplot() +
  geom_point(data = dss, aes(x = social_mating_system, y = percent_nests_EPY, size = N_nests)) +
  geom_errorbar(data = dss, aes(x = social_mating_system, ymin = EPY_lwr, ymax = EPY_upr, group = social_mating_system), 
                width = .01, position = position_dodge(width = 0.5)) +
  geom_point(data = ds, aes(x = social_mating_system, y = percent_nests_EPY, size = N_nests, color = species)) +
  theme_classic(base_size = 16) + labs(y = 'Percent nests with EPY', x = 'Species') 


require(ggpol)
ggplot() +
  # geom_point(data = dss, aes(x = social_mating_system, y = percent_nests_EPY, size = N_nests)) +
  # geom_point(data = ds, aes(x = social_mating_system, y = percent_nests_EPY, size = N_nests, color = species)) +
  geom_boxjitter(data = ds, aes(x = social_mating_system, y = percent_nests_EPY, fill  = social_mating_system), jitter.size = 2,
                 outlier.color = NA, jitter.shape = 21, jitter.color = NA, jitter.height = 0.05, jitter.width = 0.075, errorbar.draw = FALSE) +
  
  theme_classic(base_size = 16) + labs(y = 'Percent nests with EPY', x = 'Species') 

ggplot(data = ds) +
  geom_point(aes(x = species_short, y = percent_nests_EPY, size = N_nests, group = source), position = position_dodge(width = 0.5))  +
  theme_classic(base_size = 16) + labs(y = 'Percent nests with EPY', x = 'Percent polyandrous females') 

# polyandry within season
ds = d[!is.na(percent_poly_fem) & type == 'mean']
ds2 = d[!is.na(percent_poly_fem) & type == 'single_year']

ggplot() +
  geom_point(data = ds, aes(x = percent_poly_fem, y = percent_EPY_mean, color = species_short), shape = 95, size = 10) +
  geom_errorbar(data = ds, aes(x = percent_poly_fem, ymin = EPY_lwr, ymax = EPY_upr, color = species_short), width = 0) +
  geom_point(data = ds2, aes(x = percent_poly_fem, y = percent_nests_EPY, size = N_nests), shape = 95, size = 10, color = 'firebrick4') +
  geom_errorbar(data = ds2, aes(x = percent_poly_fem, ymin = EPY_lwr, ymax = EPY_upr), width = 0, color = 'firebrick4') +
  theme_classic(base_size = 16) + labs(y = 'Percent nests with EPY', x = 'Percent polyandrous females') 

# polyandry between monogamous clutch and polyandrous clutch
ds = d[type == 'monogamous' | type == 'polyandrous'| type == 'renesting']

ggplot(data = ds) +
  geom_point(aes(x = type, y = EPY_median, group = species_short, color = species_short), position = position_dodge(width = 0.5)) +
  geom_point(aes(x = type, y = percent_nests_EPY, group = species_short), position = position_dodge(width = 0.5), color = 'grey') +
  geom_errorbar(aes(x = type, ymin = EPY_lwr, ymax = EPY_upr, group = species_short, color = species_short), 
                width = .01, position = position_dodge(width = 0.5)) +
  theme_classic(base_size = 16) + labs(y = 'Percent nests with EPY', x = 'Clutch type') 


#------------------------------------------------------------------------------------------------------------------------
# 1. Plots per eggs
#------------------------------------------------------------------------------------------------------------------------

# polyandry between species
ds = d[type == 'mean' | type == 'single_year_only']

ggplot(data = ds) +
  geom_point(aes(x = species_short, y = EPY_eggs_median, group = source), shape = 95, size = 10, position = position_dodge(width = 0.5)) +
  geom_point(aes(x = species_short, y = percent_eggs_EPY, group = source, size = N_eggs), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(x = species_short, ymin = EPY_eggs_lwr, ymax = EPY_eggs_upr, group = source), width = .01, position = position_dodge(width = 0.5)) +
  scale_size_continuous(guides(name = 'N eggs'), breaks = c(100, 200, 300, 400, 500)) +
  theme_classic(base_size = 16) + labs(y = 'Percent eggs with EPY', x = 'Species') 

ggplot(data = ds) +
  geom_point(aes(x = species_short, y = percent_eggs_EPY, size = N_nests, group = source), position = position_dodge(width = 0.5))  +
  theme_classic(base_size = 16) + labs(y = 'Percent nests with EPY', x = 'Percent polyandrous females') 


# polyandry between monogamous clutch and polyandrous clutch
ds = d[type == 'monogamous' | type == 'polyandrous'| type == 'renesting']

ggplot(data = ds) +
  geom_point(aes(x = type, y = EPY_eggs_median, group = species_short, color = species_short), shape = 95, size = 10, 
             position = position_dodge(width = 0.5)) +
  geom_point(aes(x = type, y = percent_eggs_EPY, group = species_short, size = N_eggs, color = species_short), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(x = type, ymin = EPY_eggs_lwr, ymax = EPY_eggs_upr, group = species_short, color = species_short), 
                width = .01, position = position_dodge(width = 0.5)) +
  scale_size_continuous(guides(name = 'N eggs')) +
  theme_classic(base_size = 16) + labs(y = 'Percent nests with EPY', x = 'Clutch type') 









