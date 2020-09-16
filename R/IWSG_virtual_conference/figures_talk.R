#========================================================================================================================
# Figures for talk
#========================================================================================================================

# Summary
# 0. Prepare data for analysis
# 1. EPP between species
# 2. EPP & Polyandry
# 3. Timing of second and third clutches

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak', 'patchwork', 'multcomp'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')
dc = dbq(con, 'select * FROM CAPTURES')
dp = dbq(con, 'select * FROM PATERNITY')
DBI::dbDisconnect(con)

#------------------------------------------------------------------------------------------------------------------------
# 0. Prepare data for analysis
#------------------------------------------------------------------------------------------------------------------------

# Change projection
ds = d[is.na(lon)] # seperate data without position
d = d[!is.na(lon)]
st_transform_DT(d)

# Assign locations in the study area 
point_over_poly_DT(d, poly = study_site, buffer = 10)
setnames(d, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := FALSE]
d[male_id == 175189876, study_site := FALSE] # exclude one male from 2006
d = rbind(d, ds)

# check data
# bm = create_bm(d[study_site == TRUE], buffer = 500)
# bm +
#   geom_point(data = d, aes(lon, lat, color = study_site))

# nestID
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dp[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# assign datetime format
d[, found_datetime := as.POSIXct(found_datetime)]
d[, collected_datetime := as.POSIXct(collected_datetime)]
d[, initiation := as.POSIXct(initiation)]
d[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, initiation_doy := yday(initiation)]
d[, est_hatching_datetime := as.POSIXct(est_hatching_datetime)]
d[, hatching_datetime := as.POSIXct(hatching_datetime)]
d[, nest_state_date := as.POSIXct(nest_state_date)]
setorder(d, year_, initiation)
d[, YEAR_ := factor(year_)]

# nests with paternity data
dp = dp[!is.na(EPY)]
# EPY father 
dp[EPY == 1, .N, by = nestID] # more than 1 EPY father? No
dp[EPY == 1, EPY_father := IDfather]
dps = dp[, .(EPY = sum(EPY), N = .N, EPY_father = min(EPY_father, na.rm = TRUE)), by = .(nestID, year_)]
dps[, anyEPY := ifelse(EPY > 0, 1, 0)]
dps[is.infinite(EPY_father), EPY_father := NA]
dps[!is.na(EPY_father)] %>% nrow # known EPY fathers

# merge with nests
d = merge(d, dps[, .(nestID, N_parentage = N, N_EPY = EPY, anyEPY, EPY_father)], by = 'nestID', all.x = TRUE)

# assign all nests with parentage data
d[, parentage := ifelse(!is.na(N_parentage), TRUE, FALSE)]
d[, any_parentage_year := any(parentage == TRUE), by = year_]

#------------------------------------------------------------------------------------------------------------------------
# 1. EPP between species
#------------------------------------------------------------------------------------------------------------------------

dss = data.table(data = c('SPSA', 'EUDO', 'WIPH', 'RNPH', 'REPH_Dale', 'REPH_ext', 'REPH_study'), 
                 N_nests = c(34, 22, 17, 63, 18, 169, 165),
                 N_EPY = c(7, 2, 0, 4, 6, 21, 16))

dss[, EPY_nests_per := round(N_EPY / N_nests * 100, 0)]
dss[, data := factor(data, levels = c('SPSA', 'EUDO', 'WIPH', 'RNPH', 'REPH_Dale', 'REPH_ext', 'REPH_study'))]
dss[, sample_size := paste0(N_EPY, '/', N_nests)]

p = 
  ggplot(data = dss) +
  geom_bar(aes(data, EPY_nests_per), stat = 'identity', position = 'dodge', width = 0.5, fill = 'grey50') +
  xlab('') + ylab('Nests with EPP (%)') +
  geom_text(data = dss, aes(data, EPY_nests_per, label = sample_size), vjust = -0.5, size = 6) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(0, 35), expand = c(0, 0)) +
  theme_classic(base_size = 20)
p

# png(paste0('./REPORTS/FIGURES/EPY_poly_species.png'), width = 700, height = 400)
# p
# dev.off()

#------------------------------------------------------------------------------------------------------------------------
# 2. EPP & Polyandry
#------------------------------------------------------------------------------------------------------------------------

# only study site
dss = data.table(YEAR_ = c('2017', '2018', '2019'), 
                 type  = c(rep('EPP', 3), rep('Polyandry', 3)), 
                 N_nests = c(34, 35, 96, 34, 37, 91),
                 N_EPY_poly = c(1, 2, 13, 1 , 2, 6))

dss[, EPY_nests_per := round(N_EPY_poly / N_nests * 100, 2)]
dss[, sample_size := paste0(N_EPY_poly, '/', N_nests)]

p = 
  ggplot(data = dss, aes(YEAR_, EPY_nests_per, fill = type, label = sample_size)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.7) +
  geom_text(position = position_dodge(width = 0.7), size = 6, vjust = -0.5) +
  scale_fill_manual(values = c('grey50','firebrick4')) +
  xlab('Year') + ylab('Nests with EPP / polyandry (%)') + 
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.15, 0.9), legend.title = element_blank())
p

# png(paste0('./REPORTS/FIGURES/EPY_polyandry_study_site.png'), width = 500, height = 400)
# p
# dev.off()

# including polyandrous females with second clutch outside study site
dss = data.table(YEAR_ = c('2017', '2018', '2019'), 
                 type  = c(rep('EPP', 3), rep('Polyandry', 3)), 
                 N_nests = c(34, 35, 96, 34, 37, 91),
                 N_EPY_poly = c(1, 2, 13, 1 , 2, 8))

dss[, EPY_nests_per := round(N_EPY_poly / N_nests * 100, 2)]
dss[, sample_size := paste0(N_EPY_poly, '/', N_nests)]

p = 
  ggplot(data = dss, aes(YEAR_, EPY_nests_per, fill = type, label = sample_size)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.7) +
  geom_text(position = position_dodge(width = 0.7), size = 6, vjust = -0.5) +
  scale_fill_manual(values = c('grey50','firebrick4')) +
  xlab('Year') + ylab('Nests with EPP / polyandry (%)') + 
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.15, 0.9), legend.title = element_blank())
p

# png(paste0('./REPORTS/FIGURES/EPY_polyandry_study_site_and_ext.png'), width = 500, height = 400)
# p
# dev.off()


#------------------------------------------------------------------------------------------------------------------------
# 3. Timing of second and third clutches
#------------------------------------------------------------------------------------------------------------------------

setorder(d, year_, initiation)

# first and second clutches by females
d[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]
d[, N_female_clutch := .N, by = female_id]
d[, female_clutch := seq_len(.N), by = .(female_id, year_)]
d[is.na(female_id), female_clutch := 1]
d[!is.na(female_id), N_female_clutch := max(female_clutch), by = .(female_id, year_)]
d[is.na(female_id), N_female_clutch := 1]

# polyandrous clutches (second clutch with different partner)
ID2c = d[female_clutch == 2]$female_id_year
dx = d[female_id_year %in% ID2c]

dr = merge(dx[female_clutch == 1, .(year1 = year_, nestID1 = nestID, female_id_year, m1 = male_id, anyEPY1 = anyEPY, 
                                    ss1 = study_site, initiation1 = initiation)], 
           dx[female_clutch == 2, .(year2 = year_, nestID2 = nestID, female_id_year, m2 = male_id, anyEPY2 = anyEPY, 
                                    ss2 = study_site, initiation2 = initiation)],  
           by = 'female_id_year', all = TRUE)

dr[, same_male := m1 == m2]
dr[is.na(same_male), same_male := FALSE]
dr[, both_study_site := ss1 == ss2]
dr[, diff_initiation := difftime(initiation2, initiation1, units = 'days') %>% as.numeric]
setorder(dr, female_id_year)
dr

# polyandrous females
dr = dr[same_male == FALSE, .(female_id_year, polyandrous = TRUE, polyandry_study_site = both_study_site)]
d = merge(d, dr, by = 'female_id_year', all.x = TRUE)

setorder(d, year_, initiation)

# males renesting
d[, male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
d[, N_male_clutch := .N, by = male_id]
d[, male_clutch := seq_len(.N), by = male_id_year]
d[is.na(male_id), male_clutch := 1]
d[!is.na(male_id), N_male_clutch := max(male_clutch), by = male_id_year]
d[is.na(male_id), N_male_clutch := 1]
d[, .N, by = .(year_, male_clutch)]
d[, .N, by = .(male_clutch, external)]

# males renesting with same or different partner
ID2c = d[male_clutch == 2]$male_id_year
dx = d[male_id_year %in% ID2c]

dr = merge(dx[male_clutch == 1, .(year1 = year_, nestID1 = nestID, male_id_year, f1 = female_id, anyEPY1 = anyEPY, 
                                  mfc1 = male_clutch, ss1 = study_site, initiation1 = initiation)], 
           dx[male_clutch == 2, .(year2 = year_, nestID2 = nestID, male_id_year, f2 = female_id, anyEPY2 = anyEPY, 
                                  fc2 = male_clutch, ss2 = study_site, initiation2= initiation)],  
           by = 'male_id_year', all = TRUE)

dr[, same_female := f1 == f2]
dr[is.na(same_female), same_female := FALSE]
dr[, both_study_site := ss1 == ss2]
dr[, diff_initiation := difftime(initiation2, initiation1, units = 'days') %>% as.numeric]
setorder(dr, male_id_year)
dr

# renesting males
dr = dr[, .(male_id_year, renesting_male = TRUE, same_female, renesting_study_site = both_study_site)]
d = merge(d, dr, by = 'male_id_year', all.x = TRUE)

### unique ID's
d[is.na(male_id), male_id_year := NA]
d[, male_id_year_NA := male_id_year]
d[is.na(male_id), male_id_year_NA := paste0(seq_len(.N), '_', substr(year_, 3,4)), by = male_id_year_NA]

d[is.na(female_id), female_id_year := NA]
d[, female_id_year_NA := female_id_year]
d[is.na(female_id), female_id_year_NA := paste0(seq_len(.N), '_', substr(year_, 3,4)), by = female_id_year_NA]

# assign clutch identity
d[female_clutch == 3, clutch_identity := 'third']
d[female_clutch == 2, clutch_identity := 'second']
d[female_clutch == 1 & N_female_clutch > 1, clutch_identity := 'first']
d[female_clutch == 1 & N_female_clutch < 2, clutch_identity := 'one']
d[clutch_identity == 'one' & anyEPY == FALSE, clutch_identity := 'one_noEPY']
d[clutch_identity == 'one' & anyEPY == TRUE, clutch_identity := 'one_EPY']
d[is.na(clutch_identity)]
d[, .N, by = clutch_identity]

# factor order
d[, clutch_identity := factor(clutch_identity, levels = c('one_noEPY', 'one_EPY', 'first', 'second', 'third'))]

#------------------------------------------------------------------------------------------------------------------------

# all 
ds = copy(d)
ds = ds[!is.na(initiation)]
ds = ds[!is.na(anyEPY)]
ds[, anyEPY := as.character(anyEPY)]

ds[, mean_initiation := mean(initiation, na.rm = TRUE), by = year_]
ds[, initiation_st := difftime(initiation, mean_initiation, units = 'days') %>% as.numeric]

ds[, .N, by = anyEPY]
dss = data.table(anyEPY = c('0', '1'),
                 sample_size = c('295', '37'))

p1 = 
  ggplot(data = ds) +
  geom_hline(yintercept = 0, color = 'grey70') +
  geom_boxplot(aes(anyEPY, initiation_st), fill = 'grey85', outlier.alpha = 0) +
  geom_jitter(aes(anyEPY, initiation_st, fill = anyEPY), width = 0.3, height = 0, shape = 21, size = 2, show.legend = FALSE) +
  scale_fill_manual(values = c('white', 'black')) +
  scale_x_discrete(labels = c('without EPP', 'with EPP')) +
  scale_y_continuous(breaks=seq(-15, 15, 5), limits = c(-14.4, 14.4)) + 
  xlab('clutches') + ylab('') +
  geom_text(data = dss, aes(anyEPY, Inf, label = sample_size), vjust = 1, size = 6) +
  theme_classic(base_size = 20)
p1


ds = ds[clutch_identity %in% c('first', 'second', 'third')]
ds[is.na(renesting_male), renesting_male := FALSE]

ds[, any_renesting := any(renesting_male == TRUE), by = female_id_year_NA]
ds[any_renesting == FALSE, next_clutch := 'polyandrous']
ds[any_renesting == TRUE, next_clutch := 'renesting']

# add first and second clutch of females with three clutches again (otherwise linetype does not work)
ds[clutch_identity == 'third']
dss = ds[clutch_identity %in% c('first', 'second') & female_id %in% c(270170935, 19222)]
dss[, female_id_year_NA := paste0(female_id_year_NA, '2')]

ds = rbind(ds, dss)


theme_classic_edit = function (base_size = 11, base_family = "", base_line_size = base_size/22, 
                               base_rect_size = base_size/22, lp = c(0.8, 0.2)) 
{
  theme_bw(base_size = base_size, base_family = base_family, 
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = rel(1)), legend.key = element_blank(), 
          strip.background = element_rect(fill = "white", colour = "black", size = rel(2)), complete = TRUE,
          legend.position = lp)
}

dss = data.table(clutch_identity = c('first', 'second', 'third'),
                 sample_size = c('0/15', '3/15', '0/2'))



p2 = 
  ggplot(data = ds) +
  geom_hline(yintercept = 0, color = 'grey70') +
  geom_boxplot(aes(clutch_identity, initiation_st), fill = 'grey85', outlier.alpha = 0) +
  geom_line(aes(clutch_identity, initiation_st, group = female_id_year_NA, linetype = next_clutch)) +
  geom_point(aes(clutch_identity, initiation_st, fill = anyEPY), shape = 21, size = 2) +
  scale_fill_manual(values = c('white', 'black'), name = 'any EPP', labels = c('no', 'yes')) +
  scale_linetype_manual(values = c('solid', 'dotted'), name = 'next clutch') +
  scale_x_discrete(labels = c('first', 'second', 'third')) +
  scale_y_continuous(breaks=seq(-15, 15, 5), limits = c(-14.4, 14.4)) + 
  xlab('known clutch identity') + ylab('') +
  geom_text(data = dss, aes(clutch_identity, Inf, label = sample_size), vjust = 1, size = 6) +
  theme_classic_edit(base_size = 20, lp = c(0.85, 0.25))
p2


# load Dale et al. data
dale = read.csv2('./DATA/Dale_EPP.csv') %>% data.table

dale[, mean_initiation := mean(initiation_doy, na.rm = TRUE)]
dale[, initiation_st := initiation_doy - mean_initiation]
dale[, anyEPY := as.factor(anyEPY)]

dale[, .N, by = anyEPY]
dss = data.table(anyEPY = c('0', '1'),
                 sample_size = c('12', '6'))

p3 = 
  ggplot(data = dale) +
  geom_hline(yintercept = 0, color = 'grey70') +
  geom_boxplot(aes(anyEPY, initiation_st), fill = 'grey85', outlier.alpha = 0) +
  geom_jitter(aes(anyEPY, initiation_st, fill = anyEPY), width = 0.3, height = 0, shape = 21, size = 2, show.legend = FALSE) +
  scale_fill_manual(values = c('white', 'black')) +
  scale_x_discrete(labels = c('without EPP', 'with EPP')) +
  scale_y_continuous(breaks=seq(-15, 15, 5), limits = c(-14.4, 14.4)) + 
  xlab('clutches')+ ylab('clutch initiation standardized') +
  geom_text(data = dss, aes(anyEPY, Inf, label = sample_size), vjust = 1, size = 6) +
  theme_classic(base_size = 20)
p3

p1 + p2+ p3


pn = ggplot() + theme_void()

png(paste0('./REPORTS/FIGURES/EPY_timing_talk.png'), width = 1200, height = 500)
p3 + pn + p2 + pn + p1 + plot_layout(nrow = 1, widths = c(1, 0.2, 2, 0.2, 1))
dev.off()


# p1 + facet_grid(~year_)
