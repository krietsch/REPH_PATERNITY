#========================================================================================================================
# Figure 2
#========================================================================================================================

# Summary
# 0. Prepare data for analysis


# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak', 'patchwork', 'multcomp', 'viridis', 'car', 
          'scales', 'ggnewscale'),
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


setorder(d, year_, initiation)

# first and second clutches by females
d[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]
d[, N_female_clutch := .N, by = female_id]
d[, female_clutch := seq_len(.N), by = .(female_id, year_)]
d[is.na(female_id), female_clutch := 1]
d[!is.na(female_id), N_female_clutch := max(female_clutch), by = .(female_id, year_)]
d[is.na(female_id), N_female_clutch := 1]
d[, .N, by = .(year_, female_clutch)]
d[, .N, by = .(female_clutch, external)]

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
# 1. Nests on plot
#------------------------------------------------------------------------------------------------------------------------

ds = d[study_site == TRUE & !is.na(initiation_y)]

dss = ds[, .(median = median(initiation_y), q25 = quantile(initiation_y, probs = c(0.25)), 
             q75 = quantile(initiation_y, probs = c(0.75))), by = year_]

p1 = 
  ggplot(data = ds) +
  geom_violin(aes(as.character(year_), initiation_y), show.legend = FALSE, fill = 'grey70', color = 'grey50') +
  geom_point(data = dss, aes(as.character(year_), median), size = 4) +
  geom_linerange(data = dss, aes(x = as.character(year_), ymin = q75, ymax = q25), size = 1.5) +
  geom_point(data = ds[year_ == 2017 & initiation_y < as.POSIXct('2020-06-16')], aes(as.character(year_), initiation_y), 
             shape = 8, size = 2) +
<<<<<<< HEAD
  scale_y_datetime(date_breaks = "1 week", date_labels = "%d") +
  xlab('Year') + ylab('Date (June)') + 
  theme_classic(base_size = 20)
p1
=======
  xlab('Year') + ylab('Date') + 
  theme_classic(base_size = 20)
p1



# p1 = 
#   ggplot(data = ds) +
#   geom_boxplot(aes(as.character(year_), initiation_y), show.legend = FALSE, fill = 'grey70', color = 'grey50', outlier.alpha = 0) +
#   # geom_point(data = dss, aes(as.character(year_), median), size = 4) +
#   # geom_linerange(data = dss, aes(x = as.character(year_), ymin = q75, ymax = q25), size = 1.5) +
#   geom_jitter(aes(as.character(year_), initiation_y), show.legend = FALSE, color = 'black', width = 0.3, height = 0, size = 2) +
#   xlab('Year') + ylab('Date') + 
#   theme_classic(base_size = 20)
# p1
>>>>>>> 7a69dde99fcc8a50a9fbf157cce4c3ff3340c510

#------------------------------------------------------------------------------------------------------------------------
# 2. Timing of multiple clutches vs. single clutches
#------------------------------------------------------------------------------------------------------------------------

# subset all multiple clutches or in study site
ds = d[!is.na(initiation)]
ds = ds[study_site == TRUE & !(clutch_identity %in% c('first', 'second', 'third')) | clutch_identity %in% c('first', 'second', 'third')]
ds[, anyEPY := as.character(anyEPY)]

ds[, mean_initiation := mean(initiation, na.rm = TRUE), by = year_]
ds[, initiation_st := difftime(initiation, mean_initiation, units = 'days') %>% as.numeric]

ds = ds[!is.na(anyEPY)]

ds[is.na(renesting_male), renesting_male := FALSE]
ds[, any_renesting := any(renesting_male == TRUE), by = female_id_year]
ds[any_renesting == FALSE, next_clutch := 'polyandrous']
ds[any_renesting == TRUE, next_clutch := 'renesting']

# add first and second clutch of females with three clutches again (otherwise linetype does not work)
ds[clutch_identity == 'third']
dss = ds[clutch_identity %in% c('first', 'second') & female_id %in% c(270170935, 19222)]
dss[, female_id_year_NA := paste0(female_id_year, '2')]

ds = rbind(ds, dss)
ds[!(clutch_identity %in% c('first', 'second', 'third')), clutch_identity := 'single']

# factor order
ds[, clutch_identity := factor(clutch_identity, levels = c('single', 'first', 'second', 'third'))]



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

ds[anyEPY == '1', .N, clutch_identity]
ds[, .N, clutch_identity]

dss = data.table(clutch_identity = c('single', 'first', 'second', 'third'),
                 sample_size = c('14/138', '0/15', '3/15', '0/2'))

p2 = 
  ggplot() +
  geom_boxplot(data = ds, aes(clutch_identity, initiation_st), fill = 'grey85', outlier.alpha = 0) +
  geom_line(data = ds, aes(clutch_identity, initiation_st, group = female_id_year, linetype = next_clutch)) +
  geom_point(data = ds[clutch_identity != 'single'], aes(clutch_identity, initiation_st, fill = anyEPY), 
             shape = 21, size = 3) +
  geom_jitter(data = ds[clutch_identity == 'single'], aes(clutch_identity, initiation_st, fill = anyEPY), 
              shape = 21, size = 3, height = 0, width = 0.3) +
  scale_fill_manual(values = c('white', 'black'), name = 'any EPY', labels = c('no', 'yes')) +
  scale_linetype_manual(values = c('solid', 'dotted'), name = 'next clutch') +
  scale_x_discrete(labels = c('single', 'first', 'second', 'third')) +
  xlab('Clutch type') + ylab('Clutch initiation date (standardized)') +
  geom_text(data = dss, aes(clutch_identity, Inf, label = sample_size), vjust = 1, size = 6) +
  theme_classic_edit(base_size = 20, lp = c(0.85, 0.2))
p2




p1 + p2


#------------------------------------------------------------------------------------------------------------------------
# 3. Rates of EPP, polyandry and renesting
#------------------------------------------------------------------------------------------------------------------------


# data sets and data available
d[study_site == TRUE, data_type := 'study_site']
d[parentage == TRUE & study_site == FALSE & external == 0, data_type := 'own_off_site']

# plots with parentage data, total nests in years with data
plot_R = d[parentage == TRUE & study_site == FALSE & external == 1 & plot %like% 'brw']$plot %>% unique
year_R = d[parentage == TRUE & study_site == FALSE & external == 1 & plot %like% 'brw']$year_ %>% unique
d[plot %in% plot_R & year_ %in% year_R, data_type := 'survey_plot']
d[parentage == TRUE & external == 1 & !(plot %like% 'brw') & year_ %in% year_R, data_type := 'clutch_removal_exp']

# subset study site
ds = d[external == 0 & study_site == TRUE]
dss = ds[, .(N_nests = .N), by = year_]
setorder(dss, year_)

# unassigned males and females
ds[is.na(male_id)]
ds[is.na(female_id)]

# males
dsm = unique(ds, by = 'male_id_year_NA')
dsm[, .N, by = male_assigned]
dssm = dsm[, .(unique_males = .N), by = year_]

# renesting males
dsmr = dsm[renesting_male == TRUE, .(renesting_males = .N), by = year_]
dsmr_NARL = dsm[renesting_study_site == TRUE, .(renesting_males_NARL = .N), by = year_]

# females
dsf = unique(ds, by = 'female_id_year_NA')
dsf[, .N, by = female_assigned]
dssf = dsf[, .(unique_females = .N), by = year_]

# polyandrous females
dsfp = dsf[polyandrous == TRUE, .(polyandrous_females = .N), by = year_]
dsfp_NARL = dsf[polyandry_study_site == TRUE, .(polyandrous_females_NARL = .N), by = year_]

# combine data
dss = merge(dss, dssm, by = 'year_', all.x = TRUE)
dss = merge(dss, dsmr, by = 'year_', all.x = TRUE)
dss = merge(dss, dsmr_NARL, by = 'year_', all.x = TRUE)
dss = merge(dss, dssf, by = 'year_', all.x = TRUE)
dss = merge(dss, dsfp, by = 'year_', all.x = TRUE)
dss = merge(dss, dsfp_NARL, by = 'year_', all.x = TRUE)
dss[is.na(dss)] = 0

# percent renesting / polyandrous
dss[, renesting_males_per          := round(renesting_males / unique_males * 100, 1)]
dss[, renesting_males_NARL_per     := round(renesting_males_NARL / unique_males * 100, 1)]
dss[, polyandrous_females_per      := round(polyandrous_females / unique_females * 100, 1)]
dss[, polyandrous_females_NARL_per := round(polyandrous_females_NARL / unique_females * 100, 1)]

# add EPY on plot
dspe = ds[!is.na(N_parentage), .(N_parentage_sum = sum(N_parentage), N_EPY = sum(N_EPY)), by = year_]
dspe[, EPY_per := round(N_EPY / N_parentage_sum * 100, 1)]
dss = merge(dss, dspe, by = 'year_')

dsp = ds[!is.na(N_parentage), .(N_parentage = .N), by = year_]
dspn = ds[!is.na(N_parentage) & anyEPY == TRUE, .(N_anyEPY = .N), by = year_]
dsp = merge(dsp, dspn, by = 'year_')
dsp[, EPY_nests_per := round(N_anyEPY / N_parentage * 100, 1)]
dsp[, EPY := paste0(EPY_nests_per, '% (', N_anyEPY, '/', N_parentage, ')')]
dss = merge(dss, dsp, by = 'year_')

# plot EPP, polyandry and renesting
ds = rbindlist(list(dss[, .(year_, x = EPY_per, n = N_EPY, N = N_parentage_sum, type = 'EPY', study_site = TRUE)],
                    dss[, .(year_, x = EPY_nests_per, n = N_anyEPY, N = N_parentage, type = 'EPP', study_site = TRUE)],
                    dss[, .(year_, x = polyandrous_females_NARL_per, n = polyandrous_females_NARL, N = unique_females, type = 'polyandry', study_site = TRUE)],
                    dss[, .(year_, x = renesting_males_NARL_per, n = renesting_males_NARL, N = unique_males,  type = 'renesting', study_site = TRUE)],
                    dss[, .(year_, x = EPY_per, n = N_EPY, N = N_parentage_sum, type = 'EPY', study_site = FALSE)],
                    dss[, .(year_, x = EPY_nests_per, n = N_anyEPY, N = N_parentage, type = 'EPP', study_site = FALSE)],
                    dss[, .(year_, x = polyandrous_females_per, n = polyandrous_females, N = unique_females,  type = 'polyandry', study_site = FALSE)],
                    dss[, .(year_, x = renesting_males_per, n = renesting_males, N = unique_males,  type = 'renesting', study_site = FALSE)]))

ds[, year_ := as.factor(year_)]
ds[, sample_size := paste0(n, '/', N)]
ds[year_ == '2019' & type %in% c('polyandry', 'renesting'), sample_size := paste0(sample_size, '*')]

ds[, type := factor(type, levels = c('EPY', 'EPP', 'polyandry', 'renesting'))]

p3 = 
  ggplot(data = ds[study_site == FALSE], aes(year_, x, fill = type, label = sample_size)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.7) +
  # geom_text(position = position_dodge(width = 0.7), size = 6, vjust = -0.5) + # text horizontal
  geom_text(position = position_dodge(width = 0.7), size = 6, hjust = -0.1, angle = 90) +
  scale_fill_manual(values = c('grey70', 'grey50','firebrick4', '#33638DFF'), labels = c('EPY', 'nests with EPY', 'polyandrous females', 'renesting males')) +
  scale_y_continuous(limits = c(0, 16), expand = c(0, 0)) +
  xlab('Year') + ylab('Percentage of') + 
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.2, 0.9), legend.title = element_blank())
p3


#------------------------------------------------------------------------------------------------------------------------
# 4. EPY and timing 
#------------------------------------------------------------------------------------------------------------------------

# subset all multiple clutches or in study site
ds = d[!is.na(initiation) & !is.na(anyEPY)]
ds = ds[, .(year_, nestID, initiation_y, anyEPY, study_site = as.character(study_site))]
ds[study_site == TRUE, study_site := 'Intensive study']
ds[study_site == FALSE, study_site := 'Collection']

# load Dale et al. data
dale = read.csv2('./DATA/Dale_EPP.csv') %>% data.table
dale[, initiation_y := as.POSIXct(as.Date(initiation_doy, origin = '1993-01-01'))]

ds = rbind(ds, dale[, .(year_ = YEAR_, nestID, initiation_y, anyEPY, study_site = 'Dale et al.')])


ds[, mean_initiation := mean(initiation_y, na.rm = TRUE), by = year_]
ds[, initiation_st := difftime(initiation_y, mean_initiation, units = 'days') %>% as.numeric]
ds[, anyEPY := as.character(anyEPY)]
ds[, study_site := as.character(study_site)]

dss = ds[, .(median = median(initiation_st), q25 = quantile(initiation_st, probs = c(0.25)), 
             q75 = quantile(initiation_st, probs = c(0.75))), by = .(study_site, anyEPY)]

# factor order
ds[, study_site := factor(study_site, levels = c('Intensive study', 'Collection', 'Dale et al.'))]



p4 = 
  ggplot(data = ds, aes(study_site, initiation_st, fill = anyEPY)) +
  geom_violin(position = position_dodge(width = 0.9)) +
  geom_point(data = dss, aes(study_site, median, group = anyEPY), position = position_dodge(width = 0.9)) +
  # geom_errorbar(data = dss, aes(study_site, ymax = q75, ymin = q25, group = anyEPY), position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('grey70', 'grey50'), labels = c('0', '1')) +
  xlab('Data source') + ylab('Clutch initiation date (standardized)') + 
  theme_classic(base_size = 20)

p4

p4 = 
  ggplot(data = ds, aes(study_site, initiation_st, fill = anyEPY)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.alpha = 0) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3, jitter.height = 0, dodge.width = 0.9)) +
  scale_fill_manual(values = c('grey70', 'grey50'), labels = c('0', '1')) +
  xlab('Data source') + ylab('Clutch initiation date (standardized)') + 
  theme_classic(base_size = 20)

p4



# 
# # Function to produce summary statistics (mean and +/- sd)
# data_summary <- function(x) {
#   m <- mean(x)
#   ymin <- m-sd(x)
#   ymax <- m+sd(x)
#   return(c(y=m,ymin=ymin,ymax=ymax))
# }
# 
# # Use a custom summary function :
#   
#   p4 + stat_summary(fun.data=data_summary, position_dodge(width=0.75))
# 
# 










# png(paste0('./REPORTS/FIGURES/EPY_timing.png'), width = 1000, height = 800)
p1 + p2 + p3 + p4 + plot_layout(ncol = 2, nrow = 2)
# dev.off()
# 
# 
# p + stat_summary(fun.data="mean_sdl", mult=1, 
#                  geom="crossbar", width=0.2 )
# p + stat_summary(fun.data=mean_sdl, mult=1, 
#                  geom="pointrange", color="red")

