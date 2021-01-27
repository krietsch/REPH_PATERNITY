#========================================================================================================================
# Paternity analysis
#========================================================================================================================

# Summary
# 0. Prepare data for analysis
# 1. Snow melt predicting timing of breeding


# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'sf', 'auksRuak', 'patchwork', 'multcomp', 'viridis', 'car'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')
dc = dbq(con, 'select * FROM CAPTURES')
dp = dbq(con, 'select * FROM PATERNITY')
dw = dbq(con, 'select * FROM SNOW_SURVEY')
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
# 2. Snow melt predicting timing of breeding
#------------------------------------------------------------------------------------------------------------------------

# data sets and data available
d[study_site == TRUE, data_type := 'study_site']
d[parentage == TRUE & study_site == FALSE & external == 0, data_type := 'own_off_site']

# plots with parentage data, total nests in years with data
plot_R = d[parentage == TRUE & study_site == FALSE & external == 1 & plot %like% 'brw']$plot %>% unique
year_R = d[parentage == TRUE & study_site == FALSE & external == 1 & plot %like% 'brw']$year_ %>% unique
d[plot %in% plot_R & year_ %in% year_R, data_type := 'survey_plot']
d[parentage == TRUE & external == 1 & !(plot %like% 'brw') & year_ %in% year_R, data_type := 'clutch_removal_exp']

ds = d[!is.na(data_type), .(N_nests = .N), by =  data_type]
ds2 = d[parentage == TRUE, .(N_parentage = .N), by =  data_type]
ds = merge(ds, ds2, by = c('data_type'), all.x = TRUE)
ds[, N_parentage := paste0(round(N_parentage / N_nests * 100, 0), '% (', N_parentage, '/', N_nests, ')')]
ds

# position of nests with parentage data
# bm = create_bm(d)
# bm + geom_point(data = d[parentage == TRUE], aes(lon, lat, color = study_site))
# bm + geom_point(data = d[parentage == TRUE & !is.na(data_type)], aes(lon, lat, color = data_type))
# bm + geom_point(data = d[parentage == TRUE], aes(lon, lat, color = YEAR_))
# bm + geom_point(data = d[parentage == TRUE], aes(lon, lat, color = as.character(anyEPY)))

# split in year only
ds = d[, .(N_nests = .N), by = .(year_)]
ds2 = d[parentage == TRUE, .(N_parentage = .N), by = .(year_)]
ds3 = d[anyEPY == TRUE, .(N_EPY = .N), by = .(year_)]
ds4 = d[, .(N_eggs = sum(N_parentage, na.rm = TRUE), N_eggs_EPY = sum(N_EPY, na.rm = TRUE)), by = .(year_)]
ds = merge(ds, ds2, by = c('year_'), all.x = TRUE)
ds = merge(ds, ds3, by = c('year_'), all.x = TRUE)
ds = merge(ds, ds4, by = c('year_'), all.x = TRUE)
ds[is.na(ds)] = 0
ds = ds[N_parentage != 0]
ds[, EPY_nests_ := round(N_EPY / N_parentage * 100, 1)]
ds[, EPY_eggs_  := round(N_eggs_EPY / N_eggs * 100, 1)]
ds[, EPY_nests := paste0(round(N_EPY / N_parentage * 100, 1), '% (', N_EPY, '/', N_parentage, ')')]
ds[, EPY_eggs  := paste0(round(N_eggs_EPY / N_eggs * 100, 1), '% (', N_eggs_EPY, '/', N_eggs, ')')]

# plot 
ds[, sample_size := paste0(N_EPY, '/', N_parentage)]
ds[, sample_size_eggs := paste0(N_eggs_EPY, '/', N_eggs)]
ds[, year_ := as.factor(year_)]

p1 = 
  ggplot(data = ds, aes(year_, EPY_nests_, label = sample_size)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.7, fill = 'grey50') +
  geom_text(position = position_dodge(width = 0.7), size = 6, vjust = -0.5) +
  scale_y_continuous(limits = c(0, 22), expand = c(0, 0)) +
  xlab('Year') + ylab('% nests with EPY') + 
  theme_classic(base_size = 20)

p2 = 
  ggplot(data = ds, aes(year_, EPY_eggs_, label = sample_size_eggs)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.7, fill = 'grey50') +
  geom_text(position = position_dodge(width = 0.7), size = 6, vjust = -0.5) +
  scale_y_continuous(limits = c(0, 11), expand = c(0, 0)) +
  xlab('Year') + ylab('% of EPY') + 
  theme_classic(base_size = 20)

p1 + p2 + plot_layout(nrow = 2)

# add plot with initiation dates

ds2 = d[year_ %in% c(2003, 2004, 2005, 2006, 2014, 2017, 2018, 2019)]
ds3 = d[parentage == TRUE & year_ %in% c(2003, 2004, 2005, 2006, 2014, 2017, 2018, 2019)]

ds2[, data_type_parentage := 'all']
ds3[, data_type_parentage := 'with_parentage']

ds2 = rbind(ds2, ds3)

ds2[, .N, year_]

p3 = 
ggplot(data = ds2) +
  geom_boxplot(aes(as.character(year_), initiation_y , color = data_type_parentage), show.legend = FALSE) +
  xlab('Year') + ylab('Date') + 
  theme_classic(base_size = 20)
p3


p3 = 
  ggplot(data = ds2[data_type_parentage == 'all']) +
  geom_violin(aes(as.character(year_), initiation_y), show.legend = FALSE, fill = 'grey50', color = 'grey30') +
  geom_jitter(aes(as.character(year_), initiation_y), show.legend = FALSE, color = 'black', width = 0.3, height = 0) +
  xlab('Year') + ylab('Date') + 
  theme_classic(base_size = 20)
p3


p3 = 
  ggplot(data = ds2[data_type_parentage == 'all']) +
  geom_violin(aes(as.character(year_), initiation_y), show.legend = FALSE, fill = 'grey50', color = 'grey30') +
  geom_boxplot(aes(as.character(year_), initiation_y), show.legend = FALSE) +
  geom_jitter(aes(as.character(year_), initiation_y), show.legend = FALSE, color = 'black', width = 0.3, height = 0) +
  xlab('Year') + ylab('Date') + 
  theme_classic(base_size = 20)
p3


dss = ds2[data_type_parentage == 'all', .(median = median(initiation_y, na.rm = TRUE), q25 = quantile(initiation_y, probs = c(0.25), na.rm = TRUE), 
                                          q75 = quantile(initiation_y, probs = c(0.75), na.rm = TRUE), .N, max = max(initiation_y, na.rm = TRUE)), by = year_]



ggplot(data = ds2[data_type_parentage == 'all']) +
  geom_violin(aes(as.character(year_), initiation_y), show.legend = FALSE, fill = 'grey85') +
  geom_point(data = dss, aes(as.character(year_), median), size = 2) +
  geom_linerange(data = dss, aes(x = as.character(year_), ymin = q75, ymax = q25), size = 0.5) +


p1 + p2 + p3 + plot_layout(nrow = 3, heights = c(1, 1 ,2))

ds2[, initiation_median := median(initiation_y, na.rm = TRUE), by = year_]

ds2[, `:=` (q05 = quantile(initiation_doy, probs = c(0.05), na.rm = TRUE), 
            q95 = quantile(initiation_doy, probs = c(0.95), na.rm = TRUE)), by = year_]

ds2[, `:=` (q25 = quantile(initiation_doy, probs = c(0.25), na.rm = TRUE), 
            q75 = quantile(initiation_doy, probs = c(0.75), na.rm = TRUE)), by = year_]



ds2 = unique(ds2[, .(year_, q05, q95, q25, q75, initiation_median)], by = 'year_')
ds2[, year_ := as.factor(year_)]


ds = merge(ds, ds2, by = 'year_')
ds[, q95_q05 := q95 - q05]
ds[, q75_q25 := q75 - q25]

ggplot(data = ds, aes(q95_q05, EPY_nests_, size = N_parentage, label = year_)) + 
  geom_point() +
  geom_text(aes(label=year_), hjust = 0.5, vjust = 1.7, size = 5)

ggplot(data = ds, aes(q75_q25, EPY_nests_, size = N_parentage, label = year_)) + 
  geom_point() +
  geom_text(aes(label=year_), hjust = 0.5, vjust = 1.7, size = 5)



dw = dw[, .(snow_cover = mean(snow_cover, na.rm = TRUE)), by = .(year_, date_)]

# extract date
dw[, date_ := as.POSIXct(date_)]
dw[, date_y := as.POSIXct(format(date_, format = "%m-%d"), format = "%m-%d")]
dw_20 = dw[snow_cover < 20, .(snow_cover_20 = min(date_y)), by = year_]
dw_50 = dw[snow_cover < 50, .(snow_cover_50 = min(date_y)), by = year_]

dw_20[, year_ := as.factor(year_)]
dw_50[, year_ := as.factor(year_)]

ds = merge(ds, dw_20, by = 'year_', all.x = TRUE)
ds = merge(ds, dw_50, by = 'year_', all.x = TRUE)

ggplot(data = ds) +
  geom_point(aes(snow_cover_20, EPY_nests_), color = 'dodgerblue') +
  geom_point(aes(snow_cover_50, EPY_nests_), color = 'firebrick')



ggplot(data = ds) +
  geom_point(aes(snow_cover_20, initiation_median), color = 'dodgerblue') +
  geom_point(aes(snow_cover_50, initiation_median), color = 'firebrick')













