# Possible results of random sampling for COVID-19 in Germany

require(data.table)
require(foreach)
require(ggplot2)

# define parameters
GER_pop = 83738147 # source: https://www.worldometers.info/world-population/germany-population/ (28.4.2020)
known_active_cases = 35477 # source: https://www.worldometers.info/coronavirus/ (17.4.2020)
unknown_active_cases = 35477 # assume real active case number is twice as high
  
x = rep(0, c(GER_pop - known_active_cases - unknown_active_cases))
y = rep(1, unknown_active_cases)

d = c(x, y)
d = sample(d)

# once 10000 samples
ds = sample(d, size = 10000)
sum(ds)

# 1000 times 10000 samples 
dr = foreach(i = 1:1000, .combine = 'rbind') %do% {
  
  ds = sample(d, size = 10000)
  ds = data.table(run = i,
                  cases_found = sum(ds))
  
}


ggplot(data = dr) +
  geom_histogram(aes(x = cases_found), bins = max(dr$cases_found)) +
  theme_classic(base_size = 18)

mean(dr$cases_found)

# sample 4 times (one month each week)
dr = foreach(i = 1:1000, .combine = 'rbind') %do% {
  
  w1 = sample(d, size = 10000)
  w2 = sample(d, size = 10000)
  w3 = sample(d, size = 10000)
  w4 = sample(d, size = 10000)
  
  ds = data.frame(run = i,
                  week = c('w1', 'w2', 'w3', 'w4'),
                  cases_found = c(sum(w1), sum(w2), sum(w3), sum(w4)))

  
}

dr = data.table(dr)

# re-run lines below to see 1 possible outcome 
dr1 = dr[run == sample(1000, 1)]

ggplot() +
  geom_line(data = dr, aes(x = week, y = cases_found, group = run), color = 'grey60', alpha  = 0.5) +
  geom_line(data = dr1, aes(x = week, y = cases_found, group = run), color = 'firebrick2', size = 2) +
  theme_classic(base_size = 18)




