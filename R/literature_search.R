#========================================================================================================================
# Search db for literature of EPP studies with shorebirds
#========================================================================================================================
# Packages
sapply( c('data.table', 'magrittr', 'ggplot2', 'viridis'),
        require, character.only = TRUE)

# get shorebirds
df = readRDS('./DATA/family.rds')
ds = readRDS('./DATA/shorebird.rds')

# subset shorebird families
dsp = df[family %in% c('scolopacidae', 'charadriidae', 'burhinidae', 'pedionomidae', 'thinocoridae', 
                       'rostratulidae', 'jacanidae')]
# all data
d = readRDS('./DATA/promiscuity.rds')
d = readRDS('./DATA/promiscuity_v2.rds')

d = d[scinam %in% dsp$scinam]
d2 = d2[scinam %in% dsp$scinam]


  