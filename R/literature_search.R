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
d2 = readRDS('./DATA/promiscuity_v2.rds')

d = d[scinam %in% dsp$scinam]
d2 = d2[scinam %in% dsp$scinam]

# merge with shorebird data
d = merge(d, ds, by = 'scinam', all.x = TRUE)
d2 = merge(d2, ds, by = 'scinam', all.x = TRUE)

write.csv2(d, './DATA/AVES_v1.csv')
write.csv2(d2, './DATA/AVES_v2.csv')


# open Brouwer_et_al_ESM
d = read.csv2('./DATA/Brouwer_et_al_ESM.csv') %>% data.table
d[, Scientific.name := tolower(Scientific.name)]

d = d[Scientific.name %in% dsp$scinam]

write.csv(d, './DATA/Brouwer_et_al_ESM_shorebirds.csv')








  