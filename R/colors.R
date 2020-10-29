library(wesanderson)
names(wes_palettes)

x = 1:4

dt = data.table(N = c(x),
                group = as.character(x))

p = 
ggplot(data = dt, aes(N, fill = group)) +
  geom_bar() +
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 5))
p
  


ggplot_build(p)$data 



p = 
  ggplot(data = dt, aes(N, fill = group)) +
  geom_bar() +
  scale_fill_viridis(discrete = TRUE, option = 'inferno')
p

ggplot_build(p)$data 



p = 
  ggplot(data = dt, aes(N, fill = group)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1")
p


p = 
  ggplot(data = dt, aes(N, fill = group)) +
  geom_bar() +
  scale_fill_brewer(palette = "Spectral")
p

ggplot_build(p)$data 


p = 
  ggplot(data = dt, aes(N, fill = group)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2")
p

ggplot_build(p)$data 

p = 
  ggplot(data = dt, aes(N, fill = group)) +
  geom_bar() +
  scale_fill_grey()
p

ggplot_build(p)$data 



