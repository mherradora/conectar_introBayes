#---------------
# Mortalidad infantil en Uruguay
#---------------

library(tidyverse)
library(viridis)

# leemos datos
tmi_dd <- readRDS(file = 'tasas-bayes/datos_tmi.rds')

tmi_dd %>% 
  filter(Depto %in% c('MONTEVIDEO', 'INTERIOR')) %>%
  ggplot(aes(x=anio, y=Tasa, color=Depto)) + geom_point() + 
  geom_line()


# nos quedamos con datos para year > 2012 (3 obs por departamento)
dd <- filter(tmi_dd, anio > 2012,
              !Depto %in% c('TOTAL', 'INTERIOR') ) %>%
  group_by(anio) %>% 
  mutate(tasa.tot = sum(Defunciones)/sum(Nacimientos)) %>% 
  ungroup() %>% mutate(Depto = reorder(factor(Depto), Nacimientos, median),
                       smr = Defunciones/ (tasa.tot*Nacimientos) ) %>%
  dplyr::select(-tasa.tot)


# Figuras
dd %>% 
  ggplot( aes(y=Depto, x = Tasa) ) + geom_point() + 
  facet_grid(~anio)
  
dd %>% 
  ggplot( aes(x = Depto, y = Tasa, color = anio ) ) + 
  geom_point() + 
  labs(y = 'TMI (por mil)', x = '', color ='') + 
  theme(axis.text.x = element_text(angle = 60)) + 
  scale_color_viridis( breaks = c(2013,2014,2015) )

# la misma figura pero guardadda para las slides
# dd %>%
#   ggplot( aes(x = Depto, y = Tasa, color = anio ) ) +
#   geom_point(size= 5) +
#   labs(y = 'TMI (por mil)', x = '', color ='') +
#   theme(axis.text.x = element_text(angle = 90, size = I(10)), aspect.ratio = 1/2) +
#   scale_color_viridis( breaks = c(2013,2014,2015) ) +
#   ggsave(filename = 'tasas-bayes/tasas_fig1.pdf', height = 7, width = 7)


# ----------------
# Modelo Bayesiano jerarquico
# ----------------

library(rstan); 
rstan_options(auto_write = TRUE)  
# options(mc.cores = 3); # para computos en paralelo

# Primero escribimos el modelo en archivo mdjer_poigam.stan

# datos para STAN
dat <- dd %>% 
  with(list(n=length(anio), N = Nacimientos, y=Defunciones, 
       G = 19, gr = as.integer(factor(Depto)) ) )

# Obtenemos los resultados del modelo 
res.jer.poigam <- stan(file = 'tasas-bayes/mdjer_poigam.stan', data = dat)


# Otra opcion: compilar y obtener muestras mcmc en pasos separados
# mod.jer.poigam <- stan_model(file = 'tasas-bayes/mdjer_poigam.stan')
# res.jer.poigam = sampling(mod.jer.poigam, data = dat)

# diagnostico MCMC
plot(res.jer.poigam, plotfun = 'rhat')
plot(res.jer.poigam, plotfun = 'ess')

# Posterior de hyperparameters
plot(res.jer.poigam, pars=c('alpha', 'beta'), plotfun = 'hist')

pairs(res.jer.poigam, pars=c('alpha', 'beta'))

plot(res.jer.poigam, pars=c('mu'), plotfun = 'hist')

# Intervalos de credibilidad para TMI por departamento
plot(res.jer.poigam, pars=c('tmi') ) +
  scale_y_continuous(breaks=1:19,
                     labels = levels(dd$Depto)[19:1])
# Figuras: 
# combinamos los datos con los IC para TMI en cada region
summary(res.jer.poigam, pars='tmi')$summary[] %>% 
  data.frame() %>% 
  mutate(gr = 1:19) %>% 
  inner_join(mutate(dd, gr = as.integer(Depto) ) ) %>% 
  mutate(depto2 = reorder(Depto, Nacimientos, median) ) %>% 
  ggplot() + 
  geom_pointrange(aes(x=depto2, y= X50., ymin = X2.5., ymax=X97.5. )) +
  geom_point(aes(x=depto2, y = Tasa), color = 'red') + 
  labs(x = '', y = 'Mediana posterior - IC 95') + 
  theme(axis.text.x = element_text(angle = 60))

# Efecto shrinkage: 
# regiones de menor poblacion muestran estimaciones 
# de TMI cercanas a la media global
summary(res.jer.poigam, pars='tmi')$summary[] %>% 
  data.frame() %>% 
  mutate(gr = 1:19) %>% 
  inner_join(mutate(dd, gr = as.integer(Depto) ) ) %>% 
  ggplot() + geom_point(aes(y = Tasa-mean, x= log10(Nacimientos)) ) 


# Probabilidad de observar 0 muertes en Flores
dd %>% filter(Depto == 'FLORES')

pred.flores <- 
rstan::extract(res.jer.poigam, pars = 'tmi') %>% # extraigo tmi
  data.frame() %>% select(tmi.1) %>% # selectiono flores y montevideo
  mutate(lam.flores = 350*tmi.1/1e3, 
         ytilde = rpois(4e3, lambda = lam.flores)  ) 

mean(pred.flores$ytilde == 0)

plot(res.jer.poigam, pars=c('ytilde_flores'), plotfun = 'hist', binwidth=1) +
  theme_bw()

######################################################
######################################################

