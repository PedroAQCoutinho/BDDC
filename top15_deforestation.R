library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(rmarkdown)

#set
options(scipen = 999)
options(stringsAsFactors = F)

#load
database <- read.csv2('data/municipal_deforestation_multiple_sources.csv')
head(database)
#load spatial
br.uf <- st_read('data/BR_UF_2021.shp')
br.mun <- st_read('D:/dados_GPP/geo_adm/municipios/BR_Municipios_2021.shp')
br.mun <- br.mun %>% mutate(cd_munc = as.numeric(CD_MUN))
#clip br.uf
bbox.clip <- br.uf %>%
  filter(SIGLA %in% c('BA','AM', 'RR', 'RN', 'PB', 'PE')) %>% 
  st_bbox() %>%
  st_as_sfc()

br.uf.clip <- st_intersection(br.uf, bbox.clip)


#get only prodes data and selects top 15 municipalities with the largest deforestation areas accumulated
top15.accumulated.absolute.deforest <- database %>%
  filter(fonte == 'PRODES') %>%
  filter(!ano %in% c('2001','2021')) %>%
  mutate(area_desmatada_ha=as.numeric(area_desmatada_ha),
         CD_MUN=as.character(cd_mun)) %>%
  group_by(CD_MUN) %>%
  summarise(area_desmatada_ha = sum(area_desmatada_ha, na.rm = T)) %>%
  left_join(br.mun, by = c('CD_MUN'='CD_MUN')) %>%
  arrange(desc(area_desmatada_ha)) %>%
  mutate(accum = cumsum(area_desmatada_ha),
         total = sum(area_desmatada_ha),
         paccum = accum/total) %>%
  slice(1:15) %>%
  select(CD_MUN, NM_MUN, area_desmatada_ha, paccum, SIGLA, geometry);top15.accumulated.absolute.deforest

#top 15 absolute accumulated deforest
pa <- top15.accumulated.absolute.deforest %>%
ggplot() +
  geom_sf(data = br.uf.clip, fill = NA)+
  geom_sf(aes(fill = area_desmatada_ha, geometry = geometry)) +
  scale_fill_viridis('Deforested area (ha)\n(17.1% total)', option = 'G')+
  theme_bw(); pa

#save
ggsave('images/top15_deforestation_map.png', plot = pa,
       units = 'in', dpi = 200, width = 30, height = 20, scale = 0.4)





#get only prodes data and selects top 50 municipalities with the largest deforestation density
top15.deforest.density <- database %>%
  filter(fonte == 'PRODES') %>%
  filter(!ano %in% c('2001','2021')) %>%
  mutate(area_desmatada_ha=as.numeric(area_desmatada_ha),
         CD_MUN=as.character(cd_mun)) %>%
  group_by(CD_MUN) %>%
  summarise(area_desmatada_ha = sum(area_desmatada_ha, na.rm = T)) %>%
  left_join(br.mun , by = c('CD_MUN'='CD_MUN')) %>%
  mutate(dens = area_desmatada_ha/(AREA_KM2*10000)) %>%
  arrange(desc(dens)) %>%
  slice(1:15)  %>%
  select(CD_MUN, NM_MUN, area_desmatada_ha, dens, SIGLA, geometry); top15.deforest.density

sum(top15.deforest.density$area_desmatada_ha)/(database %>%
                                                    filter(fonte == 'PRODES') %>%
                                                    filter(!ano %in% c('2001','2021')) %>%
                                                    mutate(area_desmatada_ha=as.numeric(area_desmatada_ha)) %>%
                                                  summarise(area_desmatada_ha = sum(area_desmatada_ha, na.rm = T)))

#top 50 deforest density
pb <- top15.deforest.density %>%
  ggplot() +
  geom_sf(data = br.uf.clip, fill = NA)+
  geom_sf(aes(fill = dens, geometry = geometry)) +
  scale_fill_viridis('Deforestation density (ha/10km²)\n(2.2% total)', option = 'D')+
  theme_bw() ;pb

#save
ggsave('images/top15_deforestation_density_map.png', plot = pb,
       units = 'in', dpi = 200, width = 30, height = 20, scale = 0.4)










#select top 15 mun with largest deforestation accumulated area
top15.mun <- database %>%
  filter(fonte == 'PRODES') %>%
  mutate(area_desmatada_ha=as.numeric(area_desmatada_ha),
         CD_MUN=as.character(cd_mun)) %>%
  group_by(CD_MUN) %>%
  summarise(area_desmatada_ha = sum(area_desmatada_ha, na.rm = T)) %>%
  arrange(desc(area_desmatada_ha)) %>%
  mutate(accum = cumsum(area_desmatada_ha),
         total = sum(area_desmatada_ha),
         paccum = accum/total) %>%
  slice(1:15) %>%
  pull(CD_MUN) ; top15.mun


#filter and prepare to plot lines
to.plot <- database %>%
  left_join(br.mun %>% st_drop_geometry(), by = c('cd_mun'='cd_munc')) %>%
  mutate(CD_MUN=as.character(cd_mun),
         area_desmatada_ha=as.numeric(area_desmatada_ha),
         ano = as.numeric(ano),
         cd_mun = as.factor(cd_mun)) %>%
  filter((CD_MUN %in% top15.mun) & fonte == 'PRODES' & ano > 2001 & ano < 2021) %>%
  mutate(NM_MUN = paste0(NM_MUN, ' (', SIGLA, ')'))

#plot 
p2 <- to.plot %>%   
  ggplot() +
  geom_line(aes(x = ano,
                y = area_desmatada_ha,
                colour = NM_MUN))+
  labs(y = 'Deforested area (ha)',
       x = 'Year',
       colour = 'Municipalities') ; p2

#save
ggsave('images/top15_deforestation_ts.png', plot = p2,
       units = 'in', dpi = 200, width = 30, height = 20, scale = 0.4)



#removing blue outlier cd_mun == 1507300
#filter and prepare to plot
to.plot <- database %>%
  filter(cd_mun != '1507300') %>%
left_join(br.mun %>% st_drop_geometry(), by = c('cd_mun'='cd_munc')) %>%
  mutate( area_desmatada_ha=as.numeric(area_desmatada_ha),
         ano = as.numeric(ano),
         cd_mun = as.factor(cd_mun)) %>%
  filter((CD_MUN %in% top15.mun) & fonte == 'PRODES' & ano > 2001 & ano < 2021) %>%
  mutate(NM_MUN = paste0(NM_MUN, ' (', SIGLA, ')'))

#plot
p3 <- to.plot %>%   
  ggplot() +
  geom_line(aes(x = ano,
                y = area_desmatada_ha,
                colour = NM_MUN))+
  labs(y = 'Deforested area (ha)',
       x = 'Year',
       colour = 'Municipalities') ; p3

ggsave('images/top15_deforestation_ts_without_outlier.png', plot = p3,
       units = 'in', dpi = 200, width = 30, height = 20, scale = 0.4)





#save
to.plot <- database %>%
  left_join(br.mun %>% st_drop_geometry(), by = c('cd_mun'='cd_munc')) %>%
  mutate(CD_MUN=as.character(cd_mun),
         area_desmatada_ha=as.numeric(area_desmatada_ha),
         ano = as.numeric(ano),
         cd_mun = as.factor(cd_mun)) %>%
  filter((CD_MUN %in% top15.mun) & fonte == 'PRODES' & ano > 2001 & ano < 2021) %>%
  mutate(NM_MUN = paste0(NM_MUN, ' (', SIGLA, ')'))

write.csv2(to.plot, 'top15_deforestation.csv',row.names = F)




#render html
rmarkdown::render('top15_deforestation.R')










