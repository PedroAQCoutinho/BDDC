library(dplyr)
library(ggplot2)
library(viridis)
library(sf)
library(tidyverse)

rm(list=ls())
#set
options(stringsAsFactors = F)
options(scipen = 999)

#load
base.compilada <- read.csv2('data/municipal_deforestation_multiple_sources.csv')
#load
br.mun <- st_read('data/BR_Municipios_2021.shp')
br.mun$CD_MUN <- as.numeric(br.mun$CD_MUN)
#load
br.estados <- st_read('data/BR_UF_2021.shp')
#explore
#colnames
base.compilada %>%
  colnames

#anos
base.compilada %>% pull(ano) %>% unique

#ano x base
ano.base <- base.compilada %>% group_by(ano, fonte) %>% tally()

#mun prodes
mun.prodes <- base.compilada %>% filter(ano == 2002 & fonte == 'PRODES') %>%
  pull(cd_mun) %>% data.frame() %>% rename(cd_mun = '.') %>%
  mutate(check = TRUE)


#quadrienios comuns
#1º - 2002, 2003, 2004, 2005
#2º - 2006, 2007, 2008, 2009
#3º - 2010, 2011, 2012, 2013
#4º - 2014, 2015, 2016, 2017

out <- base.compilada %>% 
  filter(ano >= 2001 & ano <= 2019) %>% #filter just common years between bases
  filter(grepl('GLAD|MAPBIOMAS|PRODES',fonte)) %>% #filter just GLAD, MAPBIOMAS, PRODES
  mutate(quadrienio = case_when( #LABEL IN 4-YEAR GROUPS
    ano >= 2001 & ano < 2005 ~ 1,
    ano >= 2005 & ano < 2009 ~ 2,
    ano >= 2009 & ano < 2013 ~ 3,
    ano >= 2013 & ano < 2017 ~ 4,
    ano >= 2017 & ano < 2020 ~ 5),
    area_desmatada_ha = as.numeric(area_desmatada_ha), 
    quadrienio = as.character(quadrienio)) %>% 
    group_by(cd_mun, fonte, quadrienio) %>%
    summarise(area_desmatada_ha = sum(area_desmatada_ha, na.rm=T)) %>% #AGGREGATE DATA BY THE SUM
    pivot_wider(names_from = c('fonte','quadrienio'), values_from = 'area_desmatada_ha') %>% #TRANSFORM TO WIDE FOR COLUMN OPERATION
    left_join(mun.prodes ,by = c('cd_mun'='cd_mun')) %>% #BRING LIMITING MUNICIPALITIES (PRODES HAS LIMITES SPATIAL RANGE)
    filter(check) %>% as_tibble %>% #filter
    dplyr::select(-check) %>%
    mutate(
    #ABSOLUTE DIFFERENCE
    #4YEAR 1
    #dif_GLAD_MAPBIOMAS_1 = abs(GLAD_1-MAPBIOMAS_1),
    #dif_GLAD_PRODES_1 = abs(GLAD_1-PRODES_1),
    dif.PRODES.MAPBIOMAS_1 = abs(PRODES_1-MAPBIOMAS_1),
    #4YEAR 2
    #dif_GLAD_MAPBIOMAS_2 = abs(GLAD_2-MAPBIOMAS_2),
    #dif_GLAD_PRODES_2 = abs(GLAD_2-PRODES_2),
    dif.PRODES.MAPBIOMAS_2 = abs(PRODES_2-MAPBIOMAS_2),
    #4YEAR 3
    #dif_GLAD_MAPBIOMAS_3 = abs(GLAD_3-MAPBIOMAS_3),
    #dif_GLAD_PRODES_3 = abs(GLAD_3-PRODES_3),
    dif.PRODES.MAPBIOMAS_3 = abs(PRODES_3-MAPBIOMAS_3),
    #4YEAR 4
    #dif_GLAD_MAPBIOMAS_4 = abs(GLAD_4-MAPBIOMAS_4),
    #dif_GLAD_PRODES_4 = abs(GLAD_4-PRODES_4),
    dif.PRODES.MAPBIOMAS_4 = abs(PRODES_4-MAPBIOMAS_4),
    #4YEAR 5
    #dif_GLAD_MAPBIOMAS_5 = abs(GLAD_5-MAPBIOMAS_5),
    #dif_GLAD_PRODES_5 = abs(GLAD_5-PRODES_%),
    dif.PRODES.MAPBIOMAS_5 = abs(PRODES_5-MAPBIOMAS_5)    ) %>%
  mutate(
    #4YEAR1
    #GLAD.MAPBIOMAS_1 = (GLAD_1-MAPBIOMAS_1)/mean(c(GLAD_1,MAPBIOMAS_1)),
    #GLAD.PRODES_1 = (GLAD_1-PRODES_1)/mean(c(GLAD_1, PRODES_1)),
    dif.rel.PRODES.MAPBIOMAS_1 = (ifelse(PRODES_1 == 0, 0, (MAPBIOMAS_1-PRODES_1)/PRODES_1)),
    #4YEAR 2
    #GLAD.MAPBIOMAS_2 = (GLAD_2-MAPBIOMAS_2)/mean(c(GLAD_2,MAPBIOMAS_2)),
    #GLAD.PRODES_2 = (GLAD_2-PRODES_2)/mean(c(GLAD_2, PRODES_2)),
    dif.rel.PRODES.MAPBIOMAS_2 = (ifelse(PRODES_2 == 0, 0, (MAPBIOMAS_2-PRODES_2)/PRODES_2)),
    #4YEAR 3
    #GLAD.MAPBIOMAS_3 = (GLAD_3-MAPBIOMAS_3)/mean(c(GLAD_3,MAPBIOMAS_3)),
    #GLAD.PRODES_3 = (GLAD_3-PRODES_3)/mean(c(GLAD_3, PRODES_3)),
    dif.rel.PRODES.MAPBIOMAS_3 = (ifelse(PRODES_3 == 0, 0, (MAPBIOMAS_3-PRODES_3)/PRODES_3)),
    #4YEAR 4
    #GLAD.MAPBIOMAS_4 = (GLAD_4-MAPBIOMAS_4)/mean(c(GLAD_4,MAPBIOMAS_4)),
    #GLAD.PRODES_4 = (GLAD_4-PRODES_4)/mean(c(GLAD_4, PRODES_4)),
    dif.rel.PRODES.MAPBIOMAS_4 = (ifelse(PRODES_4 == 0, 0, (MAPBIOMAS_4-PRODES_4)/PRODES_4)),
    #4YEAR 5
   #dif_GLAD_MAPBIOMAS_5 = abs(GLAD_5-MAPBIOMAS_5),
   #dif_GLAD_PRODES_5 = abs(GLAD_5-PRODES_%),
   dif.rel.PRODES.MAPBIOMAS_5 = (ifelse(PRODES_5 == 0, 0, (MAPBIOMAS_5-PRODES_5)/PRODES_5))
   ) %>%
    pivot_longer(names_to = c('fonte','quadrienio'), names_sep = '_', #BACK TO LONGER
               values_to = 'area_desmatada_ha', cols = 2:26) %>%
    mutate(nm_quadrienio = case_when(quadrienio == '1'~ '2001 - 2004',
                                   quadrienio == '2'~ '2005 - 2008',
                                   quadrienio == '3'~ '2009 - 2012',
                                   quadrienio == '4'~ '2013 - 2016',
                                   quadrienio == '5'~ '2017 - 2019')) %>%
  left_join(br.mun[,c('CD_MUN','AREA_KM2','geometry')], by = c('cd_mun'='CD_MUN')) %>% #GETS MUNICIPAL AREA AND GEOMETRY
  mutate(AREA_HA = AREA_KM2*100,
         densidade_desmatamento = area_desmatada_ha/AREA_HA) %>% #DEFORESTATION DENSITY BASED ON THE FULL MUNICIPALITIE AREA
  arrange(quadrienio, fonte,desc(area_desmatada_ha)) %>% 
  group_by(quadrienio, fonte) %>%
  mutate(acumulado = cumsum(area_desmatada_ha),
         total = sum(area_desmatada_ha, na.rm=T),
         total_area_km2 = sum(AREA_KM2),
         rAcumulado = acumulado/total,
         area_desmatada_ha_cat = case_when(rAcumulado < 0.25 ~ '0 - 25%',
                                           (rAcumulado >=0.25 & rAcumulado< 0.5) ~ '25 - 50%',
                                           (rAcumulado >=0.5 & rAcumulado < 0.75) ~ '50 - 75%',
                                           rAcumulado >= 0.75 ~ '75 - 100% ')) %>%
  arrange(quadrienio, fonte,desc(densidade_desmatamento))


unique(out$fonte)
unique(out$quadrienio)
unique(out$nm_quadrienio)


if(!dir.exists('images')) dir.create('images')
##plot absolutos
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(fonte == 'GLAD' | fonte == "PRODES" | fonte == 'MAPBIOMAS'), aes(fill = area_desmatada_ha, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  facet_wrap(~fonte+nm_quadrienio, nrow = 3) + 
  scale_fill_gradientn('Absolute deforestation (ha)', breaks = c(seq(0,500000, by = 100000)),
                       labels = gsub(',', '\\.', format(seq(0,500000, by = 100000), big.mark = ',')),
                       colours = c("gray", 'yellow', 'orange', 'darkred') 
  )+
  theme(legend.position = 'bottom',
        legend.text = element_text(angle = 330,
                                   vjust = -0.01))



ggsave(plot = p1, filename = 'images/absolute_deforestation.png',
       units = 'in', dpi = 300, width = 50, height = 30, scale = 0.25)




##plot absolutos acumulados
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(fonte == 'GLAD' | fonte == "PRODES" | fonte == 'MAPBIOMAS'), aes(fill = area_desmatada_ha_cat, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  facet_wrap(~fonte+nm_quadrienio, nrow = 3) + 
  scale_fill_brewer('Accumulated deforested area', palette ='RdYlGn', direction = 1 )+
  theme(legend.position = 'bottom',
        legend.text = element_text())

ggsave(plot = p1, filename = 'images/accumulated_deforested_area.png',
       units = 'in', dpi = 300, width = 50, height = 30, scale = 0.25)



##plot densidade de desmatamento
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(fonte == 'GLAD' | fonte == "PRODES" | fonte == 'MAPBIOMAS'), aes(fill = densidade_desmatamento, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  facet_wrap(~fonte+nm_quadrienio, nrow = 3) + 
  scale_fill_viridis('Municipal deforestation density\n(deforestation area/\nmunicipalitie area)', option = 'H', direction = 1,  limits = c(0,0.3))+
  theme(legend.position = 'bottom',
        legend.text = element_text())

ggsave(plot = p1, filename = 'images/deforest_density.png',
       units = 'in', dpi = 300, width = 50, height = 30, scale = 0.25)





##plot diferenca absoluta1
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(grepl('dif.PRODES', fonte)) %>%
            mutate(fonte = case_when(
              fonte =='dif.PRODES.MAPBIOMAS'~'|MAPBIOMAS-PRODES|'
            )), aes(fill = area_desmatada_ha, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  scale_fill_gradient(name = "Absolute diff (ha)",  low = "orange", high = "darkgreen",
                      breaks = seq(0, 300000, by = 60000), labels = gsub(',', '\\.', format(seq(0, 300000, by = 60000), big.mark = ',')), na.value="white")+
  facet_wrap(~nm_quadrienio+fonte, nrow = 2) +
  theme(legend.position = 'bottom',
        legend.text = element_text(angle = 330,
                                   vjust = -0.01))

ggsave(plot = p1, filename = 'images/absolute_deforestation_diff.png',
       units = 'in', dpi = 300, width = 30, height = 20, scale = 0.3)


##plot diferenca absoluta1
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(grepl('dif.PRODES', fonte)) %>%
            mutate(fonte = case_when(
              fonte =='dif.PRODES.MAPBIOMAS'~'|MAPBIOMAS-PRODES|'
            )), aes(fill = area_desmatada_ha, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  scale_fill_gradient(name = "Absolute diff (ha)", limits = c(0, 15000), low = "orange", high = "darkgreen",
                      breaks = c(0, 5000, 10000, 15000, 20000), labels = gsub(',', '\\.', format(c(0, 5000, 10000, 15000, 20000), big.mark = ',')), na.value="white")+
  facet_wrap(~nm_quadrienio+fonte, nrow = 2) +
  theme(legend.position = 'bottom',
        legend.text = element_text(angle = 330,
                                   vjust = -0.01))

ggsave(plot = p1, filename = 'images/absolute_deforestation_diff2.png',
       units = 'in', dpi = 300, width = 30, height = 20, scale = 0.3)





#plot diferenca acumulada categorizada
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(grepl('dif.PRODES', fonte)), aes(fill = area_desmatada_ha_cat, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  scale_fill_brewer('Accumulated diff', palette = 'RdYlBu', direction = 1)+
  facet_wrap(~nm_quadrienio+fonte, nrow = 2) +
  theme(legend.position = 'bottom',
        legend.text = element_text())

ggsave(plot = p1, filename = 'images/accumulated_diff.png',
       units = 'in', dpi = 300, width = 20, height = 20, scale = 0.3)







##diff to the oficial data
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(grepl('dif.rel', fonte)) %>%
            mutate(fonte = case_when(
              fonte =='dif.rel.PRODES.MAPBIOMAS'~'MAPBIOMAS/PRODES'
            )), aes(fill = log(area_desmatada_ha-min(area_desmatada_ha, na.rm=T)/max(area_desmatada_ha, na.rm=T ) - min(area_desmatada_ha, na.rm=T )), geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  scale_fill_viridis('MAPBIOMAS deviation from PRODES\nlog(rDeviation - (min(rDeviation)-1)',option='magma')+
  #scale_fill_brewer('MAPBIOMAS deviation\nfrom PRODES', palette = 'RdYlBu')+
  facet_wrap(~nm_quadrienio+fonte, nrow =2) +
  theme(legend.position = 'bottom',
        legend.text = element_text())

ggsave(plot = p1, filename = 'images/mapbiomas_deviation_from_prodes.png',
       units = 'in', dpi = 300, width = 30, height = 20, scale = 0.3)






