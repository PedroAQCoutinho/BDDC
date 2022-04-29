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
base.compilada <- read.csv2('D:/pedro/DESMATAMENTO/dados/tabulares/base_compilada_27042022.csv')
#load
br.uf <- read.csv('D:/tabelas_base/chave_IBGE.csv') %>%
  dplyr::select(CD_GEOCODI, NM_ESTADO, UF)
#load
br.mun <- st_read('D:/dados_GPP/geo_adm/municipios/BR_Municipios_2021.shp')
br.mun$CD_MUN <- as.numeric(br.mun$CD_MUN)
#load
br.estados <- st_read('D:/dados_GPP/geo_adm/uf/BR_UF_2021.shp')


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


50/(50+70)


out <- base.compilada %>% 
  filter(ano >= 2001 & ano <= 2019) %>% #filtra apenas para anos de interesse
  filter(grepl('GLAD|MAPBIOMAS|PRODES',fonte)) %>% #filtra apenas projetos de interesse
  mutate(quadrienio = case_when( #cria labels para o 4 quadrienios a serem estudados
    ano >= 2001 & ano < 2005 ~ 1,
    ano >= 2005 & ano < 2009 ~ 2,
    ano >= 2009 & ano < 2013 ~ 3,
    ano >= 2013 & ano < 2017 ~ 4,
    ano >= 2017 & ano < 2020 ~ 5),
    area_desmatada_ha = as.numeric(area_desmatada_ha), 
    quadrienio = as.character(quadrienio)) %>% 
    group_by(cd_mun, fonte, quadrienio) %>%
    summarise(area_desmatada_ha = sum(area_desmatada_ha, na.rm=T)) %>% #agrega os dados por quadrienio+projeto pela soma 
    pivot_wider(names_from = c('fonte','quadrienio'), values_from = 'area_desmatada_ha') %>% #transforma dados em wide para operação de colunas
    left_join(mun.prodes ,by = c('cd_mun'='cd_mun')) %>% #tras os municipios do prodes para filtragem
    filter(check) %>% as_tibble %>% #filtra
    dplyr::select(-check) %>%
    mutate(
    #criação de colunas com diferença absoluta
    
    #quadrienio 1
    #dif_GLAD_MAPBIOMAS_1 = abs(GLAD_1-MAPBIOMAS_1),
    #dif_GLAD_PRODES_1 = abs(GLAD_1-PRODES_1),
    dif.PRODES.MAPBIOMAS_1 = abs(PRODES_1-MAPBIOMAS_1),
    #quadrienio 2
    #dif_GLAD_MAPBIOMAS_2 = abs(GLAD_2-MAPBIOMAS_2),
    #dif_GLAD_PRODES_2 = abs(GLAD_2-PRODES_2),
    dif.PRODES.MAPBIOMAS_2 = abs(PRODES_2-MAPBIOMAS_2),
    #quadrienio 3
    #dif_GLAD_MAPBIOMAS_3 = abs(GLAD_3-MAPBIOMAS_3),
    #dif_GLAD_PRODES_3 = abs(GLAD_3-PRODES_3),
    dif.PRODES.MAPBIOMAS_3 = abs(PRODES_3-MAPBIOMAS_3),
    #quadrienio 4
    #dif_GLAD_MAPBIOMAS_4 = abs(GLAD_4-MAPBIOMAS_4),
    #dif_GLAD_PRODES_4 = abs(GLAD_4-PRODES_4),
    dif.PRODES.MAPBIOMAS_4 = abs(PRODES_4-MAPBIOMAS_4),
    #quadrienio 5
    #dif_GLAD_MAPBIOMAS_5 = abs(GLAD_5-MAPBIOMAS_5),
    #dif_GLAD_PRODES_5 = abs(GLAD_5-PRODES_%),
    dif.PRODES.MAPBIOMAS_5 = abs(PRODES_5-MAPBIOMAS_5)    ) %>%
  mutate(
    #GLAD.MAPBIOMAS_1 = (GLAD_1-MAPBIOMAS_1)/mean(c(GLAD_1,MAPBIOMAS_1)),
    #GLAD.PRODES_1 = (GLAD_1-PRODES_1)/mean(c(GLAD_1, PRODES_1)),
    dif.rel.PRODES.MAPBIOMAS_1 = (ifelse(PRODES_1 == 0, 0, MAPBIOMAS_1/PRODES_1)),
    #quadrienio 2
    #GLAD.MAPBIOMAS_2 = (GLAD_2-MAPBIOMAS_2)/mean(c(GLAD_2,MAPBIOMAS_2)),
    #GLAD.PRODES_2 = (GLAD_2-PRODES_2)/mean(c(GLAD_2, PRODES_2)),
    dif.rel.PRODES.MAPBIOMAS_2 = (ifelse(PRODES_2 == 0, 0, MAPBIOMAS_2/PRODES_2)),
    #quadrienio 3
    #GLAD.MAPBIOMAS_3 = (GLAD_3-MAPBIOMAS_3)/mean(c(GLAD_3,MAPBIOMAS_3)),
    #GLAD.PRODES_3 = (GLAD_3-PRODES_3)/mean(c(GLAD_3, PRODES_3)),
    dif.rel.PRODES.MAPBIOMAS_3 = (ifelse(PRODES_3 == 0, 0, MAPBIOMAS_3/PRODES_3)),
    #quadrienio 4
    #GLAD.MAPBIOMAS_4 = (GLAD_4-MAPBIOMAS_4)/mean(c(GLAD_4,MAPBIOMAS_4)),
    #GLAD.PRODES_4 = (GLAD_4-PRODES_4)/mean(c(GLAD_4, PRODES_4)),
    dif.rel.PRODES.MAPBIOMAS_4 = (ifelse(PRODES_4 == 0, 0, MAPBIOMAS_4/PRODES_4)),
    #quadrienio 5
   #dif_GLAD_MAPBIOMAS_5 = abs(GLAD_5-MAPBIOMAS_5),
   #dif_GLAD_PRODES_5 = abs(GLAD_5-PRODES_%),
   dif.rel.PRODES.MAPBIOMAS_5 = (ifelse(PRODES_5 == 0, 0, MAPBIOMAS_5/PRODES_5))
   ) %>%
    pivot_longer(names_to = c('fonte','quadrienio'), names_sep = '_', #volta para longer para fazer os acumulados
               values_to = 'area_desmatada_ha', cols = 2:26) %>%
    mutate(nm_quadrienio = case_when(quadrienio == '1'~ '2001 - 2004',
                                   quadrienio == '2'~ '2005 - 2008',
                                   quadrienio == '3'~ '2009 - 2012',
                                   quadrienio == '4'~ '2013 - 2016',
                                   quadrienio == '5'~ '2017 - 2019')) %>%
  left_join(br.mun[,c('CD_MUN','AREA_KM2','geometry')], by = c('cd_mun'='CD_MUN')) %>% #puxa a area municipal e geometria
  mutate(AREA_HA = AREA_KM2*100,
         densidade_desmatamento = area_desmatada_ha/AREA_HA) %>% #densidade de desmatamento
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

##plot absolutos
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(fonte == 'GLAD' | fonte == "PRODES" | fonte == 'MAPBIOMAS'), aes(fill = area_desmatada_ha, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  facet_wrap(~fonte+nm_quadrienio, nrow = 3) + 
  scale_fill_gradientn('Área desmatada absoluta (ha)', breaks = c(seq(0,500000, by = 100000)),
                       labels = gsub(',', '\\.', format(seq(0,500000, by = 100000), big.mark = ',')),
                       colours = c("darkred", "orange", "yellow", "white") 
  )+
  theme(legend.position = 'bottom',
        legend.text = element_text(angle = 330,
                                   vjust = -0.01))



ggsave(plot = p1, filename = 'D:/pedro/DESMATAMENTO/dados/tabulares/desmatamento_absoluto2.png',
       units = 'in', dpi = 300, width = 50, height = 30, scale = 0.25)




##plot absolutos acumulados
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(fonte == 'GLAD' | fonte == "PRODES" | fonte == 'MAPBIOMAS'), aes(fill = area_desmatada_ha_cat, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  facet_wrap(~fonte+nm_quadrienio, nrow = 3) + 
  scale_fill_brewer('Área desmatada acumulada', palette ='RdYlGn', direction = 1 )+
  theme(legend.position = 'bottom',
        legend.text = element_text())

ggsave(plot = p1, filename = 'D:/pedro/DESMATAMENTO/dados/tabulares/desmatamento_acumulado_absoluto2.png',
       units = 'in', dpi = 300, width = 50, height = 30, scale = 0.25)



##plot densidade de desmatamento
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(fonte == 'GLAD' | fonte == "PRODES" | fonte == 'MAPBIOMAS'), aes(fill = densidade_desmatamento, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  facet_wrap(~fonte+nm_quadrienio, nrow = 3) + 
  scale_fill_viridis('Densidade de desmatamento\n(área desmatada/\nárea total do municipio)', option = 'H', direction = 1,  limits = c(0,0.3))+
  theme(legend.position = 'bottom',
        legend.text = element_text())

ggsave(plot = p1, filename = 'D:/pedro/DESMATAMENTO/dados/tabulares/densidade_desmatamento.png',
       units = 'in', dpi = 300, width = 50, height = 30, scale = 0.25)





##plot diferenca absoluta1
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(grepl('dif.PRODES', fonte)) %>%
            mutate(fonte = case_when(
              fonte =='dif.PRODES.MAPBIOMAS'~'|MAPBIOMAS-PRODES|'
            )), aes(fill = area_desmatada_ha, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  scale_fill_gradient(name = "Diferença absoluta",  low = "orange", high = "darkgreen",
                      breaks = seq(0, 300000, by = 60000), labels = gsub(',', '\\.', format(seq(0, 300000, by = 60000), big.mark = ',')), na.value="white")+
  facet_wrap(~nm_quadrienio+fonte, nrow = 2) +
  theme(legend.position = 'bottom',
        legend.text = element_text(angle = 330,
                                   vjust = -0.01))

ggsave(plot = p1, filename = 'D:/pedro/DESMATAMENTO/dados/tabulares/diferenca_desmatamento_absoluto.png',
       units = 'in', dpi = 300, width = 30, height = 20, scale = 0.3)


##plot diferenca absoluta1
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(grepl('dif.PRODES', fonte)) %>%
            mutate(fonte = case_when(
              fonte =='dif.PRODES.MAPBIOMAS'~'|MAPBIOMAS-PRODES|'
            )), aes(fill = area_desmatada_ha, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  scale_fill_gradient(name = "Diferença absoluta", limits = c(0, 15000), low = "orange", high = "darkgreen",
                      breaks = c(0, 5000, 10000, 15000, 20000), labels = gsub(',', '\\.', format(c(0, 5000, 10000, 15000, 20000), big.mark = ',')), na.value="white")+
  facet_wrap(~nm_quadrienio+fonte, nrow = 2) +
  theme(legend.position = 'bottom',
        legend.text = element_text(angle = 330,
                                   vjust = -0.01))

ggsave(plot = p1, filename = 'D:/pedro/DESMATAMENTO/dados/tabulares/diferenca_desmatamento_absoluto3.png',
       units = 'in', dpi = 300, width = 30, height = 20, scale = 0.3)





#plot diferenca acumulada categorizada
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(grepl('dif.PRODES', fonte)), aes(fill = area_desmatada_ha_cat, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  scale_fill_brewer('Diferença acumulada', palette = 'RdYlBu', direction = 1)+
  facet_wrap(~nm_quadrienio+fonte, nrow = 2) +
  theme(legend.position = 'bottom',
        legend.text = element_text())

ggsave(plot = p1, filename = 'D:/pedro/DESMATAMENTO/dados/tabulares/dif_abs_acumulada_desmatamento2.png',
       units = 'in', dpi = 300, width = 20, height = 20, scale = 0.3)







##diferenca relativa a media
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = out %>% filter(grepl('dif.rel', fonte)) %>%
            mutate(fonte = case_when(
              fonte =='dif.rel.PRODES.MAPBIOMAS'~'MAPBIOMAS/PRODES'
            )), aes(fill = (area_desmatada_ha), geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  scale_fill_gradient2('MAPBIOMAS/PRODES', low = 'red', mid = 'white', high = 'blue')+
  facet_wrap(~nm_quadrienio+fonte, nrow =2) +
  theme(legend.position = 'bottom',
        legend.text = element_text())

ggsave(plot = p1, filename = 'D:/pedro/DESMATAMENTO/dados/tabulares/dif_desmatamento_relativo2.png',
       units = 'in', dpi = 300, width = 30, height = 20, scale = 0.3)


log(0.000000005)
log(100)
log(10000)
log(10000000)
log(10000000000)


##

a <- out %>% filter(grepl('dif.rel', fonte)) %>% dplyr::select(-geometry) %>%
  group_by(cd_mun, quadrienio) %>% tally()





















#desmatamento absoluto
desmatamento.absoluto <- base.compilada %>% 
  filter(ano > 2001 & ano <= 2017) %>%
  filter(grepl('GLAD|MAPBIOMAS|PRODES',fonte)) %>%
  mutate(quadrienio = case_when(
    ano > 2001 & ano < 2006 ~ 1,
    ano >= 2006 & ano < 2010 ~ 2,
    ano >= 2010 & ano < 2014 ~ 3,
    ano >= 2014 & ano < 2018 ~ 4)) %>%
  mutate(area_desmatada_ha = as.numeric(area_desmatada_ha),
         quadrienio = as.character(quadrienio)) %>%
  group_by(cd_mun, fonte, quadrienio) %>%
  summarise(area_desmatada_ha = sum(area_desmatada_ha, na.rm=T)) %>% 
  pivot_wider(names_from = c('fonte','quadrienio'), values_from = 'area_desmatada_ha') %>%
  left_join(mun.prodes ,by = c('cd_mun'='cd_mun')) %>%
  filter(check) %>% as_tibble %>%
  left_join(br.uf, by = c('cd_mun'='CD_GEOCODI')) %>%
  dplyr::select(-UF, -check) %>%
  left_join(br.mun[,c('CD_MUN','geometry')], by = c('cd_mun'='CD_MUN')) %>%
  pivot_longer(names_to = c('fonte','quadrienio'), names_sep = '_',
               values_to = 'area_desmatada_ha', cols = 2:13) %>% as_tibble() %>%
  mutate(nm_quadrienio = case_when(quadrienio == '1'~ '2002 - 2005',
                                   quadrienio == '2'~ '2006 - 2009',
                                   quadrienio == '3'~ '2010 - 2013',
                                   quadrienio == '4'~ '2014 - 2017'))






#selecao apenas para anos e municipios comuns às tres bases. Tirando fora 2001 e PMDBBS 
base.wider <- base.compilada %>% 
  filter(ano > 2001 & ano <= 2017) %>%
  filter(grepl('GLAD|MAPBIOMAS|PRODES',fonte)) %>%
  mutate(quadrienio = case_when(
    ano > 2001 & ano < 2006 ~ 1,
    ano >= 2006 & ano < 2010 ~ 2,
    ano >= 2010 & ano < 2014 ~ 3,
    ano >= 2014 & ano < 2018 ~ 4)) %>%
  mutate(area_desmatada_ha = as.numeric(area_desmatada_ha),
         quadrienio = as.character(quadrienio)) %>%
  group_by(cd_mun, fonte, quadrienio) %>%
  summarise(area_desmatada_ha = sum(area_desmatada_ha, na.rm=T)) %>%
  pivot_wider(names_from = c('fonte','quadrienio'), values_from = 'area_desmatada_ha') %>%
  left_join(mun.prodes ,by = c('cd_mun'='cd_mun')) %>%
  filter(check) %>% as_tibble %>%
  left_join(br.uf, by = c('cd_mun'='CD_GEOCODI')) %>%
  dplyr::select(-UF, -check)
#replace NAs
base.wider[is.na(base.wider)] <- 0




#longer para plot de valores absolutos
base.longer.absoluto <- base.wider %>%
  left_join(br.mun[,c('cod','geometry')], by = c('cd_mun'='cod')) %>%
  pivot_longer(names_to = c('fonte','quadrienio'), names_sep = '_',
               values_to = 'area_desmatada_ha', cols = 2:(ncol(base.wider)-1)) %>% as_tibble() %>%
  arrange(quadrienio, fonte,desc(area_desmatada_ha)) %>%
  group_by(quadrienio, fonte) %>%
  mutate(acumulado = cumsum(area_desmatada_ha),
         total = sum(area_desmatada_ha),
         rAcumulado = acumulado/total,
         nm_quadrienio = case_when(quadrienio == '1'~ '2002 - 2005',
                                   quadrienio == '2'~ '2006 - 2009',
                                   quadrienio == '3'~ '2010 - 2013',
                                   quadrienio == '4'~ '2014 - 2017'),
         area_desmatada_ha_cat = case_when(rAcumulado < 0.25 ~ '0 - 25%',
                                           (rAcumulado >=0.25 & rAcumulado< 0.5) ~ '25 - 50%',
                                           (rAcumulado >=0.5 & rAcumulado < 0.75) ~ '50 - 75%',
                                           rAcumulado >= 0.75 ~ '75 - 100% ')) %>%
  
  dplyr::select(cd_mun, NM_ESTADO, fonte, quadrienio, area_desmatada_ha,area_desmatada_ha_cat, 
                acumulado, total, rAcumulado, nm_quadrienio, geometry) %>% st_as_sf() %>% as_tibble()


##plot absolutos
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = base.longer.absoluto, aes(fill = area_desmatada_ha_cat, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  facet_wrap(~nm_quadrienio+fonte, nrow = 4) + 
  scale_fill_brewer('Área desmatada acumulada', palette ='RdYlGn', direction = 1 )+
  theme(legend.position = 'bottom',
        legend.text = element_text())

ggsave(plot = p1, filename = 'D:/pedro/DESMATAMENTO/dados/tabulares/desmatamento_acumulado_absoluto.png',
       units = 'in', dpi = 300, width = 45, height = 49, scale = 0.25)




fscale <- 0.35
#diference
base.wider.diferenca <- base.wider %>%
  mutate(#quadrienio 1
    GLAD.MAPBIOMAS_1 = abs(GLAD_1-MAPBIOMAS_1),
    GLAD.PRODES_1 = abs(GLAD_1-PRODES_1),
    PRODES.MAPBIOMAS_1 = abs(PRODES_1-MAPBIOMAS_1),
    #quadrienio 2
    GLAD.MAPBIOMAS_2 = abs(GLAD_2-MAPBIOMAS_2),
    GLAD.PRODES_2 = abs(GLAD_2-PRODES_2),
    PRODES.MAPBIOMAS_2 = abs(PRODES_2-MAPBIOMAS_2),
    #quadrienio 3
    GLAD.MAPBIOMAS_3 = abs(GLAD_3-MAPBIOMAS_3),
    GLAD.PRODES_3 = abs(GLAD_3-PRODES_3),
    PRODES.MAPBIOMAS_3 = abs(PRODES_3-MAPBIOMAS_3),
    #quadrienio 4
    GLAD.MAPBIOMAS_4 = abs(GLAD_4-MAPBIOMAS_4),
    GLAD.PRODES_4 = abs(GLAD_4-PRODES_4),
    PRODES.MAPBIOMAS_4 = abs(PRODES_4-MAPBIOMAS_4)
  ) %>%
  dplyr::select(cd_mun, NM_ESTADO, contains('PRODES.MAPBIOMAS')) 

#replace
base.wider.diferenca[is.na(base.wider.diferenca)] <- 0

#longer
base.longer.diferenca <- base.wider.diferenca %>%
  left_join(br.mun[,c('cod','geometry')], by = c('cd_mun'='cod')) %>%
  pivot_longer(names_to = c('fonte','quadrienio'), names_sep = '_',
               values_to = 'diferenca_abs_desmatamento', cols = 3:(ncol(base.wider.diferenca))) %>% as_tibble() %>%
  arrange(quadrienio,  fonte,desc(diferenca_abs_desmatamento)) %>%
  group_by(quadrienio, fonte) %>%
  mutate(acumulado = cumsum(diferenca_abs_desmatamento),
         total = sum(diferenca_abs_desmatamento),
         rAcumulado = acumulado/total,
         nm_quadrienio = case_when(quadrienio == '1'~ '2002 - 2005',
                                   quadrienio == '2'~ '2006 - 2009',
                                   quadrienio == '3'~ '2010 - 2013',
                                   quadrienio == '4'~ '2014 - 2017'),
         diferenca_abs_desmatamento_cat = case_when(rAcumulado < 0.25 ~ '0 - 25%',
                                                    (rAcumulado >=0.25 & rAcumulado< 0.5) ~ '25 - 50%',
                                                    (rAcumulado >=0.5 & rAcumulado < 0.75) ~ '50 - 75%',
                                                    rAcumulado >= 0.75 ~ '75 - 100% ')) %>%
  filter(rAcumulado < 1) %>%
  dplyr::select(cd_mun, NM_ESTADO, fonte, quadrienio, diferenca_abs_desmatamento, diferenca_abs_desmatamento_cat, acumulado, total, rAcumulado, nm_quadrienio,geometry) 



##plot absolutos
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = base.longer.diferenca, aes(fill = diferenca_abs_desmatamento_cat, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  scale_fill_brewer('Diferença acumulada', palette = 'RdYlBu', direction = 1)+
  facet_wrap(~nm_quadrienio+fonte, nrow = 2) +
  theme(legend.position = 'bottom',
        legend.text = element_text())

ggsave(plot = p1, filename = 'D:/pedro/DESMATAMENTO/dados/tabulares/dif_abs_acumulada_desmatamento.png',
       units = 'in', dpi = 300, width = 40, height = 40, scale = fscale)



##plot absolutos
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = base.longer.diferenca, aes(fill = diferenca_abs_desmatamento, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  scale_fill_gradient(name = "Diferença absoluta", limits = c(0, 75000), low = "orange", high = "darkgreen",
                      breaks = c(0, 20000, 40000, 60000), labels = gsub(',', '\\.', format(c(0, 20000, 40000, 60000), big.mark = ',')))+
  facet_wrap(~nm_quadrienio+fonte, nrow = 2) +
  theme(legend.position = 'bottom',
        legend.text = element_text(angle = 330,
                                   vjust = -0.01))

ggsave(plot = p1, filename = 'D:/pedro/DESMATAMENTO/dados/tabulares/diferenca_desmatamento_absoluto.png',
       units = 'in', dpi = 300, width = 30, height = 30, scale = fscale)

#diferenca relativa à media

base.wider.diferenca.relativa <- base.wider %>%
  mutate(#quadrienio 1
    GLAD.MAPBIOMAS_1 = (GLAD_1-MAPBIOMAS_1)/mean(c(GLAD_1,MAPBIOMAS_1)),
    GLAD.PRODES_1 = (GLAD_1-PRODES_1)/mean(c(GLAD_1, PRODES_1)),
    PRODES.MAPBIOMAS_1 = (PRODES_1-MAPBIOMAS_1)/mean(c(PRODES_1, MAPBIOMAS_1)),
    #quadrienio 2
    GLAD.MAPBIOMAS_2 = (GLAD_2-MAPBIOMAS_2)/mean(c(GLAD_2,MAPBIOMAS_2)),
    GLAD.PRODES_2 = (GLAD_2-PRODES_2)/mean(c(GLAD_2, PRODES_2)),
    PRODES.MAPBIOMAS_2 = (PRODES_2-MAPBIOMAS_2)/mean(c(PRODES_2, MAPBIOMAS_2)),
    #quadrienio 3
    GLAD.MAPBIOMAS_3 = (GLAD_3-MAPBIOMAS_3)/mean(c(GLAD_3,MAPBIOMAS_3)),
    GLAD.PRODES_3 = (GLAD_3-PRODES_3)/mean(c(GLAD_3, PRODES_3)),
    PRODES.MAPBIOMAS_3 = (PRODES_3-MAPBIOMAS_3)/mean(c(PRODES_3, MAPBIOMAS_3)),
    #quadrienio 4
    GLAD.MAPBIOMAS_4 = (GLAD_4-MAPBIOMAS_4)/mean(c(GLAD_4,MAPBIOMAS_4)),
    GLAD.PRODES_4 = (GLAD_4-PRODES_4)/mean(c(GLAD_4, PRODES_4)),
    PRODES.MAPBIOMAS_4 = (PRODES_4-MAPBIOMAS_4)/mean(c(PRODES_4, MAPBIOMAS_4)),
  ) %>%
  dplyr::select(cd_mun, NM_ESTADO, contains('PRODES.MAPBIOMAS')) 






dt <- data.frame(a = c(1,1,1,1,1),
                 b = c(4,5,5,5,5),
                 c = c(2,2,4,4,4))

dt %>%
  group_by(c) %>%
  mutate(med = mean(c(a, b)),
         d = abs(a+b)/mean(c(a, b)),
         med2= ((a+b)/2),
         e = abs(a+b)/((a+b)/2))
  







#replace
base.wider.diferenca.relativa[is.na(base.wider.diferenca.relativa)] <- 0

#longer
base.longer.diferenca <- base.wider.diferenca.relativa %>%
  left_join(br.mun[,c('cod','geometry')], by = c('cd_mun'='cod')) %>%
  pivot_longer(names_to = c('fonte','quadrienio'), names_sep = '_',
               values_to = 'diferenca_rel_desmatamento', cols = 3:(ncol(base.wider.diferenca.relativa))) %>% as_tibble() %>%
  arrange(quadrienio,  fonte,desc(diferenca_rel_desmatamento)) %>%
  group_by(quadrienio, fonte) %>%
  mutate(acumulado = cumsum(diferenca_rel_desmatamento),
         total = sum(diferenca_rel_desmatamento),
         rAcumulado = acumulado/total,
         nm_quadrienio = case_when(quadrienio == '1'~ '2002 - 2005',
                                   quadrienio == '2'~ '2006 - 2009',
                                   quadrienio == '3'~ '2010 - 2013',
                                   quadrienio == '4'~ '2014 - 2017')) %>%
  filter(rAcumulado < 1) %>%
  dplyr::select(cd_mun, NM_ESTADO, fonte, quadrienio, diferenca_rel_desmatamento, acumulado, total, rAcumulado, nm_quadrienio,geometry) 




##plot absolutos
p1 <- ggplot()+
  theme_bw()+
  geom_sf(data = base.longer.diferenca, aes(fill = diferenca_rel_desmatamento, geometry = geometry), color = NA)+
  geom_sf(data = br.estados, fill = NA)+
  scale_fill_gradient2('Diferença relativa à\nmedia (%)')+
  facet_wrap(~nm_quadrienio+fonte, nrow =2) +
  theme(legend.position = 'bottom',
        legend.text = element_text())

ggsave(plot = p1, filename = 'D:/pedro/DESMATAMENTO/dados/tabulares/dif_desmatamento_relativo.png',
       units = 'in', dpi = 300, width = 30, height = 30, scale = fscale)
