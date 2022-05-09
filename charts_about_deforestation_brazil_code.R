###############################################################
# Comparisons between different sources of Brazil deforestation data
# Pietro Granolati Fernandes
# March, 2022
###################################################

# Cleaning the environment
rm(list = ls())

# Installing packages and reading data----
library(pacman)
p_load(data.table, ggplot2, dplyr, ggpubr, forcats, reshape2, repr)
dados <- fread("data/municipal_deforestation_multiple_sources.csv")
atributos_municipios <- fread("data/municipal_geo_aggregations.csv")
colnames(atributos_municipios)[1] <- "cd_mun"

dados <- dados %>%
  group_by(ano, cd_mun, fonte)%>%
  summarise(area_desmatada_ha = sum(area_desmatada_ha))

dados <- merge(dados, atributos_municipios, by = "cd_mun", allow.cartesian = T)

# Checking the number of observations by municipality (max = 21)----
ndados <- dados%>%
  group_by(cd_mun, fonte)%>%
  summarise(n = n())

# Removing PMDBBS data----
dados <- subset(dados, fonte != "PMDBBS")
dados$ano <- as.numeric(dados$ano)

# Grouping data at different spatial scales----
# Brazil
brasil <- dados %>%
  group_by(ano, fonte)%>%
  summarise(area_desm_ha = sum(area_desmatada_ha),
            area_desm_1000ha = area_desm_ha/1000)


# State 
estado <- dados %>%
  group_by(SIGLA_UF, ano, fonte)%>%
  summarise(area_desm_ha = sum(area_desmatada_ha),
            area_desm_1000ha = area_desm_ha/1000)

# Biome
bioma <- dados %>%
  group_by(Bioma, ano, fonte)%>%
  summarise(area_desm_ha = sum(area_desmatada_ha),
            area_desm_1000ha = area_desm_ha/1000)


# Deforestation totals
totais <- dados %>%
  group_by(fonte)%>%
  summarise(area_desm_1000ha = sum(area_desmatada_ha)/1000)


# Charts----
# Deforestation totals----
jpeg("images/desmatamento_totais.jpg", width = 1000, height = 500,)
p1 <- ggplot(totais, aes(fct_reorder(fonte, -area_desm_1000ha), area_desm_1000ha))+
  geom_col(position = "dodge", width = 0.1, fill= "chartreuse4")+
  scale_y_continuous(limits = c(0,100000), breaks = rep(seq(0,100000,25000)))+
  labs(x = "Fonte",
       y = "?rea desmatada (1000ha)",
       title =  "Compara??o entre as fontes de dados de desmatamento",
       subtitle = "Total acumulado dispon?vel de cada fonte entre 2000-2021")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.background = element_blank(),
        axis.line = element_line());p1

dev.off()

# Annual deforestation----
jpeg("images/desmatamento_anual.jpg", width = 3000, height = 1500, res = 300)
p2 <- ggplot(brasil, aes(ano, area_desm_1000ha, fill = fonte))+
  geom_col(position = "dodge", width = 0.7)+
  scale_y_continuous(limits = c(0,10000), breaks = rep(seq(0,10000,1000)))+
  scale_x_continuous(limits = c(1999,2022),breaks = rep(seq(2000,2021,1)))+
  labs(x = "Ano",
       y = "?rea desmatada (1000ha)",
       title =  "Compara??o entre as fontes de dados de desmatamento",
       fill = "Fonte")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 14, face = "bold", angle = 315),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line());p2
dev.off()

# Deforestation by biome----
# Filtering the dataset into biomes with large deforested areas (Cerrado and Amazon), and biomes with less deforestation (Atlantic Forest, Pampas, Pantanal and Caatinga)
bioma_CEAM <- filter(bioma, Bioma %in% c("Cerrado", "Amaz?nia"))
bioma_resto <- filter(bioma, Bioma %in% c("Pampa", "Pantanal", "Mata Atl?ntica",
                                          "Caatinga"))

jpeg("images/desmatamento_bioma_am_cer.jpg", width = 3000, height = 1500, res = 300)
p4 <- ggplot(bioma_CEAM, aes(ano, area_desm_1000ha, fill = fonte))+
  geom_col(position = "dodge", width = 0.8)+
  scale_y_continuous(limits = c(0,6000), breaks = rep(seq(0,6000,1000)))+
  scale_x_continuous(limits = c(1999,2022),breaks = rep(seq(2000,2021,1)))+
  facet_wrap(~Bioma)+
  labs(x = "Ano",
       y = "?rea desmatada (1000ha)",
       title =  "Compara??o entre as fontes de dados de desmatamento",
       fill = "Fonte")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = "bold", angle = 270),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(),
        strip.text = element_text(size = 14));p4
dev.off()

jpeg("images/desmatamento_bioma_resto.jpg", width = 3000, height = 1500, res = 300)
p4 <- ggplot(bioma_resto, aes(ano, area_desm_1000ha, fill = fonte))+
  geom_col(position = "dodge", width = 0.7)+
  scale_y_continuous(limits = c(0,1000), breaks = rep(seq(0,1000,200)))+
  scale_x_continuous(limits = c(1999,2022),breaks = rep(seq(2000,2021,1)))+
  facet_wrap(~Bioma, )+
  labs(x = "Ano",
       y = "?rea desmatada (1000ha)",
       title =  "Compara??o entre as fontes de dados de desmatamento",
       fill = "Fonte")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, face = "bold", angle = 270),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(),
        strip.text = element_text(size = 14));p4
dev.off()

# Correlation between sources ----
dados$area_1000 <- dados$area_desmatada_ha/1000
prodes <- subset(dados, fonte == "PRODES")
glad <- subset(dados, fonte == "GLAD")
mapbiomas <- subset(dados, fonte == "MAPBIOMAS")

#GLAD x PRODES
wide <- merge(glad, prodes, by = c("ano", "cd_mun"))

jpeg("images/prodes_glad.jpg", width = 3500, height = 1500,res = 300)
p7 <- ggplot(wide, aes(area_1000.x, area_1000.y, col = Bioma.x))+
  geom_point(alpha = 0.7)+
  geom_abline()+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  scale_y_continuous(limits = c(0,300), breaks = rep(seq(0,300,50)))+
  scale_x_continuous(limits = c(0,300), breaks = rep(seq(0,300,50)))+
  labs(x = "GLAD",
       y = "PRODES",
       title =  "Correla??o entre as ?reas desmatadas (1000ha) por munic?pio - PRODES x GLAD",
       col = "Bioma")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        axis.line = element_line(),
        strip.text = element_text(size = 14));p7
dev.off()

# PRODES x MAPBIOMAS
wide <- merge(prodes, mapbiomas, by = c("ano", "cd_mun"))

jpeg("images/prodes_mb.jpg", width = 3500, height = 1500, res = 300)
p7 <- ggplot(wide, aes(area_1000.x, area_1000.y, col = Bioma.x))+
  geom_point(alpha = 0.7)+
  geom_abline()+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  scale_y_continuous(limits = c(0,200), breaks = rep(seq(0,200,50)))+
  scale_x_continuous(limits = c(0,200), breaks = rep(seq(0,200,50)))+
  labs(x = "PRODES",
       y = "MAPBIOMAS",
       title =  "Correla??o entre as ?reas desmatadas (1000ha) por munic?pio - PRODES x MAPBIOMAS",
       col = "Bioma")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        axis.line = element_line(),
        strip.text = element_text(size = 14));p7
dev.off()

# GLAD x MAPBIOMAS
wide <- merge(glad, mapbiomas, by = c("ano", "cd_mun"))

jpeg("images/glad_mb.jpg", width = 3500, height = 1500, res = 300)
p7 <- ggplot(wide, aes(area_1000.x, area_1000.y, col = Bioma.x))+
  geom_point(alpha = 0.7)+
  geom_abline()+
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson")+
  scale_y_continuous(limits = c(0,250), breaks = rep(seq(0,250,50)))+
  scale_x_continuous(limits = c(0,250), breaks = rep(seq(0,250,50)))+
  labs(x = "GLAD",
       y = "MAPBIOMAS",
       title =  "Correla??o entre as ?reas desmatadas (1000ha) por munic?pio - GLAD x MAPBIOMAS",
       col = "Bioma")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        title = element_text(size = 16),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        axis.line = element_line(),
        strip.text = element_text(size = 14));p7
dev.off()

