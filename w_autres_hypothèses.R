# Library ----
library(tidyverse)
library(sf)
library(units)
library(mapview)
library(dplyr)
library(kableExtra)
library(png)
library(rstatix)
library(ggpubr)
library(UpSetR)
library(ComplexUpset)
library(vioplot) 
library(plotly)

source(file = "R/compter_sommer_surfaces_dans_polygone.R")


# Import des données ----

pe <-
  sf::read_sf(dsn = "data/outputs/pe_qualifies_20240220.gpkg")%>%
  st_transform(crs = 2154)

litho <- sf::read_sf(dsn = "data/carte_lithoplogique_simplifiee_vectorisee_perimetre_etude.gpkg") %>%
  st_transform(crs = 2154)

zone_etude <- sf::read_sf(dsn = "data/zone_etude.gpkg") %>%
  st_transform(crs = 2154) 

bv_me <- sf::read_sf(dsn = "data/outputs/bv_me_qualifie_20240715.gpkg") %>%
  st_transform(crs = 2154) 

departements <- sf::read_sf(dsn = "data/DEPARTEMENT.shp") %>%
  st_transform(crs = 2154)
  
rpg53 <- sf::read_sf(dsn = "data/rpg_53.gpkg") %>%
  st_transform(crs = 2154)

rpg52 <- sf::read_sf(dsn = "data/rpg_52.gpkg") %>%
  st_transform(crs = 2154)

rpg24 <- sf::read_sf(dsn = "data/rpg_24.gpkg") %>%
  st_transform(crs = 2154)

rpg28 <- sf::read_sf(dsn = "data/rpg_28.gpkg") %>%
  st_transform(crs = 2154)

rpg75 <- sf::read_sf(dsn = "data/rpg_75.gpkg") %>%
  st_transform(crs = 2154)

communes <- sf::read_sf(dsn = "data/outputs/communes_20240715.gpkg") %>%
  st_transform(crs = 2154)

her2 <- sf::read_sf(dsn = "data/her2.gpkg") %>%
  st_transform(crs = 2154)

marais_zone_etude <- sf::read_sf(dsn = "data/marais_zone_etude.gpkg") %>%
  st_transform(crs = 2154) 

# Plan d'eau et lithologie ----

## Calcul de densités ----

### Litho de la zone d'étude ----

litho_zone_etude <- litho %>%
  st_intersection(zone_etude) %>%
  mutate(surface_intersect = st_area(.), 
         lithologie = descr) %>%
#  st_drop_geometry() %>%
  group_by(lithologie) %>%
  summarise(surface_km2 = sum(surface_intersect)/1000000) %>%
  mutate(proportion_surface = (surface_km2*100)/sum(surface_km2)) %>%
  units::drop_units()

litho_zone_etude <- litho_zone_etude %>%
  mutate(surface_marais_km2 = surf_marais/1000000) %>%
  select(-surf_marais)

### Calcul associé des densités de PE par type de litho ----

surf_pe_tot_litho <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe %>% 
      units::drop_units() %>%
      st_drop_geometry(),
    var_id_polygone = lithologie,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_litho <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe %>% 
      units::drop_units() %>%
      st_drop_geometry(),
    var_id_polygone = lithologie,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_hors_mares_litho <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe %>% 
      units::drop_units() %>%
      st_drop_geometry() %>%
      filter(mare ==0),
    var_id_polygone = lithologie,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pehm_hors_mares,
    var_somme_surfaces = surf_pehm_hors_mares,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

### Jointure et calcul de densités ----

litho_densite_pe <- litho_zone_etude %>% 
  st_drop_geometry() %>%
  left_join(surf_pe_tot_litho, 
            join_by(lithologie == lithologie)) %>%
  left_join(surf_pehm_litho, 
            join_by(lithologie == lithologie)) %>%
  left_join(surf_pehm_hors_mares_litho, 
            join_by(lithologie == lithologie)) %>%
  mutate(dens_num_pe = nb_pe_tot / surface_km2, 
         dens_surf_pe = ((surf_pe_tot/1000000)*100) / surface_km2,
         dens_num_pehm = nb_pehm_tot / (surface_km2-coalesce(surface_marais_km2, 0)), 
         dens_surf_pehm = ((surf_pehm_tot/1000000)*100) / (surface_km2-coalesce(surface_marais_km2, 0)),
         dens_num_pehm_hors_mares = nb_pehm_hors_mares / (surface_km2-coalesce(surface_marais_km2, 0)), 
         dens_surf_pehm_hors_mares = ((surf_pehm_hors_mares/1000000)*100) / (surface_km2-coalesce(surface_marais_km2, 0)))

litho_densite_pe <- litho_densite_pe %>% 
  mutate(lithologie = as.factor(lithologie),
         lithologie = fct_relevel(lithologie,
                             "Argiles",
                             "Basaltes et rhyolites",
                             "Calcaires, marne et gypse",
                             "Craies",
                             "Gneiss",
                             "Granites",
                             "Micaschistes",
                             "Ophiolites",
                             "Sables",
                             "Schistes et grès"))

sf::write_sf(obj = litho_densite_pe, dsn = "data/outputs/densite_per_lithlogie_20240718.gpkg")

table_litho_densite_pe <- litho_densite_pe %>%
  st_drop_geometry() %>%
  mutate("Lithologie simplifiée" = lithologie,
         "Superficie (km²)" = round(surface_km2),
         "Proportion de surface (%)" = round(proportion_surface, 1),
         "Superficie de marais retenue (km²)" = round(coalesce(surface_marais_km2, 0)),
         "Nombre total de plans d'eau" = nb_pe_tot,
         "Densité numérique en plans d'eau par km²" = round(dens_num_pe, 2),
         "Nombre de plans d'eau hors mares et marais" = nb_pehm_hors_mares,
         "Densité numérique en plans d'eau hors mares et marais par km²" = round(dens_num_pehm_hors_mares, 2),
         ) %>%
  dplyr::select("Lithologie simplifiée", 
         "Superficie (km²)", 
         "Proportion de surface (%)", 
         "Superficie de marais retenue (km²)",
         "Nombre total de plans d'eau",
         "Densité numérique en plans d'eau par km²",
         "Nombre de plans d'eau hors mares et marais",
         "Densité numérique en plans d'eau hors mares et marais par km²")

### Représentations graphiques ----

#### Densite numérique ----

densite_numerique_pe_litho <-
  ggplot(litho_densite_pe,
       aes(y = dens_num_pe,
           x = fct_rev(lithologie))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Lithologie",
       y = "Nombre de plans d'eau par km² de classe lithologique",
       title = "Densité numérique de plans d'eau selon la lithologie\nToutes surfaces élémentaires") +
  coord_flip() +
  theme(axis.title = element_text(size = 9, margin = margin(b = 100)),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) 

ggplotly(densite_numerique_pe_litho) 

densite_numerique_pehm_litho <-
ggplot(litho_densite_pe,
       aes(y = dens_num_pehm,
           x = fct_rev(lithologie))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Lithologie",
       y = "Nombre de plans d'eau par km² de bassin versant de masse d'eau \n(surface de marais déduite)",
       title = "Densité numérique de plans d'eau selon la lithologie\nBretagne et Pays de la Loire élargis (hors marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 9, margin = margin(b = 20)),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) #+
  geom_hline(yintercept = 2.64410797263605, col = "black") +
  geom_hline(yintercept = 3.47558795935624, col = "darkblue") +
  geom_hline(yintercept = 1.66378116445733, col = "lightblue")

densite_numerique_pehm_litho

densite_numerique_pehm_hors_mares_litho <-
  ggplot(litho_densite_pe,
         aes(y = dens_num_pehm_hors_mares,
             x = fct_rev(lithologie))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Lithologie",
       y = "Nombre de plans d'eau par km² de classe lithologique \n(surface de marais déduite)",
       title = "Densité numérique de plans d'eau selon la lithologie\nHors mares et marais") +
  coord_flip() +
  theme(axis.title = element_text(size = 9, margin = margin(b = 20)),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

ggplotly(densite_numerique_pehm_hors_mares_litho)

#### Densite surfacique ----

densite_surfacique_pe_litho <-
  ggplot(litho_densite_pe,
       aes(y = dens_surf_pe,
           x = fct_rev(lithologie))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Lithologie",
       y = "Densité surfacique de plans d'eau (%)",
       title = "Densité surfacique de plans d'eau selon la lithologie\nToutes surfaces élémentaires") +
  coord_flip() +
  theme(axis.title = element_text(size = 9, margin = margin(b = 20)),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

ggplotly(densite_surfacique_pe_litho)

densite_surfacique_pehm_litho <-
  ggplot(litho_densite_pe,
       aes(y = dens_surf_pehm,
           x = fct_rev(lithologie))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Lithologie",
       y = "Densité surfacique de plans d'eau (%) \n(surface de marais déduite)",
       title = "Densité surfacique de plans d'eau selon la lithologie\nBretagne et Pays de la Loire élargis (hors marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 9, margin = margin(b = 20)),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

densite_surfacique_pehm_hors_mares_litho <-
  ggplot(litho_densite_pe,
         aes(y = dens_surf_pehm_hors_mares,
             x = fct_rev(lithologie))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Lithologie",
       y = "Densité surfacique de plans d'eau (%) \n(surface de marais déduite)",
       title = "Densité surfacique de plans d'eau selon la lithologie\nHors mares et marais") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

## Taille des plans d'eau par rapport à la lithologie ---- 

pe_taille <- pe %>%
  filter(zone_marais== 0 & mare == 0 ) %>%
  st_drop_geometry() %>%
  group_by(lithologie) %>%
  summarise(surface_moy_pe = mean(surface_m2),
            surface_med_pe = median(surface_m2))

surface_moyenne_pehm_litho <-
  ggplot(pe_taille,
       aes(y = surface_moy_pe,
           x = fct_rev(lithologie))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Lithologie",
       y = "Superficie moyenne des plans d'eau (m²)",
       title = "Superficie moyenne des plans d'eau selon la lithologie",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  ylim(0,5000)

surface_mediane_pehm_litho <-
ggplot(pe_taille,
       aes(y = surface_med_pe,
           x = fct_rev(lithologie))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Lithologie",
       y = "Superficie médiane des plans d'eau (m²)",
       title = "Superficie médiane des plans d'eau selon la lithologie",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))+
  ylim(0,1500)

pe_vf1 <- pe_vf %>%
  filter(mare == 0 & zone_marais ==0 ) %>%
  mutate(surface_ha = surface_m2/100000) %>%
  st_drop_geometry()

pe_vf2 <- pe_vf %>%
  filter(lithologie == 'Granites' | lithologie == 'Argiles') %>%
  mutate(surface_ha = as.numeric(surface_m2/100000))
  
pe_vf3 <- pe_vf %>%
  filter(mare == 0 & zone_marais ==0) %>%
  mutate(surface_ha = as.numeric(surface_m2/100000)) %>%
  filter(lithologie != 'Schistes et grès')

filter(lithologie == 'Ophiolites')

violin_surface_pehm_litho2 <- 
  ggplot(data= pe_vf1,
         aes(x = fct_rev(lithologie),
             y = surface_m2)) +
  geom_violin() +
  coord_flip() +
  scale_y_log10(labels = function(x) format(x, scientific = FALSE, big.mark = " ")) +
  labs(y = "Superficie (m², échelle log)",
       x = "Lithologie", 
       title = "Répartition de la superficie des plans d'eau selon leur \nclasse lithologique (hors mares et marais)")

ggplotly(violin_surface_pehm_litho2)

quantile(pe_vf1$surface_ha)

pe_vf1 %>% 
  st_drop_geometry() %>% 
  sample_n(10) %>% 
  pull(surface_ha)

violin_surface_pehm_litho <- 
  vioplot(pe_vf1$surface_ha~pe_vf1$lithologie,
          main = "Répartition de la superficie des plans d'eau \nselon leur classe lithologique (hors mares et marais)",
          xlab="Lithologie",
          ylab="Superficie (ha)",col="lightblue") +
  theme(axis.text.x=element_text(angle=45, hjust=0.1, vjust=0.1))  

violin_surface_pehm_litho

violin2_surface_pehm_litho <- 
  ggplot(data= pe_vf1,
         aes(x = fct_rev(lithologie),
             y = surface_ha)) +
  geom_violin() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = " ")) +
  labs(y = "Superficie (ha)",
       x = "Lithologie", 
       title = "Répartition de la superficie des plans d'eau selon leur classe \nlithologique") 

violin2_surface_pehm_litho

violin_surface_pehm_litho_hors_schistes <- 
  vioplot(pe_vf3$surface_ha~pe_vf3$lithologie,
          main = "Répartition de la superficie des plans d'eau \nselon leur classe lithologique",
          xlab="Lithologie",
          ylab="Superficie (ha)",col="lightblue")

boxsplot_surface_pehm_litho <- 
  ggplot(pe_vf1 %>% 
           st_drop_geometry() , 
         aes(x = lithologie, y = surface_ha)) +
  geom_jitter(aes(color = lithologie), width = 0.2, alpha = 0.5)  + 
  labs(x = "Lithologie",
       y = "Superficie des plans d'eau (ha)",
       title = "Répartition de la superficie des plans d'eau selon leur lithologie simplifiée",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)")+
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1)) +
  geom_boxplot()

boxsplot_surface_pehm_litho_ylim10 <- 
  ggplot(pe_vf1 %>% 
           st_drop_geometry() , 
         aes(x = lithologie, y = surface_ha)) +
  geom_jitter(aes(color = lithologie), width = 0.2, alpha = 0.5)  + 
  labs(x = "Lithologie",
       y = "Superficie des plans d'eau (ha)",
       title = "Répartition de la superficie des plans d'eau selon leur lithologie simplifiée",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)")+
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1)) +
  geom_boxplot() +
  ylim(0, 10)

boxsplot_surface_pehm_litho_ylim2_5 <- 
  ggplot(pe_vf1 %>% 
           st_drop_geometry() , 
         aes(x = lithologie, y = surface_ha)) +
  geom_jitter(aes(color = lithologie), width = 0.2, alpha = 0.5)  + 
  labs(x = "Lithologie",
       y = "Superficie des plans d'eau (ha)",
       title = "Répartition de la superficie des plans d'eau selon leur lithologie simplifiée",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)")+
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1)) +
  geom_boxplot() +
  ylim(0, 2.5)

boxsplot_surface_pehm_litho_ylim_0_1 <- 
  ggplot(pe_vf1 %>% 
           st_drop_geometry() , 
         aes(x = lithologie, y = surface_ha)) +
  geom_jitter(aes(color = lithologie), width = 0.2, alpha = 0.5)  + 
  labs(x = "Lithologie",
       y = "Superficie des plans d'eau (ha)",
       title = "Répartition de la superficie des plans d'eau selon leur lithologie simplifiée",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)")+
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1)) +
  geom_boxplot() +
  ylim(0, 0.1)
        

### Plans d'eau, lithologie et persistance -----
  
repartition_pe_litho_persistance <-
    ggplot(pe_vf %>%
             filter(!is.na(Persistanc) & Persistanc!= '' & Persistanc != 'inconnue') %>%
             st_drop_geometry()) +
    aes(x = fct_rev(lithologie), fill = Persistanc) +
    geom_bar(position = "fill") +
    xlab("Lithologie") +
    ylab("Proportion de plans d'eau") +
    labs(title = "Persistance des plans d'eau selon la lithologie", fill = "Persistance") +
    scale_fill_manual(values = c( "#18d0f0", "#2959ee"))  +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1)) +
    coord_flip()
  
ggplotly(repartition_pe_litho_persistance)

# Plans d'eau et régime hydrologique ----

## Répartition des plans d'eau selon le rapport du QMNA5 sur le module ----

pe_q <- pe_vf %>% 
  filter(!is.na(QAMOY_MN) & 
           QAMOY_MN >= 0) %>%
  mutate(ratio_q5_qa = case_when(
    (Q5MOY_MN/QAMOY_MN) >= 0 & (Q5MOY_MN/QAMOY_MN) <= 0.025 ~ '<= 40ème',
    (Q5MOY_MN/QAMOY_MN) > 0.025 & (Q5MOY_MN/QAMOY_MN) <= 0.05 ~ 'Entre le 20ème et le 40ème',
    (Q5MOY_MN/QAMOY_MN) > 0.05 & (Q5MOY_MN/QAMOY_MN) <= 0.1 ~ 'Entre le 10ème et le 20ème',
    (Q5MOY_MN/QAMOY_MN) > 0.1 ~ '> 10ème')) %>%
  st_drop_geometry()

pe_q_mares <- pe_q %>%
  filter(mare ==1)

pe_q_marais <- pe_q %>%
  filter(zone_marais==1)

pe_q_mares_marais <- pe_q %>%
  filter(zone_marais==1 | mare ==1)

pe_q_qa_nul <- pe_q %>%
  filter(QAMOY_MN == 0)

pe_q_q5_nul <- pe_q %>%
  filter(Q5MOY_MN == 0)

pe_qa_q5_nul <- pe_q %>%
  filter(QAMOY_MN == 0 | Q5MOY_MN == 0)

pe_q_non_nul <- pe_q %>%
  filter(QAMOY_MN > 0)

pe_q_inf40 <- pe_q %>%
  filter((Q5MOY_MN/QAMOY_MN) >= 0 & (Q5MOY_MN/QAMOY_MN) <= 0.025)

pe_q_40_20 <- pe_q %>%
  filter((Q5MOY_MN/QAMOY_MN) > 0.025 & (Q5MOY_MN/QAMOY_MN) <= 0.05)

pe_q_20_10 <- pe_q %>%
  filter((Q5MOY_MN/QAMOY_MN) > 0.05 & (Q5MOY_MN/QAMOY_MN) <= 0.1)

pe_q_sup10 <- pe_q %>%
  filter(Q5MOY_MN/QAMOY_MN > 0.1)

pe_q_inf1 <- pe_q %>%
  filter(Q5MOY_MN < 0.001)

repartition_pe_rh <-
  ggplot(data = pe_q, 
       aes(x = Q5MOY_MN/QAMOY_MN)) +
  geom_histogram(bins = 100, fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Débit d'étiage sur le débit de module du plus proche tronçon hydrographique (<10m)
 (échelle log, 1/40 = 0,025 - 1/20ème = 0,05 - 1/10ème = 0,1)",
       y = "Nombre de plans d'eau",
       title = "Répartition des plans d'eau selon la 'sévérité de leurs étiages' au droit \ndu plan d'eau - Toutes surfaces élémentaires") +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen") 

ggplotly(repartition_pe_rh)

repartition_pehm_rh <-
  ggplot(data = pe_q %>%
        filter(mare == 0 & zone_marais == 0), 
       aes(x = Q5MOY_MN/QAMOY_MN)) + 
  geom_histogram(bins = 100, fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Débit d'étiage sur le débit de module du plus proche tronçon hydrographique (<10m)
       (échelle log, 1/40 = 0,025 - 1/20ème = 0,05 - 1/10ème = 0,1)",
       y = "Nombre de plans d'eau",
       title = "Répartition des plans d'eau selon la 'sévérité de leurs étiages' au droit \ndu plan d'eau - Hors mares et marais") +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen")

ggplotly(repartition_pehm_rh)

pe_q_vf <- pe_q %>%
  filter(QAMOY_MN>0 &
           Persistanc != 'inconnue')

repartition_pe_rh <-
  ggplot(data = pe_q_vf %>%
           st_drop_geometry() , 
         aes(x = Q5MOY_MN/QAMOY_MN, 
             fill = Persistanc)) +
  geom_histogram(bins = 100, position = "stack") +   
  scale_fill_manual(values = c( "#18d0f0", "#2959ee")) +
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Rapport du Qmna5 sur le module du tronçon hydrographique le plus proche (<10m)
 (échelle log, 1/40 = 0,025 - 1/20ème = 0,05 - 1/10ème = 0,1)",
       y = "Nombre de plans d'eau",
       title = "Répartition des plans d'eau selon la 'sévérité de leurs étiages' au droit \ndu plan d'eau - Toutes surfaces élémentaires", 
       fill = "Persistance") +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen") 

ggplotly(repartition_pe_rh)

repartition_pehm_rh <-
  ggplot(data = pe_q_vf %>%
           filter(mare == 0 & zone_marais == 0), 
         aes(x = Q5MOY_MN/QAMOY_MN, 
             fill = Persistanc)) +
  geom_histogram(bins = 75, position = "stack") +   
  scale_fill_manual(values = c( "#18d0f0", "#2959ee")) +
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Rapport du Qmna5 sur le module du tronçon hydrographique le plus proche (<10m)
 (échelle log, 1/40 = 0,025 - 1/20ème = 0,05 - 1/10ème = 0,1)",
       y = "Nombre de plans d'eau",
       title = "Répartition des plans d'eau selon la 'sévérité de leurs étiages' au droit \ndu plan d'eau - Hors mares et marais", 
       fill = "Persistance") +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen") 

ggplotly(repartition_pehm_rh)

## Répartition des BV ME selon le rapport du QMNA5 sur le module ----

repartition_bv_rh <-
  ggplot(data = bv_me %>% filter(!is.na(QAMOY_max) & QAMOY_max>=0), 
       aes(x = Q5MOY_max/QAMOY_max)) + 
  geom_histogram(bins = 50, fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Rapport du Qmna5 sur le module à l'exutoire du BV de la masse d'eau
       (échelle log, 1/40 = 0,025 - 1/20ème = 0,05 - 1/10ème = 0,1)",
       y = "Nombre de bassins versant de masses d'eau",
       title = "Répartition des bassins versant selon la 'sévérité de l'étiage' \nà l'exutoire de la masse d'eau",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen")

ggplotly(repartition_bv_rh)

repartition_com_rh <-
  ggplot(data = communes %>% filter(!is.na(QAMOY_max) & QAMOY_max>=0), 
         aes(x = Q5MOY_max/QAMOY_max)) + 
  geom_histogram(bins = 50, fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Rapport du plus fort Qmna5 sur le plus fort module de la commune
       (échelle log, 1/40 = 0,025 - 1/20ème = 0,05 - 1/10ème = 0,1)",
       y = "Nombre de communes",
       title = "Répartition des communes selon la 'sévérité de leurs étiages'",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen")

repartition_com_rh

## Densité numérique de plans d'eau selon le rapport du QMNA5 sur le module ----

bv_me_q <- bv_me %>%
  filter(Q5MOY_max>=0 & !is.na(Q5MOY_max)) %>%
  mutate(ratio_q5_qa = case_when(
    (Q5MOY_max/QAMOY_max) >= 0 & (Q5MOY_max/QAMOY_max) <= 0.025 ~ '<= 40ème',
    (Q5MOY_max/QAMOY_max) > 0.025 & (Q5MOY_max/QAMOY_max) <= 0.05 ~ 'Entre le 20ème et le 40ème',
    (Q5MOY_max/QAMOY_max) > 0.05 & (Q5MOY_max/QAMOY_max) <= 0.1 ~ 'Entre le 10ème et le 20ème',
    (Q5MOY_max/QAMOY_max) > 0.1 ~ '> 10ème',
    is.na(Q5MOY_max/QAMOY_max) ~ 'NR')) %>%
  group_by(ratio_q5_qa) %>%
  summarise(nb_me = n(),
            surface_km2 = sum(surface_me)/1000000, 
            surface_marais_km2 = sum(surface_marais)/1000000) %>%
  select(ratio_q5_qa, nb_me, surface_km2, surface_marais_km2) %>%
  mutate(proportion_surface = (surface_km2*100)/sum(surface_km2))

pe_ratio <- pe_vf %>% # Très long ...
  st_intersection(bv_me_q) %>%
  mutate(surf_intersect = st_area(.))

pe_ratio <- pe_ratio %>%
  mutate(ratio_q5_qa = fct_relevel(ratio_q5_qa,
                                   '<= 40ème',  
                                   'Entre le 20ème et le 40ème', 
                                   'Entre le 10ème et le 20ème', 
                                   '> 10ème'))

surf_pe_tot_q <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_ratio %>% 
      units::drop_units() %>% 
      st_drop_geometry(),
    var_id_polygone = ratio_q5_qa,
    var_a_sommer = surf_intersect,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  ) 

surf_pehm_q <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_ratio %>% 
      units::drop_units() %>% 
      st_drop_geometry() %>%
      filter(mare == 0),
    var_id_polygone = ratio_q5_qa,
    var_a_sommer = surf_intersect,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

bv_me_q <- bv_me_q %>%
  left_join(surf_pe_tot_q, join_by(ratio_q5_qa == ratio_q5_qa)) %>%
  left_join(surf_pehm_q, join_by(ratio_q5_qa == ratio_q5_qa)) %>%
  mutate(dens_num_pe = nb_pe_tot/surface_km2, 
         dens_surf_pe = ((surf_pe_tot/1000000)*100) / surface_km2, 
         dens_num_pehm = nb_pehm_tot/(surface_km2-surface_marais_km2), 
         dens_surf_pehm = ((surf_pehm_tot/1000000)*100) /(surface_km2-surface_marais_km2)) %>%
  filter(ratio_q5_qa != '')


bv_me_q <- bv_me_q %>%
  mutate(ratio_q5_qa = fct_relevel(ratio_q5_qa,
                                   '<= 40ème',  
                                   'Entre le 20ème et le 40ème', 
                                   'Entre le 10ème et le 20ème', 
                                   '> 10ème'))
           
           
table_bv_me_q <- bv_me_q %>%
  filter(ratio_q5_qa != 'NR') %>%
  st_drop_geometry() %>%
  mutate("Classe de 'sévérité des étiages' (ratio q5/qa) à l'exutoire de masses d'eau" = ratio_q5_qa,
         "Nombre de masses d'eau" = nb_me,
         "Superficie (km²)" = round(surface_km2),
         "Superficie retenue de marais (km²)" = round(surface_marais_km2),
         "Proportion de surface (%)" = round(proportion_surface, 1),
         "Nombre total de plans d'eau" = nb_pe_tot,
         "Densité numérique en plans d'eau par km²" = round(dens_num_pe, 2),
         "Nombre de plans d'eau (hors mares et marais)" = nb_pehm_tot,
         "Densité numérique en plans d'eau (hors mares et marais) par km²" = round(dens_num_pehm, 2),
  ) %>%
  dplyr::select("Classe de 'sévérité des étiages' (ratio q5/qa) à l'exutoire de masses d'eau", 
                "Nombre de masses d'eau",
                "Superficie (km²)", 
         "Superficie retenue de marais (km²)",
         "Proportion de surface (%)", 
         "Nombre total de plans d'eau",
         "Densité numérique en plans d'eau par km²",
         "Nombre de plans d'eau (hors mares et marais)",
         "Densité numérique en plans d'eau (hors mares et marais) par km²")



table_bv_me_q <- table_bv_me_q %>% 
  arrange(desc(`Densité numérique en plans d'eau (hors mares et marais) par km²`))


densite_numerique_pe_rh <-
  ggplot(bv_me_q %>%
           filter(ratio_q5_qa != 'NR'),
       aes(y = dens_num_pe,
           x = fct_rev(ratio_q5_qa))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau",
       y = "Nombre de plans d'eau par km² de bassin versant de masse d'eau",
       title = "Densité numérique de plans d'eau selon la 'sévérité \ndes étiages' - Toutes surfaces élémentaires") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  ylim(0,5)

ggplotly(densite_numerique_pe_rh)

densite_numerique_pehm_rh <-
  ggplot(bv_me_q %>%
           filter(ratio_q5_qa != 'NR'),
       aes(y = dens_num_pehm,
           x = fct_rev(ratio_q5_qa))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau",
       y = "Nombre de plans d'eau par km² de bassin versant de masse d'eau\n(surface de marais déduite)",
       title = "Densité numérique de plans d'eau selon la 'sévérité \ndes étiages' - Hors mares et marais") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))+
  ylim(0,5)

ggplotly(densite_numerique_pehm_rh)

densite_surfacique_pe_rh <-
  ggplot(bv_me_q %>%
           filter(ratio_q5_qa != 'NR'),
       aes(y = dens_surf_pe,
           x = fct_rev(ratio_q5_qa))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau",
       y = "Densité surfacique de plans d'eau (%)",
       title = "Densité surfacique de plans d'eau selon la 'sévérité \ndes étiages' - Toutes surfaces élémentaires") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))+
  ylim(0,5)

ggplotly(densite_surfacique_pe_rh)

densite_surfacique_pehm_rh <-
ggplot(bv_me_q %>%
         filter(ratio_q5_qa != 'NR'),
       aes(y = dens_surf_pehm,
           x = fct_rev(ratio_q5_qa))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau",
       y = "Densité surfacique de plans d'eau (%)",
       title = "Densité surfacique de plans d'eau selon la 'sévérité \ndes étiages' - Hors mares et marais") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))+
  ylim(0,5)

ggplotly(densite_surfacique_pehm_rh)

## Persistance selon le ratio d'étiage et la pers

boxsplot_surface_pehm_ratio <- 
  ggplot(pe_ratio, 
         aes(x = fct(rev(ratio_q5_qa), y = surface_m2/1000000))) +
  geom_jitter(aes(color = Persistanc), width = 0.2, alpha = 0.5)  + 
  labs(x = "Ratio de débit",
       y = "Superficie des plans d'eau (ha)",
       title = "Répartition de la superficie des plans d'eau en fonction de la sévérité de leur débit",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)")+
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1)) +
  geom_boxplot() +
  ylim(0, 0.1)


## Taille médiane des surfaces élémentaires de plans d'eau selon la sévérité des étiages ---- 

pe_q <- pe_q %>% 
  mutate(ratio_q5_qa = fct_relevel(ratio_q5_qa, c("> 10ème", "Entre le 10ème et le 20ème", "Entre le 20ème et le 40ème","<= 40ème")))

pe_taille_q <- pe_ratio %>%
  st_drop_geometry() %>%
  group_by(ratio_q5_qa) %>%
  summarise(surface_moy_pe = mean(surf_intersect),
            surface_med_pe = median(surf_intersect)) %>%
  filter(!is.na(ratio_q5_qa))

pe_taille_q <- pe_taille_q %>%
  mutate(ratio_q5_qa = fct_relevel(ratio_q5_qa,
                                     '<= 40ème',  
                                     'Entre le 20ème et le 40ème', 
                                     'Entre le 10ème et le 20ème', 
                                     '> 10ème'))

bv_me_q <- bv_me_q %>%
  mutate(ratio_q5_qa = fct_relevel(ratio_q5_qa,
                                   '<= 40ème',  
                                   'Entre le 20ème et le 40ème', 
                                   'Entre le 10ème et le 20ème', 
                                   '> 10ème'))

pehm_taille_q <- pe_ratio %>%
  st_drop_geometry() %>%
  filter(zone_marais== 0 & mare == 0 ) %>%  
  group_by(ratio_q5_qa) %>%
  summarise(surface_moy_pehm = mean(surf_intersect),
            surface_med_pehm = median(surf_intersect))%>%
  filter(!is.na(ratio_q5_qa))

pehm_taille_q <- pehm_taille_q %>%
  mutate(ratio_q5_qa = fct_relevel(ratio_q5_qa,
                                   '<= 40ème',  
                                   'Entre le 20ème et le 40ème', 
                                   'Entre le 10ème et le 20ème', 
                                   '> 10ème'))

surface_moyenne_pe_rh <-
ggplot(pe_taille_q %>% 
         drop_units() %>%
         filter(ratio_q5_qa != 'NR'),
       aes(y = surface_moy_pe,
           x = fct_rev(ratio_q5_qa))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau",
       y = "Superficie moyenne des plans d'eau (m²)",
       title = "Superficie moyenne des plans d'eau selon la 'sévérité \ndes étiages' de la masse d'eau - Toutes surfaces élémentaires") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))+
  ylim(0,5000)

ggplotly(surface_moyenne_pe_rh)

surface_moyenne_pehm_rh <-
  ggplot(pehm_taille_q%>% 
           drop_units() %>%
           filter(ratio_q5_qa != 'NR'),
       aes(y = surface_moy_pehm,
           x = fct_rev(ratio_q5_qa))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau",
       y = "Superficie moyenne des plans d'eau (m²)",
       title = "Superficie moyenne des plans d'eau selon la 'sévérité \ndes étiages' de la masse d'eau - Hors mares et marais") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))+
  ylim(0,5000)

ggplotly(surface_moyenne_pehm_rh)

surface_mediane_pe_rh <-
  ggplot(pe_taille_q%>% 
           drop_units() %>%
           filter(ratio_q5_qa != 'NR'),
         aes(y = surface_med_pe,
           x = fct_rev(ratio_q5_qa))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau",
       y = "Superficie médiane des plans d'eau (m²)",
       title = "Superficie médiane des plans d'eau selon la 'sévérité \ndes étiages' de la masse d'eau - Toutes surfaces élémentaires") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))+
  ylim(0,2000)

ggplotly(surface_mediane_pe_rh)

surface_mediane_pehm_rh <-
  ggplot(pehm_taille_q%>% 
           drop_units() %>%
           filter(ratio_q5_qa != 'NR'),
       aes(y = surface_med_pehm,
           x = fct_rev(ratio_q5_qa))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau",
       y = "Superficie médiane des plans d'eau (m²)",
       title = "Superficie médiane des plans d'eau selon la 'sévérité \ndes étiages' de la masse d'eau - Hors mares et marais") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))+
  ylim(0,2000)

ggplotly(surface_mediane_pehm_rh)

surface_pehm_rh <-
  ggplot(pe_ratio %>% filter((Q5MOY_MN)/QAMOY_MN >= 0 & 
                               (Q5MOY_MN)/QAMOY_MN < 1 &
                               surface_m2 < 1500000),
         aes(y = (Q5MOY_MN)/QAMOY_MN,
             x = surface_m2/10000)) + 
  geom_point() +
  labs(x = "Superficie des plans d'eau (ha)",
       y = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau",
       title = "Superficie des plans d'eau selon la 'sévérité des étiages' de la masse d'eau",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  geom_hline(yintercept = 0.025, col = "darkred") +
  geom_hline(yintercept = 0.05, col = "darkorange") +
  geom_hline(yintercept = 0.1, col = "darkgreen") +
  geom_smooth(method="lm")  

surface_pehm_rh

## Densité numérique en pe des BVME en fonction de la sévérité des étiages ---- 

densite_numerique_pe_rh_bvme <-
  ggplot(bv_me %>%
           filter(!is.na(QAMOY_max)),
         aes(y = ((coalesce(nb_pe_tot,0) + coalesce(nb_mares_tot,0))/(surface_me/1000000)),
             x = (Q5MOY_max/QAMOY_max ))) + 
  geom_point() +
  labs(x = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau",
       y = "Densité numérique en plans d'eau 
du bassin versant des masses d'eau",
       title = "Densité numérique en plans d'eau des masses d'eau selon la 'sévérité des étiages'",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen") +
  geom_smooth(method="lm") 

densite_numerique_pehm_rh_bvme <-
  ggplot(bv_me %>%
           filter(!is.na(QAMOY_max) & (Q5MOY_max/QAMOY_max)<1),
         aes(y = (coalesce(nb_pehm_tot,0)/((surface_me-surface_marais)/1000000)),
             x = (Q5MOY_max/QAMOY_max ))) + 
  geom_point() +
  labs(x = "Ratio du Qmna5 sur le module  
à l'exutoire du BV des masses d'eau (ratio<1)",
       y = "Densité numérique en plans d'eau 
du bassin versant des masses d'eau",
       title = "Densité numérique en plans d'eau des masses d'eau selon la 'sévérité des étiages'",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen") +
  geom_smooth(method="lm") 

densite_surfacique_pehm_rh_bvme <-
  ggplot(bv_me %>%
           filter(!is.na(QAMOY_max) & (Q5MOY_max/QAMOY_max)<1),
         aes(y = (coalesce(surf_pehm_tot,0)*100/(surface_me-surface_marais)),
             x = (Q5MOY_max/QAMOY_max ))) + 
  geom_point() +
  labs(x = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau (ratio<1)",
       y = "Densité surfacique en plans d'eau (%) 
du bassin versant des masses d'eau",
       title = "Densité surfacique en plans d'eau des masses d'eau selon la 'sévérité des étiages'",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen") +
  geom_smooth(method="lm")

densite_numerique_pehm_rh_bvme_logx <-
  ggplot(bv_me %>%
           filter(!is.na(QAMOY_max) & Q5MOY_max/QAMOY_max < 1 & Q5MOY_max/QAMOY_max > 0 ),
         aes(y = (coalesce(nb_pehm_tot,0)/((surface_me-surface_marais)/1000000)),
             x = (Q5MOY_max/QAMOY_max ))) + 
  geom_point() +
  scale_x_log10() +
  labs(x = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau (échelle log)",
       y = "Densité numérique en plans d'eau 
du bassin versant des masses d'eau",
       title = "Densité numérique en plans d'eau des masses d'eau selon la \n'sévérité de leurs étiages - Hors mares et marais") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen") +
  geom_smooth(method="lm")

ggplotly(densite_numerique_pehm_rh_bvme_logx)

densite_surfacique_pehm_rh_bvme_logx <-
  ggplot(bv_me %>%
           filter(!is.na(QAMOY_max) & Q5MOY_max/QAMOY_max < 1 & Q5MOY_max/QAMOY_max > 0),
         aes(y = (coalesce(surf_pehm_tot,0)*100/(surface_me-surface_marais)),
             x = (Q5MOY_max/QAMOY_max ))) + 
  geom_point() +
  scale_x_log10() +
  labs(x = "Ratio du Qmna5 sur le module 
à l'exutoire du BV des masses d'eau (échelle log)",
       y = "Densité surfacique en plans d'eau (%) 
du bassin versant des masses d'eau",
       title = "Densité surfacique en plans d'eau des masses d'eau selon la \n'sévérité de leurs étiages - Hors mares et marais") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen") +
  geom_smooth(method="lm")

ggplotly(densite_surfacique_pehm_rh_bvme_logx)

## Densité numérique et surfacique en PE des communes selon la sévérité de l'étiage ----

densite_numerique_pehm_rh_com_logx <-
  ggplot(communes %>%
           filter(!is.na(QAMOY_max) & Q5MOY_max/QAMOY_max < 1 ),
         aes(y = (coalesce(nb_pehm_tot,0)/((surface_com-surface_marais)/1000000)),
             x = (Q5MOY_max/QAMOY_max ))) + 
  geom_point() +
  scale_x_log10() +
  labs(x = "Ratio du Qmna5 sur le module",
       y = "Densité numérique en plans d'eau des communes",
       title = "Densité numérique en plans d'eau des communes selon la 'sévérité des étiages'",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen") +
  geom_smooth(method="lm")

densite_surfacique_pehm_rh_com_logx <-
  ggplot(communes %>%
           filter(!is.na(QAMOY_max) & Q5MOY_max/QAMOY_max < 1 ),
         aes(y = (coalesce(surf_pehm_tot,0)*100/(surface_com-surface_marais)),
             x = (Q5MOY_max/QAMOY_max ))) + 
  geom_point() +
  scale_x_log10() +
  labs(x = "Ratio du Qmna5 sur le module",
       y = "Densité surfacique en plans d'eau des communes (%)",
       title = "Densité surfacique en plans d'eau des communes selon la 'sévérité des étiages'",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  geom_vline(xintercept = 0.025, col = "darkred") +
  geom_vline(xintercept = 0.05, col = "darkorange") +
  geom_vline(xintercept = 0.1, col = "darkgreen") +
  geom_smooth(method="lm")

### Plans d'eau, ratio de débit et persistance -----



repartition_pe_q_persistance <-
  ggplot(pe_ratio %>%
           filter(Persistanc != 'inconnue' & 
           ratio_q5_qa != 'NR')) +
  aes(x = fct_rev(ratio_q5_qa), fill = Persistanc) +
  geom_bar(position = "fill") +
  xlab("Ratio du QMNA5 sur le module à l'exutoire du BV de la masse d'eau") +
  ylab("Proportion de plans d'eau") +
  labs(title = "Persistance des plans d'eau selon la 'sévérité de \nl'étiage' du bassin versant de sa masse d'eau" , fill = "Persistance") +
  scale_fill_manual(values = c( "#18d0f0", "#2959ee"))  +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1)) +
  coord_flip()

ggplotly(repartition_pe_q_persistance)

## Superficie et ratio d'étiage ----

violin_surface_pe_ratio <- 
  ggplot(data= pe_ratio %>%
           st_drop_geometry() %>%
           filter(ratio_q5_qa != 'NR') %>%
           filter(mare == 0 & zone_marais == 0),
         aes(x = fct_rev(ratio_q5_qa),
             y = surface_m2)) +
  geom_violin() +
  coord_flip() +
  scale_y_log10(labels = function(x) format(x, scientific = FALSE, big.mark = " ")) +
  labs(y = "Superficie (m², échelle log)",
       x = "Sévérité de l'étiage à l'exutoire de la masse d'eau (Q5/Qa)",
       title = "Répartition de la superficie des plans d'eau selon la \n'sévérité de l'étiage' - Hors mares et marais")

ggplotly(violin_surface_pe_ratio)

# Densite numerique plus forte sur certains types de ME (cotiere, transition + ?) ----

bv_me_type <- bv_me %>%
  filter(substr(cdeumassed, 4,4) != 'L') %>%
  mutate(type = case_when(
    substr(cdeumassed, 4,4) == 'R' ~ 'cours deau',
    substr(cdeumassed, 4,4) == 'C' ~ 'cotiere',
    substr(cdeumassed, 4,4) == 'T' ~ 'transition')) %>%
  group_by(type) %>%
  summarise(nb_me = n(),
            surface_km2 = sum(surface_me)/1000000,
            surface_marais_km2 = sum(surface_marais)/1000000) %>%
  dplyr::select(type, nb_me, surface_km2, surface_marais_km2) %>%
  mutate(proportion_surface = (surface_km2*100)/sum(surface_km2)) %>%
  st_drop_geometry()

pe_type <- pe_vf %>%
  mutate(type = case_when(
    substr(cd_me, 4,4) == 'R' ~ 'cours deau',
    substr(cd_me, 4,4) == 'L' ~ 'plan deau',
    substr(cd_me, 4,4) == 'C' ~ 'cotiere',
    substr(cd_me, 4,4) == 'T' ~ 'transition')) 

surf_pe_type <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_type %>% 
      units::drop_units() %>% 
      st_drop_geometry(),
    var_id_polygone = type,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  ) 

surf_pehm_type <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_type %>% 
      units::drop_units() %>% 
      st_drop_geometry() %>%
      filter(mare == 0),
    var_id_polygone = type,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  ) 

bv_me_type <- bv_me_type %>%
  left_join(surf_pe_type, join_by(type == type)) %>%
  left_join(surf_pehm_type, join_by(type == type)) %>%
  mutate(dens_num_pe = nb_pe_tot/surface_km2, 
         dens_surf_pe = ((surf_pe_tot/1000000)*100) / surface_km2, 
         dens_num_pehm = nb_pehm_tot/(surface_km2-coalesce(surface_marais_km2,0)), 
         dens_surf_pehm = ((surf_pehm_tot/1000000)*100) / (surface_km2-coalesce(surface_marais_km2,0))) %>%
  filter(type != '')

table_bv_me_type <- bv_me_type %>%
  mutate("Type de masse d'eau" = type,
         "Nombre de masses d'eau" = nb_me,
         "Superficie (km²)" = round(surface_km2),
         "Proportion de surface (%)" = round(proportion_surface, 1),
         "Superficie de marais retenue (km²)" = round(coalesce(surface_marais_km2,0)),
         "Nombre total de plans d'eau" = nb_pe_tot,
         "Densité numérique en plans d'eau par km²" = round(dens_num_pe, 2),
         "Nombre de plans d'eau hors mares et marais" = nb_pehm_tot,
         "Densité numérique en plans d'eau hors mares et marais par km²" = round(dens_num_pehm, 2),
  ) %>%
  dplyr::select("Type de masse d'eau",
                "Nombre de masses d'eau",
                "Superficie (km²)", 
                "Proportion de surface (%)", 
                "Superficie de marais retenue (km²)",
                "Nombre total de plans d'eau",
                "Densité numérique en plans d'eau par km²",
                "Nombre de plans d'eau hors mares et marais",
                "Densité numérique en plans d'eau hors mares et marais par km²")

bv_me_type <- bv_me_type %>% 
  mutate(type = fct_rev(as.factor(type)))

densite_numerique_pe_type <-
  ggplot(bv_me_type,
       aes(y = dens_num_pe,
           x = type)) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Type de masse d'eau",
       y = "Nombre de plans d'eau par km² de bassin versant de masse d'eau",
       title = "Densité numérique de plans d'eau selon le type de masse d'eau\nToutes surfaces élémentaires",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

ggplotly(densite_numerique_pe_type)

densite_numerique_pehm_type <-
ggplot(bv_me_type,
       aes(y = dens_num_pehm,
           x = type)) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Type de masse d'eau",
       y = "Nombre de plans d'eau par km² de bassin versant de masse d'eau\n(surfaces de marais déduites)",
       title = "Densité numérique de plans d'eau selon le type de masse d'eau\nHors mares et marais") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

ggplotly(densite_numerique_pehm_type)

densite_surfacique_pe_type <-
  ggplot(bv_me_type,
       aes(y = dens_surf_pe,
           x = type)) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Type de masse d'eau",
       y = "Densité surfacique de plans d'eau (%)",
       title = "Densité surfacique de plans d'eau selon le type de masse d'eau",
       subtitle = "Bretagne et Pays de la Loire élargis") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

densite_surfacique_pehm_type <-
ggplot(bv_me_type,
       aes(y = dens_surf_pehm,
           x = type)) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Type de masse d'eau",
       y = "Densité surfacique de plans d'eau (%)",
       title = "Densité surfacique de plans d'eau selon le type de masse d'eau",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

### Plans d'eau, type de masse d'eau et persistance -----

pe_type <- pe_vf %>%
  mutate(type = case_when(
  substr(cd_me, 4,4) == 'L' ~ 'plan deau',
  substr(cd_me, 4,4) == 'R' ~ 'cours deau',
  substr(cd_me, 4,4) == 'C' ~ 'cotiere',
  substr(cd_me, 4,4) == 'T' ~ 'transition')) %>%
  st_drop_geometry()

repartition_pe_me_persistance <-
  ggplot(pe_type %>%
           filter(Persistanc != 'inconnue' & 
           !is.na(type))) +
  aes(x = type, fill = Persistanc) +
  geom_bar(position = "fill") +
  xlab("Type de masse d'eau") +
  ylab("Proportion de plans d'eau") +
  labs(title = "Persistance des plans d'eau selon le type de masse d'eau" , fill = "Persistance") +
  scale_fill_manual(values = c( "#18d0f0", "#2959ee"))  +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1)) +
  coord_flip()

# Plans d'eau et HER ----

her_zone_etude <- her2 %>%
  st_intersection(zone_etude) %>%
  mutate(surface_intersect = st_area(.)) %>%
  #  st_drop_geometry() %>%
  group_by(CdHER2) %>%
  summarise(surface_km2 = sum(surface_intersect)/1000000, 
            surface_marais_km2 = (sum(surface_marais_km2))) %>%
  mutate(proportion_surface = (surface_km2*100)/sum(surface_km2)) %>%
  units::drop_units()

her_zone_etude <- her_zone_etude %>%
  mutate(surface_marais_km2 = coalesce(surf_marais,0)/1000000) %>%
  dplyr::select(-surf_marais)

### Calcul associé des densités de PE par type de her ----

pe_her <- pe_vf %>%
  st_intersection(her2) %>%
  st_drop_geometry()

surf_pe_tot_her <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_her %>% 
      units::drop_units() %>%
      st_drop_geometry(),
    var_id_polygone = CdHER2,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_her <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_her %>% 
      units::drop_units() %>%
      st_drop_geometry(),
    var_id_polygone = CdHER2,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pehm_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

surf_pehm_hors_mares_her <-
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_her %>% 
      units::drop_units() %>%
      st_drop_geometry() %>%
      filter(mare ==0),
    var_id_polygone = CdHER2,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pehm_hors_mares,
    var_somme_surfaces = surf_pehm_hors_mares,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  )

### Jointure et calcul de densités ----

her_densite_pe <- her_zone_etude %>% 
  st_drop_geometry() %>%
  left_join(surf_pe_tot_her) %>%
  left_join(surf_pehm_her) %>%
  left_join(surf_pehm_hors_mares_her) %>%
  mutate(dens_num_pe = nb_pe_tot / surface_km2, 
         dens_surf_pe = ((surf_pe_tot/1000000)*100) / surface_km2,
         dens_num_pehm = nb_pehm_tot / (surface_km2-coalesce(surface_marais_km2, 0)), 
         dens_surf_pehm = ((surf_pehm_tot/1000000)*100) / (surface_km2-coalesce(surface_marais_km2, 0)),
         dens_num_pehm_hors_mares = nb_pehm_hors_mares / (surface_km2-coalesce(surface_marais_km2, 0)), 
         dens_surf_pehm_hors_mares = ((surf_pehm_hors_mares/1000000)*100) / (surface_km2-coalesce(surface_marais_km2, 0)))

her <- her_zone_etude %>%
  left_join(her2 %>%
              st_drop_geometry() %>%
              dplyr::select(CdHER2, NomHER2)) %>%
  left_join(her_densite_pe)

table_her_densite_pe <- her %>%
  st_drop_geometry() %>%
  mutate("Nom de la HER" = NomHER2,
         "Superficie au sein de la zone d'étude (km²)" = round(surface_km2),
         "Proportion de surface de la zone d'étude (%)" = round(proportion_surface, 1),
         "Superficie de marais retenue (km²)" = round(coalesce(surface_marais_km2, 0)),
         "Nombre total de plans d'eau" = nb_pe_tot,
         "Densité numérique en plans d'eau par km²" = round(dens_num_pe, 2),
         "Nombre de plans d'eau hors mares et marais" = nb_pehm_hors_mares,
         "Densité numérique en plans d'eau hors mares et marais par km²" = round(nb_pehm_hors_mares/(surface_km2-coalesce(surface_marais_km2, 0)), 2),
  ) %>%
  dplyr::select("Nom de la HER", 
                "Superficie au sein de la zone d'étude (km²)", 
                "Proportion de surface de la zone d'étude (%)", 
                "Superficie de marais retenue (km²)",
                "Nombre total de plans d'eau",
                "Densité numérique en plans d'eau par km²",
                "Nombre de plans d'eau hors mares et marais",
                "Densité numérique en plans d'eau hors mares et marais par km²")

table_her_densite_pe2 <- table_her_densite_pe %>% 
  arrange(`Nom de la HER`)

### Représentations graphiques ----

densite_numerique_pe_her <-
  ggplot(her,
         aes(y = dens_num_pe,
             x = fct_rev(NomHER2))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Nom de l'hydro-éco-région",
       y = "Nombre de plans d'eau par km² d'Hydro-Eco-Région",
       title = "Densité numérique de plans d'eau selon la HER\nToutes surfaces élémentaires") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) #+
geom_hline(yintercept = 2.96625470035425, col = "black") +
  geom_hline(yintercept = 4.06466437893931, col = "darkblue") +
  geom_hline(yintercept = 1.6712139639832, col = "lightblue")

ggplotly(densite_numerique_pe_her)

densite_numerique_pehm_her <-
  ggplot(her,
         aes(y = dens_num_pehm,
             x = fct_rev(NomHER2))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Nom de l'hydro-éco-région",
       y = "Densité numérique de plans d'eau par km² d'Hydro-Eco-Région\n(surface de marais déduite)",
       title = "Densité numérique de plans d'eau selon l'hydro-éco-région\nHors mares et marais",
       subtitle = "Bretagne et Pays de la Loire élargis (hors marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) #+
geom_hline(yintercept = 2.64410797263605, col = "black") +
  geom_hline(yintercept = 3.47558795935624, col = "darkblue") +
  geom_hline(yintercept = 1.66378116445733, col = "lightblue")

ggplotly(densite_numerique_pehm_her)

densite_numerique_pehm_hors_mares_her <-
  ggplot(her,
         aes(y = dens_num_pehm_hors_mares,
             x = fct_rev(NomHER2))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Nom de l'hydro-éco-région",
       y = "Nombre de plans d'eau par km² d'Hydro-Eco-Région\n(surface de marais déduite)",
       title = "Densité numérique de plans d'eau selon la HER\nHors mares et marais") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

ggplotly(densite_numerique_pehm_hors_mares_her)

### Densite surfacique ----

densite_surfacique_pe_her <-
  ggplot(her,
         aes(y = dens_surf_pe,
             x = fct_rev(NomHER2))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Nom de l'hydro-éco-région",
       y = "Densité surfacique de plans d'eau (%)",
       title = "Densité surfacique de plans d'eau selon la HER\nToutes surfaces élémentaires") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

ggplotly(densite_surfacique_pe_her)

densite_surfacique_pehm_her <-
  ggplot(her,
         aes(y = dens_surf_pehm,
             x = fct_rev(NomHER2))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Nom de l'hydro-éco-région",
       y = "Densité surfacique de plans d'eau (%)",
       title = "Densité surfacique de plans d'eau selon l'hydro-éco-région",
       subtitle = "Bretagne et Pays de la Loire élargis (hors marais)") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

densite_surfacique_pehm_hors_mares_her <-
  ggplot(her,
         aes(y = dens_surf_pehm_hors_mares,
             x = fct_rev(NomHER2))) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(x = "Nom de l'hydro-éco-région",
       y = "Densité surfacique de plans d'eau (%)",
       title = "Densité surfacique de plans d'eau selon la HER\nHors mares et marais") +
  coord_flip() +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

ggplotly(densite_surfacique_pehm_hors_mares_her)

### Superficie en fonction des HER ----

violin_surface_pe_her <- 
  ggplot(data= pe_her %>%
           st_drop_geometry(),
         aes(x = fct_rev(NomHER2),
             y = surface_m2)) +
  geom_violin() +
  coord_flip() +
  scale_y_log10(labels = function(x) format(x, scientific = FALSE, big.mark = " ")) +
  labs(y = "Superficie (m², échelle log)",
       x = "Nom de l'Hydro-Eco-Région",
       title = "Répartition de la superficie des plans d'eau selon \nleur Hydro-Eco-Région - Hors mares et marais")

ggplotly(violin_surface_pe_her)


boxsplot_surface_pehm_her <- 
  ggplot(pe_her, 
         aes(x = NomHER2, y = surface_m2/10000)) +
  geom_jitter(aes(color = NomHER2), width = 0.2, alpha = 0.5)  + 
  labs(x = "Nom de la HER",
       y = "Superficie des plans d'eau (ha)",
       title = "Répartition de la superficie des plans d'eau selon les HER",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)") +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1)) +
  geom_boxplot() +
  ylim(0, 500)

boxsplot_surface_pehm_her_peristance <- 
  ggplot(pe_her, 
         aes(x = NomHER2, y = surface_m2/10000)) +
  geom_jitter(aes(color = Persistanc), width = 0.2, alpha = 0.5)  + 
  labs(x = "Nom de la HER",
       y = "Superficie des plans d'eau (ha)",
       title = "Répartition de la superficie des plans d'eau selon les HER",
       subtitle = "Bretagne et Pays de la Loire élargis (hors mares et marais)")+
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) +
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1)) +
  geom_boxplot() +
  ylim(0, 500)

## PE, pesistance et HER ----

repartition_pe_her_persistance <-
  ggplot(pe_her %>%
           filter(Persistanc != 'inconnue' & 
                    !is.na(NomHER2))) +
  aes(x = NomHER2, fill = Persistanc) +
  geom_bar(position = "fill") +
  xlab("Nom de la HER") +
  ylab("Proportion de plans d'eau") +
  labs(title = "Persistance des plans d'eau selon la HER" , fill = "Persistance") +
  scale_fill_manual(values = c( "#18d0f0", "#2959ee"))  +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1)) +
  coord_flip()

ggplotly(repartition_pe_her_persistance)

# Autres analyses ----

## Synthèse générale ----

### Attribuer cd_dprt manquant ----

cd_manquant_pe <- pe %>%
  filter(cd_dprt == '' | is.na(cd_dprt))

plus_proche_dprt <- sf::st_nearest_feature(x = cd_manquant_pe,
                                              y = departements)

view(plus_proche_dprt)

dist <- st_distance(cd_manquant_pe, departements[plus_proche_dprt,], by_element = TRUE)

cd_dprt_pe <- cd_manquant_pe %>% 
  cbind(dist) %>% 
  cbind(departements[plus_proche_dprt,]) %>% 
  select(cdoh_plando,
         dprt_plus_proche = INSEE_DEP,
         distance_m = dist) %>% 
  sf::st_drop_geometry() %>% 
  mutate(distance_km = round(distance_m/1000,3))

### Mise à jour du code departement ----

pe_vf <- pe  %>%
  left_join(cd_dprt_pe, by = c("cdoh_plando" = "cdoh_plando")) %>%  
  mutate(cd_dprt = ifelse(
    (cd_dprt == '' | is.na(cd_dprt)),
    dprt_plus_proche,
    cd_dprt)) %>%
  distinct()

test <- pe_vf %>%
  filter(is.na(cd_dprt) | cd_dprt == '')

### Construction de la table de synthèse ----

synthese_pe_tot_ze <- 
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_vf %>% 
      mutate(gid = 711) %>%
      units::drop_units() %>%
      st_drop_geometry(),
    var_id_polygone = gid,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  ) %>%
  dplyr::select(-surf_pe_tot)

synthese_pe_tot_dprt <- 
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_vf %>% 
      units::drop_units() %>%
      st_drop_geometry(),
    var_id_polygone = cd_dprt,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pe_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = TRUE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  ) %>%
  dplyr::select(-surf_pe_tot)

synthese_pehm_hors_mares_tot_dprt <- 
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_vf %>%
      filter(mare == 0) %>%
      units::drop_units() %>%
      st_drop_geometry(),
    var_id_polygone = cd_dprt,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  ) %>%
  dplyr::select(-surf_pe_tot)

synthese_pehm_hors_mares_tot_ze <- 
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_vf %>% 
      mutate(gid = 711) %>%
      filter(mare == 0) %>%
      units::drop_units() %>%
      st_drop_geometry(),
    var_id_polygone = gid,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_pehm_tot,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  ) %>%
  dplyr::select(-surf_pe_tot)

synthese_mareshm_tot_dprt <- 
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_vf %>%
      filter(mare == 1) %>%
      units::drop_units() %>%
      st_drop_geometry(),
    var_id_polygone = cd_dprt,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_mares_hm,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  ) %>%
  dplyr::select(-surf_pe_tot)

synthese_mareshm_tot_ze <- 
  compter_sommer_surfaces_dans_polygone(
    couche_surface = pe_vf %>% 
      mutate(gid = 711) %>%
      filter(mare == 1) %>%
      units::drop_units() %>%
      st_drop_geometry(),
    var_id_polygone = gid,
    var_a_sommer = surface_m2,
    var_nb_objets = nb_mares_hm,
    var_somme_surfaces = surf_pe_tot,
    zone_marais_incluse = FALSE,
    seulement_permanent = FALSE, 
    seulement_tdbv = FALSE,
    seulement_connecte = FALSE, 
    seulement_sur_cours = FALSE
  ) %>%
  dplyr::select(-surf_pe_tot)

### synthese table ----

ze_synth_pe <- zone_etude %>%
#  mutate(Superficie_km2 = st_area(geom)/1000000,
#         surface_marais_km2 = coalesce(surf_marais,0)/1000000) %>%
  st_drop_geometry() %>%
  mutate("Territoire" = "Zone d'étude",
         "INSEE_DEP" = 53) %>%
  dplyr::select(Territoire, INSEE_DEP, gid, surface_km2, surface_marais_km2) %>%
  left_join(synthese_pe_tot_ze, 
            join_by(gid == gid)) %>%
  left_join(synthese_pehm_hors_mares_tot_ze, 
            join_by(gid == gid)) %>%
  left_join(synthese_mareshm_tot_ze, 
            join_by(gid == gid)) %>%
  dplyr::select(Territoire, INSEE_DEP, surface_km2, surface_marais_km2, nb_pe_tot, nb_pehm_tot, nb_mares_hm) %>%
  drop_units() %>%
  mutate(Superficie_km2 = surface_km2) %>%
  select(-surface_km2)

dprt_synth_pe <- departements %>%
  mutate(Superficie_km2 = st_area(geometry)/1000000) %>%
  st_drop_geometry() %>%
  dplyr::select(NOM_DEP, INSEE_DEP, Superficie_km2, surface_marais_km2) %>%
  left_join(synthese_pe_tot_dprt, 
            join_by(INSEE_DEP == cd_dprt)) %>%
  left_join(synthese_pehm_hors_mares_tot_dprt, 
            join_by(INSEE_DEP == cd_dprt)) %>%
  left_join(synthese_mareshm_tot_dprt, 
            join_by(INSEE_DEP == cd_dprt)) %>%
  filter(INSEE_DEP == '22' |
           INSEE_DEP == '29' |
           INSEE_DEP == '35' |
           INSEE_DEP == '44' |
           INSEE_DEP == '49' |
           INSEE_DEP == '53' |
           INSEE_DEP == '56' |
           INSEE_DEP == '72' |
           INSEE_DEP == '85' ) %>%
  mutate("Territoire" = NOM_DEP) %>%
  dplyr::select(Territoire, Superficie_km2, surface_marais_km2, INSEE_DEP, nb_pe_tot, nb_pehm_tot, nb_mares_hm) %>%
  drop_units()

bzh_synth_pe <- dprt_synth_pe %>%
  filter(INSEE_DEP == '22' |
           INSEE_DEP == '29' |
           INSEE_DEP == '35' |
           INSEE_DEP == '56' ) %>%
  mutate(Territoire = 'Bretagne') %>%
  group_by(Territoire) %>%
  summarise(Territoire = 'Bretagne',
            Superficie_km2 = sum(Superficie_km2),
            surface_marais_km2 = sum(surface_marais_km2),
            nb_pe_tot = sum(nb_pe_tot), 
            nb_pehm_tot = sum(nb_pehm_tot),
            nb_mares_hm = sum(nb_mares_hm)) %>%
  drop_units()

pdl_synth_pe <- dprt_synth_pe %>%
  filter(INSEE_DEP == '44' |
           INSEE_DEP == '49' |
           INSEE_DEP == '53' |
           INSEE_DEP == '72' |
           INSEE_DEP == '85' ) %>%
  mutate(Territoire = 'Pays-de-la-Loire') %>%
  group_by(Territoire) %>%
  summarise(Territoire = 'Pays-de-la-Loire',
            Superficie_km2 = sum(Superficie_km2),
            surface_marais_km2 = sum(surface_marais_km2),
            nb_pe_tot = sum(nb_pe_tot), 
            nb_pehm_tot = sum(nb_pehm_tot),
            nb_mares_hm = sum(nb_mares_hm)) %>%
  drop_units()

interreg_synth_pe <- dprt_synth_pe %>%
  mutate(Territoire = 'Bretagne et Pays-de-la-Loire') %>%
  group_by(Territoire) %>%
  summarise(Territoire = 'Bretagne et Pays-de-la-Loire',
            Superficie_km2 = sum(Superficie_km2),
            surface_marais_km2 = sum(surface_marais_km2),
            nb_pe_tot = sum(nb_pe_tot), 
            nb_pehm_tot = sum(nb_pehm_tot),
            nb_mares_hm = sum(nb_mares_hm)) %>%
  drop_units()

table_tot_synth_pe <- dplyr::bind_rows(ze_synth_pe,
                                       interreg_synth_pe, 
                                       bzh_synth_pe, 
                                       pdl_synth_pe, 
                                       dprt_synth_pe %>%
                                       dplyr::select(-INSEE_DEP)) %>%
  mutate("Superficie (km²)" = round(Superficie_km2),
         "Superficie de marais retenue (km²)" = round(surface_marais_km2),
         "Nombre total de plans d'eau" = nb_pe_tot,
         "Densité de plans d'eau par Km²" = round(nb_pe_tot/Superficie_km2, 2),
         "Nombre de plans d'eau (hors mares et marais)" = nb_pehm_tot,
         "Densité de plans d'eau par Km² (hors mares et marais)" = round(nb_pehm_tot/(Superficie_km2-surface_marais_km2), 2),
         "Nombre de mares (hors marais)" = nb_mares_hm,
         "Densité de mares par Km² (hors marais)" = round(nb_mares_hm/(Superficie_km2-surface_marais_km2), 2)) %>%
  dplyr::select(-Superficie_km2, -surface_marais_km2,-nb_pe_tot, -nb_pehm_tot, -nb_mares_hm, -INSEE_DEP)

## Diagramme de vent ----

pe_select <- pe_vf %>% 
  filter(mare == 0 & zone_marais == 0) %>%
  select(cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>% 
  column_to_rownames("cdoh_plando")

pe_select <- pe_select %>% 
  mutate("Sur cours" = ifelse(connecte_lh == 1, 1, 0),
         "Sur source" = ifelse(connecte_source == 1, 1, 0),
         "Sur nappe" = ifelse(connecte_nappe == 1, 1, 0),
         "Sur zone humide" = ifelse(zhp == 1, 1, 0)) %>%
  select(-connecte_rh,
         -connecte_source,
         -connecte_lh,
         -connecte_nappe,
         -zhp)

diagramme_vent <- upset(pe_select,
                        c("Sur cours", "Sur source", "Sur nappe", "Sur zone humide"),
                        name = "Alimentation en eau des plans d'eau \nde la zone d'étude",
                        queries = list(upset_query(set = 'Sur cours', fill='darkblue'),
                                       upset_query(set = 'Sur source', fill='lightblue'),
                                       upset_query(set = 'Sur nappe', fill='violet'),
                                       upset_query(set = 'Sur zone humide', fill='#259d75')),
                        base_annotations = list("Nombre de plans d'eau" = (intersection_size(bar_number_threshold = 1,
                                                                                             width = 0.5,
                                                                                             text = list(angle = 45,
                                                                                                         hjust = -0.2,
                                                                                                         vjust = 0.2,
                                                                                                         size = 3))) +
                                                  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                                                                     labels = scales::number_format(big.mark = " "),
                                                                     limits = c(0, 50000)) + 
                                                  theme(panel.grid.major = element_blank(),
                                                        panel.grid.minor = element_blank(),
                                                        axis.line = element_line(colour='black'))),
                        stripes = upset_stripes(geom = geom_segment(size = 12),
                                                colors = c('grey95', 'white')),
                        matrix = intersection_matrix(geom = geom_point(shape = 'circle filled',
                                                                       size = 4,
                                                                       stroke = 0.45)),
                        set_sizes = (upset_set_size(geom = geom_bar(width = 0.4)) + 
                                       theme(axis.line.x = element_line(colour = 'black'),
                                             axis.ticks.x = element_line())) +
                          labs(y = "Nombre de plans d'eau") +
                          scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                                             labels = scales::number_format(big.mark = ""),
                                             trans = "reverse"),
                        sort_sets = 'descending',
                        sort_intersections = 'descending')
diagramme_vent

pe_select_mares_marais <- pe_vf %>%
  select(cdoh_plando,
         mare,
         zone_marais,
         Persistanc, 
         zone_marais) %>% 
  st_drop_geometry() %>% 
  column_to_rownames("cdoh_plando")

pe_select_mares_marais <- pe_select_mares_marais %>% 
  mutate("En zone de marais" = ifelse(zone_marais == 1, 1, 0),
         "Mares" = ifelse(mare == 1, 1, 0),
         "Permanents" = ifelse(Persistanc == 'permanent', 1, 0)) %>%
  select(-mare,
         -zone_marais,
         -Persistanc)

diagramme_vent_mares_marais <- upset(pe_select_mares_marais,
                                     c("En zone de marais", "Mares", "Permanents"),
                                     name = "Catégories de plans d'eau \nde la zone d'étude",
                                     queries = list(upset_query(set = 'En zone de marais', fill='brown'),
                                                    upset_query(set = 'Mares', fill='darkgreen'),
                                                    upset_query(set = 'Permanents', fill='lightblue')),
                                     base_annotations = list("Nombre de plans d'eau" = (intersection_size(bar_number_threshold = 1,
                                                                                                          width = 0.5,
                                                                                                          text = list(angle = 45,
                                                                                                                      hjust = -0.2,
                                                                                                                      vjust = 0.2,
                                                                                                                      size = 3))) +
                                                               scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                                                                                  labels = scales::number_format(big.mark = " "),
                                                                                  limits = c(0, 125000)) + 
                                                               theme(panel.grid.major = element_blank(),
                                                                     panel.grid.minor = element_blank(),
                                                                     axis.line = element_line(colour='black'))),
                                     stripes = upset_stripes(geom = geom_segment(size = 12),
                                                             colors = c('grey95', 'white')),
                                     matrix = intersection_matrix(geom = geom_point(shape = 'circle filled',
                                                                                    size = 4,
                                                                                    stroke = 0.45)),
                                     set_sizes = (upset_set_size(geom = geom_bar(width = 0.4)) + 
                                                    theme(axis.line.x = element_line(colour = 'black'),
                                                          axis.ticks.x = element_line())) +
                                       labs(y = "Nombre de plans d'eau") +
                                       scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                                                          trans = "reverse"),
                                     sort_sets = 'descending',
                                     sort_intersections = 'descending')

diagramme_vent_mares_marais

## Table des plans d'eau et du réseau hydro ----

table_pe_alimentation <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0) %>%
  filter(cd_dprt == '22' |
           cd_dprt == '29' |
           cd_dprt == '35' |
           cd_dprt == '56' |
           cd_dprt == '44' |
           cd_dprt == '49' |
           cd_dprt == '53' |
           cd_dprt == '72' |
           cd_dprt == '85') %>%
  mutate(emprise = 'Bretagne et Pays-de-la-Loire') %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = 'Bretagne et Pays-de-la-Loire',
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2)
            )

table_pe_alimentation_bzh <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0) %>%
  filter(cd_dprt == '22' |
           cd_dprt == '29' |
           cd_dprt == '35' |
           cd_dprt == '56' ) %>%
  mutate(emprise = 'Bretagne') %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = 'Bretagne',
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2)
  )

table_pe_alimentation_pdl <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0) %>%
  filter(cd_dprt == '44' |
           cd_dprt == '49' |
           cd_dprt == '53' |
           cd_dprt == '72' |
           cd_dprt == '85') %>%
  mutate(emprise = 'Pays-de-la-Loire') %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = 'Pays-de-la-Loire',
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2)
  )

table_pe_alimentation_tot <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0) %>%
  mutate(emprise = "Zone d'étude") %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = "Zone d'étude",
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2)
  )

table_pe_alimentation_22 <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0 & cd_dprt == '22') %>%
  mutate(emprise = "Côtes d'Armor") %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = "Côtes d'Armor",
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2))

table_pe_alimentation_29 <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0 & cd_dprt == '29') %>%
  mutate(emprise = "Finistère") %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = "Finistère",
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2))

table_pe_alimentation_35 <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0 & cd_dprt == '35') %>%
  mutate(emprise = "Ille et Vilaine") %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = "Ille et Vilaine",
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2))

table_pe_alimentation_44 <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0 & cd_dprt == '44') %>%
  mutate(emprise = "Loire Atlantique") %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = "Loire Atlantique",
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2))

table_pe_alimentation_49 <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0 & cd_dprt == '49') %>%
  mutate(emprise = "Maine et Loire") %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = "Maine et Loire",
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2))

table_pe_alimentation_53 <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0 & cd_dprt == '53') %>%
  mutate(emprise = "Mayenne") %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = "Mayenne",
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2))

table_pe_alimentation_56 <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0 & cd_dprt == '56') %>%
  mutate(emprise = "Morbihan") %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = "Morbihan",
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2))

table_pe_alimentation_72 <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0 & cd_dprt == '72') %>%
  mutate(emprise = "Sarthe") %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = "Sarthe",
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2))

table_pe_alimentation_85 <- pe_vf %>%
  filter(mare == 0 & zone_marais == 0 & cd_dprt == '85') %>%
  mutate(emprise = "Vendée") %>%
  select(emprise, 
         cdoh_plando,
         connecte_rh,
         connecte_source,
         connecte_lh,
         connecte_nappe,
         zhp) %>% 
  st_drop_geometry() %>%
  summarise("Emprise" = "Vendée",
            "Nombre total de plans d'eau (hors mares et marais)" = n(), 
            "Nombre de plans d'eau en zone humide" = sum(zhp),
            "% en zone humide" = round(sum(zhp)*100/n(), 2),
            "Nombre de plans d'eau connectés" = sum(connecte_rh), 
            "% connecte" = round(sum(connecte_rh)*100/n(), 2),
            "Nombre de plans d'eau sur nappe" = sum(connecte_nappe), 
            "% sur nappe" = round(sum(connecte_nappe)*100/n(), 2),
            "Nombre de plans d'eau sur cours" = sum(connecte_lh), 
            "% sur cours" = round(sum(connecte_lh)*100/n(), 2),
            "Nombre de plans d'eau sur source" = sum(connecte_source), 
            "% sur source" = round(sum(connecte_source)*100/n(), 2))

table_tot_pe_alimentation <- dplyr::bind_rows(table_pe_alimentation_tot, 
                                              table_pe_alimentation, 
                                              table_pe_alimentation_pdl, 
                                              table_pe_alimentation_bzh, 
                                              table_pe_alimentation_22, 
                                              table_pe_alimentation_29, 
                                              table_pe_alimentation_35, 
                                              table_pe_alimentation_44, 
                                              table_pe_alimentation_53, 
                                              table_pe_alimentation_56, 
                                              table_pe_alimentation_72, 
                                              table_pe_alimentation_85)

openxlsx::write.xlsx(table_tot_pe_alimentation %>%
                       as.data.frame(), 
                     file = "data/outputs/table_tot_pe_alimentation.xlsx" )

write.csv(table_tot_pe_alimentation, file = "data/outputs/table_tot_pe_alimentation.csv" )

table_alimentation <- data.table::fread(file = "data/outputs/table_tot_pe_alimentation.csv",
                     encoding = "UTF-8")

table_tot_pe_alimentation %>%
  as.data.frame() %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 12) 


## Surface des PE ----

pe <- pe %>%
  st_drop_geometry() %>%
  filter(Persistanc != 'inconnue')

histo_surface_pe <-
  ggplot(data = pe, 
         aes(x = surface_m2/10000)) + 
  geom_histogram(bins = 100,fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + labs(
    x = "Superficie du plan d'eau (Ha)",
    y = "Nombre de plans d'eau",
    title = "Répartition des Plans d'eau selon leur superficie", width=40)

histo_surface_pe

ggplotly(histo_surface_pe_persistance)

histo_surface_pe_persistance <- pe_vf %>% 
 # sample_n(10000) %>% 
  filter(!is.na(Persistanc) & Persistanc!= '' & Persistanc != 'inconnue') %>%
  st_drop_geometry() %>%
  ggplot(aes(x = surface_m2,
             fill = Persistanc)
         ) +
  geom_histogram(bins = 50, position = "dodge") + 
  # geom_density(alpha = 0.3, position = "stack") +
  scale_fill_manual(values = c( "#18d0f0", "#2959ee")) +
  labs(
    x = "Superficie des plans d'eau (m², échelle log)",
    y = "Nombre de plans d'eau", 
    fill = "Persistance",
    title = str_wrap("Répartition des plans d'eau selon leur superficie", width=50)) +
  scale_x_log10(labels = function(x) format(x, scientific = FALSE, big.mark = " "),
                breaks=c(100,1000,10000, 100000)) +
  theme(axis.title = element_text(size = 8, margin = margin(b = 20)),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

pe_vf_ratio <- pe %>%
  left_join(bv_me %>%
              filter(!is.na(Q5MOY_max) & Q5MOY_max != ''& Q5MOY_max>=0) %>%
              st_drop_geometry() %>%
              select(cdeumassed, QAMOY_max, Q5MOY_max),
            join_by( 'cd_me'== 'cdeumassed')) %>%
  mutate(ratio_q5_qa = Q5MOY_max/QAMOY_max) %>%
  filter(!is.na(QAMOY_max))

repartition_pe_ratio_persistance <-
  ggplot(pe_vf_ratio %>%
           filter(!is.na(Q5MOY_max))) +
  aes(x = ratio_q5_qa, fill = Persistanc) +
  geom_bar(position = "fill") +
  xlab("Ratio de débits") +
  ylab("Proportion") +
  labs(fill = "Persistance") +
  scale_y_continuous(labels = scales::percent) 

## Distance au cours d'eau des PE ----

histo_distance_ce_pe <-
  ggplot(data = pe, 
         aes(x = distance_topage)) + 
  geom_histogram(bins = 2000, fill="#2374ee") +
  labs(
    x = "Distance au plus proche cours d'eau (m)",
    y = "Nombre de plans d'eau",
    title = "Répartition des plans d'eau selon leur distance au plus proche cours d'eau", width=40)+ 
  coord_cartesian(xlim = c(0, 1500)) +
  theme(axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

ggplotly(histo_distance_ce_pe_persistance)

histo_distance_ce_pe_persistance <-
  ggplot(data = pe_vf %>% 
           filter(!is.na(Persistanc) & Persistanc!= '' & Persistanc != 'inconnue') %>%
           st_drop_geometry(), 
         aes(x = distance_topage,
             fill = Persistanc)) + 
  geom_histogram(bins =500, position = "dodge")+
  scale_fill_manual(values = c( "#18d0f0", "#2959ee"))  +
  labs(
    x = "Distance au plus proche cours d'eau (m)",
    y = "Nombre de plans d'eau", 
    fill = "Persistance",
    title = "Répartition des plans d'eau selon leur distance au plus proche cours \nd'eau", width=40)+ 
  coord_cartesian(xlim=c(0,1500)) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = " "))  +
  theme(axis.title = element_text(size = 8, margin = "Large margin"),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

## Distance à la source des PE ----

histo_distance_source_pe <-
  ggplot(data = pe, 
         aes(x = distance_source)) + 
  geom_histogram(bins = 2000,fill="#2374ee") + 
  labs(
    x = "Distance à la source la plus proche (m)",
    y = "Nombre de plans d'eau",
    title = "Répartition des plans d'eau selon leur distance à la source la plus \nproche", width=40)+ 
  coord_cartesian(xlim = c(0, 2500)) +
  theme(axis.title = element_text(size = 8, margin = "Large margin"),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

ggplotly(histo_distance_source_pe_persistance)

histo_distance_source_pe_persistance <-
  ggplot(data = pe_vf %>%
           filter(!is.na(Persistanc) & Persistanc!= '' & Persistanc != 'inconnue') %>% 
           st_drop_geometry(), 
         aes(x = distance_source,
             fill = Persistanc)) + 
  geom_histogram(bins =200, position = "dodge")+
  scale_fill_manual(values = c( "#18d0f0", "#2959ee"))  +
  labs(
    x = "Distance à la source la plus proche (m)",
    y = "Nombre de plans d'eau", 
    fill = "Persistance",
    title = "Répartition des plans d'eau selon leur distance à la source la plus \nproche", width=40)+ 
  coord_cartesian(xlim = c(0, 3000)) +
  theme(axis.title = element_text(size = 8, margin = "Large margin"),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

ggplotly(histo_distance_source_pe_persistance)


## Rang de strahler du plus proche cours d'eau ----

ce_topage <- ce_topage %>%
  mutate(long_topage_m = st_length(geometry))
  
strahler <- ce_topage %>%
  st_drop_geometry() %>%
  group_by(StreamOrde) %>%
  summarise(long_topage_km = sum(long_topage_m/1000))

pe_strahler <- pe_vf %>%
  st_drop_geometry() %>%
  group_by(StreamOrde) %>%
  summarise(nb_pe = n())

pe_strahler_perm <- pe_vf %>%
  st_drop_geometry() %>%
  filter(Persistanc == 'permanent')%>%
  group_by(StreamOrde) %>%
  summarise(nb_pe_perm = n())

pe_strahler_inter <- pe_vf %>%
  st_drop_geometry() %>%
  filter(Persistanc == 'intermittent') %>%
  group_by(StreamOrde) %>%
  summarise(nb_pe_inter = n())

pe_strahler <- pe_strahler %>%
  left_join(strahler, join_by("StreamOrde" == "StreamOrde")) %>%
  mutate(nb_pe_km_topage = nb_pe/long_topage_km) %>%
  drop_units()

strahler <- strahler %>%
  left_join(pe_strahler, join_by("StreamOrde" == "StreamOrde")) %>%
  left_join(pe_strahler_inter, join_by("StreamOrde" == "StreamOrde")) %>%
  left_join(pe_strahler_perm, join_by("StreamOrde" == "StreamOrde")) %>%
  drop_units()

dens_strahler <- strahler %>%
  mutate(dens_num_pe = nb_pe/long_topage_km, 
         dens_num_pe_perm = nb_pe_perm/long_topage_km,
         dens_num_pe_inter = nb_pe_inter/long_topage_km,)

histo_strahler_pe <-
  ggplot(data = pe %>% filter(StreamOrde > 0), 
         aes(x = StreamOrde)) + 
  geom_histogram(fill="#2374ee") + 
  labs(
    x = "Rang de Strahler du plus proche cours d'eau",
    y = "Nombre de plans d'eau",
    title = "Répartition des plans d'eau selon le rang de Strahler du plus proche cours \nd'eau", width=40) +
  scale_x_continuous(breaks = 1:8)

histo_strahler_pe_persistance <-
  ggplot(data = pe_vf %>% 
           st_drop_geometry() %>%
           filter(StreamOrde > 0) %>%
           filter(!is.na(Persistanc) & Persistanc!= '' & Persistanc != 'inconnue'), 
         aes(x = StreamOrde,
             fill = Persistanc)) + 
  geom_histogram(binwidth =  1, position = "stack")+
  scale_fill_manual(values = c( "#18d0f0", "#2959ee"))  + 
  labs(
    x = "Rang de Strahler du plus proche cours d'eau",
    y = "Nombre de plans d'eau", 
    fill = "Persistance",
    title = "Répartition des plans d'eau selon le rang de Strahler du plus \nproche cours d'eau", width=40) +
  scale_x_continuous(breaks = 1:8) +
  theme(axis.title = element_text(size = 8, margin = "Large margin"),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

ggplotly(histo_strahler_pe_persistance)

histo_strahler_pe <-
ggplot(data = pe_strahler %>% filter(StreamOrde > 0,
                                     !is.na(StreamOrde)), 
         aes(x = StreamOrde,
             y = nb_pe_km_topage)) + 
  geom_col(fill = "#5599ee", width = 0.2) +
  labs(
    x = "Rang de Strahler du plus proche cours d'eau",
    y = "Nombre de plans d'eau par km \nde linéaire hydrographique",
    title = "Densité linéaire de plans d'eau selon le rang de Strahler du plus \nproche cours d'eau", width=40) +
  scale_x_continuous(breaks = 1:8) +
  theme(axis.title = element_text(size = 8, margin = "Large margin"),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

histo_dens_lin_strahler <-
plot_ly(dens_strahler %>%
          filter(StreamOrde > 0 & !is.na(StreamOrde)), 
        type = 'bar', 
        x = ~StreamOrde, 
        y = ~dens_num_pe_perm, color = I("#2959ee"), name = "Permanent") %>%
  add_bars(y = ~dens_num_pe_inter, color = I("#18d0f0"), name = "Intermittent") %>% 
  layout(yaxis = list(title = "Nombre de plans d'eau par km de cours d'eau"),barmode = 'stack')  %>%
  layout(title = "Densité linéaire en plans d'eau selon le rang de Strahler\n de leur plus proche cours d'eau",
         xaxis = list(title = "Rang de Strahler"))
  

## Linéaire intercepté par pe ----

histo_intercept_pe <-
  ggplot(data = pe_vf %>%
           
           filter(!is.na(longueur_topage_intersecte)) %>%
           filter(!is.na(Persistanc) & Persistanc!= '' & Persistanc != 'inconnue') %>%
           st_drop_geometry(), 
         aes(x = longueur_topage_intersecte, 
             fill = Persistanc)) + 
  geom_histogram(binwidth = 10, position = "dodge")+
  scale_fill_manual(values = c( "#18d0f0", "#2959ee")) + 
  labs(
    x = "Linéaire hydrographique intercepté (m)",
    y = "Nombre de plans d'eau",
    fill = "Persistance",
    title = "Répartition des plans d'eau selon la longueur de linéaire intercepté", width=40) +
  coord_cartesian(xlim = c(0, 900)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = " "))+
  theme(axis.title = element_text(size = 8, margin = "Large margin"),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

histo_intercept_pe

## Linéaire intercepté des me ----

histo_prct_intercept_pe <-
  ggplot(data = bv_me, 
         aes(x = longueur_topage_intersecte_pe_tot*100 /longueur_ce_topage)) + 
  geom_histogram(bins = 100,fill="#2374ee")  + 
  labs(
    x = "Ratio du linéaire intercepté (%)",
    y = "Nombre de masses d'eau",
    title = "Répartition des masses d'eau selon leur ratio de linéaire intercepté", width=40)+
  theme(axis.title = element_text(size = 8, margin = "Large margin"),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

histo_prct_intercept_pe

bvme_type <- bv_me %>%
  mutate(type_me = case_when(
    substr(cdeumassed, 4,4) == 'R' ~ 'cours deau',
    substr(cdeumassed, 4,4) == 'L' ~ 'plan deau',
    substr(cdeumassed, 4,4) == 'C' ~ 'cotiere',
    substr(cdeumassed, 4,4) == 'T' ~ 'transition'))

histo_prct_intercept_pe_type <-
  ggplot(data = bvme_type, 
         aes(x = coalesce(longueur_topage_intersecte_pe_tot,0)*100 /longueur_ce_topage, 
         fill = type_me)) + 
  geom_histogram(binwidth = 2, position = "dodge") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "darkblue", "orange")) +     
  labs(
  x = "Ratio du linéaire du BV de la masse d'eau intercepté (%)",
  y = "Nombre de masses d'eau",
  title = "Répartition des masses d'eau selon leur ratio de linéaire intercepté \npar un plan d'eau", width=40, 
  fill = "Type de masse d'eau")+
  theme(axis.title = element_text(size = 8, margin = "Large margin"),
        axis.text = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)))

# Calculs ----

pe_tot <- pe_vf %>%
  select(cdoh_plando, 
         cd_dprt, 
         surface_m2, 
         distance_topage, 
         distance_source, 
         StreamOrde, 
         longueur_topage_intersecte,
         Persistanc, 
         mare, 
         zone_marais, 
         zhp, 
         connecte_rh, 
         connecte_lh, 
         connecte_source,
         connecte_nappe, 
         distance_source) %>%
  st_drop_geometry()

pe_bzh_pdl <- pe_tot %>%
  filter(cd_dprt == '22' |
           cd_dprt == '29' |
           cd_dprt == '35' |
           cd_dprt == '56' |
           cd_dprt == '44' |
           cd_dprt == '49' |
           cd_dprt == '53' |
           cd_dprt == '72' |
           cd_dprt == '85')

persistance_inconnue_null <- pe_vf %>%
  st_drop_geometry() %>% 
  filter(Persistanc == 'inconnue' | Persistanc == '' | is.na(Persistanc))

superficie_moy <- 
  mean(pe_tot$surface_m2)

superficie_med <- 
  median(pe_tot$surface_m2)

dist_ce <- pe_tot %>%
  filter(!is.na(distance_topage))  

distance_ce_moy <- 
  mean(dist_ce$distance_topage)

distance_ce_med <- 
  median(dist_ce$distance_topage)

dist_source <- pe_tot %>%
  filter(!is.na(distance_source))  

distance_source_moy <- 
  mean(dist_source$distance_source)

distance_source_med <- 
  median(dist_source$distance_source)

pe_tdbv <- pe_tot %>%
  filter(StreamOrde == 1 | StreamOrde == 2) 

pe_intercept <- pe_tot %>%
  filter(distance_topage == 0) 

longueur_intercept_moy <- 
  mean(pe_intercept$longueur_topage_intersecte)

longueur_intercept_med <- 
  median(pe_intercept$longueur_topage_intersecte)

prop_intercept_bv <- bv_me %>%
  filter(!is.na(longueur_ce_topage) &
         substr(cdeumassed, 4,4) != 'L') %>%
  select(longueur_ce_topage, longueur_topage_intersecte_pe_tot) %>%
  st_drop_geometry() %>%
  mutate(prop_ce_intercept_bv = coalesce(longueur_topage_intersecte_pe_tot,0)*100/coalesce(longueur_ce_topage,0))

prop_intercept_bv_moy <- 
  mean(prop_intercept_bv$prop_ce_intercept_bv)

prop_intercept_bv_med <- 
  median(prop_intercept_bv$prop_ce_intercept_bv)

pe_perm <- pe_tot %>%
  filter(Persistanc == 'permanent')

pe_perm_hors_mares_marais <- pe_tot %>%
  filter(Persistanc == 'permanent' & 
           mare == 0 & 
           zone_marais == 0)

pe_hors_mares_marais <- pe_tot %>%
  filter(mare == 0 & 
           zone_marais == 0)

pe_hors_mares_marais_44 <- pe_tot %>%
  filter(mare == 0 & 
           zone_marais == 0 &
           cd_dprt == 44)

pe_hors_mares_marais_56 <- pe_tot %>%
  filter(mare == 0 & 
           zone_marais == 0&
           cd_dprt == 56)

pe_hors_mares_marais_zh <- pe_tot %>%
  filter(mare == 0 & 
           zone_marais == 0 & 
           zhp == 1)

pe_hors_mares_marais_hors_zh_connecte <- pe_tot %>%
  filter(mare == 0 & 
           zone_marais == 0 & 
           zhp == 0 &
           connecte_rh == 0)

pe_hors_mares_marais_connecte <- pe_tot %>%
  filter(mare == 0 & 
           zone_marais == 0 & 
           connecte_rh == 1)

pe_hors_mares_marais_connecte_44 <- pe_tot %>%
  filter(mare == 0 & 
           zone_marais == 0 & 
           connecte_rh == 1 &
           cd_dprt == 44)

pe_hors_mares_marais_connecte_56 <- pe_tot %>%
  filter(mare == 0 & 
           zone_marais == 0 & 
           connecte_rh == 1 &
           cd_dprt == 56)

pe_hors_mares_marais_connecte_l <- pe_tot %>%
  filter(mare == 0 & 
           zone_marais == 0 & 
           connecte_lh == 1)

pe_hors_mares_marais_connecte_l_s_n <- pe_tot %>%
  filter(mare == 0 & 
           zone_marais == 0 & 
           connecte_lh == 1 & 
           connecte_source == 1 & 
           connecte_nappe == 1)

pe_mare <- pe_tot %>%
  filter(mare == 1)

pe_marais <- pe_tot %>%
  filter(zone_marais == 1)

## densite_marais 

pe_marais <- pe_vf %>% 
  st_intersection(marais_zone_etude) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry()

sf::write_sf(obj = marais_decoup_me, dsn = "data/outputs/marais_decoup_me_20240110.gpkg")

densite_marais_pe <-
  compter_sommer_simple_surfaces_dans_polygone(
    couche_surface = pe_marais %>% 
      units::drop_units(),
    var_id_polygone = id,
    var_a_sommer = surface_intersect,
    var_nb_objets = nb_pe,
    var_somme_surfaces = surf_pe) 


marais_zone_etude <- marais_zone_etude %>%
  dplyr::left_join(densite_marais_pe) %>%
  units::drop_units()

marais_zone_etude <- marais_zone_etude %>%
  mutate(superficie_km2 = st_area(geom)/1000000,
         dens_num_pe_tot = nb_pe/superficie_km2,
         dens_surf_pe_tot = ((surf_pe/1000000)*100)/superficie_km2) %>%
  st_drop_geometry() %>%
  drop_units()

## fin densité marais

pe_infeg500 <- pe_tot %>%
  filter(surface_m2 <= 500)

pe_sup500_inf1000 <- pe_tot %>%
  filter(surface_m2> 500 & surface_m2 < 1000)

pe_sup1000 <- pe_tot %>%
  filter(surface_m2 >= 1000)

pe_dist_ce_barre1 <- pe_tot %>%
  filter(distance_topage < 24.105)

pe_dist_ce_10 <- pe_tot %>%
  filter(distance_topage <= 10)

pe_dist_ce_10_perm <- pe_tot %>%
  filter(distance_topage <= 10 & Persistanc == 'permanent')

pe_dist_ce_10_int <- pe_tot %>%
  filter(distance_topage <= 10 & Persistanc == 'intermittent')

pe_dist_ce_35 <- pe_tot %>%
  filter(distance_topage <= 35)

pe_dist_ce_100 <- pe_tot %>%
  filter(distance_topage <= 100)

pe_dist_ce_sup1000 <- pe_tot %>%
  filter(distance_topage > 1000)

pe_dist_source_100 <- pe_tot %>%
  filter(distance_source <= 100)
##

pe_rang1 <- pe_tot %>%
  filter(StreamOrde == 1)

dens_strahler_r1 <- dens_strahler %>%
  filter(StreamOrde == 1)

dens_strahler_r2 <- dens_strahler %>%
  filter(StreamOrde == 2)

dens_strahler_r8 <- dens_strahler %>%
  filter(StreamOrde == 8)

percentiles_superficie_pe <-
  quantile(pe_vf$surface_m2, c(.10, .25, .50, .75, .90))

pe_dist <- pe_vf %>%
  filter(!is.na(distance_topage))

percentiles_dist_ce <-
  quantile(pe_dist$distance_topage, c(.10, .25, .50, .75, .90))

pe_dist_source <- pe_vf %>%
  filter(!is.na(distance_source))

percentiles_dist_source <-
  quantile(pe_dist_source$distance_source, c(.10, .25, .50, .75, .90))

percentiles_dist_source <-
  quantile(pe_dist_source$distance_source, c(.10, .25, .50, .75, .90))

pe_q2 <- pe_q %>%
  mutate(ratio = Q5MOY_MN/QAMOY_MN) %>%
  filter(!is.na(ratio))

percentiles_severite_etiages <-
  quantile(pe_q2$ratio, c(.10, .25, .50, .75, .90))

me_ce <- bv_me %>%
  st_drop_geometry() %>%
  filter(substr(cdeumassed, 4,4) == 'R') %>%
  mutate(prop_lin_intercept = (coalesce(longueur_topage_intersecte_pe_tot, 0)*100/longueur_ce_topage))

percentiles_intercept_bv <-
  quantile(me_ce$prop_lin_intercept, c(.10, .25, .50, .75, .90)) 

percentiles_superficie <-
  quantile(pe_tot$surface_m2, c(.01, .10, .20, .40, .50, .60, .80, .90, .99)) 
  data.frame(rename(surface_m2 = .))

  rownames_to_column() 
  as_tibble() %>%
  rename(surface_m2 = value)
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 12) 

mutate(Superficie_m2 = x)

rename

view(percentiles_superficie)

pe_dist_ce_50 <- pe %>%
  filter(distance_topage <= 50)

summarise(pe_dist_ce_50)

# Test ANOVA ----

## ANOVA litho ----

litho <- litho %>%
  st_drop_geometry()

densnum_geol <- litho_densite_pe %>% left_join(litho, join_by(lithologie == descr)) %>%
  select(value, 
         lithologie,
         dens_num_pehm) %>% 
  st_drop_geometry() %>%
  unique() %>%
  column_to_rownames("value")

densnum_geol %>%
  group_by(lithologie) %>%
  identify_outliers() # il y a des valeurs aberrantes...

model_densnum_geol <- lm(dens_num_pehm ~ lithologie,
                         data = densnum_geol)

ggqqplot(residuals(model_densnum_geol))

shapiro_test(residuals(model_densnum_geol))


## Analyse stat ratio de débit et densité numérique ----

library(MASS)
library(stats)
library(tidyverse)
library(PerformanceAnalytics)

bv_me_stat_q <- bv_me %>%
  filter(Q5MOY_max >= 0 & !is.na(Q5MOY_max) & Q5MOY_max != '', 
         substring(cdeumassed, 4, 4) == 'R') %>%
  mutate(ratio_q5_qa = (coalesce(Q5MOY_max,0) / coalesce(QAMOY_max,0)),
         densite_pehm_hors_mares = coalesce(nb_pehm_tot,0)/(surface_me-coalesce(surface_marais, 0))/1000000) %>%
  dplyr::select(cdeumassed, densite_pehm_hors_mares, ratio_q5_qa) %>%
  mutate(across(densite_pehm_hors_mares, log), 
         across(ratio_q5_qa, function(x) log(1+x)))

bv_me_stat_q <- bv_me_stat_q %>%
  filter(!is.na(ratio_q5_qa)) %>%
  mutate(cdeumassed=as.character(cdeumassed)) %>%
  st_drop_geometry()

# Filtre éventuel selon la distance de cook

bv_me_stat_q <- bv_me_stat_q %>%
  left_join(dist_cook) %>%
  filter(!is.na(cook) &
           cook < 4/868)

4/868

corr_tot_test_stat_q <- bv_me_stat_q %>% 
  dplyr::select(-cdeumassed) %>%
  cor(method = "pearson")

corrplot::corrplot(corr_tot_test_stat_q, order="hclust",tl.srt=45, addrect = 8)

names(bv_me_stat_q)

bv_me_stat_q <- bv_me_stat_q %>% 
  dplyr::select(-cook) %>%
  filter(cdeumassed != 'FRHR348') %>%
  column_to_rownames("cdeumassed")

mod1 <- lm(densite_pehm_hors_mares ~
                 ratio_q5_qa,
               data = bv_me_stat_q)

summary(mod1)

plot(mod1)

cook <- cooks.distance(mod1)

dist_cook <- as.data.frame(cook) %>% 
  rownames_to_column("cdeumassed") 

sf::write_sf(obj = dist_cook, dsn = "data/outputs/distance_cook_test_q_20240605.gpkg")


mod2 <- MASS::stepAIC(mod1)

summary(mod2)

# Sauvegarde ----


save( pe_vf,
      pe_tot,
      pe_bzh_pdl,
      persistance_inconnue_null,
      superficie_moy,
      superficie_med,
      dist_ce,
      distance_ce_moy,
      distance_ce_med, 
      dist_source,
      distance_source_moy,
      distance_source_med,
      pe_tdbv,
      pe_intercept,
      longueur_intercept_moy,
      longueur_intercept_med,
      prop_intercept_bv_moy,
      prop_intercept_bv_med,
      pe_hors_mares_marais_44,
      pe_hors_mares_marais_56,
      pe_hors_mares_marais_zh,
      pe_hors_mares_marais_hors_zh_connecte ,
      pe_hors_mares_marais_connecte,
      pe_hors_mares_marais_connecte_44, 
      pe_hors_mares_marais_connecte_56,
      pe_hors_mares_marais_connecte_l, 
      pe_hors_mares_marais_connecte_l_s_n,
      pe_perm,
      pe_mare, 
      pe_marais, 
      pe_perm_hors_mares_marais,
      pe_hors_mares_marais,
      pe_perm_hors_mares_marais,
      marais_zone_etude,
      pe_infeg500, 
      pe_sup500_inf1000, 
      pe_sup1000, 
      pe_dist_ce_barre1, 
      pe_dist_ce_10, 
      pe_dist_ce_10_perm,
      pe_dist_ce_10_int,
      pe_dist_ce_35, 
      pe_dist_ce_100,  
      pe_dist_ce_sup1000,
      pe_dist_source_100,
      pe_rang1,
      dens_strahler_r1,
      dens_strahler_r2,
      dens_strahler_r8,
      me_ce,
      pe_connecte,
      bv_me,
      litho_densite_pe,
      table_litho_densite_pe,
      densite_numerique_pe_litho,
      densite_numerique_pehm_hors_mares_litho,
      densite_surfacique_pe_litho,
      densite_surfacique_pehm_hors_mares_litho,
      pe_taille,
      surface_moyenne_pehm_litho,
      surface_mediane_pehm_litho,
      violin_surface_pehm_litho,
      violin_surface_pehm_litho2,
      boxsplot_surface_pehm_litho,
      repartition_pe_litho_persistance,
      her_zone_etude,
      her_densite_pe,
      table_her_densite_pe,
      densite_numerique_pe_her,
      densite_numerique_pehm_hors_mares_her,
      densite_surfacique_pe_her,
      densite_surfacique_pehm_hors_mares_her,
      violin_surface_pe_her,
      pe_q,
      pe_q_vf,
      pe_q_mares_marais,
      pe_q_qa_nul,
      pe_q_q5_nul,
      pe_qa_q5_nul,
      pe_q_non_nul,
      pe_q_inf40,
      pe_q_40_20,
      pe_q_20_10,
      pe_q_sup10,
      pe_q_inf1,
      repartition_pe_rh,
      repartition_pehm_rh,
      repartition_bv_rh,
      repartition_com_rh,
      bv_me_q,
      pe_ratio,
      table_bv_me_q,
      densite_numerique_pe_rh,
      densite_numerique_pehm_rh,
      densite_surfacique_pe_rh,
      densite_surfacique_pehm_rh,
      densite_numerique_pehm_rh_bvme_logx,
      densite_surfacique_pehm_rh_bvme_logx,
      pe_taille_q,
      pehm_taille_q,
      surface_moyenne_pe_rh,
      surface_moyenne_pehm_rh,
      surface_mediane_pe_rh,
      surface_mediane_pehm_rh,
      violin_surface_pe_ratio,
      surface_pehm_rh,
      bv_me_type,
      table_bv_me_type,
      densite_numerique_pe_type,
      densite_numerique_pehm_type,
      densite_surfacique_pe_type,
      densite_surfacique_pehm_type,
      table_tot_synth_pe, 
      diagramme_vent,
      diagramme_vent_mares_marais,
      table_tot_pe_alimentation,
      histo_surface_pe_persistance, 
      histo_distance_ce_pe_persistance,
      histo_distance_source_pe_persistance,
      pe_strahler, 
      dens_strahler, 
      histo_strahler_pe,
      histo_strahler_pe_persistance,
      histo_dens_lin_strahler,
      histo_intercept_pe,
      histo_prct_intercept_pe_type,
      repartition_pe_q_persistance,
      repartition_pe_me_persistance,
      repartition_pe_her_persistance,
      bv_me_stat_q,
      mod1, 
      mod2,
      percentiles_superficie_pe, 
      percentiles_dist_ce,
      percentiles_dist_source,
      percentiles_intercept_bv,
      percentiles_severite_etiages,
      file = "data/outputs/w_autres_hypotheses.RData")

load(file = "data/outputs/w_autres_hypotheses.Rdata")

