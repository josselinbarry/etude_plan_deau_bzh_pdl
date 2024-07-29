# Etude de la pression des plans d’eau sur les milieux aquatiques en Bretagne et Pays de la Loire

Ce projet vise à analyser la structure d’implantation des plans d’eau sur le territoire Bretagne Pays de la Loire.

En second lieu, étudier les liens de corrélation entre ces variables d’implantation territoriales et certaines métriques de qualité.

## 1. Un premier script pour constituer et qualifier la couche plan-d’eau :

-	La couche surfaces_elementaires importée dans R a été préalablement préparée de la manière suivante :
    - Téléchargement de la couche **surfaces_elementaires de la BD Topage** : https://www.sandre.eaufrance.fr/atlas/atlas/api/records/7fa4c224-fe38-4e2c-846d-dcc2fa7ef73e 
    - Sélection des surfaces élémentaires de la zone d'étude
    - Suppression des chevauchements de zones par "Couper-collé" (sans recouvrement de zones) des objets les plus petits aux objets les plus gros
    - Suppression des invalidités de géométries (extension Qgis "Nettoyeur de polygones")
    - Création et calcul d'un champ **coef_gravelius** = perimetre / (2 x sqrt(pi x area)
    - Création et renseignement des attributs :
        - **ecoulement_naturel** (0/1) : "NatureSE" in (canal, écoulement canalisé, écoulement naturel) puis corrections manuelles)
        - **zone_marais** (0/1) : attribution manuelle à partir des zones de marais du Scan25
        - **marais_uhc1** (0/1) : intersection avec la couche UHC1  et ("Persistanc" != 'permanent' and  "NatureSE" not in ( 'Plan d''eau - mare' ,  'Plan d''eau - retenue' ,  'Plan d''eau - réservoir', 'PE-réservoir-bassinorage', 'Ecoulement naturel' )) puis corrections manuelles loire et vendée) (couche "UHC123internet" depuis le flux WFS du Forum des Marais de l'Atlantique (FMA) : http://wms.reseau-zones-humides.org/cgi-bin/wmsfma)
  
- Import de la couche dans R, puis utilisation du premier script : 
**w_10_geom_plan_deau_202311.R**

- Création et renseignement des attributs suivants :
    -	**bassin_orage** (0/1) : "NatureSE" = 'PE-réservoir-bassinorage'
    -	**bassin_eru** (0/1) : "coef_gravelius" < 1,015 and "NatureSE" not in ('Plan d''eau - mare', 'PE-réservoir-bassinorage', 'PE - réservoir -piscicult', 'Ecoulement naturel')
    -	**a_retirer** (0/1) : qualification des surfaces qui sont des écoulements naturels, des zones estuariennes, des zones de marais UHC1, des bassins d’orage ou des bassins ERU

- Obtention de la "surfaces élémentaires de plans d'Eau" en filtrant les entités de la couche surfaces élémentaires à conserver (a_retirer = 0).

- Création et renseignement des attributs :
    - **surface_pe** (m²) : surface des entités 
    - **mare** (0/1) : surface_pe < 500 et "NatureSE" not in ('PE - réservoir -piscicult' , 'Plan d'eau - gravière', 'Plan d'eau - retenue', 'Plan d'eau - réservoir')
    - **zone_humide_potentielle** (0/1) : intersection avec la couche des zones humides potentielles
    - **distance_topage** (m) : calcul de la distance des plans d'eau aux tronçons de la BD TOPAGE et affectation des valeurs attributaires (la BD Topage a été préalablement analysée afin de calculer le rang de Strahler de chacun des tronçons(**StreamOrde**))
    - **longueur_topage_intercepte** (m) : calcul du linéaire de tronçons BD Topage interceptés
    - **distance_carthage** (m) : calcul de la distance aux tronçons de la BD CARTHAGE et affectation des valeurs attributaires de débit (module et qmna5 issus de la carte des consensus https://geo.data.gouv.fr/fr/datasets/8bcfa132902a0b35747656cf802f3a8616e0cc92)
    - **distance_source** (m) : calcul de la distance aux sources de la couche noeuds hydrographiques de la BD TOPAGE
    - **connecte_lh** (0/1) : intersection du plan d'eau avec le linéaire hydroagraphique de la BD Topage
    - **connecte_nappe** (0/1) : intersection du plan d'eau avec les zones d'alluvions et certaines de zones de colluvions de la BD CHARM
    - **connecte_source** (0/1) : plan d'eau situé à 50m maximum d'une source
    - **connecte_rh** (0/1) : si la plan d'eau est connecté au linéaire hydrographique, à une source ou à la nappe
    - **nom_sage** : sage(s) avec le(s)quel(s) intersecte le plan d'eau
    - **cd_me** : masse(s) d'eau avec la(es)quelle(s) intersecte le plan d'eau
    - **nom_communes** : commune(s) avec la(es)quelle(s) intersecte le plan d'eau
    - **lithologie** : jointure spatiale de la couche "Carte lithologique simplifiée au 1/1 000 000" au centroïde du plan d'eau (couche issue du flux WMS du BRGM : http://geoservices.brgm.fr/geologie)
    - **géologie** :  jointure spatiale de la géologie issue de la BD CHARM
    - **distance_roe** (m) : calcul de la distance aux ouvrages du ROE et affectation des principales valeurs attributaires des ouvrages situées à 20m maximum

## 2. Un second script pour constituer et qualifier les couches de territoire (bassin versant des masses d’eau, communes et BV IPR) :

Les couches de territoires retenues sont les suivantes : 

**communes** : couche des communes issue de la BD TOPO V3 depuis le flux WFS de l'IGN (https://data.geopf.fr/wfs/ows?VERSION=2.0.0)

**departements** : couche des départements issue de la BD TOPO V3 depuis le flux WFS de l'IGN (https://data.geopf.fr/wfs/ows?VERSION=2.0.0)

**regions** : couche des régions issue de la BD TOPO V3 depuis le flux WFS de l'IGN (https://data.geopf.fr/wfs/ows?VERSION=2.0.0)

**bassin versant des masses d'eau** : découpage de la couche des bassins versant des masses d'eau à leur stricte partie territoriale, par différenciation avec les couches "Masses d'eau côtières" et "Masses d'eau de transition".


**Utilisation du second script : w_20_geom_synth_territoire_202312.R**

-	Une partie de ce script permet de synthétiser les variables de la couche tronçons de la BD Topage par entité de territoire :
    -	**longueur_ce_topage** : somme des linéaires de tronçons
    -	**longueur_ce_tdbv_topage** : somme des linéaires de tronçons en tête de bassin versant.
    -	**strahler_max** : rang de stahler maximum rencontré sur l'entité


-	Une partie de ce script consiste à synthétiser les variables de la couche plan d’eau par entité de territoire :
    -	**longueur_topage_intersecte_pe_tot** : somme des linéaires de tronçons de la BD Topage interceptés par des PE.
    -	**longueur_topage_intersecte_pehm_tot** : somme des linéaires de tronçons de la BD Topage interceptés par des PE (hors marais).
    -	**longueur_topage_intersecte_pehm_tdbv_tot** : somme des linéaires de tronçons de la BD Topage interceptés par des PE (hors marais) en TDBV.
    -	**longueur_topage_intersecte_pe_perm** : somme des linéaires de tronçons de la BD Topage interceptés par des PE permanents.
    -	**longueur_topage_intersecte_pehm_perm** : somme des linéaires de tronçons de la BD Topage interceptés par des PE permanents (hors marais)
    -	**longueur_topage_intersecte_pehm_tdbv_perm** : somme des linéaires de tronçons de la BD Topage interceptés par des PE permanents (hors marais) en TDBV.
    -	**nb_pe_tot** et **surf_pe_tot** : décompte et somme des surfaces des PE
    -	**nb_pehm_tot** et **surf_pehm_tot** : décompte et somme des surfaces des PE (hors marais)
    -	**nb_pe_perm** et **surf_pe_perm** : décompte et somme des surfaces des PE permanents
    -	**nb_pehm_perm** et **surf_pehm_perm** : décompte et somme des surfaces des PE permanents (hors marais)
    -	**nb_pe_zhp_tot** et **surf_pe_zhp_tot** : décompte et somme des surfaces des PE en zone humide probable 
    -	**nb_pehm_zhp_tot** et **surf_pehm_zhp_tot** : décompte et somme des surfaces des PE en zone humide probable (hors marais)
    -	**nb_pe_perm** et **surf_pe_perm** : décompte et somme des surfaces des PE permanents
    -	**nb_pe_zhp_perm** et **surf_pe_zhp_perm** : décompte et somme des surfaces des PE permanents en zone humide probable
    -	**nb_pehm_perm** et **surf_pehm_perm** : décompte et somme des surfaces des PE permanents (hors marais)
    -	**nb_pehm_tdbv_tot** et **surf_pehm_tdbv_tot** : décompte et somme des surfaces des PE en TDBV (hors marais)
    -	**nb_pehm_tdbv_perm** et **surf_pehm_tdbv_perm** : décompte et somme des surfaces des PE en TDBV permanents (hors marais)
    -	**nb_pehm_connecte_tot** et **surf_pehm_connecte_tot** : décompte et somme des surfaces des PE connectés (hors marais)
    -	**nb_pehm_connecte_perm** et **surf_pehm_connecte_perm** : décompte et somme des surfaces des PE connectés permanents (hors marais)
    -	**nb_pehm_sur_cours_tot** et **surf_pehm_sur_cours_tot** : décompte et somme des surfaces des PE sur cours (hors marais)
    -	**nb_pehm_sur_cours_perm** et **surf_pehm_sur_cours_perm** : décompte et somme des surfaces des PE sur cours permanents (hors marais)
    -	**nb_pehm_sur_source_perm** et **surf_pehm_sur_source_perm** : décompte et somme des surfaces des PE sur source permanents (hors marais)
    -	**nb_pehm_zhp_tot** et **surf_pehm_zhp_tot** : décompte et somme des surfaces des PE en zone humide probable (hors marais)
    -	**nb_pehm_zhp_perm** et **surf_pehm_zhp_perm** : décompte et somme des surfaces des PE en zone humide probable (hors marais)
    -	**nb_mares_tot** et **surf_mares_tot** : décompte et somme des surfaces des mares
    -	**nb_mares_perm** et **surf_mares_perm** : décompte et somme des surfaces des mares permanentes
    -	**nb_mares_zhp_tot** et **surf_mares_zhp_tot** : décompte et somme des surfaces des mares en zone humide probable
    -	**nb_mares_zhp_perm** et **surf_mares_zhp_perm** : décompte et somme des surfaces des mares permanentes en zone humide probable
    -	**nb_mareshm_tot** et **surf_mareshm_tot** : décompte et somme des surfaces des mares (hors marais)
    -	**nb_mareshm_perm** et **surf_mareshm_perm** : décompte et somme des surfaces des mares permanentes (hors marais)
    -	**nb_mareshm_zhp_tot** et **surf_mareshm_zhp_tot** : décompte et somme des surfaces des mares en zone humide probable (hors marais)
    -	**nb_mareshm_zhp_perm** et **surf_mareshm_zhp_perm** : décompte et somme des surfaces des mares permanentes en zone humide probable (hors marais)
    -	**surface_moy_pe_perm** : calcul de la surface moyenne des plans d’eau permanents
    -	**surface_moy_pe_perm_tdbv** : calcul de la surface moyenne des plans d’eau permanents en TDBV


- Une partie de ce script permet de synthétiser les variables des couches de débit :
    -	**QAMOY_max** (m3/s) : module maximal rencontré sur l'entité
    -	**Q5MOY_max** (m3/s) : qmna5 maximal rencontré sur l'entité


- Une partie de ce script vise à synthétiser les valeurs obtenues pour la couche communes, aux couches départements et région.

    - Somme des longueurs, des surfaces, des volumes et des nombres d'objets.
    
    - Valeurs maximales du rang de Strahler et des valeurs de débit.
    
## 3. Analyses statistiques

### Hypothèses sur les logiques d'implantation 

Ce document vise à tester différentes hypothèses sur la logique d'implantataion des plans d'eau :

- L’implantation des plans d’eau est liée à la géologie/lithologie.

- L’implantation des plans d’eau est liée au régime hydrologique.

- L’implantation des plans d’eau est liée au type de masse d’eau (densité plus forte en zone côtière et littorale).

- L’implantation des plans d’eau est liée à l’activité de maraichage.

**Script** : 

w_hypotheses_implantation.R

**Rmarkdown** : 

- hypotheses_implantation.Rmd 

- hypotheses_implantation.html 

### Liens de corrélation entre pression plans d'eau et indice de qualité IPR :

Synthèse de l'analyse sur les liens de corrélation entre les indicateurs « plans d’eau » calculés à l’échelle des bassins versants des stations IPR et les notes IPR sur ces stations.

**Scripts** : 

- w_60_statistique_BV_IPR_score_IPR_202406.R

- w_61_exploit_statistique_BV_IPR_score_IPR.R

**Rmarkdown** : 

- correlation_pression_pe_ipr.Rmd 

- correlation_pression_pe_ipr.html
