#' Décompter des objets dans un polygone.
#' 
#' A partir d'une couche d'intersection entre des objets (petits, par exemple des plans d'eau)
#'     et des polygones (grands, par exemple des BV de masses d'eau) qui contient un attribut de surface, 
#'     la fonction propose de décompter ces objets au sein de chacun des polygones. 
#'
#' @param couche_objets Objet de classe sf ...
#' @param var_id_polygone Nom dela variable de `couche_objets` indiquant  
#'     le code unique des polygones .  
#' @param var_nb_objets Nom de la variable qui contiendra le résultat du décompte
#'     des objets.
#' @param zone_marais_incluse Booléen. Conserver les surfaces en marais ?
#'     Par défaut elles sont conservées (zone_marais_incluse = FALSE).
#' @param seulement_permanent 
#'
#' @return un dataframe composé de deux attributs : code unique des polygones et 
#'     décompte des objets contenus dans le polygone. 
#' @export 
#'
#' @examples
#' \dontrun{
#' test <-
#' sommer_surfaces_dans_polygone(
#'   couche_surface = pe_decoup_sage %>% units::drop_units(),
#'   var_id_polygone = nom_sage,
#'   zone_marais_incluse = FALSE,
#'   seulement_permanent = FALSE
#' )
#' }
compter_objets_dans_polygone <- function(couche_objets,
                                          var_id_polygone,
                                          var_nb_objets = "nb_objets",
                                          zone_marais_incluse = FALSE,
                                          seulement_permanent = FALSE)
  
{
  var_id_polygone <- enquo(var_id_polygone)
  var_nb_objets <- enquo(var_nb_objets)
  
  
  if (seulement_permanent) {
    couche_objets <- couche_objets %>%
      filter(Persistanc == "permanent")
  }
  
  if (!zone_marais_incluse) {
    couche_objets <- couche_objets %>%
      filter(zone_marais == 0)
  }
  
  nb_objets_dans_polygone <- couche_objets %>% 
    group_by(!!var_id_polygone) %>%
    summarise(!!var_nb_objets := count()) %>%
    select(!!var_id_polygone, # sélection des variables à conserver
           !!var_nb_objets)
  
  nb_objets_dans_polygone 
  
}

