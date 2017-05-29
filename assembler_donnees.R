#
# Préparer les données publiques MétéoFrance
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

library("plyr")
library("stringr")
library("lubridate")
library("tidyverse")
library("ggrepel")

dir.create("meteo", showWarnings=F)


# ## Télécharger tous les fichiers ----
# # Cf https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=90&id_rubrique=32
#
# # lister les mois entre 2000 et 2016 (les données sont stockées par mois)
# dates <- seq(as.Date("2000-01-01"), as.Date("2016-12-31"), by="month")
# dates <- format(dates, "%Y%m")
#
# # télécharger les fichiers
# unlink("raw", recursive=TRUE)
# dir.create("raw")
# l_ply(dates, function(date) {
#   download.file(paste0("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/Archive/synop.",date,".csv.gz"), destfile=paste0("raw/", date, ".csv.gz"), quiet=TRUE)
# }, .progress="text")


## Lire, assembler et reformater les données ----

# lire tous les fichiers
meteo_files <- list.files("raw", full.names=TRUE)
m <- ldply(meteo_files, read_delim, delim=";", na="mq", col_types=cols(), progress=FALSE, .progress="text")
# TODO we could read the files from the server directly too, using the url as the name

# sélectionner qq variables d'intérêt et leur donner des noms explicites
# https://donneespubliques.meteofrance.fr/client/document/doc_parametres_synop_168.pdf
m <- select(m,
  numer_sta,
  date,
  pression=pmer,       # Pa, pression au niveau de la mer
                       # NB: la pression non ramenée au niveau de la mer (pres) représente surtout l'altitude
  vitesse_vent=ff,     # m/s, sur 10 min
  temperature=t,       # K
  point_de_rosee=td,   # K, https://fr.wikipedia.org/wiki/Point_de_rosée
  humidite=u,          # %
  nebulosite=n,        # %
  precipitations=rr3,  # mm, sur les 3 dernières heures
  nuages_bas=cl,       # code catégoriel. Cf ci-dessous
  nuages_moyens=cm,    #
  nuages_hauts=ch      #
)

summary(m)

# reconnaitre la date comme un objet POSIXct
m$date <- ymd_hms(m$date)

# convertir les unités/codes
# Pa -> hPa
m$pression <- m$pression / 100
# ºK -> ºC
m$temperature <- m$temperature - 273.15
m$point_de_rosee <- m$point_de_rosee - 273.15
# codes nuages
# https://library.wmo.int/pmb_ged/wmo_306-v1_1-2012_fr.pdf
# table 0513
m$nuages_bas <- factor(m$nuages_bas,
  levels=c(30:39, 62),
  labels=c("nuages bas absents", "Cumulus humilis", "Cumulus mediocris", "Cumulonimbus calvus", "Stratocumulus cumulogenitus", "Stratocumulus (autres)", "Stratus nebulosus", "Stratus fractus", "Cumulus irréguliers", "Cumulonimbus capillatus", "nuages bas invisible")
)
# table 0515
m$nuages_moyens <- factor(m$nuages_moyens,
  levels=c(20:29, 61),
  labels=c("nuages moyens absents", "Altostratus translucidus", "Altostratus opacus", "Altostratus translucidus (1 niveau)", "Altocumulus translucidus (en bancs)", "Altocumulus translucidus (en bandes)", "Altocumulus cumulogenitus", "Altocumulus translucidus", "Altocumulus castellanus", "Altocumulus (ciel cahotique)", "nuages moyens invisibles")
)
# table 0509
m$nuages_hauts <- factor(m$nuages_hauts,
  levels=c(10:19, 60),
  labels=c("nuages haut absents", "Cirrus fibratus", "Cirrus spissatus", "Cirrus spissatus cumulonimbogenitus", "Cirrus uncinus", "Cirrostratus", "Cirrostratus (abondants)", "Cirrostratus (ciel couvert)", "Cirrostratus (partiel)", "Cirrocumulus seuls", "nuages hauts invisibles")
)

# lire les caractéristiques des stations
s <- read_delim("postesSynop.csv", delim=";", col_types=cols())
# et reformater le contenu
s <- rename(s, numer_sta=ID, station=Nom)
names(s) <- str_to_lower(names(s))
s$station <- str_to_title(s$station, locale="fr")
s$station <- str_replace(s$station, "\\-.*$", "")
s$station <- factor(s$station)

# conserver uniquement les stations de métropole
# qplot(lon, lat, data=s) + coord_map()
s <- filter(s, longitude > -10, longitude < 20)

# ajouter les noms de station à partir des codes et restreindre aux stations sélectionnées ci-dessus
d <- inner_join(m, s, by="numer_sta")
d <- select(d, -numer_sta)

save(d, file="meteo/raw.RData")


## Réduire les données à un pas journalier ----

load("meteo/raw.RData")

# arrondir la date à la journée
d$date <- as.Date(d$date)

#' Majority vote
#'
#' @param x vector of any type but usually a discrete variable (factor,
#'          integer, etc.).
#' @param useNA booelan, when TRUE (the default) NAs are counted as regular
#'              levels of the variable x, i.e. when they represent the
#'              majority of x, the result is NA.
#'              NB: this is different from the usual na.rm argument because
#'                  if NAs are present but not in majority, then the result
#'                  is not NA.
#'
#' @value The majority element of x, of the same class as x (i.e. factor
#'        levels, date classes, etc. are preserved.)
#'
#' @example
#' x <- factor(c("a", NA, "b", "a", NA, "c", NA))
#' majority(x)
#' majority(x, useNA=FALSE)
majority <- function(x, useNA=TRUE) {
  # count the occurences of each modality of x
  # we could use names(which.max(table(x))) but this looses the class of x
  require("plyr")
  y <- plyr::count(x)
  if (!useNA) {
    y <- na.omit(y)
  }
  y <- y$x[which.max(y$freq)]
  # deal with empty vectors (e.g. if useNA is FALSE and x is all NA) = return NA
  if (length(y) == 0) {
    x[1] <- NA  # preserves the class of x
    y <- x[1]
  }

  return(y)
}

# fast version, not class-preserving
maj <- function(x) {
  names(which.max(table(x)))
}

# pour chaque variable aggréger les valeurs par station et date
# - pour les variables continues, calculer la moyenne (ou la somme, dans le cas des précipitations)
# - pour les variables discretes, calculer la modalité majoritaire
dd <- d %>%
  group_by(station, latitude, longitude, altitude, date) %>%
  summarise(
    pression = mean(pression, na.rm=T),
    vitesse_vent = mean(vitesse_vent, na.rm=T),
    temperature = mean(temperature, na.rm=T),
    point_de_rosee = mean(point_de_rosee, na.rm=T),
    humidite = mean(humidite, na.rm=T),
    nebulosite = mean(nebulosite, na.rm=T),
    precipitations = sum(precipitations, na.rm=T),
    pluie = precipitations > 1,
    # nuages_bas = majority(nuages_bas, useNA=F),
    # nuages_moyens = majority(nuages_moyens, useNA=F),
    # nuages_hauts = majority(nuages_hauts, useNA=F)
    nuages_bas = maj(nuages_bas),
    nuages_moyens = maj(nuages_moyens),
    nuages_hauts = maj(nuages_hauts)
  ) %>%
  ungroup()

# ajouter des éléments de date
dd$annee <- ordered(format(dd$date, "%Y"))
dd$mois <- format(dd$date, "%b")
dd$mois <- ordered(dd$mois, levels=unique(dd$mois))
dd$mois_num <- as.numeric(format(dd$date, "%m"))

save(dd, file="meteo/jour.RData")


## Explorer les données pour déterminer comment les réduire ----

load("meteo/jour.RData")

# ACP avec variables supplémentaires
library("FactoMineR")
pca <- dd %>%
  select(station, annee, mois, starts_with("nuages"), latitude, altitude, pression:nebulosite, precipitations) %>%
  PCA(graph=F, quali.sup=1:6, quanti.sup=7:8)

# variables
plot(pca, choix="var")
# -> Correlation entre temp et point de rosée
#                      vitesse moyenne du vent et précipitations
#                      nébulosité, humidité (et, dans une moindre mesure, précipitations)
#    Anticorrelation entre pression et vent + précipitations
#    Vent mal représenté
#    La température diminue avec l'altitude
#    La nébulosité et l'humidité augmentent avec la latitude (+ au Nord = + de nuages)
# Tout cela est parfaitement logique et prévisible

# extraire les variables qualitatives supplémentaires pour faire des graphiques séparés lisibles
# celles-ci décrivent en partie les observations: années, stations, mois etc.
sup <- as.data.frame(pca$quali.sup$coord[,1:2])
sup$cos2 <- rowSums(pca$quali.sup$cos2[,1:2])
sup_variables <- lapply(select(dd, station, annee, mois, starts_with("nuages")), function(x) {length(unique(x))})
sup$variable <- rep(names(sup_variables), times=sup_variables)
sup$label <- row.names(sup)
ggplot(sup, aes(x=Dim.1, y=Dim.2, alpha=cos2)) +
  geom_point() +
  geom_text_repel(aes(label=label)) +
  facet_wrap(~variable) +
  coord_fixed() + scale_x_continuous(breaks=0) + scale_y_continuous(breaks=0)

# -> En comparant les deux graphiques:
#    peu de structure interannuelle (probablement masqué par la saison)
#    forte structure saisonnière
#    lien entre certains types de nuages et les précipitations
#    certains types de nuages peuvent probablement être regroupés
#    certaines stations sont bien caractéristiques et peuvent être extraites


## Réduire les données ----

load("meteo/jour.RData")

# simplifer les codes nuages
dd$nuages_bas <- str_replace(dd$nuages_bas, "^([A-Z].*?) .*$", "\\1")
dd$nuages_moyens <- str_replace(dd$nuages_moyens, "^([A-Z].*?) .*$", "\\1")
dd$nuages_hauts <- str_replace(dd$nuages_hauts, "^([A-Z].*?) .*$", "\\1")

# sélectionner qq années et qq stations
ddr <- dd %>%
  filter(
    annee %in% c(2013:2016),
    station %in% c("Nice", "Brest", "Toulouse", "Le Puy", "Nancy")
  )

summary(ddr)


# inspecter à nouveau les données réduites avec une ACP
pcar <- ddr %>%
  select(station, annee, mois, starts_with("nuages"), latitude, altitude, pression:precipitations) %>%
  PCA(graph=F, quali.sup=1:6, quanti.sup=7:8)
plot(pcar, choix="var")
supr <- as.data.frame(pcar$quali.sup$coord[,1:2])
supr$cos2 <- rowSums(pcar$quali.sup$cos2[,1:2])
supr_variables <- lapply(select(ddr, station, annee, mois, starts_with("nuages")), function(x) {length(unique(x))})
supr$variable <- rep(names(supr_variables), times=supr_variables)
supr$label <- row.names(supr)
ggplot(supr, aes(x=Dim.1, y=Dim.2, alpha=cos2)) +
  geom_point() +
  geom_text_repel(aes(label=label)) +
  facet_wrap(~variable) +
  coord_fixed() + scale_x_continuous(breaks=0) + scale_y_continuous(breaks=0)
# -> l'essentiel du message est conservé

# inspecter qq relations 2 à 2

# types de nuages et précipitations
ggplot(ddr) + geom_boxplot(aes(x=nuages_bas, y=precipitations)) + scale_y_log10()
ggplot(ddr) + geom_boxplot(aes(x=nuages_moyens, y=precipitations)) + scale_y_log10()
ggplot(ddr) + geom_boxplot(aes(x=nuages_hauts, y=precipitations)) + scale_y_log10()

# différences entre années
ggplot(ddr) + geom_boxplot(aes(x=annee, y=precipitations)) + scale_y_log10()
ggplot(ddr) + geom_boxplot(aes(x=annee, y=temperature))

# différences entre stations
ggplot(ddr) + geom_boxplot(aes(x=station, y=precipitations)) + scale_y_log10()
ggplot(ddr) + geom_boxplot(aes(x=station, y=temperature))


## Aggréger les données à différentes échelles ----

reduce <- function(x, ...) {
  o <- group_by(x, ...) %>%
    summarise(
      pression = mean(pression, na.rm=T),
      vitesse_vent = mean(vitesse_vent, na.rm=T),
      temperature = mean(temperature, na.rm=T),
      point_de_rosee = mean(point_de_rosee, na.rm=T),
      humidite = mean(humidite, na.rm=T),
      nebulosite = mean(nebulosite, na.rm=T),
      precipitations = sum(precipitations, na.rm=T),
      nb_jours_pluie = sum(pluie, na.rm=T),
      nuages_bas = maj(nuages_bas),
      nuages_moyens = maj(nuages_moyens),
      nuages_hauts = maj(nuages_hauts)
    ) %>%
    ungroup()
  o$pression[is.nan(o$pression)] <- NA
  return(o)
}

# par mois
dm <- reduce(dd, station, latitude, longitude, altitude, annee, mois, mois_num)
dm$date <- ymd(paste0(dm$annee, "-", dm$mois_num, "-1"))
dmr <- reduce(ddr, station, latitude, longitude, altitude, annee, mois, mois_num)
dmr$date <- ymd(paste0(dmr$annee, "-", dmr$mois_num, "-1"))

# inspecter la nouvelle variable "nb de jours de pluie"
ggplot(dmr) + geom_boxplot(aes(x=station, y=nb_jours_pluie))
ggplot(dmr) + geom_histogram(aes(x=nb_jours_pluie), binwidth=1)


# par annéee
da <- reduce(dd, station, latitude, longitude, altitude, annee)
dar <- reduce(ddr, station, latitude, longitude, altitude, annee)

# sélectionner qq colonnes pour avoir une version encore plus compacte pour l'intro à R
mini <- select(dar, station, annee, pression, temperature, precipitations, nuages_bas)

# ecrire dans des fichiers texte
write_csv(dd,  path="meteo/jour_complet.csv")
write_csv(ddr, path="meteo/jour.csv")
write_csv(dm,  path="meteo/mois_complet.csv")
write_csv(dmr, path="meteo/mois.csv")
write_csv(da,  path="meteo/annee_complet.csv")
write_csv(dar, path="meteo/annee.csv")
write_csv(  mini, path="meteo/exemple.csv")
write_tsv(  mini, path="meteo/exemple.tsv")
write_delim(mini, path="meteo/exemple.txt", delim=" ")

# traduire les fichiers en anglais
in_english <- function(x) {
  # renommer les colonnes
  new_names <- c(pressure="pression", wind_speed="vitesse_vent", dew_point="point_de_rosee", humidity="humidite", nebulosity="nebulosite", rain="pluie", low_clouds="nuages_bas", mid_clouds="nuages_moyens", high_clouds="nuages_hauts", year="annee", month="mois", month_num="mois_num", nb_rainy_days="nb_jours_pluie")
  new_names <- new_names[new_names %in% names(x)]
  x <- rename_(x, .dots=new_names)

  # renommer les nuages
  x$low_clouds <- str_replace(x$low_clouds, "nuages bas", "low clouds")
  if ("mid_clouds" %in% names(x)) {
    x$mid_clouds <- str_replace(x$mid_clouds, "nuages moyens", "mid clouds")
  }
  if ("high_clouds" %in% names(x)) {
    x$high_clouds <- str_replace(x$high_clouds, "nuages hauts", "high clouds")
  }
  for (i in intersect(c("low_clouds", "mid_clouds", "high_clouds"), names(x))) {
    x[[i]] <- str_replace(x[[i]], "absents", "absent")
    x[[i]] <- str_replace(x[[i]], "invisibles", "invisible")
  }

  return(x)
}

write_csv(in_english(dd),  path="meteo/daily_full.csv")
write_csv(in_english(ddr), path="meteo/daily.csv")
write_csv(in_english(dm),  path="meteo/monthly_full.csv")
write_csv(in_english(dmr), path="meteo/monthly.csv")
write_csv(in_english(da),  path="meteo/yearly_full.csv")
write_csv(in_english(dar), path="meteo/yearly.csv")
write_csv(  in_english(mini), path="meteo/mini.csv")
write_tsv(  in_english(mini), path="meteo/mini.tsv")
write_delim(in_english(mini), path="meteo/mini.txt", delim=" ")

