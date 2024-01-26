###
### Mustapha Bouhsen
###
### impoortStations(file)
###
## importStations : fonction qui importer la base de données des numéros
## et noms de stations BIXI
##
## Argument
##
## file : contient le nom du fichier de données en format CSV
##        (avec le chemin d’accès complet, si nécessaire)
##
## Valeur
##
## la fonction retourne un tableau de données
##
## Exemple
##
## importStations("/data/Stations_2018.csv")
##
importStations <- function(file)
{
  f <- file.path(file)
  read.csv(f, header = TRUE, colClasses = c("integer", "character", rep("NULL", 2)),
           sep = "," ,fileEncoding = "UTF-8")
  
}

write.file <- function(start, end)
  {
    n <- length((debut$mon + 1):(fin$mon + 1))
    forme <- paste0("OD_", format(debut, "%Y"), "-")
    x <- seq(as.Date(start) + 1, length = n , by = "+1 month") - 1
    y <- 1:length(x)
    for (i in y)
    {
      y[i] <- paste0(forme, format(x[i], "%m"), ".csv")
    }
    y
  }

###
### importData(start, end, path = "")
###
## importData : Une fonction qui importer toutes les données
## d’utilisation des vélos BIXI pour des locations ayant débuté entre
## deux dates d’une même année
##
## Arguments
##
## start et end sont l'intervalle de dates qu'on va importer.
## les dates admissibles sont du Avril au Novembre de la même année.
## Les dates ont le format de "AAAA-MM-JJ"
##
## path est une chaine de caractère qui indique le chemin
## pour aller chercher les fichiers à importer
##
## Valeur
##
## La fonction retourne un tableau de données
##
## Exemples
##
## Importer l'utilisations du 05 juin
## importData("2018-06_05", "2018-06-05", path = "data").
##
## Importer l'utilisations entre le 05 et le 30 juin
## importdata("2018-06-05", "2018-06-30", path = "data").
##
## Importer l'utilisations entre le 31 Aout et le 10 Octobre
## importData("2018-08-31", "2018-10-10", path = "data").
###
### write.file(start, end)
###
## write.file : une fonction qui se trouve à l'intérieur de la fonction importData,
## qui sert à écrire le nom du fichier qu'on va importer.
##
##
## Arguments:
##
## La fonction prend start et end, les arguments qu'on donnera a la fonction importData
##
## Valeur :
##
## la fonction retourne un vecteur de chaine de caractère des noms
## des fichier à importer de cette forme "OD_AAAA-MM.csv"
##
## exemples :
##
## file.writer("2018-06-05", "2018-08-18"),
## retourne :  "OD_2018-06.csv" "OD_2018-07.csv" "OD_2018-08.csv"
##
importData <- function(start, end, path = "")
{
  f <- file.path(path, "/")
  debut <- as.POSIXlt(start)
  fin <- as.POSIXlt(end)

  
  if (debut$mon == fin$mon)
  {
    k <- write.file(start, end)
    frame <- read.csv(paste0(f, k), header = TRUE,
                      colClasses = c("Date", "factor", "Date", "factor", "numeric", "factor"),
                      sep = ",", fileEncoding = "UTF-8")
    
    return(subset(frame, frame$start_date >= as.Date(start) & frame$start_date <= as.Date(end)))
  }
  
  else if ((debut$mon + 1) == fin$mon)
  {
    k <- write.file(start, end)
    frame_1 <- read.csv(paste0(f, k[1]), header = TRUE,
                        colClasses = c("Date", "factor", "Date", "factor", "numeric", "factor"),
                        sep = ",", fileEncoding = "UTF-8")
    frame_1 <- subset(frame_1,  frame_1$start_date >= as.Date(start))
    
    frame_2 <- read.csv(paste0(f, k[2]), header = TRUE,
                        colClasses = c("Date", "factor", "Date", "factor", "numeric", "factor"),
                        sep = ",", fileEncoding = "UTF-8")
    frame_2 <- subset(frame_2, frame_2$start_date <= as.Date(end))
    
    return(rbind(frame_1, frame_2))
  }
  
  else if ((debut$mon + 1) < fin$mon)
  {
    k <- write.file(start, end)
    
    frame_n <- read.csv(paste0(f, k[length(k)]), header = TRUE,
                        colClasses = c("Date", "factor", "Date", "factor", "numeric", "factor"),
                        sep = ",", fileEncoding = "UTF-8")
    frame_n <- subset(frame_n, frame_n$start_date <= as.Date(end))
    frame <- data.frame()
    for (i in 2:(length(k) - 1))
    {
      frame <- rbind(frame, read.csv(paste0(f, k[i]), header = TRUE,
                                     colClasses = c("Date", "factor", "Date", "factor", "numeric", "factor"),
                                     sep = ",", fileEncoding = "UTF-8"),
                     frame_n)
    }
    
    frame_1 <- read.csv(paste0(f, k[1]), header = TRUE,
                        colClasses = c("Date", "factor", "Date", "factor", "numeric", "factor") ,
                        sep = ",", fileEncoding = "UTF-8")
    frame_1 <- subset(frame_1, frame_1$start_date >= as.Date(start))
    
    return(rbind(frame_1, frame))
  }
}

###
### getStations(data, name)
###
## getStations : une fonction qui extrait le code de toutes les stations qui se
## trouvent sur une rue donnée ou à une intersection de cette rue
##
## Argument
##
## data : est un tableau de données comportant les codes et les
##        noms complets de stations
##
## name : est un vecteur d’expressions régulières permettant de rechercher
##        des noms de rue ou d’intersections
##
## Valeur
##
## La fonction retourne une liste nommée de vecteurs de codes de station ;
## les étiquettes de la liste sont les noms de rues
##
## Exemple
##
## data <- importStations("/data/Stations_2018.csv)
##
## Les code des stations sur la Rue Rachel:
## getStations(data, "Rachel").
##
## Les code des Stations sur la Rue Rachel et Gauvin:
## getStations(data, c("Rachel, "Gauvin"))
##
getStations <- function(data, name)
{
  k <- length(name)
  liste <- vector("list", k)
  for (i in 1:k)
  {
    liste[[i]] <- data$code[grep(name[i], data$name)]
  }
  
  names(liste) <- name
  liste
}
###
### summary.duratin(x, per.status = FALSE)
###
## summary.duration calcule les statistiques suivantes pour
## la durée des locations en secondes: minimum, premier quartile, médiane,
## moyenne, troisième quartile, maximum
##
## Argument
##
## x : un tableau de données avec la même structure que celui
##     produit par la fonction importData
##
## per.status : est une valeur booléenne indiquant si les statistiques
##              doivent ou non être ventilées par type de client
##
## Valeur
##
## Si l’argument per.status = TRUE, le résultat est
## une liste de vecteurs nommés de statistiques
##
## Sinon la fonction retourne un vecteur nommé contenant les
## statistiques dans l’ordre
##
## Exemple
##
## x <- importData("2018-06-05", "2018-06-10", path = "data")
##
## Statistique par type de client:
## summary.duration(x, per.status = TRUE)
##
## Statisque pour tous les utilissateurs
## summary.duration(x)
##
summary.duration <- function(x, per.status = FALSE)
{
  if (per.status)
  {
    m <- subset(x, x$is_member == 1)
    n <- subset(x, x$is_member == 0)
    return(list('0' = summary(n$duration_sec),
                '1' = summary(m$duration_sec)))
  }
  
  return(summary(x$duration_sec))
}
###
### summary.rentals(x)
###
## summary.rentals calcule les statistiques suivantes :
## le nombre de locations, le nombre de locations effectuées par des membres
## BIXI, la proportion de locations effectuées par des membres BIXI
##
## Argument
##
## x : un tableau de données avec la même structure que celui produit par la fonction importData
##
## Valeur
##
## La fonction retourne un vecteur nommé contenant les statistiques dans l’ordre
##
## Exemple
##
## x <- importData("2019-06-05", "2018-06-10", path = "data")
##
## summary.rentals(x)
##

summary.rentals <- function(x)
{
  name <- c("locations", "locations_membres", "%membres")
  x1 <- nrow(x)
  x2 <- nrow(subset(x, x$is_member == 1))
  result <- c(x1, x2, x2 / x1)
  names(result) <- name
  
  result
}
###
### tariff_A2019r1(x)
###
## tariff_A2019r1 calcule les revenus avant taxes selon la structure suivante:
## 2,95 $ pour une location de moins de 30 minutes;
## frais supplémentaires de 1,80 $ pour une location de 30 à 45 minute;
## frais supplémentaires de 3,00 $ par tranche de 15 minutes pour une
## location au-delà de 45 minutes.
##
## Argument :
##
## x est un vecteur de durées de location en secondes
##
## Valeur:
##
## La fonction retourne un vecteur de montants de revenus en dollars.
##
## Exemple
##
## tariff_A2019r1(c(0, 29.5, 30, 35, 45, 45.1, 60, 60.01, NA) * 60)

tariff_A2019r1 <- function(x)
{
  x <- x[!is.na(x)]
  x[x >= 0  & x < 1800] <- 2.95
  x[x >= 1800 & x <= 2700] <- 4.75
  x[x > 2700] <- 4.75 + (3 * ceiling((x[x > 2700] - 2700) / 900))
  
  x
}
###
### tariff_A2019r2(x)
###
## tariff_A2019r2 calcule les revenus avant taxes selon la structure suivante:
## frais de base de 0,57 $;
## frais de 0,00109 $ par seconde pendant les 30 premières minutes;
## frais de 0,00299 $ par seconde au-delà des 30 premières minutes;
## coût de location maximal de 10,50 $.
##
## Arguments:
##
## x est un vecteur de durées de location en secondes.
##
## Valeur:
##
## La fonction retourne un vecteur de montant de revenus en dollars
##
## Exemple
##
## tariff_A2019r2(c(0, 23, 30, 45, 74.41464, 80, NA) * 60)
##

tariff_A2019r2 <- function(x)
{
  x <- x[!is.na(x)]
  x[x <= 1800] <- 0.57 + 0.00109 * x[x <= 1800]
  x[x > 1800] <- pmin(2.532 + (0.00299 * (x[x > 1800] - 1800)), 10.5)
  
  x
}
###
### revenues(x, FUN)
###
## Fonction qui calcule les revenus totaux avant taxe liés
## à des locations pour une structure de tarif donnée.
##
## Arguments:
##
## x est un tableau de données avec la même structure
## que celui produit par la fonction importData
##
## FUN est une fonction qui calcule les revenus associés
## à la durée d’une location en secondes
##
## Valeur:
##
## fonction retourne le montant total des revenus en dollars.
##
## Exemple
##
## x <- importData("2018-06-05", "2018-06-10", path = "data")
##
## revenus(x, tariff_A2019r1)
##
## revenus(x, tariff_A2019r2)

revenues <- function(x, FUN)
  sum(FUN(x$duration_sec), na.rm = TRUE)
