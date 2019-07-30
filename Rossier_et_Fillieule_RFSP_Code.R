#Revue française de science politique, Vol. 69, N° 4, 2019
#"Devenir(s) militants. Proposition de méthode pour une exploration des conséquences biographiques de l'engagement des soixante-huitard.e.s français.e.s"
#Code pour obtenir les ACM et l'AFM
#Thierry Rossier
#Olivier Fillieule

#Remerciements en particulier à Olivier Robette et Matthias Studer pour leur aide précieuse. Et à Davide Morselli pour avoir passé en revue ce code.



####Lancer le fichier "Rossier_et_Fillieule_RFSP_Data.RDATA"
####Pour toute question écrire à thierry.rossier.elites@gmail.com



####Installation packages
#install.packages("soc.ca")
#install.packages("GDAtools")
#install.packages("FactoMineR")
#install.packages("cluster")
#install.packages("WeightedCluster")
#install.packages("dplyr")

####Chargement des packages
library(soc.ca) #ACMs spécifiques
library(GDAtools) #ACMs spécifiques et AFM
library(FactoMineR) #Caractérisation des clusters
library(cluster) #Clustering de Ward
library(WeightedCluster) #Recalcul de partitionning around medoids
library(dplyr) #Recodage des clusters



####################################################
####"Annexe 5. Analyse des correspondances multiples spécifiques pour les quatre périodes"
#Nous utilisons le package soc.ca. Utilisant le moteur graphique de ggplot2, nous le préférons à d'autres packages d'ACM.
#Modalité passive pour les quatre ACM
options(passive = c("Milit: Non"))

####ACM 60_74
#Objet ACM
soc.mca_Sombrero_60_74 <- soc.mca(active_60_74[2:9])

#Contrôle modalité passive
soc.mca_Sombrero_60_74$names.passive

#Graphique axes 1 et 2
map.active(soc.mca_Sombrero_60_74, dim = c(1, 2), point.shape = "variable", point.alpha = 1, point.fill = "black", point.color = "black", point.size = "freq", label = TRUE, label.repel = TRUE, label.alpha = 1, label.color = "black", label.size = 4, label.fill = NULL, map.title = "60_74 Axes 1-2", labelx = "default", labely = "default", legend = NULL)



####ACM 75_83
#Objet ACM
soc.mca_Sombrero_75_83 <- soc.mca(active_75_83[2:11])

#Contrôle modalité passive
soc.mca_Sombrero_75_83$names.passive

#Graphique axes 1 et 2
map.active(soc.mca_Sombrero_75_83, dim = c(1, 2), point.shape = "variable", point.alpha = 1, point.fill = "black", point.color = "black", point.size = "freq", label = TRUE, label.repel = TRUE, label.alpha = 1, label.color = "black", label.size = 4, label.fill = NULL, map.title = "75_83 Axes 1-2", labelx = "default", labely = "default", legend = NULL)



####ACM 84_94
#Objet ACM
soc.mca_Sombrero_84_94 <- soc.mca(active_84_94[2:11])

#Contrôle modalité passive
soc.mca_Sombrero_84_94$names.passive

#Graphique axes 1 et 2
map.active(soc.mca_Sombrero_84_94, dim = c(1, 2), point.shape = "variable", point.alpha = 1, point.fill = "black", point.color = "black", point.size = "freq", label = TRUE, label.repel = TRUE, label.alpha = 1, label.color = "black", label.size = 4, label.fill = NULL, map.title = "84_94 Axes 1-2", labelx = "default", labely = "default", legend = NULL)



####ACM 95_15
#Objet ACM
soc.mca_Sombrero_95_15 <- soc.mca(active_95_15[2:11])

#Contrôle modalité passive
soc.mca_Sombrero_95_15$names.passive

#Graphique axes 1 et 2
map.active(soc.mca_Sombrero_95_15, dim = c(1, 2), point.shape = "variable", point.alpha = 1, point.fill = "black", point.color = "black", point.size = "freq", label = TRUE, label.repel = TRUE, label.alpha = 1, label.color = "black", label.size = 4, label.fill = NULL, map.title = "95_15 Axes 1-2", labelx = "default", labely = "default", legend = NULL)



####################################################
####Refaire les ACM avec GDAtools, en vue de l'AFM. Les variables actives sont renommées pour les graphiques de l'AFM
active_60_74_names_AFM <- active_60_74_names[2:9]
getindexcat(active_60_74_names_AFM)
speACM_Sombrero_60_74_names <- speMCA(active_60_74_names_AFM, ncp=10, excl=26)

active_75_83_names_AFM <- active_75_83_names[2:11]
getindexcat(active_75_83_names_AFM)
speACM_Sombrero_75_83_names <- speMCA(active_75_83_names_AFM, ncp=10, excl=28)

active_84_94_names_AFM <- active_84_94_names[2:11]
getindexcat(active_84_94_names_AFM)
speACM_Sombrero_84_94_names <- speMCA(active_84_94_names_AFM, ncp=10, excl=24)

active_95_15_names_AFM <- active_95_15_names[2:11]
getindexcat(active_95_15_names_AFM)
speACM_Sombrero_95_15_names <- speMCA(active_95_15_names_AFM, ncp=10, excl=26)



####################################################
####AFM
MCA_4_periodes_names <- list(speACM_Sombrero_60_74_names, speACM_Sombrero_75_83_names, speACM_Sombrero_84_94_names, speACM_Sombrero_95_15_names)

MultiMCA_names <- multiMCA(MCA_4_periodes_names, ncp = 10, compute.rv = TRUE)



####"Tableau 2. Contribution des quatre ACM aux trois premiers axes de l'AFM"
round(MultiMCA_names$group$contrib[, 1:3],1)



####"Figure 3. Analyse factorielle multiple 1960-2015, nuage des modalités, axes 1 et 2"
plot(MultiMCA_names, type = "v", col=c('black', 'gray20', 'gray40', 'gray60'), axes = 1:2, points = "all", app = 2)



####Modalités notables (Merci à Nicolas Robette et Olivier Roueff pour cette fonction. Voir: "L’espace contemporain des goûts culturels", in Sociologie, Vol. 8, N° 4, 2017, pp. 369 à 394)
temp <- do.call('rbind.data.frame', lapply(MultiMCA_names$VAR, function(x) x$coord))
aides <- list()
for(i in 1:ncol(temp)) {
  x <- temp[, i]
  names(x) <- substr(rownames(temp), 6, nchar(rownames(temp)))
  x <- x[order(x)]
  aides[[i]] <- x[abs(x)>=0.5]
}



####"Tableau 3. Modalités notablement et très associées (en gras) à l'Axe 1 de l'AFM, par période"
aides[[1]]



####Variables supplémentaires
####"Tableau 4. Pourcentage de la variance des trois premiers axes de l'AFM expliquée par les variables supplémentaires (eta2)"
l <- list(sup$Sexe, sup$Pere, sup$Cohorte, sup$Mobilite, sup$MobiliteThelot)
n <- c("Sexe", "Pere", "Cohorte", "Mobilite", "Mobilite Thelot")
dimeta2(MultiMCA_names, l, n, dim=1:3)



####Création d'objets en vue de produire des statistiques sur les variables supplémentaires
Varsup_Sexe <- varsup(MultiMCA_names, sup$Sexe)
Varsup_Pere <- varsup(MultiMCA_names, sup$Pere)
Varsup_Cohorte <- varsup(MultiMCA_names, sup$Cohorte)
Varsup_MobiliteThelot <- varsup(MultiMCA_names, sup$MobiliteThelot)
Varsup_Mobilite <- varsup(MultiMCA_names, sup$Mobilite)



####"Tableau 5. Coordonnées des modalités des variables supplémentaires sur les 3 premiers axes de l'AFM
#Coordonnées sur les trois axes"
Varsup_Sexe$coord[,1:3]
Varsup_Pere$coord[,1:3]
Varsup_Cohorte$coord[,1:3]
Varsup_Mobilite$coord[,1:3]
Varsup_MobiliteThelot$coord[,1:3]

#Fréquences des variables
table(sup$Sexe)
table(sup$Pere)
table(sup$Cohorte)
table(sup$MobiliteThelot)
table(sup$Mobilite)



####"Tableau 6. Modalités notablement et très associées (en gras) à l'Axe 2 de l'AFM, par période"
aides[[2]]



####"Figure 4. Analyse factorielle multiple 1960-2015, nuage des modalités, axes 1 et 3"
plot(MultiMCA_names, type = "v", col=c('black', 'gray20', 'gray40', 'gray60'), axes = c(1,3), points = "all", app = 2)



####"Tableau 7. Modalités notablement et très associées (en gras) à l'Axe 3 de l'AFM, par période"
aides[[3]]



####################################################
####Clustering
#Procédure de Ward sur les dix premiers axes
coords <- MultiMCA_names$ind$coord[, 1:10]
clust <- agnes(coords, method="ward")

#Partition en 5
clusters_5 <- cutree(clust, k = 5)
clusters_5 <- factor(clusters_5)
table(clusters_5)

#Procédure de Partitioning around medoids (PAM)
diss <- dist(MultiMCA_names$ind$coord[,1:10], method = "euclidean")
PAM_5 <- wcKMedoids(diss, 5, initialclust = clust)
table(PAM_5$clustering)

#Export des 5 clusters PAM dans un data.frame
Clusters <- data.frame(PAM_5$clustering)

#Recodage des 5 clusters PAM 
Clusters$PAM_5 <- factor(Clusters$PAM_5.clustering)
Clusters$PAM_5 <- recode(Clusters$PAM_5,
               "8" = "Cluster 1",
               "21" = "Cluster 2",
               "140" = "Cluster 3",
               "223" = "Cluster 4",
               "271" = "Cluster 5")
table(Clusters$PAM_5)

#Création d'un data.frame avec toutes les variables actives et supplémentaires utilisées pour qualifier les 5 clusters
Clusters_Qualification_5 <- data.frame(active_60_74_names[,2:9], active_75_83_names[,2:11], active_84_94_names[,2:11], active_95_15_names[,2:11], sup[,2:6], sup_autres[,2:6], Clusters$PAM_5)
names(Clusters_Qualification_5)



####"Annexe 6. Caractérisation des cinq clusters de l'AFM. Modalités asociées"
catdes(Clusters_Qualification_5,49,proba = 0.05)
