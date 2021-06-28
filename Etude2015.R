#----------------------------------------
# Chargement du jeu de donnees
#----------------------------------------
Data2015<-read.csv(file = "Data2015(2).csv",header = TRUE,row.names = 1, dec=",")
#----------------------------------------
# Analyse des indicateurs
#----------------------------------------
  #----------------------------------------
  # Comparaison du taux de séniors et du taux de jeunes
  #----------------------------------------
boxplot(Data2015$TJP,main = "Pourcentages de jeunes dans les populations des pays de l'UE",
        xlab = "Part dans la population (en %)",ylab = "Jeunes de 15-24 ans",col = "orange",
        border = "brown",horizontal = TRUE,notch = TRUE)
boxplot(Data2015$TJP, Data2015$TSP,
        main = "Comparaison de la part de jeunes et de la part de séniors dans les populations de l'UE",
        at = c(1,2),
        names = c("Taux de jeunes en %", "Taux de séniors en %"),
        las = 2,
        col = c("orange","red"),
        border = "brown",
        horizontal = FALSE)
  #----------------------------------------
  # Comparaison des indicateurs sociaux
  #----------------------------------------
t_suicide <- Data2015$T_SUICIDE
t_acc_soins <- Data2015$ACC_SOINS
t_ech_sco <- Data2015$ECH_SCO
t_ens_sup <- Data2015$ENS_SUP
norm_suicide<-rnorm(200,mean=mean(t_suicide, na.rm=TRUE), sd=sd(t_suicide, na.rm=TRUE))
norm_soins<-rnorm(200,mean=mean(t_acc_soins, na.rm=TRUE), sd=sd(t_acc_soins, na.rm=TRUE))
norm_scol<-rnorm(200,mean=mean(t_ech_sco, na.rm=TRUE), sd=sd(t_ech_sco, na.rm=TRUE))
norm_ens<-rnorm(200,mean=mean(t_ens_sup, na.rm=TRUE), sd=sd(t_ens_sup, na.rm=TRUE))
boxplot(t_suicide, norm_soins, norm_scol, norm_ens,
        main = "Comparaison des indicateurs d'inégalités sociales pour les jeunes dans les populations de l'UE",
        at = c(1,2,4,5),
        names = c("Taux de morts par suicide chez les jeunes en %", "Taux de jeunes en manque d'accès aux soins en %",
                  "Taux de jeunes en décrochage scolaire en %","Taux de jeunes atteignant l'enseignement secondaire en %"),
        las = 2,
        col = c("red","red","orange","orange"),
        border = "brown",
        horizontal = FALSE)
  #----------------------------------------
  # Comparaison des indicateurs économiques
  #----------------------------------------
t_temp <- Data2015$T_TEMP
seuil_pau <- Data2015$SEUIL_PAU
dif_fin <- Data2015$DIF_FIN
t_chom <- Data2015$T_CHOM
norm_temp <-rnorm(200,mean=mean(t_temp, na.rm=TRUE), sd=sd(t_temp, na.rm=TRUE))
norm_pau <-rnorm(200,mean=mean(seuil_pau, na.rm=TRUE), sd=sd(seuil_pau, na.rm=TRUE))
norm_dif <-rnorm(200,mean=mean(dif_fin, na.rm=TRUE), sd=sd(dif_fin, na.rm=TRUE))
norm_chom <-rnorm(200,mean=mean(t_chom, na.rm=TRUE), sd=sd(t_chom, na.rm=TRUE))
boxplot(norm_temp, norm_pau, norm_dif, norm_chom,
        main = "Comparaison des indicateurs d'inégalités économiques pour les jeunes dans les populations de l'UE",
        at = c(1,2,3,4),
        names = c("Taux de jeunes qui ont des contrats temporaires en %", "Taux de jeunes en dessous du seuil de pauvreté en %",
                  "Taux de jeunes en difficulté financière en %","Taux de chômage chez les jeunes en %"),
        las = 2,
        col = c("red","blue","orange","green"),
        border = "brown",
        horizontal = FALSE)
#----------------------------------------
# Corrélations entre les variables
#----------------------------------------
matrice_corelations<-round(cor(Data2015[2:34,1:9]),digits=2)

plot(Data2015$TJP,Data2015$TSP,xlab = "Taux de jeunes dans la population",ylab = "Taux de séniors dans la population")
title("Corrélation entre le taux de jeunes et le taux de séniors dans les populations de l'UE")
cor(Data2015$TJP,Data2015$TSP)
abline(1,0.004,col=2)

plot(Data2015$ENS_SUP,Data2015$ECH_SCO,xlab = "Taux de jeunes qui ont atteint l'enseignement sup?rieur",ylab = "Taux de jeune en ?chec scolaire")
title("Corr?lation entre le taux de jeunes qui ont atteint l'enseignement sup?rieur et le taux de jeunes en ?chec scolaire")
cor(Data2015$ENS_SUP,Data2015$ECH_SCO)

plot(Data2015$ECH_SCO,Data2015$T_TEMP,xlab = "Taux de jeune en ?chec scolaire",ylab = "Taux de jeunes qui ont des contrats temporaires")
title("Corr?lation entre le taux de jeune en ?chec scolaire et le taux de jeunes qui ont des contrats temporaires")
cor(Data2015$ECH_SCO,Data2015$T_TEMP)

plot(Data2015$TJP,Data2015$T_TEMP,xlab = "Taux de senior",ylab = "Taux de jeunes qui ont des contrats temporaires")
title("Corr?lation entre le taux de senior et le taux de jeunes qui ont des contrats temporaires")
cor(Data2015$TJP,Data2015$T_TEMP)

plot(Data2015$TJP,Data2015$ECH_SCO,xlab = "Taux de senior",ylab = "Taux de jeunes echec scolaire")
title("Corr?lation entre le taux de senior et le taux de jeunes echec sco")
cor(Data2015$ECH_SCO,Data2015$TJP)

plot(Data2015$TJP,Data2015$SEUIL_PAU,xlab = "Taux de senior",ylab = "Taux de jeunes seuil pauv")
title("Corr?lation entre le taux de senior et le taux de jeunes sous le seuil de pauv")
cor(Data2015$SEUIL_PAU,Data2015$TJP)

plot(Data2015$TJP,Data2015$T_SUICIDE,xlab = "Taux de senior",ylab = "Taux de jeunes seuil pauv")
title("Corr?lation entre le taux de senior et le taux de jeunes sous le seuil de pauv")
cor(Data2015$TJP,Data2015$T_SUICIDE)

#----------------------------------------
# Chargement du package PCAmixdata
#----------------------------------------
require(PCAmixdata)
#----------------------------------------
# Mise en oeuvre de l’ACP
#----------------------------------------
res<-PCAmix(Data2015[1:34,2:9]) # tous les calculs de l’ACP sont stockes dans l’objet "res"
# NB : par defaut les graphiques des plans factoriels 1-2
# sont affiches a l’ecran
res <- PCAmix(Data2015[1:34,2:9],graph=FALSE) # idem sans les graphiques
res # permet de voir l’ensemble des sorties numeriques disponibles
#-----------------------------------
# Choix du nombre d’axes à retenir
#-----------------------------------
round(res$eig,digit=2) # permet d’afficher les valeurs propres et les pourcentages
# de variances expliquees par chaque axe
# Graphique de l’ebouli des valeurs propres
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))
abline(h=1,col=2,lwd=2)
round(res$quanti$cos2,digit=2)
plot(res,axes=c(3,4),choice = "cor")
plot(res,axes=c(3,4),choice="ind")
plot(res,axes=c(3,4),choice="sqload")
#--------------------------------------------------------------------
# Sorties numeriques pour les individus et les variables
#--------------------------------------------------------------------
res$ind
round(res$ind$cos2,digit=3)
res$quanti
round(res$quanti$cos2,digit=3)

#--------------------------------------------------------------------
# description bivariées des variables
#--------------------------------------------------------------------
pairs(donnees)
