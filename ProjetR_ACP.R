x<-c(2,5,6)
summary(sum)
summary(data2.2$y_max)
mean(data2.2$y_max)
summary(data2.2$CESD)

barplot(data2.2$CESD,data2.2$groupe ,col="darkred",main="sentiment de déprime en fonction du groupe",
        xlab="groupe vulnérabilité",ylab="sentiment de déprime")
moyenne_controle=mean(data2.2$cesd in $genre=1)
moy<-append(groupe[["1"]])
d.groupe1<- subset(d,data2.2$y_max=="1")
d.groupe1
summary(data2.2$duree_desc)
summary(data2.2$duree_ASC)

plot(data2.2$groupe,data2.2$CESD, xlab="jj",ylab="ll")

####################ACP######################

install.packages("PCAmixdata")
library(PCAmixdata)

#chargement du package PCAmixdata
require(PCAmixdata)
help(PCAmix)
#Mise en oeuvre
donne=data2.2
donne

res<-PCAmix(donne[,3:17],graph=TRUE)
res #permet de voir l'ensemble des données numériques dispo

#Choix nombre d'axe
round (res$eig,digit=2)
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))
abline(h=1,col=2,lwd=2)


