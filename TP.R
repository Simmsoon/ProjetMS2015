#================================
# Fonctions utilisées dans le TP
#================================

#Fonction pour faire un histogramme à pas constants
histopas = function(x, xlim=NULL, ...) {
  # trace un histogramme a pas constant
  n <- length(x)
  k <- round(log(n)/log(2)+1)
  
  breaks <- rep(0, k+1)
  rangex <- max(x)-min(x)
  breaks[1] <- min(x)-0.025*rangex
  breaks[k+1] <- max(x)+0.025*rangex
  step <- (breaks[k+1] - breaks[1]) / k
  for (i in 2:k)
  {
    breaks[i] <- breaks[i-1] + step
  }
  col=0
  if (is.null(xlim))
    xlim<-c(breaks[1], breaks[k+1])
  # ylim<-c(0,4.0)
  h<-hist(x, breaks=breaks, col=col, xlim=xlim, probability=T, ...)
}

#Fonction pour tracer un histogramma à classes de même effectif
histoeff = function(x, xlim=NULL, ...){
  # trace un histogramme effectifs constants, approximation si problème de  divisibilité
  sx <- sort(x)
  n <- length(x)
  k <- round(log(n)/log(2)+1)
  breaks=rep(0, k+1)
  smax <- sx[length(sx)]+0.025*(sx[length(sx)]-sx[1])
  smin <- sx[1]-0.025*(sx[length(sx)]-sx[1])
  step <- round(n/k)
  breaks[1] <- smin
  for (i in 1:(k-1))
  {
    breaks[i+1]=0.5*(sx[step*i]+sx[step*i+1])
  }
  breaks[k+1] <- smax
  xlim <- c(smin, smax)
  col <- 0
  h<-hist(x, breaks=breaks, col=col, xlim=xlim, probability=T, ...)
}

#================
# Réponses au TP
#================


#Question n°1:
#==============


cuves <- read.table("cuves.csv",sep=";",header=T)
attach(cuves)
cuve1<-na.omit(cuves$cuve1)
cuve2<-na.omit(cuves$cuve2)
cuve3<-na.omit(cuves$cuve3)

# Indicateurs statistiques
summary(na.omit(cuves))

# Variance et écart-type empiriques
var(na.omit(cuves))
sd(cuve1)
sd(cuve2)
sd(cuve3)

# Coefficient de variation empirique
sd(cuve1)/mean(cuve1)
sd(cuve2)/mean(cuve2)
sd(cuve3)/mean(cuve3)

# Histogrammes à pas constant
histopas(cuve1)
histopas(cuve2)
histopas(cuve3)

# Histogrammes classes de même effectif
histoeff(cuve1)
histoeff(cuve2)
histoeff(cuve3)

#Ccl: cuve1 et cuve2  vraisemblablement de loi exponentielle
# cuve3 de loi normale


#Question n°2:
#==============
# FR: Fx(x)=1-b^a x^(-a)=1-2^a x^(-a)
# 
# 
# E[X] et  Var(X) finies pour a > 2
#


#Question n°3:
#==============
#  Y ~ Exp(a)
#


#Question n°4:
#==============
#  Fonction pivotale: 2a*sum(Yi)
# 2a*sum(Yi) ~ gamma(n,1/2)
#
# [(z2n,1-alpha/2)/(2*sum(Yi)) , (z2n,alpha/2)/(2*sum(Yi)) ] IC(alpha) pour a
#






