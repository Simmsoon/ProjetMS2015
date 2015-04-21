#================================
# Fonctions utilisées dans le TP
#================================

#Fonction pour faire un histogramme à pas constants
histopas = function(x, xlim=NULL, xname="X", ...) {
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
  h<-hist(x, breaks=breaks, col=col, xlim=xlim,main = paste("Histogramme à pas constant de " , xname), probability=T, ...)
}

#Fonction pour tracer un histogramma à classes de même effectif
histoeff = function(x, xlim=NULL,xname="X", ...){
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
  h<-hist(x, breaks=breaks, col=col, xlim=xlim,main = paste("Classes de même effectif de " , xname), probability=T, ...)
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
summary(cuve1)
summary(cuve2)
summary(cuve3)

# Variance et écart-type empiriques
var(cuve1)
var(cuve2)
var(cuve3)
sd(cuve1)
sd(cuve2)
sd(cuve3)

# Coefficient de variation empirique
sd(cuve1)/mean(cuve1)
sd(cuve2)/mean(cuve2)
sd(cuve3)/mean(cuve3)

# Histogrammes à pas constant
histopas(cuve1,xname="cuve1")
histopas(cuve2,xname="cuve2")
histopas(cuve3,xname="cuve3")

# Histogrammes classes de même effectif
histoeff(cuve1,xname="cuve1")
histoeff(cuve2,xname="cuve2")
histoeff(cuve3,xname="cuve3")



#Question n°5:
#==============

lncuve1 <- log(cuve1/2)
lncuve2 <- log(cuve2/2)
lncuve3 <- log(cuve3/2)

par(mfrow = c(1, 3))

plot(sort(lncuve1)[1 :length(lncuve1)-1],log(1-seq(1 :(length(lncuve1)-1))/(length(lncuve1))),xlab="sort(lncuve1)",ylab="log(1-seq(1 :(n-1))/(n)")

plot(sort(lncuve2)[1 :length(lncuve2)-1],log(1-seq(1 :(length(lncuve2)-1))/(length(lncuve2))),xlab="sort(lncuve2)",ylab="log(1-seq(1 :(n-1))/(n)")

plot(sort(lncuve3)[1 :length(lncuve3)-1],log(1-seq(1 :(length(lncuve3)-1))/(length(lncuve3))),xlab="sort(lncuve3)",ylab="log(1-seq(1 :(n-1))/(n)")

abline(v=0)
abline(h=0)



1/mean(lncuve1)
1/mean(lncuve2)
1/mean(lncuve3)


#Question n°6:
#==============


par(mfrow = c(1, 2))

plot(sort(cuve1)[1 :length(cuve1)-1],log(1-seq(1 :(length(cuve1)-1))/(length(cuve1))),xlab="sort(cuve1)",ylab="log(1-seq(1 :(n-1))/(n)")

plot(sort(lncuve2)[1 :length(cuve2)-1],log(1-seq(1 :(length(cuve2)-1))/(length(cuve2))),xlab="sort(cuve2)",ylab="log(1-seq(1 :(n-1))/(n)")


abline(v=0)
abline(h=0)

par(mfrow = c(1, 1))
plot(sort(cuve3)[1 :length(cuve3)-1],qnorm(seq(1 :(length(cuve3)-1))/(length(cuve3))),xlab="sort(cuve3)",ylab="qnorm(seq(1 :(n-1))/n)")
abline(v=0)
abline(h=0)

