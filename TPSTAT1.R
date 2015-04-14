#Exercice 1 TP STATS
# Données des 3 cuves
cuve1 <- c(2.007,
4.440,
2.086,
2.428,
2.057,
2.107,
2.190,
2.325,
3.950,
2.551,
2.284,
2.373,
2.464,
2.261,
2.408,
2.910,
2.775,
3.551,
3.643,
2.017,
2.767,
2.334,
2.699,
3.163,
2.748,
5.404,
2.601,
2.970,
6.416,
2.217)
cuve2 <-c(3.698,
2.536,
2.006,
2.207,
2.222,
2.591,
2.889,
2.236,
5.437,
2.406,
2.498,
3.643,
2.185,
2.661,
2.281,
2.237,
2.362,
2.512,
2.139,
2.745,
2.026,
2.035,
2.821,
2.102,
2.319)
cuve3 <- c(2.059,
3.270,
2.438,
3.070,
2.451,
3.134,
2.551,
2.781,
3.029,
2.863,
2.638,
3.770,
2.103,
3.334,
2.660,
2.229,
3.100,
2.673,
2.782,
2.720,
2.449,
2.800,
3.197,
2.662,
2.840,
2.738,
3.722,
2.921)

#Indicateurs statistiques
print("Indicateurs Statistiques")
#cuve1 Indicateurs Stats + Variance
print("Cuve1")
sum1 = summary(cuve1)
print(sum1)
print("Variance -->")
print(var(cuve1))
#cuve2 Indicateurs Stats + Variance
print("Cuve2")
sum2 = summary(cuve2)
print(sum2)
print(" Variance -->")
print( var(cuve2))
#cuve3 Indicateurs Stats + Variance
print("Cuve3")
sum3 = summary(cuve3)
print(sum3)
print("Variance -->")
print( var(cuve3))

#Histogrammes
#cuve1 Meme largeur k= 6 d'après règle de Sturges
cuve1ORD <- sort(cuve1)
n <-length(cuve1)
a0<-cuve1ORD[1]-0.025*(cuve1ORD[n]-cuve1ORD[1])
a6<-cuve1ORD[n]+0.025*(cuve1ORD[n]-cuve1ORD[1])

# Bornes des classes : on partage [a0,a6] en 6 intervalles
# de largeur h.
h <- (a6-a0)/6
bornesLar1<-seq(a0,a6,h)

#cuve1 même effectif
b<-c(5,10,15,20,25)
bornesEff1<-c(a0,(cuve1ORD[b]+ cuve1ORD[b+1])/2,a6)

#cuve2 Meme largeur k= 5 d'après règle de Sturges
cuve2ORD <- sort(cuve2)
n <-length(cuve2)
a0<-cuve2ORD[1]-0.025*(cuve2ORD[n]-cuve2ORD[1])
a5<-cuve2ORD[n]+0.025*(cuve2ORD[n]-cuve2ORD[1])

# Bornes des classes : on partage [a0,a6] en 6 intervalles
# de largeur h.
h <- (a5-a0)/5
bornesLar2<-seq(a0,a5,h)

#cuve2 même effectif
b<-c(5,10, 15, 20)
bornesEff2<-c(a0,(cuve2ORD[b]+cuve2ORD[b+1])/2,a6)

#cuve3 Meme largeur k= 6 d'après règle de Sturges
cuve3ORD <- sort(cuve3)
n <-length(cuve3)
a0<-cuve3ORD[1]-0.025*(cuve3ORD[n]-cuve3ORD[1])
a6<-cuve3ORD[n]+0.025*(cuve3ORD[n]-cuve3ORD[1])

# Bornes des classes : on partage [a0,a6] en 6 intervalles
# de largeur h.
h <- (a6-a0)/6
bornesLar3<-seq(a0,a6,h)

#cuve3 même effectif
b<-c((28/6)*1,(28/6)*2 ,(28/6)*3 ,(28/6)*4, (28/6)*5 )
bornesEff3<-c(a0,(cuve3ORD[b]+cuve3ORD[b+1])/2,a6)


# Histogrammes
par(mfcol=c(2,3))
hist(cuve1, prob=T, breaks=bornesLar1, col="red" ,main="Histogramme meme largeur Cuve1", xlab =" Valeurs des défauts Cuve 1")
hist(cuve1, prob=T, breaks=bornesEff1, col="red", main="Histogramme meme Effectif Cuve1", xlab =" Valeurs des défauts Cuve 1")
hist(cuve2, prob=T, breaks=bornesLar2, col="blue", main="Histogramme meme largeur Cuve2", xlab =" Valeurs des défauts Cuve 2")
hist(cuve2, prob=T, breaks=bornesEff2, col="blue", main="Histogramme meme Effectif Cuve2", xlab =" Valeurs des défauts Cuve 2")
hist(cuve3, prob=T, breaks=bornesLar3, col="green", main="Histogramme meme largeur Cuve3", xlab =" Valeurs des défauts Cuve 3")
hist(cuve3, prob=T, breaks=bornesEff3, col="green",  main="Histogramme meme Effectif Cuve3", xlab =" Valeurs des défauts Cuve 3")

