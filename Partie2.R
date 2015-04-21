# Projet Principes et Méthodes Statistiques
#Question 2
m <- 100
n <- 100
a <- 3
b <- 2
alpha <- 0.9

# Fonction qui simule un échantillon de taille n de la loi Pa(a,b)
EchantillonPa <- function(a,b,n)
{
	U <- runif(n,0,1)
	U <- sort(U)
	loiinv <- b/((1-U)^(1/a))
	return (loiinv);
}

#Fonction qui calcule la proportion d'intervale incluant a
Calculer_Proportion <- function(m,a,b,n, alpha)
{
Compteur <- 0
for (i in 1:m)
{
	cuves <- EchantillonPa(a,b,n)
	y <- log(cuves/2)
	#IntervallesConf
	borne1 <-qchisq(alpha/2,2*n)/(2*sum(y))
	borne2 <-qchisq(1-alpha/2,2*n)/(2*sum(y))
	if (borne1 < a & borne2 > a)  
	{
		Compteur <- Compteur + 1
	}
}

prop <- Compteur / m
return (prop);
}

par(mfcol=c(1,1))
alpha <- c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
proportion <- c(Calculer_Proportion(100,3,2,10, alpha[1]), Calculer_Proportion(100,5,b,10, alpha[2]),Calculer_Proportion(100,10,b,10, alpha[3]), Calculer_Proportion(100,3,b,30, alpha[4]), Calculer_Proportion(100,5,b,50, alpha[5]), Calculer_Proportion(100,3,2,100, alpha[6]), Calculer_Proportion(100,5,b,500, alpha[7]),Calculer_Proportion(100,10,b,10, alpha[8]),Calculer_Proportion(100,3,b,30, alpha[9]),Calculer_Proportion(100,5,b,50, alpha[10]),Calculer_Proportion(100,5,b,400, alpha[11]),Calculer_Proportion(100,5,b,300, alpha[12]),Calculer_Proportion(100,5,b,200, alpha[13]))

# Affichage sous forme graphique pour expliciter
plot(alpha,proportion, col="red", main="Proportion d'IC contenant la valeur de a pour différents alpha")
print(proportion)


#Question 3
# Méthode des Moments et Méthode de maximum de vraisemblance pour estimer le paramètre a
EMM <- function(Echantillon)
{
	Est <- 1/mean(Echantillon)
	return (Est);
}

# Estimateur sans biais et de variance minimale (ESBVM)
ESBVM <- function(Echantillon)
{
	n <- length(Echantillon)
	Est <- (n-1)/sum(Echantillon)
	return (Est);
}

# Calcul du biais et de l'EQM chacun des m estimateurs (EMV et ESBVM)

Qualite_Estimateur<-function(m,n,a)
{
	EMV<-rep(0,m)
	ESBVM<-rep(0,m)
	for (i in 1:m) 
	{
		Ech<-EchantillonPa(a,b,n)
		x <- log(Ech/2)
		EMV[i]<- 1/mean(x)
		print("EMV")
		print(EMV[i])
		ESBVM[i]<- (n-1)*EMV[i]/n
		print("ESBVM")
		print(ESBVM[i])
	}
	par(mfcol=c(1,2))
	hist(EMV,prob=T)
	lines(c(a,a),c(0,a),col="blue",lwd=4)
	lines(c(mean(EMV),mean(EMV)),c(0,a),col="red",lwd=4)
	hist(ESBVM,prob=T)
	lines(c(a,a),c(0,a),col="blue",lwd=4)
	lines(c(mean(ESBVM),mean(ESBVM)),c(0,a),col="red",lwd=4)
	cat("Moyenne des biais de l’EMV", mean(EMV-a), "\n")
	cat("Moyenne des biais de l’ESBVM", mean(ESBVM-a), "\n")
	cat("Moyenne des EQM de l’EMV", mean((EMV-a)^2), "\n")
	cat("Moyenne des EQM de l’ESBVM", mean((ESBVM-a)^2), "\n")
}

#Qualite_Estimateur(100,5,10)
#Qualite_Estimateur(100,20,10)
#Qualite_Estimateur(100,50,10)
#Qualite_Estimateur(100,100,10)
#Qualite_Estimateur(100,500,10)

#ESBVM meilleur sur le biais et sur l'EQM

#Question 4
# Vérification de la loi faible des grands nombres
Variation_E <- function (m,n,a, Epsillon){
Compteur <- 0
for (i in 1:m)
{
	cuves <- EchantillonPa(a,b,n)
	#Moyenne
	Moy <- mean(cuves)
	#Esperance
	E <- (2*a)/(a-1) - (a*(2^a))/((a-1)*(mean(cuves)^(a-1)))
	Diff <- abs( Moy - E)
	print(Diff)
	if (Diff > Epsillon )
	{
		Compteur <- Compteur + 1
	}

}
return (Compteur);
}
j <- 500
Cpt <- rep(0,j)
nb <- rep(0,j)
#for ( i in 1:j)
#{
#	nb[i] <- i
#	Cpt[i] <- Variation_E(100,i,10,1)
#}
#par(mfcol=c(1,1))
#plot(nb, Cpt, col="purple", xlab="valeur de n", ylab="Nombre d'ecarts entre Moyenne et Esperance", main=" Variation de l'écart entre Moyenne et Esperance en fonction de n (Epsilon=1)")

#Question 5
#Vérification du théorème central limite
Loi_Normale <- function (m,n,a, Epsillon){
Moy <- rep(0,m)
for (i in 1:m)
{
	Echan <- EchantillonPa(a,b,n)
	Moy[i] <- mean(Echan)	
	
}
		par(mfcol=c(1,2))
	#Histogramme pour la loi normale sur Echan
	hist(Moy, prob=T, col="gray", xlab="Moyenne des échantillons", main="Histogramme des moyennes empiriques des m échantillons")
	#Graphe de probabilités pour la loi normale
	qqnorm(Moy, col="blue",ylab="Moyenne des échantillons", main="Graphe de probabilité pour la loi normale sur les m moyennes empiriques")
	qqline(Moy, col="red")
}
#Loi_Normale(100,5,10,1)
#Loi_Normale(100,20,10,1)
#Loi_Normale(100,100,10,1)
#Loi_Normale(100,1000,10,1)
#Loi_Normale(100,10000,10,1)

# Question 6
# Etude de la convergence en loi des estimateurs
Convergence_Loi <- function (m,n,a){

	Echan <- EchantillonPa(a,b,n)
	x <- length(Echan)
	# On trace la fonction de repartition Empirique
	plot(Echan, 1:x/x,main = "Fonction de répartition empirique ",xlab="X", ylab=" F(x) = 1-2^a/x^a", col="purple");
}
# On trace la fonction de repartition normale
x <- seq(2,50)
#Pour la fonction de repartition on prend a <- 0.5
a <- 0.5
	par(mfcol=c(1,5))
Convergence_Loi(100,5,5)
Convergence_Loi(100,20,5)
Convergence_Loi(100,100,5)
Convergence_Loi(100,1000,5)
Convergence_Loi(100,10000,5)
	par(mfcol=c(1,1))
plot( x, 1-2^a/x^a, main = "Fonction de répartition", ylab=" F(x) = 1-2^a/x^a", col="gray");
lines( x, 1-2^a/x^a, col="blue");
