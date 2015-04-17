# Projet Principes et Méthodes Statistiques
#Question 2
m <- 100
n <- 100
a <- 3
b <- 2
alpha <- 0.9

# Fonction qui calcule l'inverse
EchantillonPa <- function(a,b,n)
{
	U <- runif(n,0,1)
	U <- sort(U)
	loiinv <- b/((1-U)^(1/a))
	#plot(loiinv,U)
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
	print(" Bornes -->")
	print(borne1)
	print(borne2)
	if (borne1 < a & borne2 > a)  
	{
		Compteur <- Compteur + 1
	}
}

prop <- Compteur / m
return (prop);
}

par(mfcol=c(1,2))
alpha <- c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
proportion <- c(Calculer_Proportion(100,3,2,10, alpha[1]), Calculer_Proportion(100,5,b,10, alpha[2]),Calculer_Proportion(100,10,b,10, alpha[3]), Calculer_Proportion(100,3,b,30, alpha[4]), Calculer_Proportion(100,5,b,50, alpha[5]), Calculer_Proportion(100,3,2,10, alpha[6]), Calculer_Proportion(100,5,b,10, alpha[7]),Calculer_Proportion(100,10,b,10, alpha[8]),Calculer_Proportion(100,3,b,30, alpha[9]),Calculer_Proportion(100,5,b,50, alpha[10]),Calculer_Proportion(100,5,b,50, alpha[11]),Calculer_Proportion(100,5,b,50, alpha[12]),Calculer_Proportion(100,5,b,50, alpha[13]))

# Affichage sous forme graphique pour expliciter
plot(alpha,proportion, col="red")
print(proportion)


