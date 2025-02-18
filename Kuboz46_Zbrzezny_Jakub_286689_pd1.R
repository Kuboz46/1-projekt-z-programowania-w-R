# Kody �r�d�owe funkcji.

# Zadanie 1
macierz_korelacji <- function(M)
{
  stopifnot(is.matrix(M) & is.numeric(M))
  n <- nrow(M)
  m <- ncol(M)
  b1 <- t(M - t(matrix(colMeans(M),m,n)))
  x <- sqrt(apply((t(M - t(matrix(colMeans(M),m,n))))^2, MARGIN = 1, sum))
  vb <- matrix(x,m,n)
  B <- b1 / vb
  c1 <- M - t(matrix(colMeans(M),m,n))
  vc <- t(matrix(x,m,n))
  C <- c1 / vc
  B %*% C
}

# Zadanie 2
podsumowanie <- function(M)
{
  stopifnot(ncol(M) > 3 & nrow(M) > 3 & is.matrix(M) & is.numeric(M))
  l <- list(NULL, NULL)
  l[[1]] <- colMeans(apply(M, MARGIN = 2, sort)[seq(1,3), ])
  n <- nrow(M)
  M1 <- M[seq(1, n - 1), ]
  M2 <- M[seq(2, n), ]
  l[[2]] <- 1/2 * (M1 + M2)
  l
}

# Zadanie 3

# Talia kart liczy 52 sztuk.
# W talii s� cztery kolory: pik, kier, karo, trefl.
# Ka�dy z kolor�w posiada 9 kart numerowanych: 2, 3, 4, 5, 6, 7, 8, 9, 10, 3 figury J, Q, K oraz Asa A.

# Gracz 1 ma na r�ce 6 trefl i 1 J pik (warto�� 16).
# Gracz 2 ma 2 karty, jedna z nich to K trefl, druga za� nie jest widoczna.
# Teraz gracze maj� mo�liwo�� dobrania jeszcze jednej karty.
# Gracz 2 zdecyduje, �e nie dobiera wi�cej kart.

# Liczba rozgrywek wynosi 1000.
n <- 1000

# Zauwa�my, �e gracz 1 ma na r�ce dok�adnie 6 as�W i J pik.
# Inne przypadki s� wykluczone, gdy� warto�� by�aby wi�ksza od 16.

# Przypadki:
# (I) Gracz 1 nie wyci�ga karty.

# Podprzypadki:
# 1. Gracz 2 ma dw�jk� jako drug� kart�. 
# Gracz 1 wygra.
# 2. Gracz 2 ma tr�jk� jako drug� kart�.
# Gracz 1 wygra.
# ...
# 5. Gracz 2 ma pi�tk� jako drug� kart�.
# Gracz 1 wygra.
# 6. Gracz 2 ma sz�stk� jako drug� kart�.
# Tym razem gracz 1 nie wygra, bo jest remis.
# 7. Gracz 2 ma si�demk� jako drug� kart�.
# Gracz 1 nie wygra.
# ...
# 9. Gracz 2 ma dziesi�tk� jako drug� kart�.
# Gracz 1 nie wygra.
# 10. Gracz 2 ma figur� J.
# Gracz 1 nie wygra.
# ...
# 12. Gracz 2 ma figur� K.
# Gracz 1 nie wygra. 
# 13. Gracz 2 ma Asa.
# Gracz 1 nie wygra.

# Zatem:
x <- ceiling(runif(n, 0, 44))
length(which(x <= 16)) / 1000

# (II) Gracz 1 wyci�ga kart�.

# Podprzypadki:

# 1. Gracz 1 wyci�gn�� dw�jk�.
# Wtedy ma 18 punkt�w.
# Je�li gracz 2 jako drug� kart� ma dw�jk�, tr�jk�, ... lub si�demk�, to gracz 1 wygrywa.
# Gdy gracz 2 jako drug� kart� ma �semk�, dziewi�tk� lub dziesi�tk�, to gracz 1 nie wygrywa.
# Je�eli gracz 2 jako drug� kart� ma figur� lub Asa, to te� gracz 1 nie wygrywa.
# Czyli jest (6 * 4 - 1) na 43 mo�liwo�ci, gdzie gracz 1 wygra.

pr <- rep(0,13)

x <- ceiling(runif(n, 0, 44))
pr[1] <- length(which(x <= 23)) / 1000 #0.527

# 2. Gracz 1 wyci�gn�� tr�jk�.
# Wtedy ma 19 punkt�w.
# Jest wtedy (7 * 4 - 1) na 43 mo�liwo�ci, gdzie gracz 1 wygra.

x <- ceiling(runif(n, 0, 44))
pr[2] <- length(which(x <= 27)) / 1000 #0.636

# 3. Gracz 1 wyci�gn�� czw�rk�.
# Wtedy ma 20 punkt�w.
# Jest wtedy (8 * 4 - 1) na 43 mo�liwo��i, gdzie gracz 1 wygra.

x <- ceiling(runif(n, 0, 44))
pr[3] <- length(which(x <= 31)) / 1000 #0.691

# 4. Gracz 1 wyci�gn�� pi�tk�.
# Wtedy ma 21 punkt�w.
# Jest wtedy (9 * 4 - 1  + 3 * 4 - 2) na 43 mo�liwo�ci, gdzie gracz 1 wygra.

x <- ceiling(runif(n, 0, 44))
pr[4] <- length(which(x <= 35)) / 1000 #0.818

# 5. Gracz 1 wyci�gn�� sz�stk�, si�demk�, ..., lub dziesi�tk�.
# Gracz 1 nie wygra, bo ma co najmniej 22 > 21 punkt�w.

x <- ceiling(runif(n, 0, 44))
pr[5] <- length(which(x <= 0)) / 1000
pr[6] <- length(which(x <= 0)) / 1000
pr[7] <- length(which(x <= 0)) / 1000
pr[8] <- length(which(x <= 0)) / 1000
pr[9] <- length(which(x <= 0)) / 1000


# 6. Gracz 1 wyci�gn�� figur�.
# Gracz 1 ma 26 > 21 punkt�w, wi�c przegra gr�, czyli jej nie wygra.

x <- ceiling(runif(n, 0, 44))
pr[10] <- length(which(x <= 0)) / 1000 #0
pr[11] <- length(which(x <= 0)) / 1000 #0
pr[12] <- length(which(x <= 0)) / 1000 #0

# 7. Gracz 1 wyci�gn�� Asa.
# Gracz 1 ma 17 punkt�w.
# Jest (5 * 4 - 1) na 43 mo�liwo��i, gdzie gracz 1 wygra.

x <- ceiling(runif(n, 0, 44))
pr[13] <- length(which(x <= 19)) / 1000 #0.453

mean(pr)

# P-stwem jest �rednia p-stw z wszystkich przypadk�w wzi�tej karty przez gracza 1.
# St�d p-stwo wynosi 0.2403846

# Zadanie 4
jakajestosoba <- function(osoba, narodowosc)
{
  (sum(osoba[c == narodowosc]) + 1) / (sum(length(osoba[c == narodowosc])) + 2)
}

naiwny_bayes <- function(X, c, z)
{
  # X jest macierz� zerojedynkow�, c jest wektorem napis�w, z jest wektorem zerojedynkowym.
  # Liczba element�w wektora z ma by� r�wna liczbie kolumn macierzy X, poniewa� z opisuje 5 cech cz�owieka.
  # Z za�o�enia c ma liczb� element�w r�wn� liczbie wierszy macierzy X.
  stopifnot(is.matrix(X) & is.numeric(X) & is.character(c) & is.numeric(z) & ncol(X) == length(z) & nrow(X) == length(c))
  
  nAnglik <- length(which(c == 'Anglik')) # Liczba element�w 'Anglik' w wektorze c.
  nSzkot <- length(which(c == 'Szkot')) # Liczba element�w 'Szkot' w wektorze c.
  
  n <- length(c)
  
  praprA <- nAnglik / n
  praprSz <- nSzkot / n
  
  
  naroducz <- c(praprA, praprSz)
  names(naroducz) <- c('Anglik', 'Szkot')
  
  prwarAnglik <- apply(X, MARGIN = 2, function(os) jakajestosoba(os, 'Anglik'))
  names(prwarAnglik) = c('x1 = ', 'x2 = ', 'x3 = ', 'x4 = ', 'x5 = ')
  
  prwarSzkot <- apply(X, MARGIN = 2, function(os) jakajestosoba(os, 'Szkot'))
  names(prwarSzkot) <- c('x1 = ', 'x2 = ', 'x3 = ', 'x4 = ', 'x5 = ')
  
  os1 <- log(praprA) + sum(log(z * prwarAnglik + (1 - z) * (1 - prwarAnglik)))
  os2 <- log(praprSz) + sum(log(z * prwarSzkot + (1 - z) * (1 - prwarSzkot)))
  
  if (os1 >= os2)
  {
    grupa <- 'Anglik'
  }
  else
  {
    grupa <- 'Szkot'
  }
  
  podl <- list(naroducz, prwarAnglik, prwarSzkot)
  names(podl) <- c('apriori', 'Anglik', 'Szkot')
  
  
  klas <- list(podl, grupa)
  names(klas) <- c('prob', 'group')
  klas
}

# Zadanie 5

pairs_bootstrap <- function(x, y, M)
{
  stopifnot(is.numeric(x) & is.numeric(y) & length(x) == length(y) & is.numeric(M) &  M >= 1)
  
  indeksy <- replicate(M, 1:length(x))
  
  probka_x <- x[indeksy]
  probka_y <- y[indeksy]
  
  model <- lm(probka_y~probka_x)
  c(quantile(model$coefficients[1], 0.95), quantile(model$coefficients[2], 0.95))
}

