# Kody Ÿród³owe funkcji.

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
# W talii s¹ cztery kolory: pik, kier, karo, trefl.
# Ka¿dy z kolorów posiada 9 kart numerowanych: 2, 3, 4, 5, 6, 7, 8, 9, 10, 3 figury J, Q, K oraz Asa A.

# Gracz 1 ma na rêce 6 trefl i 1 J pik (wartoœæ 16).
# Gracz 2 ma 2 karty, jedna z nich to K trefl, druga zaœ nie jest widoczna.
# Teraz gracze maj¹ mo¿liwoœæ dobrania jeszcze jednej karty.
# Gracz 2 zdecyduje, ¿e nie dobiera wiêcej kart.

# Liczba rozgrywek wynosi 1000.
n <- 1000

# Zauwa¿my, ¿e gracz 1 ma na rêce dok³adnie 6 asóW i J pik.
# Inne przypadki s¹ wykluczone, gdy¿ wartoœæ by³aby wiêksza od 16.

# Przypadki:
# (I) Gracz 1 nie wyci¹ga karty.

# Podprzypadki:
# 1. Gracz 2 ma dwójkê jako drug¹ kartê. 
# Gracz 1 wygra.
# 2. Gracz 2 ma trójkê jako drug¹ kartê.
# Gracz 1 wygra.
# ...
# 5. Gracz 2 ma pi¹tkê jako drug¹ kartê.
# Gracz 1 wygra.
# 6. Gracz 2 ma szóstkê jako drug¹ kartê.
# Tym razem gracz 1 nie wygra, bo jest remis.
# 7. Gracz 2 ma siódemkê jako drug¹ kartê.
# Gracz 1 nie wygra.
# ...
# 9. Gracz 2 ma dziesi¹tkê jako drug¹ kartê.
# Gracz 1 nie wygra.
# 10. Gracz 2 ma figurê J.
# Gracz 1 nie wygra.
# ...
# 12. Gracz 2 ma figurê K.
# Gracz 1 nie wygra. 
# 13. Gracz 2 ma Asa.
# Gracz 1 nie wygra.

# Zatem:
x <- ceiling(runif(n, 0, 44))
length(which(x <= 16)) / 1000

# (II) Gracz 1 wyci¹ga kartê.

# Podprzypadki:

# 1. Gracz 1 wyci¹gn¹³ dwójkê.
# Wtedy ma 18 punktów.
# Jeœli gracz 2 jako drug¹ kartê ma dwójkê, trójkê, ... lub siódemkê, to gracz 1 wygrywa.
# Gdy gracz 2 jako drug¹ kartê ma ósemkê, dziewi¹tkê lub dziesi¹tkê, to gracz 1 nie wygrywa.
# Je¿eli gracz 2 jako drug¹ kartê ma figurê lub Asa, to te¿ gracz 1 nie wygrywa.
# Czyli jest (6 * 4 - 1) na 43 mo¿liwoœci, gdzie gracz 1 wygra.

pr <- rep(0,13)

x <- ceiling(runif(n, 0, 44))
pr[1] <- length(which(x <= 23)) / 1000 #0.527

# 2. Gracz 1 wyci¹gn¹³ trójkê.
# Wtedy ma 19 punktów.
# Jest wtedy (7 * 4 - 1) na 43 mo¿liwoœci, gdzie gracz 1 wygra.

x <- ceiling(runif(n, 0, 44))
pr[2] <- length(which(x <= 27)) / 1000 #0.636

# 3. Gracz 1 wyci¹gn¹³ czwórkê.
# Wtedy ma 20 punktów.
# Jest wtedy (8 * 4 - 1) na 43 mo¿liwoœæi, gdzie gracz 1 wygra.

x <- ceiling(runif(n, 0, 44))
pr[3] <- length(which(x <= 31)) / 1000 #0.691

# 4. Gracz 1 wyci¹gn¹³ pi¹tkê.
# Wtedy ma 21 punktów.
# Jest wtedy (9 * 4 - 1  + 3 * 4 - 2) na 43 mo¿liwoœci, gdzie gracz 1 wygra.

x <- ceiling(runif(n, 0, 44))
pr[4] <- length(which(x <= 35)) / 1000 #0.818

# 5. Gracz 1 wyci¹gn¹³ szóstkê, siódemkê, ..., lub dziesi¹tkê.
# Gracz 1 nie wygra, bo ma co najmniej 22 > 21 punktów.

x <- ceiling(runif(n, 0, 44))
pr[5] <- length(which(x <= 0)) / 1000
pr[6] <- length(which(x <= 0)) / 1000
pr[7] <- length(which(x <= 0)) / 1000
pr[8] <- length(which(x <= 0)) / 1000
pr[9] <- length(which(x <= 0)) / 1000


# 6. Gracz 1 wyci¹gn¹³ figurê.
# Gracz 1 ma 26 > 21 punktów, wiêc przegra grê, czyli jej nie wygra.

x <- ceiling(runif(n, 0, 44))
pr[10] <- length(which(x <= 0)) / 1000 #0
pr[11] <- length(which(x <= 0)) / 1000 #0
pr[12] <- length(which(x <= 0)) / 1000 #0

# 7. Gracz 1 wyci¹gn¹³ Asa.
# Gracz 1 ma 17 punktów.
# Jest (5 * 4 - 1) na 43 mo¿liwoœæi, gdzie gracz 1 wygra.

x <- ceiling(runif(n, 0, 44))
pr[13] <- length(which(x <= 19)) / 1000 #0.453

mean(pr)

# P-stwem jest œrednia p-stw z wszystkich przypadków wziêtej karty przez gracza 1.
# St¹d p-stwo wynosi 0.2403846

# Zadanie 4
jakajestosoba <- function(osoba, narodowosc)
{
  (sum(osoba[c == narodowosc]) + 1) / (sum(length(osoba[c == narodowosc])) + 2)
}

naiwny_bayes <- function(X, c, z)
{
  # X jest macierz¹ zerojedynkow¹, c jest wektorem napisów, z jest wektorem zerojedynkowym.
  # Liczba elementów wektora z ma byæ równa liczbie kolumn macierzy X, poniewa¿ z opisuje 5 cech cz³owieka.
  # Z za³o¿enia c ma liczbê elementów równ¹ liczbie wierszy macierzy X.
  stopifnot(is.matrix(X) & is.numeric(X) & is.character(c) & is.numeric(z) & ncol(X) == length(z) & nrow(X) == length(c))
  
  nAnglik <- length(which(c == 'Anglik')) # Liczba elementów 'Anglik' w wektorze c.
  nSzkot <- length(which(c == 'Szkot')) # Liczba elementów 'Szkot' w wektorze c.
  
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

