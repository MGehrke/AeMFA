## R-Code B.1 ----
# AIC, AICc und BIC in der Panelregression berechnen
pAICBIC <- function(mod){
  # Residuen
  res <- residuals(mod)
  # Anzahl der Subjekte
  N <- attr(res, "index")[, 1] |> levels() |> length()
  # Anzahl der Zeitpunkte
  T <- attr(res, "index")[, 2] |> levels() |> length()
  # logarithmierter Standardfehler der Residuen
  logSE <- log(sum(res^2)/(N*T))
  # Anzahl der Parameter
  # enthält im Random-Effects-Modell den Intercept
  K <- coef(mod) |> length()
  # plus 1 für sigma
  K <- K + 1
  if(mod$args$model == "within" && mod$args$effect == "individual"){
    # plus Anzahl Subjekte
    K <- K + N
  } else if(mod$args$model == "within" && mod$args$effect == "time"){
    # plus Anzahl Zeitpunkte
    K <- K + T
  } else if(mod$args$model == "within" && mod$args$effect == "twoways"){
    # plus Anzahl Subjekte x Zeitpunkte
    K <- K + N*T
  } else if(mod$args$model == "random" && mod$args$effect == "individual"){
    # plus Varianz der zufälligen Effekte
    K <- K + 1
  } else if(mod$args$model == "random" && mod$args$effect == "time"){
    # plus Varianz der zufälligen Effekte
    K <- K + 1
  } else if(mod$args$model == "random" && mod$args$effect == "twoways"){
    # plus Varianz der zufälligen Effekte für Subjekt und Zeit
    K <- K + 2
  } else{
    cat("Modelltyp nicht implementiert!\n")
    return()
  }
  # AIC, AICc und BIC berechnen
  AIC <- N*T*logSE + 2*K
  AICc <- AIC + (2*K*(K+1))/(N*T-K-1)
  BIC <- N*T*logSE + K*log(N*T)
  
  retval <- c(AIC, AICc, BIC)
  names(retval) <- c("AIC", "AICc", "BIC")
  return(retval)
}


## R-Code B.2 ----
# Optimaler Cutpoint gemaß Summe y = Summe yhat
# notwendige Variablen: Modell, y-Variable, Wert für y = 1
optCP <- function(mod, yvar, success){
  # Variable mit 0 und 1 erzeugen
  y <- ifelse(mod$model[yvar] == success, 1, 0)
  sumy <- sum(y)
  # cutpoint durchlaufen lassen
  for(i in 0:10000){
    cutpoint <- 0.0001 * i
    # Typo korrigiert: KRED.glm6 durch mod ersetzt
    ypred <- ifelse(fitted(mod) > cutpoint, 1, 0)
    sumypred <- sum(ypred)
    if(sumy >= sumypred) break
  }
  sprintf("Summe y: %d, Summe yhat: %d\n", sumy, sumypred) |> cat()
  names(cutpoint) <- "Cutpoint"
  return(cutpoint)
}


## R-Code B.3 ----
# Wu-Hausmann-Test 
# H0: beide Modelle konsistent
# Hinweis: funktioniert nicht bei bife- und glmer-Modellen
whtest <- function(mod1, mod2){
  # Koeffizienten
  coef1 <- coef(mod1)
  coef2 <- coef(mod2)
  vcov1 <- vcov(mod1)
  vcov2 <- vcov(mod2)
  names1 <- names(coef1)
  names2 <- names(coef2)
  # nur gemeinsame Koeffizienten auswählen
  # damit wird (Intercept) in REM sowie
  # (Intercept) und sigma in logistischem REM gelöscht
  if(length(names1) <= length(names2)){
    # weniger oder gleich viele Koeffizienten in Modell 1
    # Standard gemäß Formel
    idx <- names2[names2 %in% names1]
    # Differenz der Koeffizienten und der VCOV-Matrix bilden
    dcoef <- coef2[idx] - coef1[idx]
    dvcov <- vcov1[idx, idx] - vcov2[idx, idx]
    # Hinweis zu HA ausgeben
    cat("HA: Model 2 ist inkonsistent\n")
  } else {
    # weniger Koeffizienten in Modell 2
    idx <- names1[names1 %in% names2]
    # Differenz der Koeffizienten und der VCOV-Matrix bilden
    dcoef <- coef1[idx] - coef2[idx]
    dvcov <- vcov2[idx, idx] - vcov1[idx, idx]
    # Hinweis zu HA ausgeben
    cat("HA: Model 1 ist inkonsistent\n")
  }
  # Anzahl Freiheitsgrade
  df <- length(dcoef)
  # Teststatistik berechnen
  chi2 <- t(dcoef) %*% solve(dvcov) %*% dcoef |> as.numeric()
  # Überschreitungswahrscheinlichkeit Chi2-Verteilung
  pval <- pchisq(chi2, df = df, lower.tail = FALSE)
  # Ausgabe
  sprintf("Chi2 = %.4f, df = %d, p-Wert = %.4g\n", chi2, df, pval) |> cat()
}


## R-Code B.4 ----
# AIC und ggf. BIC in der logistischen Panelregression berechnen
lpAICBIC <- function(mod){
  # Devianz berechnen
  deviance <- -2 * logLik(mod)
  # Anzahl der Koeffizienten
  ncoef <- coef(mod) |> length()
  if(ncoef <= 1)
    # Anderer Weg für glmer, da coef sowohl fixe als auch zufällige Effekte ausgibt
    ncoef <- fixef(mod) |> length() + 1
  # Stichprobenumfang
  # funktioniert bei den meisten Modellklassen
  nobs <- fitted(mod) |> length()
  # AIC berechnen
  AIC <- deviance + 2 * ncoef
  if(nobs){
    BIC <- deviance + log(nobs) * ncoef
  } else {
    BIC <- NA
    cat("Anzahl der Beobachtungen konnte nicht bestimmt werden!\n")
  }
  retval <- c(AIC, BIC)
  names(retval) <- c("AIC", "BIC")
  return(retval)
}


## R-Code B.5 ----
# abnormale Renditen berechnen
getAR <- function(company, index, start, end){
  # Überprüfung, ob company und index xts-Objekte sind
  if(!is.xts(company) || !is.xts(index)){
    cat("Die übergebenen Zeitreihen sind keine xts-Objekte!\n")
    return(NULL)
  }
  wstext <- paste0(start, "/", end)
  data <- merge(company[wstext], index[wstext], join = "inner")
  names(data) <- c("y", "x")
  lm(y ~ x, data = data) |> coef() -> coefs
  ar <- company - (coefs[1] + coefs[2] * index)
  names(ar) <- "AR"
  return(ar)
}


## R-Code B.6 ----
# t-Test für abnormale Renditen
tTestAR <- function(ar, wsstart, wsend, westart, weend, nParam = 2, flPlot = FALSE){
  # Schätzfenster
  wstext <- paste0(wsstart, "/", wsend)
  ws <- ar[wstext] |> na.omit()
  # Ereignisfenster
  wetext <- paste0(westart, "/", weend)
  we <- ar[wetext] |> na.omit()
  
  # Freiheitsgrade für t-Test: Länge Schätzfenster - nParam
  # nParam = 2 bei Einfaktormodell, wie z. B. Marktmodell
  df <- nrow(ws) - nParam
  # Standardfehler
  se <- sqrt(sum(ws^2)/df)
  
  # t-Werte
  t <- we/se
  # p-Werte für zweiseitigen Test
  p <- pt(abs(t), df, lower.tail = FALSE) * 2
  # Ergebnisse in xts-Objekt t.test abspeichern
  t.test <- cbind(we, se, t, p)
  names(t.test) <- c("AR", "se", "t-Wert", "p-Wert")
  
  # CAR berechnen und t-Test durchführen
  CAR <- sum(we)
  SE <- sqrt(nrow(we)) * se
  T <- CAR/SE
  pCAR <- pt(abs(T), df, lower.tail = FALSE) * 2
  
  # Ergebnisse in xts-Objekt CAR.t.test abspeichern
  CAR.t.test <- c(CAR, SE, T, pCAR)
  names(CAR.t.test) <- c("CAR", "se", "t-Wert", "p-Wert")
  
  # Grafik ausgeben
  if(flPlot){
    # Plot mit +/- 2 se
    autoplot(we) |> 
      gf_hline(yintercept = 0, color = "blue") |> 
      gf_hline(yintercept = c(-2 * se, 2 * se), color = "blue", linetype = 2)
  }
  
  retval <- list(t.test, CAR.t.test)
  names(retval) <- c("AR", "CAR")
  
  return(retval)
}


## R-Code B.7 ----
# Corrado-Rang-Test für abnormale Renditen
CRTestAR <- function(ar, wsstart, wsend, westart, weend, nParam = 2){
  # Schätzfenster
  wstext <- paste0(wsstart, "/", wsend)
  ws <- ar[wstext] |> na.omit()
  # Ereignisfenster
  wetext <- paste0(westart, "/", weend)
  we <- ar[wetext] |> na.omit()
  
  # Schätz- und Ereignisfenster kopieren
  wk <- c(ws, we)
  # Rangplätze in xts ablegen
  coredata(wk) <- rank(as.vector(wk))
  # mittlerer Rang
  kq <- nrow(wk)/2 + 0.5
  # Abweichungen vom mittleren Rang
  ak <- wk - kq
  
  # Standardfehler
  sek <- sqrt(sum(ak^2)/nrow(ak))
  
  # Ereignisfenster mit Abweichungen vom mittleren Rang
  wek <- ak[wetext]
  
  # t-Werte
  t <- wek/sek
  
  # Freiheitsgrade für t-Test: Länge Schätzfenster - nParam
  # nParam = 2 bei Einfaktormodell, wie z. B. Marktmodell
  df <- nrow(ws) - nParam
  
  # p-Werte für zweiseitigen Test
  p <- pt(abs(t), df, lower.tail = FALSE) * 2
  
  # Ergebnisse in xts-Objekt CR.test abspeichern
  CR.test <- cbind(we, wek, sek, t, p)
  names(CR.test) <- c("AR", "AK", "se", "t-Wert", "p-Wert")
  
  # CAK berechnen und t-Test durchführen
  CAK <- sum(wek)/nrow(wek)
  T <- CAK/sek
  pCAK <- pt(abs(T), df, lower.tail = FALSE) * 2
  
  # CAR
  CAR <- sum(we)
  # Ergebnisse in xts-Objekt CAR.CR.test abspeichern
  CAR.CR.test <- c(CAR, CAK, sek, T, pCAK)
  names(CAR.CR.test) <- c("CAR", "CAK", "se", "t-Wert", "p-Wert")
  retval <- list(CR.test, CAR.CR.test)
  names(retval) <- c("AR", "CAR")
  
  return(retval)
}


## R-Code B.8 ----
# Optimale Anzahl von Lags für die Funktion Box.test
optBT <- function(x, maxLags = 20, type = "Box-Pierce", fitdf = 0, flPrint = FALSE){
  vals <- data.frame(lags = 0, pval = 1)
  # Schleife von 1 bis maxLags
  for(i in 1:maxLags){
    # Box-Pierce- bzw. Ljung-Box-Test durchführen
    out <- Box.test(DAX$Rendite, lag = i, type, fitdf)
    # Ergebnis abspeichern
    vals <- rbind(vals, data.frame(lags = out$parameter, pval = out$p.value))
    # ggf. Ergebnis ausgeben
    if(flPrint == TRUE){
      sprintf("Lags: %d, p-Value: %.4f\n", out$parameter, out$p.value) |> cat()
    }
  }
  # Lag für den kleinsten p-Wert ermitteln
  minLags <- vals[which.min(vals$pval), "lags"]
  # Box-Pierce- bzw. Ljung-Box-Test mit diesem Wert zurückgeben
  return(Box.test(x, lag = minLags, type, fitdf))
}

