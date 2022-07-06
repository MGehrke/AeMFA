#' @title AeMFA: Functions of the book \emph{Angewandte empirische Methoden in Finance & Accounting}
#'
#' @description This package provides the functions which where developed for the book \emph{"Angewandte empirische Methoden in Finance & Accounting}.
#'
#'@name AeMFA
NULL


## R-Code B.1 ----
#' AIC, AICc, and BIC for panel regression models
#'
#' \code{pAICBIC()} calculates AIC, AICc, and BIC for models created with \code{plm()}.
#'
#' @param
#'   mod A panel regression model created by \code{plm()}.
#'
#' @returns
#'   Upon success the function returns a named numeric vector containing
#'   AIC, AICc, and BIC.
#'
#' @seealso
#'   \code{lpAICBIC()}
#'
#' @examples
#'   pAICBIC(mod)
#'
#' @export
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
#' Optimal cutpoint for logistic regression models
#'
#' \code{optCP()} calculates the optimal cutpoint for logistic regression models using the
#'   approach proposed by Wooldridge (2010,  p. 574).
#'   The selected cutpoint is choosen in a way that \eqn{\sum y = \sum \hat y}.
#'
#' @param
#'   mod Logistic regression model created by \code{glm()}.
#' @param
#'   yvar \code{string} Name of the $y$-variable.
#' @param
#'   sucess Value for success (\code{string} or \code{integer}).
#'
#' @returns
#'   A named numeric.
#'
#' @examples
#'   \code{optCP(mod, "name", 1)}
#'   \code{optCP(mod, "name", "yes")}
#'
#' @references
#'   Wooldrige, J. M., 2010, \emph{Econometric analysis of cross section and panel data},
#'   2nd ed.,  Cambridge: MIT Press
#'
#' @export
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
#' Performs a Wu-Hausmann-Test
#'
#' \code{whtest()} applies a Wu-Hausmann-Test (Wu, 1973; Hausman, 1978) for various model types.
#'   \emph{Note:} Not implemented for \code{bife}- and \code{glmer}-models.
#'   Under the null both models are consistent, under the alternative one of the models
#'   is inconsistent.
#'   In this case, a message is displayed which model is inconsistent.
#'
#' @param
#'   mod1 A regression model. The function was tested with \code{clogit()}, \code{pglm()},
#'   \code{plm()}, and \code{iv_robust()}.
#'
#' @param
#'   mod2 Another model of the classes given above.
#'
#' @returns
#'   None. A message for the alternative and the values for
#'   \eqn{\chi^2}, degrees of freedom applied, and the p-value are printed.
#'
#' @examples
#'   whtest(mod1, mod2)
#'
#' @references
#'   Wu, D.-M. (1973) Alternative tests of independence between stochastic regressors and
#'   disturbances, \emph{Econometrica}, \bold{41}, 733–-750, DOI 10.2307/1914093.
#'
#'   Hausman, J. A. (1978) Specification tests in econometrics, \emph{Econometrica}, \bold{46},
#'   1251-–1271.
#'
#' @export
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
#' AIC and BIC for logistic panel regression models
#'
#' \code{lpAICBIC()} calculates AIC and eventually BIC for models created with
#' \code{bife()}, \code{clogit()}, \code{glmer()}, or \code{pglm()}.
#'
#' @param
#'   mod A logistic panel regression model created by \code{bife()}, \code{clogit()},
#'   \code{glmer()}, or \code{pglm()}.
#'
#' @returns
#'   Upon success the function returns a named numeric vector containing
#'   AIC and BIC (if feasible).
#'
#' @seealso
#'   \code{pAICBIC()}
#'
#' @examples
#'   lpAICBIC(mod)
#'
#' @export
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
#' Abnormal returns in single company event studies
#'
#' \code{getAR()} calculates the abnormal return over a standard return calculated by a
#' market model.
#'
#' @param
#'   company An \code{xts}-object containing the company returns.
#' @param
#'   index An \code{xts}-object containing the market index returns.
#' @param
#'   start Date, formated "YYYY-MM-DD", for the start of the estimation window.
#' @param
#'   end Date, formated "YYYY-MM-DD", for the end of the estimation window.
#'
#' @returns
#'   Upon success the function returns a named \code{xts}-object containing
#'   the abnormal returns for the whole period given by \code{company} or \code{index}.
#'
#' @seealso
#'   \code{tTestAR()}, \code{CRTestAR()}
#'
#' @examples
#'   getAR(company, index, "2010-01-01", "2015-12-31")
#'
#' @export
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
#' t-Test for abnormal returns in single company event studies
#'
#' \code{tTestAR()} applies a t-test for abnormal returns.
#'
#' @param
#'   ar An \code{xts}-object containing the abnormal returns.
#' @param
#'   wsstart Date, formated "YYYY-MM-DD", indicating the start of the estimation window.
#' @param
#'   wsend Date, formated "YYYY-MM-DD", indicating the end of the estimation window.
#' @param
#'   westart Date, formated "YYYY-MM-DD", indicating the start of the event window.
#' @param
#'   weend Date, formated "YYYY-MM-DD", indicating the end of the event window.
#' @param
#'   nParam Number of parameters in the model used for estimating the normal return, e.g.,
#'   2 for the market model and other one-factor models. Defaults to 2.
#' @param
#'   flPlot Logical, indicates whether a plot of the abnormal returns including an interval
#'   of \eqn{\pm} 2 standard errors should be drawn. Defaults to FALSE.
#'
#' @returns
#'   A list object containing dates, abnormal returns, standard errors, t-values, and p-values,
#'   plus the cumulative abnormal return, standard error, t-value, and p-value thereof.
#'
#' @seealso
#'   \code{getAR()}, \code{CRTestAR()}
#'
#' @examples
#'   tTestAR(ar, "2010-01-01", "2015-12-31", "2016-01-05", "2016-01-10", 2, TRUE)
#'
#' @export
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
    ggplot2::autoplot(we) |>
      ggformula::gf_hline(yintercept = 0, color = "blue") |>
      ggformula::gf_hline(yintercept = c(-2 * se, 2 * se), color = "blue", linetype = 2)
  }

  retval <- list(t.test, CAR.t.test)
  names(retval) <- c("AR", "CAR")

  return(retval)
}


## R-Code B.7 ----
#' Corrado rank test for abnormal returns in single company event studies
#'
#' \code{CRTestAR()} applies a Corrado rank test for abnormal returns (Corrado, 1989.
#'
#' @param
#'   ar An \code{xts}-object containing the abnormal returns.
#' @param
#'   wsstart Date, formated "YYYY-MM-DD", indicating the start of the estimation window.
#' @param
#'   wsend Date, formated "YYYY-MM-DD", indicating the end of the estimation window.
#' @param
#'   westart Date, formated "YYYY-MM-DD", indicating the start of the event window.
#' @param
#'   weend Date, formated "YYYY-MM-DD", indicating the end of the event window.
#' @param
#'   nParam Number of parameters in the model used for estimating the normal return, e.g.,
#'   2 for the market model and other one-factor models. Defaults to 2.
#'
#' @returns
#'   A list object containing dates, abnormal returns, abnormal rank values,
#'   standard errors, t-values, and p-values,
#'   plus the cumulative abnormal return, cumulative abnormal rank place, standard error,
#'   t-value, and p-value thereof.
#'
#' @seealso
#'   \code{getAR()}, \code{tTestAR()}
#'
#' @examples
#'   CRTestAR(ar, "2010-01-01", "2015-12-31", "2016-01-05", "2016-01-10", 2)
#'
#' @references
#'  Corrado, C. J. (1989) A nonparametric test for abnormal security-price performance in
#'  event studies, \emph{Journal of Financial Economics}, \bold{23}, 385-–395.
#'
#' @export
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

