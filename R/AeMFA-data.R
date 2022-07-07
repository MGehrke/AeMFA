# Documentation of data sets
## Adidas ----
#' Quarterly revenues of Adidas
#'
#' A dataset containing the quarterly revenues in thousand Euros of the German company
#' Adidas from 2006 to 2018.
#'
#' @format A data frame with 52 obversations of 2 variables:
#' * \code{Datum} date, in the format `"YYYY-MM-DD"`
#' * \code{Umsatz} revenue in thousand Euros
#'
#' @source
#'   The data were taken from the investor relations web page of Adidas.
#'
"ADI"

## BankKurse ----
#' Daily prices of several European banks
#'
#' A dataset containing the daily prices from several UK and non-UK banks from
#' 2015-07-01 to 2016-06-30.
#' The prices are given in the respective trading currencies.
#'
#' @format A zoo time series with 259 observations of 65 variables:
#' * \code{BARC.L} daily price of Barclays bank
#' * \code{...} daily price of ..., the name is the respective Yahoo ticker symbol
#'
#' @source
#'   The data were retrieved from Yahoo finance via \code{get_prices_from_tickers()} from
#'   the \code{estudy2} package which uses \code{getSymbols()}
#'   from the \code{quantmod} package.
#'
"BankKurse"

## IndexKurse ----
#' Daily prices of the EURO STOXX 50
#'
#' A dataset containing the daily prices of the EURO STOXX 50 from
#' 2015-07-01 to 2016-06-30.
#'
#' @format A zoo time series with 254 observations of 1 variable:
#' * \code{^STOXX50E} daily price in Euro of the EURO STOXX 50
#'
#' @source
#'   The data were retrieved from Yahoo finance via \code{get_prices_from_tickers()} from
#'   the \code{estudy2} package which uses \code{getSymbols()}
#'   from the \code{quantmod} package.
#'
"IndexKurse"

## BEI_1718 ----
#' Daily prices of Beiersdorf
#'
#' A dataset containing the daily (adjusted closing) prices of the German company Beiersdorf from
#' 2016-17-01 to 2017-12-31.
#'
#' @format A xts time series with 507 observations of 1 variable:
#' * \code{BEI.DE.Adjusted} daily price in Euro of Beiersdorf
#'
#' @source
#'   The data were retrieved from Yahoo finance via \code{getSymbols()}
#'   from the \code{quantmod} package.
#'
"BEI_1718"

## DAX_1718 ----
#' Daily prices of the DAX 30
#'
#' A dataset containing the daily (adjusted closing) prices of the German DAX 30 index from
#' 2016-17-01 to 2017-12-31.
#'
#' @format A xts time series with 507 observations of 1 variable:
#' * \code{GDAXI.Adjusted} daily price in Euro of the DAX 30
#'
#' @source
#'   The data were retrieved from Yahoo finance via \code{getSymbols()}
#'   from the \code{quantmod} package.
#'
"DAX_1718"

## DAX ----
#' Daily prices and returns of the DAX 30
#'
#' A dataset containing the daily (adjusted closing) prices and returns of the German DAX 30 index
#'  from 2010-01-01 to 2018-12-31.
#'
#' @format A xts time series with 2273 observations of 2 variables:
#' * \code{Kurs} daily price in Euro
#' * `Rendite` daily return
#'
#' @source
#'   The data were retrieved from Yahoo finance via \code{getSymbols()}
#'   from the \code{quantmod} package.
#'
"DAX"

## SAP ----
#' Daily prices and returns of SAP
#'
#' A dataset containing the daily (adjusted closing) prices and returns of the German company SAP
#'  from 2010-01-01 to 2018-12-31.
#'
#' @format A xts time series with 2284 observations of 2 variables:
#' * `Kurs` daily price in Euro
#' * `Rendite` daily return
#'
#' @source
#'   The data were retrieved from Yahoo finance via \code{getSymbols()}
#'   from the \code{quantmod} package.
#'
"SAP"

## CG ----
#' Various data in relation to corporate governacne
#'
#' A dataset containing yearly data from 2012 to 2017 about market value,
#' external and internal corporate governance indicators, and several control
#' variables of 427 companies listed in the EURO STOXX 600 .
#'
#' @format A data frame with 2379 observations of 12 variables:
#' * `company` name of the company
#' * `year` year
#' * `tq` Tobin's Q
#' * `ext` external corporate governance quality
#' * `int` internal corporate governance quality
#' * `lnSize` log of total capital
#' * `debt` quotient between outside and total capital
#' * `roa` return on assets, quotient between gross earnings and total capital
#' * `capex` investments, quotient between investments and total capital
#' * `cash` quotient between cash plus short term assets and total capital
#' * `lnResvol` log of residual volatility
#' * `intangibles` quotient between intangible assets and total capital
#'
#' @source
#'   The data were retrieved from Refinitv Eikon.
#'
"CG"

## CSdf ----
#' Credit spreads from several countries
#'
#' A dataset containing quarterly data from 2012-03-30 to 2015-12-31 including credit spreads
#' of covered bonds and various determinants thereof.
#'
#' @format A data frame with 48 observations of 6 variables:
#' * `land` country
#' * `datum` date, formatted as "YYYY-MM-DD"
#' * `zspread` z-spread of the covered bonds in base points
#' * `tickets` daily trading tickets
#' * `restlz` remaining term in days
#' * `nomzins` nominal interest rate
#'
#' @source
#'   The data were retrieved from Bloomberg.
#'
"CSdf"

## DIV ----
#' Various determinants in regard to distribution of dividends
#'
#' A dataset containing yearly data from 2006 to 2010 about market value,
#' external and internal corporate governance indicators, and several control
#' variables of 427 companies listed in the EURO STOXX 600.
#'
#' @format A data frame with 1281 observations of 20 variables:
#' * `Name` name of the company
#' * `Jahr` year
#' * `Sektor` industrial sector classification according to CIGS
#'      (Consumer, Healthcare, Industrials, IT, Sonstige)
#' * `Payout` distribution of dividend (0 -- no, 1 -- yes)
#' * `PayoutVJ` distribution of dividend in the previous year (0 -- no, 1 -- yes)
#' * `Quote` quotient between dividends and revenues
#' * `ROA` return on assets
#' * `FCFUE` quotient between free cashflow and revenues
#' * `lnUEWachs` log of revenue growth
#' * `lnInv` log of quotient between net investments and total capital
#' * `lnFKQ` log of levereage ratio
#' * `lnCash` log of quotient between cash and total capital
#' * `lnGroesse` log of company size proxy
#' * `Float` proportion of free float
#' * `ARP` share buyback (0 -- no, 1 -- yes)
#' * `lnKBV` log of price to book ratio
#' * `Abgeltung` 1 for the years 2009 and 2010 due to change in taxation, else 0
#'
#' @source
#'   The data were retrieved from Bloomberg.
#'
"DIV"

## KRED ----
#' Various determinants in regard to credit default
#'
#' A dataset containing yearly data from 2006 to 2010 about market value,
#' external and internal corporate governance indicators, and several control
#' variables of 427 companies listed in the EURO STOXX 600 .
#'
#' @format A data frame with 1281 observations of 20 variables:
#' * `ausfall` credit default (no, yes)
#' * `betrag` credit amount in USD
#' * `zins` interest rate in percent
#' * `bonitaet` credit worthiness (A -- highest, G -- lowest)
#' * `alter` age of the borrower in years
#' * `wohnen` type of living (Eigentum, Grundschuld, Miete, Sonstige)
#' * `arbeit` employment term in years
#' * `einkommen` yearly income in 1000 USD
#'
#' @source
#'   The data are originally provided by Lending Club and modified by DataCamp.
#'
"KRED"

## KS ----
#' Various determinants for capital structure in companies
#'
#' A dataset containing yearly data from 2005 to 2015 about capital structure and
#' determinants thereof of 125 German companies.
#'
#' @format A data frame with 1375 observations of 19 variables:
#' * `Name` name of the company
#' * `Sektor` industrial sector
#' * `Jahr` year
#' * `Index` index, where the company is listed (DAX, MDAX, SDAX, TecDAX)
#' * `fkq` leverage ratio
#' * `lfk` long term debt (as proportion of total capital)
#' * `kfk` short term debt (as proportion of total capital)
#' * `uw` yearly growth of revenues
#' * `lnGroesse` log of revenues
#' * `sach` tangible assets (as proportion of total capital)
#' * `ebit` earning before interest and taxes (as proportion of total capital)
#' * `ntds` non debt tax shield
#' * `Steuer` taxes (as proportion of ebit)
#' * `beta` beta factor
#' * `BIP` yearly growth of GDP
#' * `ZS` interest spread between 3-month money market rate and 10-year federal bonds
#' * `INFL` inflation (yearly change of consumer price index)
#' * `MP` market performance (yearly return of DAX)
#'
#' @source
#'   The data were retrieved from Refinitiv Eikon.
#'   Macrooeconomical data were retrieved from the OECD and the German federal office
#'   for statistics.
#'
"KS"

## SP ----
#' Savings plan
#'
#' A dataset containing simulated data for a savings plan.
#'
#' @format A data frame with 100 observations of 2 variables:
#' * `Zeit` time
#' * `Ersparnis` capital saved
#'
#' @source
#'   The data were simulated.
#'
"SP"

## UR ----
#' Running yield
#'
#' A dataset containing monthly data from 1970 to 2018 about running yield and inflation rate.
#'
#' @format A data frame with 585 observations of 3 variables:
#' * `Datum` date (formatted `"YYYY-MM-DD"`)
#' * `UmlR` running yield in percent
#' * `InflR` inflation rate in percent
#'
#' @source
#'   The data were retrieved from the web pages of the Federal Bank of Germany.
#'
"UR"
