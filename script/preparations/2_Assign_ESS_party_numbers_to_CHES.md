---
title: "Assign ESS voted party numbers to CHES data"
output: 
  html_document: 
    toc: yes
    number_sections: yes
    keep_md: yes
---



# Preparations

## Load packages


```r
library(rio)
library(dplyr)
source("../custom_functions.R")
```

## Import CHES data and parties voted in ESS data


```r
# CHES file
CHES_2014<-
  read.csv2("../../data/raw/2014_CHES_dataset_means.csv",
            sep = ",",dec = ".")

# check structure
str(CHES_2014)
```

```
## 'data.frame':	268 obs. of  43 variables:
##  $ country               : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ cname                 : chr  "bel" "bel" "bel" "bel" ...
##  $ party_id              : int  102 103 104 105 106 107 108 109 110 111 ...
##  $ party_name            : chr  "PS" "SPA" "ECOLO" "Groen" ...
##  $ eu_position           : num  6 6 6.25 6.2 6.4 ...
##  $ eu_position_sd        : num  0.707 0.707 0.5 0.447 0.548 ...
##  $ eu_salience           : num  3.2 3.8 3.8 3.8 4.4 ...
##  $ eu_dissent            : num  3 3.25 1.67 3 1.33 ...
##  $ eu_benefit            : num  1.2 1.2 1 1 1 1 1 1 1.4 1 ...
##  $ eu_ep                 : num  6.4 6.6 6.8 6.8 6.75 ...
##  $ eu_intmark            : num  5 5 5 5 6.6 ...
##  $ eu_cohesion           : num  6.25 6.25 6.25 6.25 6 5.5 6 6 5.5 6 ...
##  $ eu_foreign            : num  6 6.2 5.4 5.4 6.6 ...
##  $ eu_turkey             : num  5.5 5.5 5.5 5.5 4.75 ...
##  $ eu_budgets            : num  4.2 4.2 3.8 3.8 6.4 ...
##  $ lrgen                 : num  2.6 3 2.2 2.2 7 ...
##  $ lrgen_sd              : num  0.548 0.707 0.447 0.837 0 ...
##  $ lrecon                : num  2.4 2.8 2.2 2 7.6 ...
##  $ lrecon_salience       : num  8.4 8.4 7.2 7.2 8.6 ...
##  $ lrecon_sd             : num  0.548 0.837 0.447 0.707 0.548 ...
##  $ galtan                : num  3.4 2.6 1.2 1.2 3 ...
##  $ galtan_salience       : num  3.2 4.25 5.6 5.6 3.2 ...
##  $ galtan_sd             : num  1.673 0.894 0.447 0.447 1 ...
##  $ spendvtax             : num  2.4 2.8 2.6 2.6 8 ...
##  $ deregulation          : num  2 2.6 1.8 2 7.6 ...
##  $ redistribution        : num  1.8 2 1.6 1.8 6.6 ...
##  $ econ_interven         : num  1.75 2.25 1.75 2.25 7.25 7.25 5 5.25 7.75 5.75 ...
##  $ civlib_laworder       : num  2.2 2.4 1.4 1.4 5.6 ...
##  $ sociallifestyle       : num  1.6 1.4 0.8 0.8 1.6 ...
##  $ religious_principle   : num  1 1 1.4 1.4 1 ...
##  $ immigrate_policy      : num  2.8 3.4 1.8 2.2 6.2 ...
##  $ multiculturalism      : num  2 2.4 1.2 1.4 6.2 ...
##  $ urban_rural           : num  3.33 2.33 2.33 2.33 4.67 ...
##  $ environment           : num  3.8 3.4 1 1 6.8 ...
##  $ regions               : num  6.6 6 6.8 6.8 7.2 ...
##  $ international_security: num  5.8 5.8 7.2 7.2 3.4 ...
##  $ ethnic_minorities     : num  2 2.6 1.4 1.4 5 ...
##  $ nationalism           : num  2.25 2.25 1 1 3 3 4.25 5.25 9 4.5 ...
##  $ antielite_salience    : num  2.6 2.6 3 3 1.6 ...
##  $ corrupt_salience      : num  2.25 3 6.25 6.25 3 3 3 3 4.25 4.25 ...
##  $ mip_one               : chr  "public services vs taxes" "public services vs taxes" "environment" "environment" ...
##  $ mip_two               : chr  "redistribution" "redistribution" "redistribution" "redistribution" ...
##  $ mip_three             : chr  "state intervention" "state intervention" "public services vs taxes" "public services vs taxes" ...
```

```r
# vote data constructed with Extract_parties_voted_in_ESS -script file
vote.dat<-
  import("../../data/processed/vote.dat.xlsx")
```


# Recode country to CHES_2014 so that it matches with ESS


```r
# countries in ESS
unique(vote.dat$cntry)
```

```
##  [1] "AT" "BE" "CH" "CZ" "DE" "DK" "EE" "ES" "FI" "FR" "GB" "HU" "IE" "IL"
## [15] "LT" "NL" "NO" "PL" "PT" "SE" "SI"
```

```r
# countries in CHES
unique(CHES_2014$cname)
```

```
##  [1] "bel"  "den"  "ger"  "gre"  "spa"  "fra"  "ire"  "it"   "net"  "uk"  
## [11] "por"  "aus"  "fin"  "swe"  "bul"  "cze"  "est"  "hun"  "lat"  "lith"
## [21] "pol"  "rom"  "slo"  "sle"  "cro"  "tur"  "nor"  "swi"  "mal"  "lux" 
## [31] "cyp"
```

```r
# make a new variable (cntry) to CHES data file with ESS country names
CHES_2014$cntry<-
  case_when(CHES_2014$cname=="bel"~"BE",
            CHES_2014$cname=="den"~"DK",
            CHES_2014$cname=="ger"~"DE",
            CHES_2014$cname=="spa"~"ES",
            CHES_2014$cname=="fra"~"FR",
            CHES_2014$cname=="ire"~"IE",
            CHES_2014$cname=="net"~"NL",
            CHES_2014$cname=="uk"~"GB",
            CHES_2014$cname=="por"~"PT",
            CHES_2014$cname=="aus"~"AT",
            CHES_2014$cname=="fin"~"FI",
            CHES_2014$cname=="swe"~"SE",
            CHES_2014$cname=="cze"~"CZ",
            CHES_2014$cname=="est"~"EE",
            CHES_2014$cname=="hun"~"HU",
            CHES_2014$cname=="lith"~"LT",
            CHES_2014$cname=="pol"~"PL",
            CHES_2014$cname=="sle"~"SI",
            CHES_2014$cname=="nor"~"NO",
            CHES_2014$cname=="swi"~"CH",
            TRUE~CHES_2014$cname)

table(CHES_2014$cntry,useNA="always")
```

```
## 
##   AT   BE  bul   CH  cro  cyp   CZ   DE   DK   EE   ES   FI   FR   GB  gre 
##    7   13   11   11   11    6    9   10    9    6   14    8   13    7    9 
##   HU   IE   it  lat   LT  lux  mal   NL   NO   PL   PT  rom   SE   SI  slo 
##    6    7   13    7    8    6    2   11    8    8    6    9   10    9   10 
##  tur <NA> 
##    4    0
```

# Recode party names as they are in ESS

This is done by adding "pt.nmbr" variable to CHES_2014 data frame. The variable is a number that matches with the party number for each country in ESS.

## AT


```r
vote.dat[vote.dat$cntry=="AT",c("pt.nmbr","pt.name")]
```

```
##    pt.nmbr                  pt.name
## 1        1                      SPÖ
## 2        2                      ÖVP
## 3        3                      FPÖ
## 4        4                      BZÖ
## 5        5                    Grüne
## 6        6                      KPÖ
## 7        7                     NEOS
## 8        8 Piratenpartei Österreich
## 9        9      Team Frank Stronach
## 10      10                    Other
## 11      11           Not applicable
## 12      12                  Refusal
## 13      13               Don't know
## 14      14                No answer
```

```r
CHES_2014[CHES_2014$cntry=="AT","party_name"]
```

```
## [1] "SPO"          "OVP"          "FPO"          "GRUNE"       
## [5] "NEOS"         "BZO"          "TeamStronach"
```

```r
CHES_2014$pt.nmbr.AT<-case_when(
  CHES_2014$cntry=="AT" & CHES_2014$party_name=="SPO"~1,
  CHES_2014$cntry=="AT" & CHES_2014$party_name=="OVP"~2,
  CHES_2014$cntry=="AT" & CHES_2014$party_name=="FPO"~3,
  CHES_2014$cntry=="AT" & CHES_2014$party_name=="GRUNE"~5,
  CHES_2014$cntry=="AT" & CHES_2014$party_name=="NEOS"~7,
  CHES_2014$cntry=="AT" & CHES_2014$party_name=="BZO"~4,
  CHES_2014$cntry=="AT" & CHES_2014$party_name=="TeamStronach"~9
)

table(CHES_2014$pt.nmbr.AT)
```

```
## 
## 1 2 3 4 5 7 9 
## 1 1 1 1 1 1 1
```

## BE


```r
vote.dat[vote.dat$cntry=="BE",c("pt.nmbr","pt.name")]
```

```
##    pt.nmbr         pt.name
## 15       1          Groen!
## 16       2            CD&V
## 17       3            N-VA
## 18       4  Lijst Dedecker
## 19       5            SP.A
## 20       6           PVDA+
## 21       7   Vlaams Belang
## 22       8        Open VLD
## 23       9             CDH
## 24      10           Ecolo
## 25      11  Front National
## 26      12              MR
## 27      13              PS
## 28      14             PTB
## 29      15 Parti Populaire
## 30      16           Other
## 31      17          Blanco
## 32      18        Ongeldig
## 33      19  Not applicable
## 34      20         Refusal
## 35      21      Don't know
## 36      22       No answer
```

```r
CHES_2014[CHES_2014$cntry=="BE","party_name"]
```

```
##  [1] "PS"    "SPA"   "ECOLO" "Groen" "MR"    "VLD"   "cdH"   "CD&V"  "N-VA" 
## [10] "FDF"   "VB"    "PVDA"  "PP"
```

```r
CHES_2014$pt.nmbr.BE<-case_when(
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="PS"~13,
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="SPA"~5,
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="ECOLO"~10,
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="Groen"~1,
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="MR"~12,
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="VLD"~8,
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="cdH"~9,
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="CD&V"~2,
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="N-VA"~3,
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="FDF"~NA_real_, #Francophone Democratic Federalists
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="VB"~7,
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="PVDA"~6,
  CHES_2014$cntry=="BE" & CHES_2014$party_name=="PP"~15
)

table(CHES_2014$pt.nmbr.BE)
```

```
## 
##  1  2  3  5  6  7  8  9 10 12 13 15 
##  1  1  1  1  1  1  1  1  1  1  1  1
```


## CH


```r
vote.dat[vote.dat$cntry=="CH",c("pt.nmbr","pt.name")]
```

```
##    pt.nmbr                                                           pt.name
## 37       1                                              Swiss People's Party
## 38       2                                                   Socialist Party
## 39       3                                                  Radical Liberals
## 40       4                                               Christian Democrats
## 41       5                                                       Green Party
## 42       6                                               Green Liberal Party
## 43       7                                         Bourgois-democratic Party
## 44       8                                        Evangelical People's Party
## 45       9                                          Federal Democratic Union
## 46      10                                                     Ticino League
## 47      11                                                Swiss Labour Party
## 48      12 Movement of the Citizens belonging to French-speaking Switzerland
## 49      13                                            Christian Social Party
## 50      14                                                  Alternative Left
## 51      15                                           Political women's group
## 52      16                                          Pirate Party Switzerland
## 53      17                                                              Left
## 54      18                                                       Other party
## 55      19                                                       Blank paper
## 56      20                                                        Mixed vote
## 57      21                                                    Not applicable
## 58      22                                                           Refusal
## 59      23                                                        Don't know
## 60      24                                                         No answer
```

```r
CHES_2014[CHES_2014$cntry=="CH","party_name"]
```

```
##  [1] "SVP/UDC" "SP/PS"   "FDP/PLR" "CVP/PVC" "GPS/PES" "GLP/PVL" "EVP/PEV"
##  [8] "EDU/UDF" "LdT"     "CSP/PCS" "BDP"
```

```r
CHES_2014$pt.nmbr.CH<-case_when(
  CHES_2014$cntry=="CH" & CHES_2014$party_name=="SVP/UDC"~1,#Swiss People’s Party
  CHES_2014$cntry=="CH" & CHES_2014$party_name=="SP/PS"~2,#Social Democratic Party of Switzerland
  CHES_2014$cntry=="CH" & CHES_2014$party_name=="FDP/PLR"~3,#FDP. The Liberals
  CHES_2014$cntry=="CH" & CHES_2014$party_name=="CVP/PVC"~4,#Christian Democratic People’s Party
  CHES_2014$cntry=="CH" & CHES_2014$party_name=="GPS/PES"~5,#Green Party
  CHES_2014$cntry=="CH" & CHES_2014$party_name=="GLP/PVL"~6,#Green Liberal Party
  CHES_2014$cntry=="CH" & CHES_2014$party_name=="EVP/PEV"~8,#Evangelical People’s Party
  CHES_2014$cntry=="CH" & CHES_2014$party_name=="EDU/UDF"~9,#Federal Democratic Union
  CHES_2014$cntry=="CH" & CHES_2014$party_name=="LdT"~10,#Ticino League
  CHES_2014$cntry=="CH" & CHES_2014$party_name=="CSP/PCS"~13,#Christian Social Party
  CHES_2014$cntry=="CH" & CHES_2014$party_name=="BDP"~7#Conservative Democratic Party
)

table(CHES_2014$pt.nmbr.CH)
```

```
## 
##  1  2  3  4  5  6  7  8  9 10 13 
##  1  1  1  1  1  1  1  1  1  1  1
```

## CZ


```r
vote.dat[vote.dat$cntry=="CZ",c("pt.nmbr","pt.name")]
```

```
##    pt.nmbr                              pt.name
## 61       1                                 KSČM
## 62       2                                 ČSSD
## 63       3                               TOP 09
## 64       4                             ANO 2011
## 65       5                                  ODS
## 66       6                              KDU-ČSL
## 67       7 Úsvit přímé demokracie Tomia Okamury
## 68       8                                Other
## 69       9                       Not applicable
## 70      10                              Refusal
## 71      11                           Don't know
## 72      12                            No answer
```

```r
CHES_2014[CHES_2014$cntry=="CZ","party_name"]
```

```
## [1] "CSSD"     "ODS"      "KSCM"     "KDU-CSL"  "SZ"       "TOP09"   
## [7] "ANO2011"  "USVIT"    "SVOBODNI"
```

```r
CHES_2014$pt.nmbr.CZ<-case_when(
  CHES_2014$cntry=="CZ" & CHES_2014$party_name=="CSSD"~2,
  CHES_2014$cntry=="CZ" & CHES_2014$party_name=="ODS"~5,
  CHES_2014$cntry=="CZ" & CHES_2014$party_name=="KSCM"~1,
  CHES_2014$cntry=="CZ" & CHES_2014$party_name=="KDU-CSL"~6,
  CHES_2014$cntry=="CZ" & CHES_2014$party_name=="SZ"~NA_real_, #Green Party
  CHES_2014$cntry=="CZ" & CHES_2014$party_name=="TOP09"~3,
  CHES_2014$cntry=="CZ" & CHES_2014$party_name=="ANO2011"~4,
  CHES_2014$cntry=="CZ" & CHES_2014$party_name=="USVIT"~7,
  CHES_2014$cntry=="CZ" & CHES_2014$party_name=="SVOBODNI"~NA_real_ #Party of Free Citizens
)

table(CHES_2014$pt.nmbr.CZ)
```

```
## 
## 1 2 3 4 5 6 7 
## 1 1 1 1 1 1 1
```

## DE

Parties based on prtvede2 variable (specific party).


```r
vote.dat[vote.dat$cntry=="DE" & vote.dat$vote.var=="prtvede2",c("pt.nmbr","pt.name")]
```

```
##    pt.nmbr                pt.name
## 86       1                CDU/CSU
## 87       2                    SPD
## 88       3              Die Linke
## 89       4 Bündnis 90/ Die Grünen
## 90       5                    FDP
## 91       6                    AfD
## 92       7          Piratenpartei
## 93       8                    NPD
## 94       9          Andere Partei
## 95      10         Not applicable
## 96      11                Refusal
## 97      12             Don't know
## 98      13              No answer
```

```r
CHES_2014[CHES_2014$cntry=="DE","party_name"]
```

```
##  [1] "CDU"     "SPD"     "FDP"     "Grunen"  "Linke"   "CSU"     "NPD"    
##  [8] "AfD"     "Piraten" "DieTier"
```

```r
CHES_2014$pt.nmbr.DE<-case_when(
  CHES_2014$cntry=="DE" & CHES_2014$party_name=="CDU"~1, #Coalition is ESS2014
  CHES_2014$cntry=="DE" & CHES_2014$party_name=="SPD"~2,
  CHES_2014$cntry=="DE" & CHES_2014$party_name=="FDP"~5,
  CHES_2014$cntry=="DE" & CHES_2014$party_name=="Grunen"~4,
  CHES_2014$cntry=="DE" & CHES_2014$party_name=="Linke"~3, 
  CHES_2014$cntry=="DE" & CHES_2014$party_name=="CSU"~1,#Coalition is ESS2014
  CHES_2014$cntry=="DE" & CHES_2014$party_name=="NPD"~8,
  CHES_2014$cntry=="DE" & CHES_2014$party_name=="AfD"~6,
  CHES_2014$cntry=="DE" & CHES_2014$party_name=="Piraten"~7,
  CHES_2014$cntry=="DE" & CHES_2014$party_name=="DieTier"~NA_real_ #Human Environment Animal Protection
)

table(CHES_2014$pt.nmbr.DE)
```

```
## 
## 1 2 3 4 5 6 7 8 
## 2 1 1 1 1 1 1 1
```

## DK


```r
vote.dat[vote.dat$cntry=="DK",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                                               pt.name
## 99        1      Socialdemokraterne - the Danish social democrats
## 100       2    Det Radikale Venstre - Danish Social-Liberal Party
## 101       3            Det Konservative Folkeparti - Conservative
## 102       4 SF Socialistisk Folkeparti - Socialist People's Party
## 103       5               Dansk Folkeparti - Danish peoples party
## 104       6             Kristendemokraterne - Christian democrats
## 105       7            Venstre, Danmarks Liberale Parti - Venstre
## 106       8                   Liberal Alliance - Liberal Alliance
## 107       9    Enhedslisten - Unity List - The Red-Green Alliance
## 108      10                                         Andet - other
## 109      11                                        Not applicable
## 110      12                                               Refusal
## 111      13                                            Don't know
## 112      14                                             No answer
```

```r
CHES_2014[CHES_2014$cntry=="DK","party_name"]
```

```
## [1] "SD"    "RV"    "KF"    "SF"    "V"     "EL"    "DF"    "FolkB" "LA"
```

```r
CHES_2014$pt.nmbr.DK<-case_when(
  CHES_2014$cntry=="DK" & CHES_2014$party_name=="SD"~1, 
  CHES_2014$cntry=="DK" & CHES_2014$party_name=="RV"~2,
  CHES_2014$cntry=="DK" & CHES_2014$party_name=="KF"~3,
  CHES_2014$cntry=="DK" & CHES_2014$party_name=="SF"~4,
  CHES_2014$cntry=="DK" & CHES_2014$party_name=="V"~7, 
  CHES_2014$cntry=="DK" & CHES_2014$party_name=="EL"~9,
  CHES_2014$cntry=="DK" & CHES_2014$party_name=="DF"~5,
  CHES_2014$cntry=="DK" & CHES_2014$party_name=="FolkB"~NA_real_,#People’s Movement Against the EU
  CHES_2014$cntry=="DK" & CHES_2014$party_name=="LA"~8
)

table(CHES_2014$pt.nmbr.DK)
```

```
## 
## 1 2 3 4 5 7 8 9 
## 1 1 1 1 1 1 1 1
```

## EE


```r
vote.dat[vote.dat$cntry=="EE",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                                       pt.name
## 113       1                          Eesti Reformierakond
## 114       2                             Eesti Keskerakond
## 115       3            Erakond Isamaa ja Res Publica Liit
## 116       4                  Sotsiaaldemokraatlik Erakond
## 117       5                    Erakond Eestimaa Rohelised
## 118       6 Konservatiivne Rahvaerakond (endie Rahvaliit)
## 119       7                           Vene Erakond Eestis
## 120       8          Erakond Eesti Kristlikud Demokraadid
## 121       9                        Eesti Iseseisvuspartei
## 122      10                     Üksikkandidaadid või muud
## 123      11                                Not applicable
## 124      12                                       Refusal
## 125      13                                    Don't know
## 126      14                                     No answer
```

```r
CHES_2014[CHES_2014$cntry=="EE","party_name"]
```

```
## [1] "IRL" "EK"  "ER"  "SDE" "EER" "EVE"
```

```r
CHES_2014$pt.nmbr.EE<-case_when(
  CHES_2014$cntry=="EE" & CHES_2014$party_name=="IRL"~3, 
  CHES_2014$cntry=="EE" & CHES_2014$party_name=="EK"~2,
  CHES_2014$cntry=="EE" & CHES_2014$party_name=="ER"~1,
  CHES_2014$cntry=="EE" & CHES_2014$party_name=="SDE"~4,
  CHES_2014$cntry=="EE" & CHES_2014$party_name=="EER"~5, 
  CHES_2014$cntry=="EE" & CHES_2014$party_name=="EVE"~NA_real_ #Eesti Vabaerakond/Estonian Free Party
)

table(CHES_2014$pt.nmbr.EE)
```

```
## 
## 1 2 3 4 5 
## 1 1 1 1 1
```

## ES


```r
vote.dat[vote.dat$cntry=="ES",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                                   pt.name
## 127       1 Partido Popular - PP (con UPN en Navarra)
## 128       2  Partido Socialista Obrero Español (PSOE)
## 129       3                 Convergència i Unió (CiU)
## 130       4  Izquierda Unida (IU) - (ICV en Cataluña)
## 131       5                                    AMAIUR
## 132       6       Unión, Progreso y Democracia (UPyD)
## 133       7          Partido Nacionalista Vasco (PNV)
## 134       8   Esquerra Republicana de Catalunya (ERC)
## 135       9          Bloque Nacionalista Galego (BNG)
## 136      10        Coalición Canaria - Nueva Canarias
## 137      11                          Compromís - EQUO
## 138      12                        Foro de Ciudadanos
## 139      13                                 Geroa Bai
## 140      14                                     Otros
## 141      15                            Votó en blanco
## 142      16                                 Votó nulo
## 143      17                            Not applicable
## 144      18                                   Refusal
## 145      19                                Don't know
## 146      20                                 No answer
```

```r
CHES_2014[CHES_2014$cntry=="ES","party_name"]
```

```
##  [1] "PSOE"    "PP"      "IU"      "CiU"     "EAJ/PNV" "EA"      "ERC"    
##  [8] "BNG"     "CC"      "ICV"     "UpyD"    "Amaiur"  "Podemos" "C's"
```

```r
CHES_2014$pt.nmbr.ES<-case_when(
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="PSOE"~2, 
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="PP"~1,
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="IU"~4,
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="CiU"~3,
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="EAJ/PNV"~7,#EAJ is the basque name 
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="EA"~NA_real_,#Eusko Alkartasuna/Basque Solidarity
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="ERC"~8,
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="BNG"~9,
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="CC"~10,
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="ICV"~4,#Party with which IU coalesces in Catalonia
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="UpyD"~6,
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="Amaiur"~5,
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="Podemos"~NA_real_,#Did not exist in 2011 (election year)
  CHES_2014$cntry=="ES" & CHES_2014$party_name=="C's"~NA_real_ #Ciudadanos - Citizens - (Did not run in the 2011 general elections)
)

table(CHES_2014$pt.nmbr.ES)
```

```
## 
##  1  2  3  4  5  6  7  8  9 10 
##  1  1  1  2  1  1  1  1  1  1
```

## FI


```r
vote.dat[vote.dat$cntry=="FI",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                          pt.name
## 147       1     The National Coalition Party
## 148       2 The Swedish People´s Party (SPP)
## 149       3                 The Centre Party
## 150       4                       True Finns
## 151       5              Christian Democrats
## 152       6                    Freedom Party
## 153       7                      Change 2011
## 154       8                     Pirate Party
## 155       9           Senior Citizens' Party
## 156      10               Independence Party
## 157      11                     For the Poor
## 158      12                     Green League
## 159      13          Social Democratic Party
## 160      14                    Left Alliance
## 161      15                  Communist Party
## 162      16     The Communist Workers' Party
## 163      17                    Workers Party
## 164      18                            Other
## 165      19                   Not applicable
## 166      20                          Refusal
## 167      21                       Don't know
## 168      22                        No answer
```

```r
CHES_2014[CHES_2014$cntry=="FI","party_name"]
```

```
## [1] "SDP"     "KOK"     "KESK"    "VAS"     "PS"      "RKP/SFP" "VIHR"   
## [8] "KD"
```

```r
CHES_2014$pt.nmbr.FI<-case_when(
  CHES_2014$cntry=="FI" & CHES_2014$party_name=="SDP"~13, 
  CHES_2014$cntry=="FI" & CHES_2014$party_name=="KOK"~1,
  CHES_2014$cntry=="FI" & CHES_2014$party_name=="KESK"~3,
  CHES_2014$cntry=="FI" & CHES_2014$party_name=="VAS"~14,
  CHES_2014$cntry=="FI" & CHES_2014$party_name=="PS"~4,
  CHES_2014$cntry=="FI" & CHES_2014$party_name=="RKP/SFP"~2,
  CHES_2014$cntry=="FI" & CHES_2014$party_name=="VIHR"~12,
  CHES_2014$cntry=="FI" & CHES_2014$party_name=="KD"~5
)

table(CHES_2014$pt.nmbr.FI)
```

```
## 
##  1  2  3  4  5 12 13 14 
##  1  1  1  1  1  1  1  1
```

## FR


```r
vote.dat[vote.dat$cntry=="FR",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                                 pt.name
## 169       1                          Nouveau Centre
## 170       2                     FN (Front National)
## 171       3            PR (Parti Radical Valoisien)
## 172       4    NPA (Nouveau Parti Anti-Capitaliste)
## 173       5                     LO (Lutte Ouvrière)
## 174       6                   FDG (Front de Gauche)
## 175       7                 Parti Radical de Gauche
## 176       8          MPF (Mouvement pour la France)
## 177       9                   PS (Parti Socialiste)
## 178      10 UMP (Union pour un Mouvement Populaire)
## 179      11             MODEM (Mouvement Démocrate)
## 180      12        EELV (Europe Ecologie Les Verts)
## 181      13           Autres mouvements écologistes
## 182      14                                   Autre
## 183      15                                   Blanc
## 184      16                                     Nul
## 185      17                          Not applicable
## 186      18                                 Refusal
## 187      19                              Don't know
## 188      20                               No answer
```

```r
CHES_2014[CHES_2014$cntry=="FR","party_name"]
```

```
##  [1] "PCF"   "PS"    "PRG"   "EELV"  "UMP"   "FN"    "MPF"   "MODEM" "NC"   
## [10] "PRV"   "AC"    "PG"    "Ens"
```

```r
CHES_2014$pt.nmbr.FR<-case_when(
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="PCF"~NA_real_, #PCF (Parti Communiste Français) - French Communist Party (no voters)
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="PS"~9, #PS (Parti Socialiste)
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="PRG"~7, #Parti Radical de Gauche
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="EELV"~12,
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="UMP"~10,
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="FN"~2,
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="MPF"~8,
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="MODEM"~11,
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="NC"~1,
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="PRV"~3, #PR (Parti Radical Valoisien)
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="AC"~NA_real_,#Alliance centriste/Centrist Alliance
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="PG"~NA_real_,#PG (Parti de Gauche) - Left party (no voters)
  CHES_2014$cntry=="FR" & CHES_2014$party_name=="Ens"~NA_real_#Ensemble/Together
)

table(CHES_2014$pt.nmbr.FR)
```

```
## 
##  1  2  3  7  8  9 10 11 12 
##  1  1  1  1  1  1  1  1  1
```

## GB


```r
vote.dat[vote.dat$cntry=="GB",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                                  pt.name
## 189       1                             Conservative
## 190       2                                   Labour
## 191       3                         Liberal Democrat
## 192       4                  Scottish National Party
## 193       5                              Plaid Cymru
## 194       6                              Green Party
## 195       7                    UK Independence Party
## 196       8                                    Other
## 197       9              Ulster Unionist Party (nir)
## 198      10          Democratic Unionist Party (nir)
## 199      11                          Sinn Fein (nir)
## 200      12 Social Democratic and Labour Party (nir)
## 201      13                     Alliance Party (nir)
## 202      14         Traditional Unionist Party (nir)
## 203      15                        Green Party (nir)
## 204      16                     Independent(s) (nir)
## 205      17      People Before Profit Alliance (nir)
## 206      18                              Other (nir)
## 207      19                           Not applicable
## 208      20                                  Refusal
## 209      21                               Don't know
## 210      22                                No answer
```

```r
CHES_2014[CHES_2014$cntry=="GB","party_name"]
```

```
## [1] "CONS"   "LAB"    "LIBDEM" "SNP"    "PLAID"  "GREEN"  "UKIP"
```

```r
CHES_2014$pt.nmbr.GB<-case_when(
  CHES_2014$cntry=="GB" & CHES_2014$party_name=="CONS"~1, 
  CHES_2014$cntry=="GB" & CHES_2014$party_name=="LAB"~2,
  CHES_2014$cntry=="GB" & CHES_2014$party_name=="LIBDEM"~3,
  CHES_2014$cntry=="GB" & CHES_2014$party_name=="SNP"~4,
  CHES_2014$cntry=="GB" & CHES_2014$party_name=="PLAID"~5,
  CHES_2014$cntry=="GB" & CHES_2014$party_name=="GREEN"~6,
  CHES_2014$cntry=="GB" & CHES_2014$party_name=="UKIP"~7
)

table(CHES_2014$pt.nmbr.GB)
```

```
## 
## 1 2 3 4 5 6 7 
## 1 1 1 1 1 1 1
```


## HU


```r
vote.dat[vote.dat$cntry=="HU",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                                                       pt.name
## 211       1 Fidesz - KDNP (Fidesz – Magyar Polgári Szövetség Keresztényd)
## 212       2                      Jobbik (Jobbik Magyarországért Mozgalom)
## 213       3                                    LMP (Lehet Más A Politika)
## 214       4                         MSZP-Együtt-DK-PM-MLP (Kormányváltók)
## 215       5                     Munkáspárt (Magyar Kommunista Munkáspárt)
## 216       6                                                         Other
## 217       7                                                Not applicable
## 218       8                                                       Refusal
## 219       9                                                    Don't know
## 220      10                                                     No answer
```

```r
CHES_2014[CHES_2014$cntry=="HU","party_name"]
```

```
## [1] "MSZP"   "Fidesz" "JOBBIK" "LMP"    "E14"    "DK"
```

```r
CHES_2014$pt.nmbr.HU<-case_when(
  CHES_2014$cntry=="HU" & CHES_2014$party_name=="MSZP"~4, #coalition of MSZP-Együtt-DK-PM-MLP (Kormányváltók)
  CHES_2014$cntry=="HU" & CHES_2014$party_name=="Fidesz"~1, #coalition of Fidesz - KDNP (Fidesz – Magyar Polgári Szövetség Keresztényd)
  CHES_2014$cntry=="HU" & CHES_2014$party_name=="JOBBIK"~2,
  CHES_2014$cntry=="HU" & CHES_2014$party_name=="LMP"~3,
  CHES_2014$cntry=="HU" & CHES_2014$party_name=="E14"~NA_real_, #Együtt2014 - Together2014 (no voters)
  CHES_2014$cntry=="HU" & CHES_2014$party_name=="DK"~4 #coalition of MSZP-Együtt-DK-PM-MLP (Kormányváltók)
)

table(CHES_2014$pt.nmbr.HU)
```

```
## 
## 1 2 3 4 
## 1 1 1 2
```


## IE


```r
vote.dat[vote.dat$cntry=="IE",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr              pt.name
## 221       1          Fianna Fáil
## 222       2            Fine Gael
## 223       3          Green Party
## 224       4          Independent
## 225       5               Labour
## 226       6 People Before Profit
## 227       7            Sinn Féin
## 228       8      Socialist Party
## 229       9 United Left Alliance
## 230      10                Other
## 231      11       Not applicable
## 232      12              Refusal
## 233      13           Don't know
## 234      14            No answer
```

```r
CHES_2014[CHES_2014$cntry=="IE","party_name"]
```

```
## [1] "FF"   "FG"   "Lab"  "GP"   "SF"   "SP"   "PBPA"
```

```r
CHES_2014$pt.nmbr.IE<-case_when(
  CHES_2014$cntry=="IE" & CHES_2014$party_name=="FF"~1, 
  CHES_2014$cntry=="IE" & CHES_2014$party_name=="FG"~2,
  CHES_2014$cntry=="IE" & CHES_2014$party_name=="Lab"~5,
  CHES_2014$cntry=="IE" & CHES_2014$party_name=="GP"~3,
  CHES_2014$cntry=="IE" & CHES_2014$party_name=="SF"~7,
  CHES_2014$cntry=="IE" & CHES_2014$party_name=="SP"~8,
  CHES_2014$cntry=="IE" & CHES_2014$party_name=="PBPA"~6
)

table(CHES_2014$pt.nmbr.IE)
```

```
## 
## 1 2 3 5 6 7 8 
## 1 1 1 1 1 1 1
```

## IL (not in CHES)


## LT


```r
vote.dat[vote.dat$cntry=="LT" & vote.dat$vote.var=="prtvalt1",
         c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                                                  pt.name
## 254       1   Liberals' Movement of the Republic of Lithuania (LRLS)
## 255       2                                    Republican Party (RP)
## 256       3                                        Labour Party (DP)
## 257       4                 Democratic Labour and Unity Party (DDVP)
## 258       5 Homeland Union - Lithuanian Christian Democrats (TS-LKD)
## 259       6                Political Party 'The Way of Courage' (DK)
## 260       7            Electoral Action of Poles in Lithuania (LLRA)
## 261       8                Lithuanian Social Democratic Party (LSDP)
## 262       9                             Party Order and Justice (TT)
## 263      10  National Association 'For Lithuania in Lithuania' (ULL)
## 264      11                                     Christian Party (KP)
## 265      12                          Lithuanian People's Party (LZP)
## 266      13                           Socialist People's Front (SLF)
## 267      14               Lithuanian Peasant and Greens Union (LVZS)
## 268      15                             Party 'Young Lithuania' (JL)
## 269      16                          Liberal and Centre Union (LiCS)
## 270      17                                    Emigrants' Party (EP)
## 271      18                         Political Party 'Union Yes' (ST)
## 272      19                        Did not vote for a candidate list
## 273      20              Does not know if voted for a candidate list
## 274      21                                           Not applicable
## 275      22                                                  Refusal
## 276      23                                               Don't know
## 277      24                                                No answer
```

```r
CHES_2014[CHES_2014$cntry=="LT","party_name"]
```

```
## [1] "LSDP"   "TS-LKD" "LVZS"   "LLRA"   "TT"     "DP"     "LRLS"   "DK"
```

```r
CHES_2014$pt.nmbr.LT<-case_when(
  CHES_2014$cntry=="LT" & CHES_2014$party_name=="LSDP"~8, 
  CHES_2014$cntry=="LT" & CHES_2014$party_name=="TS-LKD"~5,
  CHES_2014$cntry=="LT" & CHES_2014$party_name=="LVZS"~14,
  CHES_2014$cntry=="LT" & CHES_2014$party_name=="LLRA"~7,
  CHES_2014$cntry=="LT" & CHES_2014$party_name=="TT"~9,
  CHES_2014$cntry=="LT" & CHES_2014$party_name=="DP"~3,
  CHES_2014$cntry=="LT" & CHES_2014$party_name=="LRLS"~1,
  CHES_2014$cntry=="LT" & CHES_2014$party_name=="DK"~6
)

table(CHES_2014$pt.nmbr.LT)
```

```
## 
##  1  3  5  6  7  8  9 14 
##  1  1  1  1  1  1  1  1
```

## NL


```r
vote.dat[vote.dat$cntry=="NL",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                                  pt.name
## 347       1 People's Party for Freedom and Democracy
## 348       2                             Labour Party
## 349       3                        Party for Freedom
## 350       4                          Socialist Party
## 351       5              Christian Democratic Appeal
## 352       6                             Democrats 66
## 353       7                          Christian Union
## 354       8                               Green Left
## 355       9                 Reformed Political Party
## 356      10                    Party for the Animals
## 357      11                                   50PLUS
## 358      12                                    Other
## 359      13                                    Blanc
## 360      14                           Not applicable
## 361      15                                  Refusal
## 362      16                               Don't know
## 363      17                                No answer
```

```r
CHES_2014[CHES_2014$cntry=="NL","party_name"]
```

```
##  [1] "CDA"    "PvdA"   "VVD"    "D66"    "GL"     "SGP"    "SP"     "CU"    
##  [9] "PVV"    "PvdD"   "50PLUS"
```

```r
CHES_2014$pt.nmbr.NL<-case_when(
  CHES_2014$cntry=="NL" & CHES_2014$party_name=="CDA"~5,#Christen-Democratisch Appel/Christian Democratic Appeal 
  CHES_2014$cntry=="NL" & CHES_2014$party_name=="PvdA"~2,#Partij van de Arbeid/Labour Party
  CHES_2014$cntry=="NL" & CHES_2014$party_name=="VVD"~1,#Volkspartij voor Vrijheid en Demokratie/People’s Party for Freedom and Democracy
  CHES_2014$cntry=="NL" & CHES_2014$party_name=="D66"~6,
  CHES_2014$cntry=="NL" & CHES_2014$party_name=="GL"~8,
  CHES_2014$cntry=="NL" & CHES_2014$party_name=="SGP"~9,#Staatkundig Gereformeerde Partij/Political Reformed Party
  CHES_2014$cntry=="NL" & CHES_2014$party_name=="SP"~4,
  CHES_2014$cntry=="NL" & CHES_2014$party_name=="CU"~7,
  CHES_2014$cntry=="NL" & CHES_2014$party_name=="PVV"~3,#Partij voor de Vrijheid/Party for Freedom
  CHES_2014$cntry=="NL" & CHES_2014$party_name=="PvdD"~10,#Partij voor de Dieren/Party for the Animals
  CHES_2014$cntry=="NL" & CHES_2014$party_name=="50PLUS"~11
)

table(CHES_2014$pt.nmbr.NL)
```

```
## 
##  1  2  3  4  5  6  7  8  9 10 11 
##  1  1  1  1  1  1  1  1  1  1  1
```

## NO


```r
vote.dat[vote.dat$cntry=="NO",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                          pt.name
## 364       1             The Party Red (RØDT)
## 365       2        Socialist Left Party (SV)
## 366       3                 Labour Party (A)
## 367       4                Liberal Party (V)
## 368       5 Christian Democratic Party (KRF)
## 369       6                Centre Party (SP)
## 370       7           Conservative Party (H)
## 371       8             Progress Party (FRP)
## 372       9             Coastal Party (KYST)
## 373      10                Green Party (MDG)
## 374      11                            Other
## 375      12                   Not applicable
## 376      13                          Refusal
## 377      14                       Don't know
## 378      15                        No answer
```

```r
CHES_2014[CHES_2014$cntry=="NO","party_name"]
```

```
## [1] "AP"  "FrP" "H"   "SV"  "Sp"  "KrF" "V"   "MDG"
```

```r
CHES_2014$pt.nmbr.NO<-case_when(
  CHES_2014$cntry=="NO" & CHES_2014$party_name=="AP"~3,#Arbeiderpartiet/Labour Party
  CHES_2014$cntry=="NO" & CHES_2014$party_name=="FrP"~8,
  CHES_2014$cntry=="NO" & CHES_2014$party_name=="H"~7,
  CHES_2014$cntry=="NO" & CHES_2014$party_name=="SV"~2,
  CHES_2014$cntry=="NO" & CHES_2014$party_name=="Sp"~6,
  CHES_2014$cntry=="NO" & CHES_2014$party_name=="KrF"~5,
  CHES_2014$cntry=="NO" & CHES_2014$party_name=="V"~4,
  CHES_2014$cntry=="NO" & CHES_2014$party_name=="MDG"~10
)

table(CHES_2014$pt.nmbr.NO)
```

```
## 
##  2  3  4  5  6  7  8 10 
##  1  1  1  1  1  1  1  1
```

## PL


```r
vote.dat[vote.dat$cntry=="PL",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                        pt.name
## 379       1      Congress of the New Right
## 380       2                 Civic Platform
## 381       3             Poland Comes First
## 382       4 Polish Labour Party-August '80
## 383       5          Polish Peasants Party
## 384       6                Law and Justice
## 385       7               Palikot Movement
## 386       8       Democratic Left Alliance
## 387       9                          Other
## 388      10                 Not applicable
## 389      11                        Refusal
## 390      12                     Don't know
## 391      13                      No answer
```

```r
CHES_2014[CHES_2014$cntry=="PL","party_name"]
```

```
## [1] "SLD" "PO"  "PiS" "PSL" "RP"  "KNP" "PR"  "SP"
```

```r
CHES_2014$pt.nmbr.PL<-case_when(
  CHES_2014$cntry=="PL" & CHES_2014$party_name=="SLD"~8,#Sojusz Lewicy Demokratycznej/Democratic Left Alliance
  CHES_2014$cntry=="PL" & CHES_2014$party_name=="PO"~2,#Platforma Obywatelska/Civic Platform
  CHES_2014$cntry=="PL" & CHES_2014$party_name=="PiS"~6,#Prawo i Sprawiedliwosc/Law and Justice Party
  CHES_2014$cntry=="PL" & CHES_2014$party_name=="PSL"~5,#Polskie Stronnictwo Ludowe/Polish People’s Party
  CHES_2014$cntry=="PL" & CHES_2014$party_name=="RP"~7,#Twój Ruch (Ruch Palikota)/Your Movement (Palikot’s Movement)
  CHES_2014$cntry=="PL" & CHES_2014$party_name=="KNP"~1,#Kongres Nowej Prawicy/Congress of the New Right
  CHES_2014$cntry=="PL" & CHES_2014$party_name=="PR"~NA_real_,#Polska Razem/Poland Together // Polska Razem Zjednoczona Prawica (PR) - Poland Together United Right
  CHES_2014$cntry=="PL" & CHES_2014$party_name=="SP"~NA_real_#Solidarna Polska/United Poland (this was split from law and justice party in 2012)
)

table(CHES_2014$pt.nmbr.PL)
```

```
## 
## 1 2 5 6 7 8 
## 1 1 1 1 1 1
```

## PT


```r
vote.dat[vote.dat$cntry=="PT",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr
## 392       1
## 393       2
## 394       3
## 395       4
## 396       5
## 397       6
## 398       7
## 399       8
## 400       9
## 401      10
## 402      11
## 403      12
## 404      13
## 405      14
## 406      15
## 407      16
## 408      17
##                                                                                                             pt.name
## 392                                                                                          Bloco de Esquerda (BE)
## 393                                                            Centro Democrático Social - Partido Popular (CDS-PP)
## 394                                                                            Coligação Democrática Unitária (CDU)
## 395 Partido Comunista dos Trabalhadores Portugueses/Movimento Reorganizativo do Partido do Proletariado (PCTP-MRPP)
## 396                                                                          Partido Democrático do Atlântico (PDA)
## 397                                                                                          Partido Humanista (PH)
## 398                                                                                           Nova democracia (PND)
## 399                                                                                Partido Nacional Renovador (PNR)
## 400                                                                   Partido Operário de Unidade Socialista (POUS)
## 401                                                                                  Partido Social Democrata (PSD)
## 402                                                                                         Partido Socialista (PS)
## 403                                                                                          Votou em branco / nulo
## 404                                                                                                           Outro
## 405                                                                                                  Not applicable
## 406                                                                                                         Refusal
## 407                                                                                                      Don't know
## 408                                                                                                       No answer
```

```r
CHES_2014[CHES_2014$cntry=="PT","party_name"]
```

```
## [1] "CDU" "PP"  "PS"  "PSD" "BE"  "MPT"
```

```r
CHES_2014$pt.nmbr.PT<-case_when(
  CHES_2014$cntry=="PT" & CHES_2014$party_name=="CDU"~3,#Coligação Democrática Unitária/Democratic Unitarian Coalition
  CHES_2014$cntry=="PT" & CHES_2014$party_name=="PP"~2,
  CHES_2014$cntry=="PT" & CHES_2014$party_name=="PS"~11,
  CHES_2014$cntry=="PT" & CHES_2014$party_name=="PSD"~10,
  CHES_2014$cntry=="PT" & CHES_2014$party_name=="BE"~1,
  CHES_2014$cntry=="PT" & CHES_2014$party_name=="MPT"~NA_real_ #Partido da Terra/Earth Party (no voters)
)

table(CHES_2014$pt.nmbr.PT)
```

```
## 
##  1  2  3 10 11 
##  1  1  1  1  1
```

## SI


```r
vote.dat[vote.dat$cntry=="SI",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                                            pt.name
## 424       1 DESUS - Demokraticna stranka upokojencev Slovenije
## 425       2                            DL - Državljanska lista
## 426       3         NSI - Nova Slovenija – Kršcanski demokrati
## 427       4                           PS - Pozitivna Slovenija
## 428       5                            SD - Socialni demokrati
## 429       6                SDS - Slovenska demokratska stranka
## 430       7                   SLS  - Slovenska ljudska stranka
## 431       8                         SMC - Stranka Mira Cerarja
## 432       9                  VERJAMEM - Stranka Igorja Šoltesa
## 433      10                 ZAAB - Zavezništvo Alenke Bratušek
## 434      11     ZL - Združena levica (DSD, IDS in Stranka TRS)
## 435      12                                        Other party
## 436      13                                     Not applicable
## 437      14                                            Refusal
## 438      15                                         Don't know
## 439      16                                          No answer
```

```r
CHES_2014[CHES_2014$cntry=="SI","party_name"]
```

```
## [1] "SDS"   "SD"    "SLS"   "NSI"   "DeSUS" "SMC"   "ZL"    "ZaAB"  "PS"
```

```r
CHES_2014$pt.nmbr.SI<-case_when(
  CHES_2014$cntry=="SI" & CHES_2014$party_name=="SDS"~6,
  CHES_2014$cntry=="SI" & CHES_2014$party_name=="SD"~5,
  CHES_2014$cntry=="SI" & CHES_2014$party_name=="SLS"~7,
  CHES_2014$cntry=="SI" & CHES_2014$party_name=="NSI"~3,
  CHES_2014$cntry=="SI" & CHES_2014$party_name=="DeSUS"~1,
  CHES_2014$cntry=="SI" & CHES_2014$party_name=="SMC"~8,
  CHES_2014$cntry=="SI" & CHES_2014$party_name=="ZL"~11,
  CHES_2014$cntry=="SI" & CHES_2014$party_name=="ZaAB"~10,
  CHES_2014$cntry=="SI" & CHES_2014$party_name=="PS"~4
)

table(CHES_2014$pt.nmbr.SI)
```

```
## 
##  1  3  4  5  6  7  8 10 11 
##  1  1  1  1  1  1  1  1  1
```

## SE


```r
vote.dat[vote.dat$cntry=="SE",c("pt.nmbr","pt.name")]
```

```
##     pt.nmbr                     pt.name
## 409       1                     Centern
## 410       2     Folkpartiet liberalerna
## 411       3           Kristdemokraterna
## 412       4       Miljöpartiet de gröna
## 413       5    Moderata samlingspartiet
## 414       6          Socialdemokraterna
## 415       7              Vänsterpartiet
## 416       8 FI (Feministiskt initiativ)
## 417       9                Piratpartiet
## 418      10         Sverigedemokraterna
## 419      11                 Annat parti
## 420      12              Not applicable
## 421      13                     Refusal
## 422      14                  Don't know
## 423      15                   No answer
```

```r
CHES_2014[CHES_2014$cntry=="SE","party_name"]
```

```
##  [1] "V"     "SAP"   "C"     "FP"    "M"     "KD"    "MP"    "SD"    "PIRAT"
## [10] "FI"
```

```r
CHES_2014$pt.nmbr.SE<-case_when(
  CHES_2014$cntry=="SE" & CHES_2014$party_name=="V"~7,
  CHES_2014$cntry=="SE" & CHES_2014$party_name=="SAP"~6,#Sveriges Socialdemokratiska Arbetareparti/Swedish Social Democratic Party
  CHES_2014$cntry=="SE" & CHES_2014$party_name=="C"~1,
  CHES_2014$cntry=="SE" & CHES_2014$party_name=="FP"~2,
  CHES_2014$cntry=="SE" & CHES_2014$party_name=="M"~5,#Moderata Samlingspartiet/Moderate Party
  CHES_2014$cntry=="SE" & CHES_2014$party_name=="KD"~3,
  CHES_2014$cntry=="SE" & CHES_2014$party_name=="MP"~4,
  CHES_2014$cntry=="SE" & CHES_2014$party_name=="SD"~10,
  CHES_2014$cntry=="SE" & CHES_2014$party_name=="PIRAT"~9,
  CHES_2014$cntry=="SE" & CHES_2014$party_name=="FI"~8
)

table(CHES_2014$pt.nmbr.SE)
```

```
## 
##  1  2  3  4  5  6  7  8  9 10 
##  1  1  1  1  1  1  1  1  1  1
```

# Obtain the ESS party names through the party number values within countries


```r
# obtain vector for countries, exclude IL
countries<-unique(vote.dat$cntry)
countries<-countries[-which(countries=="IL")]
countries
```

```
##  [1] "AT" "BE" "CH" "CZ" "DE" "DK" "EE" "ES" "FI" "FR" "GB" "HU" "IE" "LT"
## [15] "NL" "NO" "PL" "PT" "SE" "SI"
```

```r
# obtain vector for party numbers within countries
nmbr.vars<-paste0("pt.nmbr.",countries)
nmbr.vars
```

```
##  [1] "pt.nmbr.AT" "pt.nmbr.BE" "pt.nmbr.CH" "pt.nmbr.CZ" "pt.nmbr.DE"
##  [6] "pt.nmbr.DK" "pt.nmbr.EE" "pt.nmbr.ES" "pt.nmbr.FI" "pt.nmbr.FR"
## [11] "pt.nmbr.GB" "pt.nmbr.HU" "pt.nmbr.IE" "pt.nmbr.LT" "pt.nmbr.NL"
## [16] "pt.nmbr.NO" "pt.nmbr.PL" "pt.nmbr.PT" "pt.nmbr.SE" "pt.nmbr.SI"
```

```r
# check if each country-party number variables is in the CHES Data

nmbr.vars %in% names(CHES_2014)
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [15] TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
# sum across the rows to construct a single party number variable
CHES_2014$pt.nmbr<-rowSums(CHES_2014[,nmbr.vars],na.rm=T)
table(CHES_2014$pt.nmbr,useNA="always")
```

```
## 
##    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14 
##  105   20   18   19   16   17   13   16   13    9    9    4    3    3    2 
##   15 <NA> 
##    1    0
```

```r
# code zeros as NA
CHES_2014$pt.nmbr<-ifelse(CHES_2014$pt.nmbr==0,NA,CHES_2014$pt.nmbr)
table(CHES_2014$pt.nmbr,useNA="always")
```

```
## 
##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
##   20   18   19   16   17   13   16   13    9    9    4    3    3    2    1 
## <NA> 
##  105
```

```r
# Join CHES and ESS party data by country and party number
CHES_2014.vote.keys<-left_join(
  x=CHES_2014,
  y=vote.dat,
  by=c("cntry","pt.nmbr")
)


# exclude the country-specific party number variables
CHES_2014.vote.keys<-
  CHES_2014.vote.keys[,-which(names(CHES_2014.vote.keys) %in% nmbr.vars)]

# check if the variable names look like they should
names(CHES_2014.vote.keys)
```

```
##  [1] "country"                "cname"                 
##  [3] "party_id"               "party_name"            
##  [5] "eu_position"            "eu_position_sd"        
##  [7] "eu_salience"            "eu_dissent"            
##  [9] "eu_benefit"             "eu_ep"                 
## [11] "eu_intmark"             "eu_cohesion"           
## [13] "eu_foreign"             "eu_turkey"             
## [15] "eu_budgets"             "lrgen"                 
## [17] "lrgen_sd"               "lrecon"                
## [19] "lrecon_salience"        "lrecon_sd"             
## [21] "galtan"                 "galtan_salience"       
## [23] "galtan_sd"              "spendvtax"             
## [25] "deregulation"           "redistribution"        
## [27] "econ_interven"          "civlib_laworder"       
## [29] "sociallifestyle"        "religious_principle"   
## [31] "immigrate_policy"       "multiculturalism"      
## [33] "urban_rural"            "environment"           
## [35] "regions"                "international_security"
## [37] "ethnic_minorities"      "nationalism"           
## [39] "antielite_salience"     "corrupt_salience"      
## [41] "mip_one"                "mip_two"               
## [43] "mip_three"              "cntry"                 
## [45] "pt.nmbr"                "vote.var"              
## [47] "pt.name"
```

```r
# save to a file

export(CHES_2014.vote.keys,
       "../../data/processed/CHES_2014.vote.keys.xlsx",
       overwrite=T)
```

# Session information


```r
s<-sessionInfo()
print(s,locale=F)
```

```
## R version 4.2.2 (2022-10-31 ucrt)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19045)
## 
## Matrix products: default
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] dplyr_1.0.10     sjlabelled_1.2.0 rio_0.5.29       knitr_1.39      
## [5] rmarkdown_2.15  
## 
## loaded via a namespace (and not attached):
##  [1] zip_2.2.0         Rcpp_1.0.9        cellranger_1.1.0  bslib_0.3.1      
##  [5] compiler_4.2.2    pillar_1.8.1      jquerylib_0.1.4   forcats_0.5.1    
##  [9] tools_4.2.2       digest_0.6.31     jsonlite_1.8.4    evaluate_0.20    
## [13] lifecycle_1.0.3   tibble_3.1.8      pkgconfig_2.0.3   rlang_1.0.6      
## [17] openxlsx_4.2.5    DBI_1.1.2         cli_3.6.0         rstudioapi_0.13  
## [21] curl_4.3.2        yaml_2.3.5        haven_2.5.0       xfun_0.30        
## [25] fastmap_1.1.0     stringr_1.5.0     generics_0.1.3    vctrs_0.5.1      
## [29] sass_0.4.1        hms_1.1.1         tidyselect_1.2.0  glue_1.6.2       
## [33] data.table_1.14.2 R6_2.5.1          fansi_1.0.3       readxl_1.4.0     
## [37] foreign_0.8-83    tzdb_0.3.0        readr_2.1.2       magrittr_2.0.3   
## [41] htmltools_0.5.2   ellipsis_0.3.2    assertthat_0.2.1  insight_0.18.8   
## [45] utf8_1.2.2        stringi_1.7.12
```



