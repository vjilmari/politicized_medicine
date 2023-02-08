---
title: "Merging ESS and CHES by vote"
output: 
  html_document: 
    toc: yes
    number_sections: yes
    keep_md: yes
---



# Preparations

## Packages


```r
library(dplyr)
library(rio)
library(sjlabelled)
```

## Import ESS and CHES data with ESS party keys


```r
ESS7<-
  import("../../data/raw/ESS7e02_2.sav")

CHES_2014.vote.keys<-
  import("../../data/processed/CHES_2014.vote.keys.xlsx")
```

# Construct single vote party number variable in ESS


```r
# use vote.dat for that

vote.dat<-
  import("../../data/processed/vote.dat.xlsx")


# obtain vote variable names in ESS
vote.vars<-unique(vote.dat$vote.var)

# exclude unused vote variables

# Germany second variable
vote.vars<-vote.vars[-which(vote.vars=="prtvede2")]

# Lithuania second and third variables

vote.vars<-vote.vars[-which(vote.vars=="prtvalt2")]
vote.vars<-vote.vars[-which(vote.vars=="prtvalt3")]

# Israel entirely
vote.vars<-vote.vars[-which(vote.vars=="prtvtcil")]

vote.vars
```

```
##  [1] "prtvtbat" "prtvtcbe" "prtvtech" "prtvtdcz" "prtvede1" "prtvtcdk"
##  [7] "prtvteee" "prtvtces" "prtvtcfi" "prtvtcfr" "prtvtbgb" "prtvtehu"
## [13] "prtvtaie" "prtvalt1" "prtvtfnl" "prtvtbno" "prtvtcpl" "prtvtbpt"
## [19] "prtvtbse" "prtvtesi"
```

```r
# check if the remaining variable names are found in ESS

vote.vars %in% names(ESS7)
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [15] TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
# sum across the voting variables columns
# this works because there is only one relevant column
# for each participant; if there isn't the result is 0

ESS7$pt.nmbr<-rowSums(ESS7[,vote.vars],na.rm=T)
table(ESS7$pt.nmbr,useNA="always")
```

```
## 
##     0     1     2     3     4     5     6     7     8     9    10    11 
## 17379  4819  4501  2443  2076  1715  1405  1410  1107   690   831   400 
##    12    13    14    15    16    17    18    19    44    55  <NA> 
##   426   500   195    87    69    46    29    18    14    25     0
```

```r
# code zeros as NA

ESS7$pt.nmbr<-ifelse(ESS7$pt.nmbr==0,NA,ESS7$pt.nmbr)
table(ESS7$pt.nmbr,useNA="always")
```

```
## 
##     1     2     3     4     5     6     7     8     9    10    11    12 
##  4819  4501  2443  2076  1715  1405  1410  1107   690   831   400   426 
##    13    14    15    16    17    18    19    44    55  <NA> 
##   500   195    87    69    46    29    18    14    25 17379
```

# Combine the election coalitions in CHES ratings


```r
# first select only the variables of interest

CHES_2014.vote.keys.combined<-CHES_2014.vote.keys %>%
  filter(vote.var!="prtvede2" & 
           vote.var!="prtvalt2" & 
           vote.var!="prtvalt3" & 
           cntry!="IL") %>%
  dplyr::select(cntry,pt.nmbr,vote.var,pt.name,
                cname,party_name,party_id,
                lrgen,lrecon,galtan,
                antielite_salience,corrupt_salience)

# group duplicates (coalitions) based on ESS names and calculate averages
CHES_2014.vote.keys.combined<-CHES_2014.vote.keys.combined %>%
  group_by(cntry,pt.name) %>%
  summarise(pt.nmbr=mean(pt.nmbr),
            lrgen=mean(lrgen),
            lrecon=mean(lrecon),
            galtan=mean(galtan),
            antielite_salience=mean(antielite_salience),
            corrupt_salience=mean(corrupt_salience))
```

```
## `summarise()` has grouped output by 'cntry'. You can override using the
## `.groups` argument.
```

# Merge the files by country and party number


```r
dat<-left_join(
  x=ESS7,
  y=CHES_2014.vote.keys.combined,
  by=c("cntry","pt.nmbr")
)
```



## Test if the merge was successful


```r
for (i in 1:length(vote.vars)){

  vote.var<-vote.vars[i]
  country<-
    vote.dat[vote.dat$vote.var==vote.var,"cntry"][1]
  
  print(country)
  
  tmp.dat<-dat %>%
    filter(cntry==country)
  
  name1<-as.character(sjlabelled::as_label(tmp.dat[,vote.var]))
  name2<-as.character(tmp.dat[,"pt.name"])

  
  for (j in 1:length(unique(name1))){
    print(c(unique(name1)[j],
            name2[which(name1==unique(name1)[j])[1]]))
  }
  
}
```

```
## [1] "AT"
## [1] NA NA
## [1] "KPÖ" NA   
## [1] "ÖVP" "ÖVP"
## [1] "FPÖ" "FPÖ"
## [1] "SPÖ" "SPÖ"
## [1] "NEOS" "NEOS"
## [1] "Grüne" "Grüne"
## [1] "Other" NA     
## [1] "BZÖ" "BZÖ"
## [1] "Team Frank Stronach" "Team Frank Stronach"
## [1] "Piratenpartei Österreich" NA                        
## [1] "BE"
## [1] "Open VLD" "Open VLD"
## [1] "SP.A" "SP.A"
## [1] "Groen!" "Groen!"
## [1] NA NA
## [1] "N-VA" "N-VA"
## [1] "PVDA+" "PVDA+"
## [1] "Other" NA     
## [1] "Vlaams Belang" "Vlaams Belang"
## [1] "CD&V" "CD&V"
## [1] "Blanco" NA      
## [1] "MR" "MR"
## [1] "PS" "PS"
## [1] "Parti Populaire" "Parti Populaire"
## [1] "PTB" NA   
## [1] "CDH" "CDH"
## [1] "Ecolo" "Ecolo"
## [1] "Ongeldig" NA        
## [1] "Front National" NA              
## [1] "CH"
## [1] NA NA
## [1] "Socialist Party" "Socialist Party"
## [1] "Radical Liberals" "Radical Liberals"
## [1] "Swiss People's Party" "Swiss People's Party"
## [1] "Green Party" "Green Party"
## [1] "Green Liberal Party" "Green Liberal Party"
## [1] "Christian Democrats" "Christian Democrats"
## [1] "Movement of the Citizens belonging to French-speaking Switzerland"
## [2] NA                                                                 
## [1] "Blank paper" NA           
## [1] "Other party" NA           
## [1] "Swiss Labour Party" NA                  
## [1] "Bourgois-democratic Party" "Bourgois-democratic Party"
## [1] "Evangelical People's Party" "Evangelical People's Party"
## [1] "Federal Democratic Union" "Federal Democratic Union"
## [1] "Christian Social Party" "Christian Social Party"
## [1] "Ticino League" "Ticino League"
## [1] "Alternative Left" NA                
## [1] "CZ"
## [1] NA NA
## [1] "Other" NA     
## [1] "ČSSD" "ČSSD"
## [1] "ANO 2011" "ANO 2011"
## [1] "Úsvit přímé demokracie Tomia Okamury"
## [2] "Úsvit přímé demokracie Tomia Okamury"
## [1] "ODS" "ODS"
## [1] "TOP 09" "TOP 09"
## [1] "KSČM" "KSČM"
## [1] "KDU-ČSL" "KDU-ČSL"
## [1] "DE"
## [1] "SPD" "SPD"
## [1] "CDU/CSU" "CDU/CSU"
## [1] "Die Linke" "Die Linke"
## [1] NA NA
## [1] "FDP" "FDP"
## [1] "Bündnis 90/ Die Grünen" "Bündnis 90/ Die Grünen"
## [1] "AfD" "AfD"
## [1] "Andere Partei" NA             
## [1] "Piratenpartei" "Piratenpartei"
## [1] "NPD" "NPD"
## [1] "DK"
## [1] NA NA
## [1] "Socialdemokraterne - the Danish social democrats"
## [2] "Socialdemokraterne - the Danish social democrats"
## [1] "Venstre, Danmarks Liberale Parti - Venstre"
## [2] "Venstre, Danmarks Liberale Parti - Venstre"
## [1] "Liberal Alliance - Liberal Alliance"
## [2] "Liberal Alliance - Liberal Alliance"
## [1] "Det Radikale Venstre - Danish Social-Liberal Party"
## [2] "Det Radikale Venstre - Danish Social-Liberal Party"
## [1] "SF Socialistisk Folkeparti - Socialist People's Party"
## [2] "SF Socialistisk Folkeparti - Socialist People's Party"
## [1] "Enhedslisten - Unity List - The Red-Green Alliance"
## [2] "Enhedslisten - Unity List - The Red-Green Alliance"
## [1] "Andet - other" NA             
## [1] "Dansk Folkeparti - Danish peoples party"
## [2] "Dansk Folkeparti - Danish peoples party"
## [1] "Det Konservative Folkeparti - Conservative"
## [2] "Det Konservative Folkeparti - Conservative"
## [1] "Kristendemokraterne - Christian democrats"
## [2] NA                                         
## [1] "EE"
## [1] "Eesti Keskerakond" "Eesti Keskerakond"
## [1] NA NA
## [1] "Eesti Reformierakond" "Eesti Reformierakond"
## [1] "Sotsiaaldemokraatlik Erakond" "Sotsiaaldemokraatlik Erakond"
## [1] "Erakond Isamaa ja Res Publica Liit" "Erakond Isamaa ja Res Publica Liit"
## [1] "Konservatiivne Rahvaerakond (endie Rahvaliit)"
## [2] NA                                             
## [1] "Üksikkandidaadid või muud" NA                         
## [1] "Erakond Eesti Kristlikud Demokraadid"
## [2] NA                                    
## [1] "Erakond Eestimaa Rohelised" "Erakond Eestimaa Rohelised"
## [1] "Eesti Iseseisvuspartei" NA                      
## [1] "Vene Erakond Eestis" NA                   
## [1] "ES"
## [1] "AMAIUR" "AMAIUR"
## [1] "Partido Nacionalista Vasco (PNV)" "Partido Nacionalista Vasco (PNV)"
## [1] NA NA
## [1] "Partido Popular - PP (con UPN en Navarra)"
## [2] "Partido Popular - PP (con UPN en Navarra)"
## [1] "Partido Socialista Obrero Español (PSOE)"
## [2] "Partido Socialista Obrero Español (PSOE)"
## [1] "Unión, Progreso y Democracia (UPyD)"
## [2] "Unión, Progreso y Democracia (UPyD)"
## [1] "Izquierda Unida (IU) - (ICV en Cataluña)"
## [2] "Izquierda Unida (IU) - (ICV en Cataluña)"
## [1] "Esquerra Republicana de Catalunya (ERC)"
## [2] "Esquerra Republicana de Catalunya (ERC)"
## [1] "Votó en blanco" NA              
## [1] "Otros" NA     
## [1] "Convergència i Unió (CiU)" "Convergència i Unió (CiU)"
## [1] "Votó nulo" NA         
## [1] "Bloque Nacionalista Galego (BNG)" "Bloque Nacionalista Galego (BNG)"
## [1] "Compromís - EQUO" NA                
## [1] "Geroa Bai" NA         
## [1] "Foro de Ciudadanos" NA                  
## [1] "Coalición Canaria - Nueva Canarias" "Coalición Canaria - Nueva Canarias"
## [1] "FI"
## [1] "The Centre Party" "The Centre Party"
## [1] NA NA
## [1] "Christian Democrats" "Christian Democrats"
## [1] "Social Democratic Party" "Social Democratic Party"
## [1] "The National Coalition Party" "The National Coalition Party"
## [1] "True Finns" "True Finns"
## [1] "Green League" "Green League"
## [1] "Left Alliance" "Left Alliance"
## [1] "The Swedish People´s Party (SPP)" "The Swedish People´s Party (SPP)"
## [1] "Communist Party" NA               
## [1] "Change 2011" NA           
## [1] "Other" NA     
## [1] "Pirate Party" NA            
## [1] "Independence Party" NA                  
## [1] "The Communist Workers' Party" NA                            
## [1] "FR"
## [1] NA NA
## [1] "MODEM (Mouvement Démocrate)" "MODEM (Mouvement Démocrate)"
## [1] "UMP (Union pour un Mouvement Populaire)"
## [2] "UMP (Union pour un Mouvement Populaire)"
## [1] "FN (Front National)" "FN (Front National)"
## [1] "PS (Parti Socialiste)" "PS (Parti Socialiste)"
## [1] "EELV (Europe Ecologie Les Verts)" "EELV (Europe Ecologie Les Verts)"
## [1] "FDG (Front de Gauche)" NA                     
## [1] "Nouveau Centre" "Nouveau Centre"
## [1] "Autre" NA     
## [1] "Parti Radical de Gauche" "Parti Radical de Gauche"
## [1] "MPF (Mouvement pour la France)" "MPF (Mouvement pour la France)"
## [1] "Autres mouvements écologistes" NA                             
## [1] "Blanc" NA     
## [1] "NPA (Nouveau Parti Anti-Capitaliste)"
## [2] NA                                    
## [1] "LO (Lutte Ouvrière)" NA                   
## [1] "Nul" NA   
## [1] "PR (Parti Radical Valoisien)" "PR (Parti Radical Valoisien)"
## [1] "GB"
## [1] "Liberal Democrat" "Liberal Democrat"
## [1] "Labour" "Labour"
## [1] NA NA
## [1] "Conservative" "Conservative"
## [1] "UK Independence Party" "UK Independence Party"
## [1] "Scottish National Party" "Scottish National Party"
## [1] "Green Party" "Green Party"
## [1] "Social Democratic and Labour Party (nir)"
## [2] NA                                        
## [1] "Plaid Cymru" "Plaid Cymru"
## [1] "Alliance Party (nir)" NA                    
## [1] "Other" NA     
## [1] "Ulster Unionist Party (nir)" NA                           
## [1] "Independent(s) (nir)" NA                    
## [1] "Other (nir)" NA           
## [1] "Sinn Fein (nir)" NA               
## [1] "Democratic Unionist Party (nir)" NA                               
## [1] "HU"
## [1] NA NA
## [1] "Fidesz - KDNP (Fidesz – Magyar Polgári Szövetség Keresztényd)"
## [2] "Fidesz - KDNP (Fidesz – Magyar Polgári Szövetség Keresztényd)"
## [1] "LMP (Lehet Más A Politika)" "LMP (Lehet Más A Politika)"
## [1] "Jobbik (Jobbik Magyarországért Mozgalom)"
## [2] "Jobbik (Jobbik Magyarországért Mozgalom)"
## [1] "MSZP-Együtt-DK-PM-MLP (Kormányváltók)"
## [2] "MSZP-Együtt-DK-PM-MLP (Kormányváltók)"
## [1] "Munkáspárt (Magyar Kommunista Munkáspárt)"
## [2] NA                                         
## [1] "Other" NA     
## [1] "IE"
## [1] "Labour" "Labour"
## [1] "Fianna Fáil" "Fianna Fáil"
## [1] NA NA
## [1] "Sinn Féin" "Sinn Féin"
## [1] "People Before Profit" "People Before Profit"
## [1] "Independent" NA           
## [1] "Fine Gael" "Fine Gael"
## [1] "Green Party" "Green Party"
## [1] "Socialist Party" "Socialist Party"
## [1] "Other" NA     
## [1] "United Left Alliance" NA                    
## [1] "LT"
## [1] "Lithuanian Social Democratic Party (LSDP)"
## [2] "Lithuanian Social Democratic Party (LSDP)"
## [1] "Party Order and Justice (TT)" "Party Order and Justice (TT)"
## [1] NA NA
## [1] "Lithuanian Peasant and Greens Union (LVZS)"
## [2] "Lithuanian Peasant and Greens Union (LVZS)"
## [1] "Homeland Union - Lithuanian Christian Democrats (TS-LKD)"
## [2] "Homeland Union - Lithuanian Christian Democrats (TS-LKD)"
## [1] "Labour Party (DP)" "Labour Party (DP)"
## [1] "Liberals' Movement of the Republic of Lithuania (LRLS)"
## [2] "Liberals' Movement of the Republic of Lithuania (LRLS)"
## [1] "Electoral Action of Poles in Lithuania (LLRA)"
## [2] "Electoral Action of Poles in Lithuania (LLRA)"
## [1] "Did not vote for a candidate list" NA                                 
## [1] "Democratic Labour and Unity Party (DDVP)"
## [2] NA                                        
## [1] "Does not know if voted for a candidate list"
## [2] NA                                           
## [1] "Liberal and Centre Union (LiCS)" NA                               
## [1] "Socialist People's Front (SLF)" NA                              
## [1] "Political Party 'The Way of Courage' (DK)"
## [2] "Political Party 'The Way of Courage' (DK)"
## [1] "Party 'Young Lithuania' (JL)" NA                            
## [1] "National Association 'For Lithuania in Lithuania' (ULL)"
## [2] NA                                                       
## [1] "Lithuanian People's Party (LZP)" NA                               
## [1] "Christian Party (KP)" NA                    
## [1] "Political Party 'Union Yes' (ST)" NA                                
## [1] "Republican Party (RP)" NA                     
## [1] "NL"
## [1] NA NA
## [1] "People's Party for Freedom and Democracy"
## [2] "People's Party for Freedom and Democracy"
## [1] "Democrats 66" "Democrats 66"
## [1] "Green Left" "Green Left"
## [1] "50PLUS" "50PLUS"
## [1] "Labour Party" "Labour Party"
## [1] "Party for Freedom" "Party for Freedom"
## [1] "Christian Democratic Appeal" "Christian Democratic Appeal"
## [1] "Socialist Party" "Socialist Party"
## [1] "Party for the Animals" "Party for the Animals"
## [1] "Reformed Political Party" "Reformed Political Party"
## [1] "Christian Union" "Christian Union"
## [1] "Other" NA     
## [1] "Blanc" NA     
## [1] "NO"
## [1] "Progress Party (FRP)" "Progress Party (FRP)"
## [1] "Labour Party (A)" "Labour Party (A)"
## [1] "Christian Democratic Party (KRF)" "Christian Democratic Party (KRF)"
## [1] "Conservative Party (H)" "Conservative Party (H)"
## [1] NA NA
## [1] "Green Party (MDG)" "Green Party (MDG)"
## [1] "Socialist Left Party (SV)" "Socialist Left Party (SV)"
## [1] "Centre Party (SP)" "Centre Party (SP)"
## [1] "Other" NA     
## [1] "Liberal Party (V)" "Liberal Party (V)"
## [1] "The Party Red (RØDT)" NA                    
## [1] "Coastal Party (KYST)" NA                    
## [1] "PL"
## [1] NA NA
## [1] "Civic Platform" "Civic Platform"
## [1] "Law and Justice" "Law and Justice"
## [1] "Polish Peasants Party" "Polish Peasants Party"
## [1] "Congress of the New Right" "Congress of the New Right"
## [1] "Democratic Left Alliance" "Democratic Left Alliance"
## [1] "Palikot Movement" "Palikot Movement"
## [1] "Other" NA     
## [1] "Poland Comes First" NA                  
## [1] "Polish Labour Party-August '80" NA                              
## [1] "PT"
## [1] "Outro" NA     
## [1] NA NA
## [1] "Centro Democrático Social - Partido Popular (CDS-PP)"
## [2] "Centro Democrático Social - Partido Popular (CDS-PP)"
## [1] "Partido Socialista (PS)" "Partido Socialista (PS)"
## [1] "Votou em branco / nulo" NA                      
## [1] "Partido Social Democrata (PSD)" "Partido Social Democrata (PSD)"
## [1] "Bloco de Esquerda (BE)" "Bloco de Esquerda (BE)"
## [1] "Coligação Democrática Unitária (CDU)"
## [2] "Coligação Democrática Unitária (CDU)"
## [1] "Partido Comunista dos Trabalhadores Portugueses/Movimento Reorganizativo do Partido do Proletariado (PCTP-MRPP)"
## [2] NA                                                                                                               
## [1] "Nova democracia (PND)" NA                     
## [1] "Partido Nacional Renovador (PNR)" NA                                
## [1] "SE"
## [1] "Moderata samlingspartiet" "Moderata samlingspartiet"
## [1] NA NA
## [1] "Kristdemokraterna" "Kristdemokraterna"
## [1] "Annat parti" NA           
## [1] "Socialdemokraterna" "Socialdemokraterna"
## [1] "Folkpartiet liberalerna" "Folkpartiet liberalerna"
## [1] "Vänsterpartiet" "Vänsterpartiet"
## [1] "Miljöpartiet de gröna" "Miljöpartiet de gröna"
## [1] "Centern" "Centern"
## [1] "Sverigedemokraterna" "Sverigedemokraterna"
## [1] "FI (Feministiskt initiativ)" "FI (Feministiskt initiativ)"
## [1] "Piratpartiet" "Piratpartiet"
## [1] "SI"
## [1] NA NA
## [1] "NSI - Nova Slovenija – Kršcanski demokrati"
## [2] "NSI - Nova Slovenija – Kršcanski demokrati"
## [1] "SDS - Slovenska demokratska stranka"
## [2] "SDS - Slovenska demokratska stranka"
## [1] "SLS  - Slovenska ljudska stranka" "SLS  - Slovenska ljudska stranka"
## [1] "SMC - Stranka Mira Cerarja" "SMC - Stranka Mira Cerarja"
## [1] "ZL - Združena levica (DSD, IDS in Stranka TRS)"
## [2] "ZL - Združena levica (DSD, IDS in Stranka TRS)"
## [1] "DESUS - Demokraticna stranka upokojencev Slovenije"
## [2] "DESUS - Demokraticna stranka upokojencev Slovenije"
## [1] "SD - Socialni demokrati" "SD - Socialni demokrati"
## [1] "ZAAB - Zavezništvo Alenke Bratušek" "ZAAB - Zavezništvo Alenke Bratušek"
## [1] "Other party" NA           
## [1] "PS - Pozitivna Slovenija" "PS - Pozitivna Slovenija"
## [1] "VERJAMEM - Stranka Igorja Šoltesa" NA                                 
## [1] "DL - Državljanska lista" NA
```

Looks good!

# Export the combined party data and the entire long format dataset


```r
export(CHES_2014.vote.keys.combined,
       "../../data/processed/CHES_2014.vote.keys.combined.xlsx",
       overwrite=T)

export(dat,"../../data/processed/dat.xlsx",
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
## [25] fastmap_1.1.0     withr_2.5.0       stringr_1.5.0     generics_0.1.3   
## [29] vctrs_0.5.1       sass_0.4.1        hms_1.1.1         tidyselect_1.2.0 
## [33] glue_1.6.2        data.table_1.14.2 R6_2.5.1          fansi_1.0.3      
## [37] readxl_1.4.0      foreign_0.8-83    tzdb_0.3.0        readr_2.1.2      
## [41] magrittr_2.0.3    htmltools_0.5.2   ellipsis_0.3.2    assertthat_0.2.1 
## [45] insight_0.18.8    utf8_1.2.2        stringi_1.7.12
```
