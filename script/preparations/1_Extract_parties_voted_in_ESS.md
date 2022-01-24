---
title: "Extracting parties that participants voted in ESS"
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
```

```
## The following rio suggested packages are not installed: 'arrow', 'feather', 'fst', 'hexView', 'pzfx', 'readODS', 'rmatio'
## Use 'install_formats()' to install them
```

```r
library(sjlabelled)
```

```
## 
## Attaching package: 'sjlabelled'
```

```
## The following object is masked from 'package:usethis':
## 
##     tidy_labels
```

```r
source("../custom_functions.R")
```

## Import ESS data


```r
dat<-import("../../data/raw/ESS7e02_2.sav")
```

# Save a vector of voting variables


```r
# use get.ESS.label custom function
cbind(sapply(dat[,27:100],get.ESS.label))
```

```
##          [,1]                                                                         
## prtvtbat "Party voted for in last national election, Austria"                         
## prtvtcbe "Party voted for in last national election, Belgium"                         
## prtvtech "Party voted for in last national election, Switzerland"                     
## prtvtdcz "Party voted for in last national election, Czechia"                         
## prtvede1 "Party voted for in last national election 1, Germany"                       
## prtvede2 "Party voted for in last national election 2, Germany"                       
## prtvtcdk "Party voted for in last national election, Denmark"                         
## prtvteee "Party voted for in last national election, Estonia"                         
## prtvtces "Party voted for in last national election, Spain"                           
## prtvtcfi "Party voted for in last national election, Finland"                         
## prtvtcfr "Party voted for in last national election, France (ballot 1)"               
## prtvtbgb "Party voted for in last national election, United Kingdom"                  
## prtvtehu "Party voted for in last national election, Hungary"                         
## prtvtaie "Party voted for in last national election, Ireland"                         
## prtvtcil "Party voted for in last national election, Israel"                          
## prtvalt1 "Party voted for in last national election 1, Lithuania (first vote, party)" 
## prtvalt2 "Party voted for in last national election 2, Lithuania (second vote, party)"
## prtvalt3 "Party voted for in last national election 3, Lithuania (third vote, party)" 
## prtvtfnl "Party voted for in last national election, Netherlands"                     
## prtvtbno "Party voted for in last national election, Norway"                          
## prtvtcpl "Party voted for in last national election, Poland"                          
## prtvtbpt "Party voted for in last national election, Portugal"                        
## prtvtbse "Party voted for in last national election, Sweden"                          
## prtvtesi "Party voted for in last national election, Slovenia"                        
## contplt  "Contacted politician or government official last 12 months"                 
## wrkprty  "Worked in political party or action group last 12 months"                   
## wrkorg   "Worked in another organisation or association last 12 months"               
## badge    "Worn or displayed campaign badge/sticker last 12 months"                    
## sgnptit  "Signed petition last 12 months"                                             
## pbldmn   "Taken part in lawful public demonstration last 12 months"                   
## bctprd   "Boycotted certain products last 12 months"                                  
## clsprty  "Feel closer to a particular party than all other parties"                   
## prtclcat "Which party feel closer to, Austria"                                        
## prtclcbe "Which party feel closer to, Belgium"                                        
## prtclech "Which party feel closer to, Switzerland"                                    
## prtcldcz "Which party feel closer to, Czechia"                                        
## prtclede "Which party feel closer to, Germany"                                        
## prtclcdk "Which party feel closer to, Denmark"                                        
## prtcleee "Which party feel closer to, Estonia"                                        
## prtcldes "Which party feel closer to, Spain"                                          
## prtclcfi "Which party feel closer to, Finland"                                        
## prtcldfr "Which party feel closer to, France"                                         
## prtclbgb "Which party feel closer to, United Kingdom"                                 
## prtclehu "Which party feel closer to, Hungary"                                        
## prtclcie "Which party feel closer to, Ireland"                                        
## prtcldil "Which party feel closer to, Israel"                                         
## prtclalt "Which party feel closer to, Lithuania"                                      
## prtclenl "Which party feel closer to, Netherlands"                                    
## prtclbno "Which party feel closer to, Norway"                                         
## prtclfpl "Which party feel closer to, Poland"                                         
## prtcldpt "Which party feel closer to, Portugal"                                       
## prtclbse "Which party feel closer to, Sweden"                                         
## prtclesi "Which party feel closer to, Slovenia"                                       
## prtdgcl  "How close to party"                                                         
## lrscale  "Placement on left right scale"                                              
## stflife  "How satisfied with life as a whole"                                         
## stfeco   "How satisfied with present state of economy in country"                     
## stfgov   "How satisfied with the national government"                                 
## stfdem   "How satisfied with the way democracy works in country"                      
## stfedu   "State of education in country nowadays"                                     
## stfhlth  "State of health services in country nowadays"                               
## gincdif  "Government should reduce differences in income levels"                      
## freehms  "Gays and lesbians free to live life as they wish"                           
## euftf    "European Union: European unification go further or gone too far"            
## imsmetn  "Allow many/few immigrants of same race/ethnic group as majority"            
## imdfetn  "Allow many/few immigrants of different race/ethnic group from majority"     
## eimpcnt  "Allow many/few immigrants from poorer countries in Europe"                  
## impcntr  "Allow many/few immigrants from poorer countries outside Europe"             
## imbgeco  "Immigration bad or good for country's economy"                              
## imueclt  "Country's cultural life undermined or enriched by immigrants"               
## imwbcnt  "Immigrants make country worse or better place to live"                      
## happy    "How happy are you"                                                          
## sclmeet  "How often socially meet with friends, relatives or colleagues"              
## inprdsc  "How many people with whom you can discuss intimate and personal matters"
```

```r
# save to vector
vote.vars<-names(dat)[which(names(dat)=="prtvtbat"):
                        which(names(dat)=="prtvtesi")]
# check if correct
cbind(sapply(dat[,vote.vars],get.ESS.label))
```

```
##          [,1]                                                                         
## prtvtbat "Party voted for in last national election, Austria"                         
## prtvtcbe "Party voted for in last national election, Belgium"                         
## prtvtech "Party voted for in last national election, Switzerland"                     
## prtvtdcz "Party voted for in last national election, Czechia"                         
## prtvede1 "Party voted for in last national election 1, Germany"                       
## prtvede2 "Party voted for in last national election 2, Germany"                       
## prtvtcdk "Party voted for in last national election, Denmark"                         
## prtvteee "Party voted for in last national election, Estonia"                         
## prtvtces "Party voted for in last national election, Spain"                           
## prtvtcfi "Party voted for in last national election, Finland"                         
## prtvtcfr "Party voted for in last national election, France (ballot 1)"               
## prtvtbgb "Party voted for in last national election, United Kingdom"                  
## prtvtehu "Party voted for in last national election, Hungary"                         
## prtvtaie "Party voted for in last national election, Ireland"                         
## prtvtcil "Party voted for in last national election, Israel"                          
## prtvalt1 "Party voted for in last national election 1, Lithuania (first vote, party)" 
## prtvalt2 "Party voted for in last national election 2, Lithuania (second vote, party)"
## prtvalt3 "Party voted for in last national election 3, Lithuania (third vote, party)" 
## prtvtfnl "Party voted for in last national election, Netherlands"                     
## prtvtbno "Party voted for in last national election, Norway"                          
## prtvtcpl "Party voted for in last national election, Poland"                          
## prtvtbpt "Party voted for in last national election, Portugal"                        
## prtvtbse "Party voted for in last national election, Sweden"                          
## prtvtesi "Party voted for in last national election, Slovenia"
```

# Save a vector of countries with vote variables


```r
table(dat$cntry)
```

```
## 
##   AT   BE   CH   CZ   DE   DK   EE   ES   FI   FR   GB   HU   IE   IL   LT 
## 1795 1769 1532 2148 3045 1502 2051 1925 2087 1917 2264 1698 2390 2562 2250 
##   NL   NO   PL   PT   SE   SI 
## 1919 1436 1615 1265 1791 1224
```

```r
vote.cntry<-c("AT","BE","CH","CZ","DE","DE","DK","EE","ES","FI","FR",
              "GB","HU","IE","IL","LT","LT","LT","NL","NO","PL","PT",
              "SE","SI")
```

# Construct a data file with countries, vote variable names, party names, and party numbers


```r
# list to save the results to
vote.list<-list()

# loop through each voting variable
for (i in 1:length(vote.vars)){
  tmp.lbls<-
    get_labels(dat[,vote.vars[i]])
  
  vote.var<-vote.vars[i]
  
  vote.list[[i]]<-cbind.data.frame(
    cntry=vote.cntry[i],
    vote.var=vote.var,
    pt.name=tmp.lbls,
    pt.nmbr=1:length(tmp.lbls))
}

# save to data frame
vote.dat<-do.call(rbind,vote.list)

# check
str(vote.dat)
```

```
## 'data.frame':	439 obs. of  4 variables:
##  $ cntry   : chr  "AT" "AT" "AT" "AT" ...
##  $ vote.var: chr  "prtvtbat" "prtvtbat" "prtvtbat" "prtvtbat" ...
##  $ pt.name : chr  "SPÖ" "ÖVP" "FPÖ" "BZÖ" ...
##  $ pt.nmbr : int  1 2 3 4 5 6 7 8 9 10 ...
```

```r
head(vote.dat)
```

```
##   cntry vote.var pt.name pt.nmbr
## 1    AT prtvtbat     SPÖ       1
## 2    AT prtvtbat     ÖVP       2
## 3    AT prtvtbat     FPÖ       3
## 4    AT prtvtbat     BZÖ       4
## 5    AT prtvtbat   Grüne       5
## 6    AT prtvtbat     KPÖ       6
```

```r
# save to a file
export(vote.dat,
       "../../data/processed/vote.dat.xlsx",
       overwrite=T)
```

# Session information


```r
s<-sessionInfo()
print(s,locale=F)
```

```
## R version 4.1.2 (2021-11-01)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19043)
## 
## Matrix products: default
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] sjlabelled_1.1.8 rio_0.5.29       rmarkdown_2.11   devtools_2.4.3  
## [5] usethis_2.0.1   
## 
## loaded via a namespace (and not attached):
##  [1] xfun_0.29         bslib_0.3.1       remotes_2.4.2    
##  [4] purrr_0.3.4       haven_2.4.3       vctrs_0.3.8      
##  [7] testthat_3.0.4    htmltools_0.5.2   yaml_2.2.1       
## [10] utf8_1.2.2        rlang_0.4.12      pkgbuild_1.3.1   
## [13] jquerylib_0.1.4   pillar_1.6.4      foreign_0.8-81   
## [16] glue_1.6.0        withr_2.4.3       sessioninfo_1.2.2
## [19] readxl_1.3.1      lifecycle_1.0.1   stringr_1.4.0    
## [22] cellranger_1.1.0  zip_2.2.0         evaluate_0.14    
## [25] memoise_2.0.1     knitr_1.37        forcats_0.5.1    
## [28] tzdb_0.2.0        callr_3.7.0       fastmap_1.1.0    
## [31] ps_1.6.0          curl_4.3.2        fansi_0.5.0      
## [34] Rcpp_1.0.7        readr_2.1.1       cachem_1.0.6     
## [37] desc_1.4.0        pkgload_1.2.4     jsonlite_1.7.2   
## [40] fs_1.5.0          hms_1.1.1         digest_0.6.29    
## [43] stringi_1.7.6     openxlsx_4.2.5    insight_0.14.5   
## [46] processx_3.5.2    rprojroot_2.0.2   cli_3.1.0        
## [49] tools_4.1.2       magrittr_2.0.1    sass_0.4.0       
## [52] tibble_3.1.6      crayon_1.4.2      pkgconfig_2.0.3  
## [55] ellipsis_0.3.2    data.table_1.14.2 prettyunits_1.1.1
## [58] R6_2.5.1          compiler_4.1.2
```
