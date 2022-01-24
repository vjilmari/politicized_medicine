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


# obtain cote variable names in ESS
vote.vars<-unique(vote.dat$vote.var)

# exclude unused vote variable

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
# sum across the voting variables

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
## `summarise()` has grouped output by 'cntry'. You can override using the `.groups` argument.
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
  
  name1<-as.character(as_label(tmp.dat[,vote.var]))
  name2<-as.character(tmp.dat[,"pt.name"])

  
  for (j in 1:length(unique(name1))){
    print(c(unique(name1)[j],
            name2[which(name1==unique(name1)[j])[1]]))
  }
  
}
```

```
## [1] "AT"
## [1] "<dbl>" NA     
## [1] "BE"
## [1] "<dbl>"    "Open VLD"
## [1] "CH"
## [1] "<dbl>" NA     
## [1] "CZ"
## [1] "<dbl>" NA     
## [1] "DE"
## [1] "<dbl>" "SPD"  
## [1] "DK"
## [1] "<dbl>" NA     
## [1] "EE"
## [1] "<dbl>"             "Eesti Keskerakond"
## [1] "ES"
## [1] "<dbl>"  "AMAIUR"
## [1] "FI"
## [1] "<dbl>"            "The Centre Party"
## [1] "FR"
## [1] "<dbl>" NA     
## [1] "GB"
## [1] "<dbl>"            "Liberal Democrat"
## [1] "HU"
## [1] "<dbl>" NA     
## [1] "IE"
## [1] "<dbl>"  "Labour"
## [1] "LT"
## [1] "<dbl>"                                    
## [2] "Lithuanian Social Democratic Party (LSDP)"
## [1] "NL"
## [1] "<dbl>" NA     
## [1] "NO"
## [1] "<dbl>"                "Progress Party (FRP)"
## [1] "PL"
## [1] "<dbl>" NA     
## [1] "PT"
## [1] "<dbl>" NA     
## [1] "SE"
## [1] "<dbl>"                    "Moderata samlingspartiet"
## [1] "SI"
## [1] "<dbl>" NA
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
## [1] dplyr_1.0.7      sjlabelled_1.1.8 rio_0.5.29       rmarkdown_2.11  
## [5] devtools_2.4.3   usethis_2.0.1   
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.7        prettyunits_1.1.1 ps_1.6.0         
##  [4] assertthat_0.2.1  rprojroot_2.0.2   digest_0.6.29    
##  [7] utf8_1.2.2        R6_2.5.1          cellranger_1.1.0 
## [10] evaluate_0.14     pillar_1.6.4      rlang_0.4.12     
## [13] curl_4.3.2        readxl_1.3.1      data.table_1.14.2
## [16] callr_3.7.0       jquerylib_0.1.4   desc_1.4.0       
## [19] readr_2.1.1       stringr_1.4.0     foreign_0.8-81   
## [22] compiler_4.1.2    xfun_0.29         pkgconfig_2.0.3  
## [25] pkgbuild_1.3.1    htmltools_0.5.2   insight_0.14.5   
## [28] tidyselect_1.1.1  tibble_3.1.6      fansi_0.5.0      
## [31] crayon_1.4.2      tzdb_0.2.0        withr_2.4.3      
## [34] jsonlite_1.7.2    lifecycle_1.0.1   DBI_1.1.2        
## [37] magrittr_2.0.1    zip_2.2.0         cli_3.1.0        
## [40] stringi_1.7.6     cachem_1.0.6      fs_1.5.0         
## [43] remotes_2.4.2     testthat_3.0.4    bslib_0.3.1      
## [46] ellipsis_0.3.2    vctrs_0.3.8       generics_0.1.1   
## [49] openxlsx_4.2.5    tools_4.1.2       forcats_0.5.1    
## [52] glue_1.6.0        purrr_0.3.4       hms_1.1.1        
## [55] processx_3.5.2    pkgload_1.2.4     fastmap_1.1.0    
## [58] yaml_2.2.1        sessioninfo_1.2.2 memoise_2.0.1    
## [61] knitr_1.37        haven_2.4.3       sass_0.4.0
```
