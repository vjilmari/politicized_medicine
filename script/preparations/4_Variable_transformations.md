---
title: "Variable transformation before running the analysis"
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
```

## Load data


```r
# Long format data with ESS and CHES merged
dat<-import("../../data/processed/dat.xlsx")

# ESS raw data from which variable labels can be obtained
ESS.dat<-import("../../data/raw/ESS7e02_2.sav")
```

# Variable transformations

## Gender


```r
attr(ESS.dat$gndr,"labels")
```

```
##      Male    Female No answer 
##         1         2         9
```

```r
# Factorial gndr

dat$gndr.f<-case_when(dat$gndr==1~"Male",
                      dat$gndr==2~"Female",
                      TRUE~NA_character_)

table(dat$gndr.f,useNA="always")
```

```
## 
## Female   Male   <NA> 
##  21292  18871     22
```

```r
# Numerical gndr

dat$gndr.c<-case_when(dat$gndr==1~-0.5,
                      dat$gndr==2~0.5,
                      TRUE~NA_real_)

table(dat$gndr.c,useNA="always")
```

```
## 
##  -0.5   0.5  <NA> 
## 18871 21292    22
```

## Age


```r
attr(ESS.dat$agea,"labels")
```

```
## Not available 
##           999
```

```r
table(dat$agea==999)
```

```
## 
## FALSE 
## 40086
```

```r
# centered age divided by 10
dat$age10.c<-(dat$agea-mean(dat$agea,na.rm=T))/10
```

## Income


```r
attr(ESS.dat$hinctnta,"labels")
```

```
##  J - 1st decile  R - 2nd decile  C - 3rd decile  M - 4th decile 
##               1               2               3               4 
##  F - 5th decile  S - 6th decile  K - 7th decile  P - 8th decile 
##               5               6               7               8 
##  D - 9th decile H - 10th decile         Refusal      Don't know 
##               9              10              77              88 
##       No answer 
##              99
```

```r
# recode deciles to quintiles

dat$income<-case_when(
  dat$hinctnta==1 | dat$hinctnta==2 ~ "quint.1",
  dat$hinctnta==3 | dat$hinctnta==4 ~ "quint.2",
  dat$hinctnta==5 | dat$hinctnta==6 ~ "quint.3",
  dat$hinctnta==7 | dat$hinctnta==8 ~ "quint.4",
  dat$hinctnta==9 | dat$hinctnta==10 ~ "quint.5"
)

table(dat$income,useNA="always")
```

```
## 
## quint.1 quint.2 quint.3 quint.4 quint.5    <NA> 
##    6427    6999    6793    6408    5262    8296
```

```r
# add missing as additional factor level (to a new variable income.f)

dat$income.f<-case_when(
  is.na(dat$income) ~ "missing",
  TRUE ~ dat$income
)

#define reference level (top quintile)
table(dat$income.f,useNA="always")
```

```
## 
## missing quint.1 quint.2 quint.3 quint.4 quint.5    <NA> 
##    8296    6427    6999    6793    6408    5262       0
```

```r
dat$income.fr = relevel(as.factor(dat$income.f),
                        ref="quint.5")
table(dat$income.fr,useNA="always")
```

```
## 
## quint.5 missing quint.1 quint.2 quint.3 quint.4    <NA> 
##    5262    8296    6427    6999    6793    6408       0
```

## Education


```r
attr(ESS.dat$eisced,"labels")
```

```
##             Not possible to harmonise into ES-ISCED 
##                                                   0 
##              ES-ISCED I , less than lower secondary 
##                                                   1 
##                        ES-ISCED II, lower secondary 
##                                                   2 
##           ES-ISCED IIIb, lower tier upper secondary 
##                                                   3 
##           ES-ISCED IIIa, upper tier upper secondary 
##                                                   4 
##        ES-ISCED IV, advanced vocational, sub-degree 
##                                                   5 
##     ES-ISCED V1, lower tertiary education, BA level 
##                                                   6 
## ES-ISCED V2, higher tertiary education, >= MA level 
##                                                   7 
##                                               Other 
##                                                  55 
##                                             Refusal 
##                                                  77 
##                                          Don't know 
##                                                  88 
##                                           No answer 
##                                                  99
```

```r
# recode education variable

dat$edu<-case_when(dat$eisced==0~NA_character_,
                   dat$eisced==1~"1. <LS",
                   dat$eisced==2~"2. LS",
                   dat$eisced==3~"3. LUS",
                   dat$eisced==4~"4. UUS",
                   dat$eisced==5~"5. AV",
                   dat$eisced==6~"6. BA",
                   dat$eisced==7~"7. MA",
                   TRUE~NA_character_)

table(dat$edu,useNA="always")
```

```
## 
## 1. <LS  2. LS 3. LUS 4. UUS  5. AV  6. BA  7. MA   <NA> 
##   4085   6760   7213   7094   5671   4366   4730    266
```

```r
# recode reference education (highest) to a new variable (edu.f)

dat$edu.f<-relevel(as.factor(dat$edu),ref="7. MA")
table(dat$edu.f)
```

```
## 
##  7. MA 1. <LS  2. LS 3. LUS 4. UUS  5. AV  6. BA 
##   4730   4085   6760   7213   7094   5671   4366
```

## Health problems


```r
# hampered health problems (example)
attr(ESS.dat$hltphhc,"labels")
```

```
## Not marked     Marked 
##          0          1
```

```r
# non-hampered (example)
attr(ESS.dat$hltprhc,"labels")
```

```
## Not marked     Marked 
##          0          1
```

```r
# hampered variable names
hampered.vars<-c(
  "hltphhc",
  "hltphhb",
  "hltphbp",
  "hltphal",
  "hltphbn",
  "hltphpa",
  "hltphpf",
  "hltphsd",
  "hltphsc",
  "hltphsh",
  "hltphdi"
)

table(rowSums(dat[,hampered.vars]),useNA="always")
```

```
## 
##     0     1     2     3     4     5     6     7     8     9    10    11 
## 21914 10012  3688  1470   570   241   107    57    45    16     8     6 
##  <NA> 
##  2051
```

```r
#non-hampered problems variable names
problems.vars<-c(
  "hltprhc",
  "hltprhb",
  "hltprbp",
  "hltpral",
  "hltprbn",
  "hltprpa",
  "hltprpf",
  "hltprsd",
  "hltprsc",
  "hltprsh",
  "hltprdi"
)

table(rowSums(dat[,problems.vars]),useNA="always")
```

```
## 
##     0     1     2     3     4     5     6     7     8     9    10    11 
## 10967  9545  7436  4752  2667  1445   713   328   174    64    29    14 
##  <NA> 
##  2051
```

```r
# test Estonia separately

table(rowSums(dat[dat$cntry=="EE",problems.vars]),useNA="always")
```

```
## 
## <NA> 
## 2051
```

```r
table(ESS.dat[ESS.dat$cntry=="EE","hltprhc"])
```

```
## < table of extent 0 >
```

```r
# loop through each type of problem and calculate the weight

# If hampered variant = 2, if non-hampered variant = 1, else = 0

problem.weights<-matrix(ncol=length(hampered.vars),
                        nrow=nrow(dat))

for (i in 1:length(hampered.vars)){
  problem.weights[,i]<-
    ifelse(dat[,hampered.vars[i]]==1,2,
           ifelse(dat[,problems.vars[i]]==1,1,0))
}

# calculate total strain on health

dat$strain.on.health<-rowSums(problem.weights)

table(dat$strain.on.health,useNA="always")
```

```
## 
##     0     1     2     3     4     5     6     7     8     9    10    11 
## 10967  5457  7171  4207  3934  1859  1778   884   757   323   336   125 
##    12    13    14    15    16    17    18    19    20    21    22  <NA> 
##   130    44    62    17    47     4    14     3     8     1     6  2051
```

## Dependent variable


```r
# visited general practitioner
attr(ESS.dat$dshltgp,"labels")
```

```
## Not marked     Marked 
##          0          1
```

```r
# visited medical specialist
attr(ESS.dat$dshltms,"labels")
```

```
## Not marked     Marked 
##          0          1
```

```r
table(dat$dshltgp,useNA="always")
```

```
## 
##     0     1  <NA> 
## 10226 29959     0
```

```r
table(dat$dshltms,useNA="always")
```

```
## 
##     0     1  <NA> 
## 23265 16920     0
```

```r
# use of conventional medicine

dat$used.conv<-ifelse(dat$dshltgp==1 | dat$dshltms==1,1,0)
table(dat$used.conv,useNA="always")
```

```
## 
##     0     1  <NA> 
##  7380 32805     0
```

```r
# use of CAMs

#acupuncture trhltacu
#acupressure trhltacp
#chinese medicine trhltcm
#homeopathy trhltho
#herbal treatment trhltht
#hypnotherapy trhlthy
#spiritual healing trhltsh

attr(ESS.dat$trhltacu,"labels")
```

```
## Not marked     Marked 
##          0          1
```

```r
table(dat$trhltacu)
```

```
## 
##     0     1 
## 38630  1555
```

```r
dat$used.CAM<-ifelse(
  dat$trhltacu==1 |
  dat$trhltacp==1 |
  dat$trhltcm==1 |
  dat$trhltho==1 |
  dat$trhltht==1 |
  dat$trhlthy==1 |
  dat$trhltsh==1,1,0)

table(dat$used.CAM,useNA="always")
```

```
## 
##     0     1  <NA> 
## 34582  5603     0
```

```r
# code CAM-use also without homeopathy


dat$used.CAM.no.home<-ifelse(
  dat$trhltacu==1 |
    dat$trhltacp==1 |
    dat$trhltcm==1 |
    dat$trhltht==1 |
    dat$trhlthy==1 |
    dat$trhltsh==1,1,0)

table(dat$used.CAM.no.home,useNA="always")
```

```
## 
##     0     1  <NA> 
## 35607  4578     0
```

```r
# Code the four category variable

dat$DV<-case_when(
  dat$used.conv==0 & dat$used.CAM==0 ~ "NN",
  dat$used.conv==1 & dat$used.CAM==0 ~ "Used_conv_ONLY",
  dat$used.conv==0 & dat$used.CAM==1 ~ "Used_CAM_ONLY",
  dat$used.conv==1 & dat$used.CAM==1 ~ "Used_conv_and_CAM")

table(dat$DV,useNA="always")
```

```
## 
##                NN     Used_CAM_ONLY Used_conv_and_CAM    Used_conv_ONLY 
##              6877               503              5100             27705 
##              <NA> 
##                 0
```

```r
# without homeopathy as CAM

dat$DV.no.home<-case_when(
  dat$used.conv==0 & dat$used.CAM.no.home==0 ~ "NN",
  dat$used.conv==1 & dat$used.CAM.no.home==0 ~ "Used_conv_ONLY",
  dat$used.conv==0 & dat$used.CAM.no.home==1 ~ "Used_CAM_ONLY",
  dat$used.conv==1 & dat$used.CAM.no.home==1 ~ "Used_conv_and_CAM")

table(dat$DV.no.home,useNA="always")
```

```
## 
##                NN     Used_CAM_ONLY Used_conv_and_CAM    Used_conv_ONLY 
##              6957               423              4155             28650 
##              <NA> 
##                 0
```

## Political orientation


```r
#add scaling SDs to the data.frame from CHES dataset
CHES_2014<-
  import("../../data/raw/2014_CHES_dataset_means.csv")

#obtain the sd for each PO variable
dat$lrgen.scaling<-sd(CHES_2014$lrgen,na.rm=T)
dat$lrecon.scaling<-sd(CHES_2014$lrecon,na.rm=T)
dat$galtan.scaling<-sd(CHES_2014$galtan,na.rm=T)
dat$antielite_salience.scaling<-
  sd(CHES_2014$antielite_salience,na.rm=T)
dat$corrupt_salience.scaling<-
  sd(CHES_2014$corrupt_salience,na.rm=T)
```


# Final set of variables needed for the analysis


```r
analysis.vars<-
  c("idno","cntry","dweight","pspwght","pweight",
    "pt.nmbr","pt.name",
    "gndr.f","gndr.c","agea","age10.c",
    "income","income.f","income.fr",
    "edu","edu.f","strain.on.health",
    "used.conv","used.CAM","DV",
    "used.CAM.no.home","DV.no.home",
    "lrgen","lrecon","galtan",
    "antielite_salience","corrupt_salience",
    "lrgen.scaling","lrecon.scaling","galtan.scaling",
    "antielite_salience.scaling","corrupt_salience.scaling")

# test if they are all in the data file
analysis.vars %in% names(dat)
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [15] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [29] TRUE TRUE TRUE TRUE
```

```r
# exclude variable not needed
fdat<-dat[,analysis.vars]
str(fdat)
```

```
## 'data.frame':	40185 obs. of  32 variables:
##  $ idno                      : num  1 2 3 4 5 6 7 13 14 21 ...
##  $ cntry                     : chr  "AT" "AT" "AT" "AT" ...
##  $ dweight                   : num  0.938 0.938 0.938 0.938 0.938 ...
##  $ pspwght                   : num  0.871 0.864 1.419 1.026 0.739 ...
##  $ pweight                   : num  0.406 0.406 0.406 0.406 0.406 ...
##  $ pt.nmbr                   : num  NA 6 2 3 NA 1 1 1 7 2 ...
##  $ pt.name                   : chr  NA NA "ÖVP" "FPÖ" ...
##  $ gndr.f                    : chr  "Male" "Male" "Female" "Male" ...
##  $ gndr.c                    : num  -0.5 -0.5 0.5 -0.5 0.5 0.5 -0.5 0.5 0.5 0.5 ...
##  $ agea                      : num  51 67 89 32 56 67 66 67 34 66 ...
##  $ age10.c                   : num  0.172 1.772 3.972 -1.728 0.672 ...
##  $ income                    : chr  "quint.2" "quint.2" "quint.1" "quint.2" ...
##  $ income.f                  : chr  "quint.2" "quint.2" "quint.1" "quint.2" ...
##  $ income.fr                 : Factor w/ 6 levels "quint.5","missing",..: 4 4 3 4 6 5 4 4 4 3 ...
##  $ edu                       : chr  "3. LUS" "1. <LS" "1. <LS" "3. LUS" ...
##  $ edu.f                     : Factor w/ 7 levels "7. MA","1. <LS",..: 4 2 2 4 4 4 4 6 1 4 ...
##  $ strain.on.health          : num  1 7 2 4 2 2 5 6 6 3 ...
##  $ used.conv                 : num  0 1 1 1 1 1 1 1 1 1 ...
##  $ used.CAM                  : num  0 0 0 0 0 0 0 1 0 1 ...
##  $ DV                        : chr  "NN" "Used_conv_ONLY" "Used_conv_ONLY" "Used_conv_ONLY" ...
##  $ used.CAM.no.home          : num  0 0 0 0 0 0 0 1 0 1 ...
##  $ DV.no.home                : chr  "NN" "Used_conv_ONLY" "Used_conv_ONLY" "Used_conv_ONLY" ...
##  $ lrgen                     : num  NA NA 6.1 8.7 NA ...
##  $ lrecon                    : num  NA NA 6.4 5.5 NA ...
##  $ galtan                    : num  NA NA 7.2 8.8 NA ...
##  $ antielite_salience        : num  NA NA 1.6 8 NA ...
##  $ corrupt_salience          : num  NA NA 2.4 5.1 NA ...
##  $ lrgen.scaling             : num  2.3 2.3 2.3 2.3 2.3 ...
##  $ lrecon.scaling            : num  2.21 2.21 2.21 2.21 2.21 ...
##  $ galtan.scaling            : num  2.63 2.63 2.63 2.63 2.63 ...
##  $ antielite_salience.scaling: num  2.59 2.59 2.59 2.59 2.59 ...
##  $ corrupt_salience.scaling  : num  2.32 2.32 2.32 2.32 2.32 ...
```

```r
# save the final data file
export(fdat,
       "../../data/processed/fdat.xlsx",overwrite=T)
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
