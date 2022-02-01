---
title: "Descriptive statistics, missing values, correlations etc."
output: 
  html_document: 
    toc: yes
    keep_md: yes
    toc_depth: 5
    number_sections: yes
---



# Preparations

## Packages


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(rio)
```

```
## The following rio suggested packages are not installed: 'arrow', 'feather', 'fst', 'hexView', 'pzfx', 'readODS', 'rmatio'
## Use 'install_formats()' to install them
```

```r
library(psych)
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'ggplot2'
```

```
## The following objects are masked from 'package:psych':
## 
##     %+%, alpha
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following object is masked from 'package:psych':
## 
##     describe
```

```
## The following objects are masked from 'package:dplyr':
## 
##     src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```
## Dataset


```r
dat<-import("../../data/processed/fdat.xlsx")
str(dat)
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
##  $ income.fr                 : chr  "quint.2" "quint.2" "quint.1" "quint.2" ...
##  $ edu                       : chr  "3. LUS" "1. <LS" "1. <LS" "3. LUS" ...
##  $ edu.f                     : chr  "3. LUS" "1. <LS" "1. <LS" "3. LUS" ...
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

## Data transformations for descriptives


### Income

(reference group needs to be redefined)


```r
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
dat$income.fr = relevel(as.factor(dat$income.f), ref="quint.5")
table(dat$income.fr,useNA="always")
```

```
## 
## quint.5 missing quint.1 quint.2 quint.3 quint.4    <NA> 
##    5262    8296    6427    6999    6793    6408       0
```

### Education

(reference group needs to be redefined)


```r
table(dat$edu,useNA="always")
```

```
## 
## 1. <LS  2. LS 3. LUS 4. UUS  5. AV  6. BA  7. MA   <NA> 
##   4085   6760   7213   7094   5671   4366   4730    266
```

```r
dat$edu.f<-relevel(as.factor(dat$edu),ref="7. MA")
table(dat$edu.f,useNA="always")
```

```
## 
##  7. MA 1. <LS  2. LS 3. LUS 4. UUS  5. AV  6. BA   <NA> 
##   4730   4085   6760   7213   7094   5671   4366    266
```

### DV


```r
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
dat$DV.f<-relevel(as.factor(dat$DV),ref="NN")
table(dat$DV.f,useNA="always")
```

```
## 
##                NN     Used_CAM_ONLY Used_conv_and_CAM    Used_conv_ONLY 
##              6877               503              5100             27705 
##              <NA> 
##                 0
```

```r
table(dat$DV.no.home,useNA="always")
```

```
## 
##                NN     Used_CAM_ONLY Used_conv_and_CAM    Used_conv_ONLY 
##              6957               423              4155             28650 
##              <NA> 
##                 0
```

```r
dat$DV.no.home.f<-relevel(as.factor(dat$DV.no.home),ref="NN")
table(dat$DV.no.home.f,useNA="always")
```

```
## 
##                NN     Used_CAM_ONLY Used_conv_and_CAM    Used_conv_ONLY 
##              6957               423              4155             28650 
##              <NA> 
##                 0
```

### Strain on health and political orientation




```r
# Calculate country means for centering

cntry.means<-dat %>%
  group_by(cntry) %>%
  summarise(strain.on.health.cntry.mean=
              mean(strain.on.health,na.rm=T),
            lrgen.cntry.mean=
              mean(lrgen,na.rm=T),
            lrecon.cntry.mean=
              mean(lrecon,na.rm=T),
            galtan.cntry.mean=
              mean(galtan,na.rm=T),
            antielite_salience.cntry.mean=
              mean(antielite_salience,na.rm=T),
            corrupt_salience.cntry.mean=
              mean(corrupt_salience,na.rm=T))

#combine data frames

dat<-left_join(
  x=dat,
  y=cntry.means,
  by="cntry"
)

#country-mean center strain on health

dat$strain.on.health.c<-
  dat$strain.on.health-dat$strain.on.health.cntry.mean

#country-mean center political orientation

dat$lrgen.c<-
  dat$lrgen-dat$lrgen.cntry.mean
dat$lrecon.c<-
  dat$lrecon-dat$lrecon.cntry.mean
dat$galtan.c<-
  dat$galtan-dat$galtan.cntry.mean
dat$antielite_salience.c<-
  dat$antielite_salience-dat$antielite_salience.cntry.mean
dat$corrupt_salience.c<-
  dat$corrupt_salience-dat$corrupt_salience.cntry.mean

#scale with CHES grand SD
dat$lrgen.z<-
  dat$lrgen.c/dat$lrgen.scaling
dat$lrecon.z<-
  dat$lrecon.c/dat$lrecon.scaling
dat$galtan.z<-
  dat$galtan.c/dat$galtan.scaling
dat$antielite_salience.z<-
  dat$antielite_salience.c/dat$antielite_salience.scaling
dat$corrupt_salience.z<-
  dat$corrupt_salience.c/dat$corrupt_salience.scaling
```

# Calculations as they appear in text


```r
table(dat$cntry!="EE" & dat$cntry!="IL")
```

```
## 
## FALSE  TRUE 
##  4613 35572
```

```r
dat$has.PO<-
  ifelse(is.na(dat$lrgen) & 
        is.na(dat$lrecon) & 
        is.na(dat$galtan) & 
        is.na(dat$antielite_salience) & 
        is.na(dat$corrupt_salience),0,1)


table(dat[dat$cntry!="EE" & dat$cntry!="IL","has.PO"])
```

```
## 
##     0     1 
## 14852 20720
```

```r
prop.table(table(dat[dat$cntry!="EE" & dat$cntry!="IL","has.PO"]))
```

```
## 
##         0         1 
## 0.4175194 0.5824806
```

```r
dat$has.PO.and.cntry<-
  ifelse(dat$has.PO & dat$cntry!="EE" & dat$cntry!="IL",1,0)


dat$has.covariates<-
  ifelse(is.na(dat$gndr.c) |
           is.na(dat$age10.c) |
           is.na(dat$income.fr) |
           is.na(dat$edu.f) |
           is.na(dat$strain.on.health),0,1)

table(dat[dat$has.PO.and.cntry==1,"has.covariates"])
```

```
## 
##     0     1 
##   128 20592
```

```r
table(is.na(dat[dat$has.PO.and.cntry==1,"income"]))
```

```
## 
## FALSE  TRUE 
## 18558  2162
```

# Calculate a frame of missingness that is general or specific for each variable


```r
table(is.na(dat$cntry))
```

```
## 
## FALSE 
## 40185
```

```r
table(is.na(dat$gndr.c))
```

```
## 
## FALSE  TRUE 
## 40163    22
```

```r
table(is.na(dat$age10.c))
```

```
## 
## FALSE  TRUE 
## 40086    99
```

```r
table(is.na(dat$income))
```

```
## 
## FALSE  TRUE 
## 31889  8296
```

```r
table(is.na(dat$edu.f))
```

```
## 
## FALSE  TRUE 
## 39919   266
```

```r
table(is.na(dat$strain.on.health.c))
```

```
## 
## FALSE  TRUE 
## 38134  2051
```

```r
table(is.na(dat$DV))
```

```
## 
## FALSE 
## 40185
```

```r
table(is.na(dat$lrgen) & 
        is.na(dat$lrecon) & 
        is.na(dat$galtan) & 
        is.na(dat$antielite_salience) & 
        is.na(dat$corrupt_salience))
```

```
## 
## FALSE  TRUE 
## 21684 18501
```


## Exclude missing variable


```r
fdat<-dat %>%
  filter(cntry!="IL" & cntry!="EE") %>%
  filter(!is.na(cntry) & 
         !is.na(gndr.c) &
         !is.na(age10.c) &
         !is.na(income.fr) &
           !is.na(edu.f) &
           !is.na(strain.on.health.c) &
           !is.na(DV) &
           !is.na(lrgen.z) &
           !is.na(lrecon.z) &
           !is.na(galtan.z) &
           !is.na(antielite_salience.z) &
           !is.na(corrupt_salience.z))
```

## Construct anweight variable for weighting


```r
fdat$anweight=fdat$pspwght*fdat$pweight
```

# Table with DV categories and covariates

Calculations with (a) and without (b) weights


```r
table(fdat$DV,useNA="always")
```

```
## 
##                NN     Used_CAM_ONLY Used_conv_and_CAM    Used_conv_ONLY 
##              3017               221              2709             14645 
##              <NA> 
##                 0
```

```r
prop.table(100*table(fdat$DV,useNA="always"))
```

```
## 
##                NN     Used_CAM_ONLY Used_conv_and_CAM    Used_conv_ONLY 
##        0.14651321        0.01073232        0.13155594        0.71119852 
##              <NA> 
##        0.00000000
```

```r
(a<-count(x = fdat, DV))
```

```
##                  DV     n
## 1                NN  3017
## 2     Used_CAM_ONLY   221
## 3 Used_conv_and_CAM  2709
## 4    Used_conv_ONLY 14645
```

```r
(b<-count(x = fdat, DV, wt = anweight))
```

```
##                  DV          n
## 1                NN  2419.9795
## 2     Used_CAM_ONLY   156.3556
## 3 Used_conv_and_CAM  2584.5312
## 4    Used_conv_ONLY 13671.2745
```

```r
sum(a$n)
```

```
## [1] 20592
```

```r
sum(b$n)
```

```
## [1] 18832.14
```

```r
round(100*a$n/sum(a$n),1)
```

```
## [1] 14.7  1.1 13.2 71.1
```

```r
round(100*b$n/sum(b$n),1)
```

```
## [1] 12.9  0.8 13.7 72.6
```

```r
round(100*prop.table(table(fdat$gndr.c,fdat$DV),1),1)
```

```
##       
##          NN Used_CAM_ONLY Used_conv_and_CAM Used_conv_ONLY
##   -0.5 18.5           1.0               9.0           71.6
##   0.5  11.0           1.2              17.2           70.7
```

```r
round(100*prop.table(table(fdat$income.fr,fdat$DV),1),1)
```

```
##          
##             NN Used_CAM_ONLY Used_conv_and_CAM Used_conv_ONLY
##   quint.5 17.1           1.3              13.2           68.4
##   missing 20.7           0.8              11.7           66.8
##   quint.1 11.1           0.5              12.1           76.3
##   quint.2 11.5           1.0              13.3           74.2
##   quint.3 14.2           1.3              14.1           70.4
##   quint.4 15.4           1.2              13.7           69.7
```

```r
round(100*prop.table(table(fdat$edu.f,fdat$DV),1),1)
```

```
##         
##            NN Used_CAM_ONLY Used_conv_and_CAM Used_conv_ONLY
##   7. MA  14.8           1.6              15.9           67.7
##   1. <LS  8.9           0.2               7.0           84.0
##   2. LS  13.7           0.6               9.7           76.0
##   3. LUS 14.2           1.0              14.6           70.2
##   4. UUS 17.7           1.0              12.4           68.8
##   5. AV  15.5           1.3              15.0           68.2
##   6. BA  15.8           1.7              14.9           67.6
```

```r
fdat %>%
    group_by(gndr.c,DV) %>%
    count(wt=anweight) %>%
  ungroup() %>%
  group_by(gndr.c) %>%
  summarise(perc=round(100*n/sum(n),1)) %>%
  t()
```

```
## `summarise()` has grouped output by 'gndr.c'. You can override using the `.groups` argument.
```

```
##        [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## gndr.c -0.5 -0.5 -0.5 -0.5  0.5  0.5  0.5  0.5
## perc   16.7  0.9  9.5 72.9  9.0  0.7 17.9 72.3
```

```r
fdat %>%
    group_by(gndr.c,DV) %>%
    count() %>%
  ungroup() %>%
  group_by(gndr.c) %>%
  summarise(perc=round(100*n/sum(n),1)) %>%
  t()
```

```
## `summarise()` has grouped output by 'gndr.c'. You can override using the `.groups` argument.
```

```
##        [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## gndr.c -0.5 -0.5 -0.5 -0.5  0.5  0.5  0.5  0.5
## perc   18.5  1.0  9.0 71.6 11.0  1.2 17.2 70.7
```

## Cross-tabs of DV categories


```r
(ct.use<-table(fdat$used.conv,fdat$used.CAM,dnn = c("CM","CAM")))
```

```
##    CAM
## CM      0     1
##   0  3017   221
##   1 14645  2709
```

```r
(pt.use<-round(100*prop.table(ct.use),1))
```

```
##    CAM
## CM     0    1
##   0 14.7  1.1
##   1 71.1 13.2
```

```r
(t.CAM.use<-table(fdat$used.CAM))
```

```
## 
##     0     1 
## 17662  2930
```

```r
(pt.CAM.use<-round(100*prop.table(t.CAM.use),1))
```

```
## 
##    0    1 
## 85.8 14.2
```

```r
(t.conv.use<-table(fdat$used.conv))
```

```
## 
##     0     1 
##  3238 17354
```

```r
(pt.conv.use<-round(100*prop.table(t.conv.use),1))
```

```
## 
##    0    1 
## 15.7 84.3
```

```r
round(100*prop.table(ct.use,margin = c(1)),1)
```

```
##    CAM
## CM     0    1
##   0 93.2  6.8
##   1 84.4 15.6
```

```r
round(100*prop.table(ct.use,margin = c(2)),1)
```

```
##    CAM
## CM     0    1
##   0 17.1  7.5
##   1 82.9 92.5
```

# Correlations between variables


```r
#recode categorical variables to numeric

#education
table(fdat$edu)
```

```
## 
## 1. <LS  2. LS 3. LUS 4. UUS  5. AV  6. BA  7. MA 
##   1977   2799   3984   3239   3117   2557   2919
```

```r
fdat$edu.ord<-as.numeric(substr(fdat$edu,1,1))
table(fdat$edu.ord)
```

```
## 
##    1    2    3    4    5    6    7 
## 1977 2799 3984 3239 3117 2557 2919
```

```r
#income
table(fdat$income)
```

```
## 
## quint.1 quint.2 quint.3 quint.4 quint.5 
##    3101    3768    3940    4029    3630
```

```r
fdat$income.ord<-as.numeric(substr(fdat$income,7,7))
table(fdat$income.ord)
```

```
## 
##    1    2    3    4    5 
## 3101 3768 3940 4029 3630
```

```r
#income missing
table(fdat$income.fr)
```

```
## 
## quint.5 missing quint.1 quint.2 quint.3 quint.4 
##    3630    2124    3101    3768    3940    4029
```

```r
fdat$income.missing<-ifelse(is.na(fdat$income),1,0)
table(fdat$income.missing)
```

```
## 
##     0     1 
## 18468  2124
```

```r
#correlation variables
cor.vars<-
  c("gndr.c","age10.c","edu.ord","income.ord","income.missing",
    "strain.on.health.c",
    "lrgen.z","lrecon.z","galtan.z",
    "antielite_salience.z","corrupt_salience.z")

psych::describe(fdat[,cor.vars],
                fast=T)
```

```
##                      vars     n mean   sd   min   max range   se
## gndr.c                  1 20592 0.01 0.50 -0.50  0.50  1.00 0.00
## age10.c                 2 20592 0.39 1.70 -3.43  5.17  8.60 0.01
## edu.ord                 3 20592 4.07 1.88  1.00  7.00  6.00 0.01
## income.ord              4 18468 3.07 1.37  1.00  5.00  4.00 0.01
## income.missing          5 20592 0.10 0.30  0.00  1.00  1.00 0.00
## strain.on.health.c      6 20592 0.07 2.44 -3.20 18.26 21.46 0.02
## lrgen.z                 7 20592 0.00 0.88 -2.12  2.20  4.32 0.01
## lrecon.z                8 20592 0.00 0.93 -2.35  2.14  4.49 0.01
## galtan.z                9 20592 0.00 0.80 -1.83  2.04  3.87 0.01
## antielite_salience.z   10 20592 0.00 0.85 -1.07  2.70  3.77 0.01
## corrupt_salience.z     11 20592 0.00 0.54 -1.28  1.75  3.03 0.00
```

```r
#pearson correlations
pearson<-
  corr.test(fdat[,cor.vars],
            method = "pearson",adjust = "none")
```

```
## Warning in cor(x, use = use, method = method): the standard deviation is zero
```

```r
export(pearson$r,
       "../../results/cors.pearson.r.xlsx",
       overwrite=T)

#spearman correlations
spearman<-
  corr.test(fdat[,cor.vars],
            method = "spearman",adjust = "none")
```

```
## Warning in cor(x, use = use, method = method): the standard deviation is zero
```

```r
export(spearman$r,"../../results/cors.spearman.r.xlsx",
       overwrite=T)


weighted_corr <- 
  cov.wt(fdat[complete.cases(fdat[,cor.vars]),
                             cor.vars],
         wt = fdat[complete.cases(fdat[,cor.vars]),
                   "anweight"], cor = TRUE)
corr_matrix <- weighted_corr$cor
round(corr_matrix,2)
```

```
##                      gndr.c age10.c edu.ord income.ord income.missing
## gndr.c                 1.00    0.04   -0.04      -0.11            NaN
## age10.c                0.04    1.00   -0.32      -0.23            NaN
## edu.ord               -0.04   -0.32    1.00       0.41            NaN
## income.ord            -0.11   -0.23    0.41       1.00            NaN
## income.missing          NaN     NaN     NaN        NaN            NaN
## strain.on.health.c     0.12    0.16   -0.12      -0.16            NaN
## lrgen.z               -0.03    0.04   -0.06       0.04            NaN
## lrecon.z              -0.03    0.05    0.01       0.09            NaN
## galtan.z              -0.03    0.09   -0.12      -0.02            NaN
## antielite_salience.z  -0.04   -0.10   -0.05      -0.10            NaN
## corrupt_salience.z    -0.02   -0.11   -0.01      -0.07            NaN
##                      strain.on.health.c lrgen.z lrecon.z galtan.z
## gndr.c                             0.12   -0.03    -0.03    -0.03
## age10.c                            0.16    0.04     0.05     0.09
## edu.ord                           -0.12   -0.06     0.01    -0.12
## income.ord                        -0.16    0.04     0.09    -0.02
## income.missing                      NaN     NaN      NaN      NaN
## strain.on.health.c                 1.00   -0.02    -0.03     0.00
## lrgen.z                           -0.02    1.00     0.85     0.82
## lrecon.z                          -0.03    0.85     1.00     0.59
## galtan.z                           0.00    0.82     0.59     1.00
## antielite_salience.z               0.02    0.08    -0.25     0.21
## corrupt_salience.z                 0.01    0.00    -0.28    -0.06
##                      antielite_salience.z corrupt_salience.z
## gndr.c                              -0.04              -0.02
## age10.c                             -0.10              -0.11
## edu.ord                             -0.05              -0.01
## income.ord                          -0.10              -0.07
## income.missing                        NaN                NaN
## strain.on.health.c                   0.02               0.01
## lrgen.z                              0.08               0.00
## lrecon.z                            -0.25              -0.28
## galtan.z                             0.21              -0.06
## antielite_salience.z                 1.00               0.77
## corrupt_salience.z                   0.77               1.00
```

```r
export(corr_matrix,
       "../../results/cors.weighted.pearson.r.xlsx",
       overwrite=T)

corr_matrix.t<-
  (corr_matrix*sqrt(weighted_corr$n.obs-2))/sqrt(1-corr_matrix^2)
```

```
## Warning in sqrt(1 - corr_matrix^2): NaNs produced
```

```r
corr_matrix.p<-
  2*(1-pt(abs(corr_matrix.t),df=weighted_corr$n.obs-2))
round(corr_matrix.p,3)
```

```
##                      gndr.c age10.c edu.ord income.ord income.missing
## gndr.c                0.000       0   0.000      0.000            NaN
## age10.c               0.000       0   0.000      0.000            NaN
## edu.ord               0.000       0     NaN      0.000            NaN
## income.ord            0.000       0   0.000      0.000            NaN
## income.missing          NaN     NaN     NaN        NaN            NaN
## strain.on.health.c    0.000       0   0.000      0.000            NaN
## lrgen.z               0.000       0   0.000      0.000            NaN
## lrecon.z              0.001       0   0.436      0.000            NaN
## galtan.z              0.000       0   0.000      0.001            NaN
## antielite_salience.z  0.000       0   0.000      0.000            NaN
## corrupt_salience.z    0.006       0   0.159      0.000            NaN
##                      strain.on.health.c lrgen.z lrecon.z galtan.z
## gndr.c                            0.000   0.000    0.001    0.000
## age10.c                           0.000   0.000    0.000    0.000
## edu.ord                           0.000   0.000    0.436    0.000
## income.ord                        0.000   0.000    0.000    0.001
## income.missing                      NaN     NaN      NaN      NaN
## strain.on.health.c                0.000   0.001    0.000    0.563
## lrgen.z                           0.001     NaN    0.000    0.000
## lrecon.z                          0.000   0.000      NaN    0.000
## galtan.z                          0.563   0.000    0.000    0.000
## antielite_salience.z              0.012   0.000    0.000    0.000
## corrupt_salience.z                0.489   0.797    0.000    0.000
##                      antielite_salience.z corrupt_salience.z
## gndr.c                              0.000              0.006
## age10.c                             0.000              0.000
## edu.ord                             0.000              0.159
## income.ord                          0.000              0.000
## income.missing                        NaN                NaN
## strain.on.health.c                  0.012              0.489
## lrgen.z                             0.000              0.797
## lrecon.z                            0.000              0.000
## galtan.z                            0.000              0.000
## antielite_salience.z                0.000              0.000
## corrupt_salience.z                  0.000                NaN
```

```r
export(corr_matrix.p,
       "../../results/cors.weighted.pearson.r.p.xlsx",
       overwrite=T)
```

# Weighted descriptive statistics


```r
#gender
(round(weighted.mean(fdat$gndr.c,w=fdat$anweight),2)->gndr.mean.wt)
```

```
## [1] 0
```

```r
(round(sqrt(wtd.var(fdat$gndr.c,w=fdat$anweight)),2)->gndr.sd.wt)
```

```
## [1] 0.5
```

```r
#age
(round(weighted.mean(fdat$agea,w=fdat$anweight),2)->agea.mean.wt)
```

```
## [1] 52.48
```

```r
(round(sqrt(wtd.var(fdat$agea,w=fdat$anweight)),2)->agea.sd.wt)
```

```
## [1] 17.15
```

```r
#education
(round(weighted.mean(fdat$edu.ord,w=fdat$anweight),2)->edu.ord.mean.wt)
```

```
## [1] 3.91
```

```r
(round(sqrt(wtd.var(fdat$edu.ord,w=fdat$anweight)),2)->edu.ord.sd.wt)
```

```
## [1] 1.92
```

```r
#income
(round(weighted.mean(fdat$income.ord,
                     w=fdat$anweight,na.rm=T),2)->income.ord.mean.wt)
```

```
## [1] 3.19
```

```r
(round(sqrt(wtd.var(fdat$income.ord,
                    w=fdat$anweight,na.rm=T)),2)->income.ord.sd.wt)
```

```
## [1] 1.36
```

```r
#strain on health
(round(weighted.mean(fdat$strain.on.health,
                     w=fdat$anweight),2)->strain.on.health.mean.wt)
```

```
## [1] 2.75
```

```r
(round(sqrt(wtd.var(fdat$strain.on.health,
                    w=fdat$anweight)),2)->strain.on.health.sd.wt)
```

```
## [1] 2.53
```

```r
#lrgen
(round(weighted.mean(fdat$lrgen,
                     w=fdat$anweight),2)->lrgen.mean.wt)
```

```
## [1] 5.47
```

```r
(round(sqrt(wtd.var(fdat$lrgen,
                    w=fdat$anweight)),2)->lrgen.sd.wt)
```

```
## [1] 2.05
```

```r
#lrecon
(round(weighted.mean(fdat$lrecon,
                     w=fdat$anweight),2)->lrecon.mean.wt)
```

```
## [1] 5.19
```

```r
(round(sqrt(wtd.var(fdat$lrecon,
                    w=fdat$anweight)),2)->lrecon.sd.wt)
```

```
## [1] 1.99
```

```r
#galtan
(round(weighted.mean(fdat$galtan,
                     w=fdat$anweight),2)->galtan.mean.wt)
```

```
## [1] 5.1
```

```r
(round(sqrt(wtd.var(fdat$galtan,
                    w=fdat$anweight)),2)->galtan.sd.wt)
```

```
## [1] 2.24
```

```r
#antielite_salience
(round(weighted.mean(fdat$antielite_salience,
                     w=fdat$anweight),2)->antielite_salience.mean.wt)
```

```
## [1] 3.24
```

```r
(round(sqrt(wtd.var(fdat$antielite_salience,
                    w=fdat$anweight)),2)->antielite_salience.sd.wt)
```

```
## [1] 2.29
```

```r
#corrupt_salience
(round(weighted.mean(fdat$corrupt_salience,
                     w=fdat$anweight),2)->corrupt_salience.mean.wt)
```

```
## [1] 3.93
```

```r
(round(sqrt(wtd.var(fdat$corrupt_salience,
                    w=fdat$anweight)),2)->corrupt_salience.sd.wt)
```

```
## [1] 1.84
```

```r
# combine to same table

mean.wt<-
  c(gndr.mean.wt,
  agea.mean.wt,
  edu.ord.mean.wt,
  income.ord.mean.wt,
  strain.on.health.mean.wt,
  lrgen.mean.wt,
  lrecon.mean.wt,
  galtan.mean.wt,
  antielite_salience.mean.wt,
  corrupt_salience.mean.wt
  )

sd.wt<-
  c(gndr.sd.wt,
  agea.sd.wt,
  edu.ord.sd.wt,
  income.ord.sd.wt,
  strain.on.health.sd.wt,
  lrgen.sd.wt,
  lrecon.sd.wt,
  galtan.sd.wt,
  antielite_salience.sd.wt,
  corrupt_salience.sd.wt
  )

var.names<-c("1. gender",
        "2. age",
        "3. education",
        "4. income",
        "5. strain on health",
        "6. left-right general",
        "7. left-right economic",
        "8. GAL-TAN",
        "9. antielite salience",
        "10. corruput salience")

desc.tbl.wt<-
  cbind.data.frame(var.names,
      mean.wt,
      sd.wt)

export(desc.tbl.wt,"../../results/desc.tbl.wt.xlsx",
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
##  [1] Hmisc_4.6-0     ggplot2_3.3.5   Formula_1.2-4   survival_3.2-13
##  [5] lattice_0.20-45 psych_2.1.9     rio_0.5.29      dplyr_1.0.7    
##  [9] knitr_1.37      rmarkdown_2.11  devtools_2.4.3  usethis_2.0.1  
## 
## loaded via a namespace (and not attached):
##  [1] nlme_3.1-153        fs_1.5.0            RColorBrewer_1.1-2 
##  [4] rprojroot_2.0.2     tools_4.1.2         backports_1.4.1    
##  [7] bslib_0.3.1         utf8_1.2.2          R6_2.5.1           
## [10] rpart_4.1-15        DBI_1.1.2           colorspace_2.0-2   
## [13] nnet_7.3-16         withr_2.4.3         tidyselect_1.1.1   
## [16] gridExtra_2.3       prettyunits_1.1.1   mnormt_2.0.2       
## [19] processx_3.5.2      curl_4.3.2          compiler_4.1.2     
## [22] cli_3.1.0           htmlTable_2.3.0     desc_1.4.0         
## [25] sass_0.4.0          scales_1.1.1        checkmate_2.0.0    
## [28] callr_3.7.0         stringr_1.4.0       digest_0.6.29      
## [31] foreign_0.8-81      base64enc_0.1-3     jpeg_0.1-9         
## [34] pkgconfig_2.0.3     htmltools_0.5.2     sessioninfo_1.2.2  
## [37] fastmap_1.1.0       htmlwidgets_1.5.4   rlang_0.4.12       
## [40] readxl_1.3.1        rstudioapi_0.13     jquerylib_0.1.4    
## [43] generics_0.1.1      jsonlite_1.7.2      zip_2.2.0          
## [46] magrittr_2.0.1      Matrix_1.3-4        Rcpp_1.0.7         
## [49] munsell_0.5.0       fansi_0.5.0         lifecycle_1.0.1    
## [52] stringi_1.7.6       yaml_2.2.1          pkgbuild_1.3.1     
## [55] grid_4.1.2          parallel_4.1.2      forcats_0.5.1      
## [58] crayon_1.4.2        haven_2.4.3         splines_4.1.2      
## [61] hms_1.1.1           tmvnsim_1.0-2       ps_1.6.0           
## [64] pillar_1.6.4        pkgload_1.2.4       glue_1.6.0         
## [67] evaluate_0.14       latticeExtra_0.6-29 data.table_1.14.2  
## [70] remotes_2.4.2       vctrs_0.3.8         png_0.1-7          
## [73] testthat_3.0.4      cellranger_1.1.0    gtable_0.3.0       
## [76] purrr_0.3.4         assertthat_0.2.1    cachem_1.0.6       
## [79] xfun_0.29           openxlsx_4.2.5      tibble_3.1.6       
## [82] memoise_2.0.1       cluster_2.1.2       ellipsis_0.3.2
```
