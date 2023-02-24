---
title: "Analysis with belonging to minority and being in a relationship added as covariates"
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
library(mclogit)
library(emmeans)
library(dplyr)
library(rio)
library(memisc)
library(psych)
library(ggplot2)
library(ggpubr)
library(lme4)
library(MetBrewer)
source("../custom_functions.R")
```
## Dataset


```r
fdat<-import("../../data/processed/fdat.xlsx")
```

## Data transformations

The reference levels for factorial variables need to be redefined, .xlsx format does not understand factor formats.

### Income


```r
fdat$income.f<-case_when(
  is.na(fdat$income) ~ "missing",
  TRUE ~ fdat$income
)

#define reference level (top quintile)
table(fdat$income.f,useNA="always")
```

```
## 
## missing quint.1 quint.2 quint.3 quint.4 quint.5    <NA> 
##    8296    6427    6999    6793    6408    5262       0
```

```r
fdat$income.fr = relevel(as.factor(fdat$income.f), ref="quint.5")
table(fdat$income.fr,useNA="always")
```

```
## 
## quint.5 missing quint.1 quint.2 quint.3 quint.4    <NA> 
##    5262    8296    6427    6999    6793    6408       0
```

### Education


```r
table(fdat$edu,useNA="always")
```

```
## 
## 1. <LS  2. LS 3. LUS 4. UUS  5. AV  6. BA  7. MA   <NA> 
##   4085   6760   7213   7094   5671   4366   4730    266
```

```r
fdat$edu.f<-relevel(as.factor(fdat$edu),ref="7. MA")
table(fdat$edu.f,useNA="always")
```

```
## 
##  7. MA 1. <LS  2. LS 3. LUS 4. UUS  5. AV  6. BA   <NA> 
##   4730   4085   6760   7213   7094   5671   4366    266
```

### DV


```r
table(fdat$DV,useNA="always")
```

```
## 
##                NN     Used_CAM_ONLY Used_conv_and_CAM    Used_conv_ONLY 
##              6877               503              5100             27705 
##              <NA> 
##                 0
```

```r
fdat$DV.f<-relevel(as.factor(fdat$DV),ref="NN")
table(fdat$DV.f,useNA="always")
```

```
## 
##                NN     Used_CAM_ONLY Used_conv_and_CAM    Used_conv_ONLY 
##              6877               503              5100             27705 
##              <NA> 
##                 0
```

### Strain on health and political orientation

Calculate country means for centering


```r
# Calculate country means for centering

cntry.means<-fdat %>%
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

fdat<-left_join(
  x=fdat,
  y=cntry.means,
  by="cntry"
)

#country-mean center strain on health

fdat$strain.on.health.c<-
  fdat$strain.on.health-fdat$strain.on.health.cntry.mean

#country-mean center political orientation

fdat$lrgen.c<-
  fdat$lrgen-fdat$lrgen.cntry.mean
fdat$lrecon.c<-
  fdat$lrecon-fdat$lrecon.cntry.mean
fdat$galtan.c<-
  fdat$galtan-fdat$galtan.cntry.mean
fdat$antielite_salience.c<-
  fdat$antielite_salience-fdat$antielite_salience.cntry.mean
fdat$corrupt_salience.c<-
  fdat$corrupt_salience-fdat$corrupt_salience.cntry.mean

#scale with CHES grand SD
fdat$lrgen.z<-
  fdat$lrgen.c/fdat$lrgen.scaling
fdat$lrecon.z<-
  fdat$lrecon.c/fdat$lrecon.scaling
fdat$galtan.z<-
  fdat$galtan.c/fdat$galtan.scaling
fdat$antielite_salience.z<-
  fdat$antielite_salience.c/fdat$antielite_salience.scaling
fdat$corrupt_salience.z<-
  fdat$corrupt_salience.c/fdat$corrupt_salience.scaling
```

## Exclude missing variable


```r
fdat<-fdat %>%
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

# Descriptive analysis

## Check the presence of all DV-groups across countries


```r
table(fdat$cntry,fdat$DV.f)
```

```
##     
##        NN Used_CAM_ONLY Used_conv_and_CAM Used_conv_ONLY
##   AT  115            18               206            718
##   BE  134             5               143            962
##   CH   89            25               131            397
##   CZ  145             4               160            755
##   DE  155            10               370           1507
##   DK  174             8               150            818
##   ES  106             7               111            807
##   FI  175            12               117           1016
##   FR   66             8               212            651
##   GB  201            10               138            951
##   HU  255             9                55            524
##   IE  255            13               103            903
##   LT  223            34               276            472
##   NL  238            17               111            968
##   NO  163             9               107            772
##   PL  122             6                49            593
##   PT   30             1                46            485
##   SE  312            17               135            947
##   SI   59             8                89            399
```

```r
round(100*prop.table(table(fdat$cntry,fdat$DV.f),
                     margin = 1),1)
```

```
##     
##        NN Used_CAM_ONLY Used_conv_and_CAM Used_conv_ONLY
##   AT 10.9           1.7              19.5           67.9
##   BE 10.8           0.4              11.5           77.3
##   CH 13.9           3.9              20.4           61.8
##   CZ 13.6           0.4              15.0           71.0
##   DE  7.6           0.5              18.1           73.8
##   DK 15.1           0.7              13.0           71.1
##   ES 10.3           0.7              10.8           78.3
##   FI 13.3           0.9               8.9           77.0
##   FR  7.0           0.9              22.6           69.5
##   GB 15.5           0.8              10.6           73.2
##   HU 30.2           1.1               6.5           62.2
##   IE 20.0           1.0               8.1           70.9
##   LT 22.2           3.4              27.5           47.0
##   NL 17.8           1.3               8.3           72.6
##   NO 15.5           0.9              10.2           73.5
##   PL 15.8           0.8               6.4           77.0
##   PT  5.3           0.2               8.2           86.3
##   SE 22.1           1.2               9.6           67.1
##   SI 10.6           1.4              16.0           71.9
```


# Analysis

## Empty model


```r
mod0<-mblogit(DV.f~1,
              random= ~1|cntry,
              estimator="ML",
              data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 30927.53 - criterion = 0.7408656
## Iteration 2 - deviance = 29867.42 - criterion = 0.2229412
## Iteration 3 - deviance = 29750.33 - criterion = 0.02726584
## Iteration 4 - deviance = 29734.34 - criterion = 0.004885409
## Iteration 5 - deviance = 29732.04 - criterion = 0.0001791504
## Iteration 6 - deviance = 29731.82 - criterion = 2.609763e-06
## Iteration 7 - deviance = 29731.84 - criterion = 6.826304e-08
## Iteration 8 - deviance = 29731.85 - criterion = 2.929665e-09
## converged
```

```r
mod0
```

```
## mblogit(formula = DV.f ~ 1, data = fdat, random = ~1 | cntry, 
##     weights = anweight, estimator = "ML")
## 
## Coefficients:
##                       Predictors
## Response categories     (Intercept)
##   Used_CAM_ONLY/NN      -2.6875    
##   Used_conv_and_CAM/NN  -0.1656    
##   Used_conv_ONLY/NN      1.5966    
## 
## (Co-)Variances:
## Grouping level: cntry 
##                      Used_CAM_ONLY~1  Used_conv_and_CAM~1  Used_conv_ONLY~1
## Used_CAM_ONLY~1       0.36106                                              
## Used_conv_and_CAM~1   0.20056          0.51447                             
## Used_conv_ONLY~1     -0.03003          0.23201              0.23705        
## 
## Null Deviance:     52210 
## Residual Deviance: 29730
```

```r
summary(mod0)
```

```
## 
## Call:
## mblogit(formula = DV.f ~ 1, data = fdat, random = ~1 | cntry, 
##     weights = anweight, estimator = "ML")
## 
## Equation for Used_CAM_ONLY vs NN:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -2.6875     0.1716  -15.66   <2e-16 ***
## 
## Equation for Used_conv_and_CAM vs NN:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)  -0.1656     0.1712  -0.967    0.333
## 
## Equation for Used_conv_ONLY vs NN:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   1.5966     0.1169   13.66   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Co-)Variances:
## Grouping level: cntry 
##                     Estimate                     Std.Err.               
## Used_CAM_ONLY~1      0.36106                     0.03253                
## Used_conv_and_CAM~1  0.20056  0.51447            0.04895 0.08778        
## Used_conv_ONLY~1    -0.03003  0.23201  0.23705   0.02127 0.04186 0.02168
## 
## Null Deviance:     52210 
## Residual Deviance: 29730 
## Number of Fisher Scoring iterations:  8
## Number of observations
##   Groups by cntry: 19
##   Individual observations:  18832.14
```

```r
mtable(mod0,show.baselevel = T)
```

```
## 
## Calls:
## mod0: mblogit(formula = DV.f ~ 1, data = fdat, random = ~1 | cntry, 
##     weights = anweight, estimator = "ML")
## 
## ================================================================================================
##                                        Used_CAM_ONLY/NN Used_conv_and_CAM/NN Used_conv_ONLY/NN  
## ------------------------------------------------------------------------------------------------
##   (Intercept)                               -2.688***         -0.166              1.597***      
##                                             (0.172)           (0.171)            (0.117)        
## ------------------------------------------------------------------------------------------------
##   Used_CAM_ONLY/NN x  VCov(~1,~1)            0.361             0.201             -0.030         
##                                             (0.033)           (0.049)            (0.021)        
##   Used_conv_and_CAM/NN x  VCov(~1,~1)        0.201             0.514              0.232         
##                                             (0.049)           (0.088)            (0.042)        
##   Used_conv_ONLY/NN x  VCov(~1,~1)          -0.030             0.232              0.237         
##                                             (0.021)           (0.042)            (0.022)        
## ------------------------------------------------------------------------------------------------
##   Groups by cntry                           19                                                  
## ------------------------------------------------------------------------------------------------
##   Deviance                               29731.8                                                
##   N                                      18832                                                  
## ================================================================================================
##   Significance: *** = p < 0.001; ** = p < 0.01; * = p < 0.05
```

## Model with covariates


```r
mod1<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+blgetmg.c+mstatus.c+
            strain.on.health.c,
              random= ~1|cntry,
              estimator="ML",
              data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28766.3 - criterion = 0.7764753
## Iteration 2 - deviance = 27488.05 - criterion = 0.1511482
## Iteration 3 - deviance = 27325.05 - criterion = 0.02516423
## Iteration 4 - deviance = 27289.36 - criterion = 0.01440642
## Iteration 5 - deviance = 27282.96 - criterion = 0.004346864
## Iteration 6 - deviance = 27281.76 - criterion = 0.0007230438
## Iteration 7 - deviance = 27281.57 - criterion = 5.982783e-05
## Iteration 8 - deviance = 27281.42 - criterion = 1.731451e-06
## Iteration 9 - deviance = 27281.39 - criterion = 2.356374e-08
## Iteration 10 - deviance = 27281.38 - criterion = 7.084194e-10
## converged
```

```r
summary(mod1)
```

```
## 
## Call:
## mblogit(formula = DV.f ~ gndr.c + age10.c + income.fr + edu.f + 
##     blgetmg.c + mstatus.c + strain.on.health.c, data = fdat, 
##     random = ~1 | cntry, weights = anweight, estimator = "ML")
## 
## Equation for Used_CAM_ONLY vs NN:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -1.68935    0.35144  -4.807 1.53e-06 ***
## gndr.c              0.35349    0.16907   2.091  0.03655 *  
## age10.c            -0.06109    0.05937  -1.029  0.30352    
## income.frmissing   -0.99036    0.38946  -2.543  0.01099 *  
## income.frquint.1   -0.27425    0.39870  -0.688  0.49155    
## income.frquint.2    0.07887    0.28096   0.281  0.77893    
## income.frquint.3    0.00737    0.25052   0.029  0.97653    
## income.frquint.4   -0.21579    0.23656  -0.912  0.36167    
## edu.f1. <LS        -2.81639    0.86451  -3.258  0.00112 ** 
## edu.f2. LS         -1.21972    0.30967  -3.939 8.19e-05 ***
## edu.f3. LUS        -1.23876    0.27528  -4.500 6.79e-06 ***
## edu.f4. UUS        -1.35763    0.32317  -4.201 2.66e-05 ***
## edu.f5. AV         -0.62099    0.25526  -2.433  0.01498 *  
## edu.f6. BA         -0.76221    0.27670  -2.755  0.00588 ** 
## blgetmg.c          -0.41629    0.48730  -0.854  0.39295    
## mstatus.c           0.43780    0.21000   2.085  0.03710 *  
## strain.on.health.c  0.26709    0.04717   5.662 1.50e-08 ***
## 
## Equation for Used_conv_and_CAM vs NN:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         0.60448    0.21586   2.800  0.00511 ** 
## gndr.c              1.17819    0.06324  18.629  < 2e-16 ***
## age10.c             0.08762    0.02017   4.343 1.40e-05 ***
## income.frmissing   -0.22261    0.11803  -1.886  0.05928 .  
## income.frquint.1   -0.06268    0.12771  -0.491  0.62357    
## income.frquint.2   -0.06520    0.10793  -0.604  0.54580    
## income.frquint.3   -0.05823    0.10047  -0.580  0.56219    
## income.frquint.4    0.02469    0.09332   0.265  0.79135    
## edu.f1. <LS        -1.12425    0.15484  -7.261 3.84e-13 ***
## edu.f2. LS         -0.63326    0.11547  -5.484 4.16e-08 ***
## edu.f3. LUS        -0.48698    0.10461  -4.655 3.24e-06 ***
## edu.f4. UUS        -0.44433    0.11670  -3.807  0.00014 ***
## edu.f5. AV         -0.29500    0.11074  -2.664  0.00772 ** 
## edu.f6. BA         -0.25017    0.11922  -2.098  0.03587 *  
## blgetmg.c           0.18609    0.16071   1.158  0.24690    
## mstatus.c           0.07332    0.07090   1.034  0.30108    
## strain.on.health.c  0.54383    0.01748  31.110  < 2e-16 ***
## 
## Equation for Used_conv_ONLY vs NN:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         2.017421   0.160231  12.591  < 2e-16 ***
## gndr.c              0.510957   0.048578  10.518  < 2e-16 ***
## age10.c             0.188110   0.015722  11.964  < 2e-16 ***
## income.frmissing   -0.114585   0.085232  -1.344  0.17882    
## income.frquint.1    0.132672   0.099530   1.333  0.18254    
## income.frquint.2    0.037364   0.084396   0.443  0.65797    
## income.frquint.3    0.009059   0.077591   0.117  0.90706    
## income.frquint.4   -0.014913   0.072957  -0.204  0.83804    
## edu.f1. <LS        -0.067582   0.114309  -0.591  0.55437    
## edu.f2. LS         -0.112461   0.088467  -1.271  0.20365    
## edu.f3. LUS        -0.141273   0.083476  -1.692  0.09058 .  
## edu.f4. UUS        -0.051393   0.088986  -0.578  0.56357    
## edu.f5. AV         -0.121444   0.089454  -1.358  0.17459    
## edu.f6. BA         -0.044191   0.092362  -0.478  0.63233    
## blgetmg.c           0.109530   0.121635   0.900  0.36786    
## mstatus.c           0.152115   0.054656   2.783  0.00538 ** 
## strain.on.health.c  0.412635   0.015914  25.929  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Co-)Variances:
## Grouping level: cntry 
##                     Estimate                     Std.Err.               
## Used_CAM_ONLY~1      0.41061                     0.02726                
## Used_conv_and_CAM~1  0.06805  0.57794            0.03934 0.10999        
## Used_conv_ONLY~1    -0.07792  0.29556  0.29814   0.02479 0.06855 0.04460
## 
## Null Deviance:     52210 
## Residual Deviance: 27280 
## Number of Fisher Scoring iterations:  10
## Number of observations
##   Groups by cntry: 19
##   Individual observations:  18832.14
```

```r
mod1
```

```
## mblogit(formula = DV.f ~ gndr.c + age10.c + income.fr + edu.f + 
##     blgetmg.c + mstatus.c + strain.on.health.c, data = fdat, 
##     random = ~1 | cntry, weights = anweight, estimator = "ML")
## 
## Coefficients:
##                       Predictors
## Response categories     (Intercept)  gndr.c     age10.c    income.frmissing
##   Used_CAM_ONLY/NN      -1.689353     0.353488  -0.061085  -0.990360       
##   Used_conv_and_CAM/NN   0.604476     1.178190   0.087625  -0.222608       
##   Used_conv_ONLY/NN      2.017421     0.510957   0.188110  -0.114585       
##                       Predictors
## Response categories     income.frquint.1  income.frquint.2  income.frquint.3
##   Used_CAM_ONLY/NN      -0.274250          0.078871          0.007370       
##   Used_conv_and_CAM/NN  -0.062681         -0.065199         -0.058233       
##   Used_conv_ONLY/NN      0.132672          0.037364          0.009059       
##                       Predictors
## Response categories     income.frquint.4  edu.f1. <LS  edu.f2. LS  edu.f3. LUS
##   Used_CAM_ONLY/NN      -0.215789         -2.816388    -1.219715   -1.238756  
##   Used_conv_and_CAM/NN   0.024687         -1.124250    -0.633258   -0.486976  
##   Used_conv_ONLY/NN     -0.014913         -0.067582    -0.112461   -0.141273  
##                       Predictors
## Response categories     edu.f4. UUS  edu.f5. AV  edu.f6. BA  blgetmg.c
##   Used_CAM_ONLY/NN      -1.357630    -0.620994   -0.762210   -0.416290
##   Used_conv_and_CAM/NN  -0.444331    -0.295003   -0.250168    0.186088
##   Used_conv_ONLY/NN     -0.051393    -0.121444   -0.044191    0.109530
##                       Predictors
## Response categories     mstatus.c  strain.on.health.c
##   Used_CAM_ONLY/NN       0.437797   0.267085         
##   Used_conv_and_CAM/NN   0.073322   0.543826         
##   Used_conv_ONLY/NN      0.152115   0.412635         
## 
## (Co-)Variances:
## Grouping level: cntry 
##                      Used_CAM_ONLY~1  Used_conv_and_CAM~1  Used_conv_ONLY~1
## Used_CAM_ONLY~1       0.41061                                              
## Used_conv_and_CAM~1   0.06805          0.57794                             
## Used_conv_ONLY~1     -0.07792          0.29556              0.29814        
## 
## Null Deviance:     52210 
## Residual Deviance: 27280
```

```r
mtable(mod1,show.baselevel = T)
```

```
## 
## Calls:
## mod1: mblogit(formula = DV.f ~ gndr.c + age10.c + income.fr + edu.f + 
##     blgetmg.c + mstatus.c + strain.on.health.c, data = fdat, 
##     random = ~1 | cntry, weights = anweight, estimator = "ML")
## 
## ================================================================================================
##                                        Used_CAM_ONLY/NN Used_conv_and_CAM/NN Used_conv_ONLY/NN  
## ------------------------------------------------------------------------------------------------
##   (Intercept)                               -1.689***         0.604**             2.017***      
##                                             (0.351)          (0.216)             (0.160)        
##   gndr.c                                     0.353*           1.178***            0.511***      
##                                             (0.169)          (0.063)             (0.049)        
##   age10.c                                   -0.061            0.088***            0.188***      
##                                             (0.059)          (0.020)             (0.016)        
##   income.fr: missing/quint.5                -0.990*          -0.223              -0.115         
##                                             (0.389)          (0.118)             (0.085)        
##   income.fr: quint.1/quint.5                -0.274           -0.063               0.133         
##                                             (0.399)          (0.128)             (0.100)        
##   income.fr: quint.2/quint.5                 0.079           -0.065               0.037         
##                                             (0.281)          (0.108)             (0.084)        
##   income.fr: quint.3/quint.5                 0.007           -0.058               0.009         
##                                             (0.251)          (0.100)             (0.078)        
##   income.fr: quint.4/quint.5                -0.216            0.025              -0.015         
##                                             (0.237)          (0.093)             (0.073)        
##   edu.f: 1. <LS/7. MA                       -2.816**         -1.124***           -0.068         
##                                             (0.865)          (0.155)             (0.114)        
##   edu.f: 2. LS/7. MA                        -1.220***        -0.633***           -0.112         
##                                             (0.310)          (0.115)             (0.088)        
##   edu.f: 3. LUS/7. MA                       -1.239***        -0.487***           -0.141         
##                                             (0.275)          (0.105)             (0.083)        
##   edu.f: 4. UUS/7. MA                       -1.358***        -0.444***           -0.051         
##                                             (0.323)          (0.117)             (0.089)        
##   edu.f: 5. AV/7. MA                        -0.621*          -0.295**            -0.121         
##                                             (0.255)          (0.111)             (0.089)        
##   edu.f: 6. BA/7. MA                        -0.762**         -0.250*             -0.044         
##                                             (0.277)          (0.119)             (0.092)        
##   blgetmg.c                                 -0.416            0.186               0.110         
##                                             (0.487)          (0.161)             (0.122)        
##   mstatus.c                                  0.438*           0.073               0.152**       
##                                             (0.210)          (0.071)             (0.055)        
##   strain.on.health.c                         0.267***         0.544***            0.413***      
##                                             (0.047)          (0.017)             (0.016)        
## ------------------------------------------------------------------------------------------------
##   Used_CAM_ONLY/NN x  VCov(~1,~1)            0.411            0.068              -0.078         
##                                             (0.027)          (0.039)             (0.025)        
##   Used_conv_and_CAM/NN x  VCov(~1,~1)        0.068            0.578               0.296         
##                                             (0.039)          (0.110)             (0.069)        
##   Used_conv_ONLY/NN x  VCov(~1,~1)          -0.078            0.296               0.298         
##                                             (0.025)          (0.069)             (0.045)        
## ------------------------------------------------------------------------------------------------
##   Groups by cntry                           19                                                  
## ------------------------------------------------------------------------------------------------
##   Deviance                               27281.4                                                
##   N                                      18832                                                  
## ================================================================================================
##   Significance: *** = p < 0.001; ** = p < 0.01; * = p < 0.05
```

### Strain on health main effects


```r
mod1.strain.trends<-
  emtrends(mod1,~1|DV.f,
           var="strain.on.health.c",
           infer=T,mode="latent",
           at=list(gndr.c=0,age10.c=0))

#effects for each DV-category
(mod1.strain.eff<-
  contrast(mod1.strain.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))
```

```
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                 -0.3059 0.0162 Inf   -0.3376   -0.2742 -18.903
##  Used_CAM_ONLY effect      -0.0388 0.0338 Inf   -0.1051    0.0275  -1.147
##  Used_conv_and_CAM effect   0.2379 0.0133 Inf    0.2119    0.2640  17.899
##  Used_conv_ONLY effect      0.1067 0.0124 Inf    0.0825    0.1310   8.642
##  p.value
##   <.0001
##   0.2516
##   <.0001
##   <.0001
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod1.strain.eff),
       "../../results/Added_covariates/mod1.strain.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod1.strain.eff.CM<-
  contrast(mod1.strain.trends,
           method = list("Conv - No conv" = contrast.weights.total(effects=mod1.strain.eff,
          signs=c(-2,-2,2,2))),
           simple="DV.f",
           infer=c(T,T)))
```

```
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.384 0.0186 Inf     0.348     0.421  20.642  <.0001
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
(mod1.strain.eff.CAM<-
    contrast(mod1.strain.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod1.strain.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f",
           infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    0.211 0.0128 Inf     0.186     0.236  16.525  <.0001
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(mod1.strain.eff.CM,
      mod1.strain.eff.CAM,adjust="none")),
      "../../results/Added_covariates/mod1.strain.eff.COMB.xlsx")
```

### gender main effects


```r
mod1.gndr.trends<-
  emtrends(mod1,~1|DV.f,
           var="gndr.c",
           infer=T,mode="latent",
           at=list(strain.on.health.c=0,age10.c=0))

#effects for each DV-category
(mod1.gndr.eff<-
  contrast(mod1.gndr.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))
```

```
##  contrast                  estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                -0.510659 0.0542 Inf   -0.6169   -0.4044  -9.422
##  Used_CAM_ONLY effect     -0.157171 0.1236 Inf   -0.3993    0.0850  -1.272
##  Used_conv_and_CAM effect  0.667532 0.0537 Inf    0.5622    0.7728  12.426
##  Used_conv_ONLY effect     0.000298 0.0459 Inf   -0.0896    0.0902   0.006
##  p.value
##   <.0001
##   0.2034
##   <.0001
##   0.9948
## 
## Results are averaged over the levels of: gndr.c, income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod1.gndr.eff),
       "../../results/Added_covariates/mod1.gndr.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod1.gndr.eff.CM<-
  contrast(mod1.gndr.trends,
           method = list("Conv - No conv" = contrast.weights.total(effects=mod1.gndr.eff,
          signs=c(-2,-2,2,2))),
           simple="DV.f",
           infer=c(T,T)))
```

```
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.677 0.0599 Inf     0.559     0.794  11.295  <.0001
## 
## Results are averaged over the levels of: gndr.c, income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
(mod1.gndr.eff.CAM<-
    contrast(mod1.gndr.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod1.gndr.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f",
           infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    0.643 0.0582 Inf     0.529     0.757  11.044  <.0001
## 
## Results are averaged over the levels of: gndr.c, income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(mod1.gndr.eff.CM,
      mod1.gndr.eff.CAM,adjust="none")),
      "../../results/Added_covariates/mod1.gndr.eff.COMB.xlsx")
```


### age main effects


```r
mod1.age10.trends<-
  emtrends(mod1,~1|DV.f,
           var="age10.c",
           infer=T,mode="latent",
           at=list(strain.on.health.c=0,gndr.c=0))

#effects for each DV-category
(mod1.age10.eff<-
  contrast(mod1.age10.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))
```

```
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                 -0.0537 0.0184 Inf  -0.08982   -0.0175  -2.909
##  Used_CAM_ONLY effect      -0.1147 0.0436 Inf  -0.20012   -0.0294  -2.634
##  Used_conv_and_CAM effect   0.0340 0.0181 Inf  -0.00153    0.0695   1.876
##  Used_conv_ONLY effect      0.1344 0.0159 Inf   0.10333    0.1656   8.468
##  p.value
##   0.0036
##   0.0084
##   0.0607
##   <.0001
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod1.age10.eff),
       "../../results/Added_covariates/mod1.age10.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod1.age10.eff.CM<-
  contrast(mod1.age10.trends,
           method = list("Conv - No conv" = contrast.weights.total(effects=mod1.age10.eff,
          signs=c(-2,-2,2,2))),
           simple="DV.f",
           infer=c(T,T)))
```

```
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.163 0.0202 Inf     0.124     0.203   8.069  <.0001
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
(mod1.age10.eff.CAM<-
    contrast(mod1.age10.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod1.age10.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f",
           infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM  -0.0635 0.0192 Inf    -0.101   -0.0258  -3.300  0.0010
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(mod1.age10.eff.CM,
      mod1.age10.eff.CAM,adjust="none")),
      "../../results/Added_covariates/mod1.age10.eff.COMB.xlsx")
```


### education main effects


```r
# first take probabilities for all categories
mod1.edu.f.trends<-emmeans(mod1,by="DV.f",
        specs="edu.f",
        infer=T,mode="prob",
        at=list(strain.on.health.c=0,gndr.c=0,age10.c=0))

mod1.edu.f.trends
```

```
## DV.f = NN:
##  edu.f     prob       SE  df asymp.LCL asymp.UCL z.ratio p.value
##  7. MA  0.09603 0.013324 Inf  0.069915   0.12214   7.207  <.0001
##  1. <LS 0.11607 0.017098 Inf  0.082560   0.14958   6.789  <.0001
##  2. LS  0.11498 0.015572 Inf  0.084461   0.14550   7.384  <.0001
##  3. LUS 0.11563 0.015452 Inf  0.085343   0.14591   7.483  <.0001
##  4. UUS 0.10748 0.014790 Inf  0.078494   0.13647   7.267  <.0001
##  5. AV  0.11060 0.015165 Inf  0.080879   0.14033   7.293  <.0001
##  6. BA  0.10390 0.014591 Inf  0.075304   0.13250   7.121  <.0001
## 
## DV.f = Used_CAM_ONLY:
##  edu.f     prob       SE  df asymp.LCL asymp.UCL z.ratio p.value
##  7. MA  0.01530 0.004714 Inf  0.006056   0.02454   3.244  0.0012
##  1. <LS 0.00111 0.000975 Inf -0.000803   0.00302   1.136  0.2558
##  2. LS  0.00542 0.002013 Inf  0.001470   0.00936   2.690  0.0071
##  3. LUS 0.00535 0.001863 Inf  0.001693   0.00900   2.868  0.0041
##  4. UUS 0.00441 0.001722 Inf  0.001037   0.00779   2.562  0.0104
##  5. AV  0.00948 0.003163 Inf  0.003278   0.01568   2.996  0.0027
##  6. BA  0.00773 0.002750 Inf  0.002342   0.01312   2.812  0.0049
## 
## DV.f = Used_conv_and_CAM:
##  edu.f     prob       SE  df asymp.LCL asymp.UCL z.ratio p.value
##  7. MA  0.16446 0.020466 Inf  0.124345   0.20457   8.036  <.0001
##  1. <LS 0.06463 0.010320 Inf  0.044405   0.08486   6.263  <.0001
##  2. LS  0.10459 0.014338 Inf  0.076487   0.13269   7.294  <.0001
##  3. LUS 0.12174 0.015997 Inf  0.090381   0.15309   7.610  <.0001
##  4. UUS 0.11808 0.016114 Inf  0.086495   0.14966   7.328  <.0001
##  5. AV  0.14107 0.018269 Inf  0.105260   0.17687   7.722  <.0001
##  6. BA  0.13859 0.018567 Inf  0.102198   0.17498   7.464  <.0001
## 
## DV.f = Used_conv_ONLY:
##  edu.f     prob       SE  df asymp.LCL asymp.UCL z.ratio p.value
##  7. MA  0.72422 0.022847 Inf  0.679438   0.76900  31.698  <.0001
##  1. <LS 0.81819 0.018706 Inf  0.781527   0.85485  43.740  <.0001
##  2. LS  0.77501 0.019513 Inf  0.736768   0.81326  39.718  <.0001
##  3. LUS 0.75729 0.020154 Inf  0.717789   0.79679  37.574  <.0001
##  4. UUS 0.77003 0.019970 Inf  0.730888   0.80917  38.559  <.0001
##  5. AV  0.73886 0.021710 Inf  0.696304   0.78141  34.033  <.0001
##  6. BA  0.74978 0.021656 Inf  0.707332   0.79222  34.623  <.0001
## 
## Results are averaged over the levels of: income.fr, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
export(data.frame(mod1.edu.f.trends),
       "../../results/Added_covariates/mod1.edu.f.eff.MN.xlsx",
       overwrite=T)


#effects for each DV-category
(mod1.edu.f.eff<-
    contrast(mod1.edu.f.trends,#simple="edu.f",
             adjust="none","eff",infer=c(T,T)))
```

```
## DV.f = NN:
##  contrast       estimate      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  7. MA effect  -0.013214 0.00547 Inf -0.023934 -0.002493  -2.416  0.0157
##  1. <LS effect  0.006829 0.00852 Inf -0.009868  0.023525   0.802  0.4228
##  2. LS effect   0.005739 0.00578 Inf -0.005581  0.017060   0.994  0.3204
##  3. LUS effect  0.006387 0.00534 Inf -0.004088  0.016862   1.195  0.2321
##  4. UUS effect -0.001761 0.00569 Inf -0.012915  0.009392  -0.310  0.7569
##  5. AV effect   0.001360 0.00581 Inf -0.010029  0.012749   0.234  0.8150
##  6. BA effect  -0.005339 0.00601 Inf -0.017121  0.006443  -0.888  0.3744
## 
## DV.f = Used_CAM_ONLY:
##  contrast       estimate      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  7. MA effect   0.008326 0.00300 Inf  0.002445  0.014207   2.775  0.0055
##  1. <LS effect -0.005861 0.00189 Inf -0.009565 -0.002157  -3.101  0.0019
##  2. LS effect  -0.001553 0.00139 Inf -0.004285  0.001179  -1.114  0.2652
##  3. LUS effect -0.001624 0.00123 Inf -0.004037  0.000789  -1.319  0.1871
##  4. UUS effect -0.002558 0.00137 Inf -0.005242  0.000127  -1.867  0.0618
##  5. AV effect   0.002508 0.00178 Inf -0.000988  0.006004   1.406  0.1597
##  6. BA effect   0.000762 0.00160 Inf -0.002370  0.003895   0.477  0.6333
## 
## DV.f = Used_conv_and_CAM:
##  contrast       estimate      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  7. MA effect   0.042580 0.00805 Inf  0.026809  0.058350   5.292  <.0001
##  1. <LS effect -0.057246 0.00852 Inf -0.073946 -0.040547  -6.719  <.0001
##  2. LS effect  -0.017289 0.00578 Inf -0.028616 -0.005961  -2.991  0.0028
##  3. LUS effect -0.000143 0.00507 Inf -0.010074  0.009788  -0.028  0.9775
##  4. UUS effect -0.003800 0.00652 Inf -0.016588  0.008988  -0.582  0.5603
##  5. AV effect   0.019187 0.00655 Inf  0.006344  0.032031   2.928  0.0034
##  6. BA effect   0.016710 0.00778 Inf  0.001469  0.031952   2.149  0.0316
## 
## DV.f = Used_conv_ONLY:
##  contrast       estimate      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  7. MA effect  -0.037692 0.00920 Inf -0.055728 -0.019656  -4.096  <.0001
##  1. <LS effect  0.056279 0.01121 Inf  0.034318  0.078241   5.023  <.0001
##  2. LS effect   0.013102 0.00759 Inf -0.001769  0.027974   1.727  0.0842
##  3. LUS effect -0.004620 0.00682 Inf -0.017977  0.008738  -0.678  0.4979
##  4. UUS effect  0.008119 0.00814 Inf -0.007833  0.024071   0.998  0.3185
##  5. AV effect  -0.023055 0.00812 Inf -0.038977 -0.007133  -2.838  0.0045
##  6. BA effect  -0.012134 0.00921 Inf -0.030194  0.005927  -1.317  0.1879
## 
## Results are averaged over the levels of: income.fr, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod1.edu.f.eff),
       "../../results/Added_covariates/mod1.edu.f.eff.MN.xlsx",
       overwrite=T)

# No Conv versus Conv

(mod1.edu.f.eff_DV<-
    contrast(mod1.edu.f.trends,simple="DV.f",
             adjust="none","eff",infer=c(T,T)))
```

```
## edu.f = 7. MA:
##  contrast                 estimate       SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                 -0.1540 0.013324 Inf    -0.180   -0.1279  -11.556
##  Used_CAM_ONLY effect      -0.2347 0.004714 Inf    -0.244   -0.2255  -49.785
##  Used_conv_and_CAM effect  -0.0855 0.020466 Inf    -0.126   -0.0454   -4.180
##  Used_conv_ONLY effect      0.4742 0.022847 Inf     0.429    0.5190   20.756
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## edu.f = 1. <LS:
##  contrast                 estimate       SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                 -0.1339 0.017098 Inf    -0.167   -0.1004   -7.833
##  Used_CAM_ONLY effect      -0.2489 0.000975 Inf    -0.251   -0.2470 -255.278
##  Used_conv_and_CAM effect  -0.1854 0.010320 Inf    -0.206   -0.1651  -17.962
##  Used_conv_ONLY effect      0.5682 0.018706 Inf     0.532    0.6049   30.375
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## edu.f = 2. LS:
##  contrast                 estimate       SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                 -0.1350 0.015572 Inf    -0.166   -0.1045   -8.671
##  Used_CAM_ONLY effect      -0.2446 0.002013 Inf    -0.249   -0.2406 -121.489
##  Used_conv_and_CAM effect  -0.1454 0.014338 Inf    -0.174   -0.1173  -10.141
##  Used_conv_ONLY effect      0.5250 0.019513 Inf     0.487    0.5633   26.906
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## edu.f = 3. LUS:
##  contrast                 estimate       SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                 -0.1344 0.015452 Inf    -0.165   -0.1041   -8.696
##  Used_CAM_ONLY effect      -0.2447 0.001863 Inf    -0.248   -0.2410 -131.289
##  Used_conv_and_CAM effect  -0.1283 0.015997 Inf    -0.160   -0.0969   -8.018
##  Used_conv_ONLY effect      0.5073 0.020154 Inf     0.468    0.5468   25.170
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## edu.f = 4. UUS:
##  contrast                 estimate       SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                 -0.1425 0.014790 Inf    -0.172   -0.1135   -9.636
##  Used_CAM_ONLY effect      -0.2456 0.001722 Inf    -0.249   -0.2422 -142.648
##  Used_conv_and_CAM effect  -0.1319 0.016114 Inf    -0.164   -0.1003   -8.187
##  Used_conv_ONLY effect      0.5200 0.019970 Inf     0.481    0.5592   26.040
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## edu.f = 5. AV:
##  contrast                 estimate       SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                 -0.1394 0.015165 Inf    -0.169   -0.1097   -9.192
##  Used_CAM_ONLY effect      -0.2405 0.003163 Inf    -0.247   -0.2343  -76.042
##  Used_conv_and_CAM effect  -0.1089 0.018269 Inf    -0.145   -0.0731   -5.963
##  Used_conv_ONLY effect      0.4889 0.021710 Inf     0.446    0.5314   22.517
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## edu.f = 6. BA:
##  contrast                 estimate       SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                 -0.1461 0.014591 Inf    -0.175   -0.1175  -10.013
##  Used_CAM_ONLY effect      -0.2423 0.002750 Inf    -0.248   -0.2369  -88.108
##  Used_conv_and_CAM effect  -0.1114 0.018567 Inf    -0.148   -0.0750   -6.000
##  Used_conv_ONLY effect      0.4998 0.021656 Inf     0.457    0.5422   23.078
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## Results are averaged over the levels of: income.fr, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
(mod1.edu.f.eff.CM<-
    contrast(mod1.edu.f.trends,
             method = 
               list("Conv - No conv" = c(0,0,1,1)),
             simple="DV.f",
             infer=c(T,T)))
```

```
## edu.f = 7. MA:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.889 0.0156 Inf     0.858     0.919  57.020  <.0001
## 
## edu.f = 1. <LS:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.883 0.0172 Inf     0.849     0.917  51.206  <.0001
## 
## edu.f = 2. LS:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.880 0.0163 Inf     0.848     0.912  53.987  <.0001
## 
## edu.f = 3. LUS:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.879 0.0162 Inf     0.847     0.911  54.407  <.0001
## 
## edu.f = 4. UUS:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.888 0.0154 Inf     0.858     0.918  57.766  <.0001
## 
## edu.f = 5. AV:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.880 0.0165 Inf     0.848     0.912  53.455  <.0001
## 
## edu.f = 6. BA:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.888 0.0157 Inf     0.858     0.919  56.758  <.0001
## 
## Results are averaged over the levels of: income.fr, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
(mod1.edu.f.eff.CM_comp<-
  contrast(mod1.edu.f.eff.CM,
         simple="edu.f",method="eff",
         infer=c(T,T),adjust="none"))
```

```
## contrast = Conv - No conv:
##  contrast1      estimate      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  7. MA effect   0.004887 0.00587 Inf  -0.00661   0.01638   0.833  0.4047
##  1. <LS effect -0.000967 0.00860 Inf  -0.01782   0.01589  -0.112  0.9105
##  2. LS effect  -0.004186 0.00590 Inf  -0.01576   0.00739  -0.709  0.4784
##  3. LUS effect -0.004762 0.00545 Inf  -0.01545   0.00592  -0.874  0.3823
##  4. UUS effect  0.004319 0.00586 Inf  -0.00717   0.01581   0.737  0.4612
##  5. AV effect  -0.003868 0.00606 Inf  -0.01574   0.00801  -0.638  0.5232
##  6. BA effect   0.004577 0.00621 Inf  -0.00760   0.01675   0.737  0.4613
## 
## Results are averaged over the levels of: income.fr, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# No CAM versus CAM


(mod1.edu.f.eff.CAM<-
    contrast(mod1.edu.f.trends,
             method = 
               list("CAM - No CAM" = c(0,1,1,0)),
             simple="DV.f",
             infer=c(T,T)))
```

```
## edu.f = 7. MA:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM   0.1798 0.0211 Inf    0.1383    0.2212   8.504  <.0001
## 
## edu.f = 1. <LS:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM   0.0657 0.0104 Inf    0.0454    0.0861   6.326  <.0001
## 
## edu.f = 2. LS:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM   0.1100 0.0146 Inf    0.0814    0.1386   7.539  <.0001
## 
## edu.f = 3. LUS:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM   0.1271 0.0162 Inf    0.0953    0.1589   7.838  <.0001
## 
## edu.f = 4. UUS:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM   0.1225 0.0163 Inf    0.0906    0.1544   7.517  <.0001
## 
## edu.f = 5. AV:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM   0.1505 0.0187 Inf    0.1139    0.1871   8.062  <.0001
## 
## edu.f = 6. BA:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM   0.1463 0.0189 Inf    0.1093    0.1833   7.747  <.0001
## 
## Results are averaged over the levels of: income.fr, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
(mod1.edu.f.eff.CAM_comp<-
    contrast(mod1.edu.f.eff.CAM,
             simple="edu.f",method="eff",
             infer=c(T,T),adjust="none"))
```

```
## contrast = CAM - No CAM:
##  contrast1     estimate      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  7. MA effect   0.05091 0.00849 Inf   0.03426   0.06755   5.993  <.0001
##  1. <LS effect -0.06311 0.00877 Inf  -0.08030  -0.04592  -7.195  <.0001
##  2. LS effect  -0.01884 0.00591 Inf  -0.03042  -0.00726  -3.190  0.0014
##  3. LUS effect -0.00177 0.00518 Inf  -0.01192   0.00839  -0.341  0.7331
##  4. UUS effect -0.00636 0.00663 Inf  -0.01935   0.00664  -0.959  0.3376
##  5. AV effect   0.02170 0.00674 Inf   0.00848   0.03491   3.218  0.0013
##  6. BA effect   0.01747 0.00790 Inf   0.00200   0.03295   2.213  0.0269
## 
## Results are averaged over the levels of: income.fr, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(mod1.edu.f.eff.CM_comp,
                        mod1.edu.f.eff.CAM_comp,adjust="none")),
       "../../results/Added_covariates/mod1.edu.f.eff.COMB_comp.xlsx",overwrite=T)

# save to file
export(data.frame(rbind(mod1.edu.f.eff.CM,
                        mod1.edu.f.eff.CAM,adjust="none")),
       "../../results/Added_covariates/mod1.edu.f.eff.COMB.xlsx",overwrite=T)
```


### income main effects


```r
# first take probabilities for all categories
mod1.income.fr.trends<-emmeans(mod1,by="DV.f",
        specs="income.fr",
        infer=T,mode="prob",
        at=list(strain.on.health.c=0,gndr.c=0,age10.c=0))

mod1.income.fr.trends
```

```
## DV.f = NN:
##  income.fr    prob      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  quint.5   0.10876 0.01480 Inf  0.079755   0.13775   7.350  <.0001
##  missing   0.12251 0.01669 Inf  0.089790   0.15523   7.338  <.0001
##  quint.1   0.09911 0.01412 Inf  0.071432   0.12680   7.018  <.0001
##  quint.2   0.10648 0.01440 Inf  0.078258   0.13471   7.394  <.0001
##  quint.3   0.10879 0.01459 Inf  0.080190   0.13738   7.456  <.0001
##  quint.4   0.10980 0.01476 Inf  0.080883   0.13872   7.441  <.0001
## 
## DV.f = Used_CAM_ONLY:
##  income.fr    prob      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  quint.5   0.00836 0.00267 Inf  0.003124   0.01360   3.128  0.0018
##  missing   0.00352 0.00156 Inf  0.000462   0.00657   2.256  0.0240
##  quint.1   0.00582 0.00249 Inf  0.000932   0.01070   2.334  0.0196
##  quint.2   0.00887 0.00300 Inf  0.002989   0.01476   2.956  0.0031
##  quint.3   0.00844 0.00276 Inf  0.003034   0.01384   3.060  0.0022
##  quint.4   0.00681 0.00222 Inf  0.002461   0.01115   3.070  0.0021
## 
## DV.f = Used_conv_and_CAM:
##  income.fr    prob      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  quint.5   0.12924 0.01678 Inf  0.096358   0.16213   7.703  <.0001
##  missing   0.11677 0.01620 Inf  0.085018   0.14851   7.209  <.0001
##  quint.1   0.11083 0.01507 Inf  0.081298   0.14036   7.356  <.0001
##  quint.2   0.11865 0.01551 Inf  0.088253   0.14905   7.650  <.0001
##  quint.3   0.12204 0.01587 Inf  0.090929   0.15315   7.688  <.0001
##  quint.4   0.13374 0.01707 Inf  0.100271   0.16720   7.833  <.0001
## 
## DV.f = Used_conv_ONLY:
##  income.fr    prob      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  quint.5   0.75364 0.02060 Inf  0.713260   0.79401  36.583  <.0001
##  missing   0.75721 0.02093 Inf  0.716183   0.79823  36.176  <.0001
##  quint.1   0.78424 0.01925 Inf  0.746511   0.82197  40.741  <.0001
##  quint.2   0.76599 0.01977 Inf  0.727238   0.80475  38.740  <.0001
##  quint.3   0.76073 0.01999 Inf  0.721556   0.79991  38.058  <.0001
##  quint.4   0.74965 0.02051 Inf  0.709455   0.78985  36.552  <.0001
## 
## Results are averaged over the levels of: edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
export(data.frame(mod1.income.fr.trends),
       "../../results/Added_covariates/mod1.income.fr.eff.MN.xlsx",
       overwrite=T)


#effects for each DV-category
(mod1.income.fr.eff<-
    contrast(mod1.income.fr.trends,#simple="income.fr",
             adjust="none","eff",infer=c(T,T)))
```

```
## DV.f = NN:
##  contrast        estimate      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  quint.5 effect -0.000487 0.00501 Inf -0.010316  0.009341  -0.097  0.9226
##  missing effect  0.013267 0.00629 Inf  0.000945  0.025589   2.110  0.0348
##  quint.1 effect -0.010129 0.00638 Inf -0.022639  0.002381  -1.587  0.1125
##  quint.2 effect -0.002758 0.00531 Inf -0.013174  0.007658  -0.519  0.6038
##  quint.3 effect -0.000455 0.00493 Inf -0.010108  0.009198  -0.092  0.9264
##  quint.4 effect  0.000562 0.00484 Inf -0.008933  0.010056   0.116  0.9077
## 
## DV.f = Used_CAM_ONLY:
##  contrast        estimate      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  quint.5 effect  0.001395 0.00134 Inf -0.001233  0.004023   1.041  0.2981
##  missing effect -0.003453 0.00149 Inf -0.006382 -0.000524  -2.311  0.0208
##  quint.1 effect -0.001152 0.00179 Inf -0.004670  0.002366  -0.642  0.5209
##  quint.2 effect  0.001904 0.00171 Inf -0.001449  0.005257   1.113  0.2658
##  quint.3 effect  0.001468 0.00145 Inf -0.001380  0.004317   1.010  0.3124
##  quint.4 effect -0.000162 0.00118 Inf -0.002469  0.002145  -0.138  0.8904
## 
## DV.f = Used_conv_and_CAM:
##  contrast        estimate      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  quint.5 effect  0.007366 0.00553 Inf -0.003467  0.018199   1.333  0.1826
##  missing effect -0.005112 0.00678 Inf -0.018403  0.008179  -0.754  0.4509
##  quint.1 effect -0.011048 0.00629 Inf -0.023384  0.001288  -1.755  0.0792
##  quint.2 effect -0.003227 0.00521 Inf -0.013443  0.006989  -0.619  0.5358
##  quint.3 effect  0.000163 0.00513 Inf -0.009898  0.010224   0.032  0.9746
##  quint.4 effect  0.011858 0.00537 Inf  0.001340  0.022376   2.210  0.0271
## 
## DV.f = Used_conv_ONLY:
##  contrast        estimate      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  quint.5 effect -0.008274 0.00699 Inf -0.021978  0.005431  -1.183  0.2367
##  missing effect -0.004702 0.00856 Inf -0.021476  0.012072  -0.549  0.5827
##  quint.1 effect  0.022329 0.00836 Inf  0.005936  0.038722   2.670  0.0076
##  quint.2 effect  0.004082 0.00697 Inf -0.009570  0.017733   0.586  0.5579
##  quint.3 effect -0.001177 0.00666 Inf -0.014238  0.011885  -0.177  0.8599
##  quint.4 effect -0.012258 0.00670 Inf -0.025399  0.000884  -1.828  0.0675
## 
## Results are averaged over the levels of: edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod1.income.fr.eff),
       "../../results/Added_covariates/mod1.income.fr.eff.MN.xlsx",
       overwrite=T)

# No Conv versus Conv

(mod1.income.fr.eff_DV<-
    contrast(mod1.income.fr.trends,simple="DV.f",
             adjust="none","eff",infer=c(T,T)))
```

```
## income.fr = quint.5:
##  contrast                 estimate      SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                  -0.141 0.01480 Inf    -0.170   -0.1122   -9.546
##  Used_CAM_ONLY effect       -0.242 0.00267 Inf    -0.247   -0.2364  -90.378
##  Used_conv_and_CAM effect   -0.121 0.01678 Inf    -0.154   -0.0879   -7.197
##  Used_conv_ONLY effect       0.504 0.02060 Inf     0.463    0.5440   24.448
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## income.fr = missing:
##  contrast                 estimate      SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                  -0.127 0.01669 Inf    -0.160   -0.0948   -7.637
##  Used_CAM_ONLY effect       -0.246 0.00156 Inf    -0.250   -0.2434 -158.165
##  Used_conv_and_CAM effect   -0.133 0.01620 Inf    -0.165   -0.1015   -8.225
##  Used_conv_ONLY effect       0.507 0.02093 Inf     0.466    0.5482   24.232
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## income.fr = quint.1:
##  contrast                 estimate      SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                  -0.151 0.01412 Inf    -0.179   -0.1232  -10.683
##  Used_CAM_ONLY effect       -0.244 0.00249 Inf    -0.249   -0.2393  -97.970
##  Used_conv_and_CAM effect   -0.139 0.01507 Inf    -0.169   -0.1096   -9.236
##  Used_conv_ONLY effect       0.534 0.01925 Inf     0.497    0.5720   27.754
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## income.fr = quint.2:
##  contrast                 estimate      SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                  -0.144 0.01440 Inf    -0.172   -0.1153   -9.965
##  Used_CAM_ONLY effect       -0.241 0.00300 Inf    -0.247   -0.2352  -80.320
##  Used_conv_and_CAM effect   -0.131 0.01551 Inf    -0.162   -0.1010   -8.469
##  Used_conv_ONLY effect       0.516 0.01977 Inf     0.477    0.5547   26.096
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## income.fr = quint.3:
##  contrast                 estimate      SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                  -0.141 0.01459 Inf    -0.170   -0.1126   -9.678
##  Used_CAM_ONLY effect       -0.242 0.00276 Inf    -0.247   -0.2362  -87.618
##  Used_conv_and_CAM effect   -0.128 0.01587 Inf    -0.159   -0.0968   -8.061
##  Used_conv_ONLY effect       0.511 0.01999 Inf     0.472    0.5499   25.551
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## income.fr = quint.4:
##  contrast                 estimate      SE  df asymp.LCL asymp.UCL  z.ratio
##  NN effect                  -0.140 0.01476 Inf    -0.169   -0.1113   -9.501
##  Used_CAM_ONLY effect       -0.243 0.00222 Inf    -0.248   -0.2388 -109.668
##  Used_conv_and_CAM effect   -0.116 0.01707 Inf    -0.150   -0.0828   -6.809
##  Used_conv_ONLY effect       0.500 0.02051 Inf     0.459    0.5399   24.362
##  p.value
##   <.0001
##   <.0001
##   <.0001
##   <.0001
## 
## Results are averaged over the levels of: edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
(mod1.income.fr.eff.CM<-
    contrast(mod1.income.fr.trends,
             method = 
               list("Conv - No conv" = c(0,0,1,1)),
             simple="DV.f",
             infer=c(T,T)))
```

```
## income.fr = quint.5:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.883 0.0159 Inf     0.852     0.914  55.428  <.0001
## 
## income.fr = missing:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.874 0.0172 Inf     0.840     0.908  50.951  <.0001
## 
## income.fr = quint.1:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.895 0.0150 Inf     0.866     0.924  59.868  <.0001
## 
## income.fr = quint.2:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.885 0.0156 Inf     0.854     0.915  56.545  <.0001
## 
## income.fr = quint.3:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.883 0.0158 Inf     0.852     0.914  56.043  <.0001
## 
## income.fr = quint.4:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.883 0.0157 Inf     0.853     0.914  56.388  <.0001
## 
## Results are averaged over the levels of: edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
(mod1.income.fr.eff.CM_comp<-
  contrast(mod1.income.fr.eff.CM,
         simple="income.fr",method="eff",
         infer=c(T,T),adjust="none"))
```

```
## contrast = Conv - No conv:
##  contrast1       estimate      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  quint.5 effect -0.000908 0.00519 Inf  -0.01107   0.00926  -0.175  0.8610
##  missing effect -0.009814 0.00635 Inf  -0.02226   0.00263  -1.545  0.1223
##  quint.1 effect  0.011281 0.00663 Inf  -0.00172   0.02428   1.701  0.0889
##  quint.2 effect  0.000854 0.00554 Inf  -0.01001   0.01172   0.154  0.8775
##  quint.3 effect -0.001013 0.00512 Inf  -0.01104   0.00902  -0.198  0.8431
##  quint.4 effect -0.000400 0.00498 Inf  -0.01016   0.00936  -0.080  0.9361
## 
## Results are averaged over the levels of: edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# No CAM versus CAM


(mod1.income.fr.eff.CAM<-
    contrast(mod1.income.fr.trends,
             method = 
               list("CAM - No CAM" = c(0,1,1,0)),
             simple="DV.f",
             infer=c(T,T)))
```

```
## income.fr = quint.5:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    0.138 0.0171 Inf    0.1041     0.171   8.040  <.0001
## 
## income.fr = missing:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    0.120 0.0163 Inf    0.0883     0.152   7.367  <.0001
## 
## income.fr = quint.1:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    0.117 0.0154 Inf    0.0865     0.147   7.590  <.0001
## 
## income.fr = quint.2:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    0.128 0.0159 Inf    0.0963     0.159   8.005  <.0001
## 
## income.fr = quint.3:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    0.130 0.0162 Inf    0.0986     0.162   8.034  <.0001
## 
## income.fr = quint.4:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    0.141 0.0173 Inf    0.1066     0.174   8.113  <.0001
## 
## Results are averaged over the levels of: edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
(mod1.income.fr.eff.CAM_comp<-
    contrast(mod1.income.fr.eff.CAM,
             simple="income.fr",method="eff",
             infer=c(T,T),adjust="none"))
```

```
## contrast = CAM - No CAM:
##  contrast1      estimate      SE  df asymp.LCL asymp.UCL z.ratio p.value
##  quint.5 effect  0.00876 0.00564 Inf  -0.00230  0.019821   1.553  0.1205
##  missing effect -0.00857 0.00691 Inf  -0.02212  0.004987  -1.239  0.2155
##  quint.1 effect -0.01220 0.00648 Inf  -0.02490  0.000498  -1.883  0.0597
##  quint.2 effect -0.00132 0.00541 Inf  -0.01193  0.009284  -0.245  0.8068
##  quint.3 effect  0.00163 0.00528 Inf  -0.00871  0.011977   0.309  0.7572
##  quint.4 effect  0.01170 0.00545 Inf   0.00101  0.022383   2.145  0.0320
## 
## Results are averaged over the levels of: edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(mod1.income.fr.eff.CM_comp,
                        mod1.income.fr.eff.CAM_comp,adjust="none")),
       "../../results/Added_covariates/mod1.income.fr.eff.COMB_comp.xlsx",overwrite=T)

# save to file
export(data.frame(rbind(mod1.income.fr.eff.CM,
                        mod1.income.fr.eff.CAM,adjust="none")),
       "../../results/Added_covariates/mod1.income.fr.eff.COMB.xlsx",overwrite=T)
```

## Model for lrgen


```r
mod2.lrgen<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+blgetmg.c+mstatus.c+
            strain.on.health.c+
            lrgen.z,
                    random= ~1|cntry,
                    estimator="ML",
                    control = mmclogit.control(maxit = 250, trace=TRUE),
                    data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28756.44 - criterion = 0.7766495
## Iteration 2 - deviance = 27476.52 - criterion = 0.1510232
## Iteration 3 - deviance = 27313.42 - criterion = 0.02506318
## Iteration 4 - deviance = 27277.77 - criterion = 0.01435918
## Iteration 5 - deviance = 27271.38 - criterion = 0.004335226
## Iteration 6 - deviance = 27270.16 - criterion = 0.0007211957
## Iteration 7 - deviance = 27269.95 - criterion = 6.006769e-05
## Iteration 8 - deviance = 27269.8 - criterion = 1.772706e-06
## Iteration 9 - deviance = 27269.77 - criterion = 2.511598e-08
## Iteration 10 - deviance = 27269.77 - criterion = 7.702987e-10
## converged
```

```r
# general model comparison

anova(mod1,mod2.lrgen,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + lrgen.z
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
## 1     61719      27281                        
## 2     61716      27270  3   11.617 0.008817 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Is lrgen associated with using conventional medicine?
ref_grid(mod2.lrgen)
```

```
## 'emmGrid' object with variables:
##     gndr.c = -0.5,  0.5
##     age10.c = 0.39152
##     income.fr = quint.5, missing, quint.1, quint.2, quint.3, quint.4
##     edu.f = 7. MA, 1. <LS, 2. LS, 3. LUS, 4. UUS, 5. AV, 6. BA
##     blgetmg.c = -0.5,  0.5
##     mstatus.c = -0.5,  0.5
##     strain.on.health.c = 0.065329
##     lrgen.z = -0.00060242
##     DV.f = multivariate response levels: NN, Used_CAM_ONLY, Used_conv_and_CAM, Used_conv_ONLY
```

```r
mod2.lrgen.trends<-
  emtrends(mod2.lrgen,~1|DV.f,
           var="lrgen.z",infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0,
                   strain.on.health.c=0))

#effects for each DV-category
(mod2.lrgen.eff<-
  contrast(mod2.lrgen.trends,simple="DV.f",
         adjust="none","eff", infer=c(T,T)))
```

```
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                 0.07262 0.0307 Inf    0.0125    0.1328   2.367
##  Used_CAM_ONLY effect     -0.02575 0.0705 Inf   -0.1640    0.1125  -0.365
##  Used_conv_and_CAM effect -0.04580 0.0303 Inf   -0.1052    0.0136  -1.512
##  Used_conv_ONLY effect    -0.00108 0.0261 Inf   -0.0522    0.0501  -0.041
##  p.value
##   0.0179
##   0.7151
##   0.1307
##   0.9670
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod2.lrgen.eff),
       "../../results/Added_covariates/mod2.lrgen.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod2.lrgen.eff.CM<-
    contrast(mod2.lrgen.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod2.lrgen.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast       estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv  -0.0646 0.034 Inf    -0.131   0.00193  -1.903  0.0570
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Use of CAM

(mod2.lrgen.eff.CAM<-
    contrast(mod2.lrgen.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod2.lrgen.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM  -0.0684 0.0326 Inf    -0.132  -0.00441  -2.095  0.0362
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(
  mod2.lrgen.eff.CM,
  mod2.lrgen.eff.CAM,adjust="none")),
  "../../results/Added_covariates/mod2.lrgen.eff.COMB.xlsx")
```

## Model for lrecon


```r
mod2.lrecon<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+blgetmg.c+mstatus.c+
            strain.on.health.c+
            lrecon.z,
                    random= ~1|cntry,
                    estimator="ML",
                    control = mmclogit.control(maxit = 250, trace=TRUE),
                    data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28760.83 - criterion = 0.7766322
## Iteration 2 - deviance = 27481.26 - criterion = 0.1511373
## Iteration 3 - deviance = 27318.09 - criterion = 0.02508362
## Iteration 4 - deviance = 27282.4 - criterion = 0.01436751
## Iteration 5 - deviance = 27276.01 - criterion = 0.004335731
## Iteration 6 - deviance = 27274.81 - criterion = 0.0007209199
## Iteration 7 - deviance = 27274.61 - criterion = 5.994663e-05
## Iteration 8 - deviance = 27274.46 - criterion = 1.753948e-06
## Iteration 9 - deviance = 27274.43 - criterion = 2.507583e-08
## Iteration 10 - deviance = 27274.42 - criterion = 7.766099e-10
## converged
```

```r
# general model comparison

anova(mod1,mod2.lrecon,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + lrecon.z
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1     61719      27281                       
## 2     61716      27274  3   6.9602  0.07318 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Is lrecon associated with using conventional medicine?
ref_grid(mod2.lrecon)
```

```
## 'emmGrid' object with variables:
##     gndr.c = -0.5,  0.5
##     age10.c = 0.39152
##     income.fr = quint.5, missing, quint.1, quint.2, quint.3, quint.4
##     edu.f = 7. MA, 1. <LS, 2. LS, 3. LUS, 4. UUS, 5. AV, 6. BA
##     blgetmg.c = -0.5,  0.5
##     mstatus.c = -0.5,  0.5
##     strain.on.health.c = 0.065329
##     lrecon.z = -0.00052762
##     DV.f = multivariate response levels: NN, Used_CAM_ONLY, Used_conv_and_CAM, Used_conv_ONLY
```

```r
mod2.lrecon.trends<-
  emtrends(mod2.lrecon,~1|DV.f,
           var="lrecon.z",infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0,
                   strain.on.health.c=0))

#effects for each DV-category
(mod2.lrecon.eff<-
  contrast(mod2.lrecon.trends,simple="DV.f",
         adjust="none","eff", infer=c(T,T)))
```

```
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                 0.06140 0.0298 Inf   0.00295    0.1199   2.059
##  Used_CAM_ONLY effect     -0.03877 0.0680 Inf  -0.17202    0.0945  -0.570
##  Used_conv_and_CAM effect -0.02705 0.0297 Inf  -0.08532    0.0312  -0.910
##  Used_conv_ONLY effect     0.00443 0.0253 Inf  -0.04518    0.0540   0.175
##  p.value
##   0.0395
##   0.5684
##   0.3628
##   0.8612
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod2.lrecon.eff),
       "../../results/Added_covariates/mod2.lrecon.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod2.lrecon.eff.CM<-
    contrast(mod2.lrecon.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod2.lrecon.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast       estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv  -0.0418 0.033 Inf    -0.107    0.0229  -1.267  0.2051
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Use of CAM

(mod2.lrecon.eff.CAM<-
    contrast(mod2.lrecon.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod2.lrecon.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM   -0.057 0.0323 Inf     -0.12   0.00632  -1.764  0.0777
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(
  mod2.lrecon.eff.CM,
  mod2.lrecon.eff.CAM,adjust="none")),
  "../../results/Added_covariates/mod2.lrecon.eff.COMB.xlsx")
```

## Model for galtan


```r
mod2.galtan<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+blgetmg.c+mstatus.c+
            strain.on.health.c+
            galtan.z,
                    random= ~1|cntry,
                    estimator="ML",
                    control = mmclogit.control(maxit = 250, trace=TRUE),
                    data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28754.19 - criterion = 0.7765056
## Iteration 2 - deviance = 27473.95 - criterion = 0.1512703
## Iteration 3 - deviance = 27309.53 - criterion = 0.02551415
## Iteration 4 - deviance = 27273.29 - criterion = 0.01472563
## Iteration 5 - deviance = 27266.87 - criterion = 0.004415278
## Iteration 6 - deviance = 27265.7 - criterion = 0.000720761
## Iteration 7 - deviance = 27265.48 - criterion = 6.131468e-05
## Iteration 8 - deviance = 27265.33 - criterion = 1.811319e-06
## Iteration 9 - deviance = 27265.3 - criterion = 2.474818e-08
## Iteration 10 - deviance = 27265.29 - criterion = 7.55469e-10
## converged
```

```r
# general model comparison

anova(mod1,mod2.galtan,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + galtan.z
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
## 1     61719      27281                        
## 2     61716      27265  3   16.091 0.001086 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Is galtan associated with using conventional medicine?
ref_grid(mod2.galtan)
```

```
## 'emmGrid' object with variables:
##     gndr.c = -0.5,  0.5
##     age10.c = 0.39152
##     income.fr = quint.5, missing, quint.1, quint.2, quint.3, quint.4
##     edu.f = 7. MA, 1. <LS, 2. LS, 3. LUS, 4. UUS, 5. AV, 6. BA
##     blgetmg.c = -0.5,  0.5
##     mstatus.c = -0.5,  0.5
##     strain.on.health.c = 0.065329
##     galtan.z = -0.00084763
##     DV.f = multivariate response levels: NN, Used_CAM_ONLY, Used_conv_and_CAM, Used_conv_ONLY
```

```r
mod2.galtan.trends<-
  emtrends(mod2.galtan,~1|DV.f,
           var="galtan.z",infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0,
                   strain.on.health.c=0))

#effects for each DV-category
(mod2.galtan.eff<-
  contrast(mod2.galtan.trends,simple="DV.f",
         adjust="none","eff", infer=c(T,T)))
```

```
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                  0.1006 0.0324 Inf   0.03710   0.16411   3.105
##  Used_CAM_ONLY effect      -0.1369 0.0746 Inf  -0.28310   0.00935  -1.835
##  Used_conv_and_CAM effect  -0.0232 0.0322 Inf  -0.08629   0.03995  -0.720
##  Used_conv_ONLY effect      0.0594 0.0276 Inf   0.00531   0.11358   2.152
##  p.value
##   0.0019
##   0.0666
##   0.4718
##   0.0314
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod2.galtan.eff),
       "../../results/Added_covariates/mod2.galtan.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod2.galtan.eff.CM<-
    contrast(mod2.galtan.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod2.galtan.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv  -0.0092 0.0357 Inf   -0.0793    0.0609  -0.257  0.7970
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Use of CAM

(mod2.galtan.eff.CAM<-
    contrast(mod2.galtan.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod2.galtan.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    -0.13 0.0348 Inf    -0.198   -0.0612  -3.718  0.0002
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(
  mod2.galtan.eff.CM,
  mod2.galtan.eff.CAM,adjust="none")),
  "../../results/Added_covariates/mod2.galtan.eff.COMB.xlsx")
```

### Contrast between CAM-only and CAM-and-conv against no-CAM


```r
pairs(mod2.galtan.eff,adjust="none",infer=c(T,T))
```

```
##  contrast                                         estimate     SE  df
##  NN effect - Used_CAM_ONLY effect                   0.2375 0.1019 Inf
##  NN effect - Used_conv_and_CAM effect               0.1238 0.0374 Inf
##  NN effect - Used_conv_ONLY effect                  0.0412 0.0287 Inf
##  Used_CAM_ONLY effect - Used_conv_and_CAM effect   -0.1137 0.1020 Inf
##  Used_CAM_ONLY effect - Used_conv_ONLY effect      -0.1963 0.0994 Inf
##  Used_conv_and_CAM effect - Used_conv_ONLY effect  -0.0826 0.0275 Inf
##  asymp.LCL asymp.UCL z.ratio p.value
##     0.0377   0.43726   2.330  0.0198
##     0.0504   0.19716   3.306  0.0009
##    -0.0151   0.09738   1.435  0.1513
##    -0.3136   0.08621  -1.115  0.2650
##    -0.3912  -0.00144  -1.974  0.0483
##    -0.1366  -0.02863  -2.999  0.0027
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file

export(data.frame(pairs(mod2.galtan.eff,
                        adjust="none",
                        infer=c(T,T))),
  "../../results/Added_covariates/mod2.galtan.eff.CAM.contrast.xlsx")
```

## Model for antielite_salience


```r
mod2.antielite_salience<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+blgetmg.c+mstatus.c+
            strain.on.health.c+
            antielite_salience.z,
                    random= ~1|cntry,
                    estimator="ML",
                    control = mmclogit.control(maxit = 250, trace=TRUE),
                    data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28764.08 - criterion = 0.7764823
## Iteration 2 - deviance = 27483.45 - criterion = 0.1512997
## Iteration 3 - deviance = 27319.58 - criterion = 0.02535144
## Iteration 4 - deviance = 27283.47 - criterion = 0.01468571
## Iteration 5 - deviance = 27277.11 - criterion = 0.004438835
## Iteration 6 - deviance = 27276.1 - criterion = 0.000721607
## Iteration 7 - deviance = 27275.57 - criterion = 4.642588e-05
## Iteration 8 - deviance = 27275.46 - criterion = 9.326506e-07
## Iteration 9 - deviance = 27275.44 - criterion = 1.933607e-09
## converged
```

```r
# general model comparison

anova(mod1,mod2.antielite_salience,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + antielite_salience.z
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1     61719      27281                     
## 2     61716      27275  3   5.9432   0.1144
```

```r
# Is antielite_salience associated with using conventional medicine?
ref_grid(mod2.antielite_salience)
```

```
## 'emmGrid' object with variables:
##     gndr.c = -0.5,  0.5
##     age10.c = 0.39152
##     income.fr = quint.5, missing, quint.1, quint.2, quint.3, quint.4
##     edu.f = 7. MA, 1. <LS, 2. LS, 3. LUS, 4. UUS, 5. AV, 6. BA
##     blgetmg.c = -0.5,  0.5
##     mstatus.c = -0.5,  0.5
##     strain.on.health.c = 0.065329
##     antielite_salience.z = -0.0002272
##     DV.f = multivariate response levels: NN, Used_CAM_ONLY, Used_conv_and_CAM, Used_conv_ONLY
```

```r
mod2.antielite_salience.trends<-
  emtrends(mod2.antielite_salience,~1|DV.f,
           var="antielite_salience.z",infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0,
                   strain.on.health.c=0))

#effects for each DV-category
(mod2.antielite_salience.eff<-
  contrast(mod2.antielite_salience.trends,simple="DV.f",
         adjust="none","eff", infer=c(T,T)))
```

```
##  contrast                  estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                 0.000679 0.0312 Inf   -0.0606  6.19e-02   0.022
##  Used_CAM_ONLY effect      0.111450 0.0714 Inf   -0.0285  2.51e-01   1.561
##  Used_conv_and_CAM effect -0.062962 0.0321 Inf   -0.1259  6.51e-06  -1.960
##  Used_conv_ONLY effect    -0.049167 0.0267 Inf   -0.1016  3.24e-03  -1.839
##  p.value
##   0.9827
##   0.1186
##   0.0500
##   0.0660
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod2.antielite_salience.eff),
       "../../results/Added_covariates/mod2.antielite_salience.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod2.antielite_salience.eff.CM<-
    contrast(mod2.antielite_salience.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod2.antielite_salience.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv  -0.0849 0.0344 Inf    -0.152   -0.0175  -2.468  0.0136
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Use of CAM

(mod2.antielite_salience.eff.CAM<-
    contrast(mod2.antielite_salience.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod2.antielite_salience.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM   0.0161 0.0354 Inf   -0.0532    0.0854   0.456  0.6483
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(
  mod2.antielite_salience.eff.CM,
  mod2.antielite_salience.eff.CAM,adjust="none")),
  "../../results/Added_covariates/mod2.antielite_salience.eff.COMB.xlsx")
```

## Model for corrupt_salience


```r
mod2.corrupt_salience<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+blgetmg.c+mstatus.c+
            strain.on.health.c+
            corrupt_salience.z,
                    random= ~1|cntry,
                    estimator="ML",
                    control = mmclogit.control(maxit = 250, trace=TRUE),
                    data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28762.99 - criterion = 0.7764802
## Iteration 2 - deviance = 27480.98 - criterion = 0.151393
## Iteration 3 - deviance = 27313.91 - criterion = 0.02615593
## Iteration 4 - deviance = 27276.62 - criterion = 0.01545803
## Iteration 5 - deviance = 27270.32 - criterion = 0.00438462
## Iteration 6 - deviance = 27269.03 - criterion = 0.0007143112
## Iteration 7 - deviance = 27268.62 - criterion = 4.495654e-05
## Iteration 8 - deviance = 27268.54 - criterion = 8.086869e-07
## Iteration 9 - deviance = 27268.53 - criterion = 1.178315e-09
## converged
```

```r
# general model comparison

anova(mod1,mod2.corrupt_salience,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + corrupt_salience.z
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
## 1     61719      27281                        
## 2     61716      27269  3   12.858 0.004955 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Is corrupt_salience associated with using conventional medicine?
ref_grid(mod2.corrupt_salience)
```

```
## 'emmGrid' object with variables:
##     gndr.c = -0.5,  0.5
##     age10.c = 0.39152
##     income.fr = quint.5, missing, quint.1, quint.2, quint.3, quint.4
##     edu.f = 7. MA, 1. <LS, 2. LS, 3. LUS, 4. UUS, 5. AV, 6. BA
##     blgetmg.c = -0.5,  0.5
##     mstatus.c = -0.5,  0.5
##     strain.on.health.c = 0.065329
##     corrupt_salience.z = -0.00015779
##     DV.f = multivariate response levels: NN, Used_CAM_ONLY, Used_conv_and_CAM, Used_conv_ONLY
```

```r
mod2.corrupt_salience.trends<-
  emtrends(mod2.corrupt_salience,~1|DV.f,
           var="corrupt_salience.z",infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0,
                   strain.on.health.c=0))

#effects for each DV-category
(mod2.corrupt_salience.eff<-
  contrast(mod2.corrupt_salience.trends,simple="DV.f",
         adjust="none","eff", infer=c(T,T)))
```

```
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                 -0.0843 0.0440 Inf    -0.171   0.00199  -1.915
##  Used_CAM_ONLY effect       0.3200 0.0994 Inf     0.125   0.51478   3.219
##  Used_conv_and_CAM effect  -0.0995 0.0450 Inf    -0.188  -0.01126  -2.210
##  Used_conv_ONLY effect     -0.1361 0.0374 Inf    -0.209  -0.06278  -3.638
##  p.value
##   0.0555
##   0.0013
##   0.0271
##   0.0003
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod2.corrupt_salience.eff),
       "../../results/Added_covariates/mod2.corrupt_salience.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod2.corrupt_salience.eff.CM<-
    contrast(mod2.corrupt_salience.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod2.corrupt_salience.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv   -0.148 0.0486 Inf    -0.244   -0.0529  -3.049  0.0023
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Use of CAM

(mod2.corrupt_salience.eff.CAM<-
    contrast(mod2.corrupt_salience.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod2.corrupt_salience.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    0.136 0.0497 Inf    0.0385     0.233   2.735  0.0062
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(
  mod2.corrupt_salience.eff.CM,
  mod2.corrupt_salience.eff.CAM,adjust="none")),
  "../../results/Added_covariates/mod2.corrupt_salience.eff.COMB.xlsx")
```


### Contrast between CAM-only and CAM-and-conv against no-CAM


```r
pairs(mod2.corrupt_salience.eff,
      adjust="none",infer=c(T,T))
```

```
##  contrast                                         estimate     SE  df
##  NN effect - Used_CAM_ONLY effect                  -0.4043 0.1360 Inf
##  NN effect - Used_conv_and_CAM effect               0.0152 0.0541 Inf
##  NN effect - Used_conv_ONLY effect                  0.0518 0.0399 Inf
##  Used_CAM_ONLY effect - Used_conv_and_CAM effect    0.4195 0.1369 Inf
##  Used_CAM_ONLY effect - Used_conv_ONLY effect       0.4561 0.1325 Inf
##  Used_conv_and_CAM effect - Used_conv_ONLY effect   0.0366 0.0413 Inf
##  asymp.LCL asymp.UCL z.ratio p.value
##    -0.6708    -0.138  -2.973  0.0029
##    -0.0908     0.121   0.281  0.7789
##    -0.0265     0.130   1.297  0.1947
##     0.1512     0.688   3.065  0.0022
##     0.1965     0.716   3.443  0.0006
##    -0.0444     0.118   0.886  0.3756
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file

export(data.frame(pairs(mod2.corrupt_salience.eff,
                        adjust="none",
                        infer=c(T,T))),
  "../../results/Added_covariates/mod2.corrupt_salience.eff.CAM.contrast.xlsx")
```

# Interactions by strain on health

## Model for lrgen


```r
mod3.lrgen<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+blgetmg.c+mstatus.c+strain.on.health.c+lrgen.z+
            strain.on.health.c:lrgen.z,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28756.52 - criterion = 0.7766371
## Iteration 2 - deviance = 27475.4 - criterion = 0.1513372
## Iteration 3 - deviance = 27311.4 - criterion = 0.02509019
## Iteration 4 - deviance = 27275.77 - criterion = 0.01426078
## Iteration 5 - deviance = 27269.41 - criterion = 0.004307336
## Iteration 6 - deviance = 27268.21 - criterion = 0.0007170371
## Iteration 7 - deviance = 27268.01 - criterion = 5.944112e-05
## Iteration 8 - deviance = 27267.86 - criterion = 1.680903e-06
## Iteration 9 - deviance = 27267.83 - criterion = 2.438175e-08
## Iteration 10 - deviance = 27267.83 - criterion = 7.431969e-10
## converged
```

```r
anova(mod2.lrgen,mod3.lrgen,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + lrgen.z
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + lrgen.z + strain.on.health.c:lrgen.z
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1     61716      27270                     
## 2     61713      27268  3   1.9426   0.5844
```

```r
#Alternative model with manually defined interaction
fdat$lrgen.zXstrain.on.health.c<-fdat$lrgen.z*fdat$strain.on.health.c

mod3.lrgen.alt<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+blgetmg.c+mstatus.c+strain.on.health.c+lrgen.z+
            lrgen.zXstrain.on.health.c,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28756.52 - criterion = 0.7766371
## Iteration 2 - deviance = 27475.4 - criterion = 0.1513372
## Iteration 3 - deviance = 27311.4 - criterion = 0.02509019
## Iteration 4 - deviance = 27275.77 - criterion = 0.01426078
## Iteration 5 - deviance = 27269.41 - criterion = 0.004307336
## Iteration 6 - deviance = 27268.21 - criterion = 0.0007170371
## Iteration 7 - deviance = 27268.01 - criterion = 5.944112e-05
## Iteration 8 - deviance = 27267.86 - criterion = 1.680903e-06
## Iteration 9 - deviance = 27267.83 - criterion = 2.438175e-08
## Iteration 10 - deviance = 27267.83 - criterion = 7.431969e-10
## converged
```

```r
mod3.lrgen.alt.trends<-
  emtrends(mod3.lrgen.alt,~1|DV.f,
           var="lrgen.zXstrain.on.health.c",
           infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0))

#effects for each DV-category
(mod3.lrgen.alt.eff<-
  contrast(mod3.lrgen.alt.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))
```

```
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                 0.00849 0.0182 Inf   -0.0272    0.0441   0.467
##  Used_CAM_ONLY effect      0.01779 0.0380 Inf   -0.0566    0.0922   0.469
##  Used_conv_and_CAM effect -0.01195 0.0149 Inf   -0.0412    0.0173  -0.801
##  Used_conv_ONLY effect    -0.01433 0.0139 Inf   -0.0415    0.0128  -1.034
##  p.value
##   0.6408
##   0.6393
##   0.4231
##   0.3009
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod3.lrgen.alt.eff),
       "../../results/Added_covariates/mod3.lrgenBYhealth.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod3.lrgen.eff.CM<-contrast(mod3.lrgen.alt.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod3.lrgen.alt.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv  -0.0248 0.0209 Inf   -0.0658    0.0162  -1.185  0.2362
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Use of CAM

#weighted across all effects

(mod3.lrgen.eff.CAM<-contrast(mod3.lrgen.alt.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod3.lrgen.alt.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM  0.00114 0.0143 Inf   -0.0269    0.0292   0.080  0.9364
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(
  mod3.lrgen.eff.CM,
  mod3.lrgen.eff.CAM,adjust="none")),
  "../../results/Added_covariates/mod3.lrgenBYhealth.eff.COMB.xlsx")
```


## Model for lrecon


```r
mod3.lrecon<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+blgetmg.c+mstatus.c+strain.on.health.c+lrecon.z+
            strain.on.health.c:lrecon.z,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             epsilon=0.0001,
                             trace=TRUE),
          data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28760.65 - criterion = 0.7766145
## Iteration 2 - deviance = 27480.27 - criterion = 0.151319
## Iteration 3 - deviance = 27315.74 - criterion = 0.0252439
## Iteration 4 - deviance = 27279.73 - criterion = 0.01454909
## Iteration 5 - deviance = 27273.31 - criterion = 0.004404457
## Iteration 6 - deviance = 27272.12 - criterion = 0.0007232964
## Iteration 7 - deviance = 27271.93 - criterion = 5.886594e-05
## converged
```

```r
anova(mod2.lrecon,mod3.lrecon,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + lrecon.z
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + lrecon.z + strain.on.health.c:lrecon.z
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1     61716      27274                     
## 2     61713      27272  3   2.4926   0.4766
```

```r
#Alternative model with manually defined interaction
fdat$lrecon.zXstrain.on.health.c<-fdat$lrecon.z*fdat$strain.on.health.c

mod3.lrecon.alt<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+blgetmg.c+mstatus.c+strain.on.health.c+lrecon.z+
            lrecon.zXstrain.on.health.c,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             epsilon=0.0001,
                             trace=TRUE),
          data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28760.65 - criterion = 0.7766145
## Iteration 2 - deviance = 27480.27 - criterion = 0.151319
## Iteration 3 - deviance = 27315.74 - criterion = 0.0252439
## Iteration 4 - deviance = 27279.73 - criterion = 0.01454909
## Iteration 5 - deviance = 27273.31 - criterion = 0.004404457
## Iteration 6 - deviance = 27272.12 - criterion = 0.0007232964
## Iteration 7 - deviance = 27271.93 - criterion = 5.886594e-05
## converged
```

```r
mod3.lrecon.alt.trends<-
  emtrends(mod3.lrecon.alt,~1|DV.f,
           var="lrecon.zXstrain.on.health.c",
           infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0))

#effects for each DV-category
(mod3.lrecon.alt.eff<-
  contrast(mod3.lrecon.alt.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))
```

```
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                -0.00384 0.0179 Inf   -0.0389   0.03125  -0.214
##  Used_CAM_ONLY effect      0.04680 0.0373 Inf   -0.0264   0.11995   1.254
##  Used_conv_and_CAM effect -0.02188 0.0148 Inf   -0.0509   0.00711  -1.479
##  Used_conv_ONLY effect    -0.02108 0.0137 Inf   -0.0478   0.00569  -1.543
##  p.value
##   0.8302
##   0.2099
##   0.1390
##   0.1227
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod3.lrecon.alt.eff),
       "../../results/Added_covariates/mod3.lreconBYhealth.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod3.lrecon.eff.CM<-contrast(mod3.lrecon.alt.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod3.lrecon.alt.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv  -0.0341 0.0206 Inf   -0.0744   0.00629  -1.654  0.0980
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Use of CAM

#weighted across all effects

(mod3.lrecon.eff.CAM<-contrast(mod3.lrecon.alt.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod3.lrecon.alt.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM   0.0085 0.0143 Inf   -0.0195    0.0366   0.594  0.5524
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(
  mod3.lrecon.eff.CM,
  mod3.lrecon.eff.CAM,adjust="none")),
  "../../results/Added_covariates/mod3.lreconBYhealth.eff.COMB.xlsx")
```


## Model for galtan

(Here it was necessary to change the epsilon convergence criterion 1e-04 instead of 1e-08)


```r
mod3.galtan<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+blgetmg.c+mstatus.c+strain.on.health.c+galtan.z+
            strain.on.health.c:galtan.z,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             epsilon=0.0001,
                             trace=TRUE),
          data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28754.1 - criterion = 0.7765009
## Iteration 2 - deviance = 27472.89 - criterion = 0.151442
## Iteration 3 - deviance = 27308.39 - criterion = 0.02546523
## Iteration 4 - deviance = 27272.45 - criterion = 0.0144793
## Iteration 5 - deviance = 27266.04 - criterion = 0.004407325
## Iteration 6 - deviance = 27264.89 - criterion = 0.0007220233
## Iteration 7 - deviance = 27264.68 - criterion = 6.037145e-05
## converged
```

```r
anova(mod2.galtan,mod3.galtan,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + galtan.z
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + galtan.z + strain.on.health.c:galtan.z
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1     61716      27265                     
## 2     61713      27265  3  0.61113   0.8939
```

```r
#Alternative model with manually defined interaction
fdat$galtan.zXstrain.on.health.c<-fdat$galtan.z*fdat$strain.on.health.c

mod3.galtan.alt<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+blgetmg.c+mstatus.c+strain.on.health.c+galtan.z+
            galtan.zXstrain.on.health.c,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE,
                             epsilon=0.0001),
          data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28754.1 - criterion = 0.7765009
## Iteration 2 - deviance = 27472.89 - criterion = 0.151442
## Iteration 3 - deviance = 27308.39 - criterion = 0.02546523
## Iteration 4 - deviance = 27272.45 - criterion = 0.0144793
## Iteration 5 - deviance = 27266.04 - criterion = 0.004407325
## Iteration 6 - deviance = 27264.89 - criterion = 0.0007220233
## Iteration 7 - deviance = 27264.68 - criterion = 6.037145e-05
## converged
```

```r
mod3.galtan.alt.trends<-
  emtrends(mod3.galtan.alt,~1|DV.f,
           var="galtan.zXstrain.on.health.c",
           infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0))

#effects for each DV-category
(mod3.galtan.alt.eff<-
  contrast(mod3.galtan.alt.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))
```

```
##  contrast                  estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                 0.000362 0.0190 Inf   -0.0368    0.0376   0.019
##  Used_CAM_ONLY effect      0.011408 0.0398 Inf   -0.0665    0.0893   0.287
##  Used_conv_and_CAM effect -0.002404 0.0157 Inf   -0.0331    0.0283  -0.153
##  Used_conv_ONLY effect    -0.009366 0.0145 Inf   -0.0378    0.0191  -0.646
##  p.value
##   0.9848
##   0.7741
##   0.8781
##   0.5185
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod3.galtan.alt.eff),
       "../../results/Added_covariates/mod3.galtanBYhealth.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod3.galtan.eff.CM<-contrast(mod3.galtan.alt.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod3.galtan.alt.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv  -0.0102 0.0218 Inf   -0.0529    0.0325  -0.469  0.6392
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Use of CAM

#weighted across all effects

(mod3.galtan.eff.CAM<-contrast(mod3.galtan.alt.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod3.galtan.alt.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM  0.00669 0.0151 Inf    -0.023    0.0363   0.442  0.6583
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(
  mod3.galtan.eff.CM,
  mod3.galtan.eff.CAM,adjust="none")),
  "../../results/Added_covariates/mod3.galtanBYhealth.eff.COMB.xlsx")
```



## Model for antielite_salience


```r
mod3.antielite_salience<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+blgetmg.c+mstatus.c+strain.on.health.c+antielite_salience.z+
            strain.on.health.c:antielite_salience.z,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28757.02 - criterion = 0.7766162
## Iteration 2 - deviance = 27474.31 - criterion = 0.1513382
## Iteration 3 - deviance = 27307.03 - criterion = 0.02638931
## Iteration 4 - deviance = 27269 - criterion = 0.01597193
## Iteration 5 - deviance = 27261.98 - criterion = 0.004905282
## Iteration 6 - deviance = 27260.83 - criterion = 0.0007387682
## Iteration 7 - deviance = 27260.37 - criterion = 5.15879e-05
## Iteration 8 - deviance = 27260.29 - criterion = 9.369216e-07
## Iteration 9 - deviance = 27260.28 - criterion = 1.098215e-09
## converged
```

```r
anova(mod2.antielite_salience,
      mod3.antielite_salience,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + antielite_salience.z
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + antielite_salience.z + strain.on.health.c:antielite_salience.z
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
## 1     61716      27275                        
## 2     61713      27260  3   15.166  0.00168 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
#Alternative model with manually defined interaction
fdat$antielite_salience.zXstrain.on.health.c<-fdat$antielite_salience.z*fdat$strain.on.health.c

mod3.antielite_salience.alt<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+blgetmg.c+mstatus.c+strain.on.health.c+antielite_salience.z+
            antielite_salience.zXstrain.on.health.c,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28757.02 - criterion = 0.7766162
## Iteration 2 - deviance = 27474.31 - criterion = 0.1513382
## Iteration 3 - deviance = 27307.03 - criterion = 0.02638931
## Iteration 4 - deviance = 27269 - criterion = 0.01597193
## Iteration 5 - deviance = 27261.98 - criterion = 0.004905282
## Iteration 6 - deviance = 27260.83 - criterion = 0.0007387682
## Iteration 7 - deviance = 27260.37 - criterion = 5.15879e-05
## Iteration 8 - deviance = 27260.29 - criterion = 9.369216e-07
## Iteration 9 - deviance = 27260.28 - criterion = 1.098215e-09
## converged
```

```r
mod3.antielite_salience.alt.trends<-
  emtrends(mod3.antielite_salience.alt,~1|DV.f,
           var="antielite_salience.zXstrain.on.health.c",
           infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0))

#effects for each DV-category
(mod3.antielite_salience.alt.eff<-
  contrast(mod3.antielite_salience.alt.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))
```

```
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                  0.0180 0.0196 Inf   -0.0205    0.0565   0.917
##  Used_CAM_ONLY effect      -0.1248 0.0431 Inf   -0.2092   -0.0403  -2.895
##  Used_conv_and_CAM effect   0.0619 0.0169 Inf    0.0288    0.0949   3.664
##  Used_conv_ONLY effect      0.0449 0.0156 Inf    0.0143    0.0755   2.879
##  p.value
##   0.3593
##   0.0038
##   0.0002
##   0.0040
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod3.antielite_salience.alt.eff),
       "../../results/Added_covariates/mod3.antielite_salienceBYhealth.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod3.antielite_salience.eff.CM<-
    contrast(mod3.antielite_salience.alt.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod3.antielite_salience.alt.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv   0.0785 0.0226 Inf    0.0343     0.123   3.479  0.0005
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Use of CAM

#weighted across all effects

(mod3.antielite_salience.eff.CAM<-
    contrast(mod3.antielite_salience.alt.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod3.antielite_salience.alt.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM  -0.0152 0.0163 Inf   -0.0471    0.0168  -0.929  0.3531
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(
  mod3.antielite_salience.eff.CM,
  mod3.antielite_salience.eff.CAM,adjust="none")),
  "../../results/Added_covariates/mod3.antielite_salienceBYhealth.eff.COMB.xlsx")
```

### Probing simple slopes


```r
ref_grid(mod3.antielite_salience)
```

```
## 'emmGrid' object with variables:
##     gndr.c = -0.5,  0.5
##     age10.c = 0.39152
##     income.fr = quint.5, missing, quint.1, quint.2, quint.3, quint.4
##     edu.f = 7. MA, 1. <LS, 2. LS, 3. LUS, 4. UUS, 5. AV, 6. BA
##     blgetmg.c = -0.5,  0.5
##     mstatus.c = -0.5,  0.5
##     strain.on.health.c = 0.065329
##     antielite_salience.z = -0.0002272
##     DV.f = multivariate response levels: NN, Used_CAM_ONLY, Used_conv_and_CAM, Used_conv_ONLY
```

```r
#pick points for inference

strain.on.health.c.points<-
  c(mean(fdat$strain.on.health.c)-
      sd(fdat$strain.on.health.c),
    mean(fdat$strain.on.health.c)-
      0*sd(fdat$strain.on.health.c),
    mean(fdat$strain.on.health.c)+
      sd(fdat$strain.on.health.c))

#conv

#low

mod3.antielite_salience.slopes.low<-
  emtrends(mod3.antielite_salience,~1|DV.f,
           var="antielite_salience.z",
           by="strain.on.health.c",
           at=list(strain.on.health.c=
                     strain.on.health.c.points[1],
                   gndr.c=0,
                   age.10.c=0),
           infer=c(T,T),
           mode="latent")

(mod3.antielite_salience.slopes.low.eff<-
  contrast(mod3.antielite_salience.slopes.low,
                                         simple="DV.f",
                                         adjust="none","eff",
           infer=c(T,T)))
```

```
## strain.on.health.c = -2.38:
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                 -0.0277 0.0398 Inf    -0.106    0.0503  -0.695
##  Used_CAM_ONLY effect       0.3153 0.0913 Inf     0.136    0.4943   3.453
##  Used_conv_and_CAM effect  -0.1791 0.0452 Inf    -0.268   -0.0905  -3.964
##  Used_conv_ONLY effect     -0.1086 0.0351 Inf    -0.177   -0.0398  -3.093
##  p.value
##   0.4868
##   0.0006
##   0.0001
##   0.0020
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#mid


mod3.antielite_salience.slopes.mid<-
  emtrends(mod3.antielite_salience,~1|DV.f,
           var="antielite_salience.z",
           by="strain.on.health.c",
           at=list(strain.on.health.c=
                     strain.on.health.c.points[2],
                   gndr.c=0,
                   age.10.c=0),
           infer=c(T,T),
           mode="latent")

(mod3.antielite_salience.slopes.mid.eff<-
  contrast(mod3.antielite_salience.slopes.mid,
                                         simple="DV.f",
                                         adjust="none","eff",
           infer=c(T,T)))
```

```
## strain.on.health.c = 0.0653:
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                 0.01627 0.0398 Inf   -0.0617    0.0942   0.409
##  Used_CAM_ONLY effect      0.01067 0.0878 Inf   -0.1614    0.1827   0.122
##  Used_conv_and_CAM effect -0.02806 0.0373 Inf   -0.1011    0.0450  -0.753
##  Used_conv_ONLY effect     0.00113 0.0323 Inf   -0.0622    0.0645   0.035
##  p.value
##   0.6825
##   0.9033
##   0.4515
##   0.9721
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#high


mod3.antielite_salience.slopes.high<-
  emtrends(mod3.antielite_salience,~1|DV.f,
           var="antielite_salience.z",
           by="strain.on.health.c",
           at=list(strain.on.health.c=
                     strain.on.health.c.points[3],
                   gndr.c=0,
                   age.10.c=0),
           infer=c(T,T),
           mode="latent")

(mod3.antielite_salience.slopes.high.eff<-
  contrast(mod3.antielite_salience.slopes.high,
                                         simple="DV.f",
                                         adjust="none","eff",
           infer=c(T,T)))
```

```
## strain.on.health.c = 2.51:
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                  0.0602 0.0786 Inf  -0.09380     0.214   0.766
##  Used_CAM_ONLY effect      -0.2940 0.1709 Inf  -0.62905     0.041  -1.720
##  Used_conv_and_CAM effect   0.1230 0.0643 Inf  -0.00307     0.249   1.912
##  Used_conv_ONLY effect      0.1108 0.0613 Inf  -0.00934     0.231   1.808
##  p.value
##   0.4435
##   0.0855
##   0.0558
##   0.0707
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file

export(rbind(data.frame(mod3.antielite_salience.slopes.low.eff),
             data.frame(mod3.antielite_salience.slopes.mid.eff),
             data.frame(mod3.antielite_salience.slopes.high.eff)),
       "../../results/Added_covariates/mod3.antielite_salience.slopes.eff.MN.xlsx")


# combined CM for low

(mod3.antielite_salience.slopes.low.CM<-
  contrast(mod3.antielite_salience.slopes.low,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod3.antielite_salience.slopes.low.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
## strain.on.health.c = -2.38:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv   -0.191 0.0434 Inf    -0.276    -0.106  -4.404  <.0001
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# combined CM for mid

(mod3.antielite_salience.slopes.mid.CM<-
  contrast(mod3.antielite_salience.slopes.mid,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod3.antielite_salience.slopes.mid.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
## strain.on.health.c = 0.0653:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv  -0.0252 0.0444 Inf    -0.112    0.0617  -0.569  0.5695
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# combined CM for high

(mod3.antielite_salience.slopes.high.CM<-
  contrast(mod3.antielite_salience.slopes.high,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod3.antielite_salience.slopes.high.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
## strain.on.health.c = 2.51:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.169 0.0901 Inf  -0.00746     0.346   1.877  0.0605
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Combined CAM for low


(mod3.antielite_salience.slopes.low.CAM<-
    contrast(mod3.antielite_salience.slopes.low,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod3.antielite_salience.slopes.low.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
## strain.on.health.c = -2.38:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM   0.0542 0.0507 Inf   -0.0452     0.154   1.069  0.2852
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Combined CAM for mid


(mod3.antielite_salience.slopes.mid.CAM<-
    contrast(mod3.antielite_salience.slopes.mid,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod3.antielite_salience.slopes.mid.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
## strain.on.health.c = 0.0653:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM  -0.0247 0.0395 Inf    -0.102    0.0528  -0.626  0.5315
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Combined CAM for high


(mod3.antielite_salience.slopes.high.CAM<-
    contrast(mod3.antielite_salience.slopes.high,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod3.antielite_salience.slopes.high.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
## strain.on.health.c = 2.51:
##  contrast     estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM  -0.0556 0.059 Inf    -0.171      0.06  -0.943  0.3458
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
export(data.frame(rbind(
  mod3.antielite_salience.slopes.low.CM,
  mod3.antielite_salience.slopes.mid.CM,
  mod3.antielite_salience.slopes.high.CM,
  mod3.antielite_salience.slopes.low.CAM,
  mod3.antielite_salience.slopes.mid.CAM,
  mod3.antielite_salience.slopes.high.CAM,adjust="none")),
  "../../results/Added_covariates/mod3.antielite_salience.slopes.eff.COMB.xlsx")
```

## Model for corrupt_salience


```r
mod3.corrupt_salience<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+blgetmg.c+mstatus.c+strain.on.health.c+corrupt_salience.z+
            strain.on.health.c:corrupt_salience.z,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28758.8 - criterion = 0.7765054
## Iteration 2 - deviance = 27473.04 - criterion = 0.1518134
## Iteration 3 - deviance = 27301.16 - criterion = 0.02739453
## Iteration 4 - deviance = 27262.43 - criterion = 0.01605653
## Iteration 5 - deviance = 27255.49 - criterion = 0.004591406
## Iteration 6 - deviance = 27254.27 - criterion = 0.0007224754
## Iteration 7 - deviance = 27253.95 - criterion = 4.692982e-05
## Iteration 8 - deviance = 27253.9 - criterion = 7.856353e-07
## Iteration 9 - deviance = 27253.89 - criterion = 7.620593e-10
## converged
```

```r
anova(mod2.corrupt_salience,
      mod3.corrupt_salience,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + corrupt_salience.z
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + corrupt_salience.z + strain.on.health.c:corrupt_salience.z
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
## 1     61716      27269                        
## 2     61713      27254  3   14.641  0.00215 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
#Alternative model with manually defined interaction
fdat$corrupt_salience.zXstrain.on.health.c<-
  fdat$corrupt_salience.z*fdat$strain.on.health.c

mod3.corrupt_salience.alt<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+blgetmg.c+mstatus.c+strain.on.health.c+corrupt_salience.z+
            corrupt_salience.zXstrain.on.health.c,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28758.8 - criterion = 0.7765054
## Iteration 2 - deviance = 27473.04 - criterion = 0.1518134
## Iteration 3 - deviance = 27301.16 - criterion = 0.02739453
## Iteration 4 - deviance = 27262.43 - criterion = 0.01605653
## Iteration 5 - deviance = 27255.49 - criterion = 0.004591406
## Iteration 6 - deviance = 27254.27 - criterion = 0.0007224754
## Iteration 7 - deviance = 27253.95 - criterion = 4.692982e-05
## Iteration 8 - deviance = 27253.9 - criterion = 7.856353e-07
## Iteration 9 - deviance = 27253.89 - criterion = 7.620593e-10
## converged
```

```r
mod3.corrupt_salience.alt.trends<-
  emtrends(mod3.corrupt_salience.alt,~1|DV.f,
           var="corrupt_salience.zXstrain.on.health.c",
           infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0))

#effects for each DV-category
(mod3.corrupt_salience.alt.eff<-
  contrast(mod3.corrupt_salience.alt.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))
```

```
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                  0.0587 0.0275 Inf   0.00488    0.1126   2.137
##  Used_CAM_ONLY effect      -0.2035 0.0577 Inf  -0.31662   -0.0905  -3.528
##  Used_conv_and_CAM effect   0.0802 0.0230 Inf   0.03522    0.1253   3.493
##  Used_conv_ONLY effect      0.0646 0.0211 Inf   0.02322    0.1059   3.060
##  p.value
##   0.0326
##   0.0004
##   0.0005
##   0.0022
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#save to file
export(data.frame(mod3.corrupt_salience.alt.eff),
       "../../results/Added_covariates/mod3.corrupt_salienceBYhealth.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod3.corrupt_salience.eff.CM<-
    contrast(mod3.corrupt_salience.alt.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod3.corrupt_salience.alt.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv   0.0973 0.0316 Inf    0.0353     0.159   3.074  0.0021
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Use of CAM

#weighted across all effects

(mod3.corrupt_salience.eff.CAM<-
    contrast(mod3.corrupt_salience.alt.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod3.corrupt_salience.alt.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM  -0.0458 0.0224 Inf   -0.0896  -0.00195  -2.047  0.0406
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file
export(data.frame(rbind(
  mod3.corrupt_salience.eff.CM,
  mod3.corrupt_salience.eff.CAM,adjust="none")),
  "../../results/Added_covariates/mod3.corrupt_salienceBYhealth.eff.COMB.xlsx")
```

### Probing simple slopes


```r
ref_grid(mod3.corrupt_salience)
```

```
## 'emmGrid' object with variables:
##     gndr.c = -0.5,  0.5
##     age10.c = 0.39152
##     income.fr = quint.5, missing, quint.1, quint.2, quint.3, quint.4
##     edu.f = 7. MA, 1. <LS, 2. LS, 3. LUS, 4. UUS, 5. AV, 6. BA
##     blgetmg.c = -0.5,  0.5
##     mstatus.c = -0.5,  0.5
##     strain.on.health.c = 0.065329
##     corrupt_salience.z = -0.00015779
##     DV.f = multivariate response levels: NN, Used_CAM_ONLY, Used_conv_and_CAM, Used_conv_ONLY
```

```r
#pick points for inference

strain.on.health.c.points<-
  c(mean(fdat$strain.on.health.c)-
      sd(fdat$strain.on.health.c),
    mean(fdat$strain.on.health.c)-
      0*sd(fdat$strain.on.health.c),
    mean(fdat$strain.on.health.c)+
      sd(fdat$strain.on.health.c))

#conv

#low

mod3.corrupt_salience.slopes.low<-
  emtrends(mod3.corrupt_salience,~1|DV.f,
           var="corrupt_salience.z",
           by="strain.on.health.c",
           at=list(strain.on.health.c=
                     strain.on.health.c.points[1],
                   gndr.c=0,
                   age.10.c=0),
           infer=c(T,T),
           mode="latent")

(mod3.corrupt_salience.slopes.low.eff<-
  contrast(mod3.corrupt_salience.slopes.low,
                                         simple="DV.f",
                                         adjust="none","eff",
           infer=c(T,T)))
```

```
## strain.on.health.c = -2.38:
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                  -0.173 0.0574 Inf    -0.285   -0.0606  -3.016
##  Used_CAM_ONLY effect        0.651 0.1311 Inf     0.394    0.9082   4.966
##  Used_conv_and_CAM effect   -0.246 0.0634 Inf    -0.370   -0.1216  -3.877
##  Used_conv_ONLY effect      -0.232 0.0501 Inf    -0.331   -0.1340  -4.633
##  p.value
##   0.0026
##   <.0001
##   0.0001
##   <.0001
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#mid


mod3.corrupt_salience.slopes.mid<-
  emtrends(mod3.corrupt_salience,~1|DV.f,
           var="corrupt_salience.z",
           by="strain.on.health.c",
           at=list(strain.on.health.c=
                     strain.on.health.c.points[2],
                   gndr.c=0,
                   age.10.c=0),
           infer=c(T,T),
           mode="latent")

(mod3.corrupt_salience.slopes.mid.eff<-
  contrast(mod3.corrupt_salience.slopes.mid,
                                         simple="DV.f",
                                         adjust="none","eff",
           infer=c(T,T)))
```

```
## strain.on.health.c = 0.0653:
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                 -0.0296 0.0549 Inf   -0.1372    0.0780  -0.539
##  Used_CAM_ONLY effect       0.1542 0.1168 Inf   -0.0748    0.3832   1.320
##  Used_conv_and_CAM effect  -0.0499 0.0505 Inf   -0.1489    0.0490  -0.989
##  Used_conv_ONLY effect     -0.0747 0.0435 Inf   -0.1600    0.0106  -1.716
##  p.value
##   0.5900
##   0.1869
##   0.3226
##   0.0862
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#high


mod3.corrupt_salience.slopes.high<-
  emtrends(mod3.corrupt_salience,~1|DV.f,
           var="corrupt_salience.z",
           by="strain.on.health.c",
           at=list(strain.on.health.c=
                     strain.on.health.c.points[3],
                   gndr.c=0,
                   age.10.c=0),
           infer=c(T,T),
           mode="latent")

(mod3.corrupt_salience.slopes.high.eff<-
  contrast(mod3.corrupt_salience.slopes.high,
                                         simple="DV.f",
                                         adjust="none","eff",
           infer=c(T,T)))
```

```
## strain.on.health.c = 2.51:
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                   0.114 0.1084 Inf   -0.0986    0.3262   1.050
##  Used_CAM_ONLY effect       -0.343 0.2232 Inf   -0.7802    0.0945  -1.536
##  Used_conv_and_CAM effect    0.146 0.0859 Inf   -0.0223    0.3143   1.701
##  Used_conv_ONLY effect       0.083 0.0811 Inf   -0.0760    0.2420   1.023
##  p.value
##   0.2936
##   0.1245
##   0.0890
##   0.3062
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# save to file

export(rbind(data.frame(mod3.corrupt_salience.slopes.low.eff),
             data.frame(mod3.corrupt_salience.slopes.mid.eff),
             data.frame(mod3.corrupt_salience.slopes.high.eff)),
       "../../results/Added_covariates/mod3.corrupt_salience.slopes.eff.MN.xlsx")


# combined CM for low

(mod3.corrupt_salience.slopes.low.CM<-
  contrast(mod3.corrupt_salience.slopes.low,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod3.corrupt_salience.slopes.low.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
## strain.on.health.c = -2.38:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv   -0.275 0.0625 Inf    -0.398    -0.153  -4.404  <.0001
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# combined CM for mid

(mod3.corrupt_salience.slopes.mid.CM<-
  contrast(mod3.corrupt_salience.slopes.mid,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod3.corrupt_salience.slopes.mid.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
## strain.on.health.c = 0.0653:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv  -0.0913 0.0617 Inf    -0.212    0.0295  -1.481  0.1387
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
# combined CM for high

(mod3.corrupt_salience.slopes.high.CM<-
  contrast(mod3.corrupt_salience.slopes.high,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod3.corrupt_salience.slopes.high.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))
```

```
## strain.on.health.c = 2.51:
##  contrast       estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.151 0.125 Inf   -0.0933     0.395   1.211  0.2261
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Combined CAM for low


(mod3.corrupt_salience.slopes.low.CAM<-
    contrast(mod3.corrupt_salience.slopes.low,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod3.corrupt_salience.slopes.low.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
## strain.on.health.c = -2.38:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    0.241 0.0713 Inf     0.102     0.381   3.387  0.0007
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Combined CAM for mid


(mod3.corrupt_salience.slopes.mid.CAM<-
    contrast(mod3.corrupt_salience.slopes.mid,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod3.corrupt_salience.slopes.mid.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
## strain.on.health.c = 0.0653:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM   0.0618 0.0538 Inf   -0.0437     0.167   1.148  0.2510
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
#Combined CAM for high


(mod3.corrupt_salience.slopes.high.CAM<-
    contrast(mod3.corrupt_salience.slopes.high,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod3.corrupt_salience.slopes.high.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))
```

```
## strain.on.health.c = 2.51:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM  -0.0478 0.0796 Inf    -0.204     0.108  -0.600  0.5485
## 
## Results are averaged over the levels of: income.fr, edu.f, blgetmg.c, mstatus.c 
## Confidence level used: 0.95
```

```r
export(data.frame(rbind(
  mod3.corrupt_salience.slopes.low.CM,
  mod3.corrupt_salience.slopes.mid.CM,
  mod3.corrupt_salience.slopes.high.CM,
  mod3.corrupt_salience.slopes.low.CAM,
  mod3.corrupt_salience.slopes.mid.CAM,
  mod3.corrupt_salience.slopes.high.CAM,adjust="none")),
  "../../results/Added_covariates/mod3.corrupt_salience.slopes.eff.COMB.xlsx")
```


# Exploratory analysis

## Nonlinearity

### lrgen


```r
fdat$lrgen.z.1<-fdat$lrgen.z
fdat$lrgen.z.2<-fdat$lrgen.z^2
fdat$lrgen.z.3<-fdat$lrgen.z^3

mod4.lrgen<-mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+blgetmg.c+mstatus.c+strain.on.health.c+
                       lrgen.z+I(lrgen.z^2)+I(lrgen.z^3),
                     random= ~1|cntry,
                     estimator="ML",
                     data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28751.3 - criterion = 0.7766738
## Iteration 2 - deviance = 27466 - criterion = 0.1512593
## Iteration 3 - deviance = 27301.74 - criterion = 0.0253547
## Iteration 4 - deviance = 27265.29 - criterion = 0.01482657
## Iteration 5 - deviance = 27258.85 - criterion = 0.004440472
## Iteration 6 - deviance = 27257.55 - criterion = 0.0007196345
## Iteration 7 - deviance = 27257.24 - criterion = 6.236174e-05
## Iteration 8 - deviance = 27257.08 - criterion = 1.826371e-06
## Iteration 9 - deviance = 27257.05 - criterion = 2.142584e-08
## Iteration 10 - deviance = 27257.05 - criterion = 5.858737e-10
## converged
```

```r
anova(mod2.lrgen,mod4.lrgen,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + lrgen.z
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + lrgen.z + I(lrgen.z^2) + I(lrgen.z^3)
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1     61716      27270                       
## 2     61710      27257  6   12.717  0.04775 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


### lrecon


```r
fdat$lrecon.z.1<-fdat$lrecon.z
fdat$lrecon.z.2<-fdat$lrecon.z^2
fdat$lrecon.z.3<-fdat$lrecon.z^3

mod4.lrecon<-mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+blgetmg.c+mstatus.c+strain.on.health.c+
                       lrecon.z+I(lrecon.z^2)+I(lrecon.z^3),
                     random= ~1|cntry,
                     estimator="ML",
                     data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28758.09 - criterion = 0.7766579
## Iteration 2 - deviance = 27478.21 - criterion = 0.1513337
## Iteration 3 - deviance = 27314.02 - criterion = 0.02538887
## Iteration 4 - deviance = 27277.58 - criterion = 0.01477489
## Iteration 5 - deviance = 27271.02 - criterion = 0.004290805
## Iteration 6 - deviance = 27269.61 - criterion = 0.0007920585
## Iteration 7 - deviance = 27269.35 - criterion = 4.042682e-05
## Iteration 8 - deviance = 27269.19 - criterion = 1.557874e-06
## Iteration 9 - deviance = 27269.16 - criterion = 1.45076e-08
## Iteration 10 - deviance = 27269.16 - criterion = 3.988857e-10
## converged
```

```r
anova(mod2.lrecon,mod4.lrecon,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + lrecon.z
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + lrecon.z + I(lrecon.z^2) + I(lrecon.z^3)
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1     61716      27274                     
## 2     61710      27269  6   5.2665   0.5101
```



### galtan


```r
fdat$galtan.z.1<-fdat$galtan.z
fdat$galtan.z.2<-fdat$galtan.z^2
fdat$galtan.z.3<-fdat$galtan.z^3

mod4.galtan<-mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+
                       strain.on.health.c+
                       galtan.z+I(galtan.z^2)+I(galtan.z^3),
                     random= ~1|cntry,
                     estimator="ML",
                     data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28751.68 - criterion = 0.7769365
## Iteration 2 - deviance = 27464.4 - criterion = 0.1519962
## Iteration 3 - deviance = 27298.75 - criterion = 0.02582705
## Iteration 4 - deviance = 27262.8 - criterion = 0.01453847
## Iteration 5 - deviance = 27256.63 - criterion = 0.004240319
## Iteration 6 - deviance = 27255.72 - criterion = 0.0007174592
## Iteration 7 - deviance = 27255.2 - criterion = 4.159405e-05
## Iteration 8 - deviance = 27255.11 - criterion = 6.658853e-07
## Iteration 9 - deviance = 27255.1 - criterion = 9.680891e-10
## converged
```

```r
anova(mod2.galtan,mod4.galtan,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + galtan.z
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + strain.on.health.c + 
##     galtan.z + I(galtan.z^2) + I(galtan.z^3)
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1     61716      27265                     
## 2     61716      27255  0    10.19
```

```r
mod5.galtan<-mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+
                       strain.on.health.c+
                       galtan.z+I(galtan.z^2),
                     random= ~1|cntry,
                     estimator="ML",
                     data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28755.85 - criterion = 0.7767838
## Iteration 2 - deviance = 27469.42 - criterion = 0.1520203
## Iteration 3 - deviance = 27303.66 - criterion = 0.02593404
## Iteration 4 - deviance = 27267.78 - criterion = 0.01450915
## Iteration 5 - deviance = 27261.63 - criterion = 0.00421073
## Iteration 6 - deviance = 27260.72 - criterion = 0.0007172807
## Iteration 7 - deviance = 27260.2 - criterion = 4.156068e-05
## Iteration 8 - deviance = 27260.11 - criterion = 6.552596e-07
## Iteration 9 - deviance = 27260.1 - criterion = 8.357881e-10
## converged
```

```r
anova(mod5.galtan,mod4.galtan,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + strain.on.health.c + 
##     galtan.z + I(galtan.z^2)
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + strain.on.health.c + 
##     galtan.z + I(galtan.z^2) + I(galtan.z^3)
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1     61719      27260                     
## 2     61716      27255  3   4.9989   0.1719
```


#### Simple slopes


```r
#low
mod4.galtan.slopes.low<-
  emtrends(mod4.galtan,~1|DV.f,
           var="galtan.z",
           by="galtan.z",
           at=list(strain.on.health.c=0,
                   gndr.c=0,
                   age10.c=0,
                   galtan.z=c(-1)),
           infer=c(T,T),
           mode="latent")


(mod4.galtan.slopes.low.eff<-
  contrast(mod4.galtan.slopes.low,
         simple="DV.f",
         infer=c(T,T),
         adjust="none","eff"))
```

```
## galtan.z = -1:
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                  0.2630 0.0991 Inf    0.0687     0.457   2.654
##  Used_CAM_ONLY effect      -0.5350 0.1976 Inf   -0.9223    -0.148  -2.708
##  Used_conv_and_CAM effect  -0.0386 0.0984 Inf   -0.2314     0.154  -0.393
##  Used_conv_ONLY effect      0.3106 0.0802 Inf    0.1535     0.468   3.875
##  p.value
##   0.0080
##   0.0068
##   0.6947
##   0.0001
## 
## Results are averaged over the levels of: income.fr, edu.f 
## Confidence level used: 0.95
```

```r
#mid
mod4.galtan.slopes.mid<-
  emtrends(mod4.galtan,~1|DV.f,
           var="galtan.z",
           by="galtan.z",
           at=list(strain.on.health.c=0,
                   gndr.c=0,
                   age10.c=0,
                   galtan.z=c(0)),
           infer=c(T,T),
           mode="latent")


(mod4.galtan.slopes.mid.eff<-
  contrast(mod4.galtan.slopes.mid,
         simple="DV.f",
         infer=c(T,T),
         adjust="none","eff"))
```

```
## galtan.z = 0:
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                  0.0269 0.0602 Inf   -0.0912     0.145   0.446
##  Used_CAM_ONLY effect      -0.1283 0.1392 Inf   -0.4012     0.145  -0.922
##  Used_conv_and_CAM effect   0.0396 0.0605 Inf   -0.0790     0.158   0.654
##  Used_conv_ONLY effect      0.0618 0.0517 Inf   -0.0395     0.163   1.196
##  p.value
##   0.6553
##   0.3567
##   0.5129
##   0.2318
## 
## Results are averaged over the levels of: income.fr, edu.f 
## Confidence level used: 0.95
```

```r
#high
mod4.galtan.slopes.high<-
  emtrends(mod4.galtan,~1|DV.f,
           var="galtan.z",
           by="galtan.z",
           at=list(strain.on.health.c=0,
                   gndr.c=0,
                   age10.c=0,
                   galtan.z=c(1)),
           infer=c(T,T),
           mode="latent")


(mod4.galtan.slopes.high.eff<-
  contrast(mod4.galtan.slopes.high,
         simple="DV.f",
         infer=c(T,T),
         adjust="none","eff"))
```

```
## galtan.z = 1:
##  contrast                 estimate     SE  df asymp.LCL asymp.UCL z.ratio
##  NN effect                  0.0499 0.0839 Inf   -0.1145    0.2142   0.595
##  Used_CAM_ONLY effect       0.3515 0.1880 Inf   -0.0171    0.7200   1.869
##  Used_conv_and_CAM effect  -0.1919 0.0916 Inf   -0.3714   -0.0123  -2.095
##  Used_conv_ONLY effect     -0.2094 0.0727 Inf   -0.3519   -0.0670  -2.882
##  p.value
##   0.5520
##   0.0616
##   0.0362
##   0.0040
## 
## Results are averaged over the levels of: income.fr, edu.f 
## Confidence level used: 0.95
```

```r
#save to file

export(
  data.frame(
  rbind(
    mod4.galtan.slopes.low.eff,
    mod4.galtan.slopes.mid.eff,
    mod4.galtan.slopes.high.eff,adjust="none")),
  "../../results/Added_covariates/mod4.galtan.nonlinear.slopes.MN.xlsx")

# CM low

(mod4.galtan.slopes.low.eff.CM<-
    contrast(mod4.galtan.slopes.low,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod4.galtan.slopes.low.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f",infer=c(T,T)))
```

```
## galtan.z = -1:
##  contrast       estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv    0.162 0.114 Inf   -0.0621     0.386   1.417  0.1566
## 
## Results are averaged over the levels of: income.fr, edu.f 
## Confidence level used: 0.95
```

```r
# CM mid

(mod4.galtan.slopes.mid.eff.CM<-
    contrast(mod4.galtan.slopes.mid,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod4.galtan.slopes.mid.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f",infer=c(T,T)))
```

```
## galtan.z = 0:
##  contrast       estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv   0.0681 0.0667 Inf   -0.0626     0.199   1.021  0.3074
## 
## Results are averaged over the levels of: income.fr, edu.f 
## Confidence level used: 0.95
```

```r
# CM high

(mod4.galtan.slopes.high.eff.CM<-
    contrast(mod4.galtan.slopes.high,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod4.galtan.slopes.high.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f",infer=c(T,T)))
```

```
## galtan.z = 1:
##  contrast       estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
##  Conv - No conv   -0.332 0.093 Inf    -0.514     -0.15  -3.571  0.0004
## 
## Results are averaged over the levels of: income.fr, edu.f 
## Confidence level used: 0.95
```

```r
#CAM low

(mod4.galtan.slopes.low.eff.CAM<-
    contrast(mod4.galtan.slopes.low,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod4.galtan.slopes.low.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f",infer=c(T,T)))
```

```
## galtan.z = -1:
##  contrast     estimate   SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    -0.48 0.11 Inf    -0.696    -0.265  -4.376  <.0001
## 
## Results are averaged over the levels of: income.fr, edu.f 
## Confidence level used: 0.95
```

```r
#CAM mid

(mod4.galtan.slopes.mid.eff.CAM<-
    contrast(mod4.galtan.slopes.mid,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod4.galtan.slopes.mid.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f",infer=c(T,T)))
```

```
## galtan.z = 0:
##  contrast     estimate     SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM  -0.0541 0.0656 Inf    -0.183    0.0745  -0.825  0.4096
## 
## Results are averaged over the levels of: income.fr, edu.f 
## Confidence level used: 0.95
```

```r
#CAM high

(mod4.galtan.slopes.high.eff.CAM<-
    contrast(mod4.galtan.slopes.high,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod4.galtan.slopes.high.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f",infer=c(T,T)))
```

```
## galtan.z = 1:
##  contrast     estimate    SE  df asymp.LCL asymp.UCL z.ratio p.value
##  CAM - No CAM    0.082 0.103 Inf    -0.119     0.283   0.799  0.4241
## 
## Results are averaged over the levels of: income.fr, edu.f 
## Confidence level used: 0.95
```

```r
export(data.frame(rbind(mod4.galtan.slopes.low.eff.CM,
      mod4.galtan.slopes.mid.eff.CM,
      mod4.galtan.slopes.high.eff.CM,
      mod4.galtan.slopes.low.eff.CAM,
      mod4.galtan.slopes.mid.eff.CAM,
      mod4.galtan.slopes.high.eff.CAM,adjust="none")),
      "../../results/Added_covariates/mod4.galtan.nonlinear.slopes.COMB.xlsx")
```

### antielite_salience


```r
fdat$antielite_salience.z.1<-fdat$antielite_salience.z
fdat$antielite_salience.z.2<-fdat$antielite_salience.z^2
fdat$antielite_salience.z.3<-fdat$antielite_salience.z^3

mod4.antielite_salience<-
  mblogit(DV.f~gndr.c+age10.c+
            income.fr+edu.f+
            strain.on.health.c+
            antielite_salience.z+
            I(antielite_salience.z^2)+
            I(antielite_salience.z^3),
                     random= ~1|cntry,
                     estimator="ML",
                     data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28774.8 - criterion = 0.7768219
## Iteration 2 - deviance = 27487.82 - criterion = 0.1520096
## Iteration 3 - deviance = 27325.49 - criterion = 0.02480423
## Iteration 4 - deviance = 27290.08 - criterion = 0.01403432
## Iteration 5 - deviance = 27283.8 - criterion = 0.004267211
## Iteration 6 - deviance = 27282.64 - criterion = 0.0007076311
## Iteration 7 - deviance = 27282.42 - criterion = 6.20945e-05
## Iteration 8 - deviance = 27282.29 - criterion = 1.489239e-06
## Iteration 9 - deviance = 27282.26 - criterion = 2.131905e-08
## Iteration 10 - deviance = 27282.26 - criterion = 5.948986e-10
## converged
```

```r
anova(mod2.antielite_salience,
      mod4.antielite_salience,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + antielite_salience.z
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + strain.on.health.c + 
##     antielite_salience.z + I(antielite_salience.z^2) + I(antielite_salience.z^3)
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1     61716      27275                     
## 2     61716      27282  0  -6.8174
```



### corrupt_salience


```r
fdat$corrupt_salience.z.1<-fdat$corrupt_salience.z
fdat$corrupt_salience.z.2<-fdat$corrupt_salience.z^2
fdat$corrupt_salience.z.3<-fdat$corrupt_salience.z^3

mod4.corrupt_salience<-
  mblogit(DV.f~gndr.c+age10.c+
            income.fr+edu.f+blgetmg.c+mstatus.c+
            strain.on.health.c+
            corrupt_salience.z+
            I(corrupt_salience.z^2)+
            I(corrupt_salience.z^3),
                     random= ~1|cntry,
                     estimator="ML",
                     data=fdat,weights=anweight)
```

```
## 
## Iteration 1 - deviance = 28754.36 - criterion = 0.7764385
## Iteration 2 - deviance = 27470.25 - criterion = 0.1517693
## Iteration 3 - deviance = 27303.65 - criterion = 0.02641782
## Iteration 4 - deviance = 27266.8 - criterion = 0.01522155
## Iteration 5 - deviance = 27260.12 - criterion = 0.004250024
## Iteration 6 - deviance = 27258.41 - criterion = 0.0007760618
## Iteration 7 - deviance = 27258.93 - criterion = 0.0002084698
## Iteration 8 - deviance = 27258.9 - criterion = 2.234206e-06
## Iteration 9 - deviance = 27258.88 - criterion = 3.96691e-09
## converged
```

```r
anova(mod2.corrupt_salience,
      mod4.corrupt_salience,test="Chisq")
```

```
## Warning in anova.mclogitlist(c(list(object), dotargs), dispersion =
## dispersion, : Results are unreliable, since deviances from quasi-likelihoods
## are not comparable.
```

```
## Analysis of Deviance Table
## 
## Model 1: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + corrupt_salience.z
## Model 2: DV.f ~ gndr.c + age10.c + income.fr + edu.f + blgetmg.c + mstatus.c + 
##     strain.on.health.c + corrupt_salience.z + I(corrupt_salience.z^2) + 
##     I(corrupt_salience.z^3)
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1     61716      27269                     
## 2     61710      27259  6   9.6508   0.1402
```

# Print out custom functions


```r
# for obtaining variables labels from ESS .sav files
get.ESS.label<-function(var){
  attr(var,which = "label")
}


contrast.weights.total<-function(effects,signs){
  
  d.eff<-data.frame(effects)
  d.eff$inv.var<-1/(d.eff$SE^2)
  
  weights<-d.eff$inv.var/sum(d.eff$inv.var)
  
  output<-weights*signs
  return(output)
  
}

# variance inflation factor of predictors in multilevel models
vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
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
##  [1] MetBrewer_0.2.0  lme4_1.1-31      ggpubr_0.4.0     ggplot2_3.4.0   
##  [5] psych_2.2.3      memisc_0.99.30.7 MASS_7.3-58.1    lattice_0.20-45 
##  [9] emmeans_1.8.4-1  mclogit_0.9.4.2  Matrix_1.5-0     dplyr_1.1.0     
## [13] sjlabelled_1.2.0 rio_0.5.29       knitr_1.39       rmarkdown_2.15  
## 
## loaded via a namespace (and not attached):
##  [1] sass_0.4.1         tidyr_1.2.1        jsonlite_1.8.4    
##  [4] splines_4.2.2      carData_3.0-5      bslib_0.3.1       
##  [7] highr_0.9          cellranger_1.1.0   yaml_2.3.5        
## [10] backports_1.4.1    pillar_1.8.1       glue_1.6.2        
## [13] digest_0.6.31      ggsignif_0.6.3     minqa_1.2.5       
## [16] colorspace_2.0-3   sandwich_3.0-1     cowplot_1.1.1     
## [19] htmltools_0.5.2    pkgconfig_2.0.3    broom_1.0.2       
## [22] haven_2.5.0        purrr_1.0.1        xtable_1.8-4      
## [25] mvtnorm_1.1-3      scales_1.2.1       openxlsx_4.2.5    
## [28] tzdb_0.3.0         tibble_3.1.8       mgcv_1.8-41       
## [31] farver_2.1.1       generics_0.1.3     car_3.0-12        
## [34] ellipsis_0.3.2     TH.data_1.1-1      withr_2.5.0       
## [37] repr_1.1.4         cli_3.6.0          mnormt_2.1.1      
## [40] survival_3.4-0     magrittr_2.0.3     readxl_1.4.0      
## [43] estimability_1.4.1 evaluate_0.20      fansi_1.0.4       
## [46] nlme_3.1-160       rstatix_0.7.0      forcats_0.5.1     
## [49] foreign_0.8-83     tools_4.2.2        data.table_1.14.2 
## [52] hms_1.1.1          lifecycle_1.0.3    multcomp_1.4-20   
## [55] stringr_1.5.0      munsell_0.5.0      zip_2.2.0         
## [58] compiler_4.2.2     jquerylib_0.1.4    rlang_1.0.6       
## [61] nloptr_2.0.3       grid_4.2.2         rstudioapi_0.13   
## [64] labeling_0.4.2     base64enc_0.1-3    boot_1.3-28       
## [67] gtable_0.3.1       codetools_0.2-18   abind_1.4-5       
## [70] curl_4.3.2         R6_2.5.1           zoo_1.8-10        
## [73] fastmap_1.1.0      utf8_1.2.3         insight_0.18.8    
## [76] readr_2.1.2        stringi_1.7.12     parallel_4.2.2    
## [79] Rcpp_1.0.10        vctrs_0.5.2        tidyselect_1.2.0  
## [82] xfun_0.30          coda_0.19-4
```
