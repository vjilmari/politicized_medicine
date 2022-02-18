#' ---
#' title: "Descriptive statistics, missing values, correlations etc."
#' output: 
#'   html_document: 
#'     toc: yes
#'     keep_md: yes
#'     toc_depth: 5
#'     number_sections: yes
#' ---
#' 
## ----setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # Preparations
#' 
#' ## Packages
#' 
## ----------------------------------------------------------------------------
library(dplyr)
library(rio)
library(psych)
library(Hmisc)

#' ## Dataset
#' 
## ----------------------------------------------------------------------------
dat<-import("../../data/processed/fdat.xlsx")
str(dat)

#' 
#' ## Data transformations for descriptives
#' 
#' 
#' ### Income
#' 
#' (reference group needs to be redefined)
#' 
## ----------------------------------------------------------------------------
dat$income.f<-case_when(
  is.na(dat$income) ~ "missing",
  TRUE ~ dat$income
)

#define reference level (top quintile)
table(dat$income.f,useNA="always")
dat$income.fr = relevel(as.factor(dat$income.f), ref="quint.5")
table(dat$income.fr,useNA="always")

#' 
#' ### Education
#' 
#' (reference group needs to be redefined)
#' 
## ----------------------------------------------------------------------------
table(dat$edu,useNA="always")
dat$edu.f<-relevel(as.factor(dat$edu),ref="7. MA")
table(dat$edu.f,useNA="always")

#' 
#' ### DV
#' 
## ----------------------------------------------------------------------------
table(dat$DV,useNA="always")
dat$DV.f<-relevel(as.factor(dat$DV),ref="NN")
table(dat$DV.f,useNA="always")

table(dat$DV.no.home,useNA="always")
dat$DV.no.home.f<-relevel(as.factor(dat$DV.no.home),ref="NN")
table(dat$DV.no.home.f,useNA="always")

#' 
#' ### Strain on health and political orientation
#' 
#' 
#' 
## ----------------------------------------------------------------------------
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

#' 
#' # Calculations as they appear in text
#' 
## ----------------------------------------------------------------------------
table(dat$cntry!="EE" & dat$cntry!="IL")

dat$has.PO<-
  ifelse(is.na(dat$lrgen) & 
        is.na(dat$lrecon) & 
        is.na(dat$galtan) & 
        is.na(dat$antielite_salience) & 
        is.na(dat$corrupt_salience),0,1)


table(dat[dat$cntry!="EE" & dat$cntry!="IL","has.PO"])
prop.table(table(dat[dat$cntry!="EE" & dat$cntry!="IL","has.PO"]))

dat$has.PO.and.cntry<-
  ifelse(dat$has.PO & dat$cntry!="EE" & dat$cntry!="IL",1,0)


dat$has.covariates<-
  ifelse(is.na(dat$gndr.c) |
           is.na(dat$age10.c) |
           is.na(dat$income.fr) |
           is.na(dat$edu.f) |
           is.na(dat$strain.on.health),0,1)

table(dat[dat$has.PO.and.cntry==1,"has.covariates"])

table(is.na(dat[dat$has.PO.and.cntry==1,"income"]))

#' 
#' # Calculate a frame of missingness that is general or specific for each variable
#' 
## ----------------------------------------------------------------------------
table(is.na(dat$cntry))
table(is.na(dat$gndr.c))
table(is.na(dat$age10.c))
table(is.na(dat$income))
table(is.na(dat$edu.f))
table(is.na(dat$strain.on.health.c))
table(is.na(dat$DV))

table(is.na(dat$lrgen) & 
        is.na(dat$lrecon) & 
        is.na(dat$galtan) & 
        is.na(dat$antielite_salience) & 
        is.na(dat$corrupt_salience))

#' 
#' 
#' ## Exclude missing variable
#' 
## ----------------------------------------------------------------------------

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

#' 
#' ## Construct anweight variable for weighting
#' 
## ----------------------------------------------------------------------------
fdat$anweight=fdat$pspwght*fdat$pweight

#' 
#' # Table with DV categories and covariates
#' 
#' Calculations with (a) and without (b) weights
#' 
## ----------------------------------------------------------------------------

table(fdat$DV,useNA="always")
prop.table(100*table(fdat$DV,useNA="always"))


(a<-count(x = fdat, DV))
(b<-count(x = fdat, DV, wt = anweight))

sum(a$n)
sum(b$n)

round(100*a$n/sum(a$n),1)
round(100*b$n/sum(b$n),1)

round(100*prop.table(table(fdat$gndr.c,fdat$DV),1),1)
round(100*prop.table(table(fdat$income.fr,fdat$DV),1),1)
round(100*prop.table(table(fdat$edu.f,fdat$DV),1),1)


fdat %>%
    group_by(gndr.c,DV) %>%
    count(wt=anweight) %>%
  ungroup() %>%
  group_by(gndr.c) %>%
  summarise(perc=round(100*n/sum(n),1)) %>%
  t()

fdat %>%
    group_by(gndr.c,DV) %>%
    count() %>%
  ungroup() %>%
  group_by(gndr.c) %>%
  summarise(perc=round(100*n/sum(n),1)) %>%
  t()


#' 
#' ## Cross-tabs of DV categories
#' 
## ----------------------------------------------------------------------------

(ct.use<-table(fdat$used.conv,fdat$used.CAM,dnn = c("CM","CAM")))
(pt.use<-round(100*prop.table(ct.use),1))


(t.CAM.use<-table(fdat$used.CAM))
(pt.CAM.use<-round(100*prop.table(t.CAM.use),1))

(t.conv.use<-table(fdat$used.conv))
(pt.conv.use<-round(100*prop.table(t.conv.use),1))


round(100*prop.table(ct.use,margin = c(1)),1)
round(100*prop.table(ct.use,margin = c(2)),1)

#' 
#' # Correlations between variables
#' 
## ----------------------------------------------------------------------------
#recode categorical variables to numeric

#education
table(fdat$edu)
fdat$edu.ord<-as.numeric(substr(fdat$edu,1,1))
table(fdat$edu.ord)

#income
table(fdat$income)
fdat$income.ord<-as.numeric(substr(fdat$income,7,7))
table(fdat$income.ord)

#income missing
table(fdat$income.fr)
fdat$income.missing<-ifelse(is.na(fdat$income),1,0)
table(fdat$income.missing)


#correlation variables
cor.vars<-
  c("gndr.c","age10.c","edu.ord","income.ord","income.missing",
    "strain.on.health.c",
    "lrgen.z","lrecon.z","galtan.z",
    "antielite_salience.z","corrupt_salience.z")

psych::describe(fdat[,cor.vars],
                fast=T)

#pearson correlations
pearson<-
  corr.test(fdat[,cor.vars],
            method = "pearson",adjust = "none")

export(pearson$r,
       "../../results/cors.pearson.r.xlsx",
       overwrite=T)

#spearman correlations
spearman<-
  corr.test(fdat[,cor.vars],
            method = "spearman",adjust = "none")

export(spearman$r,"../../results/cors.spearman.r.xlsx",
       overwrite=T)


weighted_corr <- 
  cov.wt(fdat[complete.cases(fdat[,cor.vars]),
                             cor.vars],
         wt = fdat[complete.cases(fdat[,cor.vars]),
                   "anweight"], cor = TRUE)
corr_matrix <- weighted_corr$cor
round(corr_matrix,2)

export(corr_matrix,
       "../../results/cors.weighted.pearson.r.xlsx",
       overwrite=T)

corr_matrix.t<-
  (corr_matrix*sqrt(weighted_corr$n.obs-2))/sqrt(1-corr_matrix^2)

corr_matrix.p<-
  2*(1-pt(abs(corr_matrix.t),df=weighted_corr$n.obs-2))
round(corr_matrix.p,3)

export(corr_matrix.p,
       "../../results/cors.weighted.pearson.r.p.xlsx",
       overwrite=T)


#' 
#' # Weighted descriptive statistics
#' 
## ----------------------------------------------------------------------------
#gender
(round(weighted.mean(fdat$gndr.c,w=fdat$anweight),2)->gndr.mean.wt)
(round(sqrt(wtd.var(fdat$gndr.c,w=fdat$anweight)),2)->gndr.sd.wt)

#age
(round(weighted.mean(fdat$agea,w=fdat$anweight),2)->agea.mean.wt)
(round(sqrt(wtd.var(fdat$agea,w=fdat$anweight)),2)->agea.sd.wt)

#education
(round(weighted.mean(fdat$edu.ord,w=fdat$anweight),2)->edu.ord.mean.wt)
(round(sqrt(wtd.var(fdat$edu.ord,w=fdat$anweight)),2)->edu.ord.sd.wt)

#income
(round(weighted.mean(fdat$income.ord,
                     w=fdat$anweight,na.rm=T),2)->income.ord.mean.wt)
(round(sqrt(wtd.var(fdat$income.ord,
                    w=fdat$anweight,na.rm=T)),2)->income.ord.sd.wt)

#strain on health
(round(weighted.mean(fdat$strain.on.health,
                     w=fdat$anweight),2)->strain.on.health.mean.wt)
(round(sqrt(wtd.var(fdat$strain.on.health,
                    w=fdat$anweight)),2)->strain.on.health.sd.wt)

#lrgen
(round(weighted.mean(fdat$lrgen,
                     w=fdat$anweight),2)->lrgen.mean.wt)
(round(sqrt(wtd.var(fdat$lrgen,
                    w=fdat$anweight)),2)->lrgen.sd.wt)

#lrecon
(round(weighted.mean(fdat$lrecon,
                     w=fdat$anweight),2)->lrecon.mean.wt)
(round(sqrt(wtd.var(fdat$lrecon,
                    w=fdat$anweight)),2)->lrecon.sd.wt)

#galtan
(round(weighted.mean(fdat$galtan,
                     w=fdat$anweight),2)->galtan.mean.wt)
(round(sqrt(wtd.var(fdat$galtan,
                    w=fdat$anweight)),2)->galtan.sd.wt)

#antielite_salience
(round(weighted.mean(fdat$antielite_salience,
                     w=fdat$anweight),2)->antielite_salience.mean.wt)
(round(sqrt(wtd.var(fdat$antielite_salience,
                    w=fdat$anweight)),2)->antielite_salience.sd.wt)

#corrupt_salience
(round(weighted.mean(fdat$corrupt_salience,
                     w=fdat$anweight),2)->corrupt_salience.mean.wt)
(round(sqrt(wtd.var(fdat$corrupt_salience,
                    w=fdat$anweight)),2)->corrupt_salience.sd.wt)


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

#' 
#' # Party count 
#' 
## ----------------------------------------------------------------------------

CHES<-import("../../data/processed/CHES_2014.vote.keys.combined.xlsx")

# total number of parties
nrow(CHES)

# parties per country
party.count<-
  CHES %>%
  filter(cntry!="EE" & cntry!="IL") %>%
  group_by(cntry) %>%
  summarise(n=n())

sum(party.count$n)
min(party.count$n)
median(party.count$n)
mean(party.count$n)
max(party.count$n)
nrow(party.count)


#' 
#' # Descriptives of PO-variables at the country-level
#' 
## ----------------------------------------------------------------------------
PO.cntry.desc<-
  psych::describe(CHES[,c("lrgen","lrecon",
                    "galtan","antielite_salience",
                    "corrupt_salience")])

export(cbind(PO=rownames(PO.cntry.desc),
      PO.cntry.desc),
       "../../results/PO.cntry.desc.xlsx",
       overwrite=T)


#' 
#' # Correlations between PO-variables at the country-level
#' 
## ----------------------------------------------------------------------------
cntry.PO.corrs<-
  corr.test(CHES[,c("lrgen","lrecon",
                    "galtan","antielite_salience",
                    "corrupt_salience")],adjust="none")

round(cntry.PO.corrs$r,2)

export(cntry.PO.corrs$r,
       "../../results/cntry.PO.corrs.r.xlsx",
       overwrite=T)

export(cntry.PO.corrs$p,
       "../../results/cntry.PO.corrs.p.xlsx",
       overwrite=T)

#' 
#' # Session information
#' 
## ----------------------------------------------------------------------------
s<-sessionInfo()
print(s,locale=F)

