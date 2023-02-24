#' ---
#' title: "Analysis"
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
## ----message=FALSE, warning=FALSE--------------------------------------------

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

#' ## Dataset
#' 
## ----------------------------------------------------------------------------
fdat<-import("../../data/processed/fdat.xlsx")

#' 
#' ## Data transformations
#' 
#' The reference levels for factorial variables need to be redefined, .xlsx format does not understand factor formats.
#' 
#' ### Income
#' 
## ----------------------------------------------------------------------------
fdat$income.f<-case_when(
  is.na(fdat$income) ~ "missing",
  TRUE ~ fdat$income
)

#define reference level (top quintile)
table(fdat$income.f,useNA="always")
fdat$income.fr = relevel(as.factor(fdat$income.f), ref="quint.5")
table(fdat$income.fr,useNA="always")

#' 
#' ### Education
#' 
## ----------------------------------------------------------------------------
table(fdat$edu,useNA="always")
fdat$edu.f<-relevel(as.factor(fdat$edu),ref="7. MA")
table(fdat$edu.f,useNA="always")

#' 
#' ### DV
#' 
## ----------------------------------------------------------------------------
table(fdat$DV,useNA="always")
fdat$DV.f<-relevel(as.factor(fdat$DV),ref="NN")
table(fdat$DV.f,useNA="always")

#' 
#' ### Strain on health and political orientation
#' 
#' Calculate country means for centering
#' 
## ----------------------------------------------------------------------------
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

#' 
#' ## Exclude missing variable
#' 
## ----------------------------------------------------------------------------

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


#' 
#' ## Construct anweight variable for weighting
#' 
## ----------------------------------------------------------------------------
fdat$anweight=fdat$pspwght*fdat$pweight

#' 
#' # Descriptive analysis
#' 
#' ## Check the presence of all DV-groups across countries
#' 
## ----------------------------------------------------------------------------
table(fdat$cntry,fdat$DV.f)
round(100*prop.table(table(fdat$cntry,fdat$DV.f),
                     margin = 1),1)

#' 
#' 
#' # Analysis
#' 
#' ## Empty model
#' 
## ----------------------------------------------------------------------------
mod0<-mblogit(DV.f~1,
              random= ~1|cntry,
              estimator="ML",
              data=fdat,weights=anweight)

mod0
summary(mod0)
mtable(mod0,show.baselevel = T)

#' 
#' ## Model with covariates
#' 
## ----------------------------------------------------------------------------
mod1<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+
            strain.on.health.c,
              random= ~1|cntry,
              estimator="ML",
              data=fdat,weights=anweight)

summary(mod1)
mod1
mtable(mod1,show.baselevel = T)

#' 
#' ### Strain on health main effects
#' 
## ----------------------------------------------------------------------------

mod1.strain.trends<-
  emtrends(mod1,~1|DV.f,
           var="strain.on.health.c",
           infer=T,mode="latent",
           at=list(gndr.c=0,age10.c=0))

#effects for each DV-category
(mod1.strain.eff<-
  contrast(mod1.strain.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))

#save to file
export(data.frame(mod1.strain.eff),
       "../../results/mod1.strain.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod1.strain.eff.CM<-
  contrast(mod1.strain.trends,
           method = list("Conv - No conv" = contrast.weights.total(effects=mod1.strain.eff,
          signs=c(-2,-2,2,2))),
           simple="DV.f",
           infer=c(T,T)))


(mod1.strain.eff.CAM<-
    contrast(mod1.strain.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod1.strain.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f",
           infer=c(T,T)))

# save to file
export(data.frame(rbind(mod1.strain.eff.CM,
      mod1.strain.eff.CAM,adjust="none")),
      "../../results/mod1.strain.eff.COMB.xlsx")


#' 
#' ### gender main effects
#' 
## ----------------------------------------------------------------------------

mod1.gndr.trends<-
  emtrends(mod1,~1|DV.f,
           var="gndr.c",
           infer=T,mode="latent",
           at=list(strain.on.health.c=0,age10.c=0))

#effects for each DV-category
(mod1.gndr.eff<-
  contrast(mod1.gndr.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))

#save to file
export(data.frame(mod1.gndr.eff),
       "../../results/mod1.gndr.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod1.gndr.eff.CM<-
  contrast(mod1.gndr.trends,
           method = list("Conv - No conv" = contrast.weights.total(effects=mod1.gndr.eff,
          signs=c(-2,-2,2,2))),
           simple="DV.f",
           infer=c(T,T)))


(mod1.gndr.eff.CAM<-
    contrast(mod1.gndr.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod1.gndr.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f",
           infer=c(T,T)))

# save to file
export(data.frame(rbind(mod1.gndr.eff.CM,
      mod1.gndr.eff.CAM,adjust="none")),
      "../../results/mod1.gndr.eff.COMB.xlsx")


#' 
#' 
#' ### age main effects
#' 
## ----------------------------------------------------------------------------

mod1.age10.trends<-
  emtrends(mod1,~1|DV.f,
           var="age10.c",
           infer=T,mode="latent",
           at=list(strain.on.health.c=0,gndr.c=0))

#effects for each DV-category
(mod1.age10.eff<-
  contrast(mod1.age10.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))

#save to file
export(data.frame(mod1.age10.eff),
       "../../results/mod1.age10.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod1.age10.eff.CM<-
  contrast(mod1.age10.trends,
           method = list("Conv - No conv" = contrast.weights.total(effects=mod1.age10.eff,
          signs=c(-2,-2,2,2))),
           simple="DV.f",
           infer=c(T,T)))


(mod1.age10.eff.CAM<-
    contrast(mod1.age10.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod1.age10.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f",
           infer=c(T,T)))

# save to file
export(data.frame(rbind(mod1.age10.eff.CM,
      mod1.age10.eff.CAM,adjust="none")),
      "../../results/mod1.age10.eff.COMB.xlsx")


#' 
#' 
#' ### education main effects
#' 
## ----------------------------------------------------------------------------
# first take probabilities for all categories
mod1.edu.f.trends<-emmeans(mod1,by="DV.f",
        specs="edu.f",
        infer=T,mode="prob",
        at=list(strain.on.health.c=0,gndr.c=0,age10.c=0))

mod1.edu.f.trends


export(data.frame(mod1.edu.f.trends),
       "../../results/mod1.edu.f.eff.MN.xlsx",
       overwrite=T)


#effects for each DV-category
(mod1.edu.f.eff<-
    contrast(mod1.edu.f.trends,#simple="edu.f",
             adjust="none","eff",infer=c(T,T)))

#save to file
export(data.frame(mod1.edu.f.eff),
       "../../results/mod1.edu.f.eff.MN.xlsx",
       overwrite=T)

# No Conv versus Conv

(mod1.edu.f.eff_DV<-
    contrast(mod1.edu.f.trends,simple="DV.f",
             adjust="none","eff",infer=c(T,T)))


(mod1.edu.f.eff.CM<-
    contrast(mod1.edu.f.trends,
             method = 
               list("Conv - No conv" = c(0,0,1,1)),
             simple="DV.f",
             infer=c(T,T)))

(mod1.edu.f.eff.CM_comp<-
  contrast(mod1.edu.f.eff.CM,
         simple="edu.f",method="eff",
         infer=c(T,T),adjust="none"))

# No CAM versus CAM


(mod1.edu.f.eff.CAM<-
    contrast(mod1.edu.f.trends,
             method = 
               list("CAM - No CAM" = c(0,1,1,0)),
             simple="DV.f",
             infer=c(T,T)))

(mod1.edu.f.eff.CAM_comp<-
    contrast(mod1.edu.f.eff.CAM,
             simple="edu.f",method="eff",
             infer=c(T,T),adjust="none"))


# save to file
export(data.frame(rbind(mod1.edu.f.eff.CM_comp,
                        mod1.edu.f.eff.CAM_comp,adjust="none")),
       "../../results/mod1.edu.f.eff.COMB_comp.xlsx",overwrite=T)

# save to file
export(data.frame(rbind(mod1.edu.f.eff.CM,
                        mod1.edu.f.eff.CAM,adjust="none")),
       "../../results/mod1.edu.f.eff.COMB.xlsx",overwrite=T)

#' 
#' 
#' ### income main effects
#' 
## ----------------------------------------------------------------------------
# first take probabilities for all categories
mod1.income.fr.trends<-emmeans(mod1,by="DV.f",
        specs="income.fr",
        infer=T,mode="prob",
        at=list(strain.on.health.c=0,gndr.c=0,age10.c=0))

mod1.income.fr.trends


export(data.frame(mod1.income.fr.trends),
       "../../results/mod1.income.fr.eff.MN.xlsx",
       overwrite=T)


#effects for each DV-category
(mod1.income.fr.eff<-
    contrast(mod1.income.fr.trends,#simple="income.fr",
             adjust="none","eff",infer=c(T,T)))

#save to file
export(data.frame(mod1.income.fr.eff),
       "../../results/mod1.income.fr.eff.MN.xlsx",
       overwrite=T)

# No Conv versus Conv

(mod1.income.fr.eff_DV<-
    contrast(mod1.income.fr.trends,simple="DV.f",
             adjust="none","eff",infer=c(T,T)))


(mod1.income.fr.eff.CM<-
    contrast(mod1.income.fr.trends,
             method = 
               list("Conv - No conv" = c(0,0,1,1)),
             simple="DV.f",
             infer=c(T,T)))

(mod1.income.fr.eff.CM_comp<-
  contrast(mod1.income.fr.eff.CM,
         simple="income.fr",method="eff",
         infer=c(T,T),adjust="none"))

# No CAM versus CAM


(mod1.income.fr.eff.CAM<-
    contrast(mod1.income.fr.trends,
             method = 
               list("CAM - No CAM" = c(0,1,1,0)),
             simple="DV.f",
             infer=c(T,T)))

(mod1.income.fr.eff.CAM_comp<-
    contrast(mod1.income.fr.eff.CAM,
             simple="income.fr",method="eff",
             infer=c(T,T),adjust="none"))


# save to file
export(data.frame(rbind(mod1.income.fr.eff.CM_comp,
                        mod1.income.fr.eff.CAM_comp,adjust="none")),
       "../../results/mod1.income.fr.eff.COMB_comp.xlsx",overwrite=T)

# save to file
export(data.frame(rbind(mod1.income.fr.eff.CM,
                        mod1.income.fr.eff.CAM,adjust="none")),
       "../../results/mod1.income.fr.eff.COMB.xlsx",overwrite=T)

#' 
#' ## Model for lrgen
#' 
## ----------------------------------------------------------------------------
mod2.lrgen<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+
            strain.on.health.c+
            lrgen.z,
                    random= ~1|cntry,
                    estimator="ML",
                    control = mmclogit.control(maxit = 250, trace=TRUE),
                    data=fdat,weights=anweight)

# general model comparison

anova(mod1,mod2.lrgen,test="Chisq")

# Is lrgen associated with using conventional medicine?
ref_grid(mod2.lrgen)
mod2.lrgen.trends<-
  emtrends(mod2.lrgen,~1|DV.f,
           var="lrgen.z",infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0,
                   strain.on.health.c=0))

#effects for each DV-category
(mod2.lrgen.eff<-
  contrast(mod2.lrgen.trends,simple="DV.f",
         adjust="none","eff", infer=c(T,T)))

#save to file
export(data.frame(mod2.lrgen.eff),
       "../../results/mod2.lrgen.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod2.lrgen.eff.CM<-
    contrast(mod2.lrgen.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod2.lrgen.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))

#Use of CAM

(mod2.lrgen.eff.CAM<-
    contrast(mod2.lrgen.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod2.lrgen.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))


# save to file
export(data.frame(rbind(
  mod2.lrgen.eff.CM,
  mod2.lrgen.eff.CAM,adjust="none")),
  "../../results/mod2.lrgen.eff.COMB.xlsx")

#' 
#' ## Model for lrecon
#' 
## ----------------------------------------------------------------------------
mod2.lrecon<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+
            strain.on.health.c+
            lrecon.z,
                    random= ~1|cntry,
                    estimator="ML",
                    control = mmclogit.control(maxit = 250, trace=TRUE),
                    data=fdat,weights=anweight)

# general model comparison

anova(mod1,mod2.lrecon,test="Chisq")

# Is lrecon associated with using conventional medicine?
ref_grid(mod2.lrecon)
mod2.lrecon.trends<-
  emtrends(mod2.lrecon,~1|DV.f,
           var="lrecon.z",infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0,
                   strain.on.health.c=0))

#effects for each DV-category
(mod2.lrecon.eff<-
  contrast(mod2.lrecon.trends,simple="DV.f",
         adjust="none","eff", infer=c(T,T)))

#save to file
export(data.frame(mod2.lrecon.eff),
       "../../results/mod2.lrecon.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod2.lrecon.eff.CM<-
    contrast(mod2.lrecon.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod2.lrecon.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))

#Use of CAM

(mod2.lrecon.eff.CAM<-
    contrast(mod2.lrecon.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod2.lrecon.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))


# save to file
export(data.frame(rbind(
  mod2.lrecon.eff.CM,
  mod2.lrecon.eff.CAM,adjust="none")),
  "../../results/mod2.lrecon.eff.COMB.xlsx")

#' 
#' ## Model for galtan
#' 
## ----------------------------------------------------------------------------
mod2.galtan<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+
            strain.on.health.c+
            galtan.z,
                    random= ~1|cntry,
                    estimator="ML",
                    control = mmclogit.control(maxit = 250, trace=TRUE),
                    data=fdat,weights=anweight)

# general model comparison

anova(mod1,mod2.galtan,test="Chisq")

# Is galtan associated with using conventional medicine?
ref_grid(mod2.galtan)
mod2.galtan.trends<-
  emtrends(mod2.galtan,~1|DV.f,
           var="galtan.z",infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0,
                   strain.on.health.c=0))

#effects for each DV-category
(mod2.galtan.eff<-
  contrast(mod2.galtan.trends,simple="DV.f",
         adjust="none","eff", infer=c(T,T)))

#save to file
export(data.frame(mod2.galtan.eff),
       "../../results/mod2.galtan.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod2.galtan.eff.CM<-
    contrast(mod2.galtan.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod2.galtan.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))

#Use of CAM

(mod2.galtan.eff.CAM<-
    contrast(mod2.galtan.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod2.galtan.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))


# save to file
export(data.frame(rbind(
  mod2.galtan.eff.CM,
  mod2.galtan.eff.CAM,adjust="none")),
  "../../results/mod2.galtan.eff.COMB.xlsx")

#' 
#' ### Contrast between CAM-only and CAM-and-conv against no-CAM
#' 
## ----------------------------------------------------------------------------

pairs(mod2.galtan.eff,adjust="none",infer=c(T,T))

#save to file

export(data.frame(pairs(mod2.galtan.eff,
                        adjust="none",
                        infer=c(T,T))),
  "../../results/mod2.galtan.eff.CAM.contrast.xlsx")


#' 
#' ## Model for antielite_salience
#' 
## ----------------------------------------------------------------------------
mod2.antielite_salience<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+
            strain.on.health.c+
            antielite_salience.z,
                    random= ~1|cntry,
                    estimator="ML",
                    control = mmclogit.control(maxit = 250, trace=TRUE),
                    data=fdat,weights=anweight)

# general model comparison

anova(mod1,mod2.antielite_salience,test="Chisq")

# Is antielite_salience associated with using conventional medicine?
ref_grid(mod2.antielite_salience)
mod2.antielite_salience.trends<-
  emtrends(mod2.antielite_salience,~1|DV.f,
           var="antielite_salience.z",infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0,
                   strain.on.health.c=0))

#effects for each DV-category
(mod2.antielite_salience.eff<-
  contrast(mod2.antielite_salience.trends,simple="DV.f",
         adjust="none","eff", infer=c(T,T)))

#save to file
export(data.frame(mod2.antielite_salience.eff),
       "../../results/mod2.antielite_salience.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod2.antielite_salience.eff.CM<-
    contrast(mod2.antielite_salience.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod2.antielite_salience.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))

#Use of CAM

(mod2.antielite_salience.eff.CAM<-
    contrast(mod2.antielite_salience.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod2.antielite_salience.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))


# save to file
export(data.frame(rbind(
  mod2.antielite_salience.eff.CM,
  mod2.antielite_salience.eff.CAM,adjust="none")),
  "../../results/mod2.antielite_salience.eff.COMB.xlsx")

#' 
#' ## Model for corrupt_salience
#' 
## ----------------------------------------------------------------------------
mod2.corrupt_salience<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+
            strain.on.health.c+
            corrupt_salience.z,
                    random= ~1|cntry,
                    estimator="ML",
                    control = mmclogit.control(maxit = 250, trace=TRUE),
                    data=fdat,weights=anweight)

# general model comparison

anova(mod1,mod2.corrupt_salience,test="Chisq")

# Is corrupt_salience associated with using conventional medicine?
ref_grid(mod2.corrupt_salience)
mod2.corrupt_salience.trends<-
  emtrends(mod2.corrupt_salience,~1|DV.f,
           var="corrupt_salience.z",infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0,
                   strain.on.health.c=0))

#effects for each DV-category
(mod2.corrupt_salience.eff<-
  contrast(mod2.corrupt_salience.trends,simple="DV.f",
         adjust="none","eff", infer=c(T,T)))

#save to file
export(data.frame(mod2.corrupt_salience.eff),
       "../../results/mod2.corrupt_salience.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod2.corrupt_salience.eff.CM<-
    contrast(mod2.corrupt_salience.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod2.corrupt_salience.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))

#Use of CAM

(mod2.corrupt_salience.eff.CAM<-
    contrast(mod2.corrupt_salience.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod2.corrupt_salience.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))


# save to file
export(data.frame(rbind(
  mod2.corrupt_salience.eff.CM,
  mod2.corrupt_salience.eff.CAM,adjust="none")),
  "../../results/mod2.corrupt_salience.eff.COMB.xlsx")

#' 
#' 
#' ### Contrast between CAM-only and CAM-and-conv against no-CAM
#' 
## ----------------------------------------------------------------------------

pairs(mod2.corrupt_salience.eff,
      adjust="none",infer=c(T,T))

#save to file

export(data.frame(pairs(mod2.corrupt_salience.eff,
                        adjust="none",
                        infer=c(T,T))),
  "../../results/mod2.corrupt_salience.eff.CAM.contrast.xlsx")

#' ## Figure for the main effects
#' 
## ----------------------------------------------------------------------------

# vectors where the predicted values are saved

preds.CM.lrgen<-list()
preds.CAM.lrgen<-list()

preds.CM.lrecon<-list()
preds.CAM.lrecon<-list()

preds.CM.galtan<-list()
preds.CAM.galtan<-list()

preds.CM.antielite_salience<-list()
preds.CAM.antielite_salience<-list()

preds.CM.corrupt_salience<-list()
preds.CAM.corrupt_salience<-list()

# Loop through each possible point and obtain prediction

range(c(fdat$lrgen.z,
        fdat$lrecon.z,
        fdat$galtan.z,
        fdat$antielite_salience.z,
        fdat$corrupt_salience.z))

focal.points.combined<-seq(from=-2.30,to=2.65,by=0.01)

for (i in 1:length(focal.points.combined)){
  
  # lrgen
  # estimates at certain focal points
  temp.point.lrgen<-
    emmeans(mod2.lrgen,~lrgen.z|DV.f,
            at=list(lrgen.z=
                      focal.points.combined[i],
                    strain.on.health.c=0,
                    gndr.c=0,
                    age.10.c=0),
            infer=c(T,T),
            mode="latent")
  
  # in contrast format
  temp.point.lrgen.eff<-
    contrast(temp.point.lrgen,
             simple="DV.f",
             adjust="none","eff",
             infer=c(T,T))
  
  # pooled for CM
  
  preds.CM.lrgen[[i]]<-
    data.frame(contrast(temp.point.lrgen,
                        method = list("Conv - No conv" = 
                                        contrast.weights.total(
                                          effects=
                                            temp.point.lrgen.eff,
                                          signs=c(-2,-2,2,2))),
                        simple="DV.f", infer=c(T,T)))
  
  # pooled for CAM
  
  preds.CAM.lrgen[[i]]<-
    data.frame(contrast(temp.point.lrgen,
                        method = list("CAM - No CAM" = 
                                        contrast.weights.total(
                                          effects=
                                            temp.point.lrgen.eff,
                                          signs=c(-2,2,2,-2))),
                        simple="DV.f", infer=c(T,T)))
  
  # lrecon
  # estimates at certain focal points
  temp.point.lrecon<-
    emmeans(mod2.lrecon,~lrecon.z|DV.f,
            at=list(lrecon.z=
                      focal.points.combined[i],
                    strain.on.health.c=0,
                    gndr.c=0,
                    age.10.c=0),
            infer=c(T,T),
            mode="latent")
  
  # in contrast format
  temp.point.lrecon.eff<-
    contrast(temp.point.lrecon,
             simple="DV.f",
             adjust="none","eff",
             infer=c(T,T))
  
  # pooled for CM
  
  preds.CM.lrecon[[i]]<-
    data.frame(contrast(temp.point.lrecon,
                        method = list("Conv - No conv" = 
                                        contrast.weights.total(
                                          effects=
                                            temp.point.lrecon.eff,
                                          signs=c(-2,-2,2,2))),
                        simple="DV.f", infer=c(T,T)))
  
  # pooled for CAM
  
  preds.CAM.lrecon[[i]]<-
    data.frame(contrast(temp.point.lrecon,
                        method = list("CAM - No CAM" = 
                                        contrast.weights.total(
                                          effects=
                                            temp.point.lrecon.eff,
                                          signs=c(-2,2,2,-2))),
                        simple="DV.f", infer=c(T,T)))
  
  # galtan
  # estimates at certain focal points
  temp.point.galtan<-
    emmeans(mod2.galtan,~galtan.z|DV.f,
            at=list(galtan.z=
                      focal.points.combined[i],
                    strain.on.health.c=0,
                    gndr.c=0,
                    age.10.c=0),
            infer=c(T,T),
            mode="latent")
  
  # in contrast format
  temp.point.galtan.eff<-
    contrast(temp.point.galtan,
             simple="DV.f",
             adjust="none","eff",
             infer=c(T,T))
  
  # pooled for CM
  
  preds.CM.galtan[[i]]<-
    data.frame(contrast(temp.point.galtan,
                        method = list("Conv - No conv" = 
                                        contrast.weights.total(
                                          effects=
                                            temp.point.galtan.eff,
                                          signs=c(-2,-2,2,2))),
                        simple="DV.f", infer=c(T,T)))
  
  # pooled for CAM
  
  preds.CAM.galtan[[i]]<-
    data.frame(contrast(temp.point.galtan,
                        method = list("CAM - No CAM" = 
                                        contrast.weights.total(
                                          effects=
                                            temp.point.galtan.eff,
                                          signs=c(-2,2,2,-2))),
                        simple="DV.f", infer=c(T,T)))
  
  # antielite_salience
  # estimates at certain focal points
  temp.point.antielite_salience<-
    emmeans(mod2.antielite_salience,~antielite_salience.z|DV.f,
            at=list(antielite_salience.z=
                      focal.points.combined[i],
                    strain.on.health.c=0,
                    gndr.c=0,
                    age.10.c=0),
            infer=c(T,T),
            mode="latent")
  
  # in contrast format
  temp.point.antielite_salience.eff<-
    contrast(temp.point.antielite_salience,
             simple="DV.f",
             adjust="none","eff",
             infer=c(T,T))
  
  # pooled for CM
  
  preds.CM.antielite_salience[[i]]<-
    data.frame(contrast(temp.point.antielite_salience,
                        method = list("Conv - No conv" = 
                                        contrast.weights.total(
                                          effects=
                                            temp.point.antielite_salience.eff,
                                          signs=c(-2,-2,2,2))),
                        simple="DV.f", infer=c(T,T)))
  
  # pooled for CAM
  
  preds.CAM.antielite_salience[[i]]<-
    data.frame(contrast(temp.point.antielite_salience,
                        method = list("CAM - No CAM" = 
                                        contrast.weights.total(
                                          effects=
                                            temp.point.antielite_salience.eff,
                                          signs=c(-2,2,2,-2))),
                        simple="DV.f", infer=c(T,T)))
  
  # corrupt_salience
  # estimates at certain focal points
  temp.point.corrupt_salience<-
    emmeans(mod2.corrupt_salience,~corrupt_salience.z|DV.f,
            at=list(corrupt_salience.z=
                      focal.points.combined[i],
                    strain.on.health.c=0,
                    gndr.c=0,
                    age.10.c=0),
            infer=c(T,T),
            mode="latent")
  
  # in contrast format
  temp.point.corrupt_salience.eff<-
    contrast(temp.point.corrupt_salience,
             simple="DV.f",
             adjust="none","eff",
             infer=c(T,T))
  
  # pooled for CM
  
  preds.CM.corrupt_salience[[i]]<-
    data.frame(contrast(temp.point.corrupt_salience,
                        method = list("Conv - No conv" = 
                                        contrast.weights.total(
                                          effects=
                                            temp.point.corrupt_salience.eff,
                                          signs=c(-2,-2,2,2))),
                        simple="DV.f", infer=c(T,T)))
  
  # pooled for CAM
  
  preds.CAM.corrupt_salience[[i]]<-
    data.frame(contrast(temp.point.corrupt_salience,
                        method = list("CAM - No CAM" = 
                                        contrast.weights.total(
                                          effects=
                                            temp.point.corrupt_salience.eff,
                                          signs=c(-2,2,2,-2))),
                        simple="DV.f", infer=c(T,T)))
  
  
  
}

# save predictions to data frames
preds.CM.lrgen.df<-do.call(rbind,preds.CM.lrgen)
preds.CM.lrgen.df$'Use of Medicine'<-"Conventional"
preds.CM.lrgen.df$PO<-"Left-right general"

preds.CAM.lrgen.df<-do.call(rbind,preds.CAM.lrgen)
preds.CAM.lrgen.df$'Use of Medicine'<-"Complementary/Alternative"
preds.CAM.lrgen.df$PO<-"Left-right general"


preds.CM.lrecon.df<-do.call(rbind,preds.CM.lrecon)
preds.CM.lrecon.df$'Use of Medicine'<-"Conventional"
preds.CM.lrecon.df$PO<-"Left-right economic"

preds.CAM.lrecon.df<-do.call(rbind,preds.CAM.lrecon)
preds.CAM.lrecon.df$'Use of Medicine'<-"Complementary/Alternative"
preds.CAM.lrecon.df$PO<-"Left-right economic"


preds.CM.galtan.df<-do.call(rbind,preds.CM.galtan)
preds.CM.galtan.df$'Use of Medicine'<-"Conventional"
preds.CM.galtan.df$PO<-"Gal-tan"

preds.CAM.galtan.df<-do.call(rbind,preds.CAM.galtan)
preds.CAM.galtan.df$'Use of Medicine'<-"Complementary/Alternative"
preds.CAM.galtan.df$PO<-"Gal-tan"


preds.CM.antielite_salience.df<-do.call(rbind,preds.CM.antielite_salience)
preds.CM.antielite_salience.df$'Use of Medicine'<-"Conventional"
preds.CM.antielite_salience.df$PO<-"Anti-elite"

preds.CAM.antielite_salience.df<-do.call(rbind,preds.CAM.antielite_salience)
preds.CAM.antielite_salience.df$'Use of Medicine'<-"Complementary/Alternative"
preds.CAM.antielite_salience.df$PO<-"Anti-elite"

preds.CM.corrupt_salience.df<-do.call(rbind,preds.CM.corrupt_salience)
preds.CM.corrupt_salience.df$'Use of Medicine'<-"Conventional"
preds.CM.corrupt_salience.df$PO<-"Anti-corruption"

preds.CAM.corrupt_salience.df<-do.call(rbind,preds.CAM.corrupt_salience)
preds.CAM.corrupt_salience.df$'Use of Medicine'<-"Complementary/Alternative"
preds.CAM.corrupt_salience.df$PO<-"Anti-corruption"

# combine the data
names(preds.CM.lrgen.df)[2]<-"point"
names(preds.CAM.lrgen.df)[2]<-"point"
names(preds.CM.lrecon.df)[2]<-"point"
names(preds.CAM.lrecon.df)[2]<-"point"
names(preds.CM.galtan.df)[2]<-"point"
names(preds.CAM.galtan.df)[2]<-"point"
names(preds.CM.antielite_salience.df)[2]<-"point"
names(preds.CAM.antielite_salience.df)[2]<-"point"
names(preds.CM.corrupt_salience.df)[2]<-"point"
names(preds.CAM.corrupt_salience.df)[2]<-"point"

preds.all<-rbind(preds.CM.lrgen.df,
                 preds.CAM.lrgen.df,
                 preds.CM.lrecon.df,
                 preds.CAM.lrecon.df,
                 preds.CM.galtan.df,
                 preds.CAM.galtan.df,
                 preds.CM.antielite_salience.df,
                 preds.CAM.antielite_salience.df,
                 preds.CM.corrupt_salience.df,
                 preds.CAM.corrupt_salience.df)

# transform to probabilities

preds.all$P.USE<-
  exp(preds.all$estimate)/(1+exp(preds.all$estimate))

preds.all$P.USE.LL<-
  exp(preds.all$asymp.LCL)/(1+exp(preds.all$asymp.LCL))

preds.all$P.USE.UL<-
  exp(preds.all$asymp.UCL)/(1+exp(preds.all$asymp.UCL))


# export Figure data

export(preds.all,
       "../../results/figures/Fig_main.xlsx",
       overwrite=T)

# produce the figure

# relevel 

preds.all$PO<-
  factor(preds.all$PO,
         levels=c("Left-right general","Left-right economic",
                  "Gal-tan","Anti-elite","Anti-corruption"))

# first separately for CM
Fig_CM<-
  ggplot(data=preds.all[preds.all$`Use of Medicine`=="Conventional",],
         aes(y=P.USE,x=point))+
  geom_line(size=2.75,color=met.brewer("Java")[1])+
  geom_line(size=1,color=met.brewer("Java")[1],
            linetype=2,
            aes(x=point,y=P.USE.LL))+
  geom_line(size=1,color=met.brewer("Java")[1],
            linetype=2,
            aes(x=point,y=P.USE.UL))+
  xlab("")+
  ylab("P(Use of Conventional Medicine)")+
  theme(text=element_text(size=16,  family="sans"))+
  facet_wrap(PO~.,ncol=1)

Fig_CM

Fig_CAM<-
  ggplot(data=preds.all[preds.all$`Use of Medicine`=="Complementary/Alternative",],
         aes(y=P.USE,x=point))+
  geom_line(size=2.75,color=met.brewer("Java")[5])+
  geom_line(size=1,color=met.brewer("Java")[5],
            linetype=2,
            aes(x=point,y=P.USE.LL))+
  geom_line(size=1,color=met.brewer("Java")[5],
            linetype=2,
            aes(x=point,y=P.USE.UL))+
  xlab("")+
  ylab("P(Use of Complementary/Alternative Medicine)")+
  theme(text=element_text(size=16,  family="sans"))+
  facet_wrap(PO~.,ncol=1)

Fig_CAM

Fig_main<-ggarrange(Fig_CM,Fig_CAM,ncol = 2)


jpeg(filename = 
       "../../results/figures/Fig_main.jpg",
     units = "cm",
     width = 21.0,height=29.7*(3/4),res = 600)
Fig_main
dev.off()

#' 
#' # Interactions by strain on health
#' 
#' ## Model for lrgen
#' 
## ----------------------------------------------------------------------------
mod3.lrgen<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+strain.on.health.c+lrgen.z+
            strain.on.health.c:lrgen.z,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)

anova(mod2.lrgen,mod3.lrgen,test="Chisq")

#Alternative model with manually defined interaction
fdat$lrgen.zXstrain.on.health.c<-fdat$lrgen.z*fdat$strain.on.health.c

mod3.lrgen.alt<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+strain.on.health.c+lrgen.z+
            lrgen.zXstrain.on.health.c,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)


mod3.lrgen.alt.trends<-
  emtrends(mod3.lrgen.alt,~1|DV.f,
           var="lrgen.zXstrain.on.health.c",
           infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0))

#effects for each DV-category
(mod3.lrgen.alt.eff<-
  contrast(mod3.lrgen.alt.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))

#save to file
export(data.frame(mod3.lrgen.alt.eff),
       "../../results/mod3.lrgenBYhealth.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod3.lrgen.eff.CM<-contrast(mod3.lrgen.alt.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod3.lrgen.alt.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))

#Use of CAM

#weighted across all effects

(mod3.lrgen.eff.CAM<-contrast(mod3.lrgen.alt.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod3.lrgen.alt.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))

# save to file
export(data.frame(rbind(
  mod3.lrgen.eff.CM,
  mod3.lrgen.eff.CAM,adjust="none")),
  "../../results/mod3.lrgenBYhealth.eff.COMB.xlsx")



#' 
#' 
#' ## Model for lrecon
#' 
## ----------------------------------------------------------------------------
mod3.lrecon<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+strain.on.health.c+lrecon.z+
            strain.on.health.c:lrecon.z,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)

anova(mod2.lrecon,mod3.lrecon,test="Chisq")

#Alternative model with manually defined interaction
fdat$lrecon.zXstrain.on.health.c<-fdat$lrecon.z*fdat$strain.on.health.c

mod3.lrecon.alt<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+strain.on.health.c+lrecon.z+
            lrecon.zXstrain.on.health.c,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)


mod3.lrecon.alt.trends<-
  emtrends(mod3.lrecon.alt,~1|DV.f,
           var="lrecon.zXstrain.on.health.c",
           infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0))

#effects for each DV-category
(mod3.lrecon.alt.eff<-
  contrast(mod3.lrecon.alt.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))

#save to file
export(data.frame(mod3.lrecon.alt.eff),
       "../../results/mod3.lreconBYhealth.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod3.lrecon.eff.CM<-contrast(mod3.lrecon.alt.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod3.lrecon.alt.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))

#Use of CAM

#weighted across all effects

(mod3.lrecon.eff.CAM<-contrast(mod3.lrecon.alt.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod3.lrecon.alt.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))

# save to file
export(data.frame(rbind(
  mod3.lrecon.eff.CM,
  mod3.lrecon.eff.CAM,adjust="none")),
  "../../results/mod3.lreconBYhealth.eff.COMB.xlsx")

#' 
#' 
#' ## Model for galtan
#' 
#' (Here it was necessary to change the epsilon convergence criterion 1e-04 instead of 1e-08)
#' 
## ----------------------------------------------------------------------------
mod3.galtan<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+strain.on.health.c+galtan.z+
            strain.on.health.c:galtan.z,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             epsilon=0.0001,
                             trace=TRUE),
          data=fdat,weights=anweight)



anova(mod2.galtan,mod3.galtan,test="Chisq")

#Alternative model with manually defined interaction
fdat$galtan.zXstrain.on.health.c<-fdat$galtan.z*fdat$strain.on.health.c

mod3.galtan.alt<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+strain.on.health.c+galtan.z+
            galtan.zXstrain.on.health.c,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE,
                             epsilon=0.0001),
          data=fdat,weights=anweight)


mod3.galtan.alt.trends<-
  emtrends(mod3.galtan.alt,~1|DV.f,
           var="galtan.zXstrain.on.health.c",
           infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0))

#effects for each DV-category
(mod3.galtan.alt.eff<-
  contrast(mod3.galtan.alt.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))

#save to file
export(data.frame(mod3.galtan.alt.eff),
       "../../results/mod3.galtanBYhealth.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod3.galtan.eff.CM<-contrast(mod3.galtan.alt.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod3.galtan.alt.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))

#Use of CAM

#weighted across all effects

(mod3.galtan.eff.CAM<-contrast(mod3.galtan.alt.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod3.galtan.alt.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))

# save to file
export(data.frame(rbind(
  mod3.galtan.eff.CM,
  mod3.galtan.eff.CAM,adjust="none")),
  "../../results/mod3.galtanBYhealth.eff.COMB.xlsx")

#' 
#' 
#' 
#' ## Model for antielite_salience
#' 
## ----------------------------------------------------------------------------
mod3.antielite_salience<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+strain.on.health.c+antielite_salience.z+
            strain.on.health.c:antielite_salience.z,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)

anova(mod2.antielite_salience,
      mod3.antielite_salience,test="Chisq")

#Alternative model with manually defined interaction
fdat$antielite_salience.zXstrain.on.health.c<-fdat$antielite_salience.z*fdat$strain.on.health.c

mod3.antielite_salience.alt<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+strain.on.health.c+antielite_salience.z+
            antielite_salience.zXstrain.on.health.c,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)


mod3.antielite_salience.alt.trends<-
  emtrends(mod3.antielite_salience.alt,~1|DV.f,
           var="antielite_salience.zXstrain.on.health.c",
           infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0))

#effects for each DV-category
(mod3.antielite_salience.alt.eff<-
  contrast(mod3.antielite_salience.alt.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))

#save to file
export(data.frame(mod3.antielite_salience.alt.eff),
       "../../results/mod3.antielite_salienceBYhealth.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod3.antielite_salience.eff.CM<-
    contrast(mod3.antielite_salience.alt.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod3.antielite_salience.alt.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))

#Use of CAM

#weighted across all effects

(mod3.antielite_salience.eff.CAM<-
    contrast(mod3.antielite_salience.alt.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod3.antielite_salience.alt.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))

# save to file
export(data.frame(rbind(
  mod3.antielite_salience.eff.CM,
  mod3.antielite_salience.eff.CAM,adjust="none")),
  "../../results/mod3.antielite_salienceBYhealth.eff.COMB.xlsx")



#' 
#' ### Probing simple slopes
#' 
## ----------------------------------------------------------------------------
ref_grid(mod3.antielite_salience)

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


# save to file

export(rbind(data.frame(mod3.antielite_salience.slopes.low.eff),
             data.frame(mod3.antielite_salience.slopes.mid.eff),
             data.frame(mod3.antielite_salience.slopes.high.eff)),
       "../../results/mod3.antielite_salience.slopes.eff.MN.xlsx")


# combined CM for low

(mod3.antielite_salience.slopes.low.CM<-
  contrast(mod3.antielite_salience.slopes.low,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod3.antielite_salience.slopes.low.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))


# combined CM for mid

(mod3.antielite_salience.slopes.mid.CM<-
  contrast(mod3.antielite_salience.slopes.mid,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod3.antielite_salience.slopes.mid.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))


# combined CM for high

(mod3.antielite_salience.slopes.high.CM<-
  contrast(mod3.antielite_salience.slopes.high,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod3.antielite_salience.slopes.high.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))



#Combined CAM for low


(mod3.antielite_salience.slopes.low.CAM<-
    contrast(mod3.antielite_salience.slopes.low,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod3.antielite_salience.slopes.low.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))


#Combined CAM for mid


(mod3.antielite_salience.slopes.mid.CAM<-
    contrast(mod3.antielite_salience.slopes.mid,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod3.antielite_salience.slopes.mid.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))

#Combined CAM for high


(mod3.antielite_salience.slopes.high.CAM<-
    contrast(mod3.antielite_salience.slopes.high,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod3.antielite_salience.slopes.high.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))

export(data.frame(rbind(
  mod3.antielite_salience.slopes.low.CM,
  mod3.antielite_salience.slopes.mid.CM,
  mod3.antielite_salience.slopes.high.CM,
  mod3.antielite_salience.slopes.low.CAM,
  mod3.antielite_salience.slopes.mid.CAM,
  mod3.antielite_salience.slopes.high.CAM,adjust="none")),
  "../../results/mod3.antielite_salience.slopes.eff.COMB.xlsx")

#' 
#' ## Model for corrupt_salience
#' 
## ----------------------------------------------------------------------------
mod3.corrupt_salience<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+strain.on.health.c+corrupt_salience.z+
            strain.on.health.c:corrupt_salience.z,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)

anova(mod2.corrupt_salience,
      mod3.corrupt_salience,test="Chisq")

#Alternative model with manually defined interaction
fdat$corrupt_salience.zXstrain.on.health.c<-
  fdat$corrupt_salience.z*fdat$strain.on.health.c

mod3.corrupt_salience.alt<-
  mblogit(DV.f~gndr.c+age10.c+income.fr+
            edu.f+strain.on.health.c+corrupt_salience.z+
            corrupt_salience.zXstrain.on.health.c,
          random= ~1|cntry,
          estimator="ML",
          control = 
            mmclogit.control(maxit = 250,
                             trace=TRUE),
          data=fdat,weights=anweight)


mod3.corrupt_salience.alt.trends<-
  emtrends(mod3.corrupt_salience.alt,~1|DV.f,
           var="corrupt_salience.zXstrain.on.health.c",
           infer=c(T,T),mode="latent",
           at=list(gndr.c=0,age10.c=0))

#effects for each DV-category
(mod3.corrupt_salience.alt.eff<-
  contrast(mod3.corrupt_salience.alt.trends,simple="DV.f",
         adjust="none","eff",infer=c(T,T)))

#save to file
export(data.frame(mod3.corrupt_salience.alt.eff),
       "../../results/mod3.corrupt_salienceBYhealth.eff.MN.xlsx",
       overwrite=T)

#Use of conventional medicine

(mod3.corrupt_salience.eff.CM<-
    contrast(mod3.corrupt_salience.alt.trends,
         method = list("Conv - No conv" = contrast.weights.total(effects=mod3.corrupt_salience.alt.eff,
                 signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))

#Use of CAM

#weighted across all effects

(mod3.corrupt_salience.eff.CAM<-
    contrast(mod3.corrupt_salience.alt.trends,
         method = list("CAM - No CAM" = contrast.weights.total(effects=mod3.corrupt_salience.alt.eff,
                 signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))

# save to file
export(data.frame(rbind(
  mod3.corrupt_salience.eff.CM,
  mod3.corrupt_salience.eff.CAM,adjust="none")),
  "../../results/mod3.corrupt_salienceBYhealth.eff.COMB.xlsx")



#' 
#' ### Probing simple slopes
#' 
## ----------------------------------------------------------------------------
ref_grid(mod3.corrupt_salience)

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


# save to file

export(rbind(data.frame(mod3.corrupt_salience.slopes.low.eff),
             data.frame(mod3.corrupt_salience.slopes.mid.eff),
             data.frame(mod3.corrupt_salience.slopes.high.eff)),
       "../../results/mod3.corrupt_salience.slopes.eff.MN.xlsx")


# combined CM for low

(mod3.corrupt_salience.slopes.low.CM<-
  contrast(mod3.corrupt_salience.slopes.low,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod3.corrupt_salience.slopes.low.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))


# combined CM for mid

(mod3.corrupt_salience.slopes.mid.CM<-
  contrast(mod3.corrupt_salience.slopes.mid,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod3.corrupt_salience.slopes.mid.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))


# combined CM for high

(mod3.corrupt_salience.slopes.high.CM<-
  contrast(mod3.corrupt_salience.slopes.high,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod3.corrupt_salience.slopes.high.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f", infer=c(T,T)))



#Combined CAM for low


(mod3.corrupt_salience.slopes.low.CAM<-
    contrast(mod3.corrupt_salience.slopes.low,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod3.corrupt_salience.slopes.low.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))


#Combined CAM for mid


(mod3.corrupt_salience.slopes.mid.CAM<-
    contrast(mod3.corrupt_salience.slopes.mid,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod3.corrupt_salience.slopes.mid.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))

#Combined CAM for high


(mod3.corrupt_salience.slopes.high.CAM<-
    contrast(mod3.corrupt_salience.slopes.high,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod3.corrupt_salience.slopes.high.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f", infer=c(T,T)))

export(data.frame(rbind(
  mod3.corrupt_salience.slopes.low.CM,
  mod3.corrupt_salience.slopes.mid.CM,
  mod3.corrupt_salience.slopes.high.CM,
  mod3.corrupt_salience.slopes.low.CAM,
  mod3.corrupt_salience.slopes.mid.CAM,
  mod3.corrupt_salience.slopes.high.CAM,adjust="none")),
  "../../results/mod3.corrupt_salience.slopes.eff.COMB.xlsx")

#' 
#' ### Plot association by strain on health
#' 
## ----message=FALSE, warning=FALSE--------------------------------------------
# obtain range of predictor values
range(fdat$corrupt_salience.z)

focal.points<-seq(from=-1.25,to=1.75,by=0.01)

# obtain moderator points
strain.on.health.c.points<-
  c(mean(fdat$strain.on.health.c)-
      sd(fdat$strain.on.health.c),
    mean(fdat$strain.on.health.c)-
      0*sd(fdat$strain.on.health.c),
    mean(fdat$strain.on.health.c)+
      sd(fdat$strain.on.health.c))

preds.low<-list()
preds.mid<-list()
preds.high<-list()

# Loop through each point and obtain prediction

for (i in 1:length(focal.points)){
  
  # low
  
  temp.point.low<-
      emmeans(mod3.corrupt_salience,~corrupt_salience.z|DV.f,
            at=list(corrupt_salience.z=
                      focal.points[i],
                    strain.on.health.c=
                      strain.on.health.c.points[1],
                    gndr.c=0,
                    age.10.c=0),
            infer=c(T,T),
            mode="latent")
  
  temp.point.low.eff<-
      contrast(temp.point.low,
               simple="DV.f",
               adjust="none","eff",
               infer=c(T,T))

  # combined CM for low
  
  preds.low[[i]]<-
      data.frame(contrast(temp.point.low,
               method = list("Conv - No conv" = 
                               contrast.weights.total(
                                 effects=
                                   temp.point.low.eff,
                                 signs=c(-2,-2,2,2))),
               simple="DV.f", infer=c(T,T)))
  
  # mid
  
  temp.point.mid<-
    emmeans(mod3.corrupt_salience,~corrupt_salience.z|DV.f,
            at=list(corrupt_salience.z=
                      focal.points[i],
                    strain.on.health.c=
                      strain.on.health.c.points[2],
                    gndr.c=0,
                    age.10.c=0),
            infer=c(T,T),
            mode="latent")
  
  temp.point.mid.eff<-
    contrast(temp.point.mid,
             simple="DV.f",
             adjust="none","eff",
             infer=c(T,T))
  
  # combined CM for mid
  
  preds.mid[[i]]<-
    data.frame(contrast(temp.point.mid,
                        method = list("Conv - No conv" = 
                                        contrast.weights.total(
                                          effects=
                                            temp.point.mid.eff,
                                          signs=c(-2,-2,2,2))),
                        simple="DV.f", infer=c(T,T)))
  
  
  # high
  
  temp.point.high<-
    emmeans(mod3.corrupt_salience,~corrupt_salience.z|DV.f,
            at=list(corrupt_salience.z=
                      focal.points[i],
                    strain.on.health.c=
                      strain.on.health.c.points[3],
                    gndr.c=0,
                    age.10.c=0),
            infer=c(T,T),
            mode="latent")
  
  temp.point.high.eff<-
    contrast(temp.point.high,
             simple="DV.f",
             adjust="none","eff",
             infer=c(T,T))
  
  # combined CM for high
  
  preds.high[[i]]<-
    data.frame(contrast(temp.point.high,
                        method = list("Conv - No conv" = 
                                        contrast.weights.total(
                                          effects=
                                            temp.point.high.eff,
                                          signs=c(-2,-2,2,2))),
                        simple="DV.f", infer=c(T,T)))
  
  
  
}

preds.low.df<-do.call(rbind,preds.low)
preds.low.df$'Health Status'<-"Good"

preds.mid.df<-do.call(rbind,preds.mid)
preds.mid.df$'Health Status'<-"Average"

preds.high.df<-do.call(rbind,preds.high)
preds.high.df$'Health Status'<-"Poor"

preds.all<-rbind(preds.low.df,
                 preds.mid.df,
                 preds.high.df)

# transform to probabilities

preds.all$P.CONV.USE<-
  exp(preds.all$estimate)/(1+exp(preds.all$estimate))

preds.all$P.CONV.USE.LL<-
  exp(preds.all$asymp.LCL)/(1+exp(preds.all$asymp.LCL))

preds.all$P.CONV.USE.UL<-
  exp(preds.all$asymp.UCL)/(1+exp(preds.all$asymp.UCL))

# relevel 

preds.all$'Health Status'<-
  factor(preds.all$'Health Status',
         levels=c("Poor","Average","Good"))

# export Figure 1 data

export(preds.all,
  "../../results/figures/Fig1data.xlsx")

# produce the figure

Fig1<-ggplot(data=preds.all,
       aes(y=P.CONV.USE,x=corrupt_salience.z,
           color=`Health Status`))+
  geom_line(size=2.75)+
  geom_line(size=1,
            linetype=2,
            aes(x=corrupt_salience.z,y=P.CONV.USE.LL))+
  geom_line(size=1,
            linetype=2,
            aes(x=corrupt_salience.z,y=P.CONV.USE.UL))+
  scale_color_manual(values=
                       met.brewer("Johnson",
                                  type = "continuous")
                     [c(2,3,4)])+
  xlab("Anti-corruption")+
  ylab("P(Use of Conventional Medicine)")+
  theme(text=element_text(size=16,  family="sans"))

Fig1


jpeg(filename = 
      "../../results/figures/Figure 2 corrupt health status.jpg",
    units = "cm",
    width = 21.0,height=29.7/2,res = 600)
Fig1
dev.off()



#' 
#' # Exploratory analysis
#' 
#' ## Nonlinearity
#' 
#' ### lrgen
#' 
## ----------------------------------------------------------------------------

fdat$lrgen.z.1<-fdat$lrgen.z
fdat$lrgen.z.2<-fdat$lrgen.z^2
fdat$lrgen.z.3<-fdat$lrgen.z^3

mod4.lrgen<-mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+strain.on.health.c+
                       lrgen.z+I(lrgen.z^2)+I(lrgen.z^3),
                     random= ~1|cntry,
                     estimator="ML",
                     data=fdat,weights=anweight)

anova(mod2.lrgen,mod4.lrgen,test="Chisq")



#' 
#' 
#' ### lrecon
#' 
## ----------------------------------------------------------------------------

fdat$lrecon.z.1<-fdat$lrecon.z
fdat$lrecon.z.2<-fdat$lrecon.z^2
fdat$lrecon.z.3<-fdat$lrecon.z^3

mod4.lrecon<-mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+strain.on.health.c+
                       lrecon.z+I(lrecon.z^2)+I(lrecon.z^3),
                     random= ~1|cntry,
                     estimator="ML",
                     data=fdat,weights=anweight)

anova(mod2.lrecon,mod4.lrecon,test="Chisq")



#' 
#' 
#' 
#' ### galtan
#' 
## ----------------------------------------------------------------------------

fdat$galtan.z.1<-fdat$galtan.z
fdat$galtan.z.2<-fdat$galtan.z^2
fdat$galtan.z.3<-fdat$galtan.z^3

mod4.galtan<-mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+
                       strain.on.health.c+
                       galtan.z+I(galtan.z^2)+I(galtan.z^3),
                     random= ~1|cntry,
                     estimator="ML",
                     data=fdat,weights=anweight)

anova(mod2.galtan,mod4.galtan,test="Chisq")

mod5.galtan<-mblogit(DV.f~gndr.c+age10.c+income.fr+edu.f+
                       strain.on.health.c+
                       galtan.z+I(galtan.z^2),
                     random= ~1|cntry,
                     estimator="ML",
                     data=fdat,weights=anweight)

anova(mod5.galtan,mod4.galtan,test="Chisq")



#' 
#' 
#' #### Simple slopes
#' 
## ----------------------------------------------------------------------------

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

#save to file

export(
  data.frame(
  rbind(
    mod4.galtan.slopes.low.eff,
    mod4.galtan.slopes.mid.eff,
    mod4.galtan.slopes.high.eff,adjust="none")),
  "../../results/mod4.galtan.nonlinear.slopes.MN.xlsx")

# CM low

(mod4.galtan.slopes.low.eff.CM<-
    contrast(mod4.galtan.slopes.low,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod4.galtan.slopes.low.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f",infer=c(T,T)))

# CM mid

(mod4.galtan.slopes.mid.eff.CM<-
    contrast(mod4.galtan.slopes.mid,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod4.galtan.slopes.mid.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f",infer=c(T,T)))

# CM high

(mod4.galtan.slopes.high.eff.CM<-
    contrast(mod4.galtan.slopes.high,
         method = list("Conv - No conv" = 
                         contrast.weights.total(
                           effects=
                           mod4.galtan.slopes.high.eff,
                           signs=c(-2,-2,2,2))),
         simple="DV.f",infer=c(T,T)))



#CAM low

(mod4.galtan.slopes.low.eff.CAM<-
    contrast(mod4.galtan.slopes.low,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod4.galtan.slopes.low.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f",infer=c(T,T)))

#CAM mid

(mod4.galtan.slopes.mid.eff.CAM<-
    contrast(mod4.galtan.slopes.mid,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod4.galtan.slopes.mid.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f",infer=c(T,T)))

#CAM high

(mod4.galtan.slopes.high.eff.CAM<-
    contrast(mod4.galtan.slopes.high,
         method = list("CAM - No CAM" = 
                         contrast.weights.total(
                           effects=
                           mod4.galtan.slopes.high.eff,
                           signs=c(-2,2,2,-2))),
         simple="DV.f",infer=c(T,T)))


export(data.frame(rbind(mod4.galtan.slopes.low.eff.CM,
      mod4.galtan.slopes.mid.eff.CM,
      mod4.galtan.slopes.high.eff.CM,
      mod4.galtan.slopes.low.eff.CAM,
      mod4.galtan.slopes.mid.eff.CAM,
      mod4.galtan.slopes.high.eff.CAM,adjust="none")),
      "../../results/mod4.galtan.nonlinear.slopes.COMB.xlsx")




#' 
#' #### Plot predicted probabilities
#' 
## ----message=FALSE, warning=FALSE--------------------------------------------

Fig2<-
  ggplot(data=fdat,aes(x=galtan.z,y=used.CAM))+
  geom_smooth(method = "glm", formula=y~poly(x,1),
              method.args = list(family = "binomial"), 
              se = TRUE,color=met.brewer("Johnson")[2],size=2.75)+
  geom_smooth(method = "glm", formula=y~poly(x,2),
              method.args = list(family = "binomial"), 
              se = TRUE,color=met.brewer("Johnson")[4],size=2.75)+
  xlab("GAL-TAN")+
  ylab("P(Use of Complementary/Alternative Medicine)")+
  theme(text=element_text(size=16,  family="sans"))
Fig2


jpeg(filename = 
      "../../results/figures/Figure 3 GALTAN CAM.jpg",
    units = "cm",
    width = 21.0,height=29.7/2,res = 600)
Fig2
dev.off()

#' 
#' ### antielite_salience
#' 
## ----------------------------------------------------------------------------

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

anova(mod2.antielite_salience,
      mod4.antielite_salience,test="Chisq")



#' 
#' 
#' 
#' ### corrupt_salience
#' 
## ----------------------------------------------------------------------------

fdat$corrupt_salience.z.1<-fdat$corrupt_salience.z
fdat$corrupt_salience.z.2<-fdat$corrupt_salience.z^2
fdat$corrupt_salience.z.3<-fdat$corrupt_salience.z^3

mod4.corrupt_salience<-
  mblogit(DV.f~gndr.c+age10.c+
            income.fr+edu.f+
            strain.on.health.c+
            corrupt_salience.z+
            I(corrupt_salience.z^2)+
            I(corrupt_salience.z^3),
                     random= ~1|cntry,
                     estimator="ML",
                     data=fdat,weights=anweight)

anova(mod2.corrupt_salience,
      mod4.corrupt_salience,test="Chisq")


#' 
#' # Print out custom functions
#' 
## ----code=readLines("../custom_functions.R")---------------------------------

#' 
#' # Session information
#' 
## ----------------------------------------------------------------------------
s<-sessionInfo()
print(s,locale=F)

