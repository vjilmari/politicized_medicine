#' ---
#' title: "Variable transformation before running the analysis"
#' output: 
#'   html_document: 
#'     toc: yes
#'     number_sections: yes
#'     keep_md: yes
#' ---
#' 
## ---- include=FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # Preparations
#' 
#' ## Load packages
#' 
## ----message=FALSE, warning=FALSE--------------------------------------------
library(rio)
library(dplyr)

#' 
#' ## Load data
#' 
## ----------------------------------------------------------------------------
# Long format data with ESS and CHES merged
dat<-import("../../data/processed/dat.xlsx")

# ESS raw data from which variable labels can be obtained
ESS.dat<-import("../../data/raw/ESS7e02_2.sav")

#' 
#' # Variable transformations
#' 
#' ## Gender
#' 
## ----------------------------------------------------------------------------
attr(ESS.dat$gndr,"labels")

# Factorial gndr

dat$gndr.f<-case_when(dat$gndr==1~"Male",
                      dat$gndr==2~"Female",
                      TRUE~NA_character_)

table(dat$gndr.f,useNA="always")

# Numerical gndr

dat$gndr.c<-case_when(dat$gndr==1~-0.5,
                      dat$gndr==2~0.5,
                      TRUE~NA_real_)

table(dat$gndr.c,useNA="always")


#' 
#' ## Age
#' 
## ----------------------------------------------------------------------------
attr(ESS.dat$agea,"labels")

table(dat$agea==999)

# centered age divided by 10
dat$age10.c<-(dat$agea-mean(dat$agea,na.rm=T))/10


#' 
#' ## Income
#' 
## ----------------------------------------------------------------------------
attr(ESS.dat$hinctnta,"labels")

# recode deciles to quintiles

dat$income<-case_when(
  dat$hinctnta==1 | dat$hinctnta==2 ~ "quint.1",
  dat$hinctnta==3 | dat$hinctnta==4 ~ "quint.2",
  dat$hinctnta==5 | dat$hinctnta==6 ~ "quint.3",
  dat$hinctnta==7 | dat$hinctnta==8 ~ "quint.4",
  dat$hinctnta==9 | dat$hinctnta==10 ~ "quint.5"
)

table(dat$income,useNA="always")

# add missing as additional factor level (to a new variable income.f)

dat$income.f<-case_when(
  is.na(dat$income) ~ "missing",
  TRUE ~ dat$income
)

#define reference level (top quintile)
table(dat$income.f,useNA="always")
dat$income.fr = relevel(as.factor(dat$income.f),
                        ref="quint.5")
table(dat$income.fr,useNA="always")


#' 
#' ## Education
#' 
## ----------------------------------------------------------------------------
attr(ESS.dat$eisced,"labels")

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

# recode reference education (highest) to a new variable (edu.f)

dat$edu.f<-relevel(as.factor(dat$edu),ref="7. MA")
table(dat$edu.f)


#' 
#' ## Health problems
#' 
## ----------------------------------------------------------------------------

# hampered health problems (example)
attr(ESS.dat$hltphhc,"labels")

# non-hampered (example)
attr(ESS.dat$hltprhc,"labels")

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

# test Estonia separately

table(rowSums(dat[dat$cntry=="EE",problems.vars]),useNA="always")

table(ESS.dat[ESS.dat$cntry=="EE","hltprhc"])


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



#' 
#' ## Dependent variable
#' 
## ----------------------------------------------------------------------------
# visited general practitioner
attr(ESS.dat$dshltgp,"labels")
# visited medical specialist
attr(ESS.dat$dshltms,"labels")

table(dat$dshltgp,useNA="always")
table(dat$dshltms,useNA="always")

# use of conventional medicine

dat$used.conv<-ifelse(dat$dshltgp==1 | dat$dshltms==1,1,0)
table(dat$used.conv,useNA="always")

# use of CAMs

#acupuncture trhltacu
#acupressure trhltacp
#chinese medicine trhltcm
#homeopathy trhltho
#herbal treatment trhltht
#hypnotherapy trhlthy
#spiritual healing trhltsh

attr(ESS.dat$trhltacu,"labels")
table(dat$trhltacu)

dat$used.CAM<-ifelse(
  dat$trhltacu==1 |
  dat$trhltacp==1 |
  dat$trhltcm==1 |
  dat$trhltho==1 |
  dat$trhltht==1 |
  dat$trhlthy==1 |
  dat$trhltsh==1,1,0)

table(dat$used.CAM,useNA="always")

# code CAM-use also without homeopathy


dat$used.CAM.no.home<-ifelse(
  dat$trhltacu==1 |
    dat$trhltacp==1 |
    dat$trhltcm==1 |
    dat$trhltht==1 |
    dat$trhlthy==1 |
    dat$trhltsh==1,1,0)

table(dat$used.CAM.no.home,useNA="always")

# Code the four category variable

dat$DV<-case_when(
  dat$used.conv==0 & dat$used.CAM==0 ~ "NN",
  dat$used.conv==1 & dat$used.CAM==0 ~ "Used_conv_ONLY",
  dat$used.conv==0 & dat$used.CAM==1 ~ "Used_CAM_ONLY",
  dat$used.conv==1 & dat$used.CAM==1 ~ "Used_conv_and_CAM")

table(dat$DV,useNA="always")

# without homeopathy as CAM

dat$DV.no.home<-case_when(
  dat$used.conv==0 & dat$used.CAM.no.home==0 ~ "NN",
  dat$used.conv==1 & dat$used.CAM.no.home==0 ~ "Used_conv_ONLY",
  dat$used.conv==0 & dat$used.CAM.no.home==1 ~ "Used_CAM_ONLY",
  dat$used.conv==1 & dat$used.CAM.no.home==1 ~ "Used_conv_and_CAM")

table(dat$DV.no.home,useNA="always")


#' 
#' ## Political orientation
#' 
## ----------------------------------------------------------------------------
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

#' 
#' ## Belonging to minority ethnic group
#' 
## ----------------------------------------------------------------------------

attr(ESS.dat$blgetmg,"labels")

# Factorial blgetmg

dat$blgetmg.f<-case_when(dat$blgetmg==1~"Yes",
                         TRUE~"No")

table(dat$blgetmg.f,useNA="always")

# Numerical blgetmg

dat$blgetmg.c<-case_when(dat$blgetmg==1~0.5,
                         TRUE~(-0.5))

table(dat$blgetmg.c,useNA="always")


#' 
#' ## Marital status
#' 
## ----------------------------------------------------------------------------
# does live with husband/wife/partner?

attributes(ESS.dat$icpart1)
table(ESS.dat$icpart1,useNA="always")


# "Relationship with husband/wife/partner currently living with"

attributes(ESS.dat$rshpsts)
table(ESS.dat$rshpsts,useNA="always")

table(ESS.dat$icpart1,ESS.dat$rshpsts,useNA="always")

# factorial marital status
dat$mstatus.f<-case_when(
  dat$icpart1==1 & dat$rshpsts<5~"Married",
  TRUE~"Non-married"
)

table(dat$mstatus.f,useNA="always")

# numeric marital status
dat$mstatus.c<-case_when(
  dat$mstatus.f=="Married"~0.5,
  TRUE~(-0.5))

#' 
#' # Final set of variables needed for the analysis
#' 
## ----------------------------------------------------------------------------
analysis.vars<-
  c("idno","cntry","dweight","pspwght","pweight",
    "pt.nmbr","pt.name","vote",
    "gndr.f","gndr.c","agea","age10.c",
    "income","income.f","income.fr",
    "edu","edu.f","strain.on.health",
    "blgetmg.f","blgetmg.c",
    "mstatus.f","mstatus.c",
    "used.conv","used.CAM","DV",
    "used.CAM.no.home","DV.no.home",
    "lrgen","lrecon","galtan",
    "antielite_salience","corrupt_salience",
    "lrgen.scaling","lrecon.scaling","galtan.scaling",
    "antielite_salience.scaling","corrupt_salience.scaling")

# test if they are all in the data file
analysis.vars %in% names(dat)

# exclude variable not needed
fdat<-dat[,analysis.vars]
str(fdat)

# save the final data file
export(fdat,
       "../../data/processed/fdat.xlsx",overwrite=T)

#' # Session information
#' 
## ----------------------------------------------------------------------------
s<-sessionInfo()
print(s,locale=F)

