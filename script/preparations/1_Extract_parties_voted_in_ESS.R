#' ---
#' title: "Extracting parties that participants voted in ESS"
#' output: 
#'   html_document: 
#'     toc: yes
#'     number_sections: yes
#'     keep_md: yes
#' ---
#' 
#' 
#' # Preparations
#' 
#' ## Load packages and custom functions
#' 
## ----message=FALSE, warning=FALSE------------------------------------------
library(rio)
library(sjlabelled)
source("../custom_functions.R")

#' 
#' ## Import ESS data
#' 
## --------------------------------------------------------------------------
dat<-import("../../data/raw/ESS7e02_2.sav")

#' 
#' # Save a vector of voting variables
#' 
## --------------------------------------------------------------------------
# use get.ESS.label custom function
cbind(sapply(dat[,27:100],get.ESS.label))

# save to vector
vote.vars<-names(dat)[which(names(dat)=="prtvtbat"):
                        which(names(dat)=="prtvtesi")]
# check if correct
cbind(sapply(dat[,vote.vars],get.ESS.label))

#' 
#' # Save a vector of countries with vote variables
#' 
#' Double-entry for Germany (DE), Triple entry for Lithuania (LT).
#' 
## --------------------------------------------------------------------------
table(dat$cntry)
vote.cntry<-c("AT","BE","CH","CZ","DE","DE","DK","EE","ES","FI","FR",
              "GB","HU","IE","IL","LT","LT","LT","NL","NO","PL","PT",
              "SE","SI")


#' 
#' # Construct a data file with countries, vote variable names, party names, and party numbers
#' 
## --------------------------------------------------------------------------
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
head(vote.dat)

# save to a file
export(vote.dat,
       "../../data/processed/vote.dat.xlsx",
       overwrite=T)


#' 
#' # Session information
#' 
## --------------------------------------------------------------------------
s<-sessionInfo()
print(s,locale=F)

