library(rio)
library(finalfit)
library(rempsyc) 
library(flextable) 
library(officer) 
library(tibble)
library(dplyr)

source("script/custom_functions.R")

# Original analysis

## First import the combined effects

lrgen<-import(
  "results/mod2.lrgen.eff.COMB.xlsx")
lrecon<-import(
  "results/mod2.lrecon.eff.COMB.xlsx")
galtan<-import(
  "results/mod2.galtan.eff.COMB.xlsx")
antielite_salience<-import(
  "results/mod2.antielite_salience.eff.COMB.xlsx")
corrupt_salience<-import(
  "results/mod2.corrupt_salience.eff.COMB.xlsx")

# for CM

CM_tab<-
  rbind(lrgen[1,],
        lrecon[1,],
        galtan[1,],
        antielite_salience[1,],
        corrupt_salience[1,]) %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Left-right general",
                 "Left-right economic",
                 "Gal-tan",
                 "Anti-elite",
                 "Anti-corruption"),.)

CM_tab
save_as_docx(flextable(CM_tab),
             path="results/tables/CM_tab.docx")


# for CAM

CAM_tab<-
  rbind(lrgen[2,],
        lrecon[2,],
        galtan[2,],
        antielite_salience[2,],
        corrupt_salience[2,]) %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Left-right general",
                 "Left-right economic",
                 "Gal-tan",
                 "Anti-elite",
                 "Anti-corruption"),.)

CAM_tab
save_as_docx(flextable(CAM_tab),
             path="results/tables/CAM_tab.docx")


## Import the multinomial effect for each category
lrgen_MN<-import(
  "results/mod2.lrgen.eff.MN.xlsx")
lrecon_MN<-import(
  "results/mod2.lrecon.eff.MN.xlsx")
galtan_MN<-import(
  "results/mod2.galtan.eff.MN.xlsx")
antielite_salience_MN<-import(
  "results/mod2.antielite_salience.eff.MN.xlsx")
corrupt_salience_MN<-import(
  "results/mod2.corrupt_salience.eff.MN.xlsx")

lrgen_MN_tab<-
  lrgen_MN %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Left-right general","","",""),
            'Service use'=c("Neither",
                            "CAM only",
                            "Both",
                            "CM only"),.)


lrecon_MN_tab<-
  lrecon_MN %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Left-right economic","","",""),
            'Service use'=c("Neither",
                            "CAM only",
                            "Both",
                            "CM only"),.)

galtan_MN_tab<-
  galtan_MN %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Gal-tan","","",""),
            'Service use'=c("Neither",
                            "CAM only",
                            "Both",
                            "CM only"),.)

antielite_salience_MN_tab<-
  antielite_salience_MN %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Anti-elite","","",""),
            'Service use'=c("Neither",
                            "CAM only",
                            "Both",
                            "CM only"),.)

corrupt_salience_MN_tab<-
  corrupt_salience_MN %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Anti-corruption","","",""),
            'Service use'=c("Neither",
                            "CAM only",
                            "Both",
                            "CM only"),.)



MN_tab<-
  bind_rows(lrgen_MN_tab,
            lrecon_MN_tab,
            galtan_MN_tab,
            antielite_salience_MN_tab,
            corrupt_salience_MN_tab)

MN_tab
save_as_docx(flextable(MN_tab),
             path="results/tables/MN_tab.docx")


# With minority and marital status covariates

## First import the combined effects

lrgen_covariates<-import(
  "results/Added_covariates/mod2.lrgen.eff.COMB.xlsx")
lrecon_covariates<-import(
  "results/Added_covariates/mod2.lrecon.eff.COMB.xlsx")
galtan_covariates<-import(
  "results/Added_covariates/mod2.galtan.eff.COMB.xlsx")
antielite_salience_covariates<-import(
  "results/Added_covariates/mod2.antielite_salience.eff.COMB.xlsx")
corrupt_salience_covariates<-import(
  "results/Added_covariates/mod2.corrupt_salience.eff.COMB.xlsx")

# for CM

CM_tab_covariates<-
  rbind(lrgen_covariates[1,],
        lrecon_covariates[1,],
        galtan_covariates[1,],
        antielite_salience_covariates[1,],
        corrupt_salience_covariates[1,]) %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Left-right general",
                 "Left-right economic",
                 "Gal-tan",
                 "Anti-elite",
                 "Anti-corruption"),.)

CM_tab_covariates
save_as_docx(flextable(CM_tab_covariates),
             path="results/Added_covariates/tables/CM_tab_covariates.docx")


# for CAM

CAM_tab_covariates<-
  rbind(lrgen_covariates[2,],
        lrecon_covariates[2,],
        galtan_covariates[2,],
        antielite_salience_covariates[2,],
        corrupt_salience_covariates[2,]) %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Left-right general",
                 "Left-right economic",
                 "Gal-tan",
                 "Anti-elite",
                 "Anti-corruption"),.)

CAM_tab_covariates
save_as_docx(flextable(CAM_tab_covariates),
             path="results/Added_covariates/tables/CAM_tab_covariates.docx")


## Import the multinomial effect for each category
lrgen_covariates_MN<-import(
  "results/Added_covariates/mod2.lrgen.eff.MN.xlsx")
lrecon_covariates_MN<-import(
  "results/Added_covariates/mod2.lrecon.eff.MN.xlsx")
galtan_covariates_MN<-import(
  "results/Added_covariates/mod2.galtan.eff.MN.xlsx")
antielite_salience_covariates_MN<-import(
  "results/Added_covariates/mod2.antielite_salience.eff.MN.xlsx")
corrupt_salience_covariates_MN<-import(
  "results/Added_covariates/mod2.corrupt_salience.eff.MN.xlsx")

lrgen_covariates_MN_tab<-
  lrgen_covariates_MN %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Left-right general","","",""),
            'Service use'=c("Neither",
                            "CAM only",
                            "Both",
                            "CM only"),.)


lrecon_covariates_MN_tab<-
  lrecon_covariates_MN %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Left-right economic","","",""),
            'Service use'=c("Neither",
                            "CAM only",
                            "Both",
                            "CM only"),.)

galtan_covariates_MN_tab<-
  galtan_covariates_MN %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Gal-tan","","",""),
            'Service use'=c("Neither",
                            "CAM only",
                            "Both",
                            "CM only"),.)

antielite_salience_covariates_MN_tab<-
  antielite_salience_covariates_MN %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Anti-elite","","",""),
            'Service use'=c("Neither",
                            "CAM only",
                            "Both",
                            "CM only"),.)

corrupt_salience_covariates_MN_tab<-
  corrupt_salience_covariates_MN %>%
  dplyr::select(b=estimate,
                SE=SE,
                p=p.value,
                b_LL=asymp.LCL,
                b_UL=asymp.UCL) %>%
  mutate(OR=exp(b),
         LL=exp(b_LL),
         UL=exp(b_UL)) %>%
  dplyr::select(b,SE,p,OR,LL,UL) %>%
  dplyr::mutate(b=round_tidy(b,2),
                SE=round_tidy(SE,2),
                p=sapply(p,p_coding),
                OR=round_tidy(OR,2),
                LL=round_tidy(LL,2),
                UL=round_tidy(UL,2)) %>%
  bind_cols(PO=c("Anti-corruption","","",""),
            'Service use'=c("Neither",
                            "CAM only",
                            "Both",
                            "CM only"),.)



MN_tab_covariates<-
  bind_rows(lrgen_covariates_MN_tab,
            lrecon_covariates_MN_tab,
            galtan_covariates_MN_tab,
            antielite_salience_covariates_MN_tab,
            corrupt_salience_covariates_MN_tab)

MN_tab_covariates
save_as_docx(flextable(MN_tab_covariates),
             path="results/Added_covariates/tables/MN_tab_covariates.docx")


