# Scatter plots for a selected set of countries

# MetBrewer colors: https://github.com/BlakeRMills/MetBrewer

# devtools::install_github('rensa/ggflags')

library(ggflags)
library(ggplot2)
library(rio)
library(dplyr)
library(ggrepel)
library(MetBrewer)
library(ggpubr)

display_all(colorblind_only = T)

met.brewer("Johnson")
met.brewer("Kandinsky")
met.brewer("Kandinsky",3,type = "continuous")

CHES<-import("data/processed/CHES_2014.vote.keys.combined.xlsx")
CHES.raw<-import("data/processed/CHES_2014.vote.keys.xlsx")


# Too many long names, take the values from CHES file

CHES.raw<-CHES.raw %>%
  dplyr::select(cntry, pt.nmbr, party_id, party_name)

head(CHES.raw)

CHES.raw<-CHES.raw[!duplicated(CHES.raw), ]
CHES.raw

CHES<-left_join(
  x=CHES,
  y=CHES.raw,
  by=c("cntry","pt.nmbr")
)

head(CHES)

## Correct alphabets for the parties

CHES[CHES$cntry == "AT",c("party_name","pt.name")]

CHES[,c("cntry","party_name","pt.name")]

CHES$party_name_alpha<-
  case_when(
    CHES$cntry == "AT" & CHES$party_name == "BZO"~"BZÖ",
    CHES$cntry == "AT" & CHES$party_name == "FPO"~"FPÖ",
    CHES$cntry == "AT" & CHES$party_name == "GRUNE"~"GRÜNE",
    CHES$cntry == "AT" & CHES$party_name == "SPO"~"SPÖ",
    CHES$cntry == "AT" & CHES$party_name == "OVP"~"ÖVP",
    CHES$cntry == "DE" & CHES$party_name == "Grunen"~"Grünen",
    TRUE ~ CHES$party_name
    )

## Combine country and party names

CHES$pt.cntry<-paste0(CHES$party_name_alpha," (",CHES$cntry,")")
CHES$pt.cntry

# weight the dot size by party's popularity in the ESS data

fdat<-import("data/processed/fdat.xlsx")

party.fdat<-fdat %>%
  group_by(cntry,pt.name) %>%
  summarise(n=n())

party.fdat<- party.fdat[!is.na(party.fdat$pt.name),]

country.fdat <- party.fdat %>%
  group_by(cntry) %>%
  summarise(country.n=sum(n)) %>%
  ungroup()

party.fdat<-left_join(x=party.fdat,
                      y=country.fdat,
                      by="cntry")

party.fdat$vote.share<-party.fdat$n/party.fdat$country.n

plot.dat<-left_join(
  x=CHES,
  y=party.fdat,
  by=c("cntry","pt.name")
)

# add criterion for labels and for dot color

plot.dat$lrgen.criterion<-
  ifelse(
    (plot.dat$lrgen>7.5 | plot.dat$lrgen <2.5),TRUE,FALSE)

plot.dat$lrecon.criterion<-
  ifelse(
    (plot.dat$lrecon>7.5 | plot.dat$lrecon <2.5),TRUE,FALSE)

plot.dat$galtan.criterion<-
  ifelse(
    (plot.dat$galtan>7.5 | plot.dat$galtan <2.5),TRUE,FALSE)

plot.dat$antielite_salience.criterion<-
  ifelse(
    (plot.dat$antielite_salience>7.5 | plot.dat$antielite_salience <2.5),TRUE,FALSE)

plot.dat$corrupt_salience.criterion<-
  ifelse(
    (plot.dat$corrupt_salience>7.5 | plot.dat$corrupt_salience <2.5),TRUE,FALSE)


plot.dat$large.size.criterion<-
  ifelse(
    plot.dat$vote.share > 0.29999,TRUE,FALSE)

table(plot.dat$large.size.criterion)

plot.dat$small.size.criterion<-
  ifelse(
    plot.dat$vote.share > 0.01999,TRUE,FALSE)

table(plot.dat$small.size.criterion)

# remove the combined parties, or use them as combined rather

# look for duplicates

plot.dat[duplicated(plot.dat[,c("n","cntry","lrgen","lrecon","galtan")]),]

# take a closer look at country level

plot.dat[plot.dat$cntry=="DE",]
plot.dat[plot.dat$cntry=="ES",]
plot.dat[plot.dat$cntry=="HU",]

# rename one party in the coalition

plot.dat$pt.cntry<-case_when(
  plot.dat$pt.cntry == "CDU (DE)"~"CDU/CSU (DE)",
  plot.dat$pt.cntry == "IU (ES)"~"ICV/IU (ES)",
  plot.dat$pt.cntry == "MSZP (HU)"~"DK/MSZP (HU)",
  TRUE~plot.dat$pt.cntry
)

# exclude the other parties in the coalition

plot.dat<-plot.dat %>%
  filter(pt.cntry != "CSU (DE)" &
           pt.cntry != "ICV (ES)" &
           pt.cntry != "DK (HU)")

names(plot.dat)
table(plot.dat$cntry)

# limit countries to only
# Germany, Great Britain, France, and Spain

plot.dat<-plot.dat %>%
  filter(cntry=="DE" | cntry=="GB" | cntry =="FR" | cntry =="ES")

# Add CDU/CSU and ICV/IU
plot.dat$party_name_alpha
plot.dat$party_name_alpha<-
  case_when(plot.dat$party_name_alpha=="CDU"~"CDU/CSU",
            plot.dat$party_name_alpha=="IU"~"ICV/IU",
            TRUE~plot.dat$party_name_alpha)


met.brewer("Johnson")
met.brewer("Johnson")[c(2,3,4)]
str(met.brewer("Kandinsky"))
met.brewer("Kandinsky")[c(1,2,4)]
plot.dat$party_name_alpha
plot.dat$Country<-case_when(
  plot.dat$cntry=="DE"~"Germany",
  plot.dat$cntry=="FR"~"France",
  plot.dat$cntry=="GB"~"United Kingdom",
  plot.dat$cntry=="ES"~"Spain")



# lrgen_lrecon scatterplot

lrgen_lrecon_sp <- 
  ggplot(plot.dat, aes(x = lrecon,
                       y = lrgen,color=cntry,group=cntry))+
  geom_point(aes(size=vote.share),alpha=.75)+
  scale_color_manual(values=met.brewer("Egypt")[1:4],
                     labels=c("Germany","Spain","France","United Kingdom"))+
  scale_size(range = c(5,20))+
  xlim(0,10.5)+
  ylim(0,10.5)+
  xlab("Left-Right Economic")+
  ylab("Left-Right General")+
  theme(text=element_text(size=24,  family="sans"),
        legend.title=element_blank(),
        panel.background = element_blank())+
  guides(color = guide_legend(override.aes = list(size=5)),
         size = "none")
lrgen_lrecon_sp

jpeg(filename = "results/scatterplots/revised_lrgen_lrecon_sp_cntry_color.jpg",units = "cm",
    width = 25.0,height=20.0,res = 600)
lrgen_lrecon_sp
dev.off()


# lrgen_lrecon scatterplot

corrupt_salience_lrecon_sp <- 
  ggplot(plot.dat, aes(x = lrecon,
                       y = corrupt_salience,color=cntry,group=cntry))+
  geom_point(aes(size=vote.share),alpha=.75)+
  scale_color_manual(values=met.brewer("Egypt")[1:4],
                     labels=c("Germany","Spain","France","United Kingdom"))+
  scale_size(range = c(5,20))+
  xlim(0,10.5)+
  ylim(0,10.5)+
  xlab("Left-Right Economic")+
  ylab("Anti-corruption")+
  theme(text=element_text(size=24,  family="sans"),
        legend.title=element_blank(),
        panel.background = element_blank())+
  guides(color = guide_legend(override.aes = list(size=5)),
         size = "none")
corrupt_salience_lrecon_sp

jpeg(filename = "results/scatterplots/revised_corrupt_salience_lrecon_sp_cntry_color.jpg",units = "cm",
     width = 25.0,height=20.0,res = 600)
corrupt_salience_lrecon_sp
dev.off()


# galtan_antielite_salience scatterplot

galtan_antielite_salience_sp <- 
  ggplot(plot.dat, aes(x = antielite_salience,
                       y = galtan,color=cntry,group=cntry))+
  geom_point(aes(size=vote.share),alpha=.75)+
  scale_color_manual(values=met.brewer("Egypt")[1:4],
                     labels=c("Germany","Spain","France","United Kingdom"))+
  scale_size(range = c(5,20))+
  xlim(0,10.5)+
  ylim(0,10.5)+
  xlab("Anti-elite")+
  ylab("GAL-TAN")+
  theme(text=element_text(size=24,  family="sans"),
        legend.title=element_blank(),
        panel.background = element_blank())+
  guides(color = guide_legend(override.aes = list(size=5)),
         size = "none")
galtan_antielite_salience_sp

jpeg(filename = "results/scatterplots/revised_galtan_antielite_salience_sp_cntry_color.jpg",units = "cm",
     width = 25.0,height=20.0,res = 600)
galtan_antielite_salience_sp
dev.off()


# make three figures stacked version

plot_comb<-
  ggarrange(lrgen_lrecon_sp,
            corrupt_salience_lrecon_sp,
            galtan_antielite_salience_sp,
            ncol=1, nrow=3,common.legend = T,legend = "bottom")

jpeg(filename = "results/scatterplots/revised_3_stacked_color_scatter.jpg",units = "cm",
     width = 21,height=60,res = 600)
plot_comb
dev.off()

