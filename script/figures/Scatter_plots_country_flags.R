# Try to find scatter plots for specific countries

# MetBrewer colors: https://github.com/BlakeRMills/MetBrewer


#devtools::install_github('rensa/ggflags')

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

# these were the manually picked parties in one exemplary Table

CHES$plot.name<-
  case_when(CHES$cntry=="GB" & CHES$pt.name=="Conservative"~"Conservative Party (UK)",
            CHES$cntry=="SE" & CHES$pt.name=="FI (Feministiskt initiativ)"~"Feminist Initiative (Sweden)",
            CHES$cntry=="AT" & CHES$pt.name=="FPÖ"~"Freedom Party (Austria)",
            CHES$cntry=="FR" & CHES$pt.name=="FN (Front National)"~"Front National (France)",
            CHES$cntry=="CH" & CHES$pt.name=="Green Party"~"Green Party (Switzerland)",
            CHES$cntry=="HU" & CHES$pt.name=="Jobbik (Jobbik Magyarországért Mozgalom)"~"Jobbik (Hungary)",
            CHES$cntry=="PL" & CHES$pt.name=="Law and Justice"~"Law and Justice (Poland)",
            CHES$cntry=="DE" & CHES$pt.name=="SPD"~"Social Democratic Party (Germany)",
            CHES$cntry=="ES" & CHES$pt.name=="Partido Socialista Obrero Español (PSOE)"~"Spanish Socialist Workers’ Party (Spain)",
            CHES$cntry=="FI" & CHES$pt.name=="True Finns"~"True Finns (Finland)",
            TRUE ~ NA_character_)


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

# weight the dot size by popularity in the ESS data

fdat<-import("data/processed/fdat.xlsx")
names(fdat)
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

#plot.dat$lab.criterion<-
#  ifelse(
#    ((plot.dat$galtan>7.5 | plot.dat$galtan <2.5) & 
#       (plot.dat$corrupt_salience>7.5 | plot.dat$corrupt_salience<2.5) & 
#       plot.dat$vote.share > 0.0199),TRUE,FALSE)

#table(plot.dat$lab.criterion)

#plot.dat$lab.criterion.2<-
#  ifelse(
#    ((plot.dat$lrecon>7.5 | plot.dat$lrecon <2.5) & 
#       (plot.dat$galtan>7.5 | plot.dat$galtan<2.5) & 
#       plot.dat$vote.share > 0.0199),TRUE,FALSE)

#table(plot.dat$lab.criterion.2)

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
                       y = lrgen))+
  geom_flag(aes(x = lrecon,
                y = lrgen,
                country=tolower(cntry),
                size=vote.share),
            position=position_jitter(seed=1234,
                                     width = 0.1, height = 0.1))+
  scale_size(range = c(4,12))+
  xlim(0,10)+
  ylim(0,10)+
  xlab("Left-Right Economic")+
  ylab("Left-Right General")+
  guides(size="none")+
  geom_label_repel(aes(label= 
                         as.character(party_name_alpha)),size = 4.0,
                   max.overlaps = 300,seed = 1236,
                   label.padding = 0.15,
                   box.padding = 0.50,
                   show.legend=FALSE,
                   position=position_jitter(seed=1234,
                                            width = 0.1, height = 0.1))+
  theme(legend.position = "none")+
  theme(text=element_text(size=16,  family="sans"))

lrgen_lrecon_sp

jpeg(filename = "results/scatterplots/lrgen_lrecon_sp_cntry_flag.jpg",units = "cm",
    width = 20.0,height=20.0,res = 600)
lrgen_lrecon_sp
dev.off()

# lrgen_lrecon figure text

#lrgen_lrecon_parties<-
#  plot.dat[(plot.dat$lrgen.criterion & 
#              plot.dat$lrecon.criterion & 
#              plot.dat$small.size.criterion) | plot.dat$large.size.criterion,
#           c("pt.cntry","pt.name")]

export(plot.dat[,c("cntry","party_name_alpha","pt.name")],
       "results/scatterplots/GB_DE_FR_ES_parties.xlsx")

# lrgen_galtan scatterplot

lrgen_galtan_sp <- 
  ggplot(plot.dat, aes(y = galtan,
                       x = lrgen))+
  geom_flag(aes(y = galtan,
                x = lrgen,
                country=tolower(cntry),
                size=vote.share),
            position=position_jitter(seed=1234,
                                     width = 0.1, height = 0.1))+
  scale_size(range = c(4,12))+
  xlim(0,10)+
  ylim(0,10)+
  ylab("GAL-TAN")+
  xlab("Left-Right General")+
  guides(size="none")+
  geom_label_repel(aes(label= 
                         as.character(party_name_alpha)),size = 4.0,
                   max.overlaps = 300,seed = 1236,
                   label.padding = 0.15,
                   box.padding = 0.50,
                   show.legend=FALSE,
                   position=position_jitter(seed=1234,
                                            width = 0.1, height = 0.1))+
  theme(legend.position = "none")+
  theme(text=element_text(size=16,  family="sans"))

lrgen_galtan_sp

jpeg(filename = "results/scatterplots/lrgen_galtan_sp_cntry_flag.jpg",units = "cm",
    width = 20.0,height=20.0,res = 600)
lrgen_galtan_sp
dev.off()

# lrgen_corrupt_salience scatterplot

lrgen_corrupt_salience_sp <- 
  ggplot(plot.dat, aes(y = corrupt_salience,
                       x = lrgen))+
  geom_flag(aes(y = corrupt_salience,
                x = lrgen,
                country=tolower(cntry),
                size=vote.share),
            position=position_jitter(seed=1234,
                                     width = 0.1, height = 0.1))+
  scale_size(range = c(4,12))+
  xlim(0,10)+
  ylim(0,10)+
  ylab("Corrupt Salience")+
  xlab("Left-Right General")+
  guides(size="none")+
  geom_label_repel(aes(label= 
                         as.character(party_name_alpha)),size = 4.0,
                   max.overlaps = 300,seed = 1236,
                   label.padding = 0.15,
                   box.padding = 0.50,
                   show.legend=FALSE,
                   position=position_jitter(seed=1234,
                                            width = 0.1, height = 0.1))+
  theme(legend.position = "none")+
  theme(text=element_text(size=16,  family="sans"))

lrgen_corrupt_salience_sp

jpeg(filename = "results/scatterplots/lrgen_corrupt_salience_sp_cntry_flag.jpg",units = "cm",
    width = 20.0,height=20.0,res = 600)
lrgen_corrupt_salience_sp
dev.off()

# lrgen_antielite_salience scatterplot

lrgen_antielite_salience_sp <- 
  ggplot(plot.dat, aes(y = antielite_salience,
                       x = lrgen))+
  geom_flag(aes(y = antielite_salience,
                x = lrgen,
                country=tolower(cntry),
                size=vote.share),
            position=position_jitter(seed=1234,
                                     width = 0.1, height = 0.1))+
  scale_size(range = c(4,12))+
  xlim(0,10)+
  ylim(0,10)+
  ylab("Antielite Salience")+
  xlab("Left-Right General")+
  guides(size="none")+
  geom_label_repel(aes(label= 
                         as.character(party_name_alpha)),size = 4.0,
                   max.overlaps = 300,seed = 1236,
                   label.padding = 0.15,
                   box.padding = 0.50,
                   show.legend=FALSE,
                   position=position_jitter(seed=1234,
                                            width = 0.1, height = 0.1))+
  theme(legend.position = "none")+
  theme(text=element_text(size=16,  family="sans"))

lrgen_antielite_salience_sp

jpeg(filename = "results/scatterplots/lrgen_antielite_salience_sp_cntry_flag.jpg",units = "cm",
    width = 20.0,height=20.0,res = 600)
lrgen_antielite_salience_sp
dev.off()


# lrecon_galtan scatterplot

lrecon_galtan_sp <- 
  ggplot(plot.dat, aes(y = galtan,
                       x = lrecon))+
  geom_flag(aes(y = galtan,
                x = lrecon,
                country=tolower(cntry),
                size=vote.share),
            position=position_jitter(seed=1234,
                                     width = 0.1, height = 0.1))+
  scale_size(range = c(4,12))+
  xlim(0,10)+
  ylim(0,10)+
  ylab("GAL-TAN")+
  xlab("Left-Right Economic")+
  guides(size="none")+
  geom_label_repel(aes(label= 
                         as.character(party_name_alpha)),size = 4.0,
                   max.overlaps = 300,seed = 1236,
                   label.padding = 0.15,
                   box.padding = 0.50,
                   show.legend=FALSE,
                   position=position_jitter(seed=1234,
                                            width = 0.1, height = 0.1))+
  theme(legend.position = "none")+
  theme(text=element_text(size=16,  family="sans"))

lrecon_galtan_sp

jpeg(filename = "results/scatterplots/lrecon_galtan_sp_cntry_flag.jpg",units = "cm",
    width = 20.0,height=20.0,res = 600)
lrecon_galtan_sp
dev.off()

# lrecon_corrupt_salience scatterplot

lrecon_corrupt_salience_sp <- 
  ggplot(plot.dat, aes(y = corrupt_salience,
                       x = lrecon))+
  geom_flag(aes(y = corrupt_salience,
                x = lrecon,
                country=tolower(cntry),
                size=vote.share),
            position=position_jitter(seed=1234,
                                     width = 0.1, height = 0.1))+
  scale_size(range = c(4,12))+
  xlim(0,10)+
  ylim(0,10)+
  ylab("Corrupt Salience")+
  xlab("Left-Right Economic")+
  guides(size="none")+
  geom_label_repel(aes(label= 
                         as.character(party_name_alpha)),size = 4.0,
                   max.overlaps = 300,seed = 1236,
                   label.padding = 0.15,
                   box.padding = 0.50,
                   show.legend=FALSE,
                   position=position_jitter(seed=1234,
                                            width = 0.1, height = 0.1))+
  theme(legend.position = "none")+
  theme(text=element_text(size=16,  family="sans"))

lrecon_corrupt_salience_sp

jpeg(filename = "results/scatterplots/lrecon_corrupt_salience_sp_cntry_flag.jpg",units = "cm",
    width = 20.0,height=20.0,res = 600)
lrecon_corrupt_salience_sp
dev.off()

# lrecon_antielite_salience scatterplot

lrecon_antielite_salience_sp <- 
  ggplot(plot.dat, aes(y = antielite_salience,
                       x = lrecon))+
  geom_flag(aes(y = antielite_salience,
                x = lrecon,
                country=tolower(cntry),
                size=vote.share),
            position=position_jitter(seed=1234,
                                     width = 0.1, height = 0.1))+
  scale_size(range = c(4,12))+
  xlim(0,10)+
  ylim(0,10)+
  ylab("Antielite Salience")+
  xlab("Left-Right Economic")+
  guides(size="none")+
  geom_label_repel(aes(label= 
                         as.character(party_name_alpha)),size = 4.0,
                   max.overlaps = 300,seed = 1236,
                   label.padding = 0.15,
                   box.padding = 0.50,
                   show.legend=FALSE,
                   position=position_jitter(seed=1234,
                                            width = 0.1, height = 0.1))+
  theme(legend.position = "none")+
  theme(text=element_text(size=16,  family="sans"))

lrecon_antielite_salience_sp

jpeg(filename = "results/scatterplots/lrecon_antielite_salience_sp_cntry_flag.jpg",units = "cm",
    width = 20.0,height=20.0,res = 600)
lrecon_antielite_salience_sp
dev.off()


# galtan_corrupt_salience scatterplot

galtan_corrupt_salience_sp <- 
  ggplot(plot.dat, aes(x = corrupt_salience,
                       y = galtan))+
  geom_flag(aes(x = corrupt_salience,
                y = galtan,
                country=tolower(cntry),
                size=vote.share),
            position=position_jitter(seed=1234,
                                     width = 0.1, height = 0.1))+
  scale_size(range = c(4,12))+
  xlim(0,10)+
  ylim(0,10)+
  xlab("Corrupt Salience")+
  ylab("GAL-TAN")+
  guides(size="none")+
  geom_label_repel(aes(label= 
                         as.character(party_name_alpha)),size = 4.0,
                   max.overlaps = 300,seed = 1236,
                   label.padding = 0.15,
                   box.padding = 0.50,
                   show.legend=FALSE,
                   position=position_jitter(seed=1234,
                                            width = 0.1, height = 0.1))+
  theme(legend.position = "none")+
  theme(text=element_text(size=16,  family="sans"))

galtan_corrupt_salience_sp

jpeg(filename = "results/scatterplots/galtan_corrupt_salience_sp_cntry_flag.jpg",units = "cm",
    width = 20.0,height=20.0,res = 600)
galtan_corrupt_salience_sp
dev.off()

# galtan_antielite_salience scatterplot

galtan_antielite_salience_sp <- 
  ggplot(plot.dat, aes(x = antielite_salience,
                       y = galtan))+
  geom_flag(aes(x = antielite_salience,
                y = galtan,
                country=tolower(cntry),
                size=vote.share),
            position=position_jitter(seed=1234,
                                     width = 0.1, height = 0.1))+
  scale_size(range = c(4,12))+
  xlim(0,10)+
  ylim(0,10)+
  xlab("Antielite Salience")+
  ylab("GAL-TAN")+
  guides(size="none")+
  geom_label_repel(aes(label= 
                         as.character(party_name_alpha)),size = 4.0,
                   max.overlaps = 300,seed = 1236,
                   label.padding = 0.15,
                   box.padding = 0.50,
                   show.legend=FALSE,
                   position=position_jitter(seed=1234,
                                            width = 0.1, height = 0.1))+
  theme(legend.position = "none")+
  theme(text=element_text(size=16,  family="sans"))

galtan_antielite_salience_sp

jpeg(filename = "results/scatterplots/galtan_antielite_salience_sp_cntry_flag.jpg",units = "cm",
    width = 20.0,height=20.0,res = 600)
galtan_antielite_salience_sp
dev.off()





# corrupt_salience_antielite_salience scatterplot

corrupt_salience_antielite_salience_sp <- 
  ggplot(plot.dat, aes(x = antielite_salience,
                       y = corrupt_salience))+
  geom_flag(aes(x = antielite_salience,
                y = corrupt_salience,
                country=tolower(cntry),
                size=vote.share),
            position=position_jitter(seed=1234,
                                     width = 0.1, height = 0.1))+
  scale_size(range = c(4,12))+
  xlim(0,10)+
  ylim(0,10)+
  xlab("Antielite Salience")+
  ylab("Corrupt Salience")+
  guides(size="none")+
  geom_label_repel(aes(label= 
                         as.character(party_name_alpha)),size = 4.0,
                   max.overlaps = 300,seed = 1236,
                   label.padding = 0.15,
                   box.padding = 0.50,
                   show.legend=FALSE,
                   position=position_jitter(seed=1234,
                                            width = 0.1, height = 0.1))+
  theme(legend.position = "none")+
  theme(text=element_text(size=16,  family="sans"))

corrupt_salience_antielite_salience_sp

jpeg(filename = "results/scatterplots/corrupt_salience_antielite_salience_sp_cntry_flag.jpg",units = "cm",
    width = 20.0,height=20.0,res = 600)
corrupt_salience_antielite_salience_sp
dev.off()

# Make exemplary two panel plot of lrecon-corrupt and galtan-antielite
lrecon_corrupt_salience_sp
galtan_antielite_salience_sp


plot_comb<-
  ggarrange(lrecon_corrupt_salience_sp,
            galtan_antielite_salience_sp,
            ncol=2, nrow=1)
plot_comb

jpeg(filename = "results/scatterplots/plot_comb_sp_cntry_flag.jpg",units = "cm",
    width = 40.0,height=20.0,res = 600)
plot_comb
dev.off()

jpeg(filename = "results/scatterplots/Figure 1.jpg",units = "cm",
     width = 40.0,height=20.0,res = 600)
plot_comb
dev.off()

# make also stacked version

plot_comb<-
  ggarrange(lrecon_corrupt_salience_sp,
            galtan_antielite_salience_sp,
            ncol=1, nrow=2)

jpeg(filename = "results/scatterplots/Figure 1 stacked.jpg",units = "cm",
     width = 20,height=40,res = 600)
plot_comb
dev.off()
