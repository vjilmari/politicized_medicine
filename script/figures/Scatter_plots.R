# MetBrewer colors: https://github.com/BlakeRMills/MetBrewer

library(ggplot2)
library(rio)
library(dplyr)
library(ggrepel)
library(MetBrewer)

display_all(colorblind_only = T)

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



# lrgen_lrecon scatterplot

lrgen_lrecon_sp <- 
  ggplot(plot.dat, aes(x = lrecon,
                       y = lrgen)) +
  geom_point(aes(size=2*vote.share,
                 color=(lrgen.criterion & 
                          lrecon.criterion & 
                          small.size.criterion) | large.size.criterion))+
  scale_color_manual(values=c("darkgray","black"))+
  #scale_fill_manual(values=c("gray","black"))+
  xlim(0,10)+
  ylim(0,10)+
  xlab("Left-Right Economic")+
  ylab("Left-Right General")+
  geom_label_repel(aes(label= ifelse(
    (lrgen.criterion & 
       lrecon.criterion & 
       small.size.criterion) | large.size.criterion,
    as.character(pt.cntry),
    '')),size = 3.0,
    max.overlaps = 300,seed = 1236,
    label.padding = 0.15,
    box.padding = 0.50)+
  theme(legend.position = "none")

lrgen_lrecon_sp

png(filename = "results/scatterplots/lrgen_lrecon_sp.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
lrgen_lrecon_sp
dev.off()

# lrgen_lrecon figure text

lrgen_lrecon_parties<-
  plot.dat[(plot.dat$lrgen.criterion & 
              plot.dat$lrecon.criterion & 
              plot.dat$small.size.criterion) | plot.dat$large.size.criterion,
           c("pt.cntry","pt.name")]

export(lrgen_lrecon_parties,
       "results/scatterplots/lrgen_lrecon_parties.xlsx")


# lrgen_galtan scatterplot

lrgen_galtan_sp <- 
  ggplot(plot.dat, aes(x = galtan,
                       y = lrgen)) +
  geom_point(aes(size=2*vote.share,
                 color=(lrgen.criterion & 
                          galtan.criterion & 
                          small.size.criterion) | large.size.criterion))+
  scale_color_manual(values=c("darkgray","black"))+
  #scale_fill_manual(values=c("gray","black"))+
  xlim(0,10)+
  ylim(0,10)+
  xlab("GAL-TAN")+
  ylab("Left-Right General")+
  geom_label_repel(aes(label= ifelse(
    (lrgen.criterion & 
       galtan.criterion & 
       small.size.criterion) | large.size.criterion,
    as.character(pt.cntry),
    '')),size = 3.0,
    max.overlaps = 300,seed = 1236,
    label.padding = 0.15,
    box.padding = 0.50)+
  theme(legend.position = "none")

lrgen_galtan_sp

png(filename = "results/scatterplots/lrgen_galtan_sp.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
lrgen_galtan_sp
dev.off()

# lrgen_galtan figure text

lrgen_galtan_parties<-
  plot.dat[(plot.dat$lrgen.criterion & 
              plot.dat$galtan.criterion & 
              plot.dat$small.size.criterion) | plot.dat$large.size.criterion,
           c("pt.cntry","pt.name")]

export(lrgen_galtan_parties,
       "results/scatterplots/lrgen_galtan_parties.xlsx")



# lrgen_antielite_salience scatterplot

lrgen_antielite_salience_sp <- 
  ggplot(plot.dat, aes(x = antielite_salience,
                       y = lrgen)) +
  geom_point(aes(size=2*vote.share,
                 color=(lrgen.criterion & 
                          antielite_salience.criterion & 
                          small.size.criterion) | large.size.criterion))+
  scale_color_manual(values=c("darkgray","black"))+
  #scale_fill_manual(values=c("gray","black"))+
  xlim(0,10)+
  ylim(0,10)+
  xlab("Antielite salience")+
  ylab("Left-Right General")+
  geom_label_repel(aes(label= ifelse(
    (lrgen.criterion & 
       antielite_salience.criterion & 
       small.size.criterion) | large.size.criterion,
    as.character(pt.cntry),
    '')),size = 3.0,
    max.overlaps = 300,seed = 1236,
    label.padding = 0.15,
    box.padding = 0.50)+
  theme(legend.position = "none")

lrgen_antielite_salience_sp

png(filename = "results/scatterplots/lrgen_antielite_salience_sp.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
lrgen_antielite_salience_sp
dev.off()

# lrgen_antielite_salience figure text

lrgen_antielite_salience_parties<-
  plot.dat[(plot.dat$lrgen.criterion & 
              plot.dat$antielite_salience.criterion & 
              plot.dat$small.size.criterion) | plot.dat$large.size.criterion,
           c("pt.cntry","pt.name")]

export(lrgen_antielite_salience_parties,
       "results/scatterplots/lrgen_antielite_salience_parties.xlsx")


# lrgen_corrupt_salience scatterplot

lrgen_corrupt_salience_sp <- 
  ggplot(plot.dat, aes(x = corrupt_salience,
                       y = lrgen)) +
  geom_point(aes(size=2*vote.share,
                 color=(lrgen.criterion & 
                          corrupt_salience.criterion & 
                          small.size.criterion) | large.size.criterion))+
  scale_color_manual(values=c("darkgray","black"))+
  #scale_fill_manual(values=c("gray","black"))+
  xlim(0,10)+
  ylim(0,10)+
  xlab("Corrupt salience")+
  ylab("Left-Right General")+
  geom_label_repel(aes(label= ifelse(
    (lrgen.criterion & 
       corrupt_salience.criterion & 
       small.size.criterion) | large.size.criterion,
    as.character(pt.cntry),
    '')),size = 3.0,
    max.overlaps = 300,seed = 1236,
    label.padding = 0.15,
    box.padding = 0.50)+
  theme(legend.position = "none")

lrgen_corrupt_salience_sp

png(filename = "results/scatterplots/lrgen_corrupt_salience_sp.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
lrgen_corrupt_salience_sp
dev.off()

# lrgen_corrupt_salience figure text

lrgen_corrupt_salience_parties<-
  plot.dat[(plot.dat$lrgen.criterion & 
              plot.dat$corrupt_salience.criterion & 
              plot.dat$small.size.criterion) | plot.dat$large.size.criterion,
           c("pt.cntry","pt.name")]

export(lrgen_corrupt_salience_parties,
       "results/scatterplots/lrgen_corrupt_salience_parties.xlsx")


# lrecon_galtan scatterplot

lrecon_galtan_sp <- 
  ggplot(plot.dat, aes(y = galtan,
                       x = lrecon)) +
  geom_point(aes(size=2*vote.share,
                 color=(lrecon.criterion & 
                          galtan.criterion & 
                          small.size.criterion) | large.size.criterion))+
  scale_color_manual(values=c("darkgray","black"))+
  #scale_fill_manual(values=c("gray","black"))+
  xlim(0,10)+
  ylim(0,10)+
  ylab("GAL-TAN")+
  xlab("Left-Right Economic")+
  geom_label_repel(aes(label= ifelse(
    (lrecon.criterion & 
       galtan.criterion & 
       small.size.criterion) | large.size.criterion,
    as.character(pt.cntry),
    '')),size = 3.0,
    max.overlaps = 300,seed = 1236,
    label.padding = 0.15,
    box.padding = 0.50)+
  theme(legend.position = "none")

lrecon_galtan_sp

png(filename = "results/scatterplots/lrecon_galtan_sp.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
lrecon_galtan_sp
dev.off()

# lrecon_galtan figure text

lrecon_galtan_parties<-
  plot.dat[(plot.dat$lrecon.criterion & 
              plot.dat$galtan.criterion & 
              plot.dat$small.size.criterion) | plot.dat$large.size.criterion,
           c("pt.cntry","pt.name")]

export(lrecon_galtan_parties,
       "results/scatterplots/lrecon_galtan_parties.xlsx")


# lrecon_antielite_salience scatterplot

lrecon_antielite_salience_sp <- 
  ggplot(plot.dat, aes(y = antielite_salience,
                       x = lrecon)) +
  geom_point(aes(size=2*vote.share,
                 color=(lrecon.criterion & 
                          antielite_salience.criterion & 
                          small.size.criterion) | large.size.criterion))+
  scale_color_manual(values=c("darkgray","black"))+
  #scale_fill_manual(values=c("gray","black"))+
  xlim(0,10)+
  ylim(0,10)+
  ylab("Antielite salience")+
  xlab("Left-Right Economic")+
  geom_label_repel(aes(label= ifelse(
    (lrecon.criterion & 
       antielite_salience.criterion & 
       small.size.criterion) | large.size.criterion,
    as.character(pt.cntry),
    '')),size = 3.0,
    max.overlaps = 300,seed = 1236,
    label.padding = 0.15,
    box.padding = 0.50)+
  theme(legend.position = "none")

lrecon_antielite_salience_sp

png(filename = "results/scatterplots/lrecon_antielite_salience_sp.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
lrecon_antielite_salience_sp
dev.off()

# lrecon_antielite_salience figure text

lrecon_antielite_salience_parties<-
  plot.dat[(plot.dat$lrecon.criterion & 
              plot.dat$antielite_salience.criterion & 
              plot.dat$small.size.criterion) | plot.dat$large.size.criterion,
           c("pt.cntry","pt.name")]

export(lrecon_antielite_salience_parties,
       "results/scatterplots/lrecon_antielite_salience_parties.xlsx")



# lrecon_corrupt_salience scatterplot

lrecon_corrupt_salience_sp <- 
  ggplot(plot.dat, aes(y = corrupt_salience,
                       x = lrecon)) +
  geom_point(aes(size=2*vote.share,
                 color=(lrecon.criterion & 
                          corrupt_salience.criterion & 
                          small.size.criterion) | large.size.criterion))+
  scale_color_manual(values=c("darkgray","black"))+
  #scale_fill_manual(values=c("gray","black"))+
  xlim(0,10)+
  ylim(0,10)+
  ylab("Corrupt salience")+
  xlab("Left-Right Economic")+
  geom_label_repel(aes(label= ifelse(
    (lrecon.criterion & 
       corrupt_salience.criterion & 
       small.size.criterion) | large.size.criterion,
    as.character(pt.cntry),
    '')),size = 3.0,
    max.overlaps = 300,seed = 1236,
    label.padding = 0.15,
    box.padding = 0.50)+
  theme(legend.position = "none")

lrecon_corrupt_salience_sp

png(filename = "results/scatterplots/lrecon_corrupt_salience_sp.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
lrecon_corrupt_salience_sp
dev.off()

# lrecon_corrupt_salience figure text

lrecon_corrupt_salience_parties<-
  plot.dat[(plot.dat$lrecon.criterion & 
              plot.dat$corrupt_salience.criterion & 
              plot.dat$small.size.criterion) | plot.dat$large.size.criterion,
           c("pt.cntry","pt.name")]

export(lrecon_corrupt_salience_parties,
       "results/scatterplots/lrecon_corrupt_salience_parties.xlsx")


# galtan_antielite_salience scatterplot

galtan_antielite_salience_sp <- 
  ggplot(plot.dat, aes(x = antielite_salience,
                       y = galtan)) +
  geom_point(aes(size=2*vote.share,
                 color=(galtan.criterion & 
                          antielite_salience.criterion & 
                          small.size.criterion) | large.size.criterion))+
  scale_color_manual(values=c("darkgray","black"))+
  #scale_fill_manual(values=c("gray","black"))+
  xlim(0,10)+
  ylim(0,10)+
  ylab("GAL-TAN")+
  xlab("Antielite salience")+
  geom_label_repel(aes(label= ifelse(
    (galtan.criterion & 
       antielite_salience.criterion & 
       small.size.criterion) | large.size.criterion,
    as.character(pt.cntry),
    '')),size = 3.0,
    max.overlaps = 300,seed = 1236,
    label.padding = 0.15,
    box.padding = 0.50)+
  theme(legend.position = "none")

galtan_antielite_salience_sp

png(filename = "results/scatterplots/galtan_antielite_salience_sp.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
galtan_antielite_salience_sp
dev.off()

# galtan_antielite_salience figure text

galtan_antielite_salience_parties<-
  plot.dat[(plot.dat$galtan.criterion & 
              plot.dat$antielite_salience.criterion & 
              plot.dat$small.size.criterion) | plot.dat$large.size.criterion,
           c("pt.cntry","pt.name")]

export(galtan_antielite_salience_parties,
       "results/scatterplots/galtan_antielite_salience_parties.xlsx")



# galtan_corrupt_salience scatterplot

galtan_corrupt_salience_sp <- 
  ggplot(plot.dat, aes(x = corrupt_salience,
                       y = galtan)) +
  geom_point(aes(size=2*vote.share,
                 color=(galtan.criterion & 
                          corrupt_salience.criterion & 
                          small.size.criterion) | large.size.criterion))+
  scale_color_manual(values=c("darkgray","black"))+
  #scale_fill_manual(values=c("gray","black"))+
  xlim(0,10)+
  ylim(0,10)+
  ylab("GAL-TAN")+
  xlab("Corrupt salience")+
  geom_label_repel(aes(label= ifelse(
    (galtan.criterion & 
       corrupt_salience.criterion & 
       small.size.criterion) | large.size.criterion,
    as.character(pt.cntry),
    '')),size = 3.0,
    max.overlaps = 300,seed = 1236,
    label.padding = 0.15,
    box.padding = 0.50)+
  theme(legend.position = "none")

galtan_corrupt_salience_sp

png(filename = "results/scatterplots/galtan_corrupt_salience_sp.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
galtan_corrupt_salience_sp
dev.off()

# galtan_corrupt_salience figure text

galtan_corrupt_salience_parties<-
  plot.dat[(plot.dat$galtan.criterion & 
              plot.dat$corrupt_salience.criterion & 
              plot.dat$small.size.criterion) | plot.dat$large.size.criterion,
           c("pt.cntry","pt.name")]

export(galtan_corrupt_salience_parties,
       "results/scatterplots/galtan_corrupt_salience_parties.xlsx")



# corrupt_salience_antielite_salience scatterplot

corrupt_salience_antielite_salience_sp <- 
  ggplot(plot.dat, aes(x = antielite_salience,
                       y = corrupt_salience)) +
  geom_point(aes(size=2*vote.share,
                 color=(corrupt_salience.criterion & 
                          antielite_salience.criterion & 
                          small.size.criterion) | large.size.criterion))+
  scale_color_manual(values=c("darkgray","black"))+
  #scale_fill_manual(values=c("gray","black"))+
  xlim(0,10)+
  ylim(0,10)+
  ylab("Corrupt salience")+
  xlab("Antielite salience")+
  geom_label_repel(aes(label= ifelse(
    (corrupt_salience.criterion & 
       antielite_salience.criterion & 
       small.size.criterion) | large.size.criterion,
    as.character(pt.cntry),
    '')),size = 3.0,
    max.overlaps = 300,seed = 1236,
    label.padding = 0.15,
    box.padding = 0.50)+
  theme(legend.position = "none")

corrupt_salience_antielite_salience_sp

png(filename = "results/scatterplots/corrupt_salience_antielite_salience_sp.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
corrupt_salience_antielite_salience_sp
dev.off()

# corrupt_salience_antielite_salience figure text

corrupt_salience_antielite_salience_parties<-
  plot.dat[(plot.dat$corrupt_salience.criterion & 
              plot.dat$antielite_salience.criterion & 
              plot.dat$small.size.criterion) | plot.dat$large.size.criterion,
           c("pt.cntry","pt.name")]

export(corrupt_salience_antielite_salience_parties,
       "results/scatterplots/corrupt_salience_antielite_salience_parties.xlsx")







