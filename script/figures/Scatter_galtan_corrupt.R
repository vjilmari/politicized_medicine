library(ggplot2)
library(rio)
library(dplyr)
library(ggrepel)

CHES<-import("data/processed/CHES_2014.vote.keys.combined.xlsx")
CHES.raw<-import("data/processed/CHES_2014.vote.keys.xlsx")


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

# trim extremely long party names



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

plot.dat$lab.criterion<-
  ifelse(
    ((plot.dat$galtan>7.5 | plot.dat$galtan <2.5) & 
       (plot.dat$corrupt_salience>7.5 | plot.dat$corrupt_salience<2.5) & 
       plot.dat$vote.share > 0.0199),TRUE,FALSE)

table(plot.dat$lab.criterion)

plot.dat$lab.criterion.2<-
  ifelse(
    ((plot.dat$lrecon>7.5 | plot.dat$lrecon <2.5) & 
       (plot.dat$galtan>7.5 | plot.dat$galtan<2.5) & 
       plot.dat$vote.share > 0.0199),TRUE,FALSE)

table(plot.dat$lab.criterion.2)

plot.dat$size.criterion<-
  ifelse(
    plot.dat$vote.share > 0.29999,TRUE,FALSE)

table(plot.dat$size.criterion)


table(plot.dat$lab.criterion,
      plot.dat$size.criterion
      )


# galtan_corrupt

galtan_corrupt_sp <- 
  ggplot(plot.dat, aes(x = corrupt_salience,
                       y = galtan)) +
  geom_point(aes(size=2*vote.share,
                 color=lab.criterion | size.criterion))+
  scale_color_manual(values=c("darkgray","black"))+
  #scale_fill_manual(values=c("gray","black"))+
  xlim(0,10)+
  ylim(0,10)+
  xlab("Corrupt salience")+
  ylab("GAL-TAN")+
  geom_label_repel(aes(label= ifelse(
    size.criterion | lab.criterion,
                              as.character(pt.cntry),
                              '')),size = 3.0,
    max.overlaps = 300,seed = 1236,
    label.padding = 0.15,
    box.padding = 0.50)+
  theme(legend.position = "none")

galtan_corrupt_sp
  
png(filename = "results/galtan_corrupt_sp.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
galtan_corrupt_sp
dev.off()


# 

# galtan_lrecon

galtan_lrecon_sp <- 
  ggplot(plot.dat, aes(x = lrecon,
                       y = galtan)) +
  geom_point(aes(size=2*vote.share,
                 color=lab.criterion.2 | size.criterion))+
  scale_color_manual(values=c("darkgray","black"))+
  #scale_fill_manual(values=c("gray","black"))+
  xlim(0,10)+
  ylim(0,10)+
  xlab("Left-Right Economic")+
  ylab("GAL-TAN")+
  geom_label_repel(aes(label= ifelse(
    size.criterion | lab.criterion.2,
    as.character(pt.cntry),
    '')),size = 3.0,
    max.overlaps = 300,seed = 1236,
    label.padding = 0.15,
    box.padding = 0.50)+
  theme(legend.position = "none")

galtan_lrecon_sp

png(filename = "results/galtan_lrecon_sp.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
galtan_lrecon_sp
dev.off()

cor(plot.dat$lrecon,plot.dat$galtan)


# make another with pre-selected parties


galtan_corrupt_sp_pre <- 
  ggplot(plot.dat, aes(x = corrupt_salience, y = galtan)) +
  geom_point(aes(size=2*vote.share))+
  #geom_text(aes(label = CHES$plot.name), size = 2)+
  #geom_text_repel(aes(label = plot.name,
  #                    x = corrupt_salience+0.2), size = 2,hjust = 0)+
  #geom_text_repel(aes(label = plot.name), size = 2)+
  geom_label_repel(aes(label = plot.name), size = 2.5, max.overlaps = 100)+
  xlim(0,10)+
  ylim(0,10)+
  xlab("Corrupt salience")+
  ylab("GAL-TAN")+
  #geom_label_repel(aes(label= ifelse(vote.share > 0.1999,
  #                            as.character(pt.name),
  #                            ''),fill=cntry),size = 2.0)+
  theme(legend.position = "none")

galtan_corrupt_sp_pre

png(filename = "results/galtan_corrupt_sp_pre.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
galtan_corrupt_sp_pre
dev.off()

# only very extreme parties


galtan_corrupt_sp_ext <- 
  ggplot(plot.dat, aes(x = corrupt_salience, y = galtan)) +
  geom_point(aes(size=2*vote.share))+
  #geom_text(aes(label = CHES$plot.name), size = 2)+
  #geom_text_repel(aes(label = plot.name,
  #                    x = corrupt_salience+0.2), size = 2,hjust = 0)+
  #geom_text_repel(aes(label = plot.name), size = 2)+
  #geom_label_repel(aes(label = plot.name), size = 2)+
  xlim(0,10)+
  ylim(0,10)+
  xlab("Corrupt salience")+
  ylab("GAL-TAN")+
  #geom_label_repel(aes(label= ifelse(vote.share > 0.1999,
  #                            as.character(pt.name),
  #                            ''),fill=cntry),size = 2.0)+
  geom_label_repel(aes(label= ifelse(
    ((galtan>7 | galtan <3) & 
       (corrupt_salience>7 | corrupt_salience<3) & 
       vote.share > 0.0499) ,
    paste0(as.character(pt.name)," (",cntry,")"),
    '')),size = 2,
    max.overlaps = 200)+
  theme(legend.position = "none")

galtan_corrupt_sp_ext

png(filename = "results/galtan_corrupt_sp_ext.png",units = "cm",
    width = 15.0,height=15.0,res = 300)
galtan_corrupt_sp_ext
dev.off()
  

# some extra stuff

cor(plot.dat$corrupt_salience,
    plot.dat$galtan)

cor(I(plot.dat$corrupt_salience^2),
    plot.dat$galtan)

cor(plot.dat$corrupt_salience,
    I(plot.dat$galtan^2))

lm1<-lm(corrupt_salience~galtan,data=plot.dat)
summary(lm1)

lm2<-lm(corrupt_salience~galtan+I(galtan^2),data=plot.dat)
summary(lm2)

anova(lm1,lm2)

lm3<-lm(corrupt_salience~galtan+I(galtan^2)+I(galtan^3),data=plot.dat)
summary(lm3)

anova(lm1,lm3)
anova(lm2,lm3)
