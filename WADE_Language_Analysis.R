library(plyr)
library(tidyverse)
library(lme4)
library(Rmisc)
library(reshape2)
library(wPerm)

'%!in%' = Negate('%in%')


################################################  LOAD & CLEAN UP RAW DATA  ######################################

### Load in the formants file
formants<-read_csv("WordNamingGame_Formants.csv")
formants$filename<-formants$Filename

### Load in the Ibex data and get rid of duplicate rows
ibex<-read_csv("WordNamingGame_PCIbexResults.csv")
ibex.unique<-ibex %>%
  distinct(filename, .keep_all = TRUE)

### Merge by filename and word
exp1a<-merge(ibex.unique, formants, by=c("filename"))

### Load in demographic data # (WordNamingGame_Demographics.csv)
dems<-read_csv("WordNamingGame_Demographics.csv")
dems$time<-dems$Time
dems.unique<-dems %>%
  distinct(time, .keep_all = TRUE)
exp1<-merge(exp1a, dems.unique, by="time")

### Get rid of participants who missed more than 30 words
exp1.phone<-exp1[exp1$phone%in%c("AY1", "AY2"),]
exp1.phone.sums<-summarySE(exp1.phone, measurevar="f1_80", groupvars=c("time", "Dialect", "voice"), na.rm=T)
to.omit<-exp1.phone.sums[exp1.phone.sums$N<60,]
to.omit.part<-unique(to.omit$time)  
exp1<-exp1[exp1$time%!in%c(to.omit.part),]

### Outlier Trimming
by.part<-Rmisc::summarySE(exp1, measurevar="f1_80", groupvar="time", na.rm=T)
by.part$max.p<-by.part$f1_80+(by.part$sd*3)
by.part$min.p<-by.part$f1_80-(by.part$sd*3)
by.part.trim<-dplyr::select(by.part, time, max.p, min.p)
exp1<-merge(exp1, by.part.trim, by="time")
exp1$outlier<-"no"
exp1$outlier[exp1$f1_80>exp1$max.p]<-"yes"
exp1$outlier[exp1$f1_80<exp1$min.p]<-"yes"

by.word<-Rmisc::summarySE(exp1, measurevar="f1_80", groupvar="word.x", na.rm=T)
by.word$max.w<-by.word$f1_80+(by.word$sd*3)
by.word$min.w<-by.word$f1_80-(by.word$sd*3)
by.word.trim<-dplyr::select(by.word, word.x, max.w, min.w)
exp1<-merge(exp1, by.word.trim, by="word.x")
exp1$outlier[exp1$f1_80>exp1$max.w]<-"yes"
exp1$outlier[exp1$f1_80<exp1$min.w]<-"yes"

exp1.trimmed<-exp1[exp1$outlier!="yes",]

### Note that performing the same trimming analyses on F2 does not find any additional outliers 
### besides those omitted based on the F1 omission

### Normalize with Lobanov method
exp1.normed<-exp1.trimmed %>%
  group_by(time) %>%
  mutate(z1_80=scale(f1_80), z2_80=scale(f2_80), 
         z1_70=scale(f1_70), z2_70=scale(f2_70),
         z1_50=scale(f1_50), z2_50=scale(f2_50),
         z1_30=scale(f1_30), z2_30=scale(f2_30),
         z1_20=scale(f1_20), z2_20=scale(f2_20),
         z1_10=scale(f1_10), z2_10=scale(f2_10))

exp1.ay<-exp1.normed[exp1.normed$phone=="AY1" | exp1.normed$phone=="AY2",]

### Make "participant" column from unique time identifier
exp1.ay$participant<-as.factor(exp1.ay$time)

### Make diagonal measure
exp1.ay$diag<-exp1.ay$z2_80-(2*exp1.ay$z1_80)

### Add in frequency data
freq<-read_csv("word_freq.csv")
exp1.ay$word<-exp1.ay$word.x
exp1.ay<-merge(exp1.ay, freq, by="word")

exp1.ay$phase<-as.factor(exp1.ay$phase)
exp1.ay$voice<-as.factor(exp1.ay$voice)
exp1.ay$Dialect<-as.factor(exp1.ay$Dialect)

### Add in participant's baseline
baseline<-exp1.ay[exp1.ay$phase=="baseline",]
baseline.sums <- baseline %>%
  group_by(participant) %>%
  summarise(baseline.diag=mean(diag))
exp1.ay<-merge(baseline.sums, exp1.ay, by="participant")



##################################################   CODE THE SURVEY DATA   #############################################

# Create a "Familiarity with the South" subscore
exp1.ay$Familiarity<-
  exp1.ay$I.Lived.in.South+
  exp1.ay$I.Familiar.South.Speech+
  exp1.ay$I.Friends.South+
  exp1.ay$I.have.Southern.accent+
  exp1.ay$Parents.have.accent+
  exp1.ay$Colleagues.have.accent+
  exp1.ay$I.imitate.accent
# Create a "Talker Likability" subscore
exp1.ay$Likable<-
  exp1.ay$Happy+
  exp1.ay$Kind+
  exp1.ay$Friendly+
  exp1.ay$Would.Be.Friends+
  exp1.ay$Relatable
# Create a "Prestige" subscore
exp1.ay$Prestige<-
  exp1.ay$Intelligent+
  exp1.ay$Wealthy+
  exp1.ay$Professional+
  exp1.ay$Attractive+
  exp1.ay$Educated

### Marlowe-Crowne Social Desirability Scale
exp1.ay$SocDes1=ifelse(exp1.ay$Q13.1_1==TRUE,1,0)
exp1.ay$SocDes2=ifelse(exp1.ay$Q13.1_2==TRUE,1,0)
exp1.ay$SocDes3=ifelse(exp1.ay$Q13.1_3==FALSE,1,0)
exp1.ay$SocDes4=ifelse(exp1.ay$Q13.1_4==TRUE,1,0)
exp1.ay$SocDes5=ifelse(exp1.ay$Q13.1_5==FALSE,1,0)
exp1.ay$SocDes6=ifelse(exp1.ay$Q13.1_6==FALSE,1,0)
exp1.ay$SocDes7=ifelse(exp1.ay$Q13.1_7==TRUE,1,0)
exp1.ay$SocDes8=ifelse(exp1.ay$Q13.1_8==TRUE,1,0)
exp1.ay$SocDes9=ifelse(exp1.ay$Q13.1_9==FALSE,1,0)
exp1.ay$SocDes10=ifelse(exp1.ay$Q13.1_10==FALSE,1,0)
exp1.ay$SocDes11=ifelse(exp1.ay$Q13.1_11==FALSE,1,0)
exp1.ay$SocDes12=ifelse(exp1.ay$Q13.1_12==FALSE,1,0)
exp1.ay$SocDes13=ifelse(exp1.ay$Q13.1_13==TRUE,1,0)
exp1.ay$SocDes14=ifelse(exp1.ay$Q13.1_14==FALSE,1,0)
exp1.ay$SocDes15=ifelse(exp1.ay$Q13.1_15==FALSE,1,0)
exp1.ay$SocDes16=ifelse(exp1.ay$Q13.1_16==TRUE,1,0)
exp1.ay$SocDes17=ifelse(exp1.ay$Q13.1_17==TRUE,1,0)
exp1.ay$SocDes18=ifelse(exp1.ay$Q13.1_18==TRUE,1,0)
exp1.ay$SocDes19=ifelse(exp1.ay$Q13.1_19==FALSE,1,0)
exp1.ay$SocDes20=ifelse(exp1.ay$Q13.1_20==TRUE,1,0)
exp1.ay$SocDes21=ifelse(exp1.ay$Q13.1_21==TRUE,1,0)
exp1.ay$SocDes22=ifelse(exp1.ay$Q13.1_22==FALSE,1,0)
exp1.ay$SocDes23=ifelse(exp1.ay$Q13.1_23==FALSE,1,0)
exp1.ay$SocDes24=ifelse(exp1.ay$Q13.1_24==TRUE,1,0)
exp1.ay$SocDes25=ifelse(exp1.ay$Q13.1_25==TRUE,1,0)
exp1.ay$SocDes26=ifelse(exp1.ay$Q13.1_26==TRUE,1,0)
exp1.ay$SocDes27=ifelse(exp1.ay$Q13.1_27==TRUE,1,0)
exp1.ay$SocDes28=ifelse(exp1.ay$Q13.1_28==FALSE,1,0)
exp1.ay$SocDes29=ifelse(exp1.ay$Q13.1_29==TRUE,1,0)
exp1.ay$SocDes30=ifelse(exp1.ay$Q13.1_30==FALSE,1,0)
exp1.ay$SocDes31=ifelse(exp1.ay$Q13.1_31==TRUE,1,0)
exp1.ay$SocDes32=ifelse(exp1.ay$Q13.1_32==FALSE,1,0)
exp1.ay$SocDes33=ifelse(exp1.ay$Q13.1_33==TRUE,1,0)

exp1.ay$SocDes<-exp1.ay$SocDes1+
  exp1.ay$SocDes2+exp1.ay$SocDes3+
  exp1.ay$SocDes4+exp1.ay$SocDes5+
  exp1.ay$SocDes6+exp1.ay$SocDes7+
  exp1.ay$SocDes8+exp1.ay$SocDes9+
  exp1.ay$SocDes10+exp1.ay$SocDes11+
  exp1.ay$SocDes12+exp1.ay$SocDes13+
  exp1.ay$SocDes14+exp1.ay$SocDes15+
  exp1.ay$SocDes16+exp1.ay$SocDes17+
  exp1.ay$SocDes18+exp1.ay$SocDes19+
  exp1.ay$SocDes20+exp1.ay$SocDes21+
  exp1.ay$SocDes22+exp1.ay$SocDes23+
  exp1.ay$SocDes24+exp1.ay$SocDes25+
  exp1.ay$SocDes26+exp1.ay$SocDes27+
  exp1.ay$SocDes28+exp1.ay$SocDes29+
  exp1.ay$SocDes30+exp1.ay$SocDes31+
  exp1.ay$SocDes32+exp1.ay$SocDes33


### Big Five
# Reverse items DO NOT RUN THESE AGAIN!!!!
exp1.ay$Q11_6<-(0-exp1.ay$Q11_6)+6
exp1.ay$Q11_21<-(0-exp1.ay$Q11_21)+6
exp1.ay$Q11_31<-(0-exp1.ay$Q11_31)+6
exp1.ay$Q11_2<-(0-exp1.ay$Q11_2)+6
exp1.ay$Q11_12<-(0-exp1.ay$Q11_12)+6
exp1.ay$Q11_27<-(0-exp1.ay$Q11_27)+6
exp1.ay$Q11_37<-(0-exp1.ay$Q11_37)+6
exp1.ay$Q11_8<-(0-exp1.ay$Q11_8)+6
exp1.ay$Q11_18<-(0-exp1.ay$Q11_18)+6
exp1.ay$Q11_23<-(0-exp1.ay$Q11_23)+6
exp1.ay$Q11_43<-(0-exp1.ay$Q11_43)+6
exp1.ay$Q11_9<-(0-exp1.ay$Q11_9)+6
exp1.ay$Q11_24<-(0-exp1.ay$Q11_24)+6
exp1.ay$Q11_34<-(0-exp1.ay$Q11_34)+6
exp1.ay$Q11_35<-(0-exp1.ay$Q11_35)+6
exp1.ay$Q11_41<-(0-exp1.ay$Q11_41)+6

exp1.ay$extraversion<-
  exp1.ay$Q11_1+
  exp1.ay$Q11_6+
  exp1.ay$Q11_11+
  exp1.ay$Q11_16+
  exp1.ay$Q11_21+
  exp1.ay$Q11_26+
  exp1.ay$Q11_31+
  exp1.ay$Q11_36
exp1.ay$agreeableness<-
  exp1.ay$Q11_2 +
  exp1.ay$Q11_7 +
  exp1.ay$Q11_12 +
  exp1.ay$Q11_17 +
  exp1.ay$Q11_22 +
  exp1.ay$Q11_27 +
  exp1.ay$Q11_32 +
  exp1.ay$Q11_37 +
  exp1.ay$Q11_42 
exp1.ay$conscientiousness<-
  exp1.ay$Q11_3 +
  exp1.ay$Q11_8 +
  exp1.ay$Q11_13 +
  exp1.ay$Q11_18 +
  exp1.ay$Q11_23 +
  exp1.ay$Q11_28 +
  exp1.ay$Q11_33 +
  exp1.ay$Q11_38 +
  exp1.ay$Q11_43 
exp1.ay$neuroticism<-
  exp1.ay$Q11_4 +
  exp1.ay$Q11_9 +
  exp1.ay$Q11_14 +
  exp1.ay$Q11_19 +
  exp1.ay$Q11_24 +
  exp1.ay$Q11_29 +
  exp1.ay$Q11_34 +
  exp1.ay$Q11_39 
exp1.ay$openness<-
  exp1.ay$Q11_5 +
  exp1.ay$Q11_10 +
  exp1.ay$Q11_15 +
  exp1.ay$Q11_20 +
  exp1.ay$Q11_25 +
  exp1.ay$Q11_30 +
  exp1.ay$Q11_35 +
  exp1.ay$Q11_40 +
  exp1.ay$Q11_41 + 
  exp1.ay$Q11_44

#### AQ scores
exp1.ay$AQ1<-exp1.ay$Q12_51
exp1.ay$AQ2<-exp1.ay$Q12_52
exp1.ay$AQ3<-exp1.ay$Q12_53
exp1.ay$AQ4<-exp1.ay$Q12_54
exp1.ay$AQ5<-exp1.ay$Q12_55
exp1.ay$AQ6<-exp1.ay$Q12_56
exp1.ay$AQ7<-exp1.ay$Q12_57
exp1.ay$AQ8<-exp1.ay$Q12_58
exp1.ay$AQ9<-exp1.ay$Q12_59
exp1.ay$AQ10<-exp1.ay$Q12_60
exp1.ay$AQ11<-exp1.ay$Q12_61
exp1.ay$AQ12<-exp1.ay$Q12_62
exp1.ay$AQ13<-exp1.ay$Q12_63
exp1.ay$AQ14<-exp1.ay$Q12_64
exp1.ay$AQ15<-exp1.ay$Q12_65
exp1.ay$AQ16<-exp1.ay$Q12_66
exp1.ay$AQ17<-exp1.ay$Q12_67
exp1.ay$AQ18<-exp1.ay$Q12_68
exp1.ay$AQ19<-exp1.ay$Q12_69
exp1.ay$AQ20<-exp1.ay$Q12_70
exp1.ay$AQ21<-exp1.ay$Q12_71
exp1.ay$AQ22<-exp1.ay$Q12_72
exp1.ay$AQ23<-exp1.ay$Q12_73
exp1.ay$AQ24<-exp1.ay$Q12_74
exp1.ay$AQ25<-exp1.ay$Q12_75
exp1.ay$AQ26<-exp1.ay$Q12_76
exp1.ay$AQ27<-exp1.ay$Q12_77
exp1.ay$AQ28<-exp1.ay$Q12_78
exp1.ay$AQ29<-exp1.ay$Q12_79
exp1.ay$AQ30<-exp1.ay$Q12_80
exp1.ay$AQ31<-exp1.ay$Q12_81
exp1.ay$AQ32<-exp1.ay$Q12_82
exp1.ay$AQ33<-exp1.ay$Q12_83
exp1.ay$AQ34<-exp1.ay$Q12_84
exp1.ay$AQ35<-exp1.ay$Q12_85
exp1.ay$AQ36<-exp1.ay$Q12_86
exp1.ay$AQ37<-exp1.ay$Q12_87
exp1.ay$AQ38<-exp1.ay$Q12_88
exp1.ay$AQ39<-exp1.ay$Q12_89
exp1.ay$AQ40<-exp1.ay$Q12_90
exp1.ay$AQ41<-exp1.ay$Q12_91
exp1.ay$AQ42<-exp1.ay$Q12_92
exp1.ay$AQ43<-exp1.ay$Q12_93
exp1.ay$AQ44<-exp1.ay$Q12_94
exp1.ay$AQ45<-exp1.ay$Q12_95
exp1.ay$AQ46<-exp1.ay$Q12_96
exp1.ay$AQ47<-exp1.ay$Q12_97
exp1.ay$AQ48<-exp1.ay$Q12_98
exp1.ay$AQ49<-exp1.ay$Q12_99
exp1.ay$AQ50<-exp1.ay$Q12_100

exp1.ay$AQ13<-(1-exp1.ay$AQ13)+4
exp1.ay$AQ22<-(1-exp1.ay$AQ22)+4
exp1.ay$AQ45<-(1-exp1.ay$AQ45)+4
exp1.ay$AQ2<-(1-exp1.ay$AQ2)+4
exp1.ay$AQ4<-(1-exp1.ay$AQ4)+4
exp1.ay$AQ16<-(1-exp1.ay$AQ16)+4
exp1.ay$AQ43<-(1-exp1.ay$AQ43)+4
exp1.ay$AQ46<-(1-exp1.ay$AQ46)+4
exp1.ay$AQ7<-(1-exp1.ay$AQ7)+4
exp1.ay$AQ18<-(1-exp1.ay$AQ18)+4
exp1.ay$AQ26<-(1-exp1.ay$AQ26)+4
exp1.ay$AQ33<-(1-exp1.ay$AQ33)+4
exp1.ay$AQ35<-(1-exp1.ay$AQ35)+4
exp1.ay$AQ39<-(1-exp1.ay$AQ39)+4
exp1.ay$AQ20<-(1-exp1.ay$AQ20)+4
exp1.ay$AQ21<-(1-exp1.ay$AQ21)+4
exp1.ay$AQ41<-(1-exp1.ay$AQ41)+4
exp1.ay$AQ42<-(1-exp1.ay$AQ42)+4
exp1.ay$AQ5<-(1-exp1.ay$AQ5)+4
exp1.ay$AQ6<-(1-exp1.ay$AQ6)+4
exp1.ay$AQ9<-(1-exp1.ay$AQ9)+4
exp1.ay$AQ12<-(1-exp1.ay$AQ12)+4
exp1.ay$AQ19<-(1-exp1.ay$AQ19)+4
exp1.ay$AQ23<-(1-exp1.ay$AQ23)+4

exp1.ay$AQ<-exp1.ay$AQ1+exp1.ay$AQ2+exp1.ay$AQ3+exp1.ay$AQ4+exp1.ay$AQ5+exp1.ay$AQ6+exp1.ay$AQ7+exp1.ay$AQ8+exp1.ay$AQ9+exp1.ay$AQ10+
  exp1.ay$AQ11+exp1.ay$AQ12+exp1.ay$AQ13+exp1.ay$AQ14+exp1.ay$AQ15+exp1.ay$AQ16+exp1.ay$AQ17+exp1.ay$AQ18+exp1.ay$AQ19+exp1.ay$AQ20+
  exp1.ay$AQ21+exp1.ay$AQ22+exp1.ay$AQ23+exp1.ay$AQ24+exp1.ay$AQ25+exp1.ay$AQ26+exp1.ay$AQ27+exp1.ay$AQ28+exp1.ay$AQ29+exp1.ay$AQ30+
  exp1.ay$AQ31+exp1.ay$AQ32+exp1.ay$AQ33+exp1.ay$AQ34+exp1.ay$AQ35+exp1.ay$AQ36+exp1.ay$AQ37+exp1.ay$AQ38+exp1.ay$AQ39+exp1.ay$AQ40+
  exp1.ay$AQ41+exp1.ay$AQ42+exp1.ay$AQ43+exp1.ay$AQ44+exp1.ay$AQ45+exp1.ay$AQ46+exp1.ay$AQ47+exp1.ay$AQ48+exp1.ay$AQ49+exp1.ay$AQ50

exp1.ay$SS<-exp1.ay$AQ1+
  exp1.ay$AQ11+
  exp1.ay$AQ13+
  exp1.ay$AQ15+
  exp1.ay$AQ22+
  exp1.ay$AQ36+
  exp1.ay$AQ44+
  exp1.ay$AQ45+
  exp1.ay$AQ47+
  exp1.ay$AQ48
exp1.ay$AS<-exp1.ay$AQ2+
  exp1.ay$AQ4+
  exp1.ay$AQ10+
  exp1.ay$AQ16+
  exp1.ay$AQ25+
  exp1.ay$AQ32+
  exp1.ay$AQ34+
  exp1.ay$AQ37+
  exp1.ay$AQ43+
  exp1.ay$AQ46
exp1.ay$AD<-exp1.ay$AQ5+
  exp1.ay$AQ6+
  exp1.ay$AQ9+
  exp1.ay$AQ12+
  exp1.ay$AQ19+
  exp1.ay$AQ23+
  exp1.ay$AQ28+
  exp1.ay$AQ29+
  exp1.ay$AQ30+
  exp1.ay$AQ49
exp1.ay$C<-exp1.ay$AQ7+
  exp1.ay$AQ17+
  exp1.ay$AQ18+
  exp1.ay$AQ26+
  exp1.ay$AQ27+
  exp1.ay$AQ31+
  exp1.ay$AQ33+
  exp1.ay$AQ35+
  exp1.ay$AQ38+
  exp1.ay$AQ39
exp1.ay$I<-exp1.ay$AQ3+
  exp1.ay$AQ8+
  exp1.ay$AQ14+
  exp1.ay$AQ21+
  exp1.ay$AQ24+
  exp1.ay$AQ40+
  exp1.ay$AQ41+
  exp1.ay$AQ42+
  exp1.ay$AQ50



######################################################   DATA VISUALISATION   ##############################################

### SHIFT BY PHASE & CONDITION
exp1.ay.sums<-summarySE(exp1.ay, measurevar="diag", groupvars=c("voice", "phase"))
exp1.ay.sums$Voice<-"Southern Voice Condition"
exp1.ay.sums$Voice[exp1.ay.sums$voice=="Midland"]<-"Midland Voice Condition"
exp1.ay.sums<-na.omit(exp1.ay.sums)
ggplot(exp1.ay.sums, aes(phase, diag, color=phase))+
  geom_point(size=2.5)+
  facet_grid(~Voice)+
  geom_errorbar(aes(ymin=diag-ci, ymax=diag+ci), size=1)+
  theme_bw()+
  scale_color_manual(values=c("black", "gray55", "gray75"))+
  xlab("Experiment Phase")+
  ylab("Front Diagonal (Normalized F2-2*F1)")+
  theme(legend.position="none")+
  ggtitle("Main Experiment")

### SHIFT BY PHASE, CONDITION & DIALECT
exp1.ay.sums<-summarySE(exp1.ay, measurevar="diag", groupvars=c("voice", "phase", "Dialect"))
exp1.ay.sums$Voice<-"Southern Voice Condition"
exp1.ay.sums$Voice[exp1.ay.sums$voice=="Midland"]<-"Midland Voice Condition"
exp1.ay.sums<-na.omit(exp1.ay.sums)
ggplot(exp1.ay.sums, aes(phase, diag, color=phase))+
  geom_point(size=2.5)+
  facet_grid(Dialect~Voice)+
  geom_errorbar(aes(ymin=diag-ci, ymax=diag+ci), size=1)+
  theme_bw()+
  scale_color_manual(values=c("black", "gray55", "gray75"))+
  xlab("Experiment Phase")+
  ylab("Front Diagonal (Normalized F2-2*F1)")+
  theme(legend.position="none")+
  ggtitle("Main Experiment")

### PARTICIPANT SHIFT (INDIVIDUAL DIFFERENCES)
exp1.ay.sums<-summarySE(exp1.ay, , measurevar="diag", groupvars=c("voice", "phase", "time", "Dialect"))
exp1.ay.sums$Voice<-"Southern Voice Condition"
exp1.ay.sums$Voice[exp1.ay.sums$voice=="Midland"]<-"Midland Voice Condition"
exp1.ay.sums$participant<-as.factor(exp1.ay.sums$time)
exp1.ay.sums.wide<-spread(exp1.ay.sums, phase, diag)
exp1.ay.sums<-na.omit(exp1.ay.sums)
ggplot(exp1.ay.sums[exp1.ay.sums$voice=="Southern" & exp1.ay.sums$phase!="post",], aes(phase, diag))+
  geom_point(size=3, shape=1, alpha=.5)+
  facet_wrap(~Dialect)+
  geom_line(aes(color=ave(diag,participant,FUN=diff)>=0, group=participant), alpha=.4, size=1)+
  theme_bw()+
  scale_color_manual(values=c("dodgerblue4", "orangered3"))+
  xlab("Experiment Phase")+
  ylab("Normalized F1 at glide (80%)")+
  theme(legend.position="none")


#### FINAL MODEL #####
fit1<-lmer(scale(diag)~
             phase*relevel(voice, "Southern")*scale(baseline.diag)+
             phase*relevel(voice, "Southern")*Dialect+
             scale(freq)+
             scale(duration)+
             (scale(baseline.diag)+scale(duration)|word)+
             (phase+scale(duration)+scale(freq)|participant),
           data=exp1.ay, 
           contrasts=list(Dialect=contr.sum),
           control = lmerControl(optimizer = "bobyqa", 
                                 optCtrl = list(maxfun=2e5)))

summary(fit1)

# extract coefs from model
coefs<-data.frame(ranef(fit1)$participant)
coefs$participant<-rownames(coefs)
coefs<-select(coefs, participant, phaseexposure)

#### merge w/ survey data
exp1.survey<-select(exp1.ay, participant, ProlificID, group,
                    Kind, Wealthy, Friendly, Intelligent, Professional, Attractive, Speaker.from,
                    Would.Be.Friends, From.South, From.Midwest, Happy, Relatable, Educated,
                    I.Lived.in.South, I.Familiar.South.Speech, I.Friends.South, I.Would.Live.South, 
                    I.Like.Southerners.Talk, I.have.Southern.accent, Parents.have.accent, Colleagues.have.accent,
                    I.imitate.accent, AQ, SS, AS, I, C, AD, openness, extraversion, agreeableness, neuroticism,
                    conscientiousness, SocDes, voice, Dialect)
exp1.survey.unique<-unique(exp1.survey)
exp1.ind<-merge(exp1.survey.unique, coefs, by="participant")
exp1.ay.participants<-exp1.ay %>%
  group_by(participant, phase) %>%
  summarise(diag.sum=diag)

exp1.ay.part.wide<-dcast(exp1.ay.participants, participant~phase, value.var="diag.sum", fun.aggregate=mean)
exp1.ind<-merge(exp1.ind, exp1.ay.part.wide, by="participant")

### BIG FIVE
bigfive<-select(exp1.ind, openness, extraversion, agreeableness, neuroticism,
                conscientiousness, voice, Dialect, participant, phaseexposure)
bigfive.long<-melt(bigfive, id.vars=c("voice", "Dialect", "participant", "phaseexposure"))

ggplot(bigfive.long[bigfive.long$voice=="Southern",], 
       aes(value, phaseexposure, color=Dialect, shape=Dialect))+
  geom_point(alpha=.7)+geom_smooth(method="lm", se=F)+
  facet_wrap(~variable, scales="free")+
  theme_bw()+
  scale_color_manual(values=c("black", "gray50"))+
  ggtitle("Big Five Personality Traits")+xlab("Score (Scaled)")+ylab("Baseline to Exposure Shift")

### AQ
AQ.sub<-select(exp1.ind, SS, AS, AD, C, I, voice, Dialect, participant, phaseexposure)
AQ.sub.long<-melt(AQ.sub, id.vars=c("voice", "Dialect", "participant", "phaseexposure"))

ggplot(AQ.sub.long[AQ.sub.long$voice=="Southern",], 
       aes(value, phaseexposure, color=Dialect, shape=Dialect))+
  geom_point(alpha=.7)+geom_smooth(method="lm", se=F)+
  facet_wrap(~variable, scales="free")+
  theme_bw()+
  scale_color_manual(values=c("black", "gray50"))+
  ggtitle("Autism Quotient Subscores")+xlab("Score (Scaled)")+ylab("Baseline to Exposure Shift")

### Marlowe-Crowne
ggplot(exp1.ind[exp1.ind$voice=="Southern",], 
       aes(SocDes, phaseexposure, color=Dialect, shape=Dialect))+
  geom_point(alpha=.5)+geom_smooth(method="lm", se=F)+
  theme_bw()+
  scale_color_manual(values=c("black", "gray50"))+
  ggtitle("Marlowe-Crowne \n Social Desirability Scale")+xlab("Score")+ylab("Baseline to Exposure Shift")

#### Affective/Familiarity Measures
judgments<-select(exp1.ind, participant, Kind, Wealthy, Friendly, Intelligent, Professional, Attractive, 
                  Would.Be.Friends, From.South, From.Midwest, Happy, Educated, Relatable,
                  I.Lived.in.South, I.Familiar.South.Speech, I.Friends.South, I.Would.Live.South, 
                  I.Like.Southerners.Talk, I.have.Southern.accent, Parents.have.accent, Colleagues.have.accent,
                  I.imitate.accent, phaseexposure, voice, Dialect)

judgments<-unique(judgments)
judgments.long<-melt(judgments, id.vars=c("participant", "voice", "Dialect", "phaseexposure"))

judgments<-dcast(judgments.long, participant+voice+Dialect+phaseexposure~variable, value.var="value")

# Create a "Familiarity with the South" subscore
judgments$Familiarity<-
  judgments$I.Lived.in.South+
  judgments$I.Familiar.South.Speech+
  judgments$I.Friends.South+
  judgments$I.have.Southern.accent+
  judgments$Parents.have.accent+
  judgments$Colleagues.have.accent+
  judgments$I.imitate.accent
# Create a "Talker Likability" subscore
judgments$Likability<-
  judgments$Happy+
  judgments$Kind+
  judgments$Friendly+
  judgments$Would.Be.Friends
# Create a "Prestige" subscore
judgments$Prestige<-
  judgments$Intelligent+
  judgments$Wealthy+
  judgments$Professional+
  judgments$Attractive+
  judgments$Educated

### Prestige
ggplot(judgments[judgments$voice=="Southern",], 
       aes(Prestige, phaseexposure, color=Dialect, shape=Dialect))+
  geom_point(alpha=.75, position="jitter")+geom_smooth(method="lm")+
  theme_bw()+
  scale_color_manual(values=c("black", "gray55", "gray75"))+
  ggtitle("Talker Prestige")+xlab("Score")+ylab("Baseline to Exposure Shift")
### Likability
ggplot(judgments[judgments$voice=="Southern",], 
       aes(Likability, phaseexposure, color=Dialect, shape=Dialect))+
  geom_point(alpha=.75, position="jitter")+geom_smooth(method="lm")+
  theme_bw()+
  scale_color_manual(values=c("black", "gray55", "gray75"))+
  ggtitle("Talker Likability")+xlab("Score")+ylab("Baseline to Exposure Shift")
### Familiarity
ggplot(judgments[judgments$voice=="Southern",], 
       aes(Familiarity, phaseexposure, color=Dialect, shape=Dialect))+
  geom_point(alpha=.75, position="jitter")+
  geom_smooth(method="lm", aes(group=1), se=F, color="black", linetype=2)+
  geom_smooth(method="lm")+
  scale_color_manual(values=c("black", "gray55", "gray75"))+
  theme_bw()+
  ggtitle("Familiarity with the South")+xlab("Score")+ylab("Baseline to Exposure Shift")


#### Permutations
s<-exp1.ind[exp1.ind$voice=="Southern",]
m<-exp1.ind[exp1.ind$voice=="Midland",]

ss<-s[s$Dialect=="Southern",]
sn<-s[s$Dialect=="Non-Southern",]

ms<-s[m$Dialect=="Southern",]
mn<-s[m$Dialect=="Non-Southern",]



### Only significant individual differences predictors (Southern talker condition)

# conscientiousness for southerners
cor.test(ss$conscientiousness, ss$phaseexposure)  
con.ss.test<-ss[!is.na(ss$conscientiousness),]
perm.relation(con.ss.test$conscientiousness, con.ss.test$phaseexposure, R=10000) 
# neuroticism for non-southerners
cor.test(sn$neuroticism, sn$phaseexposure)   
neur.sn.test<-sn[!is.na(sn$neuroticism),]
perm.relation(neur.sn.test$neuroticism, neur.sn.test$phaseexposure, R=10000) 
# Attention to Detail for non-southerners 
cor.test(sn$AD, sn$phaseexposure)  
AD.sn.test<-sn[!is.na(sn$AD),]
perm.relation(AD.sn.test$AD, AD.sn.test$phaseexposure, R=10000) 
# Imagination for non-southerners
cor.test(sn$I, sn$phaseexposure)  
I.sn.test<-sn[!is.na(sn$I),]
perm.relation(I.sn.test$I, I.sn.test$phaseexposure, R=10000) 

## Also significant for Southerners in the Midland (control condition)
# Imagination
cor.test(ms$I, ms$phaseexposure)
I.ms.test<-ms[!is.na(ms$I),]
perm.relation(I.ms.test$I, I.ms.test$phaseexposure, R=10000) 
# Conscientiousness
cor.test(ms$conscientiousness, ms$phaseexposure)
con.ms.test<-ms[!is.na(ms$conscientiousness),]
perm.relation(con.ms.test$conscientiousness, con.ms.test$phaseexposure, R=10000) 