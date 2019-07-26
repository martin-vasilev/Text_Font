
# ALTERNATIVE CODING IN WHICH WE KEEP RETURN-SWEEP FIXATIONS LESS THAN 80 MS!!!


# Martin R. Vasilev, 2019

rm(list= ls())


#######################################################
#             Data column explanation:                #
#######################################################

# sub:              Subject number
# item:             Item (sentence) number from the corpus file
# cond:             Condition number (1= small font short line; 2= big font short line; 3= small font long line; 
#                                     4= big font long line)
# seq:              Trial sequence, or the order in which trials were presented in the experiment
# trialStart:       Start of trial time stamp in raw asc file
# SFIX:             Start of fixation flag in the asc data (search for it in raw data to quickly locate it)
# EFIX:             End of fixation flag in the asc data (search for it in raw data to quickly locate it)
# xPos:             X position of the fixation on the screen
# yPos:             Y position of the fixation on the screen
# fix_num:          Fixation number in the current trial
# fix_dur:          Fixation duration (in ms)
# sacc_dur:         Incoming saccade duration (in ms)
# line:             Line of text on which the fixation occured
# word:             Word number in the trial on which the fixation occured
# char_trial:       Character number in the trial on which the fixation occured (includes all text in the trial)
# char_line:        Character number on the current line on which the fixation occured (each line starts at 0)
# prevChar:         Character number (line) on which the previous fixation occured
# nextChar:         Character number (line) on which the next fixation occured
# prevX:            X position on which the previous fixation occured
# nextX:            X position on which the next fixation occured
# regress:          A logical indicating whether the current fixation is a regressive one (1= yes; 0= no)
# wordID:           A string of the word on which the fixation occured
# land_pos:         Landing position in characters on the current word (0= empty space before word)
# sacc_len:         Incoming saccade length (in characters)
# outsideText:      A logical indicating whether the current fixation occured outside the text area (1= yes; 0=no)
# max_char_line:    Maximum number of characters on the current line
# Rtn_sweep:        A logical indicating whether the current fixation is a return sweep one (1= yes; 0= no)
# Rtn_sweep_type:   Type of return sweep saccade ("undersweep" or "accurate")
# LandStartLet:     Landing position relative to the start of the line in letters (equivalent to char_line)
# LandStartVA:      Landing position relative to the start of the line in degrees per visual angle
# undersweep_prob:  A logical indicating whether the current fixation is an undersweep one (1= yes; 0=no)
# launchDistLet:    Launch site distance in letters (equivalent to sacc_len)
# launchDistVA:     Launch site distance in degrees per visual angle
# launchSite:       Launch site distance relative to end of the line (letters)
# launchSiteVA:     Launch site distance relative to end of the line (visual angle)

# Please note: counting in the present data (e.g., fixation/ character/ line numbers) always starts at 1.


# load/ install required packages:
packages= c("reshape", "lme4", "ggplot2", "MASS", "arm", "effects", "lattice",
            "mgcv", "itsadug", 'ggpubr') # list of used packages:

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}


# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

source("Functions/CohensD_raw.R")

########################
# Prepare data frames: #
########################

# Load data:
load("preproc/alt_coding/Alldata.Rda")
load("preproc/alt_coding/Return_sweep.Rda")


#classify data
RS$sub = as.factor(RS$sub)
RS$item = as.factor(RS$item)
RS$cond= as.factor(RS$cond)
RS$land_pos = as.numeric(RS$land_pos)

Alldata$sub = as.factor(Alldata$sub)
Alldata$item = as.factor(Alldata$item)
Alldata$cond= as.factor(Alldata$cond)
Alldata$land_pos = as.numeric(Alldata$land_pos)


#examine data 
str(RS)
head(RS)

str(Alldata)
head(Alldata)
#merge conditions for main effect analysis 

RS$line_len= factor(ifelse(RS$cond==1| RS$cond==2, 1,2),labels = c("short line", "long line"))
RS$font_size= factor(ifelse(RS$cond==1| RS$cond==3, 1,2), labels = c("small font", "big font"))

Alldata$line_len= factor(ifelse(Alldata$cond==1| Alldata$cond==2, 1,2),labels = c("short line", "long line"))
Alldata$font_size= factor(ifelse(Alldata$cond==1| Alldata$cond==3, 1,2), labels = c("small font", "big font"))


############################
#       Descriptives       #
############################

#mean under_sweep per condition 
USP<- melt(RS, id=c('sub', 'item', 'font_size', 'line_len'), 
                measure=c("undersweep_prob"), na.rm=TRUE)
mUSP<- cast(USP, font_size + line_len ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

#mean land_pos in letters per condition 
LP<- melt(RS, id=c('sub', 'item', 'font_size', 'line_len'), 
           measure=c("LandStartLet"), na.rm=TRUE)
mLP<- cast(LP, font_size + line_len ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

#mean land_pos in visual angle per condition 
LP1<- melt(RS, id=c('sub', 'item', 'font_size', 'line_len'), 
          measure=c("LandStartVA"), na.rm=TRUE)
mLP1<- cast(LP1, font_size + line_len ~ variable
           ,function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

#mean incoming saccade length in visual angle per fixation type 
Alldata2 <- Alldata[Alldata$regress < 1,]
SLVA1<- melt(Alldata2, id=c('sub', 'item', 'font_size', 'Rtn_sweep'), 
           measure=c("launchDistVA"), na.rm=TRUE)
mSLVA1<- cast(SLVA1, font_size + Rtn_sweep ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

#mean incoming saccade length in visual angle per saccade type 
Alldata3 <- Alldata[Alldata$Rtn_sweep < 1,]
SLVA2<- melt(Alldata3, id=c('sub', 'item', 'font_size', 'regress'), 
             measure=c("launchDistVA"), na.rm=TRUE)
mSLVA2<- cast(SLVA2, font_size + regress ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

#mean incoming saccade length in letters per saccade type 
SLlet<- melt(Alldata3, id=c('sub', 'item', 'font_size', 'regress'), 
             measure=c("launchDistLet"), na.rm=TRUE)
mSLlet<- cast(SLlet, font_size + regress ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

#plots for undersweep probability per condtion 
mUPPS =tapply(RS$undersweep_prob, list(RS$sub,RS$cond), FUN=mean)
colnames(mUPPS)= c("small-font/short-line", "big-font/short-line", "small-font/long-line", "big-font/long-line")
boxplot(mUPPS)


#plots for land_pos per condition for visual angle

RS$cond= factor(RS$cond, labels= c("small-font/short-line", "big-font/short-line", 
                                   "small-font/long-line", "big-font/long-line"))

ggplot(RS, aes(x=cond, y=LandStartVA, fill=cond)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(legend.position="none") + 
  scale_fill_brewer(pallete1) + ylim(0,10) + labs(y= "Return-sweep land position", x= "Conditions")

#####



#########################################
#              (G)LMMS                  #
#########################################

#setting contrast
contrasts(RS$line_len) <- c(-1, 1)  
contrasts(RS$font_size) <- c(-1, 1)  
Alldata2$Rtn_sweep = as.factor(Alldata2$Rtn_sweep)
contrasts(Alldata2$Rtn_sweep) <- c(-1, 1)  
contrasts(Alldata2$font_size) <- c(-1, 1)  

#centering launch distance
RS$launchDistVA_C<- scale(RS$launchDistVA)
RS$launchDistLet_C<- scale(RS$launchDistLet)
Alldata2$launchDistVA_C<- scale(Alldata2$launchDistVA)
Alldata2$launchDistLet_C<- scale(Alldata2$launchDistLet)

# centre launch site:
RS$launchSiteVA_C<- scale(RS$launchSiteVA)



#------------------------------#
#   Undersweep probability     #
#------------------------------#

# NO DIFFERENCE in results (note: line_len slope for subs is removed )
if(!file.exists("preproc/alt_coding/Models/GLM1.Rda")){
  GLM1<- glmer(undersweep_prob ~ font_size* line_len*launchSiteVA_C +(1|sub)+(line_len|item),
               data= RS, family= binomial)
  save(GLM1, file= "preproc/alt_coding/Models/GLM1.Rda")
}else{
  load("preproc/alt_coding/Models/GLM1.Rda")
}

summary(GLM1)

round(coef(summary(GLM1)),3)
write.csv(round(coef(summary(GLM1)),3), 'preproc/alt_coding/Models/Undersweep_prob_GLM1.csv')

# Effect size:
CohensD_raw(data = RS, measure = 'undersweep_prob', group_var = 'line_len', baseline = 'short line')
CohensD_raw(data = RS, measure = 'undersweep_prob', group_var = 'font_size', baseline = 'small font')


#------------------------------#
#      Landing Position        #
#------------------------------#

# font_size: launch site interaction is no longer significant, but this is not of main theoretical importance
if(!file.exists('preproc/alt_coding/Models/LM1.Rda')){
  LM1<- lmer(LandStartVA ~  font_size*line_len*launchSiteVA_C + (line_len|sub) + (line_len|item),
             data=RS, REML=T)
  save(LM1, file= "preproc/alt_coding/Models/LM1.Rda")
}else{
  load('preproc/alt_coding/Models/LM1.Rda')
}

summary(LM1)

LPM= round(coef(summary(LM1)), 2)
write.csv(LPM, file= "preproc/alt_coding/Models/LPM.csv") 

CohensD_raw(data = RS, measure = 'LandStartVA', group_var = 'line_len', baseline = 'short line')
CohensD_raw(data = RS, measure = 'LandStartVA', group_var = 'font_size', baseline = 'small font')


plot(effect('font_size:launchSiteVA_C', LM1))
plot(effect('line_len:font_size:launchSiteVA_C', LM1))



#------------------------------#
#        Saccade length        #
#------------------------------#

if(!file.exists("preproc/alt_coding/Models/LM2.Rda")){
  LM2<- lmer(launchDistVA ~ font_size*line_len*Rtn_sweep + (1|item) + (1|sub), Alldata2, REML=T)
  save(LM2, file= "preproc/alt_coding/Models/LM2.Rda")
}else{
  load("preproc/alt_coding/Models/LM2.Rda")
}

summary(LM2)

round(coef(summary(LM2)),3)

write.csv(round(coef(summary(LM2)),3), 'preproc/alt_coding/Models/SaccLen_LM2.csv')

contrasts(Alldata2$font_size)
contrasts(Alldata2$Rtn_sweep)

plot(effect('font_size', LM2))
plot(effect('font_size:Rtn_sweep', LM2))

