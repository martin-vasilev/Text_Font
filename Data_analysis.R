
# Martin Vasilev, 2019

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


# Please note: counting in the present data (e.g., fixation/ character/ line numbers) always starts at 1.


# load/ install required packages:
packages= c("reshape", "lme4", "ggplot2", "MASS", "arm", "effects", "lattice",
            "mgcv", "itsadug") # list of used packages:

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

########################
# Prepare data frames: #
########################

# Load data:
load("data/Alldata.Rda")
load("data/Return_sweep.Rda")
load("data/Quest.Rda")

#classify data
RS$sub = as.factor(RS$sub)
RS$item = as.factor(RS$item)
RS$cond= as.factor(RS$cond)
RS$land_pos = as.numeric(RS$land_pos)

Alldata$sub = as.factor(Alldata$sub)
Alldata$item = as.factor(Alldata$item)
Alldata$cond= as.factor(Alldata$cond)
Alldata$land_pos = as.numeric(Alldata$land_pos)

Quest$sub<- as.factor(Quest$sub)
Quest$item<- as.factor(Quest$item)
Quest$line_len<- as.factor(Quest$line_len)
Quest$font_size<- as.factor(Quest$font_size)

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

Quest$line_len= factor(ifelse(Quest$cond==1| Quest$cond==2, 1,2),labels = c("short line", "long line"))
Quest$font_size= factor(ifelse(Quest$cond==1| Quest$cond==3, 1,2), labels = c("small font", "big font"))


############################
#  Comprehension accuracy  #
############################

100*round(mean(Quest$accuracy),3) # mean accuracy (%)
100*round(sd(Quest$accuracy),3) # SD accuracy (%)

DesQuest<- melt(Quest, id=c('sub', 'item', 'cond', 'line_len', 'font_size'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, line_len+font_size ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
mQuest

# GLMM model:
contrasts(Quest$line_len)<- c(-1, 1)
contrasts(Quest$font_size)<- c(-1, 1)

if(!file.exists("Models/CGM.Rda")){
  # model does not converge with any other random slopes
  CGM<- glmer(accuracy ~ font_size*line_len + (1|sub)+ (line_len|item), data = Quest, family= binomial)
  save(CGM, file= "Models/CGM.Rda")
  summary(CGM)
}else{
  load("Models/CGM.Rda")
  summary(CGM)
}

round(coef(summary(CGM)), 2)


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
colnames(mUPPS)= c("SFSL", "SFLL", "BFSL", "BFLL")
boxplot(mUPPS)

#plots for land_pos per condition for letters
boxplot(RS$LandStartLet~RS$cond)

ggplot(RS, aes(x=as.factor(RS$cond), y=RS$LandStartLet)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) +
  xlab("Conditions") + ylim(0,15)
  
#plots for land_pos per condition for visual angle

boxplot(RS$LandStartVA~RS$cond)

ggplot(RS, aes(x=as.factor(RS$cond), y=RS$LandStartVA)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) +
  xlab("Conditions") + ylim(0,4)


#########################################
#                  (G)LMMS                 #
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

###GLMM for undersweep probability
GLM1=glmer(undersweep_prob ~ font_size* line_len
           +launchDistVA_C +(1|item)+(1|sub) , data= RS, family= binomial)
summary(GLM1)
plot(allEffects(GLM1))

###LMM for landing position
land_pos.lm= lmer(LandStartLet ~ line_len *font_size*launchDistLet_C + (1|item)+
                   (1+line_len+font_size|sub), RS, REML=T)
summary(land_pos.lm)
plot(allEffects(land_pos.lm))

#and

land_pos.lm2= lmer(LandStartVA ~ line_len *font_size*launchDistVA_C + 
                      (1|item) + (1+ font_size|sub), RS, REML=T)
summary(land_pos.lm2)
plot(allEffects(land_pos.lm2))
#saccade lenght comparison     
length.lm= lmer(launchDistVA ~ font_size*Rtn_sweep + 
                     (1|item) + (1+ font_size|sub), Alldata2, REML=T)
summary(length.lm)
plot(allEffects(length.lm))


##############################################################
#                 Modulation by trial order:                 #
##############################################################

#-------------------------------
# Prepare dataset for analsysis:
#-------------------------------

# check contrast coding:
contrasts(RS$font_size)
contrasts(RS$line_len)
RS$sub<- as.numeric(as.character(RS$sub))

# Add block order:

RS$block_order<- NA
RS$big_font_block<- NA
RS$small_font_block<- NA

for(i in 1:nrow(RS)){
  
  if(RS$sub[i]%%2==1){ # odd subject, small font first
   
    RS$big_font_block[i]<- 2
    RS$small_font_block[i]<- 1
    
    if(RS$font_size[i]== "small font"){ # small font, block seq= seq
      RS$block_order[i]<- RS$seq[i]
    }else{ # big font, block seq = seq-50 (50 is halfway point; 100 items)
      RS$block_order[i]<- RS$seq[i]- 50
    }
     
  }else{ # even subject, big font first
    
    RS$big_font_block[i]<- 1
    RS$small_font_block[i]<- 2
    
    if(RS$font_size[i]== "small font"){ # big font, block seq = seq-50 (50 is halfway point; 100 items)
      RS$block_order[i]<- RS$seq[i]- 50
    }else{ # small font, block seq= seq
      RS$block_order[i]<- RS$seq[i] 
    }
    
  }
  
}


# take a smaller, more managable dataset:
tDat<- RS[,c("sub", "item", "seq", "cond", "font_size", "line_len", "LandStartVA", "undersweep_prob" ,  "block_order",
             "big_font_block", "small_font_block")]

#tDat$big_font_block<- as.factor(tDat$big_font_block)
#tDat$big_font_block<- factor(tDat$big_font_block, levels= c("small font", "big font"))
#contrasts(tDat$big_font_block)
is.numeric(tDat$block_order)


# gamm model:
tDat$big_font_block<- as.factor(tDat$big_font_block)
contrasts(tDat$big_font_block)

tDat$small_font_block<- as.factor(tDat$small_font_block)
contrasts(tDat$small_font_block)

### big font

bigF<- subset(tDat, font_size== "big font")

gam1 <- bam(LandStartVA ~ big_font_block+
              s(sub, bs="re", k=10) +
              s(sub, big_font_block, bs="re", k=10) +
              s(item, bs= "re", k=10)+
              s(item, big_font_block, bs="re") +
              s(block_order, bs= "cr", k=10)+
              s(block_order, by= big_font_block, k=10, bs= "cr")+
#              s(big_font_block, by= font_size, k=10, bs= "cr")+
              s(block_order, sub, bs= "fs", m=1, k=4),
            data= bigF)

summary(gam1)

#plot(gam1)


plot_smooth(gam1, view="block_order", plot_all="big_font_block", rug=F, xlab= "Trial order within block",
            ylab= "Landing position (deg)", main= "Big font sentences as a function of block order",
            col = c(pallete1[1], pallete1[2]), legend_plot_all = list(x=0, y=0), family= "serif",
            cex.axis= 1.2, cex.lab= 1.4)
legend(x = 2, y= 2.5, legend = c("first block", "second block"), col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
       box.col = "white")

plot_diff(gam1, view = "block_order", rm.ranef = F, comp = list(big_font_block = c(1,  2)), 
          col = pallete1[2], main= "Big font (2nd- 1st block difference)",
          ylab= "Mean diff.in landing position (deg)", xlab= "Trial order within block", print.summary = T, 
          family= "serif", cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)


###########

### small font

smallF<- subset(tDat, font_size== "small font")

gam2 <- bam(LandStartVA ~ small_font_block+
              s(sub, bs="re", k=10) +
              s(sub, small_font_block, bs="re", k=10) +
              s(item, bs= "re", k=10)+
              s(item, small_font_block, bs="re") +
              s(block_order, bs= "cr", k=10)+
              s(block_order, by= small_font_block, k=10, bs= "cr")+
              #              s(small_font_block, by= font_size, k=10, bs= "cr")+
              s(block_order, sub, bs= "fs", m=1, k=4),
            data= smallF)

summary(gam2)

#plot(gam2)


plot_smooth(gam2, view="block_order", plot_all="small_font_block", rug=F, xlab= "Trial order within block",
            ylab= "Landing position (deg)", main= "Small font sentences as a function of block order",
            col = c(pallete1[2], pallete1[1]), family= "serif",
            cex.axis= 1.2, cex.lab= 1.4, legend_plot_all = list(x=0, y=0))
legend(x = 35, y= 2.4, legend = c("first block", "second block"), col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
       box.col = "white")

plot_diff(gam2, view = "block_order", rm.ranef = F, comp = list(small_font_block = c(1,  2)), 
          col = pallete1[2], main= "Small font (2nd- 1st block difference)",
          ylab= "Mean diff.in landing position (deg)", xlab= "Trial order within block", print.summary = T, 
          family= "serif", cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)



##############
# same analysis, but for undersweep probability

gam3 <- bam(undersweep_prob ~ big_font_block+
              s(sub, bs="re", k=10) +
              s(sub, big_font_block, bs="re", k=10) +
              s(item, bs= "re", k=10)+
              s(item, big_font_block, bs="re") +
              s(block_order, bs= "cr", k=10)+
              s(block_order, by= big_font_block, k=10, bs= "cr")+
              s(block_order, sub, bs= "fs", m=1, k=4),
            data= bigF, family = binomial)

summary(gam3)

plot_smooth(gam3, view="block_order", plot_all="big_font_block", rug=F, xlab= "Trial order within block",
            ylab= "Undersweep probability", main= "Big font sentences as a function of block order",
            col = c(pallete1[1], pallete1[2]), family= "serif",
            cex.axis= 1.2, cex.lab= 1.4, lwd= 2, legend_plot_all = list(x=2, y=2))
legend(x = 2, y= 1.25, legend = c("first block", "second block"), col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
       box.col = "white")

plot_diff(gam3, view = "block_order", rm.ranef = F, comp = list(big_font_block = c(1,  2)), 
          col = pallete1[2], main= "Big font (2nd- 1st block difference)",
          ylab= "Mean diff. in undersweep probability", xlab= "Trial order within block", print.summary = T, 
          family= "serif", cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)



### small font

gam4 <- bam(undersweep_prob ~ small_font_block+
              s(sub, bs="re", k=10) +
              s(sub, small_font_block, bs="re", k=10) +
              s(item, bs= "re", k=10)+
              s(item, small_font_block, bs="re") +
              s(block_order, bs= "cr", k=10)+
              s(block_order, by= small_font_block, k=10, bs= "cr")+
              #              s(small_font_block, by= font_size, k=10, bs= "cr")+
              s(block_order, sub, bs= "fs", m=1, k=4),
            data= smallF, family = binomial)

summary(gam4)

#plot(gam2)


plot_smooth(gam4, view="block_order", plot_all="small_font_block", rug=F, xlab= "Trial order within block",
            ylab= "Undersweep probability", main= "Small font sentences as a function of block order",
            col = c(pallete1[2], pallete1[1]), family= "serif",
            cex.axis= 1.2, cex.lab= 1.4, lwd= 2, legend_plot_all = list(x=-2, y=-2))
legend(x = 35, y= 1.4, legend = c("first block", "second block"), col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
       box.col = "white")

plot_diff(gam4, view = "block_order", rm.ranef = F, comp = list(small_font_block = c(1,  2)), 
          col = pallete1[2], main= "Small font (2nd- 1st block difference)",
          ylab= "Mean diff. in undersweep probability", xlab= "Trial order within block", print.summary = T, 
          family= "serif", cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)


