
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


# Load data:
load("data/Alldata.Rda")
load("data/Return_sweep.Rda")

#classify data
RS$sub = as.factor(RS$sub)
RS$item = as.factor(RS$item)
RS$cond= as.factor(RS$cond)
RS$land_pos = as.numeric(RS$land_pos)

#examine data 
str(RS)
head(RS)

#merge conditions for main effect analysis 

RS$line_len= factor(ifelse(RS$cond==1| RS$cond==2, 1,2),labels = c("short line", "long line"))
RS$font_size= factor(ifelse(RS$cond==1| RS$cond==3, 1,2), labels = c("small font", "big font"))


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
#plots for undersweep probability per condtion 
tapply(RS$undersweep_prob, list(RS$sub,RS$cond), FUN=mean)
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


#########GLMM for undersweep probability
#setting contrast
contrasts(RS$line_len) <- contr.sdif(2)  
contrasts(RS$font_size) <- contr.sdif(2)

GLM1=glmer(undersweep_prob ~ font_size + line_len
           +launchDistVA +(1|item)+(1|sub) , data= RS, family= binomial)
summary(GLM1)
plot(allEffects(GLM1))

####LMM for landing position
land_pos.lm= lmer(LandStartLet ~ line_len *font_size*launchDistLet + 
                   (1+ line_len+ font_size|sub), RS, REML=T)
summary(land_pos.lm)
plot(allEffects(land_pos.lm))

#and

land_pos.lm2= lmer(LandStartVA ~ line_len *font_size*launchDistVA + 
                      (1|item) + (1+ font_size|sub), RS, REML=T)
summary(land_pos.lm2)
plot(allEffects(land_pos.lm2))
     

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

for(i in 1:nrow(RS)){
  
  if(RS$sub[i]%%2==1){ # odd subject, small font first
   
    RS$big_font_block[i]<- 2
    
    if(RS$font_size[i]== "small font"){ # small font, block seq= seq
      RS$block_order[i]<- RS$seq[i]
    }else{ # big font, block seq = seq-50 (50 is halfway point; 100 items)
      RS$block_order[i]<- RS$seq[i]- 50
    }
     
  }else{ # even subject, big font first
    
    RS$big_font_block[i]<- 1
    
    if(RS$font_size[i]== "small font"){ # big font, block seq = seq-50 (50 is halfway point; 100 items)
      RS$block_order[i]<- RS$seq[i]- 50
    }else{ # small font, block seq= seq
      RS$block_order[i]<- RS$seq[i] 
    }
    
  }
  
}


# take a smaller, more managable dataset:
tDat<- RS[,c("sub", "item", "seq", "cond", "font_size", "line_len", "LandStartVA", "block_order", "big_font_block")]

#tDat$big_font_block<- as.factor(tDat$big_font_block)
#tDat$big_font_block<- factor(tDat$big_font_block, levels= c("small font", "big font"))
#contrasts(tDat$big_font_block)
is.numeric(tDat$block_order)


# gamm model:

gam1 <- bam(LandStartVA ~ font_size+
              s(sub, bs="re", k=10) +
              s(sub, font_size, bs="re", k=10) +
              s(item, bs= "re", k=10)+
              s(item, font_size, bs="re") +
              s(block_order, bs= "cr", k=10)+
              s(block_order, by= big_font_block, k=10, bs= "cr")+
#              s(big_font_block, by= font_size, k=10, bs= "cr")+
              s(block_order, sub, bs= "fs", m=1, k=4),
            data=tDat)

summary(gam1)

plot_smooth(gam1, view="block_order", plot_all="font_size", rug=F)
plot_smooth(gam1, view="big_font_block", plot_all="font_size", rug=F)

plot_diff(gam1, view = "block_order", rm.ranef = F, comp = list(font_size = c("small font",  "big font")), 
          col = pallete1[2],
          ylab= "Mean difference in landing position (deg)", xlab= "Trial order in block", print.summary = T, 
          family= "serif", cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)
