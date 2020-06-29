
#  2019.  Martin R. Vasilev, Victoria Adedeji, Calvin Laursen, Tim Slattery 

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
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F", "#FFC107") # "Classic & trustworthy"

source("Functions/CohensD_raw.R")

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
Quest$line_len<- as.factor(Quest$line_len)
Quest$font_size<- as.factor(Quest$font_size)

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
Alldata2 <- Alldata[- which(Alldata$regress== 1 & Alldata$Rtn_sweep==0), ]
SLVA1<- melt(Alldata2, id=c('sub', 'item', 'line_len', 'font_size', 'Rtn_sweep'), 
           measure=c("launchDistVA"), na.rm=TRUE)
mSLVA1<- cast(SLVA1, line_len + font_size + Rtn_sweep ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

# get just intra-line saccades (forward vs regressive)
Alldata4<- Alldata[-which(Alldata$regress==0 & Alldata$Rtn_sweep==0), ]
RegVA<- melt(Alldata4, id=c('sub', 'item', 'line_len', 'font_size', 'Rtn_sweep'), 
             measure=c("launchDistVA"), na.rm=TRUE)
mReg<- cast(RegVA, line_len + font_size + Rtn_sweep ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))


#mean incoming saccade length in visual angle per saccade type 
Alldata3 <- Alldata[Alldata$Rtn_sweep < 1,]
SLVA2<- melt(Alldata3, id=c('sub', 'item', 'font_size', 'regress'), 
             measure=c("launchDistVA"), na.rm=TRUE)
mSLVA2<- cast(SLVA2, font_size + regress ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

# #mean incoming saccade length in letters per saccade type 
# SLlet<- melt(Alldata3, id=c('sub', 'item', 'font_size', 'regress'), 
#              measure=c("launchDistLet"), na.rm=TRUE)
# mSLlet<- cast(SLlet, font_size + regress ~ variable
#               ,function(x) c(M=signif(mean(x),3)
#                              , SD= sd(x) ))


#########################################
#              (G)LMMS                  #
#########################################

#setting contrast
contrasts(RS$line_len) <- c(-1, 1)  
contrasts(RS$font_size) <- c(-1, 1)  
Alldata2$Rtn_sweep = as.factor(Alldata2$Rtn_sweep) # contains only forward intra-line saccades (+return-sweeps)
contrasts(Alldata2$Rtn_sweep) <- c(-1, 1)  
contrasts(Alldata2$font_size) <- c(-1, 1)  

Alldata4$Rtn_sweep = as.factor(Alldata4$Rtn_sweep) # contains only regressive intra-line saccades (+return-sweeps)
contrasts(Alldata4$Rtn_sweep) <- c(-1, 1)  
contrasts(Alldata4$font_size) <- c(-1, 1)  


#centering launch distance
RS$launchDistVA_C<- scale(RS$launchDistVA)
Alldata2$launchDistVA_C<- scale(Alldata2$launchDistVA)
Alldata4$launchDistVA_C<- scale(Alldata4$launchDistVA)

# centre launch site:
RS$launchSiteVA_C<- scale(RS$launchSiteVA)



#------------------------------#
#   Undersweep probability     #
#------------------------------#

if(!file.exists("Models/GLM1.Rda")){
  GLM1<- glmer(undersweep_prob ~ font_size* line_len*launchSiteVA_C +(line_len|sub)+(line_len|item),
               data= RS, family= binomial)
  save(GLM1, file= "Models/GLM1.Rda")
}else{
  load("Models/GLM1.Rda")
}

summary(GLM1)

round(coef(summary(GLM1)),3)
write.csv(round(coef(summary(GLM1)),3), 'Models/Undersweep_prob_GLM1.csv')

# Effect size:
CohensD_raw(data = RS, measure = 'undersweep_prob', group_var = 'line_len', baseline = 'short line')
CohensD_raw(data = RS, measure = 'undersweep_prob', group_var = 'font_size', baseline = 'small font')


#------------------------------#
#      Landing Position        #
#------------------------------#

if(!file.exists('Models/LM1.Rda')){
  LM1<- lmer(LandStartVA ~  font_size*line_len*launchSiteVA_C + (line_len|sub) + (line_len|item),
             data=RS, REML=T)
  save(LM1, file= "Models/LM1.Rda")
}else{
  load('Models/LM1.Rda')
}

summary(LM1)

LPM= round(coef(summary(LM1)), 2)
write.csv(LPM, file= "Models/LPM.csv") 

CohensD_raw(data = RS, measure = 'LandStartVA', group_var = 'line_len', baseline = 'short line')
CohensD_raw(data = RS, measure = 'LandStartVA', group_var = 'font_size', baseline = 'small font')


plot(effect('font_size:launchSiteVA_C', LM1))
plot(effect('line_len:font_size:launchSiteVA_C', LM1))



#------------------------------#
#        Saccade length        #
#------------------------------#

# forward saccades:

if(!file.exists("Models/LM2.Rda")){
  LM2<- lmer(launchDistVA ~ font_size*line_len*Rtn_sweep + (font_size|sub) + (1|item), Alldata2, REML=T)
  save(LM2, file= "Models/LM2.Rda")
}else{
  load("Models/LM2.Rda")
}

summary(LM2)

round(coef(summary(LM2)),3)

write.csv(round(coef(summary(LM2)),3), 'Models/SaccLen_LM2.csv')

contrasts(Alldata2$font_size)
contrasts(Alldata2$Rtn_sweep)

plot(effect('font_size', LM2))
plot(effect('font_size:Rtn_sweep', LM2))
effect('font_size:Rtn_sweep', LM2)



# Regressive saccades:
if(!file.exists("Models/LM3.Rda")){
  LM3<- lmer(launchDistVA ~ font_size*line_len*Rtn_sweep + (font_size|sub) + (1|item), Alldata4, REML=T)
  save(LM3, file= "Models/LM3.Rda")
}else{
  load("Models/LM3.Rda")
}

summary(LM3)

round(coef(summary(LM3)),3)

write.csv(round(coef(summary(LM3)),3), 'Models/SaccLen_LM3.csv')

contrasts(Alldata4$font_size)
contrasts(Alldata4$Rtn_sweep)

plot(effect('font_size:Rtn_sweep', LM3))
effect('font_size:Rtn_sweep', LM3)




# ### font size in letters
# dat3<- subset(Alldata2, Rtn_sweep==0)
# 
# summary(LM4<- lmer(sacc_len ~ font_size+ (1|sub) + (1|item), dat3, REML=T))


#########################################
#             RESULTS PLOTS             #
#########################################


#------------------------------#
#      Descriptives plot       #
#------------------------------#

# UNDERSWEEP PROBABILITY:
dfU<- subset(RS, !is.na(RS$launchDistVA_C))
dfU$fitted<- fitted(GLM1)

# average over subjects:
DesUSP_M<- melt(dfU, id=c('sub', 'item', 'font_size', 'line_len'), 
                measure=c("fitted", 'undersweep_prob'), na.rm=TRUE)
dfUSP<- cast(DesUSP_M, font_size + line_len+ sub ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

dfUSP$font_size= paste(' ', dfUSP$font_size, '    ', sep = '')
dfUSP$font_size<- as.factor(dfUSP$font_size)
dfUSP$font_size<- factor(dfUSP$font_size, levels = c(" small font    ", " big font    "))



USP_plot <- ggplot(dfUSP, aes(x=line_len, y=fitted_M, fill= font_size, shape= font_size, linetype= font_size)) + 
  theme_classic (22)+ geom_violin(alpha= 0.3, size=1)+ ylim(0, 1)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6, position=position_dodge(0.9), alpha= 0.8)+
  geom_boxplot(width=0.1, position=position_dodge(0.9), size=1 , notch = T, notchwidth = 0.4, varwidth =F,
               fill= 'white', color= 'black', show.legend=FALSE, linetype='solid')+
  labs(x= "Line length", y= "Under-sweep probability", color= "", shape= '', linetype= '', fill= '')+
  theme(legend.position = 'none', legend.title=element_blank(),
        legend.key.width = unit(1.25, 'cm'), legend.key.height = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c(pallete1[1], pallete1[4]))+
  scale_color_manual(values=c(pallete1[1], pallete1[4]))+ ggtitle('b)')

USP_plot

ggsave(filename = 'Plots/USP.pdf', plot = USP_plot, width = 7, height = 7)



# LANDING POSITION:
dfL<- subset(RS, !is.na(RS$launchDistVA_C))
dfL$fitted<- fitted(LM1)

# average over subjects:
DesLP_M<- melt(dfL, id=c('sub', 'item', 'font_size', 'line_len'), 
               measure=c("fitted", 'LandStartVA'), na.rm=TRUE)
dfLP<- cast(DesLP_M, font_size + line_len+ sub ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

dfLP$font_size= paste(' ', dfLP$font_size, '    ', sep = '')
dfLP$font_size<- as.factor(dfLP$font_size)
dfLP$font_size<- factor(dfLP$font_size, levels = c(" small font    ", " big font    "))



LP_plot <- ggplot(dfLP, aes(x=line_len, y=fitted_M, fill= font_size, shape= font_size, linetype= font_size)) + 
  theme_classic (22)+ geom_violin(alpha= 0.3, size=1)+ #ylim(0, 8)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6, position=position_dodge(0.9), alpha= 0.8)+
  geom_boxplot(width=0.1, position=position_dodge(0.9), size=1 , notch = T, notchwidth = 0.4, varwidth =F,
               fill= 'white', color= 'black', show.legend=FALSE, linetype= 'solid')+
  labs(x= "Line length", y= "Landing position (deg)", color= "", shape= '', linetype= '', fill= '')+
  theme(legend.position =  c(0.2, 0.88), legend.title=element_blank(),
        legend.key.width = unit(1.25, 'cm'), legend.key.height = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c(pallete1[1], pallete1[4]))+
  scale_color_manual(values=c(pallete1[1], pallete1[4]))+ ggtitle('a)')

LP_plot

ggsave(filename = 'Plots/LP.pdf', plot = LP_plot, width = 7, height = 7)

#### merge two descriptives plots:
figure <- ggarrange(LP_plot, USP_plot, ncol = 2, nrow = 1, common.legend = TRUE, legend = "top")
ggsave(filename = 'Plots/Des_merged.pdf', plot = figure, width = 15, height = 7)




#---------------------------------#
#   3-way interaction plot (LP)   #
#---------------------------------#

###ALL EFFECTS PLOT -THREE WAY INTERACTION
lp= allEffects(LM1)
summary(lp)
x= as.data.frame(lp)
x=as.data.frame(x)
colnames(x)= c("font_size", "line_len", "launchSiteVA_C", "fit", "se", "lower", "upper")


df<-  x
df$line_len <- droplevels(df$line_len)
#levels(df$line_len)<- c("long", 'short')
df$line_len<- factor(df$line_len, levels= c("short line", "long line"))
levels(df$line_len)<- c(" short line", " long line")

G1<- ggplot(df, aes(x= launchSiteVA_C, y=fit, ymax= upper, ymin= lower,
                    color=line_len, linetype= line_len, fill= line_len, shape= line_len)) + theme_bw (22)+
  geom_line(size= 1)+ geom_point(size=4)+
  labs(x= "Return-sweep launch position from end of first line (centred in deg)", y= "Return-sweep landing position (deg)", 
       color= "", shape= '', linetype= '', fill= '') +
  facet_wrap(~font_size)+
  geom_ribbon(alpha= 0.2, color= NA) + theme(legend.position = c(0.87, 0.88), legend.title=element_blank(),
  legend.key.width = unit(1.5, 'cm'), legend.key.height = unit(0.75, 'cm'), 
  panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
  strip.background = element_rect(colour="white", fill="white"),
  strip.text = element_text(size=22, face="bold"), text=element_text(family="serif"))+
  scale_fill_manual(values=c(pallete1[1], pallete1[2]))+
  scale_color_manual(values=c(pallete1[1], pallete1[2])); G1

ggsave(filename = 'Plots/3-way.pdf', plot = G1, width = 10, height = 7)



###############################################################################
#                 Modulation by trial order:  LANDING POSITIONS               #
###############################################################################

#-------------------------------
# Prepare dataset for analsysis:
#-------------------------------

# fix seq issues for a few subjects where the gaze-box was not triggered properly:
a<- which(RS$sub==6)
RS$seq[a]<- 1:length(a)

a<- which(RS$sub==12)
RS$seq[a[1:25]]<- c(1:25)
RS$seq[a[26:38]]<- 26:38 
RS$seq[a[39:99]]<- 40:100

a<- which(RS$sub==16) # seq 72 rpt
RS$seq[a[70:98]]<- 72:100

a<- which(RS$sub==22) # seq 11, 67 rpt
RS$seq[a[6:60]]<- RS$seq[a[6:60]]-1 

a<- which(RS$sub==46) #seq 53
RS$seq[a[53:100]]<- RS$seq[a[53:100]]-1

a<- which(RS$sub==51)
RS$seq[a[5:95]]<- RS$seq[a[5:95]]-1


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
tDat<- RS[,c("sub", "item", "seq", "cond", "font_size", "line_len", "LandStartVA", "launchDistVA",  "undersweep_prob" ,  "block_order",
             "big_font_block", "small_font_block")]

#tDat$big_font_block<- as.factor(tDat$big_font_block)
#tDat$big_font_block<- factor(tDat$big_font_block, levels= c("small font", "big font"))
#contrasts(tDat$big_font_block)
is.numeric(tDat$block_order)


# gamm model:
tDat$big_font_block<- as.factor(tDat$big_font_block)
contrasts(tDat$big_font_block)<- c(-1, 1)

tDat$small_font_block<- as.factor(tDat$small_font_block)
contrasts(tDat$small_font_block)<- c(-1, 1)

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



####################
# GAMM panel plot: #
####################

pdf('Plots/GAMMs.pdf', width = 11, height = 11)
par(mfrow=c(2,2), mar= c(5,5,4,3))


## big font condition

plot_smooth(gam1, view="block_order", plot_all="big_font_block", rug=F, xlab= "Trial number within block",
            ylab= "Landing position (deg)", main= "a) Large font sentences",
            col = c(pallete1[1], pallete1[2]), legend_plot_all = list(x=0, y=0), family= "serif",
            cex.axis= 1.6, cex.lab= 1.7, hide.label = T, lwd= 2, lty= c(2,1), ylim= c(1.15, 2.35),
            cex.main=1.7)


# small font condition
plot_smooth(gam2, view="block_order", plot_all="small_font_block", rug=F, xlab= "Trial number within block",
            ylab= "Landing position (deg)", main= "b) Small font sentences",
            col = c(pallete1[2], pallete1[1]), family= "serif",
            cex.axis= 1.6, cex.lab= 1.7, legend_plot_all = list(x=0, y=0),
            hide.label = T, lwd= 2, lty= c(1,2), ylim= c(1.15, 2.35), cex.main=1.7)

legend(x = 25, y= 2.37, legend = c("first block", "second block"), col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
       box.col = "white", lty= c(1,2), seg.len=2, cex = 1.5)


### Add block order effect:
plot_diff(gam1, view = "block_order", rm.ranef = F, comp = list(big_font_block = c(1,  2)), 
          col = pallete1[3], main= "c) Large font sentences (1st- 2nd block difference)",
          ylab= "Mean diff. in landing position (deg)", xlab= "Trial number within block", print.summary = T, 
          family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, hide.label = T, ylim= c(-0.6, 0.6))


plot_diff(gam2, view = "block_order", rm.ranef = F, comp = list(small_font_block = c(1,  2)), 
          col = pallete1[3], main= "d) Small font sentences (1st- 2nd block difference)",
          ylab= "Mean diff. in landing position (deg)", xlab= "Trial number within block", print.summary = T, 
          family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, hide.label = T, ylim= c(-0.6, 0.6))


dev.off()


##############################
# GAMM panel plot: VERSION 2 #
##############################

tDat$block<- ifelse(tDat$small_font_block==1, "first", "second")
tDat$block<- as.factor(tDat$block)

contrasts(tDat$font_size)

#contrasts(tDat$block)<- c(-1, 1)
#contrasts(tDat$block)

block1<- subset(tDat, block== "first")
block2<- subset(tDat, block== "second")


### first block

gam1V2 <- bam(LandStartVA ~ font_size+
              s(sub, bs="re", k=10) +
              s(sub, font_size, bs="re", k=10) +
              s(item, bs= "re", k=10)+
              s(item, font_size, bs="re") +
              s(block_order, bs= "cr", k=10)+
              s(block_order, by= font_size, k=10, bs= "cr")+
              #              s(big_font_block, by= font_size, k=10, bs= "cr")+
              s(block_order, sub, bs= "fs", m=1, k=4),
            data= block1)

summary(gam1V2)

plot_diff(gam1V2, view = "block_order", rm.ranef = F, comp = list(font_size = c("small font", "big font")), 
          col = pallete1[3], main= "d) Small font sentences (1st- 2nd block difference)",
          ylab= "Mean diff. in landing position (deg)", xlab= "Trial number within block", print.summary = T, 
          family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, hide.label = T, ylim= c(-1, 0.4))


### second block

gam2V2 <- bam(LandStartVA ~ font_size+
                s(sub, bs="re", k=10) +
                s(sub, font_size, bs="re", k=10) +
                s(item, bs= "re", k=10)+
                s(item, font_size, bs="re") +
                s(block_order, bs= "cr", k=10)+
                s(block_order, by= font_size, k=10, bs= "cr")+
                #              s(big_font_block, by= font_size, k=10, bs= "cr")+
                s(block_order, sub, bs= "fs", m=1, k=4),
              data= block2)

summary(gam2V2)

plot_diff(gam2V2, view = "block_order", rm.ranef = F, comp = list(font_size = c("small font", "big font")), 
          col = pallete1[3], main= "d) Small font sentences (1st- 2nd block difference)",
          ylab= "Mean diff. in landing position (deg)", xlab= "Trial number within block", print.summary = T, 
          family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, hide.label = T, ylim= c(-1, 0.4))



pdf('Plots/GAMMs_V2.pdf', width = 11, height = 11)
par(mfrow=c(2,2), mar= c(5,5,4,3))


## big font condition

plot_smooth(gam1, view="block_order", plot_all="big_font_block", rug=F, xlab= "Trial number within block",
            ylab= "Landing position (deg)", main= "a) Large font sentences",
            col = c(pallete1[1], pallete1[2]), legend_plot_all = list(x=0, y=0), family= "serif",
            cex.axis= 1.6, cex.lab= 1.7, hide.label = T, lwd= 2, lty= c(2,1), ylim= c(1.15, 2.35),
            cex.main=1.7)


# small font condition
plot_smooth(gam2, view="block_order", plot_all="small_font_block", rug=F, xlab= "Trial number within block",
            ylab= "Landing position (deg)", main= "b) Small font sentences",
            col = c(pallete1[1], pallete1[2]), family= "serif",
            cex.axis= 1.6, cex.lab= 1.7, legend_plot_all = list(x=0, y=0),
            hide.label = T, lwd= 2, lty= c(2,1), ylim= c(1.15, 2.35), cex.main=1.7)

legend(x = 16, y= 2.35, legend = c("large 1st - small 2nd", "small 1st - large 2nd"), title = "Block order:",
       col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
       box.col = "white", lty= c(1,2), seg.len=2, cex = 1.5)


# Add font size effect:

# plot(NA, main= "c) Font size effect (large 1st - small 2nd)",
#           ylab= "Mean diff. in landing position (deg)", xlab= "Trial number within block", 
#           family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, ylim= c(-1, 0.5), xlim = c(0, 50), axes = F)
# rect(5.94, -2, 50, 2, col= 'gray90', border = NA)
# axis(side = 1, at = c(0,10, 20, 30, 40, 50), cex.axis= 1.6, cex.lab= 1.7, family= "serif")
# axis(side=2, at= c(-1, -0.5, 0, 0.5, 1), cex.axis= 1.6, cex.lab= 1.7, family= "serif")
# 
# plot_diff(gam2V2, view = "block_order", rm.ranef = F, comp = list(font_size = c("small font", "big font")),
#           col = pallete1[2], print.summary = T,
#           family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, hide.label = T, add = T, col.diff = 'gray60')


plot_diff(gam2V2, view = "block_order", rm.ranef = F, comp = list(font_size = c("small font", "big font")),
          col = pallete1[2], main= "c) Font size effect (large 1st - small 2nd)",
          ylab= "Mean diff. in landing position (deg)", xlab= "Trial number within block", print.summary = T,
          family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, hide.label = T, ylim= c(-1, 0.5), mark.diff = T, 
          col.diff = NA)
abline(v = c(5.949495, 50), col= pallete1[6], lwd= 2.5, lty= 4 )
segments(x0 = 5.949495, x1 = 50, y0 = -1.06, y1 = -1.06, col= pallete1[6], lwd= 3)


plot_diff(gam1V2, view = "block_order", rm.ranef = F, comp = list(font_size = c("small font", "big font")), 
          col = pallete1[1], main= "d) Font size effect (small 1st- large 2nd)",
          ylab= "Mean diff. in landing position (deg)", xlab= "Trial number within block", print.summary = T, 
          family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, lty=2 , hide.label = T, ylim= c(-1, 0.5), 
          mark.diff = T, col.diff = NA)
abline(v = c(14.858586, 50), col= pallete1[6], lwd= 2.5, lty= 4 )
segments(x0 = 14.858586, x1 = 50, y0 = -1.06, y1 = -1.06, col= pallete1[6], lwd= 3)


dev.off()




####################################################################
#         Modulation of SACCADE LENGTH by trial order:             #
####################################################################

#-------------------------------
# Prepare dataset for analsysis:
#-------------------------------

# fix seq issues for a few subjects where the gaze-box was not triggered properly:

for(i in 1:nrow(Alldata2)){
  
  # subject 6
  if(Alldata2$sub[i]==6){
    # 40 & 55
    if(is.element(Alldata2$seq[i], c(51:54))){
      Alldata2$seq[i]<- Alldata2$seq[i]-1 
    }
    
    if(is.element(Alldata2$seq[i], c(56:102))){
      Alldata2$seq[i]<- Alldata2$seq[i]-2 
    }
    
  } # end of subject 6
  
  
  # subject 12
  if(Alldata2$sub[i]== 12){
    # 25, 46
    
    if(is.element(Alldata2$seq[i], c(26:46))){
      Alldata2$seq[i]<- Alldata2$seq[i]-1 
    }
    
    if(Alldata2$seq[i]>46){
      Alldata2$seq[i]<- Alldata2$seq[i]-2 
    }
    
  } # end of subject 12
  
  
  # subject 16
  if(Alldata2$sub[i]== 16){
    # 72
    if(Alldata2$seq[i]>71){
      Alldata2$seq[i]<- Alldata2$seq[i]-1 
    }
    
  } # end of subject 16
  
  
  # subject 22
  if(Alldata2$sub[i]== 22){
    # 11, 67
    
    if(is.element(Alldata2$seq[i], c(12:66))){
      Alldata2$seq[i]<- Alldata2$seq[i]-1 
    }
    
    if(Alldata2$seq[i]>66){
      Alldata2$seq[i]<- Alldata2$seq[i]-2 
    }
    
  } # end of subject 22
  
  
  # subject 46
  if(Alldata2$sub[i]== 46){
    # 53
    if(Alldata2$seq[i]>53){
      Alldata2$seq[i]<- Alldata2$seq[i]-1 
    }
  } # end of subject 46
  
  
  # subject 51
  if(Alldata2$sub[i]== 51){
    # 5
    
    if(Alldata2$seq[i]>5){
      Alldata2$seq[i]<- Alldata2$seq[i]-1 
    }
    
  }# subject 51
  
}





# check contrast coding:
contrasts(Alldata2$font_size)
contrasts(Alldata2$line_len)
Alldata2$sub<- as.numeric(as.character(Alldata2$sub))

Alldata2<- subset(Alldata2, Rtn_sweep==0) # remove return sweep saccades
table(Alldata2$Rtn_sweep)

# Add block order:

Alldata2$block_order<- NA
Alldata2$big_font_block<- NA
Alldata2$small_font_block<- NA

for(i in 1:nrow(Alldata2)){
  
  if(Alldata2$sub[i]%%2==1){ # odd subject, small font first
    
    Alldata2$big_font_block[i]<- 2
    Alldata2$small_font_block[i]<- 1
    
    if(Alldata2$font_size[i]== "small font"){ # small font, block seq= seq
      Alldata2$block_order[i]<- Alldata2$seq[i]
    }else{ # big font, block seq = seq-50 (50 is halfway point; 100 items)
      Alldata2$block_order[i]<- Alldata2$seq[i]- 50
    }
    
  }else{ # even subject, big font first
    
    Alldata2$big_font_block[i]<- 1
    Alldata2$small_font_block[i]<- 2
    
    if(Alldata2$font_size[i]== "small font"){ # big font, block seq = seq-50 (50 is halfway point; 100 items)
      Alldata2$block_order[i]<- Alldata2$seq[i]- 50
    }else{ # small font, block seq= seq
      Alldata2$block_order[i]<- Alldata2$seq[i] 
    }
    
  }
  
}


# take a smaller, more managable dataset:
tDat2<- Alldata2[,c("sub", "item", "seq", "cond", "font_size", "line_len", "launchDistVA", "undersweep_prob" ,  "block_order",
             "big_font_block", "small_font_block")]

#tDat2$big_font_block<- as.factor(tDat2$big_font_block)
#tDat2$big_font_block<- factor(tDat2$big_font_block, levels= c("small font", "big font"))
#contrasts(tDat2$big_font_block)
is.numeric(tDat2$block_order)


# gamm model:
tDat2$big_font_block<- as.factor(tDat2$big_font_block)
contrasts(tDat2$big_font_block)<- c(-1, 1)

tDat2$small_font_block<- as.factor(tDat2$small_font_block)
contrasts(tDat2$small_font_block)<- c(-1, 1)


### big font

bigF2<- subset(tDat2, font_size== "big font")

gam1S <- bam(launchDistVA ~ big_font_block+
              s(sub, bs="re", k=10) +
              s(sub, big_font_block, bs="re", k=10) +
              s(item, bs= "re", k=10)+
              s(item, big_font_block, bs="re") +
              s(block_order, bs= "cr", k=10)+
              s(block_order, by= big_font_block, k=10, bs= "cr")+
              #              s(big_font_block, by= font_size, k=10, bs= "cr")+
              s(block_order, sub, bs= "fs", m=1, k=4),
            data= bigF2)

summary(gam1S)

#plot(gam1S)


plot_smooth(gam1S, view="block_order", plot_all="big_font_block", rug=F, xlab= "Trial order within block",
            ylab= "Intra-line saccade length (deg)", main= "Big font sentences as a function of block order",
            col = c(pallete1[1], pallete1[2]), legend_plot_all = list(x=0, y=0), family= "serif",
            cex.axis= 1.2, cex.lab= 1.4)
legend(x = 2, y= 2.5, legend = c("first block", "second block"), col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
       box.col = "white")

plot_diff(gam1S, view = "block_order", rm.ranef = F, comp = list(big_font_block = c(1,  2)), 
          col = pallete1[2], main= "Big font (2nd- 1st block difference)",
          ylab= "Mean diff.in saccade length (deg)", xlab= "Trial order within block", print.summary = T, 
          family= "serif", cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)


###########

### small font

smallF2<- subset(tDat2, font_size== "small font")

gam2S <- bam(launchDistVA ~ small_font_block+
              s(sub, bs="re", k=10) +
              s(sub, small_font_block, bs="re", k=10) +
              s(item, bs= "re", k=10)+
              s(item, small_font_block, bs="re") +
              s(block_order, bs= "cr", k=10)+
              s(block_order, by= small_font_block, k=10, bs= "cr")+
              #              s(small_font_block, by= font_size, k=10, bs= "cr")+
              s(block_order, sub, bs= "fs", m=1, k=4),
            data= smallF2)

summary(gam2S)

#plot(gam2)


plot_smooth(gam2S, view="block_order", plot_all="small_font_block", rug=F, xlab= "Trial order within block",
            ylab= "Intra-line saccade length (deg)", main= "Small font sentences as a function of block order",
            col = c(pallete1[2], pallete1[1]), family= "serif",
            cex.axis= 1.2, cex.lab= 1.4, legend_plot_all = list(x=0, y=0))
legend(x = 35, y= 2.4, legend = c("first block", "second block"), col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
       box.col = "white")

plot_diff(gam2S, view = "block_order", rm.ranef = F, comp = list(small_font_block = c(1,  2)), 
          col = pallete1[2], main= "Small font (2nd- 1st block difference)",
          ylab= "Mean diff.in saccade length (deg)", xlab= "Trial order within block", print.summary = T, 
          family= "serif", cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)




####################
# GAMM panel plot: #
####################

tDat2$block<- ifelse(tDat2$small_font_block==1, "first", "second")
tDat2$block<- as.factor(tDat2$block)

contrasts(tDat2$font_size)

block1S<- subset(tDat2, block== "first")
block2S<- subset(tDat2, block== "second")


### first block

gamS1V2 <- bam(launchDistVA ~ font_size+
                s(sub, bs="re", k=10) +
                s(sub, font_size, bs="re", k=10) +
                s(item, bs= "re", k=10)+
                s(item, font_size, bs="re") +
                s(block_order, bs= "cr", k=10)+
                s(block_order, by= font_size, k=10, bs= "cr")+
                #              s(big_font_block, by= font_size, k=10, bs= "cr")+
                s(block_order, sub, bs= "fs", m=1, k=4),
              data= block1S)

summary(gamS1V2)

plot_diff(gamS1V2, view = "block_order", rm.ranef = F, comp = list(font_size = c("small font", "big font")), 
          col = pallete1[3], main= "d) Small font sentences (1st- 2nd block difference)",
          ylab= "Mean diff. in landing position (deg)", xlab= "Trial number within block", print.summary = T, 
          family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, hide.label = T, ylim= c(-1, 0.4))


### second block

gamS2V2 <- bam(launchDistVA ~ font_size+
                s(sub, bs="re", k=10) +
                s(sub, font_size, bs="re", k=10) +
                s(item, bs= "re", k=10)+
                s(item, font_size, bs="re") +
                s(block_order, bs= "cr", k=10)+
                s(block_order, by= font_size, k=10, bs= "cr")+
                #              s(big_font_block, by= font_size, k=10, bs= "cr")+
                s(block_order, sub, bs= "fs", m=1, k=4),
              data= block2S)

summary(gamS2V2)

plot_diff(gamS2V2, view = "block_order", rm.ranef = F, comp = list(font_size = c("small font", "big font")), 
          col = pallete1[3], main= "d) Small font sentences (1st- 2nd block difference)",
          ylab= "Mean diff. in landing position (deg)", xlab= "Trial number within block", print.summary = T, 
          family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, hide.label = T, ylim= c(-1, 0.4))


##################################################################

pdf('Plots/IntraLine_GAMMsv2.pdf', width = 11, height = 11)
par(mfrow=c(2,2), mar= c(5,5,4,3))


## big font condition

plot_smooth(gam1S, view="block_order", plot_all="big_font_block", rug=F, xlab= "Trial number within block",
            ylab= "Intra-line saccade length (deg)", main= "a) Large font sentences",
            col = c(pallete1[1], pallete1[2]), legend_plot_all = list(x=0, y=0), family= "serif",
            cex.axis= 1.6, cex.lab= 1.7, hide.label = T, lwd= 2, lty= c(2,1), ylim= c(2, 4),
            cex.main=1.7)


# small font condition
plot_smooth(gam2S, view="block_order", plot_all="small_font_block", rug=F, xlab= "Trial number within block",
            ylab= "Intra-line saccade length (deg)", main= "b) Small font sentences",
            col = c(pallete1[1], pallete1[2]), family= "serif",
            cex.axis= 1.6, cex.lab= 1.7, legend_plot_all = list(x=0, y=0),
            hide.label = T, lwd= 2, lty= c(2,1), ylim= c(2, 4), cex.main=1.7)

legend(x = 16, y= 3.88, legend = c("large 1st - small 2nd", "small 1st - large 2nd"), title = "Block order:",
       col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
       box.col = "white", lty= c(1,2), seg.len=2, cex = 1.5)


# ### Add block order effect:
# plot_diff(gam1S, view = "block_order", rm.ranef = F, comp = list(big_font_block = c(1,  2)), 
#           col = pallete1[3], main= "c) Large font sentences (1st- 2nd block difference)",
#           ylab= "Mean diff. in saccade length (deg)", xlab= "Trial number within block", print.summary = T, 
#           family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, hide.label = T, ylim= c(-0.6, 0.6))
# 
# 
# plot_diff(gam2S, view = "block_order", rm.ranef = F, comp = list(small_font_block = c(1,  2)), 
#           col = pallete1[3], main= "d) Small font sentences (1st- 2nd block difference)",
#           ylab= "Mean diff. in saccade length (deg)", xlab= "Trial number within block", print.summary = T, 
#           family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, hide.label = T, ylim= c(-0.6, 0.6))


plot_diff(gamS2V2, view = "block_order", rm.ranef = F, comp = list(font_size = c("small font", "big font")),
          col = pallete1[2], main= "c) Font size effect (large 1st - small 2nd)",
          ylab= "Mean diff. in landing position (deg)", xlab= "Trial number within block", print.summary = T,
          family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, hide.label = T, ylim= c(-1, 0.5), mark.diff = T, 
          col.diff = NA)
abline(v = c(1, 50), col= pallete1[6], lwd= 2.5, lty= 4 )
segments(x0 = 1, x1 = 50, y0 = -1.06, y1 = -1.06, col= pallete1[6], lwd= 3)


plot_diff(gamS1V2, view = "block_order", rm.ranef = F, comp = list(font_size = c("small font", "big font")), 
          col = pallete1[1], main= "d) Font size effect (small 1st- large 2nd)",
          ylab= "Mean diff. in landing position (deg)", xlab= "Trial number within block", print.summary = T, 
          family= "serif", cex.axis= 1.6, cex.lab= 1.7, cex.main= 1.7, lwd= 2, lty=2 , hide.label = T, ylim= c(-1, 0.5), 
          mark.diff = T, col.diff = NA)
abline(v = c(1, 50), col= pallete1[6], lwd= 2.5, lty= 4 )
segments(x0 = 1, x1 = 50, y0 = -1.06, y1 = -1.06, col= pallete1[6], lwd= 3)


dev.off()



#########################################################################################################################
#                                  Post-hoc analyses suggested by Reviewers                                             #
#########################################################################################################################

####### Do variences of under-sweep probability differs between the two font size conditions:

# Since the data is binomial, we average over items to get a single mean for each condition:



small_font<- subset(RS, font_size== "small font")
small_font<- small_font$LandStartVA


big_font<- subset(RS, font_size== "big font")
big_font<- big_font$LandStartVA

#db<- data.frame(small= small_font, big= big_font)

#db2<- data.frame(Mean= c(db$small, db$big))
#db2$Font<- c(rep("small", 128), rep("big", 128))

library(tidyverse)
library(ggpubr)
library(ggplot2)

# let's check normality distribution:
ggqqplot(small_font)
ggqqplot(small_font)

shapiro.test(small_font)
shapiro.test(big_font)

# density plot:
p <- ggplot(RS, aes(x=LandStartVA, color= font_size)) + 
  geom_density()+scale_color_brewer(palette="Dark2")+ theme_minimal(22)+ xlab("Landing position(deg)")
p





# normality assumption is violated in both conditions, so we use non-parametric test:
fligner.test(LandStartVA ~ font_size, data = RS)



##################################################################################################################################


# Reviewer 1:
# Compare fixations before regression and under-sweeps



Alldata$pre_Reg<- NA
Dat<- NULL

nsubs<- unique(Alldata$sub)

for(i in 1:length(nsubs)){
  n<- subset(Alldata, sub== nsubs[i])
  
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    
    for(k in 1:nrow(m)){
      
      if(k>1){
        if(!is.na(m$regress[k]) & !is.na(m$regress[k-1])){
          if(m$regress[k]==1){
            if(m$regress[k-1]==1){
              next
            }else{
              m$pre_Reg[k-1]<- 1
            }
            
          }
        }

      }
      
    } # end of k
    #c<- m[, c('regress', 'pre_Reg')]
    
    Dat<- rbind(Dat, m)
    
  } # end of j
  cat(i); cat(" ")
}

table(Dat$pre_Reg)

d1<- subset(Dat, pre_Reg==1)
d1$pre_Reg<- NULL
d1$fix_type<- "Pre-regression fixation"
d1$distance<- d1$xPos -d1$nextX
d1<- d1[-which(d1$distance<0),]


d2<- subset(Alldata, undersweep_prob==1)
d2$pre_Reg<- NULL
d2$fix_type<- "Under-sweep fixation"
d2$distance<- d2$xPos - d2$nextX

Dat<- rbind(d1, d2)
rm(d1, d2)
table(Dat$fix_type)



### plot:
library(ggplot2)

P1<- ggplot(Dat, aes(x=fix_dur, color=fix_type, fill=fix_type)) +
 # geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins=20)+
  geom_density(alpha=0.6)+ theme_classic2(22) + xlab("Fixation duration (ms)")+ ylab("Density")+
  scale_color_manual(values=c("#E69F00", "#999999"))+
  scale_fill_manual(values=c("#E69F00", "#999999")) +theme(legend.position = c(0.7, 0.8))+
  scale_x_continuous(breaks = c(150, 350, 550 , 750))
P1

ggsave(filename = "Plots/Fix_comp.pdf", plot = P1, width = 7, heigh=6)


library(lme4)

Dat$fix_type<- as.factor(Dat$fix_type)
contrasts(Dat$fix_type)<- c(1, -1)

Dat$distance<- scale((Dat$xPos - Dat$nextX)*0.02461393513610085)

summary(FDM<- lmer(log(fix_dur)~ fix_type*distance +(fix_type|sub) + (1|item), data= Dat)) 

library(effects)

plot(effect('distance', FDM))
plot(effect('fix_type', FDM))
plot(effect('fix_type:distance', FDM))


fd= allEffects(FDM)

x= as.data.frame(fd)
x=as.data.frame(x)
colnames(x)= c("fix_type", "distance", "fit", "se", "lower", "upper")


df<-  x
df$fix_type<- as.factor(df$fix_type)
df$fix_type<- droplevels(df$fix_type)
levels(df$fix_type)

G1<- ggplot(df, aes(x= distance, y=fit, ymax= upper, ymin= lower,
                    color=fix_type, linetype= fix_type, fill= fix_type, shape= fix_type)) + theme_classic(20)+
  geom_line(size= 1)+ geom_point(size=4)+
  scale_color_manual(values=c("#E69F00", "#999999"))+
  scale_fill_manual(values=c("#E69F00", "#999999"))+
  labs(x= "Distance to next saccade location (centred in deg)", y= "Fixation duration (log)", 
       color= "", shape= '', linetype= '', fill= '') +geom_ribbon(alpha= 0.2, color= NA) +
  theme(legend.position = c(0.32, 0.15))

G1
  

ggsave(filename = 'Plots/fix_dur_x_Dist.pdf', plot = G1, width = 8, height = 8)
