
# Martin R. Vasilev, 2019

# Data pre-processing:


# Manual pre-processing of asc files:

# Calvin 1-21
# Victoria- 22- 43
# Martin 44+

# EyeDoctor_PadLines(data_dir = data_dir, paddingSize = 5)

# Install/ load R package used in preprocessing:

if('EMreading' %in% rownames(installed.packages())==FALSE){
  if('devtools' %in% rownames(installed.packages())==FALSE){
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
  install_github('martin-vasilev/EMreading')
}else{
  library(EMreading)
}


data_dir= "D:/Data/Font_size" # Martin
#data_dir= "E:/FontSizeData" # Victoria

###########################
# Comprehension accuracy: #
###########################

if(!file.exists("data/Quest.Rda")){
  Quest<- Question(data_list = data_dir, maxtrial = 100)
  save(Quest, file= "data/Quest.Rda")
  write.csv2(Quest, "data/Quest.csv")
} else{
  load("data/Quest.Rda")
}


library(reshape)
DesQuest<- melt(Quest, id=c('sub', 'item', 'cond'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
mQuest

################
# Trial times: #
################

if(!file.exists("data/Trial_time.Rda")){
  
  Trialt<- trialTime(data_list = data_dir, maxtrial = 100)
  save(Trialt, file= "data/Trial_time.Rda")
  write.csv2(Trialt, "data/Trial_time.csv")
}else{
  load("data/Trial_time.Rda")
}

DesTime<- melt(Trialt, id=c('sub', 'item', 'cond'), 
                measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, cond ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
mTime


##################
# Raw Fixations: #
##################

if(!file.exists("preproc/raw_fix.Rda")){
  # extract raw data & merge it with da1 files:
  raw_fix<- preprocFromDA1(data_dir = data_dir, maxtrial = 100, padding = 5)
  save(raw_fix, file= "preproc/raw_fix.Rda")
  write.csv2(raw_fix, file= "preproc/raw_fix.csv")
}


##############################
# Preprocessing of raw data: #
##############################

# See pre-registered protocol for data preprocessing criteria:
# https://osf.io/9sngw#analysis-plan.data-exclusion

load("preproc/raw_fix.Rda")

raw_fix$hasText<- NULL

nAllTrials<- 64*100

# check number of trials per subject
nTrials<- NULL

for(i in 1:64){
  n<- subset(raw_fix, sub==i)
  nTrials[i]<- length(unique(n$item))
}
nTrials

# 3 trials were discarded during manual processing (due to track losses, etc.)

nDiscardedTrials<- nAllTrials- sum(nTrials)


# check if there is only 1 return sweep per trial (expected, due to the two lines):

noRS_sub<- NULL # no return sweeps this trial
noRS_item<- NULL # no return sweeps this trial
for(i in 1:64){
  n<- subset(raw_fix, sub==i)
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    
    if(sum(m$Rtn_sweep)>1){ # >1 RS detected..
      cat(sprintf("Subject %g, item %g, %g return sweeps\n", i, nitems[j], sum(m$Rtn_sweep)))
    }
    
    if(sum(m$Rtn_sweep)==0){ # no return sweeps detected
      cat(sprintf("Subject %g, item %g, %g return sweeps\n", i, nitems[j], sum(m$Rtn_sweep)))
      noRS_sub<- c(noRS_sub, i)
      noRS_item<- c(noRS_item, nitems[j])
      
    }
  }
}

# remove trials with no return sweeps:
for(i in 1:length(noRS_sub)){
  out<- which(raw_fix$sub== noRS_sub[i]& raw_fix$item== noRS_item[i])
  raw_fix<- raw_fix[-out,]
  
}
nNoRS<- length(noRS_sub)

# check for blinks occuring on return sweep saccade/fixation:
RS_blinks<- raw_fix[which(raw_fix$Rtn_sweep==1), ]
RS_blinks<- subset(RS_blinks, blink==1 | prev_blink==1 | after_blink==1)

nBlinks<- nrow(RS_blinks)

for(i in 1:nrow(RS_blinks)){
  raw_fix<- raw_fix[-which(raw_fix$sub== RS_blinks$sub[i]& raw_fix$item== RS_blinks$item[i]),]
}

# confirm we got the correct num of trials:
RS_blinks<- raw_fix[which(raw_fix$Rtn_sweep==1), ]
nrow(RS_blinks)== nAllTrials- nBlinks- nNoRS- nDiscardedTrials

RS_blinks<- subset(RS_blinks, blink==1 | prev_blink==1 | after_blink==1)

if(nrow(RS_blinks)==0){
  cat("GOOD!")
  rm(RS_blinks)
}else{
  cat(":(")
}

# check for outliers:

# first, we need to code in the data if prev or next fixation is a return sweep:

raw_fix_new<- NULL

raw_fix$prev_RS<- NA
raw_fix$next_RS<- NA
raw_fix$prevChar<-NA
raw_fix$nextChar<- NA
raw_fix$prevX<- NA
raw_fix$nextX<- NA

for(i in 1:64){
  n<- subset(raw_fix, sub==i)
  nitems<- unique(n$item)
  cat(i); cat(" ")
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    
    for(k in 1:nrow(m)){
      if(k==1){
        m$prev_RS[k]<- 0
        m$next_RS[k]<- 0
        m$next_RS[k+1]<- 0
        
        ####
        raw_fix$nextChar[k]<- raw_fix$char_line[k+1] # next char
        raw_fix$nextX[k]<- raw_fix$xPos[k+1]
        
      }else{
        if(m$Rtn_sweep[k]==1){
          m$prev_RS[k-1]<- 1
          
          if(k+1 <= nrow(m)){
            m$next_RS[k+1]<- 1
          }
          
        }else{
          m$prev_RS[k-1]<- 0
          
          if(k+1 <= nrow(m)){
            m$next_RS[k+1]<- 0
          }
        }
        ###
        raw_fix$prevChar[k]<- raw_fix$char_line[k-1] # prev char
        raw_fix$prevX[k] <- raw_fix$xPos[k-1] # prev x
        
        if(k+1<= nrow(m)){
          raw_fix$nextChar[k]<- raw_fix$char_line[k+1] # next char
          raw_fix$nextX[k]<- raw_fix$xPos[k+1] # next x
        }
        
        
      }
      
      if(k== nrow(m)){
        m$prev_RS[k]<- 0
      }
      
    } # end of m
    raw_fix_new<- rbind(raw_fix_new, m)
  } # end of j
  
  
}

raw_fix<- raw_fix_new;
rm(raw_fix_new)

outliers<- raw_fix[which(raw_fix$fix_dur>1000),]

outliers<- subset(outliers, Rtn_sweep==1 | prev_RS==1 | next_RS==1)

nOutliers<- nrow(outliers)

for(i in 1:nrow(outliers)){
  raw_fix<- raw_fix[-which(raw_fix$sub== outliers$sub[i] & raw_fix$item== outliers$item[i]), ]
}


# Now, let's merge fixations smaller than 80 ms
less80<- raw_fix[which(raw_fix$fix_dur<80 & raw_fix$Rtn_sweep==1), ]

raw_fix_new<- cleanData(raw_fix, removeOutsideText = F, removeBlinks = F, combineNearbySmallFix = T, 
                        combineMethod = "pix", combineDist = 14, removeSmallFix = F, 
                        removeOutliers = F, keepRS = T)
raw_fix<- raw_fix_new
rm(raw_fix_new)

# remove remaining fixations less than 80 only if they are not return sweeps:
raw_fix<- raw_fix[-which(raw_fix$fix_dur<80 & raw_fix$Rtn_sweep==0), ]


# let's verify we have the correct number of trials:
RS<- subset(raw_fix, Rtn_sweep==1)

nrow(RS)+ nOutliers+ nBlinks+ nNoRS+ nDiscardedTrials == nAllTrials

# now let's print a summary:

fileConn<- file("preproc/Preproc_summary.txt", "w")

writeLines(sprintf("- %#.2f percent of trials manually discarded due to tracking loss, etc.",  
                   round((nDiscardedTrials/nAllTrials)*100, 2)), fileConn)

writeLines(sprintf("- %#.2f percent of trials discarded due to the lack of a return sweep in the trial",  
                   round((nNoRS/nAllTrials)*100, 2)), fileConn)

writeLines(sprintf("- %#.2f percent of trials discarded due to blinks on or around return sweeps",  
                   round((nBlinks/nAllTrials)*100, 2)), fileConn)

writeLines(sprintf("- %#.2f percent of trials discarded due to outliers",  
                   round((nOutliers/nAllTrials)*100, 2)), fileConn)

writeLines(sprintf("- %#.2f percent of trials remaining for analysis",  
                   round((nrow(RS)/nAllTrials)*100, 2)), fileConn)

close(fileConn)


