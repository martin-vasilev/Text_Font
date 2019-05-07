
# MArtin R. Vasilev, 2019

# Data pre-processing:


#hgfdgdfcf

# Calvin 1-21
# Victoria- 22- 43
# Martin 44+

library(EMreading)

###########################
# Comprehension accuracy: #
###########################

<<<<<<< HEAD
Quest<- Question(data_list = "E:/FontSizeData", maxtrial = 100)
=======
Quest<- Question(data_list = "D:/Data/Font_size", maxtrial = 100)
>>>>>>> eb2605d88f422e57fab0ffacc81f382dd1b92126

save(Quest, file= "data/Quest.Rda")


<<<<<<< HEAD
# Trial times:
Trialt<- trialTime(data_list = "E:/FontSizeData", maxtrial = 100)

save(Trialt, file= "data/Trial_time.Rda")

M<- MultiLine(data_list = "D:/Data/Font_size/new", maxtrial = 100, reAlign = F)

data_dir<-setwd()

Paddedfiles <-EyeDoctor_PadLines(data_dir = "E:/FontSizeData", paddingSize = 5)
=======
library(reshape)
DesQuest<- melt(Quest, id=c('sub', 'item', 'cond'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

################
# Trial times: #
################

Trialt<- trialTime(data_list = "D:/Data/Font_size/", maxtrial = 100)

save(Trialt, file= "data/Trial_time.Rda")


DesTime<- melt(Trialt, id=c('sub', 'item', 'cond'), 
                measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, cond ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))



#M<- MultiLine(data_list = "D:/Data/Font_size/new", maxtrial = 100, reAlign = F)


EyeDoctor_PadLines("D:/Data/Font_size/new")


>>>>>>> eb2605d88f422e57fab0ffacc81f382dd1b92126
