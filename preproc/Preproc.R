
# MArtin R. Vasilev, 2019

# Data pre-processing:

library(EMreading)


# COmprehension accuracy:

Quest<- Question(data_list = "D:/Data/Font_size/new", maxtrial = 100)

save(Quest, file= "data/Quest.Rda")


# Trial times:
Trialt<- trialTime(data_list = "D:/Data/Font_size/new", maxtrial = 100)

save(Trialt, file= "data/Trial_time.Rda")

M<- MultiLine(data_list = "D:/Data/Font_size/new", maxtrial = 100, reAlign = F)
