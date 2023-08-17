library(dplyr)
library(plyr)
library(stringr)

#Coding_data <- read.delim("/Users/sydneyliu/Desktop/purmTest/CardGameVideo_Fall2022_Spring2023_Data_v3.txt")
Coding_data <- read.delim("/Users/sydneyliu/Desktop/purmTest/CardGameVideo_Fall2022_Spring2023_Data_Aug.txt")

#Accounting for camera malfunction in file 01
Coding_data$Onset_sec[((Coding_data$Filename == "NEWCARD_01alter") & (Coding_data$Onset_sec >= 1200))]  = Coding_data[((Coding_data$Filename == "NEWCARD_01alter") & (Coding_data$Onset_sec >= 1200)), 5] + 1200
Coding_data$Offset_sec[((Coding_data$Filename == "NEWCARD_01alter") & (Coding_data$Offset_sec >= 1200))]  = Coding_data[((Coding_data$Filename == "NEWCARD_01alter") & (Coding_data$Offset_sec >= 1200)), 6] + 1200

Coding_data$Filename[Coding_data$Filename == 'NEWCARD_01alter_WAV'] = 'NEWCARD_01alter'

######______Dahan's Code for ordered TrialNb______####
Coding_data$TierOrder <- ifelse(Coding_data$TierName=="Hand", "1", ifelse(Coding_data$TierName=="Trial", "2", "3"))

Coding_data_ordered<- Coding_data[order(Coding_data$Filename, round(Coding_data$Onset_sec), Coding_data$TierOrder), ]

Coding_data_trialnb <- ddply(Coding_data_ordered, .(Filename), function(x){
  x<- Coding_data_ordered
  for(i in 1:nrow(x)){
    if(x[i, c("TierName")]== "Hand"){
      onsetHand <- round(x[i, c("Onset_sec")])
      handnb <- x[i, c("Annotation")]
      x[i, c("TrialNb")] <- "none"
      x[i, c("TrialOnset")] <- "none"
      x[i, c("TrialOffset")] <- "none"
      x[i, c("HandNb")] <- "none"
      x[i, c("OnsetHand")] <- "none"
    }
    else{
      if(x[i, c("TierName")] == "Trial"){
        onsetTrial <- round(x[i, c("Onset_sec")])
        offsetTrial <- round(x[i, c("Offset_sec")])
        trialnb <- x[i, c("Annotation")]
        x[i, c("TrialNb")] <- "none"
        x[i, c("TrialOnset")] <- "none"
        x[i, c("TrialOffset")] <- "none"
        x[i, c("HandNb")] <- "none"
        x[i, c("OnsetHand")] <- "none"
      }
      else{
        x[i, c("HandNb")] <- handnb
        x[i, c("OnsetHand")] <- onsetHand
        if((round(x[i, c("Onset_sec")]) >= onsetTrial) & (round(x[i, c("Offset_sec")]) <= offsetTrial)){
          x[i, c("TrialNb")] <- trialnb
          x[i, c("TrialOnset")] <- onsetTrial
          x[i, c("TrialOffset")] <- offsetTrial
        }
        else{
          x[i, c("TrialNb")] <- "error"
          x[i, c("TrialOnset")] <- "error"
          x[i, c("TrialOffset")] <- "error"
        }
      }
    }
  }
  return(x)
})

# Checking for the errors in order to deal with them.
table(Coding_data_trialnb$TrialNb, useNA="always")
df <- Coding_data_trialnb

######_____ADDING CONTENTS COL______####
#Adding Contents as a dataframe column
contents <- distinct(df[grep("Contents", df$TierName), ])
df_noContents <- distinct(df[-grep("Contents", df$TierName), ])

mergedContents <- left_join(x=df_noContents, y=contents, by = c('Filename', 'Onset_sec', 'TrialNb', 'HandNb'), relationship = "many-to-many")

#Remove redundant hand and trial rows
df_c <- mergedContents %>% transmute(Filename = Filename,
                                      TierName = TierName.x,
                                      HandNb = HandNb,
                                      TrialNb = TrialNb,
                                      Type = Annotation.x,
                                      Contents = Annotation.y,
                                      Onset = Onset_sec)

df_cleaned <- df_c[df_c$TierName != "Hand" & df_c$TierName != "Trial", ] 


###________ERROR CHECK: Contents_____#####
#df_NAContents <- df_cleaned[is.na(df_cleaned$Contents) == TRUE, ]
#df_needContents <- df_NAContents[-grep("doubles|gap|combination|cards", df_NAContents$Type), ]
#View(df_needContents)


######______DIRECTOR (Confed vs. Naive) Column______####
df_handCode <- read.delim("/Users/sydneyliu/Desktop/purmTest/HandComposition_14participants_June2023 copy 3.txt") %>% 
  select(Filename, HandNb, HANDCODE)

df_handCodeMerge <- merge(x=df_cleaned, y=df_handCode, by = c('Filename', 'HandNb'))
df_handCodeMerge <- df_handCodeMerge[order(df_handCodeMerge$Filename, df_handCodeMerge$HandNb, df_handCodeMerge$TrialNb, df_handCodeMerge$Onset), ]

full <- df_handCodeMerge %>% 
  mutate(Director = ifelse(str_detect(HANDCODE, '\\d'), "Confed", "Naive")) %>%
  select(-HANDCODE)
#View(full)

#######____REMOVE NAIVE DIRECTORS from full___##
full <- full[full$Director == "Confed",]

#####____ADD RANK COL, ADD Number of Answers (Answers Col) to full___###
currTrial <- full[1, "TrialNb"]
prevTrial <- full[1, "TrialNb"]
#rank <- 1
prevQ <- 1
numAns <- 0
for (r in 1:nrow(full)){
  #update turnrank / trial number (didn't end up using turn rank...)
  currTrial <- full[r, "TrialNb"]
  if (currTrial != prevTrial){
    prevTrial <- currTrial
    #rank <- 1
  }
  #full[r, c("TurnRank")] <- rank
  #rank <- rank + 1
  
  #update answers column
  if (r == 1){
    #do nothing
  } else if (str_detect(full[r, "Type"], 'spec\\?|any_type\\?|any_suit\\?|no_type\\?|no_suit\\?')
      || currTrial != prevTrial){ 
    full[prevQ, c("Answers")] <- numAns
    numAns <- 0
    prevQ <- r
  } else {
    numAns <- numAns + 1
    #fill in last trial
    full[r, c("Answers")] <- NA
    if (r == nrow(full)){
      full[prevQ, c("Answers")] <- numAns
    }
  }
}


#CHANGE HAND NUMBER TO INT (remove H)
full$HandNb <- substring(full$HandNb, 2, 3) %>% as.numeric()
View(full)



#######________TRIALS OF / TRIAL CONTEXT VIEWER FUNCTIONS_____##
TrialsOf <- function(df_rows){
  df_rows <- df_rows %>% select(Filename, HandNb, TrialNb) %>% distinct()
  trials_of_rows <- merge(x=df_rows, y=full, by = c("Filename", "HandNb", "TrialNb"), all.x = TRUE, all.y = FALSE)
  trials_of_rows <- trials_of_rows[order(trials_of_rows$Filename, trials_of_rows$HandNb, trials_of_rows$TrialNb, trials_of_rows$Onset), ]
  return (trials_of_rows %>% distinct())
}

View_trials <- function(df_rows){
  View(TrialsOf(df_rows))
}


###____MULTIPLE QUESTION TRIALS ___##
# q_not_rankOne <- full[grep("spec|any_type?|any_suit?", full$Type), ]
# q_not_rankOne <- q_not_rankOne[q_not_rankOne$TurnRank > 1, ]
# View(q_not_rankOne)
# View_trials(q_not_rankOne)
#These trials need to be broken up(?)



####____ERROR CHECK: "-0" Error___##
# Checking for -0 (eg. typeA-0)
# df_zero <- full[grep("-0", full$Contents), ]
# df_zero <- df_zero %>% select(Filename, TrialNb, HandNb) %>% distinct
# 
# #merge with trials to find which rows should be kept
# 
# trials_zero <- merge(x=df_zero, y=full, all.x=TRUE, all.y=FALSE)
# trials_zero <- trials_zero[order(trials_zero$Filename, trials_zero$HandNb, trials_zero$TrialNb, trials_zero$Onset), ]
# View(trials_zero)
# 
# #let us remove trials in which spec? occurs.
# zero_spec_trials <- trials_zero[trials_zero$Type == "spec?" | trials_zero$Type == "spec_2?" | trials_zero$Type == "what_suit?", ] %>%
#   select(Filename, TrialNb, HandNb) %>% distinct()
# View(zero_spec_trials)
# 
# #anti-join these guys
# trials_zero_bad <- anti_join(x=trials_zero, y=zero_spec_trials, by=c('Filename', 'TrialNb', 'HandNb'))
# View(trials_zero_bad)




#####________________OFFER type?_____#####
OfferKind <- function(qSubset, kind){
  if (qSubset[1, "Type"] == "spec?" || qSubset[1, "Type"] == "no_spec?"){
    return(colContainsStr(qSubset$Type, kind))#r_id_type/suit/neither || r_num_type/suit/neither
  }
  if (kind == "neither"){
    return(colContainsStr(qSubset$Type, "neither"))
  }
  return(NA)
}


#####_____CATEGORIZE_Q_____######
#Calls helper functions for categorizing diff questions.
CategorizeQ <- function(qSubset){
  #change to question type?
  #Are we categorizing when the helper asks?
  if (qSubset[1, "TierName"] == "LeftIndividualSpeech"){
    return("helper_ask")
  }
  #spec?
  if (qSubset[1, "Type"] == "spec?" || qSubset[1, "Type"] == "no_spec?"){
    return(Categorize_spec(qSubset))
  } 
  #no_spec would be dealt with separately: yes and no function differently there.
  
  #any_type?
  else if (qSubset[1, "Type"] == "any_type?" || qSubset[1, "Type"] == "no_type?"){
    return(Categorize_anyType(qSubset))
  } 
  #any_suit
  else if (qSubset[1, "Type"] == "any_suit?" || qSubset[1, "Type"] == "no_suit?"){
    return (Categorize_anySuit(qSubset))
  }
  #else - ERROR or TODO
  return("Q_NA")
}


##_colContainsStr(col, s): partial default, exact matches using regex "^___$"#
colContainsStr <- function(col, s){
  return(sum(str_detect(col, s), na.rm = TRUE) > 0)
}

#####_____CATEGORIZE SPEC_____________######
Categorize_spec <- function(qSubset){
  spec_card <-  qSubset$Contents[1]
  
  #no suit/no type "-0" in contents.
  #don't think I'll need to check the exact contents that are -0'ed - manually checking, seems fine so far
  if (colContainsStr(qSubset$Contents, "-0")){
    if (colContainsStr(qSubset$Type, "r_id_suit")){
      #8D? I have no Diamonds (suitD-0). But I have an 8C (r_id)
      return("no_offerSuit")
    } else if (colContainsStr(qSubset$Type, "r_id_type")){
      return("no_offerType")
    } else if (colContainsStr(qSubset$Type, "r_id_neither")){
      return("no_offerNeither")
    }
    return("no_suit/no_type")
  }  
  
 
  if (colContainsStr(qSubset$Type, '^q_no$')){
    #just a no
    if (nrow(qSubset) == 2){
      return("no_polar")
    } 
    #otherwise, there is no, and something else => another offer of some sort
    #(recall that I am subsetting by question as opposed to trial)
    #this should be fine based on the way we're categorizing
    return("no_but")
  } 
 
  #hand immediately [end], or "yes" then hand [end]
  if (nrow(qSubset) == 2 && qSubset$Type[2] == "HandingCard" ||
      nrow(qSubset) == 3 && qSubset$Type[2] == "q_yes"
      && qSubset$Type[3] == "HandingCard"){
    return("yes_unprompted")
  }
  

  #If q_yes || they mentioned they had the card in question in contents,
  #NOTE: sum(str_detect(qSubset$Contents, spec_card), na.rm = TRUE) -> number of rows with contains containing partial string of the spec card 
  #>1, not >0 because spec? will have that content
  if (colContainsStr(qSubset$Type, 'q_yes') ||
      sum(str_detect(qSubset$Contents, spec_card), na.rm = TRUE) > 1){
    handedAlready <- FALSE
    for (i in 1:nrow(qSubset)){
      if (qSubset$Type[i] == "HandingCard"){
        handedAlready <- TRUE
      }
      #offer that is not the spec? content
      if (str_detect(qSubset$Type[i], "r_id")
          && qSubset$Contents[i] != spec_card
          || qSubset$Type[i] == "r_num_type"){
        if (handedAlready){
          return("yes_hand_offer")
        }
        return("yes_offer_hand")
      }
    }
    return("yes_prompted")
  }

  if (colContainsStr(qSubset$Type, 'r_num') || colContainsStr(qSubset$Type, 'r_id')){
    #Do you have AH? I have two Aces, AD and AS.
    #do you have 8C? I have 8D.
    return("no_implicature")
  }
  return("spec_ERROR")
}


#####_____CATEGORIZE ANY_TYPE_______######
Categorize_anyType <- function(qSubset){
  if (colContainsStr(qSubset$Type, '^q_no$')){
    if (nrow(qSubset) == 2){
      return("no_polar")
    } 
    return("no_but")
  } 
  if (colContainsStr(qSubset$Type, "neither") 
      && (colContainsStr(qSubset$Type, "^r_id_type$") 
          || colContainsStr(qSubset$Type, "^q_yes$"))){
    #list and offer (offer is id_neither)
    return("yes_list_offer")
  }
  
  #Give number
  if (colContainsStr(qSubset$Type, "^r_num_type$")){
    if (colContainsStr(qSubset$Type, "^r_id_type$")){
      #number and list
      return("yes_num_list")
    }
    #just number
    return("yes_num_only")
  }
  
  #no number given, but relevant id
  if (colContainsStr(qSubset$Type, "^r_id_type$")){
    return("yes_list_relevant")
    #Any 8s? i have 8D and 8C.
  }
  #no ids of any sort, just a yes (handing opt)
  if (colContainsStr(qSubset$Type, "q_yes")){
    return("yes_polar")
  }
  
  return("any_type_ERROR")
}

#####_____CATEGORIZE ANY_SUIT_______######
Categorize_anySuit <- function(qSubset){
  #just no
  if (colContainsStr(qSubset$Type, '^q_no$')){
    if (nrow(qSubset) == 2){
      return("no_polar")
    } 
    return("no_but")
  } 
  #yes, what suit? list => yes polar
  if (nrow(qSubset) > 2 && qSubset[2, "Type"] == "q_yes"
      && qSubset[3, "Type"] == "what_suit?"){
    return("yes_polar")
  }
  
  if (colContainsStr(qSubset$Type, "what_suit?")
      && colContainsStr(qSubset$Type, "^r_num_suit$")){
    return("yes_num")
  }
  
  if (colContainsStr(qSubset$Type, '^r_id_suit$')){
    if (colContainsStr(qSubset$Type, 'neither')){
      #clubs? 10C and JC. I also have two other 10s.
      return("list_and_extra")
    }
    #any hearts? I have AH and KH.
    #OR, any hearts? I have two. AH and KH.
    return("list_relevant")
  }
  #Don't think this really occurs: any hearts? I have two. [END]
  if (colContainsStr(qSubset$Type, '^r_num_suit$')){
    return("yes_num")
    #the only one i'm seeing is the num suit 0 which should be coded as no - fixed in ELAN.
  }
  
  #Don't think this actually happens - just a yes, no what_suit follow up
  if (colContainsStr(qSubset$Type, '^q_yes$')){
    return("yes_polar")
  }
  
  return("any_suit_ERROR")
}
#########



###__CATEGORIZE FULL: Calling CategorizeQ on each question subset___##
#Now that CategorizeQ function exists, we must A) identify each question subset, and B) call CategorizeQ on it.
i=1
qSubset <- NA
currQ <- 1
endInd <- 1 #ending index of qSubset
skipped <- c()
while (i <= nrow(full)){
  #Answers column contains the number of "answers" (any row following said question, before the next question) for rows with Type question.
  #We use this column to subset based on question
  #(Note: Answers column contains NA if it is an "answer" row (rather than a question row) - this is the first if statement
  if (is.na(full$Answers[i])){ #this if statement should not be entered, in theory, based on the way we increment i... but if so its ok (see skipped df)
    skipped <- skipped %>% append(i)
    if (is.na(full[i, "Category"] == FALSE )){
      full[i, "Category"] <- "skipped" #error check
    }
    i <- i+1
  } 
  else { #it is a question row.
    #endInd is the upper bound of our subset. We know the number of rows that should be in this subset due to the Answers column.
    endInd <- i + as.numeric(full$Answers[i])
    
    #qSubset is the dataframe from the given question that cuts off before the next question
    qSubset <- full[i : endInd, ]
    
    #Then we call CategorizeQ function on qSubset to determine the category
    full[i : endInd, "Category"] <- CategorizeQ(qSubset) 
    full[i : endInd, "Question"] <- full[i, "Type"]
    full[i : endInd, "OfferSuit"] <- OfferKind(qSubset, "suit")
    full[i : endInd, "OfferType"] <- OfferKind(qSubset, "type")
    full[i : endInd, "OfferNeither"] <- OfferKind(qSubset, "neither")
    
    #instead of incrementing i by 1 and visiting each row individually (as in a for loop),
    #we increment i by endInd, so as to hit the next question row (thus, a while loop, as i is not incremented by a constant)
    i <- endInd + 1
  }
}
###
View(full)

temp <-full


#TODO - no_spec? confed ask

#error management (checking for issues - not sure why these rows were skipped, but it wouldn't be an issue for final categorization.)
# View(full[skipped, ])
# View_trials(full[skipped, ])


###___BY QUESTION -> BY TRIAL___##
qRows <- full[grep("spec\\?|any_type\\?|any_suit\\?|no_spec|no_type|no_suit", full$Type), ] #rows with question (not including what_suit?) - there is one per qSubset
qRows <- qRows %>% filter (qRows$TierName == "RightIndividualSpeech")  #exclude helper_ask rows!!
trial_categories <- qRows %>% select(Filename, HandNb, TrialNb, Question, Category, OfferSuit, OfferType, OfferNeither)
View(trial_categories)

## MULTI QUESTION ##
#DUPLICATE trials - many categories per question? How to handle???
tCat <- qRows %>% select(Filename, HandNb, TrialNb)
multiQ_trials <- tCat[duplicated(tCat), ]
#View_trials(multiQ_trials)
#these are the trials with multiple questions that were categorized separately. 
#We want to just come up with a single catagorization per trial

#View_trials(multiQ_trials)


##TODO - double check #
Categorized_multiQ_trials <- function(mq_trials){
  for (i in 1:nrow(mq_trials)){
    tSubset <- TrialsOf(mq_trials[i, ])
    mq_trials[i, "Category"] <- CategorizeQ(tSubset)
    mq_trials[i, "OfferSuit"] <- TRUE %in% tSubset$OfferSuit
    mq_trials[i, "OfferType"] <-  TRUE %in% tSubset$OfferType
    mq_trials[i, "OfferNeither"] <- TRUE %in% tSubset$OfferNeither
  }
  return(mq_trials)
}

#re-categorized (some incorrect)
c_multiQ_trials <- Categorized_multiQ_trials(multiQ_trials)


#Final categorization! Coalescing double question trials into the correct category..
#updated full.
full <- left_join(full, c_multiQ_trials, by = c('Filename', 'TrialNb', 'HandNb')) %>%
  mutate(Category = ifelse(is.na(Category.y), Category.x, Category.y),
         OfferSuit = ifelse(is.na(OfferSuit.y), OfferSuit.x, OfferSuit.y),
         OfferType = ifelse(is.na(OfferType.y), OfferType.x, OfferType.y),
         OfferNeither = ifelse(is.na(OfferNeither.y), OfferNeither.x, OfferNeither.y)) %>%
  select(-Category.x, -Category.y, -OfferSuit.x, -OfferSuit.y, -OfferType.x, -OfferType.y, -OfferNeither.x, -OfferNeither.y)
View(full)

#View_trials(c_multiQ_trials)

#This function consolidates a df such that each trial is only one row
ByTrial <- function(dataframe) {
  dataframe %>% 
    select(Filename, TrialNb, HandNb, Question, Category, OfferSuit, OfferType, OfferNeither) %>%
    distinct()
}

categories_by_trial <- ByTrial(full)
#View(categories_by_trial)

# 
# categories_by_trial <- full %>% 
#   select(Filename, TrialNb, HandNb, Question, Category) %>%
#   distinct()



#Remove helper ask trials
categories_by_trial <- categories_by_trial[categories_by_trial$Category != "helper_ask", ]
View(categories_by_trial)
#Final categorization ^^


alternating <- categories_by_trial %>% 
  subset(grepl("alter", categories_by_trial$Filename))

blocked <- categories_by_trial %>%
  subset(grepl("block", categories_by_trial$Filename))



#Tertile
alternating <- alternating %>% 
  mutate(Tertile = ifelse(HandNb<=5*2, 1, ifelse(HandNb<=10*2, 2, 3)))

blocked <- blocked %>% 
  mutate(Tertile = ifelse(HandNb<=5, 1, ifelse(HandNb<=10, 2, 3)))

View(alternating)
View(blocked)

alt_spec <- alternating[grepl("spec", alternating$Question), ]
blk_spec <- blocked[grepl("spec", blocked$Question), ]



TertileNb <- c(1, 2, 3)


##### SPEC TYPES OF OFFER BREAKDOWN (AFFIRMATIVE AND NEGATIVE)#####
offerPattern <- data.frame(TertileNb)
for (i in 1:3){
  alt_tertile <- alt_spec[alt_spec$Tertile == i, ]
  
  alt_SuitNb <- nrow(alt_tertile[alt_tertile$OfferSuit == TRUE, ])
  alt_TypeNb <- nrow(alt_tertile[alt_tertile$OfferType == TRUE, ])
  alt_NeitherNb <- nrow(alt_tertile[alt_tertile$OfferNeither == TRUE, ])
  alt_totalOffers <- alt_SuitNb + alt_TypeNb + alt_NeitherNb
  
  offerPattern[i, "alt_OfferSuit"] <- alt_SuitNb / alt_totalOffers
  offerPattern[i, "alt_OfferType"] <- alt_TypeNb / alt_totalOffers
  offerPattern[i, "alt_OfferNeither"] <- alt_NeitherNb / alt_totalOffers
  offerPattern[i, "alt_NbOffers"] <- alt_totalOffers
  
  blk_tertile <- blk_spec[blk_spec$Tertile == i, ]
  
  blk_SuitNb <- nrow(blk_tertile[blk_tertile$OfferSuit == TRUE, ])
  blk_TypeNb <- nrow(blk_tertile[blk_tertile$OfferType == TRUE, ])
  blk_NeitherNb <-nrow(blk_tertile[blk_tertile$OfferNeither == TRUE, ])
  blk_totalOffers <- blk_SuitNb + blk_TypeNb + blk_NeitherNb
  
  offerPattern[i, "blk_OfferSuit"] <- blk_SuitNb / blk_totalOffers
  offerPattern[i, "blk_OfferType"] <- blk_TypeNb / blk_totalOffers
  offerPattern[i, "blk_OfferNeither"] <- blk_NeitherNb / blk_totalOffers
  offerPattern[i, "blk_NbOffers"] <- blk_totalOffers
}

View(offerPattern)
write.csv(offerPattern, "/Users/sydneyliu/Desktop/purmTest/OfferBreakdown_allSpec.csv")

##### SPEC TYPES OF OFFER BREAKDOWN (ONLY AFFIRMATIVE) #####
percent_affirmSpec_offers <- nrow(affirmativeOnly(alt_spec)) / nrow(alt_spec)
print(percent_affirmSpec_offers)

View_trials(alt_spec %>% affirmativeOnly())

affirmativeOnly <- function(df){
  df %>% filter(grepl("no_", df$Category) == FALSE)
}

TertileNb <- c(1, 2, 3)
offerPatternAffirmSpec <- data.frame(TertileNb)


for (i in 1:3){
  alt_tertile <- alt_spec[alt_spec$Tertile == i, ]
  
  alt_SuitNb <- nrow(alt_tertile[alt_tertile$OfferSuit == TRUE, ] %>% affirmativeOnly())
  alt_TypeNb <- nrow(alt_tertile[alt_tertile$OfferType == TRUE, ] %>% affirmativeOnly())
  alt_NeitherNb <- nrow(alt_tertile[alt_tertile$OfferNeither == TRUE, ] %>% affirmativeOnly())
  alt_totalOffers <- alt_SuitNb + alt_TypeNb + alt_NeitherNb
  
  offerPatternAffirmSpec[i, "alt_OfferSuit"] <- alt_SuitNb / alt_totalOffers
  offerPatternAffirmSpec[i, "alt_OfferType"] <- alt_TypeNb / alt_totalOffers
  offerPatternAffirmSpec[i, "alt_OfferNeither"] <- alt_NeitherNb / alt_totalOffers
  offerPatternAffirmSpec[i, "alt_NbOffers"] <- alt_totalOffers
  
  blk_tertile <- blk_spec[blk_spec$Tertile == i, ]
  
  blk_SuitNb <- nrow(blk_tertile[blk_tertile$OfferSuit == TRUE, ]  %>% affirmativeOnly())
  blk_TypeNb <- nrow(blk_tertile[blk_tertile$OfferType == TRUE, ]  %>% affirmativeOnly())
  blk_NeitherNb <-nrow(blk_tertile[blk_tertile$OfferNeither == TRUE, ]  %>% affirmativeOnly())
  blk_totalOffers <- blk_SuitNb + blk_TypeNb + blk_NeitherNb
  
  offerPatternAffirmSpec[i, "blk_OfferSuit"] <- blk_SuitNb / blk_totalOffers
  offerPatternAffirmSpec[i, "blk_OfferType"] <- blk_TypeNb / blk_totalOffers
  offerPatternAffirmSpec[i, "blk_OfferNeither"] <- blk_NeitherNb / blk_totalOffers
  offerPatternAffirmSpec[i, "blk_NbOffers"] <- blk_totalOffers
}


View(offerPatternAffirmSpec)
write.csv(offerPatternAffirmSpec, "/Users/sydneyliu/Desktop/purmTest/OfferBreakdown_yesSpec.csv")


####

cbt <- categories_by_trial
cbt <- cbt %>% 
  mutate(hasOffer = ifelse(is.na(OfferSuit), OfferNeither, OfferSuit | OfferType | OfferNeither))
View(cbt)

alt_cbt <- cbt[grepl("alter", cbt$Filename), ] %>% 
  mutate(Tertile = ifelse(HandNb<=5*2, 1, ifelse(HandNb<=10*2, 2, 3)))

blk_cbt <- cbt[grepl("block", cbt$Filename), ] %>% 
  mutate(Tertile = ifelse(HandNb<=5, 1, ifelse(HandNb<=10, 2, 3)))

View(alt_cbt)
View(blk_cbt)

##### NUMBER OF OFFER TRIALS BREAKDOWN (ALL)#####
NbOffers <- data.frame(TertileNb)

for (i in 1:3){
  alt_tertile_offer <- alt_cbt[alt_cbt$Tertile == i, ] %>%
    filter(hasOffer)
  
  blk_tertile_offer <- blk_cbt[blk_cbt$Tertile == i, ] %>%
    filter(hasOffer)
  
  a_spec <- alt_tertile_offer[grepl("spec", alt_tertile_offer$Question), ] %>% nrow() 
  a_type <- alt_tertile_offer[grepl("type", alt_tertile_offer$Question), ] %>% nrow()
  a_suit <- alt_tertile_offer[grepl("suit", alt_tertile_offer$Question), ] %>% nrow()
  a_offers <- nrow(alt_tertile_offer)
  
  NbOffers[i, "Alt_spec"] <- a_spec / a_offers
  NbOffers[i, "Alt_type"] <- a_type / a_offers
  NbOffers[i, "Alt_suit"] <- a_suit / a_offers
  NbOffers[i, "Alt_total_offer"] <- a_offers
  
  b_spec <- blk_tertile_offer[grepl("spec", blk_tertile_offer$Question), ] %>% nrow()
  b_type <- blk_tertile_offer[grepl("type", blk_tertile_offer$Question), ] %>% nrow()
  b_suit <- blk_tertile_offer[grepl("suit", blk_tertile_offer$Question), ] %>% nrow()
  b_offers <- nrow(blk_tertile_offer)
  
  NbOffers[i, "Blk_spec"] <- b_spec / b_offers
  NbOffers[i, "Blk_type"] <- b_type / b_offers
  NbOffers[i, "Blk_suit"] <- b_suit / b_offers
  NbOffers[i, "Blk_total_offer"] <- b_offers
}
View(NbOffers)

write.csv(NbOffers, "/Users/sydneyliu/Desktop/purmTest/allOfferNb.csv")

##### NUMBER OF OFFER TRIALS BREAKDOWN (AFFIRMATIVE ONLY)#####
NbOffersAffirmative <- data.frame(TertileNb)
for (i in 1:3){
  alt_tertile_offer <- alt_cbt[alt_cbt$Tertile == i, ] %>%
    filter(hasOffer) %>%
    affirmativeOnly()
  
  blk_tertile_offer <- blk_cbt[blk_cbt$Tertile == i, ] %>%
    filter(hasOffer) %>%
    affirmativeOnly()
  
  a_spec <- alt_tertile_offer[grepl("spec", alt_tertile_offer$Question), ] %>% nrow() 
  a_type <- alt_tertile_offer[grepl("type", alt_tertile_offer$Question), ] %>% nrow()
  a_suit <- alt_tertile_offer[grepl("suit", alt_tertile_offer$Question), ] %>% nrow()
  a_offers <- nrow(alt_tertile_offer)
  
  NbOffersAffirmative[i, "Alt_spec"] <- a_spec / a_offers
  NbOffersAffirmative[i, "Alt_type"] <- a_type / a_offers
  NbOffersAffirmative[i, "Alt_suit"] <- a_suit / a_offers
  NbOffersAffirmative[i, "Alt_total_offer"] <- a_offers
  
  b_spec <- blk_tertile_offer[grepl("spec", blk_tertile_offer$Question), ] %>% nrow()
  b_type <- blk_tertile_offer[grepl("type", blk_tertile_offer$Question), ] %>% nrow()
  b_suit <- blk_tertile_offer[grepl("suit", blk_tertile_offer$Question), ] %>% nrow()
  b_offers <- nrow(blk_tertile_offer)
  
  NbOffersAffirmative[i, "Blk_spec"] <- b_spec / b_offers
  NbOffersAffirmative[i, "Blk_type"] <- b_type / b_offers
  NbOffersAffirmative[i, "Blk_suit"] <- b_suit / b_offers
  NbOffersAffirmative[i, "Blk_total_offer"] <- b_offers
}
View(NbOffersAffirmative)

write.csv(NbOffersAffirmative, "/Users/sydneyliu/Desktop/purmTest/affirmativeOfferNb.csv")


##### NUMBER OF OFFER TRIALS BREAKDOWN (NEGATIVE ONLY)#####
NbOffersNegative <- data.frame(TertileNb)
for (i in 1:3){
  alt_tertile_offer <- alt_cbt[alt_cbt$Tertile == i, ] %>%
    filter(hasOffer) 
  
  alt_tertile_offer <-  alt_tertile_offer %>% filter(grepl("no_", alt_tertile_offer$Category))
  
  blk_tertile_offer <- blk_cbt[blk_cbt$Tertile == i, ] %>%
    filter(hasOffer)
  
  blk_tertile_offer <-  blk_tertile_offer %>% filter(grepl("no_", blk_tertile_offer$Category))
  
  a_spec <- alt_tertile_offer[grepl("spec", alt_tertile_offer$Question), ] %>% nrow() 
  a_type <- alt_tertile_offer[grepl("type", alt_tertile_offer$Question), ] %>% nrow()
  a_suit <- alt_tertile_offer[grepl("suit", alt_tertile_offer$Question), ] %>% nrow()
  a_offers <- nrow(alt_tertile_offer)
  
  NbOffersNegative[i, "Alt_spec"] <- a_spec / a_offers
  NbOffersNegative[i, "Alt_type"] <- a_type / a_offers
  NbOffersNegative[i, "Alt_suit"] <- a_suit / a_offers
  NbOffersNegative[i, "Alt_total_offer"] <- a_offers
  
  b_spec <- blk_tertile_offer[grepl("spec", blk_tertile_offer$Question), ] %>% nrow()
  b_type <- blk_tertile_offer[grepl("type", blk_tertile_offer$Question), ] %>% nrow()
  b_suit <- blk_tertile_offer[grepl("suit", blk_tertile_offer$Question), ] %>% nrow()
  b_offers <- nrow(blk_tertile_offer)
  
  NbOffersNegative[i, "Blk_spec"] <- b_spec / b_offers
  NbOffersNegative[i, "Blk_type"] <- b_type / b_offers
  NbOffersNegative[i, "Blk_suit"] <- b_suit / b_offers
  NbOffersNegative[i, "Blk_total_offer"] <- b_offers
}
View(NbOffersNegative)

write.csv(NbOffersNegative, "/Users/sydneyliu/Desktop/purmTest/negativeOfferNb.csv")

