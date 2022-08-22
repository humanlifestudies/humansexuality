library(tidyverse)
library(googlesheets4)
gs4_deauth()



get_raw_dataset<- function(sheet_url=NA){
  if(is.na(sheet_url)){
    sheet_url<-config::get()$sheet_url
  }
  raw_dataset<-googlesheets4::read_sheet(sheet_url,range="A:AO",guess_max=3300)
  table(c(raw_dataset$`Which gender identity below do you most identify with?`,
          raw_dataset$`To the best of your knowledge, which gender identity below does your partner most identify with?...24`))
  
  #map gender
  #To the best of your knowledge, which gender identity below does your partner most identify with?
  #To the best of your knowledge, which gender identity below does your partner most identify with?
  raw_dataset$RespondentGender3C <- get_3c_gender(paste0(raw_dataset$`Which gender identity below do you most identify with?`))
  raw_dataset$PartnerGender3C <- get_3c_gender(raw_dataset$`To the best of your knowledge, which gender identity below does your partner most identify with?...24`)
  
  raw_dataset$RespondentGender6C <- get_6c_gender(paste0(raw_dataset$`Which gender identity below do you most identify with?`))
  raw_dataset$PartnerGender6C <- get_6c_gender(raw_dataset$`To the best of your knowledge, which gender identity below does your partner most identify with?...24`)
  
  table(raw_dataset$RespondentGender6C)
  table(raw_dataset$PartnerGender6C)
  
  
  table(c(raw_dataset$PartnerGender3C,raw_dataset$RespondentGender3C))
  raw_dataset$HeteroRShip <- ((raw_dataset$RespondentGender3C=="Male" & raw_dataset$PartnerGender3C=="Female") | 
                                (raw_dataset$RespondentGender3C=="Female" & raw_dataset$PartnerGender3C=="Male"))
  
  raw_dataset$SameSex <- raw_dataset$RespondentGender3C==raw_dataset$PartnerGender3C
  
  
  return(raw_dataset)
  
}

#creates an ordered vector with ordering by frequency
factors_by_frequency<-function(vec){
  vec<-as.character(vec)
  sorted_order <- sort(table(vec),decreasing = TRUE)
  vec_as_factor<-factor(vec,levels=names(sorted_order))
  return(vec_as_factor)
}
create_straight_couple_data <- function(data_per_participant){
  #easier to user data_per_participant than go from raw, because it's pre-processed
  
  #grab the female partners
  
  female_partners<-data_per_participant %>% filter(Gender3C=="Female" & HeteroRShip==TRUE)
  male_respondents <- data_per_participant %>% filter(Gender3C=="Male") # we will filter by their match to their female partner
  #now merge on their male counterparts
  straight_couple_data<-merge(female_partners,male_respondents,
        by.x = "ResponseId",by.y="ResponseId",
        all.x = TRUE, all.y=FALSE,
        suffixes = c("Female","Male"))
  
  return(straight_couple_data)
  
}
#create respondent data

create_participant_data <- function(raw_dataset){
  raw_dataset$ResponseId<-1:nrow(raw_dataset)
  ## this creates a "doubled" dataset where each row represents one person, either hte respondent or their partner.
  code_respondent_country<-'Please select the country you currently live in (for this and following "country" selections, you can start typing your country\'s name to find it faster)'
  code_partner_country <- "Select the country your partner lives in, if different from the one you live in currently"
  respondent_data <- raw_dataset[,c("ResponseId",
    "Timestamp","What kind of relationship are you in? (If you are polyamorous, please pick one partner in particular and respond to the survey about your relationship with them)",
    "RespondentGender3C","PartnerGender3C","My current age","My partner's current age",
    "How do you currently keep your pubic hair?","How does your partner currently keep their pubic hair?",
    "Please check all pubic hair styles you’ve had over the last 12 months","Please check all pubic hair styles your partner has had over the last 12 months",
    "How would your partner ideally like you to keep your pubic hair?",
    "In general, what pubic hair styling do you most like in people you are sexually intimate with?",
    "What would best describe your attitude and behavior during sex?","What would best describe your partner's attitude and behavior during sex?",
    "If you had to place yourself on a political spectrum, where would you be?","If you had to place YOUR PARTNER on a political spectrum, where would THEY be?",
    code_respondent_country,
    "Which of the following sexual orientations do you most identify with?",
    "HeteroRShip","SameSex","How did you find out about this survey?")]
  
  respondent_data$SurveyRole<-"PrimaryRespondent"
  
  partner_data <- raw_dataset[,c("ResponseId",
    "Timestamp","What kind of relationship are you in? (If you are polyamorous, please pick one partner in particular and respond to the survey about your relationship with them)",
    "PartnerGender3C","RespondentGender3C","My partner's current age","My current age",
    "How does your partner currently keep their pubic hair?","How do you currently keep your pubic hair?",
    "Please check all pubic hair styles your partner has had over the last 12 months","Please check all pubic hair styles you’ve had over the last 12 months",
    "How would you ideally like your partner to keep their pubic hair?", 
    "In general, what pubic hair styling does your partner most like in people they are sexually intimate with?",
    "What would best describe your partner's attitude and behavior during sex?","What would best describe your attitude and behavior during sex?",
    "If you had to place YOUR PARTNER on a political spectrum, where would THEY be?","If you had to place yourself on a political spectrum, where would you be?",
    code_partner_country,
    "Which of the following sexual orientations does your partner most identify with (to the best of your knowledge)?",
    "HeteroRShip","SameSex","How did you find out about this survey?")]
  
  partner_data$SurveyRole<-"RespondentPartner"
  
  partner_data[is.na(partner_data[,code_partner_country]), code_partner_country]<-raw_dataset[is.na(partner_data[,code_partner_country]),code_respondent_country]
  
  new_table_colnames <- c("ResponseId","Timestamp","Relationship type","Gender3C","PartnerGender3C","Age","PartnerAge",
                          "How do you currently keep your pubic hair?","PartnerPubicHairCurrently",
                          "PubicHair12Months","PartnerPubicHair12Months",
                          "Partner's preference","PubicHairGeneralPreferenceRaw",
                          "SexTendencyRaw","PartnerSexTendencyRaw",
                          "Politics","PartnerPolitics", "Country",
                          "SexualOrientation", "HeteroRShip","SameSex",
                          "How did you find out about this survey?",
                          "SurveyRole")
  colnames(respondent_data) <- new_table_colnames
  colnames(partner_data) <- new_table_colnames
  data_per_participant <- rbind(
    respondent_data,
    partner_data
  )
  
  hair_style_substitutes <- list(
    "Natural - not shaved or trimmed at all in the last month or more" = "1. Natural",
    "Trimmed short but not shaved or waxed" = "2. Trimmed",
    "Partially removed (e.g., landing strip, triangle, etc)"="3. Partially removed",
    "Fully removed (shaved, waxed, or other method)"="4. Fully removed"
  )
  
  
  data_per_participant$PubicHairGeneralPreference<-data_per_participant$PubicHairGeneralPreferenceRaw
  
  for (orig_name in names(hair_style_substitutes)){
    data_per_participant$`How do you currently keep your pubic hair?`[data_per_participant$`How do you currently keep your pubic hair?`==orig_name]<-hair_style_substitutes[[orig_name]]
    data_per_participant$`Partner's preference`[data_per_participant$`Partner's preference`==orig_name]<-hair_style_substitutes[[orig_name]]
    data_per_participant$PubicHairGeneralPreference[data_per_participant$PubicHairGeneralPreferenceRaw==orig_name]<-hair_style_substitutes[[orig_name]]
    data_per_participant$PartnerPubicHairCurrently[data_per_participant$PartnerPubicHairCurrently==orig_name]<-hair_style_substitutes[[orig_name]]
  }
  
  
  data_per_participant$`How do you currently keep your pubic hair?` <- factor(
    data_per_participant$`How do you currently keep your pubic hair?`,levels=hair_style_substitutes
  )
  
  data_per_participant$PubicHairGeneralPreferenceNum <- factor(
    data_per_participant$PubicHairGeneralPreference,levels=hair_style_substitutes
  )
  
  table(data_per_participant$`How do you currently keep your pubic hair?`)
  table(data_per_participant$`Partner's preference`)
  
  #deal with the question about most pubic hair removed in last 12 months
  get_most_removed_from_style_list <- function(style_list_vec){
    most_trimmed_12m<-factor(rep(NA,length(style_list_vec)),levels=hair_style_substitutes)
    for (style_id in 1:4){
      #because hair_style_substitutes are in ascending order of hair removal
      #the MOST hair removed is whatever level matches LAST
      most_trimmed_12m[grepl(names(hair_style_substitutes)[[style_id]],style_list_vec,fixed=TRUE)]<-hair_style_substitutes[[style_id]]
    }
    return(most_trimmed_12m)
  }
  data_per_participant$MostHairRemoved12Months <- get_most_removed_from_style_list(data_per_participant$PubicHair12Months)
  data_per_participant$PartnerMostHairRemoved12Months <- get_most_removed_from_style_list(data_per_participant$PartnerPubicHair12Months)
  
  #preferences
  
  data_per_participant$PubicHairGeneralPreference[data_per_participant$PubicHairGeneralPreference=="I don't know what they like most"]<-NA
  data_per_participant$PubicHairGeneralPreference[data_per_participant$PubicHairGeneralPreference %in% c("I don't care","They don't care")]<-"Don't care"
  data_per_participant$PubicHairGeneralPreference[data_per_participant$PubicHairGeneralPreference %in% c("I don't care","They don't care")]<-"Don't care"
  
  
  #relationship type
  
  relationship_shorthand <- list(
    "Committed relationship, live apart" = "Committed relationship",
    "Committed relationship, live together, but not married" = "Living together",
    "Married or other legally recognized relationship, e.g., a civil partnership" = "Married",
    "Dating"="Dating",
    "FWB/casual" = "Casual",
    "Single"="Single"
  )
  
  for (orig_name in names(relationship_shorthand)){
    data_per_participant$`Relationship type`[data_per_participant$`Relationship type`==orig_name]<-relationship_shorthand[[orig_name]]
  }
  data_per_participant$`Relationship type` <- factor(
    data_per_participant$`Relationship type`,levels=c("Married","Living together","Committed relationship","Dating","Casual","Single")
  )
  data_per_participant$RTypeNumericC<-as.numeric(data_per_participant$`Relationship type`)-median(as.numeric(data_per_participant$`Relationship type`),na.rm=TRUE)
  data_per_participant$RTypeLivingTogetherVs<-factor(
    data_per_participant$`Relationship type`,levels=c("Living together","Married","Committed relationship","Dating","Casual","Single")
  )
  

  
  ## sex tendency
  process_sex_tendency <- function(SexTendencyRaw){
    SexTendencyRaw2<-SexTendencyRaw
    SexTendencyRaw2[SexTendencyRaw %in% c("Switch between dominant and submissive about equally","Not dominant or submissive")]<-"Neutral"
    SexDominance<-factor(
      SexTendencyRaw2,
      levels=c("Very submissive","Slightly submissive","Neutral","Slightly dominant","Very dominant"))
    return(SexDominance)
  }
  
  data_per_participant$SexDominance<-process_sex_tendency(data_per_participant$SexTendencyRaw)
  data_per_participant$NotDomSub<-data_per_participant$SexTendencyRaw=="Not dominant or submissive"
  data_per_participant$SexDominanceNum<-as.numeric(data_per_participant$SexDominance)-3
  
  data_per_participant$PartnerSexDominance<-process_sex_tendency(data_per_participant$PartnerSexTendencyRaw)
  data_per_participant$PartnerNotDomSub<-data_per_participant$PartnerSexTendencyRaw=="Not dominant or submissive"
  data_per_participant$PartnerSexDominanceNum<-as.numeric(data_per_participant$PartnerSexDominance)-3
  
  #print(data_per_participant$Gender[data_per_participant$Gender %in% c(gender_binaries, non_binary_gender_names)  == FALSE])
  
  #data_per_participant$Gender[data_per_participant$Gender %in% non_binary_gender_names] <- "Non-binary"
  #data_per_participant$Gender[(data_per_participant$Gender %in% c(gender_binaries,non_binary_gender_names))==FALSE]<-NA
  #almost there. now we should be able to classify ahir styles by gender
  
  
  data_per_participant$`How do you currently keep your pubic hair?`<-factor(
    data_per_participant$`How do you currently keep your pubic hair?`,
    hair_style_substitutes
  )
  
  
  politics_levels <- c("Very conservative or right-wing","Mainstream conservative or right-wing","Lean conservative or right-wing","Centrist","Lean liberal or left-wing","Mainstream liberal or left-wing","Very liberal or left-wing")
  data_per_participant$Politics<-factor(data_per_participant$Politics,levels=politics_levels)
  data_per_participant$PartnerPolitics<-factor(data_per_participant$PartnerPolitics,levels=politics_levels)
  
  get_politics_5L<-function(politics_vec){
    politics_vec<-as.character(politics_vec)
    politics_vec[politics_vec %in% c("Very conservative or right-wing","Mainstream conservative or right-wing","Lean conservative or right-wing")]<-"Conservative or right-wing (any level)"
    politics_levels_5L<-c("Conservative or right-wing (any level)","Centrist","Lean liberal or left-wing","Mainstream liberal or left-wing","Very liberal or left-wing")
    politics_vec<-factor(politics_vec,politics_levels_5L)
    return(politics_vec)
  }
  get_politics_4L<-function(politics_vec){
    politics_vec<-as.character(politics_vec)
    politics_vec[politics_vec %in% c("Very conservative or right-wing","Mainstream conservative or right-wing","Lean conservative or right-wing","Centrist")]<-"Centrist or conservative/right-wing"
    politics_levels_4L<-c("Centrist or conservative/right-wing","Lean liberal or left-wing","Mainstream liberal or left-wing","Very liberal or left-wing")
    politics_vec<-factor(politics_vec,politics_levels_4L)
    return(politics_vec)
  }
  data_per_participant$Politics4L<-get_politics_4L(data_per_participant$Politics)
  data_per_participant$PartnerPolitics4L<-get_politics_4L(data_per_participant$PartnerPolitics)
  data_per_participant$Politics5L<-get_politics_5L(data_per_participant$Politics)
  data_per_participant$PartnerPolitics5L<-get_politics_5L(data_per_participant$PartnerPolitics)
  
  
  data_per_participant$SexualOrientation5C<-get_5c_so(data_per_participant$SexualOrientation)
  data_per_participant$SexualOrientation6C<-get_6c_so(data_per_participant$SexualOrientation)
  #now we will arbitrarily relabel female gay respondents as lesbians. sorry
  data_per_participant$SexualOrientation5C[data_per_participant$SexualOrientation5C=="Gay" & data_per_participant$Gender3C=="Female"]<-"Lesbian"
  data_per_participant$SexualOrientation6C[data_per_participant$SexualOrientation6C=="Gay" & data_per_participant$Gender3C=="Female"]<-"Lesbian"
  
  #some simplifications
  
  data_per_participant$IsInUSA<-data_per_participant$Country=="United States of America"
  data_per_participant$OutsideUSA<-data_per_participant$IsInUSA==FALSE
  data_per_participant$PoliticalLiberalism<-as.numeric(data_per_participant$Politics)-4
  data_per_participant$PartnerPoliticalLiberalism<-as.numeric(data_per_participant$PartnerPolitics)-4
  data_per_participant$AgeGap<-data_per_participant$Age-data_per_participant$PartnerAge
  data_per_participant$MarriedOrLivingTogether<-data_per_participant$`Relationship type` %in% c("Married","Living together")
  
  data_per_participant$`How did you find out about this survey?`<-factors_by_frequency(data_per_participant$`How did you find out about this survey?`)
  median_age <- median(data_per_participant$Age,na.rm = TRUE)
  print(paste0("median age: ",median_age))
  data_per_participant$AgeC<-data_per_participant$Age-median_age
  return(data_per_participant)
}

get_3c_gender <- function(raw_gender_col){
  gender_binaries <- c("Male","Female")
  non_binary_gender_names <- c("Non-binary","Non binary","Nonbinary","non binary","nonbinary","Non-binary","NB","Nb","Not","none", "Bigender, Bio F",
                               "Trans masculine nonbinary", "Genderqueer", "Genderfluid", "Gender fluid",
                               "Non-binary (AFAB and primarily feminine)","AFAB, but NonBinary.","Nonbinary cis female",
                               "Male and non-binary", "Agender","agender","Non-binary (agender)",
                               "Non-binary, genderfluid, or genderqueer (transmasc)", 
                               "Non-binary, genderfluid, or genderqueer (transfem)",
                               "Non-binary, genderfluid, or genderqueer (prefer not to further disclose)"
  )
  
  male_gender_codes <- c("Male","Male (cisgender man)", "Male (transgender man)","Trans male","Trans Man")
  female_gender_codes <-c("Female","Female (cisgender woman)","Female (transgender woman)","Female (Bisexual with preference for males)")
  
  
  raw_gender_col[raw_gender_col %in% non_binary_gender_names] <- "Non-binary"
  raw_gender_col[raw_gender_col %in% male_gender_codes] <- "Male"
  raw_gender_col[raw_gender_col %in% female_gender_codes] <- "Female"
  
  print(table(raw_gender_col))
  gender_3c<-c("Female","Male","Non-binary")
  raw_gender_col[(raw_gender_col %in% gender_3c)==FALSE]<-NA
  raw_gender_col<-factor(raw_gender_col,levels = gender_3c)
  return(raw_gender_col)
}

get_6c_gender <- function(raw_gender_col){
  gender_binaries <- c("Male","Female")
  
  non_binary_gender_names <- c("Non-binary","Non binary","Nonbinary","non binary","nonbinary","Non-binary","NB","Nb","Not","none", "Bigender, Bio F",
                               "Trans masculine nonbinary", "Genderqueer", "Genderfluid", "Gender fluid",
                               "Non-binary (AFAB and primarily feminine)","AFAB, but NonBinary.","Nonbinary cis female",
                               "Male and non-binary", "Agender","agender","Non-binary (agender)",
                               "Non-binary, genderfluid, or genderqueer (transmasc)", 
                               "Non-binary, genderfluid, or genderqueer (transfem)",
                               "Non-binary, genderfluid, or genderqueer (prefer not to further disclose)"
  )
  gender_map <- list(
    "Male"=c("Male","Male (cisgender man)", "Male (transgender man)","Trans male","Trans Man"),
    "Female"=c("Female","Female (cisgender woman)","Female (transgender woman)","Female (Bisexual with preference for males)"),
    "Non-binary (transmasc)"=c("Non-binary, genderfluid, or genderqueer (transmasc)","Trans masculine nonbinary"),
    "Non-binary (transfem)"=c("Non-binary, genderfluid, or genderqueer (transfem)"),
    "Non-binary (Not further specified)"=c(
      "Non-binary, genderfluid, or genderqueer (prefer not to further disclose)",
      "Non-binary","Non binary","Nonbinary","non binary","nonbinary","Non-binary","NB","Nb",
      "Genderqueer", "Genderfluid", "Gender fluid",
      "Bigender, Bio F","Non-binary (AFAB and primarily feminine)","AFAB, but NonBinary.","Nonbinary cis female"),
    "Agender or None"=c("Not","none","Agender","agender")
  )
  print(length(raw_gender_col))
  new_col<-as.character(rep(NA,length(raw_gender_col)))
  
  for (short_gender in names(gender_map)){
    print(gender_map[[short_gender]])
    new_col[raw_gender_col %in% gender_map[[short_gender]]] <- short_gender
  }
  
  new_col<-factor(new_col,levels = names(gender_map))
  return(new_col)
}

get_5c_so <- function(raw_col){
  category_map <- list(
    "Straight"=c("Straight/heterosexual"),
    "Lesbian"=c("Lesbian"),
    "Gay"=c("Gay"),
    "Bisexual or Pansexual"=c("Bisexual","Pansexual","Polysexual"),
    "Asexual"=c("Asexual","Aromantic", "Asexual Biromantic")
    #"Queer"=c("queer")#not enough responses
  )
  return(standardize_column(raw_col,category_map))
}


get_6c_so <- function(raw_col){
  category_map <- list(
    "Straight"=c("Straight/heterosexual"),
    "Lesbian"=c("Lesbian"),
    "Gay"=c("Gay"),
    "Bisexual"=c("Bisexual"),
    "Pansexual"=c("Pansexual"),#,"Polysexual"),  “Polysexual” can be a sort of “umbrella term” that encompasses bisexuality and pansexuality, so fits into neither category in this schema
    #https://www.healthline.com/health/bisexual-vs-pansexual
    "Asexual"=c("Asexual","Aromantic", "Asexual Biromantic")
    #"Queer"=c("queer")#not enough responses
  )
  return(standardize_column(raw_col,category_map))
}

standardize_column<- function(raw_col,category_map){
  
  
  new_col<-as.character(rep(NA,length(raw_col)))
  
  for (short_name in names(category_map)){
    print(category_map[[short_name]])
    new_col[raw_col %in% category_map[[short_name]]] <- short_name
  }
  
  new_col<-factor(new_col,levels = names(category_map))
  return(new_col)
}



get_pattern_named_vec<-function(image_key,image_folder){
  image_key_df <- read_csv("image_key.csv")
  image_key_df$key_interaction<-interaction(image_key_df$`Hair style`,image_key_df$Gender3C)
  image_key_named_vec<-paste0(image_folder,image_key_df$Filename)
  names(image_key_named_vec)<-image_key_df$key_interaction
  return(image_key_named_vec)
}