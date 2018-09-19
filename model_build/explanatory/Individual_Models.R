library(tidyverse)

############################################################################
############## MANUAL ANALYSIS
############################################################################

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_w_ints.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("frequency|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)


test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("baseline|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)


test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("channel|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)


test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("device|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)


test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("medium|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)


test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("prst|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)


test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("rolling|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)


test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("year|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)


test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("month|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)


test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("day_time|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)


test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("creative_size|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)


test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("recency|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)


test_model <- glm(answer_desired_Aided_awareness~., data=df[grep("total|grp_EXP|awareness", names(df))], family='binomial')
summary(test_model)



form <- formula(
  answer_desired_Aided_awareness ~
      grp_EXP 
    + prst_GDN
    + rolling_imps_3.0
    + year_2018
    + month_2
    + month_6
    + day_time_AM
    + day_time_PM
    + creative_size_970x66
    + prst_GDN_int
    + rolling_imps_3.0_int
    + year_2018_int
    + month_2_int
    + month_6_int
    + day_time_AM_int
    + day_time_PM_int
    + creative_size_970x66_int
)

final_model <- glm(form, data=df, family='binomial')
summary(final_model)





