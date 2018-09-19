library(tidyverse)
library(uplift)

############################################################################
#### BIDIRECTIONAL STEPWISE MODELS - INTS ONLY
############################################################################

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_ints_only.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

null_model <- glm(answer_desired_Aided_awareness~grp_EXP, data=df, family='binomial')
full_model <- glm(answer_desired_Aided_awareness~., data=df, family='binomial')
summary(full_model)

step_model <- step(null_model, scope=list(lower=null_model, upper=full_model), direction='both')
summary(step_model)

form <- update(formula(step_model),
               ~  .
               
          +     baseline
          +     prst_GDN
          +     recency_0
          +     weeks_elapsed_12
          +     prst_FUSION_MEDIA_GROUP
          +     prst_SAMBA
          +     weeks_elapsed_3
          +     frequency
          +     creative_size_06
          +     weeks_elapsed_5
          +     creative_size_970x66
          +     prst_VOX_MEDIA
          +     day_time_Late_Night
          +     device_name_Mobile
          +     medium_name_Hybrid
          +     recency_7
          +     recency_1
          +     creative_size_300x600
          +     day_time_PM
               
)

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_w_ints.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

revised_model <- glm(form, data=df, family='binomial')
summary(revised_model)

############################################################################
#### FORWARD STEPWISE MODELS - INTS ONLY
############################################################################

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_ints_only.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

null_model <- glm(answer_desired_Aided_awareness~1, data=df, family='binomial')
full_model <- glm(answer_desired_Aided_awareness~., data=df, family='binomial')
summary(full_model)

step_model <- step(null_model, scope=list(lower=null_model, upper=full_model), direction='forward')
summary(step_model)

form <- update(formula(step_model),
               ~ .
              + prst_GDN
              + recency_0
              + prst_FUSION_MEDIA_GROUP
              + creative_size_06
              + frequency
              + prst_SAMBA
              + weeks_elapsed_13.0
              + weeks_elapsed_3.0
              + baseline
              + creative_size_970x66
              + weeks_elapsed_5.0
              + device_name_Mobile
              + medium_name_Hybrid
              + day_time_Late_Night
              + recency_4
              + creative_size_300x600
              + weeks_elapsed_9.0
              + day_time_PM
              + recency_7
              + prst_VOX_MEDIA
)

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_w_ints.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

revised_model <- glm(form, data=df, family='binomial')
summary(revised_model)


############################################################################
#### BACKWARD STEPWISE MODELS
############################################################################

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_ints_only.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

null_model <- glm(answer_desired_Aided_awareness~grp_EXP, data=df, family='binomial')
full_model <- glm(answer_desired_Aided_awareness~., data=df, family='binomial')
summary(full_model)

step_model <- step(full_model, scope=list(lower=null_model, upper=full_model), direction='backward')
summary(step_model)


form <- update(formula(step_model),
               ~ . 
           +    frequency
           +    baseline
           +    channel_name_Video
          +     device_name_Mobile
          +     medium_name_Hybrid
          +     prst_ABC
          +     prst_CBS_SPORTS
          +     prst_CNET
          +     prst_FUSION_MEDIA_GROUP
          +     prst_FUSION.NET
          +     prst_GDN
          +     prst_NBC
          +     prst_SAMBA
          +     prst_VOX_MEDIA
          +     weeks_elapsed_7.0
          +     weeks_elapsed_8.0
          +     weeks_elapsed_9.0
          +     weeks_elapsed_13.0
          +     day_time_Late_Night
          +     day_time_PM
          +     creative_size_06
          +     creative_size_15
          +     creative_size_970x250
          +     recency_0
          +     recency_4
               
)

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_w_ints.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

revised_model <- glm(form, data=df, family='binomial')
summary(revised_model)

