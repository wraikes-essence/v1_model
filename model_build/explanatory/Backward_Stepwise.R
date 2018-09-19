library(tidyverse)


############################################################################
#### BACKWARD STEPWISE MODELS
############################################################################

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_w_ints.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

null_model <- glm(answer_desired_Aided_awareness~grp_EXP, data=df, family='binomial')
full_model <- glm(answer_desired_Aided_awareness~., data=df, family='binomial')
summary(full_model)

step_model <- step(full_model, scope=list(lower=null_model, upper=full_model), direction='backward')
summary(step_model)

form <- formula(
  answer_desired_Aided_awareness ~
    grp_EXP
  
+  prst_ABC
+  prst_GDN
+  prst_NBC
+  prst_SAMBA
+  prst_VOX_MEDIA
+  weeks_elapsed_1.0
+  weeks_elapsed_4.0
+  weeks_elapsed_7.0
+  weeks_elapsed_8.0
+  weeks_elapsed_9.0
+  weeks_elapsed_13.0
+  day_time_Late_Night
+  day_time_Noon
+  day_time_PM
+  creative_size_970x250
+  creative_size_970x66
+  recency_4

+  prst_ABC_int
+  prst_GDN_int
+  prst_NBC_int
+  prst_SAMBA_int
+  prst_VOX_MEDIA_int
+  weeks_elapsed_1.0_int
+  weeks_elapsed_4.0_int
+  weeks_elapsed_7.0_int
+  weeks_elapsed_8.0_int
+  weeks_elapsed_9.0_int
+  weeks_elapsed_13.0_int
+  day_time_Late_Night_int
+  day_time_Noon_int
+  day_time_PM_int
+  creative_size_970x250_int
+  creative_size_970x66_int
+  recency_4_int
)

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_w_ints.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

revised_model <- glm(form, data=df, family='binomial')
summary(revised_model)

############################################################################
############################################################################
############################################################################

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_ints_only.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

null_model <- glm(answer_desired_Aided_awareness~1, data=df, family='binomial')
full_model <- glm(answer_desired_Aided_awareness~., data=df, family='binomial')
summary(full_model)

step_model <- step(null_model, scope=list(lower=null_model, upper=full_model), direction='both')
summary(step_model)

form <- update(formula(step_model),
               ~ .
               +      prst_GDN         
               +        month_2             
               +         prst_SAMBA            
               +     frequency       
               +      baseline               
               +     creative_size_15      
               +           creative_size_970x66  
               +        day_time_Late_Night      
               +         recency_7       
               +        device_name_Mobile     
               +       creative_size_300x250   
               +      medium_name_Hybrid      
               +     recency_8_plus         
               +    prst_FUSION_MEDIA_GROUP  
               +           day_time_PM   
               +            recency_1               
               +           recency_5                
               +          month_1      
               +grp_EXP
)

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_w_ints.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

revised_model <- glm(form, data=df, family='binomial')
summary(revised_model)
