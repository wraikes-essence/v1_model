library(tidyverse)

############################################################################
############## FULL MODEL
############################################################################

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_w_ints.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

full_model <- glm(answer_desired_Aided_awareness~., data=df, family='binomial')
summary(full_model)



############################################################################
############## REDUCED MODEL
############################################################################

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_w_ints.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

form <- formula(
    answer_desired_Aided_awareness~
      grp_EXP
+    weeks_elapsed_9.0
+    weeks_elapsed_4.0
+    weeks_elapsed_8.0
+    day_time_PM
+    rolling_imps_3.0
+    weeks_elapsed_1.0
+    prst_VOX_MEDIA
+    creative_size_970x66
+    day_time_Late_Night
+    rolling_imps_7.0
+    recency_3
+    creative_size_300x600
+    device_name_Mobile
+    prst_GDN
+    recency_5
+    weeks_elapsed_7.0
    
+    weeks_elapsed_9.0_int
+    weeks_elapsed_4.0_int
+    weeks_elapsed_8.0_int
+    day_time_PM_int
+    rolling_imps_3.0_int
+    weeks_elapsed_1.0_int
+    prst_VOX_MEDIA_int
+    creative_size_970x66_int
+    day_time_Late_Night_int
+    rolling_imps_7.0_int
+    recency_3_int
+    creative_size_300x600_int
+    device_name_Mobile_int
+    prst_GDN_int
+    recency_5_int
+    weeks_elapsed_7.0_int
    
)

reduced_model <- glm(form, data=df, family='binomial')
summary(reduced_model)




############################################################################
############## VIFs
############################################################################

library(car)

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_wo_ints.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)


vif_model <- glm(answer_desired_Aided_awareness~., data=df, family='binomial')
vif(vif_model)
max(vif(vif_model))
