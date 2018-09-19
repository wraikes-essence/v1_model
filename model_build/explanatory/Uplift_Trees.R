library(tidyverse)
library(uplift)

############################################################################
#### UPLIFT MODELING CONDITIONAL INFERENTIAL TREES
############################################################################

df <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_wo_ints.csv')
df['answer_desired_Aided_awareness'] <- ifelse(df['answer_desired_Aided_awareness']=='True', 1, 0)

tree_01 <- ccif(answer_desired_Aided_awareness~trt(grp_EXP) + ., data=df, pvalue=0.01, split_method='ED')
tree_05 <- ccif(answer_desired_Aided_awareness~trt(grp_EXP) + ., data=df, pvalue=0.05, split_method='ED')

#test1 <- modelProfile(answer_desired_Aided_awareness~trt(grp_EXP) + device_name_Desktop + recency_1 + recency_2, data=df)
#explore(answer_desired_Aided_awareness~trt(grp_EXP) + ., data=df)