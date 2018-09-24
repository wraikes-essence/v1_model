library(tidyverse)
library(survey)
library(car)

p_val <- 0.4
vif_score <- 4

###############
####  Setup
###############

df_raw <- read_csv('C:\\Users\\william.raikes\\Programming\\Python\\abacus\\v1_model\\data\\clean\\explanatory\\chromebook_all_imps_w_ints.csv')
df_raw['answer_desired_Aided_awareness'] <- ifelse(df_raw['answer_desired_Aided_awareness']=='True', 1, 0)

df <- svydesign(
  ids=~1,
  data=df_raw,
  weights=~weights
)

base_model <- svyglm(
  formula=answer_desired_Aided_awareness~grp_EXP,
  design=df,
  family='binomial'
)

base_aic <- base_model$aic

###############
####  Loop
###############

form <- formula(
  answer_desired_Aided_awareness~grp_EXP
)
vif_form <- form

labels <- '_int|weights|grp_EXP|desired|olive'
vars <- names(df_raw)[!grepl(labels, names(df_raw))]

update_vif_form <- function(original_form){
  vars <- all.vars(original_form)
  vars <- vars[grepl('_int', vars)]
  
  new_form <- update(form, 
                     as.formula(paste(c('.~.', vars), collapse='-')
                     ))
  
  new_form
}

backstep <- function(formu, p_val){
  
  tmp_mdl <- svyglm(
    formula=formu,
    design=df,
    family='binomial'
  )
  tmp <- summary(tmp_mdl)$coefficients
  row.names(tmp[tmp[,4]>=p_val, ])
  
}

for (i in 1:length(vars)){
  cols <- c()
  int_cols <- c()
  pvalues <- c()
  
  vars <- names(df_raw)[!grepl(labels, names(df_raw))]
  for (col in vars){
    col_int = paste(col, '_int', sep='')
    cols <- append(cols, col)
    int_cols <- append(int_cols, col_int)
    
    ###########################################
    ####### Check VIF
    ###########################################
    
    vif_check <- update(vif_form,
                        as.formula(paste(".~.+", col)))

    vif_model <- svyglm(
      formula=vif_check,
      design=df,
      family='binomial'
    )

    if (any(vif(vif_model) >= vif_score)){
      pvalues <- append(pvalues, 1)
      next
    }
    
    ###########################################
    ####### Include interactions
    ###########################################
    
    tmp_form_int <- update(form,
                           as.formula(paste(".~.+", col, "+", col_int)))
    
    tmp_model_int <- svyglm(
      formula=tmp_form_int,
      design=df,
      family='binomial'
    )
    
    mdl <- summary(tmp_model_int)$coefficients
    pvalues <- append(pvalues, mdl[nrow(mdl), 4])
  }

  tmp_df <- data.frame(columns=cols, 
                       inters=int_cols, 
                       p=pvalues)
  
  tmp_df <- tmp_df[tmp_df$p < p_val,]

  if (nrow(tmp_df) == 0){
    final_model <- svyglm(
      formula=form,
      design=df,
      family='binomial'
    )
    break
  }
  
  which_min_p <- which.min(tmp_df$p)
  
  form <- update(form,
                 as.formula(paste(".~.+", 
                                  tmp_df$columns[which_min_p], 
                                  "+", 
                                  tmp_df$inters[which_min_p])))
  print(form)
  
  rm_label <- backstep(form, p_val)
  if (length(rm_label) > 0){
    for (lbl in rm_label){
      if (grepl('_int', lbl)){
        form <- update(form,
                       as.formula(paste(".~.-", 
                                        lbl, 
                                        "-", 
                                        gsub('_int', '', lbl)
                                         )))
      }
    }
  }
  print(form)
  labels <- paste(labels, '|', tmp_df$columns[which_min_p], sep='')
  vif_form <- update_vif_form(form) 
}




  

summary(final_model)





