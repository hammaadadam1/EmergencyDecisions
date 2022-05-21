library(tidyverse)
library(stringr)
library(broom)
library(lme4)
library(afex)
library(texreg)
library(rstatix)

clean <- read.csv('../data/healthcare_data.csv',stringsAsFactors = FALSE)
clean$ResponseId <- as.character(clean$ResponseId)
clean <- clean %>% select(-X)

data <- clean %>% 
  dplyr::select(ResponseId, Group, Rec_Type, Att_Check,
                V1_Race, V2_Race, V3_Race, V4_Race, V5_Race, V6_Race, V7_Race, V8_Race, 
                V1_Religion, V2_Religion, V3_Religion, V4_Religion, V5_Religion, V6_Religion, V7_Religion, V8_Religion,
                V1_Rec, V2_Rec, V3_Rec, V4_Rec, V5_Rec, V6_Rec, V7_Rec, V8_Rec)

for(i in 1:8){
  newcol <- paste0("V", i)
  data[,newcol] <- ""
  
  data[clean[,paste0("V", i, "_B_NM")] != "",newcol] <- clean[clean[,paste0("V", i, "_B_NM")] != "",paste0("V", i, "_B_NM")]
  data[clean[,paste0("V", i, "_B_M")] != "",newcol] <- clean[clean[,paste0("V", i, "_B_M")] != "",paste0("V", i, "_B_M")]
  data[clean[,paste0("V", i, "_AI_NM")] != "",newcol] <- clean[clean[,paste0("V", i, "_AI_NM")] != "",paste0("V", i, "_AI_NM")]
  data[clean[,paste0("V", i, "_AI_M")] != "",newcol] <- clean[clean[,paste0("V", i, "_AI_M")] != "",paste0("V", i, "_AI_M")]
  data[clean[,paste0("V", i, "_F_NM")] != "",newcol] <- clean[clean[,paste0("V", i, "_F_NM")] != "",paste0("V", i, "_F_NM")]
  data[clean[,paste0("V", i, "_F_M")] != "",newcol] <- clean[clean[,paste0("V", i, "_F_M")] != "",paste0("V", i, "_F_M")]
}

responses <- data.frame(matrix(NA, nrow=0, ncol=8))
names(responses) <- c("ResponseId", "group", "rec_type", "race", "religion", "ai_rec", "response","vignette")

for(i in 1:8){
  data_this <- data %>% dplyr::select(ResponseId, Group, Rec_Type,
                                      paste0("V", i, "_Race"),
                                      paste0("V", i, "_Religion"),
                                      paste0("V", i, "_Rec"),
                                      paste0("V", i))
  data_this$vignette <- i
  names(data_this) <- names(responses)
  responses <- rbind(responses, data_this)
}

responses$police <- 0 + 1*(responses$response=="Contact the police department for immediate assistance")
responses$afam <- 0 + 1*(responses$race=="African American")
responses$muslim <- 0 + 1*(responses$religion=="Muslim")
responses$rec_police <- 0 + 1*(responses$ai_rec=="police")
responses$vignette <- as.factor(responses$vignette)
responses$ResponseId <- as.factor(responses$ResponseId)

latexTable <- function(results, coefnames, modelnames){
  ncoef <- do.call(max, lapply(results, function(x) nrow(coef(summary(x)))))
  est <- data.frame(do.call(cbind, lapply(results, function(x) return(c(coef(summary(x))[,1], rep(NA, times=ncoef - length(coef(summary(x))[,1])))))))
  se <- data.frame(do.call(cbind, lapply(results, function(x) return(c(coef(summary(x))[,2], rep(NA, times=ncoef - length(coef(summary(x))[,2])))))))
  pvalues <- data.frame(do.call(cbind, lapply(results, function(x) return(c(coef(summary(x))[,4], rep(NA, times=ncoef - length(coef(summary(x))[,4])))))))
  
  rownames(est) <- coefnames
  rownames(se) <- coefnames
  rownames(pvalues) <- coefnames
  
  tr <- list()
  for (j in 1:ncol(est)) {
    tr[[j]] <- createTexreg(coef.names = rownames(est), 
                            coef = est[, j], 
                            se = se[,j], 
                            pvalues = pvalues[,j])
  }
  return(texreg(tr, custom.coef.names = coefnames, 
                custom.model.names = modelnames))
}

# Sample: Initial Attitudes

baseline_attitudes <- clean %>% dplyr::select(ResponseId, 
                                              Baseline_Response_1, 
                                              Baseline_Response_2,
                                              Baseline_Response_3,
                                              Baseline_Response_4,
                                              Baseline_Response_5,
                                              Baseline_Response_6) %>%
  gather(key=prompt, value = response, -ResponseId) %>% 
  mutate(police = 0+1*(response=="Police Assistance"))

baseline_summary_prompt <- baseline_attitudes %>% group_by(prompt) %>% summarise(police=sum(police)/n())
baseline_summary_resp <- baseline_attitudes %>% dplyr::select(-response) %>%
  spread(key=prompt, value = police) %>% 
  mutate(baseline_score = Baseline_Response_1 + Baseline_Response_2 + Baseline_Response_3+
           Baseline_Response_4 + Baseline_Response_5 + Baseline_Response_6)

police_attitudes <- clean %>% dplyr::select(ResponseId, 
                                            Police_1, 
                                            Police_2,
                                            Police_3,
                                            Police_4,
                                            Police_5) %>%
  gather(key=prompt, value = response, -ResponseId) %>% 
  mutate(police = 0+1*(response=="Disagree") + 2*(response=="Neutral") + 3*(response=="Agree")) %>% 
  dplyr::select(-response) %>%
  spread(key=prompt, value = police) %>% 
  mutate(police_score = (Police_1 + Police_2 + Police_3 + Police_4 + Police_5)/5)
clean <- clean %>% left_join(police_attitudes %>% select(ResponseId, police_score))
## Results

demos <- clean %>% select(ResponseId, Gender, Race, Education, Politic)
police <- clean %>% select(ResponseId, Police_1, Police_2, Police_3, Police_4, Police_5)
demos$resp_female <- 0 + 1*(demos$Gender=="Female")
demos$resp_nonwhite <- 0 + 1*(demos$Race!="White or Caucasian")
demos$resp_nondem <- 0 + 1*(demos$Politic!="Democratic Party")

responses <- responses %>% 
              left_join(baseline_summary_resp %>% dplyr::select(ResponseId, baseline_score)) %>% 
                left_join(police_attitudes %>% select(ResponseId, police_score))
responses$ResponseId <- as.factor(responses$ResponseId)

healthcare <- clean %>% left_join(police_attitudes %>% select(ResponseId, police_score)) %>% 
  left_join(baseline_summary_resp %>% select(ResponseId, baseline_score))
healthcare$Race_Clean <- healthcare$Race
healthcare$Race_Clean[grepl(",", healthcare$Race_Clean)] <- "Multiple"

healthcare_demos <- demos
responses <- responses %>% 
  left_join(healthcare %>% select(ResponseId, Gender, Familiarity, Experience, Age, Healthcare_Level, Race_Clean, Politic))


responses$fam_score <- 0 + 
  1*(responses$Familiarity == 'Definitely not') + 
  2*(responses$Familiarity == 'Probably not') + 
  3*(responses$Familiarity == 'Might or might not') + 
  4*(responses$Familiarity == 'Probably yes') + 
  5*(responses$Familiarity == 'Definitely yes') 

responses$nonwhite <- 0 + 1*(responses$Race_Clean!='White or Caucasian')
responses$nonmale <- 0 + 1*(responses$Gender != 'Male')
responses$nondem <- 0 + 1*(responses$Politic != 'Democratic Party')
responses$exp_yes <- 0 + 1*(responses$Experience != 'No')


glmm_baseline <-  mixed(police ~ (1|ResponseId) + (1|vignette) + baseline_score + afam + muslim, 
                        data = responses %>% filter(group=="Baseline"), 
                        family = binomial, method = "LRT", progress = FALSE)

glmm_unbiased <-  mixed(police ~ (1|ResponseId) + (1|vignette) + baseline_score + afam + muslim, 
                        data = responses %>% filter(group=="Unbiased", rec_type=="Explicit"), 
                        family = binomial, method = "LRT", progress = FALSE)

glmm_biased <-  mixed(police ~ (1|ResponseId) + (1|vignette) + baseline_score + afam + muslim, 
                      data = responses %>% filter(group=="Biased", rec_type=="Explicit"), 
                      family = binomial, method = "LRT", progress = FALSE)

glmm_unbiased_flag <-  mixed(police ~ (1|ResponseId) + (1|vignette) + baseline_score + afam + muslim, 
                             data = responses %>% filter(group=="Unbiased", rec_type=="Flag"), 
                             family = binomial, method = "LRT", progress = FALSE)

glmm_biased_flag <-  mixed(police ~ (1|ResponseId) + (1|vignette) + baseline_score + afam + muslim, 
                           data = responses %>% filter(group=="Biased", rec_type=="Flag"), 
                           family = binomial, method = "LRT", progress = FALSE)

results <- list(glmm_baseline, glmm_unbiased, glmm_biased, glmm_unbiased_flag, glmm_biased_flag)
latexTable(results, 
           coefnames = c("Intercept","Baseline Score","African American", "Muslim"), 
           modelnames = c(" ", "Unbiased", "Biased", "Unbiased", "Biased"))

glmm_unbiased <-  mixed(police ~ (1|ResponseId) + (1|vignette) + baseline_score + rec_police, 
                        data = responses %>% filter(group=="Unbiased", rec_type=="Explicit"), 
                        family = binomial, method = "LRT", progress = FALSE)

glmm_biased <-  mixed(police ~ (1|ResponseId) + (1|vignette) + baseline_score + rec_police, 
                      data = responses %>% filter(group=="Biased", rec_type=="Explicit"), 
                      family = binomial, method = "LRT", progress = FALSE)

glmm_unbiased_flag <-  mixed(police ~ (1|ResponseId) + (1|vignette) + baseline_score + rec_police, 
                             data = responses %>% filter(group=="Unbiased", rec_type=="Flag"), 
                             family = binomial, method = "LRT", progress = FALSE)

glmm_biased_flag <-  mixed(police ~ (1|ResponseId) + (1|vignette) + baseline_score + rec_police, 
                           data = responses %>% filter(group=="Biased", rec_type=="Flag"), 
                           family = binomial, method = "LRT", progress = FALSE)

results <- list(glmm_unbiased, glmm_biased, glmm_unbiased_flag, glmm_biased_flag)
latexTable(results, 
           coefnames = c("Intercept", "Baseline Score","AI Recommendation"), 
           modelnames = c("Unbiased", "Biased", "Unbiased", "Biased"))
