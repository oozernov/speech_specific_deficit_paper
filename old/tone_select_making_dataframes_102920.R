# tone.R
# Ola Ozernov-Palchik 
#
# Updated-Aug 2 2019
#
# MIT  Perception and Adaptation in Dyslexia study
#

#### Setup ####
setwd("~/Dropbox (MIT)/Com_Dys_2016_data/final_for_sharing/final_code_JT")

Packages <- c("dplyr", "readr", "magrittr", "tidyr", "ggplot2", "lme4", "lmerTest",
              "emmeans", "sjstats","dabestr","gridExtra")
lapply(Packages, library, character.only = TRUE)



#### Load and organize the data ####
# adult data
adult <- read_csv("data/tonethres_data_a_010719.csv")%>%
  dplyr::rename(PartID = Subject) %>%
  dplyr::mutate(cond=ifelse(cond == "a", "Standard", "No-standard"))

d<-adult

## select data from last seven reversals
d_reversals <- d %>%
  group_by(PartID)  %>% 
  dplyr::summarize(max_rev=max(reversals),rev_num=max_rev-7) %>% 
  left_join(d, by = c("PartID"))%>% 
  filter(reversals >= rev_num) 

##Look at indivdual performance
d_ind_jnd_hz <- d_reversals %>%
  mutate(cond = ifelse(cond == "No-standard", "NS", "S"))%>%
  group_by(PartID, cond) %>%
  dplyr::summarize(jnd = mean(freqDiff, na.rm = T))

mean_thresh<- d %>%
  group_by(PartID) %>%
  dplyr::summarize(m_fd = mean(freqDiff),
            se = plotrix::std.error(freqDiff))

#individual graphs
ggplot(data = d_ind_jnd_hz, aes(x = cond, y = jnd, group = PartID)) +
  geom_point() +
  geom_line() +
  labs(x = 'Condition', y = "JND (Hz)", title = "Individual JND in Hz") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        plot.title = element_text(hjust = 0.5))

##Calculate JND
d_s_td = d_ind_jnd_hz %>% 
  filter(cond == 'S') %>% 
  group_by(PartID) %>% 
  dplyr::summarise(m_fd_hz = mean(jnd))

d_ns_td = d_ind_jnd_hz %>% 
  filter(cond == 'NS')%>% 
  group_by(PartID) %>% 
  dplyr::summarise(m_fd_hz = mean(jnd))

#calculate NTD
d_extra = d_ns_td %>% filter(!(PartID %in% d_s_td$PartID))
d_ns_td = d_ns_td %>% filter(!(PartID %in% d_extra$PartID))
d_ntd = as.data.frame((d_s_td$m_fd_hz - d_ns_td$m_fd_hz )/(d_s_td$m_fd_hz + d_ns_td$m_fd_hz))
names(d_ntd)[names(d_ntd) == '(d_s_td$m_fd_hz - d_ns_td$m_fd_hz)/(d_s_td$m_fd_hz + d_ns_td$m_fd_hz)'] <- 'ntd'
d_ntd$PartID = d_s_td$PartID

#making final dataframe
jnd_wide<-spread(d_ind_jnd_hz,cond,jnd)
tone=merge(jnd_wide,mean_thresh,all=TRUE)
tone=merge(tone,d_ntd,all=TRUE)

write.csv(final_d,"export_tone_051220.csv") 

######################################
#selective adaptation
d <- read_csv("data/phonselec_data_a_010719.csv")
d %<>%
  dplyr::rename(PartID = Subject) %>% # rename Subject column
  dplyr::select(-X1)  %>%# drop X1 colum
  filter(!PartID %in% 'ABCD_1776') %>% # 
  separate(soundfile, into = c("x", "step", "y"), sep = "_", remove = FALSE) %>%
  separate(step, into = c("x", "step"), sep = "d", remove = TRUE) %>%
  dplyr::select(-x, -y) %>% # only keep step column from these split columns
  mutate(step = as.numeric(step), # convert step to numeric
         # add column for whether response was /b/ (ignores NAs)
         b = ifelse(is.na(response), NA, ifelse(response == 'b', 1, 0)))

#plot slopes
ind_d <- d %>%
  group_by(PartID, step, adaptor) %>%
  dplyr::summarize(prop_b = sum(b)/4) 
ind_d_subs <- ind_d %>%
  group_by(PartID, adaptor) %>%
  dplyr::summarize(m = mean(prop_b)) 

# plot
ind_d_subs_plot <- ggplot(data = ind_d_subs,
                          aes(x = adaptor, y = m, group = PartID)) +
  geom_point() +
  geom_line() +
  labs(x = 'Adaptor', y = "mean /b/ response",
       title = "Individual Performance (Mean /b/ response by Condition)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        plot.title = element_text(hjust = 0.5))
ind_d_subs_plot

#### Create d export ####
ind_d_subs_b <- ind_d_subs %>%
  filter(adaptor == "b") %>%
  rename(mean_b_under_b_adaptor = m)
d_export <- dplyr::select(ungroup(ind_d_subs_b), PartID)

#### Run logistic regression models to extract slope and inflection ####
d_glm_fit_list <- vector(mode = "list", length = nrow(d_export)*2)
index <- 0
for (subj in d_export$PartID) {
  for (a in c("d", "b")) {
    index <- index + 1
    #print(paste(index, subj, a))
    d_subj <- ind_d %>%
      filter(adaptor == a, PartID == subj)
    subj_model <- glm(prop_b ~ step,
                      data = d_subj)
    b <- as.numeric(subj_model$coefficients[1]) # coefficient intercept
    m <- as.numeric(subj_model$coefficients[2]) # coefficient slope
    i <- ifelse(a == "d", -b/m, i) # only recalculate step for "d"
    dev_fit <- as.numeric(deviance(subj_model))
    value <- as.numeric(predict(subj_model, list(step = i), type = "response"))
    
    d_glm_fit_list[[index]] <- list(
      PartID = subj,
      adaptor = a,
      slope_coef = m,
      inflection_step = -b/m, # recalculate for "d" and "b"
      value_at_da_i = value,
      deviance = dev_fit
    )
  }
}
d_glm_fit <- bind_rows(d_glm_fit_list)
str(d_glm_fit)
d_glm_fit$adaptor<-as.factor(d_glm_fit$adaptor)

#### Slope transformation ####
d_glm_fit$slope_coef_tran <- d_glm_fit$slope_coef * -1
min_slope <- min(d_glm_fit$slope_coef_tran)
d_glm_fit$slope_coef_tran <- (d_glm_fit$slope_coef_tran + abs(min_slope)) + 1
d_glm_fit$slope_coef_tran <- log10(d_glm_fit$slope_coef_tran)

#### Plot distribution ####
ggplot(d_glm_fit, aes(x = slope_coef_tran)) +
  geom_density(alpha = .5) +
  labs(title = "Transformed Slope Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

#### Calculate inflection differences ####
adaptor_diff <- d_glm_fit %>%
  dplyr::select(PartID, adaptor, inflection_step) %>%
  spread(adaptor, inflection_step) %>%
  dplyr::mutate(diff = d - b)

step8 <- ind_d %>%
  dplyr::select(PartID, step, adaptor, prop_b) %>%
  spread(adaptor, prop_b) %>%
  dplyr::filter(step==8)%>%
  dplyr::mutate(diff_8 = d - b)


group_inflection <- d_glm_fit %>%
  filter(adaptor == "d") %>%
  left_join(adaptor_diff, by = "PartID") %>%
  dplyr::select(PartID,
         da_i = slope_coef,
         adaptor_diff = diff,deviance)

select_long<-d_glm_fit%>%dplyr::select(PartID,adaptor,slope_coef_tran)
select_long_wide<-spread(select_long,adaptor,slope_coef_tran)
select_adap<-merge(select_long_wide,group_inflection,'PartID')
step8<-step8%>%ungroup%>%dplyr::select(PartID,diff_8)
select_adap<-merge(select_adap,step8,'PartID')

###
tone_sa=merge(tone,select_adap,"PartID",all=TRUE)

write.csv(tone_sa,'ind_tone_selct_111220.csv')



#### Load and organize Child data ####
# child data
d <- read_csv("data/child_tonethres_data_122718.csv")%>%
  dplyr::mutate(cond=ifelse(cond == "a", "Standard", "No-standard"))

d_reversals <- d %>%
  group_by(PartID)  %>% 
  dplyr::summarize(max_rev=max(reversals),rev_num=max_rev-7) %>% 
  left_join(d, by = c("PartID"))%>% 
  filter(reversals >= rev_num) 

##Individual performance
d_ind_jnd_hz <- d_reversals %>%
  mutate(cond = ifelse(cond == "No-standard", "NS", "S"))%>%
  group_by(PartID, cond) %>%
  dplyr::summarize(jnd = mean(freqDiff, na.rm = T))

mean_thresh<- d %>%
  group_by(PartID) %>%
  dplyr::summarize(m_fd = mean(freqDiff),
            se = plotrix::std.error(freqDiff))

#individual graphs
ggplot(data = d_ind_jnd_hz, aes(x = cond, y = jnd, group = PartID)) +
  geom_point() +
  geom_line() +
  labs(x = 'Condition', y = "JND (Hz)", title = "Individual JND in Hz") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        plot.title = element_text(hjust = 0.5))

#Calculate JND
d_s_td = d_ind_jnd_hz %>% 
  filter(cond == 'S') %>% 
  group_by(PartID) %>% 
  summarise(m_fd_hz = mean(jnd))

d_ns_td = d_ind_jnd_hz %>% 
  filter(cond == 'NS')%>% 
  group_by(PartID) %>% 
  summarise(m_fd_hz = mean(jnd))



#calculate NTD
d_extra = d_ns_td %>% filter(!(PartID %in% d_s_td$PartID))
d_ns_td = d_ns_td %>% filter(!(PartID %in% d_extra$PartID))
d_ntd = as.data.frame((d_s_td$m_fd_hz - d_ns_td$m_fd_hz )/(d_s_td$m_fd_hz + d_ns_td$m_fd_hz))
names(d_ntd)[names(d_ntd) == '(d_s_td$m_fd_hz - d_ns_td$m_fd_hz)/(d_s_td$m_fd_hz + d_ns_td$m_fd_hz)'] <- 'ntd'
d_ntd$PartID = d_s_td$PartID

#making final dataframe
jnd_wide<-spread(d_ind_jnd_hz,cond,jnd)
tone=merge(jnd_wide,mean_thresh,all=TRUE)
tone=merge(tone,d_ntd,all=TRUE)

write.csv(tone,"tone_child_080320.csv") 

######################################
#selective adaptation
d <- read_csv("data/phonselec_data_122718.csv")
d %<>%
 separate(soundfile, into = c("x", "step", "y"), sep = "_", remove = FALSE) %>%
  separate(step, into = c("x", "step"), sep = "d", remove = TRUE) %>%
  dplyr::select(-x, -y) %>% # only keep step column from these split columns
  mutate(step = as.numeric(step), # convert step to numeric
         # add column for whether response was /b/ (ignores NAs)
         b = ifelse(is.na(response), NA, ifelse(response == 'b', 1, 0)))

#plot slopes
ind_d <- d %>%
  group_by(PartID, step, adaptor) %>%
  dplyr::summarize(prop_b = sum(b)/4) 
ind_d_subs <- ind_d %>%
  group_by(PartID, adaptor) %>%
  dplyr::summarize(m = mean(prop_b)) 

# plot
ind_d_subs_plot <- ggplot(data = ind_d_subs,
                          aes(x = adaptor, y = m, group = PartID)) +
  geom_point() +
  geom_line() +
  labs(x = 'Adaptor', y = "mean /b/ response",
       title = "Individual Performance (Mean /b/ response by Condition)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        plot.title = element_text(hjust = 0.5))
ind_d_subs_plot

#### Create d export ####
ind_d_subs_b <- ind_d_subs %>%
  filter(adaptor == "b") %>%
  dplyr::rename(mean_b_under_b_adaptor = m)
d_export <- dplyr::select(ungroup(ind_d_subs_b), PartID)

#### Run logistic regression models to extract slope and inflection ####
d_glm_fit_list <- vector(mode = "list", length = nrow(d_export)*2)
index <- 0
for (subj in d_export$PartID) {
  for (a in c("d", "b")) {
    index <- index + 1
    #print(paste(index, subj, a))
    d_subj <- ind_d %>%
      filter(adaptor == a, PartID == subj)
    subj_model <- glm(prop_b ~ step,
                      data = d_subj)
    b <- as.numeric(subj_model$coefficients[1]) # coefficient intercept
    m <- as.numeric(subj_model$coefficients[2]) # coefficient slope
    i <- ifelse(a == "d", -b/m, i) # only recalculate step for "d"
    dev_fit <- as.numeric(deviance(subj_model))
    value <- as.numeric(predict(subj_model, list(step = i), type = "response"))
    
    d_glm_fit_list[[index]] <- list(
      PartID = subj,
      adaptor = a,
      slope_coef = m,
      inflection_step = -b/m, # recalculate for "d" and "b"
      value_at_da_i = value,
      deviance = dev_fit
    )
  }
}
d_glm_fit <- bind_rows(d_glm_fit_list)
str(d_glm_fit)
d_glm_fit$adaptor<-as.factor(d_glm_fit$adaptor)

#### Slope transformation ####
d_glm_fit$slope_coef_tran <- d_glm_fit$slope_coef * -1
min_slope <- min(d_glm_fit$slope_coef_tran)
d_glm_fit$slope_coef_tran <- (d_glm_fit$slope_coef_tran + abs(min_slope)) + 1
d_glm_fit$slope_coef_tran <- log10(d_glm_fit$slope_coef_tran)

#### Plot distribution ####
ggplot(d_glm_fit, aes(x = slope_coef_tran)) +
  geom_density(alpha = .5) +
  labs(title = "Transformed Slope Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

#### Calculate inflection differences ####
adaptor_diff <- d_glm_fit %>%
  dplyr::select(PartID, adaptor, inflection_step) %>%
  spread(adaptor, inflection_step) %>%
  dplyr::mutate(diff = d - b)

group_inflection <- d_glm_fit %>%
  filter(adaptor == "d") %>%
  left_join(adaptor_diff, by = "PartID") %>%
  dplyr::select(PartID,
                da_i = slope_coef,
                adaptor_diff = diff,deviance)


step8 <- ind_d %>%
  dplyr::select(PartID, step, adaptor, prop_b) %>%
  spread(adaptor, prop_b) %>%
  dplyr::filter(step==8)%>%
  dplyr::mutate(diff_8 = d - b)

select_long<-d_glm_fit%>%dplyr::select(PartID,adaptor,slope_coef_tran)
select_wide<-spread(select_long,adaptor,slope_coef_tran)
select_adap<-merge(select_wide,group_inflection,'PartID',all = TRUE)
step8<-step8%>%dplyr::select(PartID, diff_8,all = TRUE)
select_adap<-merge(select_adap,step8,'PartID',all = TRUE)

tone_sa=merge(tone,select_adap,"PartID",all=TRUE)
write.csv(tone_sa,'ind_tone_selct_child_11120.csv')
          
          
          