# tone.R
# Ola Ozernov-Palchik 
#
# Updated-Aug 2 2019
#
# MIT Speech Perception study, Tone Anchoring Task
#

#### Setup ####
#setwd("~/Dropbox (MIT)/Com_Dys_2016_data/final_for_sharing/final_code_JT")
setwd("~/Dropbox (MIT)/GitHub/speech_specific_deficit_paper")
Packages <- c("dplyr", "readr", "magrittr", "tidyr", "ggplot2", "lme4", "lmerTest",
              "emmeans", "sjstats","dabestr","gridExtra")
lapply(Packages, library, character.only = TRUE)


#### Load and organize the data ####
# adult data
adult <- read_csv("data/tonethres_data_a_010719.csv")
groups_a <- read_csv("data/adult_groups_012418.csv") %>%
    mutate(group = as.factor(group)) %>%
    dplyr::rename(PartID = Subject)    

adult %<>%
    rename(PartID = Subject) %>%
    inner_join(groups_a, by = c("PartID")) %>% # add DD columns to the data
    dplyr::select(-X1)

# child data
child <- read_csv("data/child_tonethres_data_122718.csv")
groups_c <- read.csv("data/groups_041817.csv") %>%
    mutate(group = ifelse(Typical == 1, "Typ", "Dys")) %>%
    dplyr::select(-DD, -Typical)

child %<>%
    inner_join(groups_c, by = "PartID") %>% # add DD columns to the data
    mutate(group = as.factor(group)) %>%
    dplyr::select(-Subject)

# combine the datasets
d <- bind_rows("Adult" = adult,
               "Child" = child,
               .id = "age") %>%
    mutate(com_cond = paste(group, "-", age),
           # rename condition to standard/no-standard
           cond = ifelse(cond == "a", "Standard", "No-standard"))

groups <- bind_rows("Adult" = groups_a,
                    "Child" = groups_c,
                    .id = "age")

# exclude outlier participant
#d = d %>% filter(PartID != '5174')

# convert hz to cent function
hz_to_cents <- function(a, b) {
    abs(1200 * log2(b/a))
}

# add freqDiff in cents
d$cents <- hz_to_cents(d$lowFreq, d$highFreq)

#### Get sample sizes by group ####
counts <- d %>%
    group_by(PartID, age) %>%
    dplyr::summarize(m = mean(cents)) %>%
    ungroup() %>%
    left_join(groups, by = c("PartID", "age"))
count(counts, group, age)

#### Analysis ####

#### Percent Accuracy ####

#### Plot ####
d_correct <- d %>%
    group_by(cond, com_cond, group) %>%
    dplyr::summarize(m_correct = mean(correct, na.rm = TRUE),
              se = plotrix::std.error(correct, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(upper = m_correct + 1.96*se,
           lower = m_correct - 1.96*se)

ggplot(d_correct, aes(x = cond, y = m_correct*100, fill = cond)) +
    geom_bar(stat = 'identity', position = position_dodge()) +
    scale_fill_grey() +
    geom_errorbar(aes(ymin = lower*100, ymax = upper*100, width = .2), position = position_dodge(.9)) +
    labs(title = "Tone Threshold Accuracy", x = "Condition", y = "% Correct") +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~ com_cond, ncol = 2)
#write.csv(d_correct,'tone_mean_correct_group111620.csv')

#### Analysis ####

d_2 <- d %>%
    group_by(PartID, cond, group, age) %>%
    dplyr::summarize(m_correct = mean(correct, na.rm = TRUE))
acc_model <- lmer(m_correct ~ cond*group*age + (1|PartID), data = d_2, REML = FALSE)
anova(acc_model) #main effect for accuracy by cond, group, age
effectsize::eta_squared(acc_model)

#### JND ####
# Calculate
# mean frequency difference in last seven reversals (9-16)

d_reversals <- d %>%
    group_by(PartID)  %>% 
    dplyr::summarize(max_rev=max(reversals),rev_num=max_rev-7) %>% 
    left_join(d, by = c("PartID"))%>% 
    filter(reversals >= rev_num) 

    #mutate(cents = hz_to_cents(lowFreq, highFreq))
 # d_idthres <- d_reversals %>%
 #     group_by(cond, com_cond, group) %>%
 #     dplyr::summarize(m_fd = mean(freqDiff))

#calculate percent difference per reviewer
d_idthres<- d_reversals %>%
    group_by(cond, com_cond, group) %>%
    dplyr::summarize(m_fd = mean(freqDiff/lowFreq*100))


# d_jnd_hz <- d_reversals %>%
#     mutate(cond = ifelse(cond == "No-standard", "NS", "S"))%>%
#     group_by(cond, com_cond, group) %>%
#     dplyr::summarize(m_fd = mean(freqDiff),
#               se = plotrix::std.error(freqDiff)) %>%
#     mutate(upper = m_fd + 1.96 * se,
#            lower = m_fd - 1.96 * se)
# 

# In percent
d_jnd_hz <- d_reversals %>%
    mutate(cond = ifelse(cond == "No-standard", "NS", "S"))%>%
    group_by(cond, com_cond, group) %>%
    dplyr::summarize(m_fd = mean(freqDiff/lowFreq*100),
                     se = plotrix::std.error(freqDiff/lowFreq*100)) %>%
    mutate(upper = m_fd + 1.96 * se,
           lower = m_fd - 1.96 * se)


# ##Individual performance
# d_ind_jnd_hz <- d_reversals %>%
#     mutate(cond = ifelse(cond == "No-standard", "No-Standard", "Standard"))%>%
#     group_by(PartID, cond, group, age) %>%
#     dplyr::summarize(jnd = mean(freqDiff, na.rm = T))

#in percent
d_ind_jnd_hz <- d_reversals %>%
    mutate(cond = ifelse(cond == "No-standard", "No-Standard", "Standard"))%>%
    group_by(PartID, cond, group, age) %>%
    dplyr::summarize(jnd = mean(freqDiff/lowFreq*100, na.rm = T))


#plot individual slopes by age
ggplot(data = d_ind_jnd_hz, aes(x = cond, y = jnd, group = PartID, color = group)) +
    geom_point() +
    geom_line() +
    labs(x = 'Condition', y = "JND (Hz)", title = "Individual JND in Hz") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white'),
          plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~age, ncol = 2)

####  Jnd effects ####

#Plot effects
ggplot(d_jnd_hz, aes(x = cond, y = m_fd, fill = cond)) +
    geom_bar(stat = 'identity', position = position_dodge()) +
    scale_fill_grey() +
    geom_errorbar(aes(ymin = lower, ymax = upper, width = .2), position = position_dodge(.9)) +
    labs(title = "JND in Hz", x = "Condition", y = "JND in Hertz (Hz)") +
    theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank()) +
    facet_wrap(~com_cond, ncol = 2)

d_ind_jnd_hz$com_cond <- paste(d_ind_jnd_hz$group,d_ind_jnd_hz$age)
d_ind_jnd_hz$group_cond = paste(d_ind_jnd_hz$group, "-", d_ind_jnd_hz$cond)

#https://acclab.github.io/DABEST-python-docs/robust-beautiful.html


multi.two.group.unpaired <- 
    d_ind_jnd_hz %>%
    dabest(com_cond,jnd,
           idx = list(c("Typ Child", "Dys Child"), 
                      c("Typ Adult", "Dys Adult")),
           paired = FALSE
    )

multi.two.group.unpaired2 <- dabestr::mean_diff(multi.two.group.unpaired)

plot(multi.two.group.unpaired2,
     rawplot.ylabel = "JND % (Hz)",color.column = group_cond,
     palette = c("blue", "light blue","red","pink"),
     effsize.ylabel = "Unpaired mean difference")

d_ind_jnd_hz$age<-as.factor(d_ind_jnd_hz$age)
d_ind_jnd_hz$cond<-as.factor(d_ind_jnd_hz$cond)

#Analyze effects
jnd_model <- lmer(jnd ~ cond*group*age + (1|PartID), data = d_ind_jnd_hz,REML = TRUE)
#rand(jnd_model) #test the random effect in the model
anova(jnd_model)
effectsize::eta_squared(jnd_model)
#anova(lm(jnd ~ cond*group*age, data = d_ind_jnd_hz))

# ###Post hoc comparisons
#lsmeans(jnd_model, list(pairwise ~ group|cond), adjust = "tukey")
#lsmeans(jnd_model, list(pairwise ~ group|age), adjust = "tukey")


#### NTD ####
# Calculations

d_s_td = d_ind_jnd_hz %>% 
    dplyr::filter(cond == 'Standard') %>% 
    group_by(PartID) %>% 
    dplyr::summarise(m_fd_hz = mean(jnd))

d_ns_td = d_ind_jnd_hz %>% 
    filter(cond == 'No-Standard')%>% 
    group_by(PartID) %>% 
    dplyr::summarise(m_fd_hz = mean(jnd))

#remove participants who don't have both conditions
d_extra = d_ns_td %>% filter(!(PartID %in% d_s_td$PartID))
d_ns_td = d_ns_td %>% filter(!(PartID %in% d_extra$PartID))

d_ntd = as.data.frame((d_s_td$m_fd_hz - d_ns_td$m_fd_hz )/(d_s_td$m_fd_hz + d_ns_td$m_fd_hz))
names(d_ntd)[names(d_ntd) == '(d_s_td$m_fd_hz - d_ns_td$m_fd_hz)/(d_s_td$m_fd_hz + d_ns_td$m_fd_hz)'] <- 'm_fd_hz'
d_ntd$PartID = d_s_td$PartID

# now add risk columns and split into groups, then put back together for plot
d_ntd = merge(d_ntd, groups, "PartID")
d_ntd$com_cond = paste(d_ntd$group,d_ntd$age)

#Plot effect size
#https://cran.r-project.org/web/packages/dabestr/vignettes/using-dabestr.html

multi.two.group.unpaired <- 
    d_ntd %>%
    dabest(com_cond,m_fd_hz,
           idx = list(c("Typ Child", "Dys Child"), 
                      c("Typ Adult", "Dys Adult")),
           paired = FALSE
    )

multi.two.group.unpaired2 <- dabestr::mean_diff(multi.two.group.unpaired)

plot(multi.two.group.unpaired2,
     rawplot.ylim = c(-1, 0.7),color.column = group,
     palette = c("blue", "red"),
     rawplot.ylabel = "NTD",
     effsize.ylabel = "Unpaired mean difference")

#jpeg("jnd_ntd_plots052721.jpeg") # Open a new pdf file
#grid.arrange(p1, p2, ncol=2) # Write the grid.arrange in the file
#dev.off() # Close the file

#### Analysis ####
d_ntd$age<-as.factor(d_ntd$age)
d_ntd$group<-as.factor(d_ntd$group)
d_ntd$com_cond<-as.factor(d_ntd$com_cond)
model_ntd <- lm(m_fd_hz ~ group*age, data = d_ntd)
anova(model_ntd)
eta_sq(model_ntd)
lsmeans(model_ntd, list(pairwise ~ age), adjust = "tukey")

#### Frequency difference by trial ####
d_fd_trial_hz <- d %>%
    group_by(trialNum, group, com_cond, cond) %>%
    summarize(m_fd = mean(freqDiff),
              se = plotrix::std.error(freqDiff)) %>%
    ungroup() %>%
    mutate(upper = m_fd + 1.96 * se,
           lower = m_fd - 1.96 * se)

ggplot(d_fd_trial_hz, aes(colour = cond, y = m_fd, x = trialNum)) +
    scale_color_manual(values = c('red', 'blue')) +
    geom_line() + geom_errorbar(aes(ymin = lower, ymax = upper, width = 1)) +
    labs(x = "Trial number", y = "Frequency Difference (Hz)", title = "Frequency Difference by Trial") +
    facet_wrap(~com_cond, ncol = 2) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank())

new_fd_hz <- d_fd_trial_hz %>%
    dplyr::select(trialNum, cond, com_cond, m_fd) %>%
    mutate(cond = ifelse(cond == "No-standard", "ns", "s")) %>%
    spread(cond, m_fd) %>%
    mutate(ns_diff_s = ns - s)

#pdf("~/Dropbox (MIT)/Com_Dys_2016_data/final_for_sharing/final_code_JT/plots/freq_by_trial.pdf") # Open a new pdf file
ggplot(new_fd_hz, aes(colour = com_cond, y = ns_diff_s, x = trialNum)) +
    geom_point() + stat_smooth(method = "loess") +
    labs(x = "Trial number", y = "Frequency Difference (NS - S) in Hz") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),panel.background=element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.text = element_text(size=14))+
    scale_color_brewer(palette="Dark2")

dev.off() # Close the file

##create an ind.diff file
d_jnd <- d_ind_jnd_hz %>%
    group_by(PartID) %>%
    summarize(m_jnd = mean(jnd))

names(d_ntd)[names(d_ntd)=="m_fd_hz"] <- "ntd"
names(d_jnd)[names(d_jnd)=="m_jnd"] <- "tone_thresh"

tone_data<-merge(d_jnd,d_ntd)            
write.csv(tone_data,"~/Dropbox (MIT)/GitHub/speech_specific_deficit_paper/data/tone_jnd_ntd_062721.csv")
