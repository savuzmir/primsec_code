####################################################################################################################
################################ load up libraries & setup prelims #################################################
####################################################################################################################
libs = c("ggplot2", "ggridges", "lme4", "lmerTest", "dplyr", "RColorBrewer", "viridis", 
         "psych", "ppcor", "dplyr", "hBayesDM", "cowplot", "rstan", "rstanarm", 
         "reshape2", "gridExtra", "BayesianFirstAid", "BayesFactor", "sjPlot",
         "bayesplot", "loo", "rstantools", "hBayesDM", "matlab")

lapply(libs, require, character.only = TRUE)

# load in functions
mypath = ".../rstudio_cloud/util/"
setwd(mypath)
util_fncs= list.files(mypath, pattern = "*.R")
lapply(util_fncs, source)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")  

pub_theme = theme(
  text = element_text(size = 24),
  axis.title.x = element_text(size = 24),
  axis.title.y = element_text(size = 24),
  axis.text = element_text(size = 24),
  legend.title=element_text(size=24),
  legend.text=element_text(size=24),
  plot.title = element_text(lineheight=.8, face="bold", size = 18),
  panel.border = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=1, linetype='solid')) 

####################################################################################################################
################################  load and organise data for fig 2 #################################################
####################################################################################################################
mypath = ".../rstudio_cloud/compiled/"
setwd(mypath)

# load in everything 
eff_files = list.files(path=mypath, pattern="*eff.txt") 
eff_df    <- lapply(eff_files, function(x) {read.table(file = x, header = T, sep ="\t")})
eff_df    <- do.call("rbind", lapply(eff_df, as.data.frame))

# correct duplicate variable names in this task 
tmp = eff_df[eff_df$sub_id==129, ]
eff_df$sub_id[4561:4712] = 148
eff_df = eff_df[eff_df$sub_id != 129, ]
eff_df = eff_df[complete.cases(eff_df$sub_id), ]

# check trials for each subject
eff_df <- data.frame(eff_df %>% 
                       dplyr::group_by(sub_id) %>% 
                       dplyr::slice(5:n()))

# assign groups correctly
eff_df$group = NA
eff_df$group[eff_df$sub_id < 126 | (eff_df$sub_id >= 150 & eff_df$sub_id < 175)] = "Adults"
eff_df$group[(eff_df$sub_id >= 126 & eff_df$sub_id < 150) | eff_df$sub_id >= 175] = "Children"
eff_df[eff_df$trial > 144, ] = NA # removes trials that were additional / unnecessary
eff_df = eff_df[complete.cases(eff_df$trial),]

# recode variables
eff_df$difficulty = trimws(eff_df$difficulty)
eff_df$difficulty[eff_df$difficulty == "easiest"] = "1"
eff_df$difficulty[eff_df$difficulty == "easy"] = "2"
eff_df$difficulty[eff_df$difficulty == "medium"] = "3"
eff_df$difficulty[eff_df$difficulty == "hard"] = "4"

eff_df$difficulty = as.integer(eff_df$difficulty)
eff_df$difficulty <- factor(eff_df$difficulty)

# summarize data for plotting purposes
summarised_eff <- na.exclude(eff_df) %>% 
  dplyr::group_by(reinforcer, reward_size, difficulty, sub_id) %>% 
  dplyr::summarise(value_m = mean(choice), value_rt = mean(rt))

summarised_eff <- data.frame(summarised_eff)

summarised_eff$group <- "Adults"
summarised_eff$group[(summarised_eff$sub_id >= 126 & summarised_eff$sub_id < 150) | summarised_eff$sub_id >= 175] <- "Children"

fin_eff <- data.frame(na.exclude(summarised_eff) %>% 
                        dplyr::group_by(reinforcer, reward_size, difficulty, group) %>% 
                        dplyr::summarise(choice_prob = mean(value_m), choice_rt = median(value_rt)))

# -------------- plot first part of the figure -------------- #
# response probabilities
resp_prob = ggplot(data = fin_eff, aes(x=difficulty, y=reward_size, fill=choice_prob)) + 
  geom_tile() + facet_grid(group~reinforcer) + 
  scale_fill_viridis(name = "Choice Prob.", option = "magma") +
  
  labs(y="Reward Size [a.u.]", x="Difficulty level") +
  pub_theme + 
  theme(legend.position = "right") 

resp_prob

####################################################################################################################
################################ load and organize data for control task ###########################################
####################################################################################################################
mypath = ".../rstudio_cloud/compiled/nasa_questions/"
setwd(mypath)

nasa_files = list.files(path=mypath, pattern="*nasa_tlx.csv).txt") 
nasa_df    <- lapply(nasa_files, function(x) {read.table(file = x, header = T, sep ="\t")})
nasa_df    <- do.call("rbind", lapply(nasa_df, as.data.frame))

nasa_df$reinforcer = as.character(nasa_df$reinforcer)
nasa_df$item = NaN

# assign item info
for (i in 1:length(nasa_df$reinforcer))
{
  tmp = strsplit(as.character(nasa_df$reinforcer[i]), "_")
  nasa_df$reinforcer[i] = sapply(tmp, `[`, 1)
  tmp = sapply(tmp, `[`, 2)
  tmp = sapply(strsplit(tmp, "-"), `[`, 1)
  nasa_df$item[i] = substring(tmp, 5)
}

# recode variable names
nasa_df$reinforcer = factor(nasa_df$reinforcer, levels = c("easiest", "easy", "medium", "hard"))
nasa_df$item = as.numeric(nasa_df$item)

# recode ids to be correct 
nasa_df[nasa_df$sub_id == 129,]
nasa_df$sub_id[113:116] = 148
# other needs to be removed 
nasa_df = nasa_df[nasa_df$sub_id != 129,]

# assign group info
nasa_df$group[nasa_df$sub_id < 126 | (nasa_df$sub_id >= 150 & nasa_df$sub_id < 175)] = "Adults"
nasa_df$group[(nasa_df$sub_id >= 126 & nasa_df$sub_id < 150) | nasa_df$sub_id >= 175] = "Children"

# alternative analysis where levels are recoded and a joint interaction is investigated
# nasa_df$reinforcer[nasa_df$reinforcer == 'easy'] = 'easiest'
# nasa_df$reinforcer[nasa_df$reinforcer == 'medium'] = 'hard'
# out = lm(nasa_q1 ~ reinforcer*group, data = nasa_df)
# car::Anova(out, type="III")

# stats for how difficult both groups found different cognitive effort levels
out = lm(nasa_q1 ~ reinforcer + item, data = nasa_df[nasa_df$group == "Adults",])
anova(out)
# Df Sum Sq Mean Sq F value  Pr(>F)    
# reinforcer   3  196.1   65.38  3.3253 0.02082 *  
#  item         1 1602.5 1602.49 81.5040 < 2e-16 ***
#  Residuals  191 3755.3   19.66 

out = lm(nasa_q1 ~ reinforcer + item, data = nasa_df[nasa_df$group == "Children",])
anova(out)
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# reinforcer   3  750.97 250.322  11.459 9.253e-07 ***
#  item         1  219.94 219.938  10.068  0.001858 ** 
#  Residuals  139 3036.59  21.846 

nasa_df$joint_group = paste(nasa_df$group, nasa_df$reinforcer)

summarised_score = nasa_df %>%
  dplyr::group_by(joint_group, group, reinforcer) %>%
  dplyr::summarise(mean_resp = mean(nasa_q1), sem_resp = mean(nasa_q1)/sqrt(length(nasa_q1)), mean_item = mean(item))

summarised_score

# rename all of them
summarised_score$joint_group[summarised_score$joint_group=="Adults easiest"] = "Ad. Easiest"
summarised_score$joint_group[summarised_score$joint_group=="Adults easy"]    = "Ad. Easy"
summarised_score$joint_group[summarised_score$joint_group=="Adults medium"]  = "Ad. Medium"
summarised_score$joint_group[summarised_score$joint_group=="Adults hard"]    = "Ad. Hard"
summarised_score$joint_group[summarised_score$joint_group=="Children easiest"] = "Ch. Easiest"
summarised_score$joint_group[summarised_score$joint_group=="Children easy"]    = "Ch. Easy"
summarised_score$joint_group[summarised_score$joint_group=="Children medium"]  = "Ch. Medium"
summarised_score$joint_group[summarised_score$joint_group=="Children hard"]    = "Ch. Hard"

# extract only relevant colors
cbPalette_n = cbPalette[3:5]
cbPalette_n = c(cbPalette_n[2], cbPalette_n[1])

perceived_Val = ggplot(summarised_score, aes(x = joint_group, y = mean_resp, fill = group)) + 
  geom_bar(stat = "identity", color="black", size = 1.25) +
  geom_errorbar(data=summarised_score, aes(ymin=mean_resp - sem_resp, ymax=mean_resp + sem_resp), width = 0.1, size = 1.5) + 
  pub_theme +
  labs(x = "Group by Condition", y = "Estimated cognitive load [a.u.]") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") + 
  scale_fill_manual(values = cbPalette_n)

perceived_Val


####################################################################################################################
################################ parameter extraction ##############################################################
####################################################################################################################
mypath = ".../rstudio_cloud/data/stan/models"
setwd(mypath)
stan_models = list.files(path=mypath, pattern="*.rds")  
# extract param of context sensitive model
currModel = readRDS(stan_models[1])  
prim_a_hyp = extract_parameters(currModel, 'cs') # sensitive 
currModel = readRDS(stan_models[7]) 
sec_a_hyp = extract_parameters(currModel, 'cs')

currModel = readRDS(stan_models[2]) 
prim_c_hyp = extract_parameters(currModel, 'cs')
currModel = readRDS(stan_models[8]) 
sec_c_hyp = extract_parameters(currModel, 'cs')

# generate DF to test anova 
discounting_df = NULL

# make sure we are in the correct path 
mypath = ".../rstudio_cloud/compiled"
setwd(mypath)

stanModels = ".../rstudio_cloud/stan"

# it will read in individual text files that end with eff.
eff_files = list.files(path=mypath, pattern="*eff.txt")  
eff_df <- lapply(eff_files, function(x) {read.table(file = x, header = T, sep ="\t")})
eff_df <- do.call("rbind", lapply(eff_df, as.data.frame)) 

# rename duplicate
tmp = eff_df[eff_df$sub_id==129, ]
eff_df$sub_id[4561:4712] = 148
# now remove this
eff_df = eff_df[eff_df$sub_id != 129, ]
eff_df = eff_df[complete.cases(eff_df$sub_id), ]

# check trials for each subject
eff_df <- data.frame(eff_df %>% 
                       group_by(sub_id) %>% 
                       dplyr::slice(5:n())) # remove training

eff_df[eff_df$trial > 144, ] = NA # clip additional addons
eff_df = eff_df[complete.cases(eff_df$trial),]

eff_df$group[eff_df$sub_id < 126 | (eff_df$sub_id >= 150 & eff_df$sub_id < 175)] = "Adults"
eff_df$group[(eff_df$sub_id >= 126 & eff_df$sub_id < 150) | eff_df$sub_id >= 175] = "Children"

eff_df_adults <- eff_df[eff_df$sub_id < 126 | (eff_df$sub_id >= 150 & eff_df$sub_id < 175), ]
eff_df_children <- eff_df[(eff_df$sub_id >= 126 & eff_df$sub_id < 150) | eff_df$sub_id >= 175, ]

discounting_df$sub_id = c(unique(eff_df_children$sub_id[eff_df_children$reinforcer == " primary "]), unique(eff_df_children$sub_id[eff_df_children$reinforcer == " secondary "]), unique(eff_df_adults$sub_id[eff_df_adults$reinforcer == " primary "]), unique(eff_df_adults$sub_id[eff_df_adults$reinforcer == " secondary "]))
discounting_df$r = c(prim_c_hyp$r$mean, sec_c_hyp$r$mean, prim_a_hyp$r$mean, sec_a_hyp$r$mean)
discounting_df$rhat_r = c(prim_c_hyp$r$Rhat, sec_c_hyp$r$Rhat, prim_a_hyp$r$Rhat, sec_a_hyp$r$Rhat)
discounting_df$group = c(rep("Children", times = 67),  rep("Adults", times = 90))
discounting_df$reinforcer = c(rep("Primary", times = 33),  rep("Secondary", times = 34), rep("Primary", times = 45),  rep("Secondary", times = 45))

discounting_df = data.frame(discounting_df)

discounting_df$group <- factor(discounting_df$group, levels = c("Children", "Adults"))
discounting_df$reinforcer <- factor(discounting_df$reinforcer, levels = c("Primary", "Secondary"))

out1 = (lm(r ~ group*reinforcer, data = discounting_df))
summary(out1)
car::Anova(out1, type="III")
# group            0.03745   1  17.374 5.120e-05 ***
#  reinforcer       0.03333   1  15.462 0.0001273 ***
#  group:reinforcer 0.07498   1  34.784 2.278e-08 ***
#  Residuals        0.32979 153            

overall_r = mean(discounting_df$r)

r_vals <- ggplot(data = discounting_df, aes(x = reinforcer, y = r, color = reinforcer, group = reinforcer, fill = reinforcer)) +
  geom_errorbar(width=0.7, linetype = "dashed", aes(ymax=overall_r, ymin=overall_r), colour="#009E73") + 
  stat_summary(fun.data = mean_cl_boot, size=0.85, color="black", position = position_nudge(x = .3, y = 0)) +
  geom_point(position = position_jitter(width=.125, seed = 1), size = 2.5, color = "black") +
  geom_point(position = position_jitter(width=.125, seed = 1), size = 1.5) +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values= cbPalette) +  
  theme(legend.position="none") + 
  facet_wrap(~group) +
  ylab("Discount Rate [a.u.]") + xlab("Condition") +
  pub_theme

r_vals

t.test(discounting_df$r[discounting_df$group=="Adults" & discounting_df$reinforcer == "Primary"], 
       discounting_df$r[discounting_df$group=="Adults" & discounting_df$reinforcer == "Secondary"], paired = TRUE)
# t = 4.9381, df = 44, p-value = 1.183e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  0.02590028 0.06161988

ch_prim    = discounting_df$r[discounting_df$group=="Children" & discounting_df$reinforcer == "Primary"]
ch_prim_gr = discounting_df$sub_id[discounting_df$group=="Children" & discounting_df$reinforcer == "Primary"]
ch_sec     = discounting_df$r[discounting_df$group=="Children" & discounting_df$reinforcer == "Secondary"]
ch_sec_gr  = discounting_df$sub_id[discounting_df$group=="Children" & discounting_df$reinforcer == "Secondary"]

rel_subset = intersect(ch_prim_gr, ch_sec_gr)
ch_prim = c()
ch_sec  = c()

for (i in 1:length(rel_subset)){
  if (length(discounting_df$r[discounting_df$sub_id == rel_subset[i]]) == 2){
    tmp = discounting_df$r[discounting_df$sub_id == rel_subset[i]]
    ch_prim[i] = tmp[1]
    ch_sec[i]  = tmp[2]}
}

t.test(ch_prim, 
       ch_sec, paired = TRUE)

# t = -10.68, df = 28, p-value = 2.207e-11
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#  -0.05378679 -0.03647520
# sample estimates:
#   mean difference 
# -0.04513099 

t.test(discounting_df$r[discounting_df$group=="Adults" & discounting_df$reinforcer == "Primary"], 
       discounting_df$r[discounting_df$group=="Children" & discounting_df$reinforcer == "Primary"])
# t = 4.0329, df = 56.328, p-value = 0.0001675
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.02232353 0.06637865
# sample estimates:
#   mean of x  mean of y 
# 0.14385956 0.09950847 

t.test(discounting_df$r[discounting_df$group=="Adults" & discounting_df$reinforcer == "Secondary"], 
       discounting_df$r[discounting_df$group=="Children" & discounting_df$reinforcer == "Secondary"])
# t = -6.0404, df = 44.749, p-value = 2.773e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.05870064 -0.02934006
# sample estimates:
#   mean of x mean of y 
# 0.1000995 0.1441198 


###################################################################################################################
################################ need for cognition scale #########################################################
###################################################################################################################
mypath = ".../rstudio_cloud/compiled/"
setwd(mypath)

# load in
nfc_files = list.files(path=mypath, pattern="*nfc.csv).txt") 
nfc_df <- lapply(nfc_files, function(x) {read.table(file = x, header = T, sep ="\t")})
nfc_df <- do.call("rbind", lapply(nfc_df, as.data.frame)) 

# reverse score for qs
nfc_df$nfc_3 <- 6 - nfc_df$nfc_3
nfc_df$nfc_4 <- 6 - nfc_df$nfc_4
nfc_df$nfc_5 <- 6 - nfc_df$nfc_5
nfc_df$nfc_7 <- 6 - nfc_df$nfc_7
nfc_df$nfc_8 <- 6 - nfc_df$nfc_8
nfc_df$nfc_9 <- 6 - nfc_df$nfc_9
nfc_df$nfc_12 <- 6 - nfc_df$nfc_12
nfc_df$nfc_16 <- 6 - nfc_df$nfc_16
nfc_df$nfc_17 <- 6 - nfc_df$nfc_17

# generate average 
nfc_df$score <- rowSums(nfc_df[,1:18])/18

# relabel ID to keep consistency
nfc_df$sub_id[31] = 148
nfc_df$sub_id[30] = NA

# generate new data structure 
main_nfc <- NULL
main_nfc$score  <- nfc_df$score
main_nfc$sub_id <- nfc_df$sub_id

main_nfc <- data.frame(main_nfc)

# assign labels to IDs to correctly match adults / children
main_nfc$group = rep(c(c(rep(1, times = 26), rep(2, times = 22)), c(rep(1, times = 25), rep(2, times = 15))))

# did they differ on the need for cognition
t.test(main_nfc$score[main_nfc$group == 1], main_nfc$score[main_nfc$group == 2])
# t = 1.7986, df = 73.999, p-value = 0.07616
