libs = c("ggplot2", "ggridges", "lme4", "lmerTest", "dplyr", "RColorBrewer", "viridis", 
         "psych", "ppcor", "plyr", "Hmisc", "cowplot", "ggpubr", "sjPlot", "BayesFactor")

lapply(libs, require, character.only = TRUE)

# load in functions
mypath = ".../rstudio_cloud/util"
setwd(mypath)
util_fncs= list.files(mypath, pattern = "*.R")
lapply(util_fncs, source)

######################################################################################################################
################################ LMER analysis #######################################################################
######################################################################################################################
mypath = ".../rstudio_cloud/compiled/processed_data"
setwd(mypath)
data_files= list.files(mypath, pattern = "*.Rdata")
lapply(data_files, load, .GlobalEnv)

# normalize trial value to help with convergence
gng_go$trial <- zscore(gng_go$trial)

# remove bad participant to keep consistent across tasks
gng_go[gng_go$sub==129,] = NA

lmer.resp_mod_1 <- glmer(response ~group + (1|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

lmer.resp_mod_2 <- glmer(response  ~ group + reinforcer + (1|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_3 <- glmer(response ~ group*reinforcer + (1|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_4 <- glmer(response ~ group*reinforcer + (trial|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_5 <- glmer(response ~ group*reinforcer + (reinforcer|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_6 <- glmer(response ~ group*reinforcer + (reinforcer + trial|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_7 <- glmer(response ~ group*reinforcer + (group |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_8 <- glmer(response ~ group*reinforcer + (group + trial|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_9 <- glmer(response ~ group*reinforcer + (group + reinforcer|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_10 <- glmer(response ~ group*reinforcer + (group + reinforcer + trial|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_11 <- glmer(response ~ group*reinforcer + (direction|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_12 <- glmer(response ~ group*reinforcer + (direction + trial|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_13 <- glmer(response ~ group + reinforcer + (direction + trial|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_14 <- glmer(response ~ group*reinforcer + (direction + group + trial|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_15 <- glmer(response ~ group*reinforcer + (direction + reinforcer + trial|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))


# addition of interaction is significant 
anova(lmer.resp_mod_1, lmer.resp_mod_2, lmer.resp_mod_3, lmer.resp_mod_4, lmer.resp_mod_5, 
      lmer.resp_mod_6, lmer.resp_mod_7, lmer.resp_mod_8, lmer.resp_mod_9, lmer.resp_mod_10, 
      lmer.resp_mod_11, lmer.resp_mod_12, lmer.resp_mod_13, lmer.resp_mod_14)

# compare competing models
anova(lmer.resp_mod_15, lmer.resp_mod_11, lmer.resp_mod_12, lmer.resp_mod_14) # between j and k, j is the winning model

# summary info
summary(lmer.resp_mod_12)
car::Anova(lmer.resp_mod_12, type="III")
# (Intercept)      747.919  1  < 2.2e-16 ***
#  group             37.887  1  7.497e-10 ***
#  reinforcer        22.939  1  1.673e-06 ***
#  group:reinforcer  12.756  1  0.0003549 ***

anova(lmer.resp_mod_12, lmer.resp_mod_13)
# F = 11.638, 0.0006461  ** p value

confint(lmer.resp_mod_12, method = c("Wald"))
# -0.31478 [-0.5314080 -0.09814748] - this is for the estimate
#groupChildren                       -0.8133809 -0.4204884
#reinforcer secondary                 0.1656554  0.3951537
#groupChildren:reinforcer secondary  -0.4437624 -0.1292859

# extract model predictions
model_pred = predict(lmer.resp_mod_12, type="response")

# remove nans 
gng_go_mod = na.exclude(gng_go)
gng_go_mod$prediction <- model_pred

# generate predictive estimate for each subject
summ_gng_go_mod <- gng_go_mod %>%
  dplyr::group_by(sub_id, reinforcer, group) %>%
  dplyr::summarise(mean_pred = mean(prediction), sem_pred = sem(prediction))

# convert to df
summ_gng_go_mod = data.frame(summ_gng_go_mod)

# -------------- plot main go trials result  -------------- #
overall_pred  = mean(summ_gng_go_mod$mean_pred*100)
summ_gng_go_mod$group <- factor(summ_gng_go_mod$group, levels = c("Children", "Adults"))

resp_pred <- ggplot(data = summ_gng_go_mod, aes(x = reinforcer, y = mean_pred*100, color = reinforcer, group = reinforcer, fill = reinforcer)) +
  stat_summary(fun.data = mean_cl_boot, size=1.25, color="black", position = position_nudge(x = .3, y = 0)) +
  geom_errorbar(width=0.7, linetype = "dashed", aes(ymax=overall_pred, ymin=overall_pred), colour="#009E73") + 
  geom_point(position = position_jitter(width=.2, seed = 1), size = 4, color = "black") +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 2) +
  labs(y = "Accuracy [%]", x = "Reinforcer") +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values= cbPalette) +  
  theme(legend.position="none") + 
  facet_wrap(~group) +
  labs(y="Probability to Hit [%]") +
  pub_theme

# extract both groups and do key t-test comparison
rel_prim = summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " primary " & summ_gng_go_mod$group=="Adults"]
rel_sec  = summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " secondary " & summ_gng_go_mod$group=="Adults"]
rel_prim_gr = summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " primary " & summ_gng_go_mod$group=="Children"]
rel_sec_gr  = summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " secondary " & summ_gng_go_mod$group=="Children"]

t.test(rel_prim, rel_sec, paired=TRUE)
# t = -7.7907, df = 50, p-value = 3.508e-10

# remove case without a pair across both conditions
summ_gng_go_mod = summ_gng_go_mod[summ_gng_go_mod$sub_id != 181,]

t.test(summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " primary " & summ_gng_go_mod$group=="Children"], 
       summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " secondary " & summ_gng_go_mod$group=="Children"], paired=TRUE)
# t = 0.26858, df = 36, p-value = 0.7898

# look for bayes factor strength
# do bayesian t-test for additional confirmation
tmp = ttestBF(x = summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " primary " & summ_gng_go_mod$group=="Children"], y = summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " secondary " & summ_gng_go_mod$group=="Children"], rscale = "medium", paired = TRUE)
1/tmp
# Null, mu=0 : 5.468703 ±0.05%

# to compute correlation, first reoder so they match in IDs
ev_df = ev_df[with(ev_df, order(reinforcer, sub_id)),]
summ_gng_go_mod = summ_gng_go_mod[with(summ_gng_go_mod, order(reinforcer, sub_id)),]

# manually add ev_df to summ_gng_go_mod
unq_ids  = unique(ev_df$sub_id)
ref_type = c(" primary ", " secondary ")

for (k in 1:2){
  for (id in 1:length(unq_ids)){
    if (length(ev_df$score[ev_df$sub_id == unq_ids[id] & ev_df$reinforcer == ref_type[k]]) > 0) {
      summ_gng_go_mod$ev_response[summ_gng_go_mod$sub_id == unq_ids[id] & summ_gng_go_mod$reinforcer == ref_type[k]] =
        ev_df$score[ev_df$sub_id == unq_ids[id] & ev_df$reinforcer == ref_type[k]]}
    else {
      summ_gng_go_mod$ev_response[summ_gng_go_mod$sub_id == unq_ids[id] & summ_gng_go_mod$reinforcer == ref_type[k]] = NaN
    }
  }
}

# assign the info
summ_gng_go_mod$mean_pred   <- summ_gng_go_mod$mean_pred
summ_gng_go_mod$age         <- ev_df$age

# -------------- plot top part of fig3  -------------- #
coeff_ev <- ggplot(summ_gng_go_mod, aes(x = ev_response, y = mean_pred*100, color = reinforcer)) + 
  geom_point(size=4, color = "black") +
  geom_point(size=2)  + 
  geom_smooth(method="lm", se=FALSE) + 
  stat_cor(aes(color = reinforcer), method="pearson", size = 8.5, label.y = c(40, 46)) +
  scale_fill_manual(values = cbPalette) +
  coord_cartesian(ylim=c(25, 105)) +
  scale_color_manual(values= cbPalette) +
  facet_wrap(~group) + 
  pub_theme + xlab("Subjective value [a.u.]") +
  ylab("Probability to Hit [%]") + theme(legend.position = "none")

up = cowplot::plot_grid(resp_pred, coeff_ev)
up

tab_model(lmer.resp_mod_2j, show.stat=TRUE,
          title="Table S1: Group by Reinforcer interaction on probability to hit")

# correlation between discounting param and mean pred
ch_prim    = discounting_df$r[discounting_df$group=="Adults" & discounting_df$reinforcer == "Secondary"]
ch_prim_gr = discounting_df$sub_id[discounting_df$group=="Adults" & discounting_df$reinforcer == "Secondary"]
ch_sec     = summ_gng_go_mod$mean_pred[summ_gng_go_mod$group=="Adults" & summ_gng_go_mod$reinforcer == "Secondary"]
ch_sec_gr  = summ_gng_go_mod$sub_id[summ_gng_go_mod$group=="Adults" & summ_gng_go_mod$reinforcer == " secondary "]

rel_subset = intersect(ch_prim_gr, ch_sec_gr)
ch_prim = c()
ch_sec  = c()

for (i in 1:length(rel_subset)){
  if (length(discounting_df$r[discounting_df$sub_id == rel_subset[i]]) == 2){
    tmp = discounting_df$r[discounting_df$sub_id == rel_subset[i]]
    ch_prim[i] = tmp[2]}
  if (length(summ_gng_go_mod$mean_pred[summ_gng_go_mod$sub_id == rel_subset[i]])==2){
    tmp = summ_gng_go_mod$mean_pred[summ_gng_go_mod$sub_id == rel_subset[i]]
    ch_sec[i] = tmp[2]}  
}

corr.test(ch_prim, ch_sec)

############################## gng go RT  ##########################################################
gng_go$log_rt <- log(gng_go$rt)

lmer.resp_mod_1 <- lmer(log_rt ~group + (1|sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2 <- lmer(log_rt  ~ group + reinforcer + (1|sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
lmer.resp_mod_3 <- lmer(log_rt ~ group*reinforcer + (1|sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
lmer.resp_mod_4 <- lmer(log_rt ~ group*reinforcer + (trial|sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

lmer.resp_mod_5 <- lmer(log_rt ~ group*reinforcer + (reinforcer|sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
lmer.resp_mod_6 <- lmer(log_rt ~ group*reinforcer + (reinforcer + trial|sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
lmer.resp_mod_7 <- lmer(log_rt ~ group*reinforcer + (group |sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
lmer.resp_mod_8 <- lmer(log_rt ~ group*reinforcer + (group + trial|sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
lmer.resp_mod_9 <- lmer(log_rt ~ group*reinforcer + (group + reinforcer|sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
lmer.resp_mod_10 <- lmer(log_rt ~ group*reinforcer + (group + reinforcer + trial|sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

lmer.resp_mod_11 <- lmer(log_rt ~ group*reinforcer + (direction|sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
lmer.resp_mod_12 <- lmer(log_rt ~ group*reinforcer + (direction + trial|sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))

# we know trial will be influential
lmer.resp_mod_13 <- lmer(log_rt ~ group*reinforcer + (direction + group + trial|sub_id) + (1|reinforcer), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
lmer.resp_mod_14 <- lmer(log_rt ~ group*reinforcer + (direction + reinforcer + trial|sub_id), data=na.exclude(gng_go), REML = FALSE, control = lmerControl(optimizer = "bobyqa"))


anova(lmer.resp_mod_1, lmer.resp_mod_2, lmer.resp_mod_3, lmer.resp_mod_4, lmer.resp_mod_5,
      lmer.resp_mod_6, lmer.resp_mod_7, lmer.resp_mod_8, lmer.resp_mod_9, lmer.resp_mod_10,
      lmer.resp_mod_11, lmer.resp_mod_12, lmer.resp_mod_13)

anova(lmer.resp_mod_11, lmer.resp_mod_6,
      lmer.resp_mod_9, lmer.resp_mod_10, 
      lmer.resp_mod_12, lmer.resp_mod_14)

anova(lmer.resp_mod_6, lmer.resp_mod_12, lmer.resp_mod_14)

run_diagnostics(lmer.resp_mod_2)

summary(lmer.resp_mod_14)
car::Anova(lmer.resp_mod_14,type=c("III"), vcov.=vcov(lmer.resp_mod_14, complete=FALSE))

gng_go_mod = na.exclude(gng_go)
gng_go_mod$prediction <- model_pred

# generate predictive estimate for each subject
summ_gng_go_mod <- gng_go_mod %>%
  dplyr::group_by(sub_id, reinforcer, group) %>%
  dplyr::summarise(mean_pred = mean(prediction), sem_pred = sem(prediction))

# generate df  s
summ_gng_go_mod = data.frame(summ_gng_go_mod)

overall_rt = mean(summ_gng_go_mod$mean_pred)

resp_pred <- ggplot(data = summ_gng_go_mod, aes(x = reinforcer, y = exp(mean_pred), color = reinforcer, group = reinforcer, fill = reinforcer)) +
  geom_errorbar(width=0.7, linetype = "dashed", aes(ymax=exp(overall_rt), ymin=exp(overall_rt)), colour="#009E73") + 
  stat_summary(fun.data = mean_cl_boot, size=1.25, color="black", position = position_nudge(x = .3, y = 0)) +
  #  stat_summary(geom = "crossbar", width = 0.12, fatten=4, color= "#233E6C", position = position_nudge(x = .3, y=0),
  #                 fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))}) +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 4, color = "black") +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 2) +
  labs(y = "Accuracy [%]", x = "Reinforcer") +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values= cbPalette) +  
  theme(legend.position="none") + 
  facet_wrap(~group) +
  labs(y="Reaction time [msec]") +
  pub_theme

resp_pred

t.test(summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " primary " & summ_gng_go_mod$group=="Adults"], 
       summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " secondary " & summ_gng_go_mod$group=="Adults"], paired=TRUE)

t.test(summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " primary " & summ_gng_go_mod$group=="Children"], 
       summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " secondary " & summ_gng_go_mod$group=="Children"], paired=TRUE)

t.test(summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " primary " & summ_gng_go_mod$group=="Adults"], 
       summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " primary " & summ_gng_go_mod$group=="Children"])

t.test(summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " secondary " & summ_gng_go_mod$group=="Adults"], 
       summ_gng_go_mod$mean_pred[summ_gng_go_mod$reinforcer == " secondary " & summ_gng_go_mod$group=="Children"])


tab_model(lmer.resp_mod_14, show.stat=TRUE, p.val = c("satterthwaite"),
          title="Table S1: Group by Reinforcer interaction on log-Reaction time")

summary(lmer.resp_mod_14)
car::Anova(lmer.resp_mod_14,type=c("III"), vcov.=vcov(lmer.resp_mod_2l, complete=FALSE))
confint(lmer.resp_mod_14, method=c("Wald"))
#reinforcer
# -0.07412753 -0.027005331
#group
# 0.01030985  0.182226713

################################# gng nogo group by reinforcer ##############################
gng_nogo$trial <- zscore(gng_nogo$trial)

# rm outliers
# gng_nogo$response[gng_nogo$sub_id == 103] = NA
# gng_nogo$response[gng_nogo$sub_id == 146] = NA

# gng_nogo$response[gng_nogo$sub_id == 114 & gng_nogo$reinforcer == " primary "]   = NA
# gng_nogo$response[gng_nogo$sub_id == 181 & gng_nogo$reinforcer == " secondary "] = NA
# gng_nogo$response[gng_nogo$sub_id == 188 & gng_nogo$reinforcer == " primary "]   = NA

# rm NA
gng_nogo = gng_nogo[complete.cases(gng_nogo$response), ]

lmer.resp_mod_1 <- glmer(response ~ group + (1|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

lmer.resp_mod_2 <- glmer(response  ~ group + reinforcer + (1|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2a <- glmer(response ~ group*reinforcer + (1|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2b <- glmer(response ~ group*reinforcer + (trial|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2bi <- glmer(response ~ group + reinforcer + (trial|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2c <- glmer(response ~ group*reinforcer + (reinforcer|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2d <- glmer(response ~ group*reinforcer + (reinforcer + trial|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2e <- glmer(response ~ group*reinforcer + (group |sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2f <- glmer(response ~ group*reinforcer + (group + trial|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2g <- glmer(response ~ group*reinforcer + (group + reinforcer|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2h <- glmer(response ~ group*reinforcer + (group + reinforcer + trial|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2i <- glmer(response ~ group*reinforcer + (direction|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2j <- glmer(response ~ group*reinforcer + (direction + trial|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2ji <- glmer(response ~ group + reinforcer + (direction + trial|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))

# we know trial will be influential
lmer.resp_mod_2k <- glmer(response ~ group*reinforcer + (direction + group + trial|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2l <- glmer(response ~ group*reinforcer + (direction + reinforcer + trial|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))


# addition of interaction is significant 
anova(lmer.resp_mod_1, lmer.resp_mod_2, lmer.resp_mod_2a, lmer.resp_mod_2b, lmer.resp_mod_2c, 
      lmer.resp_mod_2d, lmer.resp_mod_2e, lmer.resp_mod_2f, lmer.resp_mod_2g, lmer.resp_mod_2h, 
      lmer.resp_mod_2i, lmer.resp_mod_2j, lmer.resp_mod_2k, lmer.resp_mod_2l)

# compare competing models
anova(lmer.resp_mod_2, lmer.resp_mod_2a, lmer.resp_mod_2b, lmer.resp_mod_2d, lmer.resp_mod_2f, lmer.resp_mod_2j, lmer.resp_mod_2k) # between j and k, j is the winning model

# summary info
summary(lmer.resp_mod_2b)
car::Anova(lmer.resp_mod_2b, type="III")
#anova(lmer.resp_mod_2j, lmer.resp_mod_2ji)

run_diagnostics(lmer.resp_mod_2b)

model_pred = predict(lmer.resp_mod_2b, type="response")

gng_nogo_mod = gng_nogo
gng_nogo_mod$prediction <- model_pred

# generate predictive estimate for each subject
summ_gng_nogo_mod <- gng_nogo_mod %>%
  dplyr::group_by(sub_id, reinforcer, group) %>%
  dplyr::summarise(mean_pred = mean(prediction), sem_pred = sem(prediction))

# generate df  s
summ_gng_nogo_mod = data.frame(summ_gng_nogo_mod)

summ_gng_nogo_mod$group <- factor(summ_gng_nogo_mod$group, levels = c("Children", "Adults"))

overall_pred = mean(summ_gng_nogo_mod$mean_pred)

mean_pred <- ggplot(data = summ_gng_nogo_mod, aes(x = reinforcer, y = mean_pred, color = reinforcer, group = reinforcer, fill = reinforcer)) +
  geom_errorbar(width=0.7, linetype = "dashed", aes(ymax=overall_pred, ymin=overall_pred), colour="#009E73") + 
  stat_summary(fun.data = mean_cl_boot, size=1.25, color="black", position = position_nudge(x = .3, y = 0)) +
  #  stat_summary(geom = "crossbar", width = 0.12, fatten=4, color= "#233E6C", position = position_nudge(x = .3, y=0),
  #                 fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))}) +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 4, color = "black") +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 2) +
  labs(y = "Accuracy [%]", x = "Reinforcer") +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values= cbPalette) +  
  theme(legend.position="none") + 
  facet_grid(~group) +
  labs(y="Probability to correctly reject [%]") +
  pub_theme

mean_pred


t.test(summ_gng_nogo_mod$mean_pred[summ_gng_nogo_mod$reinforcer == " primary " & summ_gng_nogo_mod$group=="Adults"], 
       summ_gng_nogo_mod$mean_pred[summ_gng_nogo_mod$reinforcer == " secondary " & summ_gng_nogo_mod$group=="Adults"], paired=TRUE)
# t = -1.764, df = 50, p-value = 0.08384
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#  -0.053119700  0.003443346
# sample estimates:
#   mean difference 
# -0.02483818 

summ_gng_nogo_mod = summ_gng_nogo_mod[summ_gng_nogo_mod$sub_id != 181,]

t.test(summ_gng_nogo_mod$mean_pred[summ_gng_nogo_mod$reinforcer == " primary " & summ_gng_nogo_mod$group=="Children"], 
       summ_gng_nogo_mod$mean_pred[summ_gng_nogo_mod$reinforcer == " secondary " & summ_gng_nogo_mod$group=="Children"], paired=TRUE)
# t = -0.62406, df = 36, p-value = 0.5365
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -0.04011827  0.02123827
# sample estimates:
#  mean difference 
# -0.00944 

# compute stdev
# generate predictive estimate for each subject
summ_gng_nogo_mod_mod <- gng_nogo_mod %>%
  dplyr::group_by(reinforcer, group) %>%
  dplyr::summarise(mean_pred = mean(prediction), sem_pred = sd(prediction))

above_2sd = summ_gng_nogo_mod_mod$mean_pred + summ_gng_nogo_mod_mod$sem_pred*2
below_2sd = summ_gng_nogo_mod_mod$mean_pred - summ_gng_nogo_mod_mod$sem_pred*2

# find ids to remove 
summ_gng_nogo_mod$sub_id[summ_gng_nogo_mod$group == 'Adults'][summ_gng_nogo_mod$mean_pred[summ_gng_nogo_mod$group == 'Adults'] < below_2sd[1]]
# 103 both conditions, 114 juice condition
summ_gng_nogo_mod$sub_id[summ_gng_nogo_mod$group == 'Children'][summ_gng_nogo_mod$mean_pred[summ_gng_nogo_mod$group == 'Children'] < below_2sd[4]]
# 146 both conditions, 181 = 2nd, 188 = prim


## join all of them now

down = cowplot::plot_grid(mean_pred, mean_pred)
cowplot::plot_grid(up, down, nrow=2)


############################## group by reward size by difficulty ##########################################################

lmer.resp_mod_1 <- glmer(response  ~ group + reward_size + difficulty + (1|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2a <- glmer(response ~ group*reward_size + difficulty +  (1|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2b <- glmer(response ~ group*reward_size*difficulty + (1|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2c <- glmer(response ~ group*reward_size*difficulty + (trial|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2d <- glmer(response ~ group*reward_size*difficulty + (reward_size |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2e <- glmer(response ~ group*reward_size*difficulty + (difficulty |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2f <- glmer(response ~ group*reward_size*difficulty+ (reward_size + difficulty |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2g <- glmer(response ~ group*reward_size*difficulty + (reward_size + difficulty + trial|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2h <- glmer(response ~ group*reward_size*difficulty + (difficulty + trial|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2i <- glmer(response ~ group*reward_size*difficulty + (reward_size + trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2j <- glmer(response ~ group*reward_size*difficulty + (direction  |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2k <- glmer(response ~ group*reward_size*difficulty + (direction + trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2ki <- glmer(response ~ group*reward_size + difficulty + (direction + trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2l <- glmer(response ~ group*reward_size*difficulty + (direction + reward_size |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2m <- glmer(response ~ group*reward_size*difficulty + (direction + difficulty |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2n <- glmer(response ~ group*reward_size*difficulty + (direction + reward_size + trial + trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2o <- glmer(response ~ group*reward_size*difficulty + (direction + difficulty + trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2p <- glmer(response ~ group*reward_size*difficulty + (direction + difficulty + reward_size + trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

# addition of interaction is significant 
anova(lmer.resp_mod_1, lmer.resp_mod_2a, lmer.resp_mod_2b, lmer.resp_mod_2c, lmer.resp_mod_2d,
      lmer.resp_mod_2e, lmer.resp_mod_2f, lmer.resp_mod_2g, lmer.resp_mod_2h, lmer.resp_mod_2i,
      lmer.resp_mod_2j, lmer.resp_mod_2k, lmer.resp_mod_2l, lmer.resp_mod_2m, lmer.resp_mod_2n,
      lmer.resp_mod_2o)

anova(lmer.resp_mod_2c, lmer.resp_mod_2j, lmer.resp_mod_2k, lmer.resp_mod_2n)

summary(lmer.resp_mod_2k)
car::Anova(lmer.resp_mod_2k, type=c("III"))

model_pred = predict(lmer.resp_mod_2k, type = "response")

gng_go_mod = na.exclude(gng_go)
gng_go_mod$prediction <- model_pred

# generate predictive estimate for each subject
summ_gng_go_mod <- gng_go_mod %>%
  dplyr::group_by(sub_id, difficulty,group) %>%
  dplyr::summarise(mean_pred = mean(prediction), sem_pred = sem(prediction))

# generate df  s
summ_gng_go_mod = data.frame(summ_gng_go_mod)

overall_pred = mean(summ_gng_go_mod$mean_pred*100)

cbPalette_red = c("#0072B2", "#D55E00", "#CC79A7")

resp_pred <- ggplot(data = summ_gng_go_mod, aes(x = difficulty, y = mean_pred*100, color = difficulty, group = difficulty, fill = difficulty)) +
  geom_errorbar(width=0.7, linetype = "dashed", aes(ymax=overall_pred, ymin=overall_pred), colour="#009E73") + 
  stat_summary(fun.data = mean_cl_boot, size=1.25, color="black", position = position_nudge(x = .3, y = 0)) +
  #  stat_summary(geom = "crossbar", width = 0.12, fatten=4, color= "#233E6C", position = position_nudge(x = .3, y=0),
  #                 fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))}) +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 4, color = "black") +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 2) +
  labs(y = "Accuracy [%]", x = "Difficulty") +
  scale_fill_manual(values = cbPalette_red) +
  scale_color_manual(values= cbPalette_red) +  
  theme(legend.position="none") + 
  facet_grid(~group) +
  labs(y="Probability to hit [%]") +
  pub_theme

resp_pred

run_diagnostics(lmer.resp_mod_2k)


tab_model(lmer.resp_mod_2k, show.stat=TRUE, 
          title="Table S1: Group by Reinforcer interaction on log-Reaction timeshit")
summary(lmer.resp_mod_2k)
# beta =  0.273596   z =  2.197 p   0.028 * 
car::Anova(lmer.resp_mod_2k, type=c("III"))
# xi = 4.8258  p = 0.02804 *  
confint(lmer.resp_mod_2k, method=c("Wald"))
# group by difficulty
# 0.273596, 0.02949303  0.51769956

anova(lmer.resp_mod_2k, lmer.resp_mod_2ki)
# 7.385  3    0.06059 .

################################# gng nogo reward size x difficulty x group interaction ##############################

lmer.resp_mod_1 <- glmer(response  ~ group + reward_size + difficulty + (1|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2a <- glmer(response ~ group*reward_size + difficulty +  (1|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2b <- glmer(response ~ group*reward_size*difficulty + (1|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2c <- glmer(response ~ group*reward_size*difficulty + (trial|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2d <- glmer(response ~ group*reward_size*difficulty + (reward_size |sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2e <- glmer(response ~ group*reward_size*difficulty + (difficulty |sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2f <- glmer(response ~ group*reward_size*difficulty+ (reward_size + difficulty |sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2g <- glmer(response ~ group*reward_size*difficulty + (reward_size + difficulty + trial|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2h <- glmer(response ~ group*reward_size*difficulty + (difficulty + trial|sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2i <- glmer(response ~ group*reward_size*difficulty + (reward_size + trial |sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2j <- glmer(response ~ group*reward_size*difficulty + (direction  |sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2k <- glmer(response ~ group*reward_size*difficulty + (direction + trial |sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2l <- glmer(response ~ group*reward_size*difficulty + (direction + reward_size |sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2m <- glmer(response ~ group*reward_size*difficulty + (direction + difficulty |sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2n <- glmer(response ~ group*reward_size*difficulty + (direction + reward_size + trial + trial |sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2o <- glmer(response ~ group*reward_size*difficulty + (direction + difficulty + trial |sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2p <- glmer(response ~ group*reward_size*difficulty + (direction + difficulty + reward_size + trial |sub_id), data=gng_nogo, family = binomial, control = glmerControl(optimizer = "bobyqa"))

# addition of interaction is significant 
anova(lmer.resp_mod_1, lmer.resp_mod_2a, lmer.resp_mod_2b, lmer.resp_mod_2c, lmer.resp_mod_2d,
      lmer.resp_mod_2e, lmer.resp_mod_2f, lmer.resp_mod_2g, lmer.resp_mod_2h, lmer.resp_mod_2i,
      lmer.resp_mod_2j, lmer.resp_mod_2k, lmer.resp_mod_2l, lmer.resp_mod_2m, lmer.resp_mod_2n,
      lmer.resp_mod_2o)

anova(lmer.resp_mod_2j, lmer.resp_mod_2e, lmer.resp_mod_2h, lmer.resp_mod_2g)

# for primary c is winning model, for secondary h is winning model 
summary(lmer.resp_mod_2h)
car::Anova(lmer.resp_mod_2h, type=c("III"))


model_pred = predict(lmer.resp_mod_2h, type="response")

gng_nogo_mod = gng_nogo
gng_nogo_mod$prediction <- model_pred

# generate predictive estimate for each subject
summ_gng_nogo_mod <- gng_nogo_mod %>%
  dplyr::group_by(sub_id, reward_size, group) %>%
  dplyr::summarise(mean_pred = mean(prediction), sem_pred = sem(prediction))

# generate df  s
summ_gng_nogo_mod = data.frame(summ_gng_nogo_mod)

overall_pred = mean(summ_gng_nogo_mod$mean_pred)

cbPalette_red = c("#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
mean_pred <- ggplot(data = summ_gng_nogo_mod, aes(x = reward_size, y = mean_pred, color = reward_size, group = reward_size, fill = reward_size)) +
  geom_errorbar(width=0.7, linetype = "dashed", aes(ymax=overall_pred, ymin=overall_pred), colour="#009E73") + 
  stat_summary(fun.data = mean_cl_boot, size=1.25, color="black", position = position_nudge(x = .3, y = 0)) +
  #  stat_summary(geom = "crossbar", width = 0.12, fatten=4, color= "#233E6C", position = position_nudge(x = .3, y=0),
  #                 fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))}) +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 4, color = "black") +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 2) +
  labs(y = "Accuracy [%]", x = "Reward Size") +
  scale_fill_manual(values = cbPalette_red) +
  scale_color_manual(values= cbPalette_red) +  
  theme(legend.position="none") + 
  facet_grid(~group) +
  labs(y="Probability to correctly reject [%]") +
  pub_theme

mean_pred

tab_model(lmer.resp_mod_2h, show.stat=TRUE,
          title="Table S1: Group by Reinforcer interaction on log-Reaction")

summary(lmer.resp_mod_2h)
# beta = 0.291783   z = 2.021   p = 0.0433 * 
car::Anova(lmer.resp_mod_2h, type=c("III"))
# xi = 4.0835, p = 0.0433 *

confint(lmer.resp_mod_2h, method=c("Wald"))
#  beta = 0.291783,  0.008778798  0.5747864

############################## correct responses full model ##########################################################

lmer.resp_mod_1 <- glmer(response  ~ group + reinforcer + reward_size + difficulty + (1|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2a <- glmer(response ~ group*reward_size + reinforcer + difficulty +  (1|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2b <- glmer(response ~ group*reinforcer + reward_size + difficulty + (1|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2c <- glmer(response ~ group*difficulty +  reinforcer + reward_size + (1|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2d <- glmer(response ~ group*reward_size*reinforcer + difficulty + (1|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2e <- glmer(response ~ group*reward_size*reinforcer*difficulty + (1|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2f <- glmer(response ~ group*reward_size*reinforcer*difficulty + (trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2g <- glmer(response ~ group*reward_size*difficulty*reinforcer + (reward_size |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2h <- glmer(response ~ group*reward_size*difficulty*reinforcer + (difficulty |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2i <- glmer(response ~ group*reward_size*reinforcer*difficulty + (direction|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2j <- glmer(response ~ group*reward_size*reinforcer*difficulty + (reinforcer|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2z1 <- glmer(response ~ group*reward_size*difficulty*reinforcer + (difficulty + reinforcer |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2z2 <- glmer(response ~ group*reward_size*difficulty*reinforcer + (difficulty + direction |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2z2 <- glmer(response ~ group*reward_size*difficulty*reinforcer + (difficulty + reward_size |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2k <- glmer(response ~ group*reward_size*difficulty*reinforcer + (trial + reward_size |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2l <- glmer(response ~ group*reward_size*difficulty*reinforcer + (trial + difficulty + trial|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2m <- glmer(response ~ group*reward_size*difficulty*reinforcer + (trial + direction|sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2n <- glmer(response ~ group*reward_size*difficulty*reinforcer + (trial + reinforcer |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2o <- glmer(response ~ group*reward_size*difficulty*reinforcer + (trial + reward_size + difficulty |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2p <- glmer(response ~ group*reward_size*difficulty*reinforcer + (trial + reward_size + direction |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2r <- glmer(response ~ group*reward_size*difficulty*reinforcer + (trial + reward_size + reinforcer |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2t <- glmer(response ~ group*reward_size*difficulty*reinforcer + (trial + difficulty + reinforcer |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2u <- glmer(response ~ group*reward_size*difficulty*reinforcer + (trial + direction + reinforcer |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2v <- glmer(response ~ group*reward_size*difficulty*reinforcer + (reward_size + difficulty + reinforcer |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2z <- glmer(response ~ group*reward_size*difficulty*reinforcer + (reward_size + difficulty + direction |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2p <- glmer(response ~ group*reward_size*difficulty*reinforcer + (direction + difficulty + reward_size + trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2p1 <- glmer(response ~ group*reward_size*difficulty*reinforcer + (direction + difficulty + reward_size + trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2p2 <- glmer(response ~ group*reward_size*difficulty*reinforcer + (reinforcer + difficulty + reward_size + trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2p3 <- glmer(response ~ group*reward_size*difficulty*reinforcer + (direction + reinforcer + reward_size + trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))
lmer.resp_mod_2p4 <- glmer(response ~ group*reward_size*difficulty*reinforcer + (direction + difficulty + reinforcer + trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

lmer.resp_mod_2s <- glmer(response ~ group*reward_size*difficulty*reinforcer + (direction + difficulty + reward_size + reinforcer + trial |sub_id), data=na.exclude(gng_go), family = binomial, control = glmerControl(optimizer = "bobyqa"))

# addition of interaction is significant 
anova(lmer.resp_mod_1, lmer.resp_mod_2a, lmer.resp_mod_2b, lmer.resp_mod_2c, lmer.resp_mod_2d,
      lmer.resp_mod_2e, lmer.resp_mod_2f, lmer.resp_mod_2g, lmer.resp_mod_2h, lmer.resp_mod_2i,
      lmer.resp_mod_2j, lmer.resp_mod_2k, lmer.resp_mod_2l, lmer.resp_mod_2m, lmer.resp_mod_2n,
      lmer.resp_mod_2o, lmer.resp_mod_2p, lmer.resp_mod_2r, lmer.resp_mod_2s, lmer.resp_mod_2t, 
      lmer.resp_mod_2u, lmer.resp_mod_2v, lmer.resp_mod_2z, lmer.resp_mod_2z1, lmer.resp_mod_2z2,
      lmer.resp_mod_2p1, lmer.resp_mod_2p2, lmer.resp_mod_2p3, lmer.resp_mod_2p4, lmer.resp_mod_2s)

# highest chisq model 2m
anova(lmer.resp_mod_2i, lmer.resp_mod_2m, lmer.resp_mod_2s, lmer.resp_mod_2u, lmer.resp_mod_2z, lmer.resp_mod_2p3)

summary(lmer.resp_mod_2m)
car::Anova(lmer.resp_mod_2m, type=c("III"))

model_pred = predict(lmer.resp_mod_2m, type="response")

gng_go_mod = na.exclude(gng_go)
gng_go_mod$prediction <- model_pred

# generate predictive estimate for each subject
summ_gng_go_mod <- gng_go_mod %>%
  dplyr::group_by(sub_id, reinforcer, group) %>%
  dplyr::summarise(mean_pred = mean(prediction), sem_pred = sem(prediction))

# generate df  s
summ_gng_go_mod = data.frame(summ_gng_go_mod)

overall_pred = mean(summ_gng_go_mod$mean_pred)

mean_pred <- ggplot(data = summ_gng_go_mod, aes(x = reinforcer, y = mean_pred, color = reinforcer, group = reinforcer, fill = reinforcer)) +
  geom_errorbar(width=0.7, linetype = "dashed", aes(ymax=overall_pred, ymin=overall_pred), colour="#009E73") + 
  stat_summary(fun.data = mean_cl_boot, size=1.25, color="black", position = position_nudge(x = .3, y = 0)) +
  #  stat_summary(geom = "crossbar", width = 0.12, fatten=4, color= "#233E6C", position = position_nudge(x = .3, y=0),
  #                 fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))}) +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 4, color = "black") +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 2) +
  labs(y = "Accuracy [%]", x = "Reinforcer") +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values= cbPalette) +  
  theme(legend.position="none") + 
  facet_grid(~group) +
  labs(y="Probability to hit [%]") +
  pub_theme

mean_pred

tab_model(lmer.resp_mod_2m, show.stat=TRUE, p.val = c("satterthwaite"),
          title="Table S1: Group by Reinforcer interaction on log-Reaction timeshit")

############################## RT full model ##########################################################

gng_go$log_rt = log(gng_go$rt)

lmer.resp_mod_1 <- lmer(log_rt  ~ group + reinforcer + reward_size + difficulty + (1|sub_id), data=na.exclude(gng_go), REML = FALSE)

lmer.resp_mod_2a <- lmer(log_rt ~ group*reward_size + reinforcer + difficulty +  (1|sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2b <- lmer(log_rt ~ group*reinforcer + reward_size + difficulty + (1|sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2c <- lmer(log_rt ~ group*difficulty +  reinforcer + reward_size + (1|sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2d <- lmer(log_rt ~ group*reward_size*reinforcer + difficulty + (1|sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2e <- lmer(log_rt ~ group*reward_size*reinforcer*difficulty + (1|sub_id), data=na.exclude(gng_go), REML = FALSE)

lmer.resp_mod_2f <- lmer(log_rt ~ group*reward_size*reinforcer*difficulty + (trial |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2g <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (reward_size |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2h <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (difficulty |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2i <- lmer(log_rt ~ group*reward_size*reinforcer*difficulty + (direction|sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2j <- lmer(log_rt ~ group*reward_size*reinforcer*difficulty + (reinforcer|sub_id), data=na.exclude(gng_go), REML = FALSE)

lmer.resp_mod_2z1 <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (difficulty + reinforcer |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2z2 <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (difficulty + direction |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2z2 <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (difficulty + reward_size |sub_id), data=na.exclude(gng_go), REML = FALSE)

lmer.resp_mod_2k <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (trial + reward_size |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2l <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (trial + difficulty + trial|sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2m <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (trial + direction|sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2n <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (trial + reinforcer |sub_id), data=na.exclude(gng_go), REML = FALSE)

lmer.resp_mod_2o <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (trial + reward_size + difficulty |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2p <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (trial + reward_size + direction |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2r <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (trial + reward_size + reinforcer |sub_id), data=na.exclude(gng_go), REML = FALSE)

lmer.resp_mod_2t <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (trial + difficulty + reinforcer |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2u <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (trial + direction + reinforcer |sub_id), data=na.exclude(gng_go), REML = FALSE)

lmer.resp_mod_2v <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (reward_size + difficulty + reinforcer |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2z <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (reward_size + difficulty + direction |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2p <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (direction + difficulty + reward_size + trial |sub_id), data=na.exclude(gng_go), REML = FALSE)

lmer.resp_mod_2p1 <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (direction + difficulty + reward_size + trial |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2p2 <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (reinforcer + difficulty + reward_size + trial |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2p3 <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (direction + reinforcer + reward_size + trial |sub_id), data=na.exclude(gng_go), REML = FALSE)
lmer.resp_mod_2p4 <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (direction + difficulty + reinforcer + trial |sub_id), data=na.exclude(gng_go), REML = FALSE)

lmer.resp_mod_2s <- lmer(log_rt ~ group*reward_size*difficulty*reinforcer + (direction + difficulty + reward_size + reinforcer + trial |sub_id), data=na.exclude(gng_go), REML = FALSE)

# addition of interaction is significant 
anova(lmer.resp_mod_1, lmer.resp_mod_2a, lmer.resp_mod_2b, lmer.resp_mod_2c, lmer.resp_mod_2d,
      lmer.resp_mod_2e, lmer.resp_mod_2f, lmer.resp_mod_2g, lmer.resp_mod_2h, lmer.resp_mod_2i,
      lmer.resp_mod_2j, lmer.resp_mod_2k, lmer.resp_mod_2l, lmer.resp_mod_2m, lmer.resp_mod_2n,
      lmer.resp_mod_2o, lmer.resp_mod_2p, lmer.resp_mod_2r, lmer.resp_mod_2s, lmer.resp_mod_2t, 
      lmer.resp_mod_2u, lmer.resp_mod_2v, lmer.resp_mod_2z, lmer.resp_mod_2z1, lmer.resp_mod_2z2,
      lmer.resp_mod_2p1, lmer.resp_mod_2p2, lmer.resp_mod_2p3, lmer.resp_mod_2p4, lmer.resp_mod_2s)

# 
anova(lmer.resp_mod_2f, lmer.resp_mod_2j, lmer.resp_mod_2s, lmer.resp_mod_2u, lmer.resp_mod_2z, lmer.resp_mod_2p3)

summary(lmer.resp_mod_2m)
car::Anova(lmer.resp_mod_2m, type=c("III"))


anova(lmer.resp_mod_1, lmer.resp_mod_2a, lmer.resp_mod_2b, lmer.resp_mod_2c, lmer.resp_mod_2d,
      lmer.resp_mod_2e, lmer.resp_mod_2f, lmer.resp_mod_2g, lmer.resp_mod_2h, lmer.resp_mod_2i,
      lmer.resp_mod_2j, lmer.resp_mod_2k, lmer.resp_mod_2l, lmer.resp_mod_2m, lmer.resp_mod_2n,
      lmer.resp_mod_2o, lmer.resp_mod_2p)

anova(lmer.resp_mod_2j, lmer.resp_mod_2k, lmer.resp_mod_2n)

summary(lmer.resp_mod_2c)
car::Anova(lmer.resp_mod_2c, type=c("III"))

secondary_resp_plot = plot_model(lmer.resp_mod_2c, type="pred", terms = c("group", "reinforcer", "reward_size", "difficulty"), color = cbPalette) 
secondary_resp_plot

run_diagnostics(lmer.resp_mod_2k)

model_pred = predict(lmer.resp_mod_2c)

gng_go_mod = na.exclude(gng_go)
gng_go_mod$prediction <- model_pred

# generate predictive estimate for each subject
summ_gng_go_mod <- gng_go_mod %>%
  dplyr::group_by(sub_id, reward_size,reinforcer, group) %>%
  dplyr::summarise(mean_pred = mean(prediction), sem_pred = sem(prediction))

# generate df  s
summ_gng_go_mod = data.frame(summ_gng_go_mod)

overall_pred = exp(mean(summ_gng_go_mod$mean_pred))

mean_pred <- ggplot(data = summ_gng_go_mod, aes(x = reinforcer, y = exp(mean_pred), color = reinforcer, group = reinforcer, fill = reinforcer)) +
  geom_errorbar(width=0.7, linetype = "dashed", aes(ymax=overall_pred, ymin=overall_pred), colour="#009E73") + 
  stat_summary(fun.data = mean_cl_boot, size=1.25, color="black", position = position_nudge(x = .3, y = 0)) +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 4, color = "black") +
  geom_point(position = position_jitter(width=.2, seed = 1), size = 2) +
  labs(y = "Accuracy [%]", x = "Reinforcer") +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values= cbPalette) +  
  theme(legend.position="none") + 
  facet_grid(reward_size~group) +
  labs(y="Probability to hit [%]") +
  pub_theme

mean_pred
