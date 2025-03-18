###################################################################################################################
################################ load up libraries & setup prelims ################################################
###################################################################################################################
devtools::install_github("zeehio/facetscales")

libs = c("car", "Hmisc", "ggpubr", "devtools", "facetscales", "BayesFactor")

for (i in 1:length(libs)) {
  install.packages(libs[i])
}

lapply(libs, require, character.only = TRUE)


# set path and load in files
mypath = ".../Analysis/rstudio_cloud/util/"
setwd(mypath)
util_fncs= list.files(mypath, pattern = "*.R")
lapply(util_fncs, source)

# set up plotting variables 
pp <- position_jitter(width=.15, seed = 123)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


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

#####################################################################################################################
################################ load and organise data for fig 1 ###################################################
#####################################################################################################################

mypath = ".../rstudio_cloud/compiled/"
setwd(mypath)

# load in everything
ev_files = list.files(path=mypath, pattern="*ev.txt") 
ev_df <- lapply(ev_files, function(x) {read.table(file = x, skip= 1, header = F, sep ="\t")})
ev_df <- do.call("rbind", lapply(ev_df, as.data.frame)) 

# assign column names
colnames(ev_df) <- c("trial", "question1", "answer1", "rt1", "question2", "answer2", "rt2", "question3", "answer3", "rt3", "reinforcer", "sub_id")

# create composite score
ev_df$score <- (ev_df$answer1 + ev_df$answer2 + ev_df$answer3)/3
ev_df$score_rt <- (ev_df$rt1 + ev_df$rt2 + ev_df$rt3)/3

ev_df$group <- c(rep("Adults", times = 52), rep("Children", times = 46), rep("Adults", times = 38), rep("Children", times = 23))
ev_df$group <- as.factor(ev_df$group)

# need to change one 129 into an additional ID, this is consistent across tasks due to naming error
ev_df$sub_id[61:62] <- 148
ev_df =  ev_df[ev_df$sub_id != 129,]

# examine means
summ_ev_df <- ev_df %>%
  dplyr::group_by(group, reinforcer) %>%
  dplyr::summarise(mean_answer = mean(score), se_answer = sem(score), 
                   mean_rt = mean(score_rt), se_answer_rt = sem(score_rt))
summ_ev_df
#1 Adults   " primary "          4.38     0.156    3.98        0.216
#2 Adults   " secondary "        5.08     0.195    5.15        1.01 
#3 Children " primary "          4.75     0.189    5.55        0.355
#4 Children " secondary "        4.36     0.252    5.86        0.546

# examine same interaction while controlling for trial presentation
out = lm(score ~ group*reinforcer + trial, data = ev_df)
car::Anova(out, type="III")
#group              2.48   1   1.6262  0.204181    
#reinforcer        11.17   1   7.3232  0.007585 ** 
#trial              0.39   1   0.2559  0.613688    
#group:reinforcer  11.53   1   7.5556  0.006708 ** 

# sub-group t-tests
# need to remove non duplicated IDs
ev_df$sub_id[ev_df$group == 'Adults']
rem = c(153, 154, 155, 159, 160, 161, 165, 166, 167, 171, 172, 173)

rel_df = ev_df[ev_df$group == 'Adults', ]
for (i in 1:length(rem)) {
  rel_df$score[rel_df$sub_id==rem[i]] = NaN;
}

adul_prim = rel_df$score[rel_df$group == 'Adults' & rel_df$reinforcer == ' primary ']
adul_sec  = rel_df$score[rel_df$group == 'Adults' & rel_df$reinforcer == ' secondary ']

t.test(adul_prim[complete.cases(adul_prim)], adul_sec[complete.cases(adul_sec)], paired=TRUE)
# t = -3.1549, df = 38, p-value = 0.003135

# same for children
ev_df$sub_id[ev_df$group == 'Children']
rem = c(177, 178, 179, 180, 183, 184, 185, 189, 190)

rel_df = ev_df[ev_df$group == 'Children', ]
for (i in 1:length(rem)) {
  rel_df$score[rel_df$sub_id==rem[i]] = NaN;
}

chil_prim = rel_df$score[rel_df$group == 'Children' & rel_df$reinforcer == ' primary ']
chil_sec  = rel_df$score[rel_df$group == 'Children' & rel_df$reinforcer == ' secondary ']

t.test(chil_prim[complete.cases(chil_prim)], chil_sec[complete.cases(chil_sec)], paired=TRUE)
# t = 0.75763, df = 28, p-value = 0.455

# compute bayes factor for additional confirmation
tmp = ttestBF(x = chil_prim[complete.cases(chil_prim)], y = chil_sec[complete.cases(chil_sec)], rscale = "medium", paired = TRUE)
1/tmp
# 3.89

# compute mean val for plotting purposes
ev_df_sort <-
  ev_df %>%
  arrange(sub_id, group, reinforcer)

overall_score = mean(ev_df$score)
ev_df$group <- factor(ev_df$group, levels = c("Children", "Adults"))

# -------------- plot first part of the figure -------------- #
ev_val <- ggplot(data = ev_df, aes(x = reinforcer, y = score, color = reinforcer, group = reinforcer, fill = reinforcer)) +
  geom_errorbar(width=0.7, linetype = "dashed", aes(ymax=overall_score, ymin=overall_score), colour="#009E73") + 
  stat_summary(fun.data = mean_cl_boot, size=1.25, color="black", position = position_nudge(x = .3, y = 0)) +
  geom_point(position = position_jitter(width=.125, seed = 1), size = 4, color = "black") +
  geom_point(position = position_jitter(width=.125, seed = 1), size = 2) +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values= cbPalette) +  
  theme(legend.position="none") + 
  facet_wrap(~group) +
  ylab("Subjective value [a.u.]") + xlab("Condition") +
  pub_theme

ev_val

# key tests across groups 
t.test(ev_df$score[ev_df$reinforcer == " secondary " & ev_df$group==  "Adults"],
       ev_df$score[ev_df$reinforcer == " secondary " & ev_df$group== "Children"])
# t = 2.2561, df = 66.321, p-value = 0.02737

t.test(ev_df$score[ev_df$reinforcer == " primary " & ev_df$group== "Adults"],
       ev_df$score[ev_df$reinforcer == " primary " & ev_df$group== "Children"])
# t = -1.5081, df = 67.57, p-value = 0.1362


# -------------- load in subjects perception of pound value  -------------- #
## add the perceived value of primary reinforcer
primary_price_eval  = list.files(path=mypath, pattern="*valuation.txt") 
primary_price_eval <- lapply(primary_price_eval, function(x) {read.table(file = x, header = F, sep ="\t")})
primary_price_eval <- do.call("rbind", lapply(primary_price_eval, as.data.frame))
colnames(primary_price_eval) <- c("value", "id")

primary_price_eval$group = rep(c(rep(1, times = 26), rep(2, times = 22), rep(1, times = 25), rep(2, times = 15)))

# convert strings to their numeric values
primary_price_eval$value[39] = 20
primary_price_eval$value[74] = 50
primary_price_eval$value[83] = 20
primary_price_eval$value[85] = 1
primary_price_eval$value[86] = 100

# relabel the IDs to be correct
primary_price_eval[primary_price_eval$id == 129,]
primary_price_eval$id[31]    = 148
primary_price_eval[30,]      = NA
primary_price_eval$id[79]    = 180

primary_price_eval = na.exclude(primary_price_eval)
primary_price_eval$value = log(as.numeric(primary_price_eval$value))

# test for group differences in perceived worth of juice
t.test(primary_price_eval$value[primary_price_eval$group == 1], primary_price_eval$value[primary_price_eval$group == 2], paired=FALSE)
# t = -0.7602, df = 82.202, p-value = 0.4493

# rename for plot
primary_price_eval$group = as.factor(primary_price_eval$group)
primary_price_eval$group = recode_factor(primary_price_eval$group, "1" = "Adults", "2" = "Children")
primary_price_eval$group = factor(primary_price_eval$group, levels = c("Children", "Adults"))

# generate mean
overall_score_prim = mean(primary_price_eval$value)

# take new colors 
cbPalette_n = cbPalette[3:5]

# -------------- plot remainder of fig1 and concatenate everything  -------------- #
val_worth <- ggplot(data = primary_price_eval, aes(x = group, y = value, color = group, fill = group)) +
  geom_errorbar(width=0.7, linetype = "dashed", aes(ymax=overall_score_prim, ymin=overall_score_prim), colour="#009E73") + 
  stat_summary(fun.data = mean_cl_boot, size=1.25, color="black", position = position_nudge(x = .3, y = 0)) +
  geom_point(position = position_jitter(width=.125, seed = 1), size = 4, color = "black") +
  geom_point(position = position_jitter(width=.125, seed = 1), size = 2) +
  scale_fill_manual(values = cbPalette_n) +
  scale_color_manual(values= cbPalette_n) +  
  theme(legend.position="none") + 
  ylab("Perceived worth [a.u.]") + xlab("Group") +
  pub_theme

val_worth

cowplot::plot_grid(ev_val, val_worth, rel_widths=c(2, 1))

# -------------- check initial interaction with perceived worth as a covariate  -------------- #
# initial interaction with perceived worth as a covariate
for (i in 1:length(primary_price_eval$id)){
  curr_id = primary_price_eval$id[i]
  ev_df$worth[ev_df$sub_id == curr_id] <- primary_price_eval$value[primary_price_eval$id == curr_id]
}

out = lm(score ~ group*reinforcer + trial + worth, data = ev_df)
car::Anova(out, type="III")
# group              2.725   1   1.7652  0.186033    
# reinforcer         2.923   1   1.8933  0.170920    
# trial              0.666   1   0.4316  0.512210    
# worth              2.316   1   1.4999  0.222649    
# group:reinforcer  12.055   1   7.8086  0.005894 ** 
# Residuals        226.946 147          

eff_sizes(out)

####################################################################################################################
################################ load in the control questions  ####################################################
####################################################################################################################
mypath = ".../rstudio_cloud/compiled/"
setwd(mypath)

iniq_files = list.files(path=mypath, pattern="*initial_q.csv).txt") 
iniq_df <- lapply(iniq_files, function(x) {read.table(file = x, header = T, sep ="\t")})
iniq_df <- do.call("rbind", lapply(iniq_df, as.data.frame))

iniq_df$group = c(rep(1, times = 26), rep(2, times = 23), rep(1, times = 25), rep(2, times = 16)) # 2nd part new add
iniq_df[iniq_df$sub_id==129,]
# these need to have their IDs adjusted to be correct and in line with the rest of the datapoints
iniq_df$sub_id[31] = 148
iniq_df$sub_id[80] = 180 # new add
iniq_df[30,] = NA
iniq_df = na.exclude(iniq_df)

# supplementary tests for group differences
t.test(iniq_df$init_quest_1[iniq_df$group==1], iniq_df$init_quest_1[iniq_df$group==2])
# t = 3.4283, df = 64.913, p-value = 0.00106
t.test(iniq_df$init_quest_2[iniq_df$group==1], iniq_df$init_quest_2[iniq_df$group==2])
# t = 3.6722, df = 72.408, p-value = 0.000457
t.test(iniq_df$init_quest_3[iniq_df$group==1], iniq_df$init_quest_3[iniq_df$group==2])
# t = 5.1017, df = 56.558, p-value = 4.077e-06
