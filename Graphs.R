
install.packages("data.table")
install.packages("compute.es")
install.packages("effectsize")
install.packages("clubSandwich")
install.packages("esc")

library(meta)
library(effectsize)
library(compute.es)
library(data.table)
library(tidyverse)
library(dplyr)
library(metafor)
library(metaforest)
require(devtools)
library(clubSandwich)
library(ggplot2)
library(esc)


svg(file='forestplot_TG_eidry.svg') # Open svg device with specific file name
forest(m_TG_eidry, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       xlim=c(-2.5,1.8),
       ilab = cbind(alldata5$N_comb_TG_ctrl, alldata5$N_comb_TG_eidry),
       ilab.xpos = c(-0.9,-0.6),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       cex =0.7,
       col = "blue")
text(-.75, 16.1, "Sample Size", cex=.7, font=2)
text(c(-.93,-.64), 15.5, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-2.5, -2.3), -2, pos = 4, c(expression(I[study]^2), "= 88.12%"), cex=0.7)
dev.off() # Turn the svg device off)


svg(file='forestplot_TG_advdry.svg') # Open svg device with specific file name
forest(m_TG_advdry, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       
      alim = c(-1.5,0.5),
       ilab = cbind(alldata5$N_comb_TG_ctrl, alldata5$N_comb_TG_advdry),
       ilab.xpos = c(-1.2,-0.9),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       cex =0.7,
       col = "blue")
text(-1.05, 4, "Sample Size", cex=.7, font=2)
text(c(-1.2,-.9), 3.5, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-2.37, -2.2), -1.5, pos = 4, c(expression(I[study]^2), "= 19.58%"), cex=0.7)
dev.off() # Turn the svg device off))


svg(file='forestplot_TG_advwet.svg') # Open svg device with specific file name
forest(m_TG_advwet, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       xlim=c(-5,3),
       ilab = cbind(alldata5$N_comb_TG_ctrl, alldata5$N_comb_TG_advwet),
       ilab.xpos = c(-2.8,-2.3),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       cex =0.7,
       col = "blue")
text(-2.55, 8, "Sample Size", cex=.7, font=2)
text(c(-2.8,-2.3), 7.5, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-5, -4.7), -1.5, pos = 4, c(expression(I[study]^2), "= 95.96%"), cex=0.7)
dev.off() # Turn the svg device off)
       

svg(file='forestplot_TC_allAMD.svg') # Open svg device with specific file name
forest(m_TC_allAMD, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       alim=c(-2,2),
       xlim = c(-6,4),
       ilab = cbind(alldata5$N_comb_TC_ctrl, alldata5$N_comb_TC_allAMD),
       ilab.xpos = c(-2.3,-1.8),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       cex =0.5,
       col = "blue")
text(-2.0, 72, "Sample Size", cex=.5, font=2)
text(c(-2.3,-1.8), 70.9, c("Ctrl","all AMD"), cex=.5, font=1)
text(c(-6, -5.5), -2.6, pos = 4, c(expression(I[study]^2), "= 78.04%"), cex=0.5)
dev.off() # Turn the svg device off)

svg(file='forestplot_TC_eidry.svg') # Open svg device with specific file name
forest(m_TC_eidry, 
       slab = alldata5$StudyID, 
       addcred = TRUE, 
       showweights = TRUE, 
       ilab = cbind(alldata5$N_comb_TC_ctrl, alldata5$N_comb_TC_eidry),
       ilab.xpos = c(-1.7,-1.4),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       xlim = c(-3,1.6),
       alim = c(-1.5,.5),
       cex =0.7,
       col = "blue")
text(-1.55, 28.5, "Sample Size", cex=.7, font=2)
text(c(-1.7,-1.4), 27.7, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-3, -2.8), -2.1, pos = 4, c(expression(I[study]^2), "= 93.96%"), cex=0.7)
dev.off() # Turn the svg device off

svg(file='forestplot_TC_advdry.svg') # Open svg device with specific file name
forest(m_TC_advdry, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       ilab = cbind(alldata5$N_comb_TC_ctrl, alldata5$N_comb_TC_advdry),
       ilab.xpos = c(-1,-0.8),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       xlim = c(-2,1.3),
       cex =0.7,
       col = "blue")
text(-0.9, 6, "Sample Size", cex=.7, font=2)
text(c(-1,-0.8), 5.7, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-2, -1.7), -1.5, pos = 4, c(expression(I[study]^2), "= 0.00%"), cex=0.7)
dev.off() # Turn the svg device off)

svg(file='forestplot_TC_advwet.svg') # Open svg device with specific file name
forest(m_TC_advwet, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       ilab = cbind(alldata5$N_comb_TC_ctrl, alldata5$N_comb_TC_advwet),
       ilab.xpos = c(-0.9,-0.6),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       xlim = c(-2,2),
       cex =0.7,
       col = "blue")
text(-0.75, 10, "Sample Size", cex=.7, font=2)
text(c(-.9,-0.6), 9.5, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-2, -1.7), -1.5, pos = 4, c(expression(I[study]^2), "= 43.27%"), cex=0.7)
dev.off() # Turn the svg device off

svg(file='forestplot_HDL_allAMD.svg') # Open svg device with specific file name
forest(m_HDL_allAMD, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       ilab = cbind(alldata5$N_comb_HDL_ctrl, alldata5$N_comb_HDL_allAMD),
       ilab.xpos = c(-2.3,-1.75),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       xlim = c(-6,3),
       steps = 6,
       alim = c(-2,3),
       cex =0.5,
       col = "blue")
text(-2.05, 69, "Sample Size", cex=.5, font=2)
text(c(-2.3,-1.75), 67.7, c("Ctrl","all AMD"), cex=.5, font=1)
text(c(-6, -5.7), -2.5, pos = 4, c(expression(I[study]^2), "= 99.99%"), cex=0.5)
dev.off() # Turn the svg device off

svg(file='forestplot_HDL_eidry.svg') # Open svg device with specific file name
forest(m_HDL_eidry, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       ilab = cbind(alldata5$N_comb_HDL_ctrl, alldata5$N_comb_HDL_eidry),
       ilab.xpos = c(-0.9,-0.6),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       xlim = c(-3,2),
       cex =0.7,
       col = "blue")
text(-0.75, 25.5, "Sample Size", cex=.7, font=2)
text(c(-.9,-0.6), 24.7, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-3, -2.8), -2, pos = 4, c(expression(I[study]^2), "= 99.00%"), cex=0.7)
dev.off() # Turn the svg device off

svg(file='forestplot_HDL_advdry.svg') # Open svg device with specific file name
forest(m_HDL_advdry, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       ilab = cbind(alldata5$N_comb_HDL_ctrl, alldata5$N_comb_HDL_advdry),
       ilab.xpos = c(-1.2,-0.9),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       xlim = c(-2.5,2),
       cex =0.7,
       col = "blue")
text(-1.05, 6, "Sample Size", cex=.7, font=2)
text(c(-1.2,-0.9), 5.5, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-2.5, -2.3), -1.5, pos = 4, c(expression(I[study]^2), "= 0.00%"), cex=0.7)
dev.off() # Turn the svg device off

svg(file='forestplot_HDL_advwet.svg') # Open svg device with specific file name
forest(m_HDL_advwet, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       ilab = cbind(alldata5$N_comb_HDL_ctrl, alldata5$N_comb_HDL_advwet),
       ilab.xpos = c(-1.4,-1.1),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       xlim = c(-2.7,2),
       cex =0.7,
       col = "blue")
text(-1.25, 10, "Sample Size", cex=.7, font=2)
text(c(-1.4,-1.1), 9.5, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-2.7, -2.5), -1.5, pos = 4, c(expression(I[study]^2), "= 23.55%"), cex=0.7)
dev.off() # Turn the svg device off

svg(file='forestplot_LDL_allAMD.svg') # Open svg device with specific file name
forest(m_LDL_allAMD, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       ilab = cbind(alldata5$N_comb_LDL_ctrl, alldata5$N_comb_LDL_allAMD),
       ilab.xpos = c(-1.75,-1.05),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       xlim = c(-7,6.5),
       cex =0.7,
       col = "blue")
text(-1.4, 47.5, "Sample Size", cex=.7, font=2)
text(c(-1.75,-1.05), 46.5, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-7, -6.5), -2.5, pos = 4, c(expression(I[study]^2), "= 99.98%"), cex=0.7)
dev.off() # Turn the svg device off

svg(file='forestplot_LDL_eidry.svg') # Open svg device with specific file name
forest(m_LDL_eidry, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       ilab = cbind(alldata5$N_comb_LDL_ctrl, alldata5$N_comb_LDL_eidry),
       ilab.xpos = c(-.35,-.25),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       xlim = c(-1,.65),
       cex =0.7,
       col = "blue")
text(-.3, 13, "Sample Size", cex=.7, font=2)
text(c(-.35,-.25), 12.5, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-1, -.9), -1.5, pos = 4, c(expression(I[study]^2), "= 91.65%"), cex=0.7)
dev.off() # Turn the svg device off

svg(file='forestplot_LDL_advdry.svg') # Open svg device with specific file name
forest(m_LDL_advdry, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE, 
       xlim=c(-5,4), 
       alim=c(-2,2),
       ilab = cbind(alldata5$N_comb_LDL_ctrl, alldata5$N_comb_LDL_advdry),
       ilab.xpos = c(-3,-2.5),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       cex =0.7,
       col = "blue")
text(-2.75, 4, "Sample Size", cex=.7, font=2)
text(c(-3,-2.5), 3.7, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-5, -4.6), -1.5, pos = 4, c(expression(I[study]^2), "= 0.00%"), cex=0.7)
dev.off() # Turn the svg device off

svg(file='forestplot_LDL_advwet.svg') # Open svg device with specific file name
forest(m_LDL_advwet, 
       slab = alldata5$ID_2, 
       addcred = TRUE, 
       showweights = TRUE,
       ilab = cbind(alldata5$N_comb_LDL_ctrl, alldata5$N_comb_LDL_advwet),
       ilab.xpos = c(-.9,-.7),
       header = c("Author (Year)", "Weight    SMD       [95% CI]"),
       xlim = c(-1.8,2),
       cex =0.7,
       col = "blue")
text(-.8, 8, "Sample Size", cex=.7, font=2)
text(c(-.9,-.7), 7.6, c("Ctrl","all AMD"), cex=.70, font=1)
text(c(-1.8, -1.5), -1.5, pos = 4, c(expression(I[study]^2), "= 91.65%"), cex=0.7)
dev.off() # Turn the svg device off)






###TG_allAMD
#outlier analysis
m_TG_allAMD$ci.lb
m_TG_allAMD$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_TG_allAMD <- alldata5$yi_TG_ctrl_allAMD_comb + 1.96 * sqrt(alldata5$vi_TG_ctrl_allAMD_comb)
alldata5$lowerci_TG_allAMD <- alldata5$yi_TG_ctrl_allAMD_comb - 1.96 * sqrt(alldata5$vi_TG_ctrl_allAMD_comb)
# Create filter variable
alldata5$outlier_TG_allAMD <- alldata5$upperci_TG_allAMD < m_TG_allAMD$ci.lb | alldata5$lowerci_TG_allAMD > m_TG_allAMD$ci.ub
# Count number of outliers:
sum(alldata5$outlier_TG_allAMD, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_TG_ctrl_allAMD_comb, colour = outlier_TG_allAMD, fill = outlier_TG_allAMD)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_TG_allAMD$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_TG_allAMD <- alldata5[-c(29, 90),] #create new df excluding the outliers

m_no_outliers_TG_allAMD <- rma.mv(yi_TG_ctrl_allAMD_comb, vi_TG_ctrl_allAMD_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_TG_allAMD)
m_no_outliers_TG_allAMD

cat("G = ", round(m_no_outliers_TG_allAMD$b[1], 2),
    ", 95% CI [", round(m_no_outliers_TG_allAMD$ci.lb, 2),
    ", ", round(m_no_outliers_TG_allAMD$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_TG_allAMD$b[1], 2),
    ", 95% CI [", round(m_TG_allAMD$ci.lb, 2),
    ", ", round(m_TG_allAMD$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_TG_allAMD <- rma(yi = yi_TG_ctrl_allAMD_comb, vi = vi_TG_ctrl_allAMD_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_TG_allAMD

m_subgroup_regression_TG_allAMD <- rma(yi = yi_TG_ctrl_allAMD_comb, vi = vi_TG_ctrl_allAMD_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_TG_allAMD

#subgroup analysis; by origin of data (OR vs. means)
alldata5$origin_data <- factor(alldata5$Ratio_type %in% c("OR", "USA/Korea"),
                                          labels = c("WARMGS", "USA"))
m_subgroup2_TG_allAMD <- rma(yi = yi_TG_ctrl_allAMD_comb, vi = vi_TG_ctrl_allAMD_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_TG_allAMD

m_subgroup2_TG_allAMD <- rma(yi = yi_TG_ctrl_allAMD_comb, vi = vi_TG_ctrl_allAMD_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_TG_allAMD

#meta-regression
m_metareg_TG_allAMD <- rma(yi = yi_TG_ctrl_allAMD_comb,
             vi = vi_TG_ctrl_allAMD_comb,
             mods = ~proportion_f,
             data = alldata5)
m_metareg_TG_allAMD

#publication bias via funnel plot
funnel(m_TG_allAMD, xlab = "Hedges' g")
funnel(m_TG_allAMD, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_TG_allAMD)

svg(file='funnel_TG_allAMD.svg') # Open svg device with specific file name
funnel(m_TG_allAMD, xlab = "Hedges' g")
funnel(m_TG_allAMD, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off

#funnelplot without outliers
funnel(m_no_outliers_TG_allAMD, xlab = "Hedges' g")
funnel(m_no_outliers_TG_allAMD, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_no_outliers_TG_allAMD)

svg(file='funnel_TG_allAMD_outliersremoved.svg') # Open svg device with specific file name
funnel(m_no_outliers_TG_allAMD, xlab = "Hedges' g")
funnel(m_no_outliers_TG_allAMD, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off


#Robust variance estimation
clubSandwich::coef_test(m_TG_allAMD, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_TG_allAMD <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_TG_allAMD <- rma.mv(yi_TG_ctrl_allAMD_comb, vi_TG_ctrl_allAMD_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_TG_allAMD)
m_no_Park2_TG_allAMD

cat("G = ", round(m_no_Park2_TG_allAMD$b[1], 2),
    ", 95% CI [", round(m_no_Park2_TG_allAMD$ci.lb, 2),
    ", ", round(m_no_Park2_TG_allAMD$ci.ub, 2), "] (no Parks)", sep = "")

cat("G = ", round(m_TG_allAMD$b[1], 2),
    ", 95% CI [", round(m_TG_allAMD$ci.lb, 2),
    ", ", round(m_TG_allAMD$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_TG_allAMD, slab = df_no_Park2_TG_allAMD$ID_2, addcred = TRUE, showweights = TRUE)

###TG_eidry#########################
#outlier analysis
m_TG_eidry$ci.lb
m_TG_eidry$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_TG_eidry <- alldata5$yi_TG_ctrl_eidry_comb + 1.96 * sqrt(alldata5$vi_TG_ctrl_eidry_comb)
alldata5$lowerci_TG_eidry <- alldata5$yi_TG_ctrl_eidry_comb - 1.96 * sqrt(alldata5$vi_TG_ctrl_eidry_comb)
# Create filter variable
alldata5$outlier_TG_eidry <- alldata5$upperci_TG_eidry < m_TG_eidry$ci.lb | alldata5$lowerci_TG_eidry > m_TG_eidry$ci.ub
# Count number of outliers:
sum(alldata5$outlier_TG_eidry, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_TG_ctrl_eidry_comb, colour = outlier_TG_eidry, fill = outlier_TG_eidry)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_TG_eidry$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
#conclusion: Park e/i dry data is outlier. This is covered in sensitivity analysis below

#sensitivity analysis for removing Park studies
df_no_Park2_TG_eidry <- alldata5[-c(26),] #create new df excluding the outliers

m_no_Park2_TG_eidry <- rma.mv(yi_TG_ctrl_eidry_comb, vi_TG_ctrl_eidry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_TG_eidry)
m_no_Park2_TG_eidry


df_no_Park2_NOSlow_TG_eidry <- alldata5[-c(32),] #create new df excluding the outliers

m_no_Park2_NOSlow_TG_eidry <- rma.mv(yi_TG_ctrl_eidry_comb, vi_TG_ctrl_eidry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_NOSlow_TG_eidry)
m_no_Park2_NOSlow_TG_eidry

#I2 for removal of Park and removal of four low-scoring NOS
W <- diag(1/m_no_Park2_TG_eidry$vi)
X <- model.matrix(m_no_Park2_TG_eidry)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(m_no_Park2_TG_eidry$sigma2) / (sum(m_no_Park2_TG_eidry$sigma2) + (m_no_Park2_TG_eidry$k-m_no_Park2_TG_eidry$p)/sum(diag(P)))

W <- diag(1/m_no_Park2_NOSlow_TG_eidry$vi)
X <- model.matrix(m_no_Park2_NOSlow_TG_eidry)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(m_no_Park2_NOSlow_TG_eidry$sigma2) / (sum(m_no_Park2_NOSlow_TG_eidry$sigma2) + (m_no_Park2_NOSlow_TG_eidry$k-m_no_Park2_NOSlow_TG_eidry$p)/sum(diag(P)))



#subgroup analysis; by AMD classfication methodology
m_subgroup1_TG_eidry <- rma(yi = yi_TG_ctrl_eidry_comb, vi = vi_TG_ctrl_eidry_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_TG_eidry

m_subgroup_regression_TG_eidry <- rma(yi = yi_TG_ctrl_eidry_comb, vi = vi_TG_ctrl_eidry_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_TG_eidry

#subgroup analysis; by origin of data (OR vs. means)
alldata5$origin_data <- factor(alldata5$Ratio_type %in% c("OR", "USA/Korea"),
                               labels = c("WARMGS", "USA"))
m_subgroup2_TG_eidry <- rma(yi = yi_TG_ctrl_eidry_comb, vi = vi_TG_ctrl_eidry_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_TG_eidry

m_subgroup2_TG_eidry <- rma(yi = yi_TG_ctrl_eidry_comb, vi = vi_TG_ctrl_eidry_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_TG_eidry

#meta-regression
m_metareg_TG_eidry <- rma(yi = yi_TG_ctrl_eidry_comb,
                           vi = vi_TG_ctrl_eidry_comb,
                           mods = ~proportion_f,
                           data = alldata5)
m_metareg_TG_eidry

#publication bias via funnel plot
funnel(m_TG_eidry, xlab = "Hedges' g")
funnel(m_TG_eidry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_TG_eidry)

svg(file='funnel_TG_eidry.svg') # Open svg device with specific file name
funnel(m_TG_eidry, xlab = "Hedges' g")
funnel(m_TG_eidry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_TG_eidry, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_TG_eidry <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_TG_eidry <- rma.mv(yi_TG_ctrl_eidry_comb, vi_TG_ctrl_eidry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_TG_eidry)
m_no_Park2_TG_eidry

cat("G = ", round(m_no_Park2_TG_eidry$b[1], 2),
    ", 95% CI [", round(m_no_Park2_TG_eidry$ci.lb, 2),
    ", ", round(m_no_Park2_TG_eidry$ci.ub, 2), "] (no Park)", sep = "")

cat("G = ", round(m_TG_eidry$b[1], 2),
    ", 95% CI [", round(m_TG_eidry$ci.lb, 2),
    ", ", round(m_TG_eidry$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_TG_eidry, slab = df_no_Park2_TG_eidry$ID_2, addcred = TRUE, showweights = TRUE)


###TG_advwet
#outlier analysis
m_TG_advwet$ci.lb
m_TG_advwet$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_TG_advwet <- alldata5$yi_TG_ctrl_advwet_comb + 1.96 * sqrt(alldata5$vi_TG_ctrl_advwet_comb)
alldata5$lowerci_TG_advwet <- alldata5$yi_TG_ctrl_advwet_comb - 1.96 * sqrt(alldata5$vi_TG_ctrl_advwet_comb)
# Create filter variable
alldata5$outlier_TG_advwet <- alldata5$upperci_TG_advwet < m_TG_advwet$ci.lb | alldata5$lowerci_TG_advwet > m_TG_advwet$ci.ub
# Count number of outliers:
sum(alldata5$outlier_TG_advwet, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_TG_ctrl_advwet_comb, colour = outlier_TG_advwet, fill = outlier_TG_advwet)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_TG_advwet$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_TG_advwet <- alldata5[-c(29),] #create new df excluding the outliers

m_no_outliers_TG_advwet <- rma.mv(yi_TG_ctrl_advwet_comb, vi_TG_ctrl_advwet_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_TG_advwet)
m_no_outliers_TG_advwet

cat("G = ", round(m_no_outliers_TG_advwet$b[1], 2),
    ", 95% CI [", round(m_no_outliers_TG_advwet$ci.lb, 2),
    ", ", round(m_no_outliers_TG_advwet$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_TG_advwet$b[1], 2),
    ", 95% CI [", round(m_TG_advwet$ci.lb, 2),
    ", ", round(m_TG_advwet$ci.ub, 2), "]", sep = "")


#subgroup analysis; by AMD classfication methodology
m_subgroup1_TG_advwet <- rma(yi = yi_TG_ctrl_advwet_comb, vi = vi_TG_ctrl_advwet_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_TG_advwet

m_subgroup_regression_TG_advwet <- rma(yi = yi_TG_ctrl_advwet_comb, vi = vi_TG_ctrl_advwet_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_TG_advwet

#subgroup analysis; by origin of data (OR vs. means)
alldata5$origin_data <- factor(alldata5$Ratio_type %in% c("OR", "USA/Korea"),
                               labels = c("WARMGS", "USA"))
m_subgroup2_TG_advwet <- rma(yi = yi_TG_ctrl_advwet_comb, vi = vi_TG_ctrl_advwet_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_TG_advwet

m_subgroup2_TG_advwet <- rma(yi = yi_TG_ctrl_advwet_comb, vi = vi_TG_ctrl_advwet_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_TG_advwet

#meta-regression
m_metareg_TG_advwet <- rma(yi = yi_TG_ctrl_advwet_comb,
                           vi = vi_TG_ctrl_advwet_comb,
                           mods = ~proportion_f,
                           data = alldata5)
m_metareg_TG_advwet

#publication bias via funnel plot
funnel(m_TG_advwet, xlab = "Hedges' g")
funnel(m_TG_advwet, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_TG_advwet)

svg(file='funnel_TG_advwet.svg') # Open svg device with specific file name
funnel(m_TG_advwet, xlab = "Hedges' g")
funnel(m_TG_advwet, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_TG_advwet, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_TG_advwet <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_TG_advwet <- rma.mv(yi_TG_ctrl_advwet_comb, vi_TG_ctrl_advwet_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_TG_advwet)
m_no_Park2_TG_advwet

cat("G = ", round(m_no_Park2_TG_advwet$b[1], 2),
    ", 95% CI [", round(m_no_Park2_TG_advwet$ci.lb, 2),
    ", ", round(m_no_Park2_TG_advwet$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_TG_advwet$b[1], 2),
    ", 95% CI [", round(m_TG_advwet$ci.lb, 2),
    ", ", round(m_TG_advwet$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_TG_advwet, slab = df_no_Park2_TG_advwet$ID_2, addcred = TRUE, showweights = TRUE)


###TG_advdry
#outlier analysis
m_TG_advdry$ci.lb
m_TG_advdry$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_TG_advdry <- alldata5$yi_TG_ctrl_advdry_comb + 1.96 * sqrt(alldata5$vi_TG_ctrl_advdry_comb)
alldata5$lowerci_TG_advdry <- alldata5$yi_TG_ctrl_advdry_comb - 1.96 * sqrt(alldata5$vi_TG_ctrl_advdry_comb)
# Create filter variable
alldata5$outlier_TG_advdry <- alldata5$upperci_TG_advdry < m_TG_advdry$ci.lb | alldata5$lowerci_TG_advdry > m_TG_advdry$ci.ub
# Count number of outliers:
sum(alldata5$outlier_TG_advdry, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_TG_ctrl_advdry_comb, colour = outlier_TG_advdry, fill = outlier_TG_advdry)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_TG_advdry$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_TG_advdry <- alldata5[-c(29, 90),] #create new df excluding the outliers

m_no_outliers_TG_advdry <- rma.mv(yi_TG_ctrl_advdry_comb, vi_TG_ctrl_advdry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_TG_advdry)
m_no_outliers_TG_advdry

cat("G = ", round(m_no_outliers_TG_advdry$b[1], 2),
    ", 95% CI [", round(m_no_outliers_TG_advdry$ci.lb, 2),
    ", ", round(m_no_outliers_TG_advdry$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_TG_advdry$b[1], 2),
    ", 95% CI [", round(m_TG_advdry$ci.lb, 2),
    ", ", round(m_TG_advdry$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_TG_advdry <- rma(yi = yi_TG_ctrl_advdry_comb, vi = vi_TG_ctrl_advdry_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_TG_advdry

m_subgroup_regression_TG_advdry <- rma(yi = yi_TG_ctrl_advdry_comb, vi = vi_TG_ctrl_advdry_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_TG_advdry

#subgroup analysis; by origin of data (OR vs. means)
alldata5$origin_data <- factor(alldata5$Ratio_type %in% c("OR", "USA/Korea"),
                               labels = c("WARMGS", "USA"))
m_subgroup2_TG_advdry <- rma(yi = yi_TG_ctrl_advdry_comb, vi = vi_TG_ctrl_advdry_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_TG_advdry

m_subgroup2_TG_advdry <- rma(yi = yi_TG_ctrl_advdry_comb, vi = vi_TG_ctrl_advdry_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_TG_advdry

#meta-regression
m_metareg_TG_advdry <- rma(yi = yi_TG_ctrl_advdry_comb,
                           vi = vi_TG_ctrl_advdry_comb,
                           mods = ~proportion_f,
                           data = alldata5)
m_metareg_TG_advdry

#publication bias via funnel plot
funnel(m_TG_advdry, xlab = "Hedges' g")
funnel(m_TG_advdry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_TG_advdry)

svg(file='funnel_TG_advdry.svg') # Open svg device with specific file name
funnel(m_TG_advdry, xlab = "Hedges' g")
funnel(m_TG_advdry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_TG_advdry, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_TG_advdry <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_TG_advdry <- rma.mv(yi_TG_ctrl_advdry_comb, vi_TG_ctrl_advdry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_TG_advdry)
m_no_Park2_TG_advdry

cat("G = ", round(m_no_Park2_TG_advdry$b[1], 2),
    ", 95% CI [", round(m_no_Park2_TG_advdry$ci.lb, 2),
    ", ", round(m_no_Park2_TG_advdry$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_TG_advdry$b[1], 2),
    ", 95% CI [", round(m_TG_advdry$ci.lb, 2),
    ", ", round(m_TG_advdry$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_TG_advdry, slab = df_no_Park2_TG_advdry$ID_2, addcred = TRUE, showweights = TRUE)


###TC_allAMD
#outlier analysis
m_TC_allAMD$ci.lb
m_TC_allAMD$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_TC_allAMD <- alldata5$yi_TC_ctrl_allAMD_comb + 1.96 * sqrt(alldata5$vi_TC_ctrl_allAMD_comb)
alldata5$lowerci_TC_allAMD <- alldata5$yi_TC_ctrl_allAMD_comb - 1.96 * sqrt(alldata5$vi_TC_ctrl_allAMD_comb)
# Create filter variable
alldata5$outlier_TC_allAMD <- alldata5$upperci_TC_allAMD < m_TC_allAMD$ci.lb | alldata5$lowerci_TC_allAMD > m_TC_allAMD$ci.ub
# Count number of outliers:
sum(alldata5$outlier_TC_allAMD, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_TC_ctrl_allAMD_comb, colour = outlier_TC_allAMD, fill = outlier_TC_allAMD)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_TC_allAMD$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_TC_allAMD <- alldata5[-c(4,5,23,36,44,68,71,76,90),] #create new df excluding the outliers

m_no_outliers_TC_allAMD <- rma.mv(yi_TC_ctrl_allAMD_comb, vi_TC_ctrl_allAMD_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_TC_allAMD)
m_no_outliers_TC_allAMD

cat("G = ", round(m_no_outliers_TC_allAMD$b[1], 2),
    ", 95% CI [", round(m_no_outliers_TC_allAMD$ci.lb, 2),
    ", ", round(m_no_outliers_TC_allAMD$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_TC_allAMD$b[1], 2),
    ", 95% CI [", round(m_TC_allAMD$ci.lb, 2),
    ", ", round(m_TC_allAMD$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_TC_allAMD <- rma(yi = yi_TC_ctrl_allAMD_comb, vi = vi_TC_ctrl_allAMD_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_TC_allAMD

m_subgroup_regression_TC_allAMD <- rma(yi = yi_TC_ctrl_allAMD_comb, vi = vi_TC_ctrl_allAMD_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_TC_allAMD

#subgroup analysis; by origin of data (OR vs. means)
alldata5$origin_data <- factor(alldata5$Ratio_type %in% c("OR", "USA/Korea"),
                               labels = c("WARMGS", "USA"))
m_subgroup2_TC_allAMD <- rma(yi = yi_TC_ctrl_allAMD_comb, vi = vi_TC_ctrl_allAMD_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_TC_allAMD

m_subgroup2_TC_allAMD <- rma(yi = yi_TC_ctrl_allAMD_comb, vi = vi_TC_ctrl_allAMD_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_TC_allAMD

#meta-regression
m_metareg_TC_allAMD <- rma(yi = yi_TC_ctrl_allAMD_comb,
                           vi = vi_TC_ctrl_allAMD_comb,
                           mods = ~proportion_f,
                           data = alldata5)
m_metareg_TC_allAMD

#publication bias via funnel plot
funnel(m_TC_allAMD, xlab = "Hedges' g")
funnel(m_TC_allAMD, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_TC_allAMD)

svg(file='funnel_TC_allAMD.svg') # Open svg device with specific file name
funnel(m_TC_allAMD, xlab = "Hedges' g")
funnel(m_TC_allAMD, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_TC_allAMD, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_TC_allAMD <- alldata5[-c(26, 28),] #create new df excluding the Park studies

m_no_Park2_TC_allAMD <- rma.mv(yi_TC_ctrl_allAMD_comb, vi_TC_ctrl_allAMD_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_TC_allAMD)
m_no_Park2_TC_allAMD

cat("G = ", round(m_no_Park2_TC_allAMD$b[1], 2),
    ", 95% CI [", round(m_no_Park2_TC_allAMD$ci.lb, 2),
    ", ", round(m_no_Park2_TC_allAMD$ci.ub, 2), "] (no Park studies)", sep = "")

cat("G = ", round(m_TC_allAMD$b[1], 2),
    ", 95% CI [", round(m_TC_allAMD$ci.lb, 2),
    ", ", round(m_TC_allAMD$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_TC_allAMD, slab = df_no_Park2_TC_allAMD$ID_2, addcred = TRUE, showweights = TRUE)

#sensitivity analysis for studies classified by non-specified classifcations
df_nonspecified_TC_allAMD <- alldata5[-c(5,29,32,55,77,86),] #create new df excluding the outliers

m_nonspecified_TC_allAMD <- rma.mv(yi_TC_ctrl_allAMD_comb, vi_TC_ctrl_allAMD_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_nonspecified_TC_allAMD)
m_nonspecified_TC_allAMD

cat("G = ", round(m_nonspecified_TC_allAMD$b[1], 2),
    ", 95% CI [", round(m_nonspecified_TC_allAMD$ci.lb, 2),
    ", ", round(m_nonspecified_TC_allAMD$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_TC_allAMD$b[1], 2),
    ", 95% CI [", round(m_TC_allAMD$ci.lb, 2),
    ", ", round(m_TC_allAMD$ci.ub, 2), "]", sep = "")

###TC and eidry###
#outlier analysis
m_TC_eidry$ci.lb
m_TC_eidry$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_TC_eidry <- alldata5$yi_TC_ctrl_eidry_comb + 1.96 * sqrt(alldata5$vi_TC_ctrl_eidry_comb)
alldata5$lowerci_TC_eidry <- alldata5$yi_TC_ctrl_eidry_comb - 1.96 * sqrt(alldata5$vi_TC_ctrl_eidry_comb)
# Create filter variable
alldata5$outlier_TC_eidry <- alldata5$upperci_TC_eidry < m_TC_eidry$ci.lb | alldata5$lowerci_TC_eidry > m_TC_eidry$ci.ub
# Count number of outliers:
sum(alldata5$outlier_TC_eidry, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_TC_ctrl_eidry_comb, colour = outlier_TC_eidry, fill = outlier_TC_eidry)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_TC_eidry$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_TC_eidry <- alldata5[-c(23,36,44,76),] #create new df excluding the outliers

m_no_outliers_TC_eidry <- rma.mv(yi_TC_ctrl_eidry_comb, vi_TC_ctrl_eidry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_TC_eidry)
m_no_outliers_TC_eidry

W <- diag(1/m_no_outliers_TC_eidry$vi)
X <- model.matrix(m_no_outliers_TC_eidry)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(m_no_outliers_TC_eidry$sigma2) / (sum(m_no_outliers_TC_eidry$sigma2) + (m_no_outliers_TC_eidry$k-m_no_outliers_TC_eidry$p)/sum(diag(P)))


cat("G = ", round(m_no_outliers_TC_eidry$b[1], 3),
    ", 95% CI [", round(m_no_outliers_TC_eidry$ci.lb, 3),
    ", ", round(m_no_outliers_TC_eidry$ci.ub, 3), "] (no outliers)", sep = "")

cat("G = ", round(m_TC_eidry$b[1], 2),
    ", 95% CI [", round(m_TC_eidry$ci.lb, 2),
    ", ", round(m_TC_eidry$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_TC_eidry <- rma(yi = yi_TC_ctrl_eidry_comb, vi = vi_TC_ctrl_eidry_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_TC_eidry

m_subgroup_regression_TC_eidry <- rma(yi = yi_TC_ctrl_eidry_comb, vi = vi_TC_ctrl_eidry_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_TC_eidry

#subgroup analysis; by origin of data (OR vs. means)
alldata5$origin_data <- factor(alldata5$Ratio_type %in% c("OR", "USA/Korea"),
                               labels = c("WARMGS", "USA"))
m_subgroup2_TC_eidry <- rma(yi = yi_TC_ctrl_eidry_comb, vi = vi_TC_ctrl_eidry_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_TC_eidry

m_subgroup2_TC_eidry <- rma(yi = yi_TC_ctrl_eidry_comb, vi = vi_TC_ctrl_eidry_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_TC_eidry

#meta-regression
m_metareg_TC_eidry <- rma(yi = yi_TC_ctrl_eidry_comb,
                           vi = vi_TC_ctrl_eidry_comb,
                           mods = ~proportion_f,
                           data = alldata5)
m_metareg_TC_eidry

#publication bias via funnel plot
funnel(m_TC_eidry, xlab = "Hedges' g")
funnel(m_TC_eidry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_TC_eidry)

svg(file='funnel_TG_eidry.svg') # Open svg device with specific file name
funnel(m_TC_eidry, xlab = "Hedges' g")
funnel(m_TC_eidry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_TC_eidry, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_TC_eidry <- alldata5[-c(26, 28),] #create new df excluding the Park studies

m_no_Park2_TC_eidry <- rma.mv(yi_TC_ctrl_eidry_comb, vi_TC_ctrl_eidry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_TC_eidry)
m_no_Park2_TC_eidry

cat("G = ", round(m_no_Park2_TC_eidry$b[1], 2),
    ", 95% CI [", round(m_no_Park2_TC_eidry$ci.lb, 2),
    ", ", round(m_no_Park2_TC_eidry$ci.ub, 2), "] (no Park studies)", sep = "")

cat("G = ", round(m_TC_eidry$b[1], 2),
    ", 95% CI [", round(m_TC_eidry$ci.lb, 2),
    ", ", round(m_TC_eidry$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_TC_eidry, slab = df_no_Park2_TC_eidry$ID_2, addcred = TRUE, showweights = TRUE)

#sensitivity analysis for studies classified by non-specified classifcations
df_nonspecified_TC_eidry <- alldata5[-c(5,29,32,55,77,86),] #create new df excluding the outliers

m_nonspecified_TC_eidry <- rma.mv(yi_TC_ctrl_eidry_comb, vi_TC_ctrl_eidry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_nonspecified_TC_eidry)
m_nonspecified_TC_eidry

cat("G = ", round(m_nonspecified_TC_eidry$b[1], 2),
    ", 95% CI [", round(m_nonspecified_TC_eidry$ci.lb, 2),
    ", ", round(m_nonspecified_TC_eidry$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_TC_eidry$b[1], 2),
    ", 95% CI [", round(m_TC_eidry$ci.lb, 2),
    ", ", round(m_TC_eidry$ci.ub, 2), "]", sep = "")

####TC_advdry###
#outlier analysis
m_TC_advdry$ci.lb
m_TC_advdry$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_TC_advdry <- alldata5$yi_TC_ctrl_advdry_comb + 1.96 * sqrt(alldata5$vi_TC_ctrl_advdry_comb)
alldata5$lowerci_TC_advdry <- alldata5$yi_TC_ctrl_advdry_comb - 1.96 * sqrt(alldata5$vi_TC_ctrl_advdry_comb)
# Create filter variable
alldata5$outlier_TC_advdry <- alldata5$upperci_TC_advdry < m_TC_advdry$ci.lb | alldata5$lowerci_TC_advdry > m_TC_advdry$ci.ub
# Count number of outliers:
sum(alldata5$outlier_TC_advdry, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_TC_ctrl_advdry_comb, colour = outlier_TC_advdry, fill = outlier_TC_advdry)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_TC_advdry$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_TC_advdry <- alldata5[-c(29, 90),] #create new df excluding the outliers

m_no_outliers_TC_advdry <- rma.mv(yi_TC_ctrl_advdry_comb, vi_TC_ctrl_advdry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_TC_advdry)
m_no_outliers_TC_advdry

cat("G = ", round(m_no_outliers_TC_advdry$b[1], 2),
    ", 95% CI [", round(m_no_outliers_TC_advdry$ci.lb, 2),
    ", ", round(m_no_outliers_TC_advdry$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_TC_advdry$b[1], 2),
    ", 95% CI [", round(m_TC_advdry$ci.lb, 2),
    ", ", round(m_TC_advdry$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_TC_advdry <- rma(yi = yi_TC_ctrl_advdry_comb, vi = vi_TC_ctrl_advdry_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_TC_advdry

m_subgroup_regression_TC_advdry <- rma(yi = yi_TC_ctrl_advdry_comb, vi = vi_TC_ctrl_advdry_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_TC_advdry

#subgroup analysis; by origin of data (OR vs. means)
alldata5$origin_data <- factor(alldata5$Ratio_type %in% c("OR", "USA/Korea"),
                               labels = c("WARMGS", "USA"))
m_subgroup2_TC_advdry <- rma(yi = yi_TC_ctrl_advdry_comb, vi = vi_TC_ctrl_advdry_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_TC_advdry

m_subgroup2_TC_advdry <- rma(yi = yi_TC_ctrl_advdry_comb, vi = vi_TC_ctrl_advdry_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_TC_advdry

#meta-regression
m_metareg_TC_advdry <- rma(yi = yi_TC_ctrl_advdry_comb,
                           vi = vi_TC_ctrl_advdry_comb,
                           mods = ~proportion_f,
                           data = alldata5)
m_metareg_TC_advdry

#publication bias via funnel plot
funnel(m_TC_advdry, xlab = "Hedges' g")
funnel(m_TC_advdry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_TC_advdry)

svg(file='funnel_TC_advdry.svg') # Open svg device with specific file name
funnel(m_TC_advdry, xlab = "Hedges' g")
funnel(m_TC_advdry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_TC_advdry, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_TC_advdry <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_TC_advdry <- rma.mv(yi_TC_ctrl_advdry_comb, vi_TC_ctrl_advdry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_TC_advdry)
m_no_Park2_TC_advdry

cat("G = ", round(m_no_Park2_TC_advdry$b[1], 2),
    ", 95% CI [", round(m_no_Park2_TC_advdry$ci.lb, 2),
    ", ", round(m_no_Park2_TC_advdry$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_TC_advdry$b[1], 2),
    ", 95% CI [", round(m_TC_advdry$ci.lb, 2),
    ", ", round(m_TC_advdry$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_TC_advdry, slab = df_no_Park2_TC_advdry$ID_2, addcred = TRUE, showweights = TRUE)

####TC_advwet###
#outlier analysis
m_TC_advwet$ci.lb
m_TC_advwet$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_TC_advwet <- alldata5$yi_TC_ctrl_advwet_comb + 1.96 * sqrt(alldata5$vi_TC_ctrl_advwet_comb)
alldata5$lowerci_TC_advwet <- alldata5$yi_TC_ctrl_advwet_comb - 1.96 * sqrt(alldata5$vi_TC_ctrl_advwet_comb)
# Create filter variable
alldata5$outlier_TC_advwet <- alldata5$upperci_TC_advwet < m_TC_advwet$ci.lb | alldata5$lowerci_TC_advwet > m_TC_advwet$ci.ub
# Count number of outliers:
sum(alldata5$outlier_TC_advwet, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_TC_ctrl_advwet_comb, colour = outlier_TC_advwet, fill = outlier_TC_advwet)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_TC_advwet$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_TC_advwet <- alldata5[-c(29, 90),] #create new df excluding the outliers

m_no_outliers_TC_advwet <- rma.mv(yi_TC_ctrl_advwet_comb, vi_TC_ctrl_advwet_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_TC_advwet)
m_no_outliers_TC_advwet

cat("G = ", round(m_no_outliers_TC_advwet$b[1], 2),
    ", 95% CI [", round(m_no_outliers_TC_advwet$ci.lb, 2),
    ", ", round(m_no_outliers_TC_advwet$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_TC_advwet$b[1], 2),
    ", 95% CI [", round(m_TC_advwet$ci.lb, 2),
    ", ", round(m_TC_advwet$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_TC_advwet <- rma(yi = yi_TC_ctrl_advwet_comb, vi = vi_TC_ctrl_advwet_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_TC_advwet

m_subgroup_regression_TC_advwet <- rma(yi = yi_TC_ctrl_advwet_comb, vi = vi_TC_ctrl_advwet_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_TC_advwet

#subgroup analysis; by origin of data (OR vs. means)
m_subgroup2_TC_advwet <- rma(yi = yi_TC_ctrl_advwet_comb, vi = vi_TC_ctrl_advwet_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_TC_advwet

m_subgroup2_TC_advwet <- rma(yi = yi_TC_ctrl_advwet_comb, vi = vi_TC_ctrl_advwet_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_TC_advwet

#meta-regression
m_metareg_TC_advwet <- rma(yi = yi_TC_ctrl_advwet_comb,
                           vi = vi_TC_ctrl_advwet_comb,
                           mods = ~proportion_f,
                           data = alldata5)
m_metareg_TC_advwet

#publication bias via funnel plot
funnel(m_TC_advwet, xlab = "Hedges' g")
funnel(m_TC_advwet, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_TC_advwet)

svg(file='funnel_TC_advwet.svg') # Open svg device with specific file name
funnel(m_TC_advwet, xlab = "Hedges' g")
funnel(m_TC_advwet, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_TC_advwet, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_TC_advwet <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_TC_advwet <- rma.mv(yi_TC_ctrl_advwet_comb, vi_TC_ctrl_advwet_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_TC_advwet)
m_no_Park2_TC_advwet

cat("G = ", round(m_no_Park2_TC_advwet$b[1], 2),
    ", 95% CI [", round(m_no_Park2_TC_advwet$ci.lb, 2),
    ", ", round(m_no_Park2_TC_advwet$ci.ub, 2), "] (no Park)", sep = "")

cat("G = ", round(m_TC_advwet$b[1], 2),
    ", 95% CI [", round(m_TC_advwet$ci.lb, 2),
    ", ", round(m_TC_advwet$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_TC_advwet, slab = df_no_Park2_TC_advwet$ID_2, addcred = TRUE, showweights = TRUE)

####HDL_allAMD###
#outlier analysis
m_HDL_allAMD$ci.lb
m_HDL_allAMD$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_HDL_allAMD <- alldata5$yi_HDL_ctrl_allAMD_comb + 1.96 * sqrt(alldata5$vi_HDL_ctrl_allAMD_comb)
alldata5$lowerci_HDL_allAMD <- alldata5$yi_HDL_ctrl_allAMD_comb - 1.96 * sqrt(alldata5$vi_HDL_ctrl_allAMD_comb)
# Create filter variable
alldata5$outlier_HDL_allAMD <- alldata5$upperci_HDL_allAMD < m_HDL_allAMD$ci.lb | alldata5$lowerci_HDL_allAMD > m_HDL_allAMD$ci.ub
# Count number of outliers:
sum(alldata5$outlier_HDL_allAMD, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_HDL_ctrl_allAMD_comb, colour = outlier_HDL_allAMD, fill = outlier_HDL_allAMD)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_HDL_allAMD$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_HDL_allAMD <- alldata5[-c(4,77,90),] #create new df excluding the outliers

m_no_outliers_HDL_allAMD <- rma.mv(yi_HDL_ctrl_allAMD_comb, vi_HDL_ctrl_allAMD_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_HDL_allAMD)
m_no_outliers_HDL_allAMD



cat("G = ", round(m_no_outliers_HDL_allAMD$b[1], 2),
    ", 95% CI [", round(m_no_outliers_HDL_allAMD$ci.lb, 2),
    ", ", round(m_no_outliers_HDL_allAMD$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_HDL_allAMD$b[1], 2),
    ", 95% CI [", round(m_HDL_allAMD$ci.lb, 2),
    ", ", round(m_HDL_allAMD$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#remove outliers and 4 low NOS studies
df_no_outlier_NOSlow_HDL_allAMD <- alldata5[-c(4,77,90,32),] #create new df excluding the outliers

m_no_outlier_NOSlow_HDL_allAMD <- rma.mv(yi_HDL_ctrl_allAMD_comb, vi_HDL_ctrl_allAMD_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_NOSlow_HDL_allAMD)
m_no_outlier_NOSlow_HDL_allAMD

W <- diag(1/m_no_outlier_NOSlow_HDL_allAMD$vi)
X <- model.matrix(m_no_outlier_NOSlow_HDL_allAMD)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(m_no_outlier_NOSlow_HDL_allAMD$sigma2) / (sum(m_no_outlier_NOSlow_HDL_allAMD$sigma2) + (m_no_outlier_NOSlow_HDL_allAMD$k-m_no_outlier_NOSlow_HDL_allAMD$p)/sum(diag(P)))


#subgroup analysis; by AMD classfication methodology
m_subgroup1_HDL_allAMD <- rma(yi = yi_HDL_ctrl_allAMD_comb, vi = vi_HDL_ctrl_allAMD_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_HDL_allAMD

m_subgroup_regression_HDL_allAMD <- rma(yi = yi_HDL_ctrl_allAMD_comb, vi = vi_HDL_ctrl_allAMD_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_HDL_allAMD

#subgroup analysis; by origin of data (OR vs. means)
m_subgroup2_HDL_allAMD <- rma(yi = yi_HDL_ctrl_allAMD_comb, vi = vi_HDL_ctrl_allAMD_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_HDL_allAMD

m_subgroup2_HDL_allAMD <- rma(yi = yi_HDL_ctrl_allAMD_comb, vi = vi_HDL_ctrl_allAMD_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_HDL_allAMD

#meta-regression
m_metareg_HDL_allAMD <- rma(yi = yi_HDL_ctrl_allAMD_comb,
                           vi = vi_HDL_ctrl_allAMD_comb,
                           mods = ~proportion_f,
                           data = alldata5)
m_metareg_HDL_allAMD

#publication bias via funnel plot
funnel(m_HDL_allAMD, xlab = "Hedges' g")
funnel(m_HDL_allAMD, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_HDL_allAMD)
#funnel plot w/o outliers
funnel(m_no_outliers_HDL_allAMD, xlab = "Hedges' g")
funnel(m_no_outliers_HDL_allAMD, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_no_outliers_HDL_allAMD)

svg(file='funnel_HDL_allAMD.svg') # Open svg device with specific file name
funnel(m_HDL_allAMD, xlab = "Hedges' g")
funnel(m_HDL_allAMD, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
svg(file='funnel_HDL_allAMD_nooutliers.svg') # Open svg device with specific file name
funnel(m_no_outliers_HDL_allAMD, xlab = "Hedges' g")
funnel(m_no_outliers_HDL_allAMD, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_HDL_allAMD, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_HDL_allAMD <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_HDL_allAMD <- rma.mv(yi_HDL_ctrl_allAMD_comb, vi_HDL_ctrl_allAMD_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_HDL_allAMD)
m_no_Park2_HDL_allAMD

cat("G = ", round(m_no_Park2_HDL_allAMD$b[1], 2),
    ", 95% CI [", round(m_no_Park2_HDL_allAMD$ci.lb, 2),
    ", ", round(m_no_Park2_HDL_allAMD$ci.ub, 2), "] (no Park)", sep = "")

cat("G = ", round(m_HDL_allAMD$b[1], 2),
    ", 95% CI [", round(m_HDL_allAMD$ci.lb, 2),
    ", ", round(m_HDL_allAMD$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_HDL_allAMD, slab = df_no_Park2_HDL_allAMD$ID_2, addcred = TRUE, showweights = TRUE)

####HDL_eidry###
#outlier analysis
m_HDL_eidry$ci.lb
m_HDL_eidry$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_HDL_eidry <- alldata5$yi_HDL_ctrl_eidry_comb + 1.96 * sqrt(alldata5$vi_HDL_ctrl_eidry_comb)
alldata5$lowerci_HDL_eidry <- alldata5$yi_HDL_ctrl_eidry_comb - 1.96 * sqrt(alldata5$vi_HDL_ctrl_eidry_comb)
# Create filter variable
alldata5$outlier_HDL_eidry <- alldata5$upperci_HDL_eidry < m_HDL_eidry$ci.lb | alldata5$lowerci_HDL_eidry > m_HDL_eidry$ci.ub
# Count number of outliers:
sum(alldata5$outlier_HDL_eidry, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_HDL_ctrl_eidry_comb, colour = outlier_HDL_eidry, fill = outlier_HDL_eidry)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_HDL_eidry$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_HDL_eidry <- alldata5[-c(17,18,69,70,84),] #create new df excluding the outliers

m_no_outliers_HDL_eidry <- rma.mv(yi_HDL_ctrl_eidry_comb, vi_HDL_ctrl_eidry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_HDL_eidry)
m_no_outliers_HDL_eidry

W <- diag(1/m_no_outliers_HDL_eidry$vi)
X <- model.matrix(m_no_outliers_HDL_eidry)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(m_no_outliers_HDL_eidry$sigma2) / (sum(m_no_outliers_HDL_eidry$sigma2) + (m_no_outliers_HDL_eidry$k-m_no_outliers_HDL_eidry$p)/sum(diag(P)))


cat("G = ", round(m_no_outliers_HDL_eidry$b[1], 2),
    ", 95% CI [", round(m_no_outliers_HDL_eidry$ci.lb, 2),
    ", ", round(m_no_outliers_HDL_eidry$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_HDL_eidry$b[1], 2),
    ", 95% CI [", round(m_HDL_eidry$ci.lb, 2),
    ", ", round(m_HDL_eidry$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_HDL_eidry <- rma(yi = yi_HDL_ctrl_eidry_comb, vi = vi_HDL_ctrl_eidry_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_HDL_eidry

m_subgroup_regression_HDL_eidry <- rma(yi = yi_HDL_ctrl_eidry_comb, vi = vi_HDL_ctrl_eidry_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_HDL_eidry

#subgroup analysis; by origin of data (OR vs. means)
m_subgroup2_HDL_eidry <- rma(yi = yi_HDL_ctrl_eidry_comb, vi = vi_HDL_ctrl_eidry_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_HDL_eidry

m_subgroup2_HDL_eidry <- rma(yi = yi_HDL_ctrl_eidry_comb, vi = vi_HDL_ctrl_eidry_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_HDL_eidry

#meta-regression
m_metareg_HDL_eidry <- rma(yi = yi_HDL_ctrl_eidry_comb,
                            vi = vi_HDL_ctrl_eidry_comb,
                            mods = ~proportion_f,
                            data = alldata5)
m_metareg_HDL_eidry

#publication bias via funnel plot
funnel(m_HDL_eidry, xlab = "Hedges' g")
funnel(m_HDL_eidry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_HDL_eidry)

svg(file='funnel_HDL_eidry.svg') # Open svg device with specific file name
funnel(m_HDL_eidry, xlab = "Hedges' g")
funnel(m_HDL_eidry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_HDL_eidry, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_HDL_eidry <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_HDL_eidry <- rma.mv(yi_HDL_ctrl_eidry_comb, vi_HDL_ctrl_eidry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_HDL_eidry)
m_no_Park2_HDL_eidry

cat("G = ", round(m_no_Park2_HDL_eidry$b[1], 2),
    ", 95% CI [", round(m_no_Park2_HDL_eidry$ci.lb, 2),
    ", ", round(m_no_Park2_HDL_eidry$ci.ub, 2), "] (no Park)", sep = "")

cat("G = ", round(m_HDL_eidry$b[1], 2),
    ", 95% CI [", round(m_HDL_eidry$ci.lb, 2),
    ", ", round(m_HDL_eidry$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_HDL_eidry, slab = df_no_Park2_HDL_eidry$ID_2, addcred = TRUE, showweights = TRUE)

####HDL_advdry###
#outlier analysis
m_HDL_advdry$ci.lb
m_HDL_advdry$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_HDL_advdry <- alldata5$yi_HDL_ctrl_advdry_comb + 1.96 * sqrt(alldata5$vi_HDL_ctrl_advdry_comb)
alldata5$lowerci_HDL_advdry <- alldata5$yi_HDL_ctrl_advdry_comb - 1.96 * sqrt(alldata5$vi_HDL_ctrl_advdry_comb)
# Create filter variable
alldata5$outlier_HDL_advdry <- alldata5$upperci_HDL_advdry < m_HDL_advdry$ci.lb | alldata5$lowerci_HDL_advdry > m_HDL_advdry$ci.ub
# Count number of outliers:
sum(alldata5$outlier_HDL_advdry, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_HDL_ctrl_advdry_comb, colour = outlier_HDL_advdry, fill = outlier_HDL_advdry)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_HDL_advdry$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_HDL_advdry <- alldata5[-c(17,69,70,84),] #create new df excluding the outliers

m_no_outliers_HDL_advdry <- rma.mv(yi_HDL_ctrl_advdry_comb, vi_HDL_ctrl_advdry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_HDL_advdry)
m_no_outliers_HDL_advdry

cat("G = ", round(m_no_outliers_HDL_advdry$b[1], 2),
    ", 95% CI [", round(m_no_outliers_HDL_advdry$ci.lb, 2),
    ", ", round(m_no_outliers_HDL_advdry$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_HDL_advdry$b[1], 2),
    ", 95% CI [", round(m_HDL_advdry$ci.lb, 2),
    ", ", round(m_HDL_advdry$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_HDL_advdry <- rma(yi = yi_HDL_ctrl_advdry_comb, vi = vi_HDL_ctrl_advdry_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_HDL_advdry

m_subgroup_regression_HDL_advdry <- rma(yi = yi_HDL_ctrl_advdry_comb, vi = vi_HDL_ctrl_advdry_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_HDL_advdry

#subgroup analysis; by origin of data (OR vs. means)
m_subgroup2_HDL_advdry <- rma(yi = yi_HDL_ctrl_advdry_comb, vi = vi_HDL_ctrl_advdry_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_HDL_advdry

m_subgroup2_HDL_advdry <- rma(yi = yi_HDL_ctrl_advdry_comb, vi = vi_HDL_ctrl_advdry_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_HDL_advdry

#meta-regression
m_metareg_HDL_advdry <- rma(yi = yi_HDL_ctrl_advdry_comb,
                           vi = vi_HDL_ctrl_advdry_comb,
                           mods = ~proportion_f,
                           data = alldata5)
m_metareg_HDL_advdry

#publication bias via funnel plot
funnel(m_HDL_advdry, xlab = "Hedges' g")
funnel(m_HDL_advdry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_HDL_advdry)

svg(file='funnel_HDL_advdry.svg') # Open svg device with specific file name
funnel(m_HDL_advdry, xlab = "Hedges' g")
funnel(m_HDL_advdry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_HDL_advdry, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_HDL_advdry <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_HDL_advdry <- rma.mv(yi_HDL_ctrl_advdry_comb, vi_HDL_ctrl_advdry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_HDL_advdry)
m_no_Park2_HDL_advdry

cat("G = ", round(m_no_Park2_HDL_advdry$b[1], 2),
    ", 95% CI [", round(m_no_Park2_HDL_advdry$ci.lb, 2),
    ", ", round(m_no_Park2_HDL_advdry$ci.ub, 2), "] (no Park)", sep = "")

cat("G = ", round(m_HDL_advdry$b[1], 2),
    ", 95% CI [", round(m_HDL_advdry$ci.lb, 2),
    ", ", round(m_HDL_advdry$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_HDL_advdry, slab = df_no_Park2_HDL_advdry$ID_2, addcred = TRUE, showweights = TRUE)

####HDL_advwet###
#outlier analysis
m_HDL_advwet$ci.lb
m_HDL_advwet$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_HDL_advwet <- alldata5$yi_HDL_ctrl_advwet_comb + 1.96 * sqrt(alldata5$vi_HDL_ctrl_advwet_comb)
alldata5$lowerci_HDL_advwet <- alldata5$yi_HDL_ctrl_advwet_comb - 1.96 * sqrt(alldata5$vi_HDL_ctrl_advwet_comb)
# Create filter variable
alldata5$outlier_HDL_advwet <- alldata5$upperci_HDL_advwet < m_HDL_advwet$ci.lb | alldata5$lowerci_HDL_advwet > m_HDL_advwet$ci.ub
# Count number of outliers:
sum(alldata5$outlier_HDL_advwet, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_HDL_ctrl_advwet_comb, colour = outlier_HDL_advwet, fill = outlier_HDL_advwet)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_HDL_advwet$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_HDL_advwet <- alldata5[-c(17,69,70,84)] #create new df excluding the outliers

m_no_outliers_HDL_advwet <- rma.mv(yi_HDL_ctrl_advwet_comb, vi_HDL_ctrl_advwet_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_HDL_advwet)
m_no_outliers_HDL_advwet

cat("G = ", round(m_no_outliers_HDL_advwet$b[1], 2),
    ", 95% CI [", round(m_no_outliers_HDL_advwet$ci.lb, 2),
    ", ", round(m_no_outliers_HDL_advwet$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_HDL_advwet$b[1], 2),
    ", 95% CI [", round(m_HDL_advwet$ci.lb, 2),
    ", ", round(m_HDL_advwet$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_HDL_advwet <- rma(yi = yi_HDL_ctrl_advwet_comb, vi = vi_HDL_ctrl_advwet_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_HDL_advwet

m_subgroup_regression_HDL_advwet <- rma(yi = yi_HDL_ctrl_advwet_comb, vi = vi_HDL_ctrl_advwet_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_HDL_advwet

#subgroup analysis; by origin of data (OR vs. means)
m_subgroup2_HDL_advwet <- rma(yi = yi_HDL_ctrl_advwet_comb, vi = vi_HDL_ctrl_advwet_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_HDL_advwet

m_subgroup2_HDL_advwet <- rma(yi = yi_HDL_ctrl_advwet_comb, vi = vi_HDL_ctrl_advwet_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_HDL_advwet

#meta-regression
m_metareg_HDL_advwet <- rma(yi = yi_HDL_ctrl_advwet_comb,
                            vi = vi_HDL_ctrl_advwet_comb,
                            mods = ~proportion_f,
                            data = alldata5)
m_metareg_HDL_advwet

#publication bias via funnel plot
funnel(m_HDL_advwet, xlab = "Hedges' g")
funnel(m_HDL_advwet, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_HDL_advwet)

svg(file='funnel_HDL_advwet.svg') # Open svg device with specific file name
funnel(m_HDL_advwet, xlab = "Hedges' g")
funnel(m_HDL_advwet, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_HDL_advwet, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_HDL_advwet <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_HDL_advwet <- rma.mv(yi_HDL_ctrl_advwet_comb, vi_HDL_ctrl_advwet_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_HDL_advwet)
m_no_Park2_HDL_advwet

cat("G = ", round(m_no_Park2_HDL_advwet$b[1], 2),
    ", 95% CI [", round(m_no_Park2_HDL_advwet$ci.lb, 2),
    ", ", round(m_no_Park2_HDL_advwet$ci.ub, 2), "] (no Park)", sep = "")

cat("G = ", round(m_HDL_advwet$b[1], 2),
    ", 95% CI [", round(m_HDL_advwet$ci.lb, 2),
    ", ", round(m_HDL_advwet$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_HDL_advwet, slab = df_no_Park2_HDL_advwet$ID_2, addcred = TRUE, showweights = TRUE)

####LDL_allAMD###
#outlier analysis
m_LDL_allAMD$ci.lb
m_LDL_allAMD$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_LDL_allAMD <- alldata5$yi_LDL_ctrl_allAMD_comb + 1.96 * sqrt(alldata5$vi_LDL_ctrl_allAMD_comb)
alldata5$lowerci_LDL_allAMD <- alldata5$yi_LDL_ctrl_allAMD_comb - 1.96 * sqrt(alldata5$vi_LDL_ctrl_allAMD_comb)
# Create filter variable
alldata5$outlier_LDL_allAMD <- alldata5$upperci_LDL_allAMD < m_LDL_allAMD$ci.lb | alldata5$lowerci_LDL_allAMD > m_LDL_allAMD$ci.ub
# Count number of outliers:
sum(alldata5$outlier_LDL_allAMD, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_LDL_ctrl_allAMD_comb, colour = outlier_LDL_allAMD, fill = outlier_LDL_allAMD)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_LDL_allAMD$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_LDL_allAMD <- alldata5[-c(1,5,17,18,19,20,39,57,68,70,84,85,88,90),] #create new df excluding the outliers

m_no_outliers_LDL_allAMD <- rma.mv(yi_LDL_ctrl_allAMD_comb, vi_LDL_ctrl_allAMD_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_LDL_allAMD)
m_no_outliers_LDL_allAMD

cat("G = ", round(m_no_outliers_LDL_allAMD$b[1], 2),
    ", 95% CI [", round(m_no_outliers_LDL_allAMD$ci.lb, 2),
    ", ", round(m_no_outliers_LDL_allAMD$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_LDL_allAMD$b[1], 2),
    ", 95% CI [", round(m_LDL_allAMD$ci.lb, 2),
    ", ", round(m_LDL_allAMD$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_LDL_allAMD <- rma(yi = yi_LDL_ctrl_allAMD_comb, vi = vi_LDL_ctrl_allAMD_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_LDL_allAMD

m_subgroup_regression_LDL_allAMD <- rma(yi = yi_LDL_ctrl_allAMD_comb, vi = vi_LDL_ctrl_allAMD_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_LDL_allAMD

#subgroup analysis; by origin of data (OR vs. means)
m_subgroup2_LDL_allAMD <- rma(yi = yi_LDL_ctrl_allAMD_comb, vi = vi_LDL_ctrl_allAMD_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_LDL_allAMD

m_subgroup2_LDL_allAMD <- rma(yi = yi_LDL_ctrl_allAMD_comb, vi = vi_LDL_ctrl_allAMD_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_LDL_allAMD

#meta-regression
m_metareg_LDL_allAMD <- rma(yi = yi_LDL_ctrl_allAMD_comb,
                            vi = vi_LDL_ctrl_allAMD_comb,
                            mods = ~proportion_f,
                            data = alldata5)
m_metareg_LDL_allAMD

#publication bias via funnel plot
funnel(m_LDL_allAMD, xlab = "Hedges' g")
funnel(m_LDL_allAMD, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_LDL_allAMD, exact = FALSE)


svg(file='funnel_LDL_allAMD.svg') # Open svg device with specific file name
funnel(m_LDL_allAMD, xlab = "Hedges' g")
funnel(m_LDL_allAMD, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_LDL_allAMD, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_LDL_allAMD <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_LDL_allAMD <- rma.mv(yi_LDL_ctrl_allAMD_comb, vi_LDL_ctrl_allAMD_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_LDL_allAMD)
m_no_Park2_LDL_allAMD

cat("G = ", round(m_no_Park2_LDL_allAMD$b[1], 2),
    ", 95% CI [", round(m_no_Park2_LDL_allAMD$ci.lb, 2),
    ", ", round(m_no_Park2_LDL_allAMD$ci.ub, 2), "] (no Park)", sep = "")

cat("G = ", round(m_LDL_allAMD$b[1], 2),
    ", 95% CI [", round(m_LDL_allAMD$ci.lb, 2),
    ", ", round(m_LDL_allAMD$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_LDL_allAMD, slab = df_no_Park2_LDL_allAMD$ID_2, addcred = TRUE, showweights = TRUE)

####LDL_eidry###
#outlier analysis
m_LDL_eidry$ci.lb
m_LDL_eidry$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_LDL_eidry <- alldata5$yi_LDL_ctrl_eidry_comb + 1.96 * sqrt(alldata5$vi_LDL_ctrl_eidry_comb)
alldata5$lowerci_LDL_eidry <- alldata5$yi_LDL_ctrl_eidry_comb - 1.96 * sqrt(alldata5$vi_LDL_ctrl_eidry_comb)
# Create filter variable
alldata5$outlier_LDL_eidry <- alldata5$upperci_LDL_eidry < m_LDL_eidry$ci.lb | alldata5$lowerci_LDL_eidry > m_LDL_eidry$ci.ub
# Count number of outliers:
sum(alldata5$outlier_LDL_eidry, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_LDL_ctrl_eidry_comb, colour = outlier_LDL_eidry, fill = outlier_LDL_eidry)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_LDL_eidry$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_LDL_eidry <- alldata5[-c(36),] #create new df excluding the outliers

m_no_outliers_LDL_eidry <- rma.mv(yi_LDL_ctrl_eidry_comb, vi_LDL_ctrl_eidry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_LDL_eidry)
m_no_outliers_LDL_eidry

cat("G = ", round(m_no_outliers_LDL_eidry$b[1], 2),
    ", 95% CI [", round(m_no_outliers_LDL_eidry$ci.lb, 2),
    ", ", round(m_no_outliers_LDL_eidry$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_LDL_eidry$b[1], 2),
    ", 95% CI [", round(m_LDL_eidry$ci.lb, 2),
    ", ", round(m_LDL_eidry$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_LDL_eidry <- rma(yi = yi_LDL_ctrl_eidry_comb, vi = vi_LDL_ctrl_eidry_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_LDL_eidry

m_subgroup_regression_LDL_eidry <- rma(yi = yi_LDL_ctrl_eidry_comb, vi = vi_LDL_ctrl_eidry_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_LDL_eidry

#subgroup analysis; by origin of data (OR vs. means)
m_subgroup2_LDL_eidry <- rma(yi = yi_LDL_ctrl_eidry_comb, vi = vi_LDL_ctrl_eidry_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_LDL_eidry

m_subgroup2_LDL_eidry <- rma(yi = yi_LDL_ctrl_eidry_comb, vi = vi_LDL_ctrl_eidry_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_LDL_eidry

#meta-regression
m_metareg_LDL_eidry <- rma(yi = yi_LDL_ctrl_eidry_comb,
                            vi = vi_LDL_ctrl_eidry_comb,
                            mods = ~proportion_f,
                            data = alldata5)
m_metareg_LDL_eidry

#publication bias via funnel plot
funnel(m_LDL_eidry, xlab = "Hedges' g")
funnel(m_LDL_eidry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_LDL_eidry)

svg(file='funnel_LDL_eidry.svg') # Open svg device with specific file name
funnel(m_LDL_eidry, xlab = "Hedges' g")
funnel(m_LDL_eidry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_LDL_eidry, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_LDL_eidry <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_LDL_eidry <- rma.mv(yi_LDL_ctrl_eidry_comb, vi_LDL_ctrl_eidry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_LDL_eidry)
m_no_Park2_LDL_eidry

cat("G = ", round(m_no_Park2_LDL_eidry$b[1], 2),
    ", 95% CI [", round(m_no_Park2_LDL_eidry$ci.lb, 2),
    ", ", round(m_no_Park2_LDL_eidry$ci.ub, 2), "] (no Park)", sep = "")

cat("G = ", round(m_LDL_eidry$b[1], 2),
    ", 95% CI [", round(m_LDL_eidry$ci.lb, 2),
    ", ", round(m_LDL_eidry$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_LDL_eidry, slab = df_no_Park2_LDL_eidry$ID_2, addcred = TRUE, showweights = TRUE)

####LDL_advdry###
#outlier analysis
m_LDL_advdry$ci.lb
m_LDL_advdry$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_LDL_advdry <- alldata5$yi_LDL_ctrl_advdry_comb + 1.96 * sqrt(alldata5$vi_LDL_ctrl_advdry_comb)
alldata5$lowerci_LDL_advdry <- alldata5$yi_LDL_ctrl_advdry_comb - 1.96 * sqrt(alldata5$vi_LDL_ctrl_advdry_comb)
# Create filter variable
alldata5$outlier_LDL_advdry <- alldata5$upperci_LDL_advdry < m_LDL_advdry$ci.lb | alldata5$lowerci_LDL_advdry > m_LDL_advdry$ci.ub
# Count number of outliers:
sum(alldata5$outlier_LDL_advdry, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_LDL_ctrl_advdry_comb, colour = outlier_LDL_advdry, fill = outlier_LDL_advdry)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_LDL_advdry$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_outlier_LDL_advdry <- alldata5[-c(36),] #create new df excluding the outliers

m_no_outliers_LDL_advdry <- rma.mv(yi_LDL_ctrl_advdry_comb, vi_LDL_ctrl_advdry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_outlier_LDL_advdry)
m_no_outliers_LDL_advdry

cat("G = ", round(m_no_outliers_LDL_advdry$b[1], 2),
    ", 95% CI [", round(m_no_outliers_LDL_advdry$ci.lb, 2),
    ", ", round(m_no_outliers_LDL_advdry$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_LDL_advdry$b[1], 2),
    ", 95% CI [", round(m_LDL_advdry$ci.lb, 2),
    ", ", round(m_LDL_advdry$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_LDL_advdry <- rma(yi = yi_LDL_ctrl_advdry_comb, vi = vi_LDL_ctrl_advdry_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_LDL_advdry

m_subgroup_regression_LDL_advdry <- rma(yi = yi_LDL_ctrl_advdry_comb, vi = vi_LDL_ctrl_advdry_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_LDL_advdry

#subgroup analysis; by origin of data (OR vs. means)
m_subgroup2_LDL_advdry <- rma(yi = yi_LDL_ctrl_advdry_comb, vi = vi_LDL_ctrl_advdry_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_LDL_advdry

m_subgroup2_LDL_advdry <- rma(yi = yi_LDL_ctrl_advdry_comb, vi = vi_LDL_ctrl_advdry_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_LDL_advdry

#meta-regression
m_metareg_LDL_advdry <- rma(yi = yi_LDL_ctrl_advdry_comb,
                           vi = vi_LDL_ctrl_advdry_comb,
                           mods = ~proportion_f,
                           data = alldata5)
m_metareg_LDL_advdry

#publication bias via funnel plot
funnel(m_LDL_advdry, xlab = "Hedges' g")
funnel(m_LDL_advdry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_LDL_advdry)

svg(file='funnel_LDL_advdry.svg') # Open svg device with specific file name
funnel(m_LDL_advdry, xlab = "Hedges' g")
funnel(m_LDL_advdry, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_LDL_advdry, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_LDL_advdry <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_LDL_advdry <- rma.mv(yi_LDL_ctrl_advdry_comb, vi_LDL_ctrl_advdry_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_LDL_advdry)
m_no_Park2_LDL_advdry

cat("G = ", round(m_no_Park2_LDL_advdry$b[1], 2),
    ", 95% CI [", round(m_no_Park2_LDL_advdry$ci.lb, 2),
    ", ", round(m_no_Park2_LDL_advdry$ci.ub, 2), "] (no Park)", sep = "")

cat("G = ", round(m_LDL_advdry$b[1], 2),
    ", 95% CI [", round(m_LDL_advdry$ci.lb, 2),
    ", ", round(m_LDL_advdry$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_LDL_advdry, slab = df_no_Park2_LDL_advdry$ID_2, addcred = TRUE, showweights = TRUE)

####LDL_advwet###
#outlier analysis
m_LDL_advwet$ci.lb
m_LDL_advwet$ci.ub
# Calculate CI for all observed effect sizes
alldata5$upperci_LDL_advwet <- alldata5$yi_LDL_ctrl_advwet_comb + 1.96 * sqrt(alldata5$vi_LDL_ctrl_advwet_comb)
alldata5$lowerci_LDL_advwet <- alldata5$yi_LDL_ctrl_advwet_comb - 1.96 * sqrt(alldata5$vi_LDL_ctrl_advwet_comb)
# Create filter variable
alldata5$outlier_LDL_advwet <- alldata5$upperci_LDL_advwet < m_LDL_advwet$ci.lb | alldata5$lowerci_LDL_advwet > m_LDL_advwet$ci.ub
# Count number of outliers:
sum(alldata5$outlier_LDL_advwet, na.rm = TRUE)


# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = alldata5, aes(x = yi_LDL_ctrl_advwet_comb, colour = outlier_LDL_advwet, fill = outlier_LDL_advwet)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_LDL_advwet$b[1]) +
  # Apply a black and white theme
  theme_bw()

##sensitivity analysis
df_no_lowNOS_LDL_advwet <- alldata5[-c(32),] #create new df excluding the outliers

m_no_lowNOS_LDL_advwet <- rma.mv(yi_LDL_ctrl_advwet_comb, vi_LDL_ctrl_advwet_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_lowNOS_LDL_advwet)
m_no_lowNOS_LDL_advwet

#remove low NOS

W <- diag(1/m_no_lowNOS_LDL_advwet$vi)
X <- model.matrix(m_no_lowNOS_LDL_advwet)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(m_no_lowNOS_LDL_advwet$sigma2) / (sum(m_no_lowNOS_LDL_advwet$sigma2) + (m_no_lowNOS_LDL_advwet$k-m_no_lowNOS_LDL_advwet$p)/sum(diag(P)))

#TEST
100 * m_no_lowNOS_LDL_advwet$sigma2 / (sum(m_no_lowNOS_LDL_advwet$sigma2) + (m_no_lowNOS_LDL_advwet$k-m_no_lowNOS_LDL_advwet$p)/sum(diag(P)))



cat("G = ", round(m_no_outliers_LDL_advwet$b[1], 2),
    ", 95% CI [", round(m_no_outliers_LDL_advwet$ci.lb, 2),
    ", ", round(m_no_outliers_LDL_advwet$ci.ub, 2), "] (no outliers)", sep = "")

cat("G = ", round(m_LDL_advwet$b[1], 2),
    ", 95% CI [", round(m_LDL_advwet$ci.lb, 2),
    ", ", round(m_LDL_advwet$ci.ub, 2), "]", sep = "")
#conclusion: the outliers do not significantly bias the pooled effect?? but it does drastically increase the range

#subgroup analysis; by AMD classfication methodology
m_subgroup1_LDL_advwet <- rma(yi = yi_LDL_ctrl_advwet_comb, vi = vi_LDL_ctrl_advwet_comb, mods = ~ classification_method-1, data = alldata5)
m_subgroup1_LDL_advwet

m_subgroup_regression_LDL_advwet <- rma(yi = yi_LDL_ctrl_advwet_comb, vi = vi_LDL_ctrl_advwet_comb, mods = ~ classification_method, data = alldata5)
m_subgroup_regression_LDL_advwet

#subgroup analysis; by origin of data (OR vs. means)
m_subgroup2_LDL_advwet <- rma(yi = yi_LDL_ctrl_advwet_comb, vi = vi_LDL_ctrl_advwet_comb, mods = ~ Ratio_type-1, data = alldata5)
m_subgroup2_LDL_advwet

m_subgroup2_LDL_advwet <- rma(yi = yi_LDL_ctrl_advwet_comb, vi = vi_LDL_ctrl_advwet_comb, mods = ~ Ratio_type, data = alldata5)
m_subgroup2_LDL_advwet

#meta-regression
m_metareg_LDL_advwet <- rma(yi = yi_LDL_ctrl_advwet_comb,
                            vi = vi_LDL_ctrl_advwet_comb,
                            mods = ~proportion_f,
                            data = alldata5)
m_metareg_LDL_advwet

#publication bias via funnel plot
funnel(m_LDL_advwet, xlab = "Hedges' g")
funnel(m_LDL_advwet, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
ranktest(m_LDL_advwet)

svg(file='funnel_LDL_advwet.svg') # Open svg device with specific file name
funnel(m_LDL_advwet, xlab = "Hedges' g")
funnel(m_LDL_advwet, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), refline=0)
dev.off() # Turn the svg device off
#Robust variance estimation
clubSandwich::coef_test(m_LDL_advwet, vcov = "CR2")

#sensitivity analysis for removing Park studies
df_no_Park2_LDL_advwet <- alldata5[-c(26, 28),] #create new df excluding the outliers

m_no_Park2_LDL_advwet <- rma.mv(yi_LDL_ctrl_advwet_comb, vi_LDL_ctrl_advwet_comb, random = list(~ 1 | EffectID, ~ 1 | StudyID), data = df_no_Park2_LDL_advwet)
m_no_Park2_LDL_advwet

cat("G = ", round(m_no_Park2_LDL_advwet$b[1], 2),
    ", 95% CI [", round(m_no_Park2_LDL_advwet$ci.lb, 2),
    ", ", round(m_no_Park2_LDL_advwet$ci.ub, 2), "] (no Park)", sep = "")

cat("G = ", round(m_LDL_advwet$b[1], 2),
    ", 95% CI [", round(m_LDL_advwet$ci.lb, 2),
    ", ", round(m_LDL_advwet$ci.ub, 2), "]", sep = "")

forest(m_no_Park2_LDL_advwet, slab = df_no_Park2_LDL_advwet$ID_2, addcred = TRUE, showweights = TRUE)








