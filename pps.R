#https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html
install.packages("ResourceSelection")
install.packages("pROC")
install.packages("car")
install.packages("car")
install.packages("rsq")
install.packages("rms")

#additions
install.packages("dplyr")
install.packages("magrittr")
install.packages("MatchIt")

library(dplyr)
library(ggplot2)
library(MatchIt)
library(gridExtra)
library(pROC)
library(ResourceSelection)
library(car)
library(rsq)
library(rms)

# additions
library(magrittr)

dataset <- read.csv(path)


dataset %>%
  group_by(treated) %>%
  summarise(observations = n(),
            mean_math = mean(change),
            std_error = sd(change) / sqrt(observations))


#Welch Two Sample t-test
with(dataset, t.test(change ~ treated))



#Covariate means
dataset_cov <-  c('sett_mean', 'pop_mean', 'road_mean')   #c    , 'ag16_sum'            ('vv_max', 'vv_min', 'vv_std')
dataset %>%
  group_by(treated) %>%
  select(one_of(dataset_cov)) %>%
  summarise_all(list(~mean(., na.rm = T)))


# T-tests
lapply(dataset_cov, function(v) {
  t.test(dataset[, v] ~ dataset[, 'treated'])
})


# GLM
m_ps = glm(treated ~ pop_mean+road_mean+sett_mean,
            family = binomial(), data = dataset)
summary(m_ps)


# R2
rsq(m_ps,adj=FALSE,type=c('v','kl','sse','lr','n')) #,data=NULL)


# Multicollinearity
vif(m_ps)



#### ROC and AUC ####

par(pty = "s")
roc(dataset$treated, m_ps$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4)



# Propensity score calculation for each observation
prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     treated = m_ps$model$treated)
#head(prs_df)


# Examining the region of common support

labs <- paste("treatment status:", c("treated", "not treated"))
prs_df %>%
  mutate(treated = ifelse(treated == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~treated) +
  xlab("Probability of being treated") +
  theme_bw()



# Matching using MatchIt

dataset_nomiss <- dataset %>%  # MatchIt does not allow missing values
  select(change, treated, one_of(dataset_cov)) %>%
  na.omit()

mod_match <- matchit(treated ~ pop_mean + road_mean + sett_mean,
                     method = "nearest", data = dataset_nomiss)


# Assessing success of matching
summary(mod_match)
plot(mod_match)



# Get data frame containing only matched observations (distance is the propensity score)
dta_m <- match.data(mod_match)
dim(dta_m)
print(dta_m)


#uses library(gridExtra)
grid.arrange(
  fn_bal(dta_m, "sett_mean"),
  fn_bal(dta_m, "pop_mean"),
  fn_bal(dta_m, "road_mean"),
  #fn_bal(dta_m, "ag16_sum"), #+ theme(legend.position = "none"),
  nrow = 3, widths = c(1, 0.8)
)
print(dta_m)


###Examining covariate balance###

#Difference-in-means

dta_m %>%
  group_by(treated) %>%
  select(one_of(dataset_cov)) %>%
  summarise_all(funs(mean))


# More formally using t-test (ideally we should not be able to reject the null-hypothesis (being: there is no mean difference) for each covariate)

lapply(dataset_cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$treated)
})



# Estimating treatment effect (once we have a satisfactory match)


# three things needed:
#the mean difference, the standard deviation of each group, and the number of data values of each group. 
#T-test

with(dta_m, t.test(change ~ treated))



#OLS - No covariates
lm_treat1 <- lm(change ~ treated, data = dta_m)
summary(lm_treat1)


#OLS - With covariates
lm_treat2 <- lm(change ~ treated + pop_mean + road_mean + sett_mean, data = dta_m) #sett_mean
summary(lm_treat2)
