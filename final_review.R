library(mvtnorm)
library(survival)
library(splines)
library(multcomp)
library(DescTools)
library(agricolae)
library(ggplot2)
library(Hmisc)

setwd('/Users/travis/Documents/Education/SMU/OneDrive - Southern Methodist University/Code/6371_hwk/6371_finalexam')

####Homework 3 -- T-test

# Gather descriptive statistics
Fired = c(34,37,37,38,41,42,43,44,44,45,45,45,46,48,49,53,53,54,54,55,56)
Not_Fired = c(27,33,36,37,38,38,39,42,42,43,43,44,44,44,45,45,45,45,46,46,47,47,48,48,49,49,51,51,52,54)
qqnorm(Fired, main='Fired')
qqnorm(Not_Fired, main='Not Fired')
hist(Fired, main='Fired')
hist(Not_Fired, main='Not Fired')
boxplot(Fired, Not_Fired, names=c('Fired', 'Not_fired'))

# t-test and Log transformation
smu = c(34, 1200, 23, 50, 60, 50, 0, 0, 30, 89, 0, 300, 400, 20, 10, 0)
sea = c(20, 10, 5, 0, 30, 50, 0, 100, 110, 0, 40, 10, 3, 0)
sea[sea == 0] = 1
smu[smu == 0] = 1
smu = log(smu)
sea= log(sea)
t.test(smu, sea, c('two.sided', 'less', 'greater'))
test <- t.test(smu, sea, 'two.sided')
test$conf.int # <- gives vector of length 2 containing confidence interval
test$statistic # <- gives test (T) statistic for the t.test

### Homework 4 -- Non-parametric Testing and wilcoxon rank sum

logging = read.csv('data/logging.csv')
names(logging) = c('action', 'percent_lost')
logged = subset(logging, action == 'L')$percent_lost
unlogged = subset(logging, action == 'U')$percent_lost
#boxplot from data.frame
boxplot(logging$percent_lost~action, data=logging, xlab='Logging Action', ylab='Percent Lost')
wilcox.test(logged, unlogged, correct=FALSE, exact = TRUE, conf.level= .95)# <- gives a W value
# Paired Rank test
children <- data.frame(c(1,2,3,4,5,6,7,8,9),
                       c(85,70,60,45,80,75,55,20,70),
                       c(70,50,50,40,20,65,40,25,30))
names(children) <- c('id', 'before', 'after')
children$diff = children$after - children$before
wilcox.test(children$before, children$after,
            paired= TRUE,
            conf.level= .95)

### HOMEWORK 5 -- ANOVA
income_df = read.csv('data/ex0525_2.csv')
names(income_df) <- tolower(names(income_df))
income_df$logincome <- log(income_df$income2005)
income_anova <- aov(logincome~educ, data= income_df)
# build your own ANOVA
income_df$grp16s = gsub('>', '', income_df$educ) # <- take a subset of original data
sub_anova = aov(logincome~grp16s, data= income_df)# <- find the anova for the new data
f <- 71.89 / 54.41 # <- Divide the total MSE by the sub_model MSE (obtained in summary) and use as F value
# obtain p valiue by from table of f_values and dof as the SSError from models
p_val = pf(f, sum(income_anova$residuals **2), sum(sub_anova$residuals **2)) 
# If ANOVA assumptions (most importantly equal varaiance) is not met, we can use Kruskal.Wallis test (non-parametric ANOVA)
kruskal.test(income2005~educ, data= income_df)# <- uses chi squared 


### HOMEWORK 6 -- Multi Group Testing
hcap <- read.csv('data/case0601handicap_2.csv')
names(hcap) = sapply(names(hcap), tolower)
amp.crutches <- subset(hcap, handicap == 'Amputee' | handicap == 'Crutches') # <- subset with crutches or amputee
amp.wheelchair <- subset(hcap, handicap == 'Amputee' | handicap == 'Wheelchair')
crutches.wheelchair <- subset(hcap, handicap == 'Crutches' | handicap == 'Wheelchair')
## bonferroni, tukey, and Dunnet's test
model = lm(score ~ handicap, data=hcap)
lsd <- LSD.test(model, "handicap", alpha = 0.05, p.adj="bonferroni")
lsd # <- gives outputs for lsd
mc = glht(model, mcp(handicap = "Tukey"))# <- tukey_kramer testing
summary(mc, test=adjusted("single-step"))# <- special code to grab output
DunnettTest(score ~ handicap, data = hcap, control='None')# <- compare all to one testing (Dunnetts)

### HOMEWORK 8 -- Coorelations
baseball <- read.csv('data/Baseball_Data.csv')
names(baseball) <- sapply(names(baseball), tolower)
cor(baseball$payroll, baseball$wins)# <- give coorelation coefficent


### HOMEWORK 9 -- Single Linear regression
model <- lm(payroll~wins, data=baseball)
confint(model)# <- gives confidence intervals for the coefficents

## HOMEWORK 10 -- Validating Linear Regression
wheaters <- read.csv('data/MaleDisplayDataSet_2_2.csv')
names(wheaters) <- sapply(names(wheaters), tolower)
model = lm(mass~tcell, data=wheaters)
wheaters$resid <- model$residuals
slope <- model$coefficients['tcell']
intercept <- model$coefficients[1]
# plot regression line with confidence intervals, residuals and prediction intervals
pred.int =  predict(model, interval="prediction")
#Confidence intervals
conf.int =  predict(model, interval="confidence")
wheaters$pred.lower <- pred.int[,2]
wheaters$pred.upper <- pred.int[,3]
wheaters$ci.upper <- conf.int[,2]
wheaters$ci.lower <- conf.int[,3]
ggplot(data=wheaters, aes(x=tcell, y=mass, main='Scatterplot of data with regression line and Confidence intervals')) + 
  geom_point(color= 'red') +
  geom_abline(intercept=intercept, slope=slope, color='black') +
  geom_ribbon(data=wheaters, aes(ymin= pred.lower, ymax= pred.upper), fill = "blue", alpha = 0.2) +
  geom_ribbon(data=wheaters, aes(ymin= ci.lower, ymax= ci.upper), fill = "cyan", alpha = 0.3) 

summary(model)
print(confint(model, level=.975))
# Find critical t-value for slope
qt(.975, 19)


