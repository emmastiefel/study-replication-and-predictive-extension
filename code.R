###DATA PROCESSING AND SETUP #####
library(Matching)

#2089 girls in 3 rounds and sample, so all the girls who were selected into the sample and retained throughout the study
#original paper: "We began with a sample of 2,284 respondents who were in school at baseline and formed the experimental sample for our study of conditionality. Of this sample, 2,186 were tracked successfully for the Round 3 household survey and 2,089 were successful interviewed in all three rounds, a tracking rate of over 90%."
dataset <- read.csv("~/cs112_final_project_data.csv")


#this dataset contains 2089*3 observations, one for each girl in each of 3 rounds

#round 3 data only
round3data <- dataset[dataset$round == 3, ]
length(round3data$corespid.R2)#2089

#cut out those with missing test scores
round3data <-  round3data[which(round3data$eng.std != 0 | round3data$math.malawi.std != 0 | round3data$TIMMS.std != 0 | round3data$cog.std != 0),]
length(round3data$corespid.R2)
#2,059, not the 2,057 listed in the paper

#trying to find those pesky missing data units
round3data[is.na(round3data$asset.index.baseline), ]
#found them! rows 1302 and 1902 id  1409705 1587603
round3data <- round3data[-which(is.na(round3data$asset.index.baseline)), ]
length(round3data$corespid.R2) #should be 2057


###REPLICATING TABLE VI#####
# age dummies, strata dummies, household asset index, highest grade attended, an indicator for ever had sex, and whether the respondent participated in the pilot phase of the development of testing instruments.
eng_lm <- lm(eng.std ~ cond.treat.year3 + uncond.treat.year3 + .Iage.R1.14 + .Iage.R1.15 + .Iage.R1.16 + .Iage.R1.17 + .Iage.R1.18 + .Iage.R1.19 + .Iage.R1.20 + stratum1 + stratum2 + asset.index.baseline + highest.grade.baseline + never.had.sex.baseline + eng.pilot + math.pilot, data=round3data, weights=round3data$wgt.SSR3)
summary(eng_lm)

math_lm <- lm(math.malawi.std ~ cond.treat.year3 + uncond.treat.year3 + .Iage.R1.14 + .Iage.R1.15 + .Iage.R1.16 + .Iage.R1.17 + .Iage.R1.18 + .Iage.R1.19 + .Iage.R1.20 + stratum1 + stratum2 + asset.index.baseline + highest.grade.baseline + never.had.sex.baseline + eng.pilot + math.pilot, data=round3data, weights=round3data$wgt.SSR3)
summary(math_lm)

timms_lm <- lm(TIMMS.std ~ cond.treat.year3 + uncond.treat.year3 + .Iage.R1.14 + .Iage.R1.15 + .Iage.R1.16 + .Iage.R1.17 + .Iage.R1.18 + .Iage.R1.19 + .Iage.R1.20 + stratum1 + stratum2 + asset.index.baseline + highest.grade.baseline + never.had.sex.baseline + eng.pilot + math.pilot, data=round3data, weights=round3data$wgt.SSR3)
summary(timms_lm)

cog_lm <- lm(cog.std ~ cond.treat.year3 + uncond.treat.year3 + .Iage.R1.14 + .Iage.R1.15 + .Iage.R1.16 + .Iage.R1.17 + .Iage.R1.18 + .Iage.R1.19 + .Iage.R1.20 + stratum1 + stratum2 + asset.index.baseline + highest.grade.baseline + never.had.sex.baseline + eng.pilot + math.pilot, data=round3data, weights=round3data$wgt.SSR3)
summary(cog_lm)


psens(x = round3data[round3data$group == 5,]$eng.std, y = round3data[round3data$group == 4,]$eng.std, Gamma = 2, GammaInc = 0.1)


#####EXTENSION: Can we predict whether a girl will do better under a uct or a cct?#####

##how good is baird et al.'s cutoff? who does it miss? 

#what percentage of girls was under 16 but still got married? 
length(round3data[round3data$age.R1 < 16 & round3data$ever.married == TRUE, ]$corespid.R2) / length(round3data$corespid.R2)
#about 7 percent

#what percentage of girls was under 16 but still got pregnant? 
length(round3data[round3data$age.R1 < 16 & round3data$ever.pregnant == TRUE, ]$corespid.R2) / length(round3data$corespid.R2)
#about 9 percent

#what percentage of girls was under 16 but got married and pregnant?
length(round3data[round3data$age.R1 < 16 & round3data$ever.pregnant == TRUE & round3data$ever.married == TRUE, ]$corespid.R2) / length(round3data$corespid.R2)
#about 5 percent


####building data we'll need to train + test our model#####

###generating overall success score: 
##we will combine the separate outcome variables measured by the original study into a single value that represents overall success, combining schooling and marriage/pregnancy outcomes

#success = marriage * .25 + pregnant * .25 + test scores * .20 (multiply each of four scores by .05) + enrollment * .15
##we originally intended to include attendance, but are missing a large portion of the data
#measure each individual outcome on scale from 0 (worst) to 1 (best)
#then overall, closer to 1 means more success

##rescales each test score to be positive
eng.shift <- min(round3data$eng.std) * -1
eng.scale <- max(round3data$eng.std) + eng.shift
math.shift <- min(round3data$math.malawi.std) * -1
math.scale <- max(round3data$math.malawi.std) + math.shift
timms.shift <- min(round3data$TIMMS.std) * -1
timms.scale <- max(round3data$TIMMS.std) + timms.shift
cog.shift <- min(round3data$cog.std) * -1
cog.scale <- max(round3data$cog.std) + cog.shift

##initializes vectors to score life, school, and overall outcomes
life.success <- rep(0, length(round3data$corespid.R2))
school.success <- rep(0, length(round3data$corespid.R2))
overall.success <- rep(0, length(round3data$corespid.R2))


##iterate through each unit measured in round 3, calculates score for each
for (i in 1:length(round3data$corespid.R2)){

  #both binary variables where 1 = marriage/pregnancy
  #so optimal outcome is 0
  marriage.i <- (1 - round3data$ever.married[i]) * 0.25
  #1 unit missing married data
  pregnant.i <- (1 - round3data$ever.pregnant[i]) * 0.25
  #0 units missing pregnant data
  #overall success for avoiding marriage/pregnancy
  life.i <- marriage.i + pregnant.i
  #store outcome
  life.success[i] <- life.i
  
  #max of six terms enrolled from start of summary, so success is percentage out of 6
  #3 units missing enroll data
  enrollment.i <- (round3data$num.terms.enrolled[i] / 6) * .15
  #already a fraction, 1 = 100% attendance
  #shifts score scale so that 0 is the lowest and max + shift value is highest
  eng.i <- ((round3data$eng.std[i] +  eng.shift) / eng.scale) * .05
  math.i <- ((round3data$math.malawi.std[i] +  math.shift) / math.scale) * .05
  timms.i <- ((round3data$TIMMS.std[i] +  timms.shift) / timms.scale) * .05
  cog.i <- ((round3data$cog.std[i] +  cog.shift) / cog.scale) * .05
  #overall success for school
  school.i <- enrollment.i + attendance.i + eng.i + math.i + timms.i + cog.i
  #store outcome
  school.success[i] <- school.i
  
  #overall success outcome
  success.i <- life.i + school.i 
  #store outcome
  overall.success[i] <- success.i
}
hist(overall.success) #seems legit

#adds new success columns to round3data
round3data$life.success <- life.success
round3data$school.success <- school.success
round3data$overall.success <- overall.success

str(round3data) # seems legit hooray


###generating paired baseline characteristics######

#we are only matching those who received treatment, so not group = 3 (controls)
transfer <- round3data[round3data$group != 3, ]

#we will pair unconditional and conditional treatment units based on household size, asset index, female headed household, mobile phone access, age, highest grade attended, mother alive, father alive, never had sex, ever pregnant r1, strata, the variables they already measured balance on

#father.alive is missing for one, will kick out for now
transfer <- transfer[-which(is.na(transfer$father.alive)),]

covars <- cbind(transfer$asset.index.baseline, transfer$age.R1, transfer$highest.grade.baseline, transfer$mother.alive, transfer$father.alive, transfer$never.had.sex.baseline, transfer$stratum1)
#unfrortunately missing data for  hhsize, female.headed, mobile, ever.preg.r1

##pair up units
set.seed(505)
#treatment is T2b, 1 for unconditional 0 for conditional
gm <- GenMatch(Tr=transfer$T2b, X=covars, pop.size=500, max.generations = 25, wait.generations=10, exact = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
m.out <- Match(Tr=transfer$T2b, X=covars, Weight.matrix=gm, exact = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
overall.mb <- MatchBalance(T2b ~ asset.index.baseline + age.R1 + highest.grade.baseline + mother.alive + father.alive + never.had.sex.baseline + stratum1, data=transfer, match.out=m.out, nboots=500)

##passes matched pairs into a new dataset
model.data <- m.out$mdata$X
colnames(model.data) <- c("asset.index.baseline", "age.R1", "highest.grade.baseline", "mother.alive", "father.alive", "never.had.sex.baseline", "stratum1")

#initialize columns to store pair averages
asset <- rep(0, length(model.data[,1]) / 2)
age <- rep(0, length(model.data[,1]) / 2)
grade <- rep(0, length(model.data[,1]) / 2)
mother <- rep(0, length(model.data[,1]) / 2)
father <- rep(0, length(model.data[,1]) / 2)
sex <- rep(0, length(model.data[,1]) / 2)
stratum <- rep(0, length(model.data[,1]) / 2)

##go through each pair and create average characteristics
k <- 1
for (j in 1:(length(model.data[,1]) / 2)) {
  asset.k <- mean(c(model.data[k,1], model.data[k + 1,1]))
  asset[j] <- asset.k
  age.k <- mean(c(model.data[k, 2], model.data[k+1, 2]))
  age[j] <- age.k
  grade.k <- mean(c(model.data[k, 3], model.data[k+1, 3]))
  grade[j] <- grade.k
  mother.k <- mean(c(model.data[k, 4], model.data[k+1, 4]))
  mother[j] <- mother.k
  father.k <- mean(c(model.data[k, 5], model.data[k+1, 5]))
  father[j] <- father.k
  sex.k <- mean(c(model.data[k, 6], model.data[k+1, 6]))
  sex[j] <- sex.k
  stratum.k <- mean(c(model.data[k, 7], model.data[k+1, 7]))
  stratum[j] <- stratum.k

  k <- k + 2
  
}

building.data <- data.frame(cbind(asset, age, grade, mother, father, sex, stratum))  

#initialize vector to store outcome differentials
outcome.difference <- rep(0, length(model.data[,1]) / 2)
##go through each pair and create outcome differential
k <- 1
for (j in 1:(length(model.data[,1]) / 2)) {
  u <- m.out$index.treated[j]
  c <- m.out$index.control[j]
  uct.outcome <- transfer[u,]$overall.success
  cct.outcome <- transfer[c,]$overall.success
  uct.life <- transfer[u,]$life.success
  cct.life <- transfer[c,]$life.success
  uct.school <- transfer[u,]$school.success
  cct.school <- transfer[c,]$school.success
  
  ##improved success comparison:
  
  ##if theyre the same, only look at school
  if(uct.life == cct.life){
    outcome.diff <- uct.school - cct.school
  }
  
  ##if school mostly looks the same only look at life
  else if (abs(uct.school - cct.school) < 0.05){
    outcome.diff <- uct.life - cct.life
  }
    
  ##otherwise look at overall difference?
  else{
    outcome.diff <- uct.outcome - cct.outcome
  }
  #do not transform outcome.diff to a boolean yet because we want the algorithm to see differences between those who were very different and those who were almost the same
  outcome.difference[j] <- outcome.diff

  k <- k + 2
}
#append outcome column to building dataset
building.data <- cbind(building.data, outcome.difference)

#####training and testing the model#####

#subset training data
training.data <- building.data[1:100,]
#subset test data
test.data <- building.data[101:length(building.data[,1]),]

#very simple model, just checking if we can do better than simple cutoff
predict.model <- lm(outcome.difference ~ asset + age + grade + mother + father + sex + stratum, data=training.data)

summary(predict.model)

#running model on test data
test.out <- predict(predict.model, newdata = test.data)

##convert test.out and actual results to predictions, TRUE if above 0, which would mean a UCT recommendation, and vice versa for CCT
test.predict <- test.out > 0
actual.predict <- test.data$outcome.difference > 0
##print percentage of matching predictions

model.accuracy <- length(which(test.predict == actual.predict)) / length(test.predict)
#our primitive model has 58% accuracy

#testing the 16 year old cutoff predictions, so each person greater than 16 gets uct/true
cutoff.predict <- test.data$age > 16
cutoff.accuracy <- length(which(cutoff.predict == actual.predict)) / length(test.predict)

diff <- model.accuracy - cutoff.accuracy

