install.packages('tidyverse')
library(dplyr)
library(ggplot2)
library(MASS)
install.packages('gvlma')
library(gvlma)
install.packages("pkgconfig", dependencies = TRUE)
install.packages('car')
library(car)

burn.out = read.csv('test.csv')
burn.out = data.frame(burn.out)
summary(burn.out)



#Mutating the data for multiple linear regression analysis

burn.out = burn.out %>%
  mutate(Gender=relevel(Gender,ref = 'Male')) %>%
  mutate(Designation = factor(Designation)) %>%
  mutate(Resource.Allocation = factor(Resource.Allocation))



#EDA with ggplots

WFH.box = ggplot(data=burn.out, aes(x=WFH.Setup.Available, y=Mental.Fatigue.Score)) +
  geom_boxplot() + xlab('Ability to Work From Home') + 
  ylab('Mental Fatigue Score')

Designation.box = ggplot(data=burn.out, aes(x=factor(Designation), y=Mental.Fatigue.Score)) + 
  geom_boxplot()
Designation.box

total.burn = ggplot(data = burn.out, aes(x=Mental.Fatigue.Score)) +
  geom_bar()
total.burn


#Multiple Linear Regression Analysis 

model2 = lm(Mental.Fatigue.Score ~ Company.Type + Gender + 
            WFH.Setup.Available + Designation + Resource.Allocation,
            data=burn.out)
summary(model2)
gvlma(model2)


#attemping to make a few changes to improve linearity 

model.gender = lm(Mental.Fatigue.Score ~ Gender, data = burn.out)
model.company.type = lm(Mental.Fatigue.Score ~ Company.Type, data = burn.out)
model.wfh = lm(Mental.Fatigue.Score ~ WFH.Setup.Available, data = burn.out)
model.designation = lm(Mental.Fatigue.Score ~ Designation, data = burn.out)
model.resource = lm(Mental.Fatigue.Score ~ Resource.Allocation, data = burn.out)

#AIC

models = c(model.gender, model.company.type, model.wfh, model.designation,
           model.resource)
model.empty = lm(Mental.Fatigue.Score ~ 1, data = burn.out)
scope = list(lower = formula(model.empty), upper = formula(model2))

forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
summary(forwardAIC)


model.burn.reduced = lm(Mental.Fatigue.Score ~ Resource.Allocation + WFH.Setup.Available + 
                          Designation +Gender, data = burn.out)
summary(model.burn.reduced)
influencePlot(forwardAIC)
plot(model.burn.reduced)

#Model is still inaccurate there may not be a perfect way of doing this
#removing the most extremes in mental fatigue score may help.
#could be an issue with the survey a large amount voted 10 or zero


burn.no.extremes = burn.out %>%
  filter(Mental.Fatigue.Score > 0 & Mental.Fatigue.Score <10)

bne.model = lm(Mental.Fatigue.Score ~ Company.Type + Gender + 
                 WFH.Setup.Available + Designation + Resource.Allocation,
               data=burn.no.extremes)
summary(bne.model)
bne.model.empty = lm(Mental.Fatigue.Score ~ 1, data = burn.no.extremes)
bne.scope = list(lower = formula(bne.model.empty), upper = formula(bne.model))
forwardAIC.bne = step(bne.model.empty, bne.scope, direction = "forward", k = 1)
bne.model.reduced = lm(Mental.Fatigue.Score ~ Resource.Allocation + WFH.Setup.Available + 
                         Designation + Gender, data= burn.no.extremes)
summary(bne.model.reduced)
# Its EVEN WORSE



#Model appears to be nonlinear attempting to noramlize the data
#Decided not to go this route due to it possibly over complicating
#going to pivot a little bit

burn.out2 = burn.out %>%
  mutate(Mental.Fatigue.Score = Mental.Fatigue.Score + 1)

burn2.trans = boxcox(burn.out2$Mental.Fatigue.Score ~ burn.out2$Company.Type + 
                       burn.out2$Gender + 
                       burn.out2$WFH.Setup.Available + 
                       burn.out2$Designation + 
                       burn.out2$Resource.Allocation)
lambda = burn2.trans$x[which.max(burn2.trans$y)]
burn.out2 = burn.out2 %>%
  mutate(Mental.Fatigue.Score = (Mental.Fatigue.Score^lambda-1)/lambda) 
         
model.burn2.bc = lm(Mental.Fatigue.Score ~ Company.Type + 
                      Gender + 
                      WFH.Setup.Available + 
                      Designation + 
                      Resource.Allocation, data=burn.out2)
summary(model.burn2.bc)

burn2.empty = lm(Mental.Fatigue.Score ~ 1, data = burn.out2)
burn2.scope = list(lower = formula(burn2.empty), upper = formula(model.burn2.bc))
forwardAIC.burn2 = step(burn2.empty, burn2.scope, direction = "forward", k = 2)
burn2.reduced = lm(Mental.Fatigue.Score ~ Resource.Allocation + WFH.Setup.Available + 
                     Designation + Gender, data=burn.out2)
summary(burn2.reduced)
gvlma(burn2.reduced)

#While this is better. I am worried that too many transformation
#will over complicate the model



# Testing the possibility to use predictive modeling based on 
# designation

low.tenure = burn.out %>%
  mutate(Designation = as.numeric(as.character(Designation))) %>%
  filter(Designation <= 2)

high.tenure = burn.out %>%
  mutate(Designation = as.numeric(as.character(Designation))) %>%
  filter(Designation > 2)

low.tenure.model = lm(Mental.Fatigue.Score ~ Resource.Allocation + WFH.Setup.Available + 
                                        Gender, data=low.tenure)
summary(low.tenure.model)
high.tenure.model = lm(Mental.Fatigue.Score ~ Resource.Allocation + WFH.Setup.Available + 
                         Gender, data=high.tenure)
summary(high.tenure.model)

#Missed the mark .. RIP

summary(model.burn.reduced)
