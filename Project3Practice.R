#VARIOUS COMMANDS FOR PROJECT 3:
  
birthwt_adj <- with(birthwt, data.frame(low=as.factor(low), age=age, lwt=lwt, race=as.factor(race), 
                                        smoke=as.factor(smoke), pt=pt, ht=as.factor(ht), ui=as.factor(ui), ftv=ftv))

bwt <- with(birthwt, {
  race <- factor(race, labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0)
  ftv <- factor(ftv)
  levels(ftv)[-(1:2)] <- "2+"
  data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0),
             ptd, ht = (ht > 0), ui = (ui > 0), ftv)
})
options(contrasts = c("contr.treatment", "contr.poly"))
glm(low ~ ., binomial, bwt)


with(bwt_adj, pairs(low~age+lwt+ptl+ftv))
with(bwt_adj, cor(cbind(low,age,lwt,ptl,ftv)))

#This output gives us insight to which variables are strongly correlated with our response variable (low) or that 
#are strongly correlated with each other.
#Based on the scatter plots, age and lwt seem to be moderately positively correlated. 
#The remaining scatter plots all display no pattern within the data. 
#So it is concluded that the rest of the scatter plots seem to display no true correlation between covariates.
#Based on the correlation
