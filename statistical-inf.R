library(plyr)

emp <- read.csv(file ="emp.csv")
df1_complete <- na.omit(emp)

Q1 <- factor(df1_complete$Age, levels=c("26-30 years","31-40 years","20-25 years"), labels=c(0,1,2))
df1_complete$Age[, c("26-30 years","31-40 years","20-25 years")] <- sapply(df1_complete$Age[, c(1,2,3)], unclass)

mean(df1_complete$Age_convert)
 
age = table(df1_complete$Age)

Mean_I.value.the.people.I.work.with <- mean(df1_complete$I.value.the.people.I.work.with)

Mode_I.value.the.people.I.work.with <- mode(df1_complete$I.value.the.people.I.work.with)
median_I.value.the.people.I.work.with <- median(df1_complete$I.value.the.people.I.work.with)
range_I.value.the.people.I.work.with <- range_I.value.the.people.I.work.with(df1_complete$I.value.the.people.I.work.with)
range_I.value.the.people.I.work.with <- range(df1_complete$I.value.the.people.I.work.with)
variance_I.value.the.people.I.work.with <- var(df1_complete$I.value.the.people.I.work.with)
quantiles_I.value.the.people.I.work.with <- quantile(df1_complete$I.value.the.people.I.work.with,na.rm = T,probs = c(0.25,0.75))
sd_I.value.the.people.I.work.with <- sd(df1_complete$I.value.the.people.I.work.with)
table(df1_complete$I.value.the.people.I.work.with)
quantiles_I.value.the.people.I.work.with <- quantile(df1_complete$I.value.the.people.I.work.with,na.rm = T,probs = c(0.25,0.75))


mean(df1_complete$I.enjoy.working.with.the.people.at.my.job)
mode(df1_complete$I.enjoy.working.with.the.people.at.my.job)
median(df1_complete$I.enjoy.working.with.the.people.at.my.job)
range(df1_complete$I.enjoy.working.with.the.people.at.my.job)
var(df1_complete$I.enjoy.working.with.the.people.at.my.job)
quantile(df1_complete$I.enjoy.working.with.the.people.at.my.job,na.rm = T,probs = c(0.25,0.75))
sd(df1_complete$I.enjoy.working.with.the.people.at.my.job)
table(df1_complete$I.enjoy.working.with.the.people.at.my.job)


mean(df1_complete$I.get.along.well.with.the.people.at.my.job)
mode(df1_complete$I.get.along.well.with.the.people.at.my.job)
median(df1_complete$I.get.along.well.with.the.people.at.my.job)
range(df1_complete$I.get.along.well.with.the.people.at.my.job)
var(df1_complete$I.get.along.well.with.the.people.at.my.job)
quantile(df1_complete$I.get.along.well.with.the.people.at.my.job,na.rm = T,probs = c(0.25,0.75))
sd(df1_complete$I.get.along.well.with.the.people.at.my.job)
table(df1_complete$I.get.along.well.with.the.people.at.my.job)


mean(df1_complete$I.have.a.relationship.of.trust.with.the.people.at.my.job)
mode(df1_complete$I.have.a.relationship.of.trust.with.the.people.at.my.job)
median(df1_complete$I.have.a.relationship.of.trust.with.the.people.at.my.job)
range(df1_complete$I.have.a.relationship.of.trust.with.the.people.at.my.job)
var(df1_complete$I.have.a.relationship.of.trust.with.the.people.at.my.job)
quantile(df1_complete$I.have.a.relationship.of.trust.with.the.people.at.my.job,na.rm = T,probs = c(0.25,0.75))
sd(df1_complete$I.have.a.relationship.of.trust.with.the.people.at.my.job)
table(df1_complete$I.have.a.relationship.of.trust.with.the.people.at.my.job)


mean(df1_complete$I.find.my.job.exciting)
mode(df1_complete$I.find.my.job.exciting)
median(df1_complete$I.find.my.job.exciting)
range(df1_complete$I.find.my.job.exciting)
var(df1_complete$I.find.my.job.exciting)
quantile(df1_complete$I.find.my.job.exciting,na.rm = T,probs = c(0.25,0.75))
sd(df1_complete$I.find.my.job.exciting)
table(df1_complete$I.find.my.job.exciting)

mean(df1_complete$I.like.my.job)
mode(df1_complete$I.like.my.job)
median(df1_complete$I.like.my.job)
range(df1_complete$I.like.my.job)
var(df1_complete$I.like.my.job)
quantile(df1_complete$I.like.my.job,na.rm = T,probs = c(0.25,0.75))
sd(df1_complete$I.like.my.job)
table(df1_complete$I.like.my.job)

hist( table(df1_complete$I.like.my.job))
hist( table(df1_complete$I.value.the.people.I.work.with))
hist( table(df1_complete$I.enjoy.working.with.the.people.at.my.job))
hist( table(df1_complete$I.get.along.well.with.the.people.at.my.job))
hist( table(df1_complete$I.have.a.relationship.of.trust.with.the.people.at.my.job))
hist( table(df1_complete$I.find.my.job.exciting))
hist( table(df1_complete$I.like.my.job))



mean(df1_complete$I.am.proud.of.the.job.I.have)
mode(df1_complete$I.am.proud.of.the.job.I.have)
median(df1_complete$I.am.proud.of.the.job.I.have)
range(df1_complete$I.am.proud.of.the.job.I.have)
var(df1_complete$I.am.proud.of.the.job.I.have)
quantile(df1_complete$I.am.proud.of.the.job.I.have,na.rm = T,probs = c(0.25,0.75))
sd(df1_complete$I.am.proud.of.the.job.I.have)
table(df1_complete$I.am.proud.of.the.job.I.have)
hist(table(df1_complete$I.am.proud.of.the.job.I.have))


mean(df1_complete$I.find.meaning.in.my.work)
mode(df1_complete$I.find.meaning.in.my.work)
median(df1_complete$I.find.meaning.in.my.work)
range(df1_complete$I.find.meaning.in.my.work)
var(df1_complete$I.find.meaning.in.my.work)
quantile(df1_complete$I.find.meaning.in.my.work,na.rm = T,probs = c(0.25,0.75))
sd(df1_complete$I.find.meaning.in.my.work)
table(df1_complete$I.find.meaning.in.my.work)
hist(table(df1_complete$I.find.meaning.in.my.work))


mean(df1_complete$I.know.I.am.capable.of.doing.my.job)
mode(df1_complete$I.know.I.am.capable.of.doing.my.job)
median(df1_complete$I.know.I.am.capable.of.doing.my.job)
range(df1_complete$I.know.I.am.capable.of.doing.my.job)
var(df1_complete$I.know.I.am.capable.of.doing.my.job)
quantile(df1_complete$I.know.I.am.capable.of.doing.my.job,na.rm = T,probs = c(0.25,0.75))
sd(df1_complete$I.know.I.am.capable.of.doing.my.job)
table(df1_complete$I.know.I.am.capable.of.doing.my.job)
hist(table(df1_complete$I.know.I.am.capable.of.doing.my.job))



mean(df1_complete$I.feel.confident.at.work)
mode(df1_complete$I.feel.confident.at.work)
median(df1_complete$I.feel.confident.at.work)
range(df1_complete$I.feel.confident.at.work)
var(df1_complete$I.feel.confident.at.work)
quantile(df1_complete$I.feel.confident.at.work,na.rm = T,probs = c(0.25,0.75))
sd(df1_complete$I.feel.confident.at.work)
table(df1_complete$I.feel.confident.at.work)
hist(table(df1_complete$I.feel.confident.at.work))
hist(table(df1_complete$I.know.I.am.capable.of.doing.my.job))


mean(df1_complete$I.feel.confident.at.work)
mode(df1_complete$I.feel.confident.at.work)
median(df1_complete$I.feel.confident.at.work)
range(df1_complete$I.feel.confident.at.work)
var(df1_complete$I.feel.confident.at.work)
quantile(df1_complete$I.feel.confident.at.work,na.rm = T,probs = c(0.25,0.75))
sd(df1_complete$I.feel.confident.at.work)
table(df1_complete$I.feel.confident.at.work)
hist(table(df1_complete$I.feel.confident.at.work))



mean(df1_complete$I.feel.effective.and.competent.in.my.work)
mode(df1_complete$I.feel.effective.and.competent.in.my.work)
median(df1_complete$I.feel.effective.and.competent.in.my.work)
range(df1_complete$I.feel.effective.and.competent.in.my.work)
var(df1_complete$I.feel.effective.and.competent.in.my.work)
quantile(df1_complete$I.feel.effective.and.competent.in.my.work,na.rm = T,probs = c(0.25,0.75))
sd(df1_complete$I.feel.effective.and.competent.in.my.work)
table(df1_complete$I.feel.effective.and.competent.in.my.work)
hist(table(df1_complete$I.feel.effective.and.competent.in.my.work))


mean(df1_complete$I.feel.that.my.work.is.recognized)
mode(df1_complete$I.feel.that.my.work.is.recognized)
median(df1_complete$I.feel.that.my.work.is.recognized)
range(df1_complete$I.feel.that.my.work.is.recognized)
var(df1_complete$I.feel.that.my.work.is.recognized)
quantile(df1_complete$I.feel.that.my.work.is.recognized,na.rm = T,probs = c(0.25,0.75))
sd(df1_complete$I.feel.that.my.work.is.recognized)
table(df1_complete$I.feel.that.my.work.is.recognized)
hist(table(df1_complete$I.feel.that.my.work.is.recognized))



Highest.level.of.education_I.feel.that.the.people.I.work.with.recognize.my.abilities = table(df1_complete$Highest.level.of.education, df1_complete$I.feel.that.the.people.I.work.with.recognize.my.abilities)
Highest.level.of.education_I.feel.that.the.people.I.work.with.recognize.my.abilities
                                
plot( Highest.level.of.education_I.feel.that.the.people.I.work.with.recognize.my.abilities)


plot( df1_complete$I.feel.that.my.work.is.recognized, df1_complete$I.feel.that.the.people.I.work.with.recognize.my.abilities)
abline(lm(df1_complete$I.feel.that.my.work.is.recognized~ df1_complete$I.feel.that.the.people.I.work.with.recognize.my.abilities), col="red")
a<- lm(df1_complete$I.feel.that.my.work.is.recognized~ df1_complete$I.feel.that.the.people.I.work.with.recognize.my.abilities)

summary(a)



chisq.test(df1_complete$Age,df1_complete$I.value.the.people.I.work.with )
work_experience <- factor(df1_complete$Working.experience, levels=c("2-5 years","< 2 years","5-10 years"), labels=c(0,1,2))
chisq.test(df1_complete$I.feel.confident.at.work,work_experience )

plot( df1_complete$I.feel.that.my.work.is.recognized, df1_complete$I.feel.that.the.people.I.work.with.recognize.my.abilities, "Salay in lakhs", "How much employee like the job")
plot( df1_complete$I.feel.that.my.work.is.recognized, df1_complete$I.feel.that.the.people.I.work.with.recognize.my.abilities)abline(lm(df1_complete$I.feel.that.my.work.is.recognized~ df1_complete$I.feel.that.the.people.I.work.with.recognize.my.abilities), col="red")

plot( df1_complete$I.feel.that.my.work.is.recognized, df1_complete$I.feel.that.the.people.I.work.with.recognize.my.abilities)
abline(lm(df1_complete$I.feel.that.my.work.is.recognized~ df1_complete$I.feel.that.the.people.I.work.with.recognize.my.abilities), col="red")

res <- cor.test(df1_complete$I.feel.that.my.work.is.recognized, df1_complete$I.feel.that.the.people.I.work.with.recognize.my.abilities, 
+                 method = "pearson")


chisq.test(df1_complete$I.feel.confident.at.work,work_experience )

monthly_income <- factor(df1_complete$Monthly.income, levels=c("< Rs.100,000","Rs 200,001 - Rs 500,000","Rs 500,000 – Rs 10,00000"), labels=c(0,1,2))
chisq.test(monthly_income,work_experience )

