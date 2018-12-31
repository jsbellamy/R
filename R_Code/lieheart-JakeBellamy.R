#1
df <- read.table("suspectheartrates.csv",header=TRUE,sep=",")
df

#2 (5 columns)
sapply(df, typeof)

#3
length(df$suspectid)

#4
summary(df$heartbpm)

#5
hist(df$heartbpm)
plot(df$heartbpm ~ df$suspectid)

#6 
mean(df$age)

#7
sum(df$sex == 'Female')
sum(df$sex == 'Male')


#8
sum(df$veracity == 'Truth')
sum(df$veracity == 'Lie')


#9
# Suspects with higher heartbeats are more likly to be lying
# Suspects with higher average age are more likely to have lower heartbeats

#10
summary(glm(veracity ~ heartbpm, family='binomial',data=df))

#11
cor(df$age, df$heartbpm)

#12
plot(df$veracity, df$heartbpm)
