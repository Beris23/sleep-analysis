

data<-read.csv("c:\\Users\\GIMBIYA BENJAMIN\\Desktop\\Sleep_health_and_lifestyle_dataset.csv")
print(data)

data$Sleep.Disorder <- as.factor(data$Sleep.Disorder)
table(data$Sleep.Disorder)


lm1 <- lm(as.numeric(Sleep.Disorder) ~ Sleep.Duration, data = data)
summary(lm1)

lm2 <- lm(as.numeric(Sleep.Disorder) ~ Sleep.Duration + Age + BMI.Category + Stress.Level, data = data)
summary(lm2)

health_model<-glm(Sleep.Disorder ~ Sleep.Duration + Age + BMI.Category + Stress.Level, data = data, family = binomial )
print(health_model)
summary(health_model)


health_pred <- predict(health_model, type = "response")
health_class <- ifelse(health_pred > 0.5, "Disorder", "No_Disorder")
table(Predicted = health_class, Actual = data$Sleep.Disorder)

library(randomForest)


data$Sleep.Disorder <- as.factor(data$Sleep.Disorder)


rf_model <- randomForest(Sleep.Disorder ~ Sleep.Duration + Age + BMI.Category + Stress.Level, data = data, ntree = 100)


print(rf_model)
importance(rf_model)
varImpPlot(rf_model)

library(ggplot2)

ggplot(data, aes(x = Sleep.Disorder, y = Sleep.Duration, fill = Sleep.Disorder)) +
  geom_boxplot() +
  labs(title = "Sleep Duration by Sleep Disorder", x = "Sleep Disorder", y = "Sleep Duration (hours)")


ggplot(data, aes(x = as.factor(Stress.Level), fill = Sleep.Disorder)) +
  geom_bar(position = "fill") +
  labs(title = "Stress Level by Sleep Disorder", x = "Stress Level", y = "Proportion")







