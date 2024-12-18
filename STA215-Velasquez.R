#set working Directory 
setwd("h:/sta215")

# load fifth grader data 
raw_data<- read.csv("raw_data.csv")

################################################################################
########################### Descriptive statistics #############################
################################################################################

#mean 
mean(raw_data$number_characters)
mean(raw_data$dexter_inner_thoughts)

#standard deviation
sd(raw_data$number_characters)
sd(raw_data$dexter_inner_thoughts)


#summary 
summary(raw_data$number_characters)
summary(raw_data$dexter_inner_thoughts)

################################################################################
############################ Figure 2: Scatter Plot ############################
################################################################################

linear_plot <- plot(raw_data$number_characters, raw_data$dexter_inner_thoughts)
plot(raw_data$number_characters, raw_data$dexter_inner_thoughts, main = "Figure 1: Scatter Plot Showing the Relationship Between Number of Characters and Dexters' Inner Thoughts", xlab = "Number of Characters" , ylab = "Dexters' Inner Thoughts")
print(linear_plot)

# add x line and y line for means
meany <- mean(raw_data$dexter_inner_thoughts)
meanx <- mean(raw_data$number_characters)
abline(v = meanx, col = "black")
abline(h = meany, col = "black")
linear_relationship <- lm(dexter_inner_thoughts ~ number_characters, data = raw_data)
summary(linear_relationship)
abline(linear_relationship)

################################################################################
########################## Figure 3: Residual Plot #############################
################################################################################

plot(raw_data$number_characters, residuals(linear_relationship))
plot(raw_data$number_characters, raw_data$dexter_inner_thoughts, main = "Figure 2: Scatter Plot of Number of Characters and Residuals from Regression Analysis", xlab = "Number of Characters" , ylab = "Dexters Inner Thoughts")
abline(h = 0, col = "black")