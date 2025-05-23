# -------------- The Golden Ratio and Facial Beauty -------------- 

# Golden ratio
golden_ratio <- 1.618

#### Classical Approach

# Length-to-width ratios (w/h) of 25 female faces in a tribe in Africa
women_length_width <- c(1.443, 1.335, 1.529, 1.617, 1.493, 1.511, 1.488, 1.626, 
                        1.650, 1.619, 1.449, 1.592, 1.497, 1.618, 1.637, 1.650, 
                        1.642, 1.664, 1.808, 1.754, 1.185, 1.736, 1.072, 1.615, 
                        1.620)

sample_size <- length(women_length_width)


## 1.- Observing the distribution
par(mfrow = c(1, 1))

# Plot histogram of the data
hist(women_length_width, main = "Histogram of Length-to-Width ratios",
     xlab = "Length-to-Width ratios", xlim=c(1.0,1.9),
     xaxt='n') # disables the default x-axis labels

# Set limits in x axis how i need them
axis(1, at=seq(1.0, 1.9, by=0.1), labels=seq(1.0, 1.9, by=0.1))
abline(v = golden_ratio, lty = 2, col = "red")
?seq

# Boxplot
boxplot(women_length_width, main = "Boxplot of Length-to-Width ratios",
        ylab = "Length-to-Width ratios", horizontal=TRUE, col="lightblue1")
abline(v=median(women_length_width), lty=2, col="red")

# Sample median
median(women_length_width) # 1.617


# QQ plot
qqnorm(women_length_width, main = "Q-Q plot of Length-to-Width ratios")
qqline(women_length_width, col = "red")

## 2.- Computing the CI

# Sample mean
sampmean_ratio <- mean(women_length_width)
sampmean_ratio # 1.554

# Sample variance
sampvar_ratio <- var(women_length_width)   
sampvar_ratio # 0.02764133

# Sample standard deviation
sd_ratio <- sd(women_length_width)
sd_ratio

# Confidence level
confidence_level <- 0.96
significance_level <- 1 - confidence_level

# Critical value using a t distribution
critical_value <- qt(significance_level / 2, sample_size - 1, lower.tail = FALSE)
critical_value

# Standard error
standard_error <- sqrt(sampvar_ratio / sample_size)
standard_error

# Margin of error
MOE <- round(critical_value * standard_error,6)
MOE

# Confidence interval limits
lower_limit <- sampmean_ratio - MOE
upper_limit <- sampmean_ratio + MOE
sprintf("Confidence Interval: [%f, %f]", lower_limit, upper_limit)

# With 96% confidence, we cannot conclude that the mean ratio of this tribe's facial 
# measurements differs significantly from the golden ratio, since the golden ratio 
# (1.618) is contained into the CI.

# This suggests that the tribe's facial geometry may align with the golden ratio,
# despite their distinct cultural standards for beauty



#### CI using the bootstrap method 
B<-10^5

set.seed(1645) # Reproductibility of data
mean_ratio_boot<-c()
for (i in 1:B){
  # Creates 10^5 new samples of size 25
  resample<-sample(x=women_length_width, size=sample_size, replace=TRUE)
  mean_ratio_boot[i]<-mean(resample)
}

# Mean of the sample length to width ratios

# Bootstrap sample mean
sampmean_ratio_boot<-mean(mean_ratio_boot) 
sampmean_ratio_boot # 1.553873

# Bootstrap sample variance
sampvar_ratio_boot<-var(mean_ratio_boot)
sampvar_ratio_boot # 0.001058887

# Bootstrap sample standard deviation
sd_ratio_boot<-sd(mean_ratio_boot)
sd_ratio_boot # 0.03254054


## 1.- Observing the distribution
par(mfrow=c(1,1))
hist(x=mean_ratio_boot, main="Bootstrap distribution of Length-to-Width ratios 
     of female faces",xlab="Length-to-Width ratios", ylim=c(0,30000))
abline(v=golden_ratio, lty = 2, col = "red")

# Add label below the golden ratio line
axis(1, at=golden_ratio, labels=sprintf("   %.3f", golden_ratio), col.axis="red")


# The distribution does resemble a normal distribution, although, there is a
# "a bit of" asymmetry to the left


# Computing the CI for the mean
sample_size_boot<-length(mean_ratio_boot)

# Comparing the CI using a normal and a t distribution

# 1) Using t distribution
critical_valueT_boot<-qt(significance_level/2,sample_size_boot-1, lower.tail=FALSE)
critical_valueT_boot #2.053776

standard_errorT_boot<-sqrt(sampvar_ratio_boot/sample_size_boot)
standard_errorT_boot # 0.0001029022

MOET_boot<-critical_valueT_boot*standard_errorT_boot
MOET_boot # 0.0002113381

lower_limitT_boot<-sampmean_ratio_boot - MOET_boot
upper_limitT_boot<-sampmean_ratio_boot + MOET_boot
sprintf("Confidence interval: [%f,%f]", lower_limitT_boot, upper_limitT_boot)
# Confidence interval: [1.553661,1.554084]


# 2) Using normal distribution
critical_valueZ_boot<-qnorm(significance_level/2, lower.tail = FALSE)
critical_valueZ_boot # 2.053749

standard_errorZ_boot<-sqrt(sampvar_ratio_boot/sample_size_boot)
standard_errorZ_boot # 0.0001029022

MOEZ_boot<-critical_valueZ_boot*standard_errorZ_boot
MOEZ_boot  # 0.0002113353

lower_limitZ_boot<-sampmean_ratio_boot - MOEZ_boot
upper_limitZ_boot<-sampmean_ratio_boot + MOEZ_boot
sprintf("Confidence interval: [%f ,%f]", lower_limitZ_boot, upper_limitZ_boot)
# Confidence interval: [1.553661 ,1.554084]

