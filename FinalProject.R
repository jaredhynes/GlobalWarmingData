library(dplyr)

improvedAverageSeaLevel <- cbind(ID=1:nrow(AverageSeaLevel), AverageSeaLevel)
# Hypothesis test using z-scores and p-values
# Null hypothesis - There is no change in average temperature between the 2000s and the 1950s. 
# Alternative Hypothesis - In the 2000s, the average temperature is higher.
# are greater than it was in the 1950s.

plot(historic_temp$Year, historic_temp$AverageCelsiusTemperature, type="l", lty=1, xlab="Year", ylab="Average Celsius Temperature")


data_2000s = filter(historic_temp, historic_temp$Year >= 2000)

mean_2000s = mean(data_2000s$MaxCelsiusTemp, na.rm = TRUE)
sd_2000s = sd(data_2000s$MaxCelsiusTemp, na.rm = TRUE)
times_2000s = length(data_2000s$X)

data_1950s = filter(historic_temp, (historic_temp$Year < 1960 & historic_temp$Year >= 1950))

mean_1950s = mean(data_1950s$MaxCelsiusTemp, na.rm = TRUE)
sd_1950s = sd(data_1950s$MaxCelsiusTemp, na.rm = TRUE)
times_1950s = length(data_1950s$X)

differenceInMeans = mean_2000s - mean_1950s
standardError = sqrt(((sd_2000s^2)/times_2000s) + ((sd_1950s^2)/times_1950s))

z_score = differenceInMeans/standardError
z_score

p_value = 1 - pnorm(z_score)
p_value
# p-value =  0

# Because my p-value is 0, this means that the value of pnorm(z_score) is so close to 1 that it is returning 0. 
# This goes to prove the point that global warming exists. There is almost a 0% chance of the max temperature from the 2000s data being the same as the 1950s data.

# Simple graph that clearly shows that the Average Sea Level has increased a lot. 
plot(AverageSeaLevel$Time, AverageSeaLevel$GMSL, type="l", lty=1, xlab="Year", ylab="Global Mean Sea Level")

dataSL_firstHalf = filter(improvedAverageSeaLevel, improvedAverageSeaLevel$ID <= 83)
dataSL_secondHalf = filter(improvedAverageSeaLevel, improvedAverageSeaLevel$ID > 150)

meanSL_firstHalf = mean(dataSL_firstHalf$GMSL, na.rm = TRUE)
meanSL_secondHalf = mean(dataSL_secondHalf$GMSL, na.rm = TRUE)

sdSL_firstHalf = sd(dataSL_firstHalf$GMSL, na.rm = TRUE)
sdSL_secondHalf = sd(dataSL_secondHalf$GMSL, na.rm = TRUE)

timesSL_firstHalf = length(dataSL_firstHalf$ID)
timesSL_secondHalf = length(dataSL_secondHalf$ID)

differenceInSLMeans = meanSL_secondHalf - meanSL_firstHalf
standardSLError = sqrt(((sdSL_secondHalf^2)/timesSL_secondHalf) + ((sdSL_firstHalf^2)/timesSL_firstHalf))
z_scoreSL = differenceInSLMeans/standardSLError
z_scoreSL  

p_valueSL = 1 - pnorm(z_scoreSL)
p_valueSL
# Because the p-value is 0, this means that the value of pnorm(z_scoreSL) is so close to 1 that it is returning 0. 
# This goes to prove that the average sea level has indeed increased. There is almost a 0% chance that the sea level in the early 2000s is the same as 
# the sea level in the 1990s. 

# Sea level increasing this drastically is because 