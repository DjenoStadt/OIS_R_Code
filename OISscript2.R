# Creating a dataframe for the time spent in VR and RL settings
Participanten <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
                   18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
                   33, 34, 35, 36)

TijdVR <- c(106, 118, 202, 84, 141, 147, 'x', 179, 116, 142, 135, 135, 182
            ,105, 131, 120, 97, 123, 136, 121, 224, 90, 190, 113, 148, 155, 153
            ,163, 182, 'x', 106, 135, 107, 154, 167, 135)

TijdRL <- c(63, 180, 140, 73, 110, 148, 'x', 118, 115, 113, 102, 102, 110
            ,107, 117, 114, 104, 98, 115, 73, 160, 102, 122, 92, 101, 160, 187
            ,140, 105, 'x', 152, 113, 126, 93, 100, 146)

df <- data.frame(Participanten, TijdVR, TijdRL) 

# 'x' Values in the dataframe could not be used and were replaced with NA
df$TijdVR[df$TijdVR == 'x'] <- NA
df$TijdRL[df$TijdRL == 'x'] <- NA
# Removing NA from the dataframe
df <- na.omit(df)
# The numbers in the dataframe were still seen as non-numeric values, so we converted them
# to numeric
df$TijdVR <- as.numeric(df$TijdVR)
df$TijdRL <- as.numeric(df$TijdRL)
# Now we were able to do a paired samples t-test
paired_t_test <- t.test(df$TijdVR, df$TijdRL, paired = TRUE, conf.level = 0.90)
print(paired_t_test)

# Then we calculated the effect size for the paired samples t-test
# Calculate the mean difference
mean_difference <- mean(df$TijdVR - df$TijdRL)

# Calculate the standard deviation of the differences
sd_difference <- sd(df$TijdVR - df$TijdRL)

# Calculate Cohen's d
cohens_d <- mean_difference / sd_difference

# Print Cohen's d
print(cohens_d)

# Generating descriptive statistics for the columns
# For mean TijdRL 
mean(df$TijdRL)
# TijdRL standarddeviation
sd(df$TijdRL)
# Inter Quartile Range TijdRL
IQR(df$TijdRL)
# Range of TijdRL
range(df$TijdRL)
# Median of TijdRL
median(df$TijdRL)

# For TijdVR mean
mean(df$TijdVR)
# TijdVR standarddeviation
sd(df$TijdVR)
# Inter Quartile range TijdVR
IQR(df$TijdVR)
# Range of TijdVR
range(df$TijdVR)
# Median of TijdVR
median(df$TijdVR)

# Loading in ggplot2 to make a scatterplot of TimeVR and TimeRL
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

ggplot(df, aes(x = Participanten)) +
  geom_point(aes(y = as.numeric(TijdVR), color = "VR"), shape = 1, size = 1) +  # Puntjes (VR)
  geom_point(aes(y = as.numeric(TijdRL), color = "RL"), shape = 15, size = 1) +  # Vierkantjes (RL)
  geom_hline(yintercept = 180, linetype = "dashed", color = "black") +  # Stippellijn op 180 seconden
  geom_line(aes(y = mean(as.numeric(TijdVR), na.rm = TRUE)), color = "blue",
            linetype = "solid", size = 0.5) +  # Gemiddelde lijn voor TijdVR
  geom_line(aes(y = mean(as.numeric(TijdRL), na.rm = TRUE)), color = "red", 
            linetype = "solid", size = 0.5) +  # Gemiddelde lijn voor TijdRL
  annotate("text", x = max(df$Participanten), y = 180, label = "3 minutes", 
           hjust = 1, vjust = -1, size = 3) +  # Label for the 3-minute mark
  labs(title = "",
       x = "Participants",
       y = "Time (s)") +
  scale_color_manual(values = c("red", "blue"), labels = c("Mean RL", "Mean VR")) +
  theme_minimal()

# Then we created a descriptive table encompassing all the descriptives 
# that we have employed in the results
descriptives <- data.frame(
  Setting = c("Real Life", "Virtual Reality"),
  Min = c(63, 84),
  Max = c(187, 224),
  IQR = c(34.5, 38.25),
  SD = c(28.54, 32.82),
  Mean = c(117.68, 139.47)
)

# Then we inputted the data from the first survey

# Participant number
Participanten <- c(1, 2, 3, 5, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 15, 17, 18, 19, 20, 21, 22, 23, 24, 26, 25, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37)

# May we collect and use your data anonymously for the research?
Mogen <- c("Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")

# Gender
Geslacht <- c("Man", "Man", "Man", "Man", "Man", "Man", "Man", "Vrouw", "Man", "Man", "Man", "Man", "Man", "Vrouw", "Man", "Man", "Vrouw", "Man", "Man", "Vrouw", "Man", "Man", "Man", "Man", "Man", "Vrouw", "Vrouw", "Man", "Man", "Man", "Man", "Man", "Man", "Vrouw", "Vrouw", "Man", "Man")

# What id your field of study?
Studie <- c("Informatiekunde", "Informatica", "Informatica", "Informatiekunde", "Informatiekunde", "Informatiekunde", "Informatiekunde", "Kunstmatige Intelligentie", "Informatiekunde", "Kunstmatige Intelligentie", "Kunstmatige Intelligentie", "Human resource studies", "Kunstmatige Intelligentie", "Interdisciplinaire sociale wetenschap", "Kunstmatige Intelligentie", "Kunstmatige Intelligentie", "Kunstmatige Intelligentie", "Kunstmatige Intelligentie", "Kunstmatige Intelligentie", "Theaterwetenschap", "Business Analytics", "Business Analytics", "Informatica", "Informatica", "Future planet studies", "Future planet studies", "Informatica", "Rechtsgeleerdheid", "Human resource studies", "Physics", "Natuurkunde", "Informatiekunde", "Informatiekunde", "Informatica", "Informatica", "Kunstmatige Intelligentie", "Kunstmatige Intelligentie")

# What is your age?
Leeftijd <- c(21, 19, 21, 19, 20, 20, 22, 22, 22, 26, 20, 22, 23, 19, 20, 20, 23, 21, 22, 22, 18, 19, 20, 22, 23, 19, 19, 23, 23, 24, 23, 23, 19, 21, 20, 18, 19)

# Do you have prior pingpong experience?
PingpongErvaring <- c("Ja", "Nee", "Nee", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "NA", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", "Nee", "Ja", "Nee", "Nee", "Ja", "Ja", "Ja", "Ja", "Ja", "Nee", "Ja", "Nee", "Ja", "Ja")

# Replace "Ja" with "Yes"
PingpongErvaring <- gsub("Ja", "Yes", PingpongErvaring)

# Replace "Nee" with "No"
PingpongErvaring <- gsub("Nee", "No", PingpongErvaring)

# If you answered 'Yes' to the previous question, on a scale from 1 to 7, how experienced are you in ping-pong?
PingpongErvaringSchaal <- c(2, 3, NA, 2, 5, 2, 5, 2, 5, 7, 4, 5, 4, 2, 3, 4, 1, 4, 4, 3, 6, 4, 4, NA, 3, NA, NA, 3, 6, 5, 5, 5, NA, 2, NA, 3, 4)

# Do you have prior experience with VR environments?
VRErvaring <- c("Ja", "Nee", "Nee", "Ja", "Nee", "Nee", "Nee", "Ja", "Ja", "Ja", "Ja", "Nee", "Ja", "Ja", "Ja", "Ja", "Nee", "Ja", "Ja", "Ja", "Ja", "Nee", "Ja", "Ja", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Ja", "Nee", "Ja", "Nee", "Ja", "Nee")

# Replace "Ja" with "Yes"
VRErvaring <- gsub("Ja", "Yes", VRErvaring)

# Replace "Nee" with "No"
VRErvaring <- gsub("Nee", "No", VRErvaring)

# If you answered 'Yes' to the previous question, on a scale from 1 to 7, how experienced are you with VR?
VRErvaringSchaal <- c(4, NA, NA, 6, NA, NA, NA, 2, 2, 3, 2, 1, 2, 2, 2, 4, NA, 5, 2, 3, 4, 1, 5, 3, NA, NA, NA, NA, NA, NA, NA, 4, NA, 4, NA, 5, 1)

# Do you have experience with ping-pong in a VR environment?
PingpongVRErvaring <- c("Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee", "Nee")

# Replace "Ja" with "Yes"
PingpongVRErvaring <- gsub("Ja", "Yes", PingpongVRErvaring)

# Replace "Nee" with "No"
PingpongVRErvaring <- gsub("Nee", "No", PingpongVRErvaring)

# Then we created a dataframe of the first survey
dfEnquete1 <- data.frame(
  Participanten, Mogen, Geslacht, Studie, Leeftijd,
  PingpongErvaring, PingpongErvaringSchaal,
  VRErvaring, VRErvaringSchaal,
  PingpongVRErvaring
)

# Then we merged dfEnquete1 with the time dataframe, based on participant number
Merged <- merge(df,dfEnquete1,by="Participanten")

# Generating descriptives of participants
# Calculate mean age of participants
mean(Merged$Leeftijd)

# Calculate standard deviation of participants
sd(Merged$Leeftijd)

# Count genders of the participants
table(Merged$Geslacht)

# Create a QQ plot for TijdVR
qqplot_TijdVR <- ggplot(Merged, aes(sample = TijdVR)) +
  geom_qq() +
  geom_qq_line(color = "blue") +
  labs(title = "QQ Plot for TijdVR", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Create a QQ plot for TijdRL
qqplot_TijdRL <- ggplot(Merged, aes(sample = TijdRL)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(title = "QQ Plot for TijdRL", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Display the plots
print(qqplot_TijdVR)
print(qqplot_TijdRL)

# Then we inputted the results of the second survey on the top 3 emotions experienced during both settings
# Which top 3 emotions mentioned above have you experienced while playing ping pong in a real-life setting?
emotionsRL <- c(
  "fascination, boredom, sadness", 
  "satisfaction, joy, fascination", 
  "joy, desire, satisfaction", 
  "contempt, boredom", 
  "joy, sporty, satisfaction", 
  "pride, joy, boredom", 
  "shame, joy, satisfaction", 
  "admiration, joy, satisfaction", 
  "shame, joy, hope", 
  "joy, satisfaction, hope", 
  "joy, boredom, satisfaction", 
  "pride, joy, angry", 
  "joy, satisfaction, pride", 
  "joy, pride, satisfaction", 
  "admiration, hope, satisfaction", 
  "satisfaction, pride, joy", 
  "hope, fascination, pride", 
  "joy, desire, satisfaction", 
  "joy, satisfaction, hope", 
  "joy, pride, satisfaction", 
  "joy, satisfaction, hope", 
  "joy, satisfaction, desire", 
  "satisfaction, fascination, desire", 
  "joy, angry", 
  "hope, joy, satisfaction", 
  "joy, desire, pride", 
  "joy, satisfaction, pride", 
  "joy, anger, shame", 
  "pride, joy, satisfaction", 
  "pride, joy, boredom", 
  "joy, pride, satisfaction", 
  "joy, satisfaction, pride", 
  "pride, joy, satisfaction", 
  "joy, satisfaction, pride", 
  "joy, hope, satisfaction"
)
# Splitting each string into individual emotions
split_emotionsRL <- strsplit(emotionsRL, ", ")

# Unlisting and trimming whitespace
all_emotionsRL <- trimws(unlist(split_emotionsRL))

# Tabulating frequencies
emotion_countsRL <- table(all_emotionsRL)

# Displaying the counts
print(emotion_countsRL)

# Which top 3 emotions mentioned above have you experienced while playing ping pong in the VR setting?
emotionsVR <- c("fascination, joy, admiration", 
"shame, fascination, anger", 
"satisfaction, fascination, admiration", 
"joy, satisfaction, fascination", 
"pride, joy, fascination", 
"shame, fascination, boredom", 
"joy, fascination, satisfaction", 
"admiration, fascination, joy", 
"joy, satisfaction, hope", 
"fascination, admiration, joy", 
"anger, shame, fear", 
"shame, joy, fascination", 
"fascination, satisfaction, admiration", 
"joy, fascination, admiration", 
"fascination, hope, boredom", 
"fascination, joy, pride", 
"fascination, shame, desire", 
"shame, boredom, joy", 
"desire, fascination, anger", 
"joy, fascination, admiration", 
"joy, anger, satisfaction", 
"contempt, joy, satisfaction",
"joy, satisfaction, admiration", 
"anger, joy, fascination", 
"joy, satisfaction, fascination", 
"fascination, joy, admiration", 
"fascination, joy, hope", 
"boredom, shame, contempt", 
"boredom, shame, satisfaction", 
"boredom, hope, fascination", 
"joy, fascination, satisfaction", 
"boredom, admiration, desire", 
"joy, fascination, desire"
)

# Splitting each string into individual emotions
split_emotionsVR <- strsplit(emotionsVR, ", ")

# Unlisting and trimming whitespace
all_emotionsVR <- trimws(unlist(split_emotionsVR))

# Tabulating frequencies
emotion_countsVR <- table(all_emotionsVR)

# Displaying the counts
print(emotion_countsVR)

# Convert emotion counts to data frames
df_rl <- as.data.frame(emotion_countsRL)
df_vr <- as.data.frame(emotion_countsVR)

# Ensure the column names are the same for both data frames
names(df_rl) <- c("emotion", "count")
names(df_vr) <- c("emotion", "count")

# Add a 'setting' column
df_rl$setting <- 'Real Life'
df_vr$setting <- 'Virtual Reality'

# Combine the data frames
combined_df <- rbind(df_rl, df_vr)

# Then we created a bar chart of the frequency of the top 3 emotions for both settings
ggplot(combined_df, aes(fill = setting, y = count, x = emotion)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), # Adjust the dodge width if necessary
            vjust = -0.3, # Adjust vertical position to sit above the bars
            size = 3) + # Adjust text size as needed
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "", x = "Emotion", y = "Frequency")
