library(dplyr) 
library(ggplot2) 
library(tidyr)
load("C:/Users/user/Downloads/pisa2018.Rdata")
summary(newdata)

##DATA PRE-PROCESSING

missing_values <- colSums(is.na(newdata))


print(missing_values[missing_values > 0])


data_imputed <- newdata

#Replace NA data with the mean of the column (only for numeric columns)
numeric_columns <- sapply(data_imputed, is.numeric)
data_imputed[, numeric_columns] <- lapply(data_imputed[, numeric_columns], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})


#The only columns left that include NAs are the non-numeric ones
print(colSums(is.na(data_imputed)))


# Data Exploration

data <- data_imputed

data_greece <- data  %>%
  filter(CNT == "Greece")

data_exclude_greece <- data %>%
  filter(CNT != "Greece")


summary_greece <- data_greece %>%
  summarize(
    Math_Mean = mean(MATH, na.rm = TRUE),
    Math_Median = median(MATH, na.rm = TRUE),
    Math_Q1 = quantile(MATH, probs = 0.25, na.rm = TRUE),
    Math_Q3 = quantile(MATH, probs = 0.75, na.rm = TRUE),
    Reading_Mean = mean(READ, na.rm = TRUE),
    Reading_Median = median(READ, na.rm = TRUE),
    Reading_Q1 = quantile(READ, probs = 0.25, na.rm = TRUE),
    Reading_Q3 = quantile(READ, probs = 0.75, na.rm = TRUE),
    Science_Mean = mean(SCIE, na.rm = TRUE),
    Science_Median = median(SCIE, na.rm = TRUE),
    Science_Q1 = quantile(SCIE, probs = 0.25, na.rm = TRUE),
    Science_Q3 = quantile(SCIE, probs = 0.75, na.rm = TRUE)
  )


summary_top_countries_exclude_greece <- data_exclude_greece %>%
  group_by(CNT) %>%
  summarize(
    Math_Mean = mean(MATH, na.rm = TRUE),
    Math_Median = median(MATH, na.rm = TRUE),
    Math_Q1 = quantile(MATH, probs = 0.25, na.rm = TRUE),
    Math_Q3 = quantile(MATH, probs = 0.75, na.rm = TRUE),
    Reading_Mean = mean(READ, na.rm = TRUE),
    Reading_Median = median(READ, na.rm = TRUE),
    Reading_Q1 = quantile(READ, probs = 0.25, na.rm = TRUE),
    Reading_Q3 = quantile(READ, probs = 0.75, na.rm = TRUE),
    Science_Mean = mean(SCIE, na.rm = TRUE),
    Science_Median = median(SCIE, na.rm = TRUE),
    Science_Q1 = quantile(SCIE, probs = 0.25, na.rm = TRUE),
    Science_Q3 = quantile(SCIE, probs = 0.75, na.rm = TRUE)
  )


print(summary_greece)
print(summary_top_countries_exclude_greece)


mean_scores_greece <- data_greece %>%
  summarize(
    Mean_Math = mean(MATH, na.rm = TRUE),
    Mean_Reading = mean(READ, na.rm = TRUE),
    Mean_Science = mean(SCIE, na.rm = TRUE),
    Overall_Mean = (Mean_Math + Mean_Reading + Mean_Science) / 3
  )


print(mean_scores_greece)

mean_scores_top_countries <- data_exclude_greece %>%
  summarize(
    Math_Mean = mean(MATH, na.rm = TRUE),
    Reading_Mean = mean(READ, na.rm = TRUE),
    Science_Mean = mean(SCIE, na.rm = TRUE)
  )

mean_scores_all <- data %>%
  summarize(
    Math_Mean = mean(MATH, na.rm = TRUE),
    Reading_Mean = mean(READ, na.rm = TRUE),
    Science_Mean = mean(SCIE, na.rm = TRUE)
  )


print(mean_scores_all)


mean_scores_by_country <- data %>%
  group_by(CNT, ST004D01T, .groups='DROP') %>%
  summarize(
    Mean_Math = mean(MATH, na.rm = TRUE),
    Mean_Reading = mean(READ, na.rm = TRUE),
    Mean_Science = mean(SCIE, na.rm = TRUE)
  ) %>%
  #Calculate overall mean score for each country
  mutate(Overall_Mean = (Mean_Math + Mean_Reading + Mean_Science) / 3) %>%
 
  arrange(desc(Overall_Mean))

##1. Comparison of Greece's scores vs best country

comparison_data <- data.frame(
  Subject = rep(c("Math", "Reading", "Science"), each = 2),
  Country = c("Greece", "Best Country", "Greece", "Best Country", "Greece", "Best Country"),
  Mean_Score = c(mean_scores_greece$Mean_Math, max(mean_scores_by_country$Mean_Math),
                 mean_scores_greece$Mean_Reading, max(mean_scores_by_country$Mean_Reading),
                 mean_scores_greece$Mean_Science, max(mean_scores_by_country$Mean_Science))
)

# Plotting
ggplot(comparison_data, aes(x = Subject, y = Mean_Score, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Comparison of Greece's Scores vs. Best Country",
       x = "Subject", y = "Mean Score") +
  scale_fill_manual(values = c("Greece" = "red", "Best Country" = "skyblue")) +
  theme_minimal()




#2. Ranking of Greece among the other countries regarding GLCM 


overall_scores <- data %>%
  group_by(CNT) %>%
  summarize(
    Overall_Score = mean(GLCM, na.rm = TRUE)
  )


overall_scores <- overall_scores %>%
  arrange(desc(Overall_Score))


overall_scores$Rank <- 1:nrow(overall_scores)


greece_rank <- overall_scores$Rank[overall_scores$CNT == "Greece"]





ggplot(overall_scores[1:20, ], aes(x = reorder(CNT, Rank), y = Rank, fill = CNT == "Greece")) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = ifelse(CNT == "Greece", as.character(Rank), "")), vjust = -0.5) +
  labs(title = "GLCM ranking - Top 20 Participating Countries",
       x = "Country", y = "Rank") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "lightgrey"),
                    guide = 'none') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#3. Gender gap - Mean scores by Subject & Gender 



data_country <- data[data$CNT != "Greece", ]
data_greece <- data[data$CNT == "Greece", ]


mean_scores_greece_gender <- data_greece %>%
  group_by(ST004D01T) %>%
  summarize(
    Math = mean(MATH, na.rm = TRUE),
    Reading = mean(READ, na.rm = TRUE),
    Science = mean(SCIE, na.rm = TRUE)
  )

#calculate mean scores by gender and subject for other countries
mean_scores_other_gender <- data_country %>%
  group_by(CNT, ST004D01T) %>%
  summarize(
    Math = mean(MATH, na.rm = TRUE),
    Reading = mean(READ, na.rm = TRUE),
    Science = mean(SCIE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ST004D01T) %>%
  summarize(
    Math = mean(Math),
    Reading = mean(Reading),
    Science = mean(Science),
    .groups = "drop"
  )

mean_scores_gender_combined <- rbind(
  cbind(mean_scores_greece_gender, Country = "Greece"),
  cbind(mean_scores_other_gender, Country = "Other Countries")
)
mean_scores_gender_combined_long <- pivot_longer(mean_scores_gender_combined, cols = -c(Country, ST004D01T), names_to = "Subject", values_to = "Mean_Score")
mean_scores_gender_combined_filtered <- mean_scores_gender_combined_long[!is.na(mean_scores_gender_combined_long$ST004D01T), ]

ggplot(mean_scores_gender_combined_filtered, aes(x = Subject, y = Mean_Score, fill = ST004D01T)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~Country) +
  labs(title = "Gender Gap Analysis: Mean Scores by Subject and Gender",
       x = "Subject", y = "Mean Score") +
  scale_fill_manual(values = c("Female" = "lightpink", "Male" = "lightblue")) +
  theme_minimal() +
  theme(legend.title = element_blank())



##5. Top countries by Average score 



all_countries <- data %>%
  group_by(CNT) %>%
  summarize(Math_Score = mean(MATH), na.rm = TRUE)

all_countries <- all_countries %>%
  arrange(desc(Math_Score))

# Add rank column to the data
all_countries <- all_countries %>%
  mutate(Rank = rank(-Math_Score))

  

#Get the top 15 countries by Math score
top_15_countries <- data %>%
  group_by(CNT) %>%
  summarize(Math_Score = mean(MATH), na.rm = TRUE) %>%
  top_n(15, Math_Score) 

#Add rank column to the data
top_15_countries <- top_15_countries %>%
  mutate(Rank = rank(-Math_Score))


# Filter for Greece
greece_info <- all_countries %>%
  filter(CNT == "Greece")

combined_data <- bind_rows(top_15_countries, greece_info)

combined_data <- combined_data %>%
  arrange(Rank)



ggplot(combined_data, aes(x = reorder(CNT, Rank), y = Math_Score, fill = CNT)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Rank), vjust = -0.5, size = 3) +  # Add text labels for rank
  labs(title = "Top 15 Countries by Math Score",
       x = "Country",
       y = "Math Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c(ifelse(top_15_countries$CNT == "Greece", "skyblue", "skyblue"), "skyblue"), guide = FALSE)

  
  

#calculate mean scores by country
mean_scores_by_country <- data %>%
  group_by(CNT, ST004D01T, .groups='DROP') %>%
  summarize(
    Mean_Math = mean(MATH, na.rm = TRUE),
    Mean_Reading = mean(READ, na.rm = TRUE),
    Mean_Science = mean(SCIE, na.rm = TRUE)
  ) %>%
  # Calculate overall mean score for each country
  mutate(Overall_Mean = (Mean_Math + Mean_Reading + Mean_Science) / 3) %>%
  # Arrange by overall mean score in descending order
  arrange(desc(Overall_Mean)) 



##7. Gender gap - top countries 

# Calculate the gender gap for each country
gender_gap_by_country <- mean_scores_by_country %>%
  group_by(CNT) %>%
  summarize(
    Gender_Gap_Math = Mean_Math[ST004D01T == "Female"] - Mean_Math[ST004D01T == "Male"],
    Gender_Gap_Reading = Mean_Reading[ST004D01T == "Female"] - Mean_Reading[ST004D01T == "Male"],
    Gender_Gap_Science = Mean_Science[ST004D01T == "Female"] - Mean_Science[ST004D01T == "Male"]
  ) %>%
  ungroup()

# Calculate the absolute gender gap for each country
gender_gap_by_country <- gender_gap_by_country %>%
  mutate(
    Absolute_Gender_Gap = abs(Gender_Gap_Math) + abs(Gender_Gap_Reading) + abs(Gender_Gap_Science)
  )

#Calculate the gender gap for each country
gender_gap_by_country <- mean_scores_by_country %>%
  group_by(CNT) %>%
  summarize(
    Gender_Gap_Math = mean(Mean_Math[ST004D01T == "Female"], na.rm = TRUE) - mean(Mean_Math[ST004D01T == "Male"], na.rm = TRUE),
    Gender_Gap_Reading = mean(Mean_Reading[ST004D01T == "Female"], na.rm = TRUE) - mean(Mean_Reading[ST004D01T == "Male"], na.rm = TRUE),
    Gender_Gap_Science = mean(Mean_Science[ST004D01T == "Female"], na.rm = TRUE) - mean(Mean_Science[ST004D01T == "Male"], na.rm = TRUE)
  ) %>%
  ungroup()

#Calculate the absolute gender gap for each country
gender_gap_by_country <- gender_gap_by_country %>%
  mutate(
    Absolute_Gender_Gap = abs(Gender_Gap_Math) + abs(Gender_Gap_Reading) + abs(Gender_Gap_Science)
  )

gender_gap_by_country

#Select the top 10 countries with the biggest gender gaps
top_10_gender_gap <- gender_gap_by_country %>%
  top_n(10, wt = Absolute_Gender_Gap)



library(waffle)

custom_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#000000")



library(forcats)

#Reorder the levels of the "CNT" factor variable based on the absolute gender gap
top_10_gender_gap <- top_10_gender_gap %>%
  mutate(CNT = fct_reorder(CNT, Absolute_Gender_Gap))



#Reorder levels of CNT variable in the dataframe
top_10_gender_gap$CNT <- factor(top_10_gender_gap$CNT, levels = rev(levels(top_10_gender_gap$CNT)))


ggplot(top_10_gender_gap, aes(fill = CNT)) +
  geom_waffle(aes(fill = CNT, values = Absolute_Gender_Gap)) +
  labs(title = "Top 10 Countries with the greatest Gender Gap",
       x = "Countries %",
       y = "Absolute Gender Gap",
       fill = "Country") +  # Change the legend title
  scale_fill_manual(values = custom_palette) +  # Use custom color palette
  scale_x_continuous(breaks = seq(0, 100, by = 25), labels = seq(0, 100, by = 25)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##8. Gender gap in Greece 


data_greece <- data %>% 
  filter(CNT == "Greece")

#calculate mean scores for male and female students in Greece
mean_scores_greece_gender <- data_greece %>%
  group_by(ST004D01T) %>%
  summarize(
    Math = mean(MATH, na.rm = TRUE),
    Reading = mean(READ, na.rm = TRUE),
    Science = mean(SCIE, na.rm = TRUE)
  )

mean_scores_greece_gender_long <- mean_scores_greece_gender %>%
  pivot_longer( cols = c(Math, Reading, Science), names_to = "Subject", values_to = "Mean_Score")




ggplot(mean_scores_greece_gender_long, aes(x = Subject, y = Mean_Score, fill = ST004D01T)) +
  geom_violin(trim = FALSE) +  #create violin plot without trimming
  geom_boxplot(width = 0.1, fill = "white", outlier.colour = NA) +  #add boxplot for visual reference
  labs(title = "Gender Gap Analysis in Greece: Mean Scores by Subject and Gender",
       x = "Subject", y = "Mean Score") +
  scale_fill_manual(values = c("Female" = "lightpink", "Male" = "lightblue")) +
  theme_minimal() +
  theme(legend.title = element_blank())








##9. Global gender gap - Mean score by subject & gender 


mean_scores_gender <- data %>%
  group_by(ST004D01T) %>%
  summarize(
    Math = mean(MATH, na.rm = TRUE),
    Reading = mean(READ, na.rm = TRUE),
    Science = mean(SCIE, na.rm = TRUE)
  )

mean_scores_gender_long <- mean_scores_gender %>%
  pivot_longer( cols = c(Math, Reading, Science), names_to = "Subject", values_to = "Mean_Score")

mean_scores_gender_filtered <- mean_scores_gender_long[!is.na(mean_scores_gender_long$ST004D01T), ]




gg <- ggplot(mean_scores_gender_filtered, aes(x = Subject, y = Mean_Score, fill = ST004D01T)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Global Gender Gap Analysis: Mean Scores by Subject and Gender",
      ) +
  scale_fill_manual(values = c("Female" = "lightpink", "Male" = "lightblue")) +
  theme_minimal() +
  theme(legend.title = element_blank())


gg_radial <- gg + coord_polar(theta = "y") +
  theme(
    axis.text.x = element_text(angle = 0, vjust = -0.5),
    axis.text.y = element_blank()
        
  )

gg_radial


# Convert data to wide format
mean_scores_gender_wide <- reshape2::dcast(mean_scores_gender_filtered, Subject ~ ST004D01T, value.var = "Mean_Score")

# Convert data to wide format
mean_scores_gender_wide <- reshape2::dcast(mean_scores_gender_filtered, Subject ~ ST004D01T, value.var = "Mean_Score")





##10. Overall student performance 

mean_scores_by_country <- data %>%
  group_by(CNT) %>%
  summarize(
    Mean_Math = mean(MATH, na.rm = TRUE),
    Mean_Reading = mean(READ, na.rm = TRUE),
    Mean_Science = mean(SCIE, na.rm = TRUE),
    .groups = "drop"
  )

#overall mean score for each country
mean_scores_by_country <- mean_scores_by_country %>%
  mutate(
    Overall_Mean = (Mean_Math + Mean_Reading + Mean_Science) / 3
  )

#select top 10 countries based on overall mean score
top_10_countries <- mean_scores_by_country %>%
  arrange(desc(Overall_Mean)) %>%
  head(10)


data_greece <- data.frame(
  CNT = "Greece",
  Math_Mean = mean_scores_greece$Mean_Math,
  Reading_Mean = mean_scores_greece$Mean_Reading,
  Science_Mean = mean_scores_greece$Mean_Science,
  Overall_Mean = mean_scores_greece$Overall_Mean
)

#add Greece to the top countries
top_10_with_greece <- bind_rows(top_10_countries, data_greece)



#add a column to indicate whether each row corresponds to Greece or not
top_10_with_greece$Is_Greece <- ifelse(top_10_with_greece$CNT == "Greece", TRUE, FALSE)


sorted_data <- mean_scores_by_country[order(mean_scores_by_country$Overall_Mean, decreasing = TRUE), ]
greece_rank <- which(sorted_data$CNT == "Greece")


#plot the top 10 countries and Greece based on overall mean score in descending order
ggplot(top_10_with_greece, aes(x = reorder(CNT, desc(Overall_Mean)), y = Overall_Mean, fill = Is_Greece)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Is_Greece, greece_rank, which(sorted_data$CNT == CNT))), vjust = -0.5, size = 3) +  # Add text label for Greece
  labs(title = "Overall Student Performance",
       x = "Country",
       y = "Overall Mean Score") +
  scale_fill_manual(values = c("skyblue", "red"), guide = 'none') +  # Use red color for Greece and skyblue for others
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##11.  Mean student scores by subject & gender - Qatar 



biggest_gender_gap_country <- gender_gap_by_country$CNT[which.max(gender_gap_by_country$Absolute_Gender_Gap)]
biggest_gender_gap <- max(gender_gap_by_country$Absolute_Gender_Gap)


smallest_gender_gap_country <- gender_gap_by_country$CNT[which.min(gender_gap_by_country$Absolute_Gender_Gap)]
smallest_gender_gap <- min(gender_gap_by_country$Absolute_Gender_Gap)

biggest_gender_gap_country
smallest_gender_gap_country


biggest_gap_country <- mean_scores_by_country %>%
  filter(CNT == biggest_gender_gap_country)

data_biggest_gap <- data  %>%
  filter(CNT == biggest_gender_gap_country)


mean_scores_biggest_gap <- data_biggest_gap %>%
  group_by(ST004D01T) %>%
  summarize(
    Mean_Math = mean(MATH, na.rm = TRUE),
    Mean_Reading = mean(READ, na.rm = TRUE),
    Mean_Science = mean(SCIE, na.rm = TRUE),
    Overall_Mean = (Mean_Math + Mean_Reading + Mean_Science) / 3
  )

print(mean_scores_biggest_gap)

mean_scores_plot <- mean_scores_biggest_gap %>%
  pivot_longer(cols = c(Mean_Math, Mean_Reading, Mean_Science),
               names_to = "Subject",
               values_to = "Mean_Score") %>%
  mutate(Subject = case_when(
    Subject == "Mean_Math" ~ "Math",
    Subject == "Mean_Reading" ~ "Reading",
    Subject == "Mean_Science" ~ "Science",
    TRUE ~ Subject  
  ))


ggplot(mean_scores_plot, aes(x = Subject, y = Mean_Score, fill = ST004D01T)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean student scores by Subject & gender in Qatar", x = "Subject", y = "Mean Score", fill = 'Gender') +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  theme_minimal()



mean_scores_plot



#12. Overall scores vs GLCM scores (NAI)

library(ggrepel)
library(gridExtra)


mean_scores_by_country <- data %>%
  group_by(CNT, .groups='DROP') %>%
  summarize(
    Mean_Math = mean(MATH, na.rm = TRUE),
    Mean_Reading = mean(READ, na.rm = TRUE),
    Mean_Science = mean(SCIE, na.rm = TRUE),
    Mean_GLCM = mean(GLCM, na.rm = TRUE)  
  ) %>%
  #calculate overall mean score for each country
  mutate(Overall_Mean = (Mean_Math + Mean_Reading + Mean_Science) / 3) %>%
  #arrange by overall mean score in descending order
  arrange(desc(Overall_Mean))

mean_scores_by_country <- mean_scores_by_country %>%
  mutate(Is_Greece = ifelse(CNT == "Greece", "Yes", "No"))



overall_scores_greece <- mean_scores_by_country %>%
  filter(CNT == "Greece") %>%
  summarize(
    Mean_Math = mean(Mean_Math),
    Mean_Reading = mean(Mean_Reading),
    Mean_Science = mean(Mean_Science),
    Overall_Mean = mean(Overall_Mean),
    Mean_GLCM = mean(Mean_GLCM)
  )


overall_plot <- ggplot(mean_scores_by_country, aes(x = Overall_Mean, y = Mean_GLCM, label = CNT)) +
  geom_point() +
  geom_text_repel(data = overall_scores_greece, aes(label = "Greece"), color = "red", vjust = 1.5, hjust = 1.5) +
  labs(title = "Relationship between overall scores and global competency scores",
       x = "Overall Mean Score",
       y = "Mean GLCM score") +
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue")  

overall_plot


### INTERACTIVE PLOTS ###

##1. Position of Greece among participating countries (Math score)


library(plotly)


plot_ly(combined_data, x = ~reorder(CNT, Rank), y = ~Math_Score, type = "scatter", mode = "markers+text", text = ~Rank,
        marker = list(color = "skyblue", size = 10), textfont = list(color = "black")) %>%
  
  
  layout(
    title = "Position of Greece Among Participating Countries",  
    xaxis = list(title = "Country"),  
    yaxis = list(title = "Math Score"), 
    hoverinfo = "text",  
    hovertext = ~paste("Country: ", CNT, "<br>Math Score: ", Math_Score, "<br>Rank: ", Rank), 
    showlegend = FALSE,  
    
    # Additional capabilities
    clickmode = "event+select",  
    selection = list(mode = "lasso"),  
    hoverlabel = list(bgcolor = "white", font = list(color = "black")), 
    updatemenus = list(  # Add interactive controls
      list(
        buttons = list(
          list(method = "relayout", args = list("title", "New Title"), label = "Change Title"),  
          list(method = "restyle", args = list("marker.color", "red"), label = "Highlight Data")  
        ),
        direction = "left",
        x = 0.1,
        y = 0.9
      )
    )
    
  )

##2. Gender gap




ggplotly(gg) %>%
  layout(
    title = "Global Gender Gap Analysis: Mean Scores by Subject and Gender",
    xaxis = list(title = "Subject"),
    yaxis = list(title = "Mean Score"),
    hoverlabel = list(bgcolor = "white", font = list(color = "black")),
    barmode = "group",  
    showlegend = TRUE,  
    legend = list(title = "Gender"),  
    annotations = list(
      x = 0.5, y = -0.15,  
      text = "Hover over bars for details",  
      showarrow = FALSE,  
      xref = "paper", yref = "paper",  
      xanchor = "center", yanchor = "auto",  
      font = list(size = 12, color = "gray")  
    ),
    plot_bgcolor = "rgb(240, 240, 240)",  
    paper_bgcolor = "rgb(255, 255, 255)"  
  )



comparison_plotly <- ggplotly(ggplot(comparison_data, aes(x = Subject, y = Mean_Score, fill = Country)) +
                                geom_bar(stat = "identity", position = "dodge", width = 0.7) +
                                labs(title = "Comparison of Greece's Scores vs. Best Country",
                                     x = "Subject", y = "Mean Score") +
                                scale_fill_manual(values = c("Greece" = "red", "Best Country" = "skyblue")) +
                                theme_minimal()) %>%
  layout(
    hoverlabel = list(bgcolor = "white", font = list(color = "black")), 
    legend = list(title = list(text = "Country")),  
    title = "Comparison of Greece's Scores vs. Best Country", 
    xaxis = list(title = "Subject"),  
    yaxis = list(title = "Mean Score"), 
    hovermode = "closest",  
    dragmode = "zoom", 
    showlegend = TRUE,  
    annotations = list(  
      text = "Hover over bars for details",  
      x = 0.5, y = -0.15,  
      xref = "paper", yref = "paper",  
      showarrow = FALSE,  
      font = list(size = 12, color = "gray")  
    ),
    updatemenus = list(  
      list(
        buttons = list(
          list(args = list("visible", c(FALSE, TRUE)), label = "Show Greece"),  
          list(args = list("visible", c(TRUE, FALSE)), label = "Show Best Country"),  
          list(args = list("visible", c(TRUE, TRUE)), label = "Show Both")  
        ),
        direction = "down",  
        showactive = TRUE,  
        type = "buttons",  
        x = 0.95,  
        xanchor = "left",  
        y = 0.5,  
        yanchor = "top"  
      )
    )
  )

comparison_plotly



