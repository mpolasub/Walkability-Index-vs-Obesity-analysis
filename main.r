# import library
library(dplyr)
library(stringr)


# Data Preprocessing
walkability_df <- read.csv("EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv")
obesity_df <- read.csv("National_Obesity_By_State.csv")

# because our walkability_df was divided into block groups, we averaged the block groups' 
# walkability score by state to simplify the process, lowering the number of rows
# and creating new numerical variable (averaging the walkability index)
walkability_df$CBSA_Name <- str_sub(walkability_df$CBSA_Name, nchar(walkability_df$CBSA_Name) - 1, nchar(walkability_df$CBSA_Name))
walkability_by_state_df <- summarise(group_by(walkability_df, CBSA_Name), avg_index = mean(NatWalkInd, na.rm = TRUE))

# created new categorical variable with state abbreviations to prep for df joining
for (i in 1:nrow(obesity_df)) {
  obesity_df[i, "state_abv"] <- state.abb[match(obesity_df[i, "NAME"], state.name)] 
}

# (cleaning datasets) remove territories from CBSA_Name and RI from obesity_df 
# (remove row 36, 37, 39 from obesity_df)
obesity_df <- obesity_df[-c(36, 37, 39), ]
# (remove row 1 and 40 from walkability_by_state_df)
walkability_by_state_df <- walkability_by_state_df[-c(1, 40), ]

# left join both dataframes
final_df <- left_join(obesity_df, walkability_by_state_df, c("state_abv"="CBSA_Name"))

# exporting to CSV
#write.csv(final_df, "C:\\Users\\nondh\\OneDrive\\Documents\\0 UW\\INFO 201\\final proj\\obesity_walkability.csv", row.names=FALSE)
