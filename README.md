## Developing a Demographic Multi-Dimensional Contingency Table Using PUMS Data and `pivot_wider`  

In this exercise, we'll construct a demographic multi-dimensional contingency table, a key component of a contract I managed that involved developing regional household distribution tables for Metropolitan Planning Organizations (MPOs) in Texas. This table aggregates critical demographic indicators—including income groups, household sizes (ranging from 1-person to 5+ person households), and the number of workers per household (from 0 to 2+)—using Public Use Microdata Sample (PUMS) data for Bexar County, TX. The resulting table serves as input for travel demand models.  

**Note:** The income categories used in this example are for demonstration purposes only. In practice, these categories are often derived from household surveys provided by the governing agency. Additionally, adjustments based on the Consumer Price Index for All Urban Consumers (CPI-U) are sometimes necessary, but such adjustments have not been applied in this example.

## Set working directory
```r
setwd()
```

## Verify working directory
```r
getwd()
```

## Load library
```r
library(pacman)
```

## Install & load packages
```r
p_load(
  car,
  dplyr,
  ggplot2,
  matrixStats,
  purrr,
  questionr,
  sf,
  scales,
  srvyr,
  survey,
  stringr,
  tidycensus,
  tidyverse,
  tidyr,
  tigris
)
```

## Load census API key
```r
census_api_key(Sys.getenv("CENSUS_API_KEY"))
```

## Load PUMS variables for 2023
```r
pums_vars_2023 <- pums_variables %>%
  filter(year == 2023, survey == "acs5")
```

## Display unique variables
```r
pums_vars_2023 %>%
  distinct(var_code, var_label, data_type, level)
```

## Display person-level variables
```r
pums_vars_2023 %>%
  distinct(var_code, var_label, data_type, level) %>%
  filter(level == "person")
```

## Download PUMS data from the Census API
```r
# Download PUMS data from the Census API
tx_pums <- get_pums(
  variables = c("PUMA", "SEX", "AGEP", "RELSHIPP", "NP", "ESR", "SERIALNO", "PWGTP", "STATE", "WAGP", "ADJINC", "HINCP", "WGTP"),
  state = "TX",
  survey = "acs5",
  year = 2023
)

```

## Filter PUMS data to include specific PUMA codes
```r
pumas <- tx_pums %>%
  filter(PUMA %in% c("05901", "05902", "05903", "05904", "05905",
                     "05906", "05907", "05908", "05909", "05910",
                     "05911", "05912", "05913", "05914", "05915",
                     "05916", "05917", "05918", "05919", "05920"))
```

## Generate county variable for Bexar County
```r
pumas$county <- Recode(
  pumas$PUMA,
  recodes = " '05901'='Bexar'; '05902'='Bexar'; '05903'='Bexar'; '05904'='Bexar';
              '05905'='Bexar'; '05906'='Bexar'; '05907'='Bexar'; '05908'='Bexar';
              '05909'='Bexar'; '05910'='Bexar'; '05911'='Bexar'; '05912'='Bexar';
              '05913'='Bexar'; '05914'='Bexar'; '05915'='Bexar'; '05916'='Bexar';
              '05917'='Bexar'; '05918'='Bexar'; '05919'='Bexar'; '05920'='Bexar';
              else = NA"
)
```

## Convert SERIALNO into string
```r
pumas$SERIALNO <- as.character(pumas$SERIALNO)

str(pumas)
```

## Replace NAs
```r
pumas[is.na(pumas)] <- 0
```

## Remove negative
```r
pumas$HINCP[pumas$HINCP < 0] <- 0
```

## Convert `ADJINC` to Numeric 
```r
pumas$ADJINC <- as.numeric(as.character(pumas$ADJINC))
```

## Standardize household income for the base year (BY) using the ADJINC adjustment factor of 1019518 per https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2019-2023.pdf
```r
pumas$adj_inc <- pumas$HINCP * (1019518 / 1000000)
```

## Compare summary of old and new variable
```r
with(pumas, summary(HINCP))

with(pumas, summary(adj_inc)
```

## Generate grouped a grouped income variable (the income groups should reflect the household survey from the transportation planning team)
```r
pumas$incgrp <-
  Recode(
    pumas$adj_inc,
    recodes = "0:22841='$0 - $22,841'; 22842:44963='$22,842 - $44,963'; 44964:67446='$44,964 - $67,446'; 67447:112410='$67,447 - $112,410'; 112411:1471776.2='$112,411+'; else = NA",
    as.factor = TRUE,
    levels = c(
      "$0 - $22,841",
      "$22,842 - $44,963",
      "$44,964 - $67,446",
      "$67,447 - $112,410",
      "$112,411+"
    )
  )
```

## Generate grouped household size variable
```r
pumas$hhsize <- Recode(pumas$NP, recodes = "1=1; 2=2; 3=3; 4=4; 5:20='5+'; else=NA", as.factor = T)
```

## Create flag variable for workers in the household
```r
pumas$worker <- factor(ifelse(pumas$ESR == 1, "1", ifelse(pumas$ESR == 2, "1", NA)))
```

## Create a flag to identify workers in the household
```r
pumas %>%
  group_by(SERIALNO) %>%
  mutate(wihh = sum(worker == 1, na.rm = TRUE)) -> pumas
```

## Create flag to identify workers in the household
```r
pumas <- pumas %>%
  group_by(SERIALNO) %>%
  mutate(wihh = sum(worker == 1, na.rm = TRUE))
```

## Generate grouped number of workers in household variable
```r
pumas$hhworker <- factor(
  ifelse(pumas$wihh == 0, "0", 
         ifelse(pumas$wihh == 1, "1", 
                ifelse(pumas$wihh >= 2, "2+", NA_character_)))
)
```

## Generate variable to distinguish different types of households
```r
pumas$hhtype <- factor(ifelse(pumas$WGTP != 0, "1", ifelse(pumas$WGTP != 2, "2", NA)))
```

## Generate flag variable identifying householders
```r
pumas$hholder <- Recode(pumas$RELSHIPP, recodes = "20=1; else = 0", as.factor = T)
```

## View updated dataset with weights applied
```r
head(pumas)
```

## Aggregate household data if householder is present
```r
hhinc_all <-
  pumas %>%
  filter(hholder == 1) %>%
  group_by(county, incgrp, hhsize, hhworker) %>%
  dplyr::count(hhworker, wt = WGTP, na.rm = TRUE)
```

## Apply pivot_wider with correct values_fill to ceeate a multi-dimensional contingency table
```r
multid_incdist <- pivot_wider(
  hhinc_all,
  id_cols = c(county, incgrp),
  values_fill = 0,
  names_from = c(hhworker, hhsize),
  values_from = n
)
```

##  Add a Row Total, making sure the data is not grouped
```r
multid_incdist <- multid_incdist %>%
  ungroup() %>%
  # For each row, sum all numeric columns
  mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE))
```

## Summarize each numeric column (summing over all rows)
```r
total_row <- multid_incdist %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  # Label this row for your ID columns
  mutate(county = "", incgrp = "Total") %>%
  # Reorder columns so county and incgrp appear first
  select(county, incgrp, everything())
```

## Append the "Total" row to the bottom of your data frame
```r
multid_incdist <- bind_rows(multid_incdist, total_row)
````

## Identify the grand total from the final row's "Total" column
grand_total <- multid_incdist %>%
  filter(incgrp == "Total") %>%
  pull(Total)

## Convert each numeric column to its fraction of the grand total
multid_incdist_fraction <- multid_incdist %>%
  mutate(across(where(is.numeric), ~ .x / grand_total))

## Outputs

## Side-by-Side via left_join() with Suffixes for table output
```r
combined_table <- left_join(
  multid_incdist,
  multid_incdist_fraction,
  by = c("county", "incgrp"),
  suffix = c("_abs", "_fraction")
)
```

## OR
## Rename numeric columns in the fraction table to add a "_fraction" suffix
```r
multid_incdist_fraction2 <- multid_incdist_fraction %>%
  rename_with(~ paste0(.x, "_fraction"), where(is.numeric))

## Bind the columns side by side
combined_table <- bind_cols(
  multid_incdist,
  multid_incdist_fraction2 %>% select(ends_with("_fraction"))
)
```

## Save the processed data to a CSV file
```r
write.csv(combined_table, "...")
```

## This is the output in absolute value format

| county | incgrp              | 0_1   | 1_1   | 0_2   | 1_2   | 2+_2  | 0_3   | 1_3   | 2+_3  | 0_4   | 1_4   | 2+_4  | 0_5+  | 1_5+  | 2+_5+ | Total  |
|--------|---------------------|-------|-------|-------|-------|-------|-------|-------|-------|-------|-------|-------|-------|-------|-------|--------|
| Bexar  | $0 - $22,841        | 46218 | 20006 | 13141 | 9606  | 2372  | 4779  | 6598  | 1238  | 2816  | 3713  | 1081  | 2408  | 4551  | 825   | 119352 |
| Bexar  | $22,842 - $44,963   | 18150 | 39993 | 12173 | 18034 | 8102  | 2706  | 10335 | 5011  | 1148  | 7615  | 4688  | 1377  | 6185  | 4950  | 140467 |
| Bexar  | $44,964 - $67,446    | 11057 | 32266 | 8412  | 16026 | 15859 | 2047  | 8204  | 10051 | 953   | 6036  | 7526  | 685   | 5408  | 7788  | 132318 |
| Bexar  | $67,447 - $112,410   | 5985  | 22157 | 11528 | 18666 | 27222 | 2094  | 10703 | 20604 | 788   | 7625  | 17195 | 777   | 4887  | 17104 | 167335 |
| Bexar  | $112,411+           | 2829  | 10348 | 8469  | 16575 | 34467 | 1192  | 7166  | 28313 | 784   | 6930  | 31339 | 685   | 4907  | 26926 | 180930 |
|        | Total               | 84239 | 124770| 53723 | 78907 | 88022 | 12818 | 43006 | 65217 | 6489  | 31919 | 61829 | 5932  | 25938 | 57593 | 740402 |

## This is the output in fraction format

| county | incgrp              | 0_1    | 1_1    | 0_2    | 1_2    | 2+_2   | 0_3    | 1_3    | 2+_3   | 0_4    | 1_4    | 2+_4   | 0_5+   | 1_5+   | 2+_5+  | Total  |
|--------|---------------------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|
| Bexar  | $0 - $22,841        | 0.0624 | 0.0270 | 0.0178 | 0.0130 | 0.0032 | 0.0065 | 0.0089 | 0.0017 | 0.0038 | 0.0050 | 0.0015 | 0.0033 | 0.0062 | 0.0011 | 0.1613 |
| Bexar  | $22,842 - $44,963   | 0.0245 | 0.0540 | 0.0165 | 0.0244 | 0.0109 | 0.0037 | 0.0140 | 0.0068 | 0.0016 | 0.0103 | 0.0063 | 0.0019 | 0.0084 | 0.0067 | 0.1898 |
| Bexar  | $44,964 - $67,446    | 0.0149 | 0.0436 | 0.0114 | 0.0216 | 0.0214 | 0.0028 | 0.0111 | 0.0136 | 0.0013 | 0.0082 | 0.0102 | 0.0009 | 0.0073 | 0.0105 | 0.1787 |
| Bexar  | $67,447 - $112,410   | 0.0081 | 0.0299 | 0.0156 | 0.0252 | 0.0368 | 0.0028 | 0.0145 | 0.0278 | 0.0011 | 0.0103 | 0.0233 | 0.0010 | 0.0066 | 0.0231 | 0.2260 |
| Bexar  | $112,411+           | 0.0038 | 0.0140 | 0.0114 | 0.0224 | 0.0466 | 0.0016 | 0.0097 | 0.0383 | 0.0011 | 0.0094 | 0.0423 | 0.0009 | 0.0066 | 0.0364 | 0.2444 |
|        | Total               | 0.1138 | 0.1685 | 0.0725 | 0.1066 | 0.1189 | 0.0173 | 0.0581 | 0.0881 | 0.0088 | 0.0431 | 0.0835 | 0.0080 | 0.0350 | 0.0778 | 1.0000 |

## Household Composition by Income Group visualization
```r
ggplot(pumas, aes(x = incgrp, fill = hhworker)) +
  geom_bar(position = "dodge", aes(weight = WGTP)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set2") +  # Change the palette as desired
  labs(
    title = "Household Composition by Income Group",
    x = "Income Group",
    y = "Weighted Count",
    fill = "Workers in HH"
  ) +
  theme_minimal()
```

![image](https://github.com/user-attachments/assets/4d7a2438-5e65-4ebf-a2e9-2ec628ac9986)

