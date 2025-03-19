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
`
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
write.csv(combined_table, "C:\\Users\\RAFAPC\\Documents\\output.csv")
```

## Household Composition by Income Group visualization
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
