# How to develop a demographic multi-dimensional contingency table using PUMS data and 'pivot_wider'
The manual process of downloading separate household and individual-level Public Use Microdata Sample (PUMS) files is now obsolete. Using the `tidycensus` package, downloading, processing, and summarizing PUMS data in R has become straightforward and efficient. In this exercise, we'll create a demographic multi-dimensional contingency table. This table aggregates key demographic indicators—including age groups, Hispanic population, and household sizes (1-person, 2-person, 3-person, 4-person, and 5+ person households)—specifically for Bexar County, TX.

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
rlibrary(pacman)
```

## Install & load packages
```r
p_load(
  car,
  dplyr,
  ggplot,
  matrixStats,
  purrr,
  questionr,
  scales,
  srvyr,
  survey,
  stringr,
  tidycensus,
  tidyverse,
  tidyr
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
tx_pums <- get_pums(
  variables = c("PUMA", "SEX", "AGEP", "RAC1P", "HISP", "RELSHIPP", "NP", "ESR"),
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

## Generate grouped household size variable
```r
pumas$hhsize <- Recode(pumas$NP, recodes = "1=1; 2=2; 3=3; 4=4; 5:20='5+'; else=NA", as.factor = T)
```

## Create flag variable for workers in the household
```r
pumas$worker <- factor(ifelse(pumas$ESR == 1, "1", ifelse(pumas$ESR == 2, "1", NA)))
```

## Create flag to identify workers in the household
```r
pumas <- pumas %>%
  group_by(SERIALNO) %>%
  mutate(wihh = sum(worker == 1, na.rm = TRUE))
```

## Generate grouped number of workers in household variable
```r
pumas$hhworker <- factor(ifelse(pumas$wihh == 0, "0", ifelse(pumas$wihh == 1, "1", ifelse(pumas$wihh >= 2, "2+", NA))))
```

## Generate variable to distinguish different types of households
```r
pumas$hhtype <- factor(ifelse(pumas$WGTP != 0, "1", ifelse(pumas$WGTP != 2, "2", NA)))
```

## Generate flag variable identifying householders
```r
pumas$hholder <- Recode(pumas$RELSHIPP, recodes = "20=1; else = 0", as.factor = T)
```

## Generate grouped age variable
```r
pumas$agegrp <- Recode(
  pumas$AGEP,
  recodes =
    "0:15='0-15';
     16:19='16-19';
     20:24='20-24';
     25:29='25-29';
     30:34='30-34';
     35:39='35-39';
     40:44='40-44';
     45:49='45-49';
     50:54='50-54';
     55:59='55-59';
     60:64='60-64';
     65:69='65-69';
     70:74='70-74';
     75:79='75-79';
     80:92='80+';
     else = NA",
  as.factor = T,
  levels = c(
    "0-15", "16-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-79", "80+"
  )
)
```

## Create flag variable to identify workers by sex
```r
pumas$SEX_Recode <- factor(ifelse(pumas$SEX == 1, "Male", ifelse(pumas$SEX == 2, "Female", NA)))
```

## View the first few rows to verify
```r
head(pumas)
```

## Check data type of key variables
```r
class(pumas$HISP)   # HISP data type
class(pumas$RAC1P)  # RAC1P data type
class(pumas$PWGTP)  # PWGTP data type
```

## Convert HISP and RAC1P to numeric if they are characters
```r
pumas <- pumas %>%
  mutate(
    HISP = as.numeric(HISP),
    # Ensure HISP is numeric
    RAC1P = as.numeric(RAC1P),
    # Ensure RAC1P is numeric
    
    # Identify Hispanic Population: HISP (2-24) AND RAC1P (1-9)
    Hispanic = ifelse(HISP >= 2 &
                        HISP <= 24 &
                        RAC1P >= 1 & RAC1P <= 9, PWGTP, 0),
    
    # Identify White Alone Population: HISP (1) AND RAC1P (1)
    White_Alone = ifelse(HISP == 1 & RAC1P == 1, PWGTP, 0)
  )
```

## Create flag variables for Hispanic and White Alone households
```r
pumas <- pumas %>%
  mutate(
    HISP_FLAG = ifelse(HISP >= 2 & HISP <= 24 & RAC1P >= 1 & RAC1P <= 9, "HISPANIC", NA),
    WHITE_FLAG = ifelse(HISP == 1 & RAC1P == 1, "WHITE_ALONE", NA)
  )
```

## View updated dataset with weights applied
```r
head(pumas)
```

## Summarize the weighted totals for Hispanic and White Alone populations
```r
hispanic_summary <- pumas %>%
  summarize(
    Total_Hispanic = sum(Hispanic, na.rm = TRUE),
    Total_White_Alone = sum(White_Alone, na.rm = TRUE)
  )
```

## Print the summarized results
```r
print(hispanic_summary)
```

## Ensure correct column types
```r
pumas <- pumas %>%
  mutate(
    county = as.character(county),
    agegrp = as.character(agegrp),
    HISP_FLAG = as.character(HISP_FLAG),
    WHITE_FLAG = as.character(WHITE_FLAG),
    hhsize = as.character(hhsize),
    hhworker = as.character(hhworker),
    SEX_Recode = as.character(SEX_Recode),
    PWGTP = as.numeric(PWGTP)
  )
```

## Aggregate household data if householder is present
```r
Bexar_all <- pumas %>%
  group_by(county, agegrp, HISP_FLAG, WHITE_FLAG, hhsize, hhworker, SEX_Recode) %>%
  summarize(n = sum(PWGTP, na.rm = TRUE), .groups = "drop")
```

## Identify list columns (if any)
```r
list_cols <- map_lgl(Bexar_all, is.list)
print(names(Bexar_all)[list_cols])
```

## Ensure no duplicate groupings
```r
Bexar_all <- Bexar_all %>%
  group_by(county, agegrp, HISP_FLAG, WHITE_FLAG, hhsize) %>%
  summarize(n = sum(n, na.rm = TRUE), .groups = "drop")
```

## Filter out rows with NA in HISP_FLAG or hhsize
```r
Bexar_all <- Bexar_all %>%
  filter(!is.na(HISP_FLAG) & !is.na(hhsize))
```

## Replace NA values in WHITE_FLAG with "WHITE_ALONE"
```r
Bexar_all <- Bexar_all %>%
  mutate(WHITE_FLAG = ifelse(is.na(WHITE_FLAG), "WHITE_ALONE", WHITE_FLAG))
```

## Apply pivot_wider with correct values_fill to ceeate a multi-dimensional contingency table
```r
multid <- pivot_wider(
  Bexar_all,
  id_cols = c("county", "agegrp"),
  names_from = c("HISP_FLAG", "hhsize"),
  values_from = "n",
  values_fill = list("n" = 0)  # Ensure named list for values_fill with correct type
)
```

## Save the processed data to a CSV file
```r
write.csv(multid, "...", row.names = FALSE)
```

## Checking the data 
The output of the data is found on the table below:

| county   | agegrp   |   HISPANIC_1 |   HISPANIC_2 |   HISPANIC_3 |   HISPANIC_4 |   HISPANIC_5+ |   Total      |
|:---------|:---------|-------------:|-------------:|-------------:|-------------:|--------------:|-------------:|
| Bexar    | 0-15     |          597 |        11804 |        45282 |        86024 |        156458 |       300165 |
| Bexar    | 16-19    |         3744 |         6261 |        13687 |        20824 |         32806 |        77322 |
| Bexar    | 20-24    |         8030 |        17807 |        20249 |        21395 |         26046 |        93527 |
| Bexar    | 25-29    |        10804 |        23306 |        20213 |        19566 |         21495 |        95384 |
| Bexar    | 30-34    |         8938 |        19837 |        19623 |        19277 |         26288 |        93963 |
| Bexar    | 35-39    |         7606 |        15429 |        16716 |        21725 |         27158 |        88634 |
| Bexar    | 40-44    |         7651 |        12772 |        18172 |        20771 |         26476 |        85842 |
| Bexar    | 45-49    |         6590 |        14890 |        17057 |        19901 |         17008 |        75446 |
| Bexar    | 50-54    |         8024 |        18242 |        14180 |        14410 |         12379 |        67235 |
| Bexar    | 55-59    |         8513 |        18565 |        14392 |         9497 |          9315 |        60282 |
| Bexar    | 60-64    |        10353 |        20617 |         9640 |         5952 |          7346 |        53908 |
| Bexar    | 65-69    |         9615 |        16976 |         6576 |         4079 |          5106 |        42352 |
| Bexar    | 70-74    |         8282 |        14032 |         4814 |         3252 |          3419 |        33799 |
| Bexar    | 75-79    |         5365 |         8810 |         2803 |         1853 |          2160 |        20991 |
| Bexar    | 80+      |         7581 |         8380 |         3919 |         2236 |          1884 |        24000 |
| **Total**|          |       111693 |       227728 |       227323 |       270762 |        375344 |  **1212850** |

The total number of Hispanics in Bexar County calculated here (1,212,850), categorized by age and household size (1-person, 2-person, 3-person, 4-person, and 5+ person households), closely aligns with the 2023 ACS 5-year estimate of 1,212,855. This estimate can be verified through the Census Bureau's official data portal: [ACS 2023 5-Year Hispanic or Latino Data for Bexar County](https://data.census.gov/table/ACSDP5Y2023.DP05?q=Hispanic+or+Latino&g=050XX00US48029).

# Population Distribution by Age Group using ggplot2 visualizations
### Summary of Visualizations

The visualizations created in this analysis aim to clearly illustrate the demographic composition of the Hispanic population in Bexar County, Texas. They specifically highlight the distribution of this population across various age groups and household sizes. Utilizing bar charts and stacked bar graphs generated with `ggplot2`, these visualizations facilitate quick interpretation of demographic patterns, making it easier to identify trends and differences in population composition across multiple dimensions.
## Population Distribution by Age Group

```r
df_long <- multid %>%
  pivot_longer(cols = starts_with("HISPANIC"), 
               names_to = "Household_Size", 
               values_to = "Population") %>%
  group_by(agegrp) %>%
  summarize(Total = sum(Population, na.rm = TRUE))

# Plot with formatted y-axis
ggplot(df_long, aes(x = agegrp, y = Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Hispanic Population by Age Group",
       x = "Age Group",
       y = "Population") +
  scale_y_continuous(labels = comma) +  # adds comma formatting
  theme_minimal()
```
![image](https://github.com/user-attachments/assets/1017b85b-02bb-443f-a81d-69ee05a959ff)

## Stacked Bar Chart by Age Group and Household Size
```r
df_stacked <- multid %>%
  pivot_longer(cols = starts_with("HISPANIC"), 
               names_to = "Household_Size", 
               values_to = "Population")

ggplot(df_stacked, aes(x = agegrp, y = Population, fill = Household_Size)) +
  geom_bar(stat = "identity") +
  labs(title = "Hispanic Population by Age Group and Household Size",
       x = "Age Group",
       y = "Population",
       fill = "Household Size") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
```
![image](https://github.com/user-attachments/assets/448aa55c-9e9e-4636-8276-a3b1024745dd)


## Heatmap of Population by Age Group and Household Size
```r
ggplot(df_stacked, aes(x = Household_Size, y = agegrp, fill = Population)) +
  geom_tile(color = "white") +
  labs(title = "Heatmap of Hispanic Population by Age and Household Size",
       x = "Household Size",
       y = "Age Group",
       fill = "Population") +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  theme_minimal()
```
![image](https://github.com/user-attachments/assets/7d59edf8-187c-4baa-b9d3-db43066b61d8)




