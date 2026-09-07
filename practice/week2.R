library(dplyr)
library(tidycensus)
library(tidyverse)

PA_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
)

dim(PA_income)
glimpse(PA_income)
head(PA_income)
# shape: 67 rows and 5 columns
# Pennsylvania has 67 counties. I GOT IT!

PA_income$GEOID
as.numeric("01001")

filter(PA_income, estimate > 60000) # get 56!
# Counties where the margin of error is bigger than 3000
filter(PA_income, moe > 3000) # get 15
# Counties where the estimate is under 50000
filter(PA_income, moe<50000) # get 67


select(PA_income, NAME, estimate, moe)
select(PA_income, GEOID, estimate)

mutate(PA_income, moe_pct = moe / estimate * 100)
PA_income <- mutate(PA_income, moe_pct = moe / estimate * 100)
PA_income


arrange(PA_income, moe_pct)
arrange(PA_income, desc(moe_pct))


step1 <- filter(PA_income, moe_pct > 5)
step2 <- arrange(step1, desc(moe_pct))
step3 <- select(step2, NAME, estimate, moe, moe_pct)
step3

PA_income %>%
  filter(moe_pct > 5) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, estimate, moe, moe_pct)


# Keep counties with moe_pct over 8, sort by estimate, show NAME and moe_pct

worst <- PA_income %>%
  filter(moe_pct > 8) %>%
  arrange(estimate) %>%
  select(NAME, moe_pct)
worst


PA_income <- mutate(PA_income, reliable = moe_pct < 5)
PA_income %>%
  group_by(reliable) %>%
  summarize(n = n(),
            avg_income = mean(estimate))

PA_income <- PA_income %>%
  mutate(reliability = case_when(
    moe_pct < 3 ~ "High confidence",
    moe_pct < 6 ~ "Moderate",
    TRUE        ~ "Low confidence"
  ))
count(PA_income, reliability)


## A tibble: 3 × 2
# reliability         n
# <chr>             <int>
# 1 High confidence    26
# 2 Low confidence      7
# 3 Moderate           34


pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001", "B01003_001"),
  state = "PA", year = 2023, survey = "acs5"
)

pa_two

pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop    = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)

pa_wide


pa_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)

