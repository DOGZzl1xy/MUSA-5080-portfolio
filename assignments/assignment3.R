library(tidycensus)
library(dplyr)
library(tidyverse)
library(sf)
library(gt)

# H0: The poverty rate in tracts of Philadelphia is not changed from 2010 to 2017.

acs_list <- c("B17001_001", "B17001_002")

format_poverty <- function(df){
  df %>%
  mutate(
    variable = recode(
      variable,
      "B17001_001" = "Total Population",
      "B17001_002" = "Population in Poverty"
    )
  )%>%
    pivot_wider(
      id_cols = c(GEOID, NAME),
      names_from = variable,
      values_from = c(estimate, moe),
      names_glue = "{variable}_{.value}",
      names_vary = "slowest"
    )
}

poverty_2010 <- get_acs(
  geography = "tract",
  variables = acs_list,
  state = "PA",
  county = "Philadelphia",
  year = 2010,
  survey = "acs5"
) %>%
  format_poverty()

poverty_2017 <- get_acs(
  geography = "tract",
  variables = acs_list,
  state = "PA",
  county = "Philadelphia",
  year = 2017,
  survey = "acs5"
) %>%
  format_poverty()


poverty <- poverty_2010 %>%
  inner_join(
    poverty_2017,
    by = c("GEOID", "NAME"),
    suffix = c("_2010", "_2017")
  )


poverty_withrate <- poverty %>%
  mutate(
    poverty_rate_2010 =
      `Population in Poverty_estimate_2010` /
      na_if(`Total Population_estimate_2010`, 0),

    poverty_rate_moe_2010 = tidycensus::moe_prop(
      num = `Population in Poverty_estimate_2010`,
      denom = na_if(`Total Population_estimate_2010`, 0),
      moe_num = `Population in Poverty_moe_2010`,
      moe_denom = `Total Population_moe_2010`
    ),

    poverty_rate_2017 =
      `Population in Poverty_estimate_2017` /
      na_if(`Total Population_estimate_2017`, 0),

    poverty_rate_moe_2017 = tidycensus::moe_prop(
      num = `Population in Poverty_estimate_2017`,
      denom = na_if(`Total Population_estimate_2017`, 0),
      moe_num = `Population in Poverty_moe_2017`,
      moe_denom = `Total Population_moe_2017`
    )
  ) %>%
  mutate(
    cv_2010 = poverty_rate_moe_2010 / 1.645 / poverty_rate_2010 * 100,
    cv_2017 = poverty_rate_moe_2017 / 1.645 / poverty_rate_2017 * 100
  ) %>%
  mutate(
    delta_p = poverty_rate_2017 - poverty_rate_2010,
    delta_moe = sqrt(poverty_rate_moe_2010^2 + poverty_rate_moe_2017^2)
  ) %>%
  mutate(
    max_p = delta_p + delta_moe,
    min_p = delta_p - delta_moe,
    include_zero = ifelse(min_p <= 0 & max_p >= 0, TRUE, FALSE)
  )


increase_top_15 <- filter(poverty_withrate) %>%
  arrange(desc(delta_p)) %>%
  slice_head(n = 15) %>%
  mutate(NAME = str_remove(NAME, ", Philadelphia County, Pennsylvania"))

decrease_top_15 <- filter(poverty_withrate) %>%
  arrange(delta_p) %>%
  slice_head(n = 15) %>%
  mutate(NAME = str_remove(NAME, ", Philadelphia County, Pennsylvania"))


ggplot(increase_top_15) +
  geom_col(aes(x = reorder(NAME, delta_p), y = delta_p, fill = include_zero)) +
  geom_errorbar(aes(x = reorder(NAME, delta_p), ymin = min_p, ymax = max_p), width = 0.3) +
  coord_flip() +
  labs(
    x = "Census Tract",
    y = "Increase in Poverty Rate (2010-2017)",
    title = "Increase in Poverty Rate in Philadelphia Census Tracts (2010-2017)",
    subtitle = "Error bars represent the moe for the increase in poverty rate",
    fill = "Includes Zero"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "salmon")) +
  theme_minimal()


ggplot(decrease_top_15) +
  geom_col(aes(x = reorder(NAME, delta_p), y = delta_p, fill = include_zero)) +
  geom_errorbar(aes(x = reorder(NAME, delta_p), ymin = min_p, ymax = max_p), width = 0.3) +
  coord_flip() +
  labs(
    x = "Census Tract",
    y = "Decrease in Poverty Rate (2010-2017)",
    title = "Decrease in Poverty Rate in Philadelphia Census Tracts (2010-2017)",
    subtitle = "Error bars represent the moe for the decrease in poverty rate",
    fill = "Includes Zero"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "salmon")) +
  theme_minimal()


reliability_table <- poverty_withrate %>%
  mutate(
  dot_2010 = case_when(
    is.na(cv_2010)       ~ "⚪",
    is.infinite(cv_2010) ~ "⚪",
    cv_2010 < 12  ~ "🟢",
    cv_2010 <= 40 ~ "🟡",
    TRUE           ~ "🔴"
  ),
  flag_2010 = case_when(
    is.na(cv_2010)       ~ "Not available",
    is.infinite(cv_2010) ~ "Undefined CV (zero estimate)",
    cv_2010 < 12        ~ "Reliable",
    cv_2010 <= 40       ~ "Somewhat reliable",
    TRUE                ~ "Unreliable"
  ),
  dot_2017 = case_when(
    is.na(cv_2017)       ~ "⚪",
    is.infinite(cv_2017) ~ "⚪",
    cv_2017 < 12  ~ "🟢",
    cv_2017 <= 40 ~ "🟡",
    TRUE           ~ "🔴"
  ),
  flag_2017 = case_when(
    is.na(cv_2017)       ~ "Not available",
    is.infinite(cv_2017) ~ "Undefined CV (zero estimate)",
    cv_2017 < 12        ~ "Reliable",
    cv_2017 <= 40       ~ "Somewhat reliable",
    TRUE                ~ "Unreliable"
  ),
  NAME = str_remove(NAME, ", Philadelphia County, Pennsylvania")
) %>%
  arrange(NAME) %>%
  select(NAME,dot_2010, poverty_rate_2010, poverty_rate_moe_2010, cv_2010, flag_2010,
              dot_2017, poverty_rate_2017, poverty_rate_moe_2017, cv_2017, flag_2017)


reliability_table %>%
  gt() %>%
  tab_header(
    title = "Reliability of Poverty Rate Estimates in Philadelphia Census Tracts"
  ) %>%
  cols_label(

    NAME = "Census Tract",
    dot_2010 = "",
    poverty_rate_2010 = "Poverty Rate (2010)",
    poverty_rate_moe_2010 = "MoE (2010)",
    cv_2010 = "CV (2010)",
    flag_2010 = "Reliability Flag",
    dot_2017 = "",
    poverty_rate_2017 = "Poverty Rate (2017)",
    poverty_rate_moe_2017 = "MoE (2017)",
    cv_2017 = "CV (2017)",
    flag_2017 = "Reliability Flag"

  ) %>%
  fmt_percent(
    columns = c(poverty_rate_2010, poverty_rate_2017),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(poverty_rate_moe_2010, cv_2010, poverty_rate_moe_2017, cv_2017),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = px(12)
  )

change_summary <- poverty_withrate %>%
  mutate(
    result = case_when(
      is.na(include_zero) ~ "Not testable",
      !include_zero & delta_p > 0 ~ "Significant Increase",
      !include_zero & delta_p < 0 ~ "Significant Decrease",
      TRUE ~ "Not Significant"
    )
  )%>%
  count(result)

ggplot(change_summary, aes(x = result, y = n, fill = result)) +
  geom_col(width = 0.3) +
  labs(
    x = "Change Result",
    y = "Number of Census Tracts",
    title = "Summary of Poverty Rate Change in Philadelphia Census Tracts (2010-2017)",
  ) +
  scale_fill_manual(values = c(
    "Significant Increase" = "salmon",
    "Significant Decrease" = "lightblue",
    "Not Significant" = "gray",
    "Not testable" = "yellow"
  )) +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.2) +
  theme(legend.position = "none")

