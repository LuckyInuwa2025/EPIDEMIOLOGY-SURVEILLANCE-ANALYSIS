# ============================================================
# Epidemiology practice script  (data are SYNTHETIC)
# ============================================================

# Install Packages ----

install.packages("tidyverse")
install.packages("lubridate")
install.packages("readr")  
install.packages("dplyr")
install.packages("tidyr")  
install.packages("ggplot2")
install.packages("readxl") 
install.packages("here")
library(tidyverse)
  library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(here)

# Import Dataset ----

library(readxl)
epi_annual_cases_wide_csv <- read_excel("Data/epi_annual_cases_wide.csv.xlsx")
View(epi_annual_cases_wide_csv)

library(readxl)
epi_linelist_csv <- read_excel("Data/epi_linelist.csv.xlsx")
View(epi_linelist_csv)

library(readxl)
epi_surveillance_long_csv <- read_excel("Data/epi_surveillance_long.csv.xlsx")
View(epi_surveillance_long_csv)

library(readxl)
state_metadata <- read_excel("Data/state_metadata.xlsx")
View(state_metadata)

# Assigning Names to Dataset ---- 

long   <- epi_surveillance_long_csv |> 
  mutate(date = ymd(date))
wide_a <- epi_annual_cases_wide_csv
ll     <- epi_linelist_csv
meta   <- state_metadata

glimpse(long); summary(long)
colSums(is.na(long))                      # missing values to handle


# 1. LONG <-> WIDE ----
# a) wide -> long : years are columns
annual_long <- wide_a |> 
  pivot_longer(cols = `2019`:`2024`, names_to = "year", values_to = "cases") |> 
  mutate(year = as.integer(year))
view(annual_long)
write.csv(annual_long, file = "Data/annual_long.csv", row.names = FALSE)


# b) long -> wide : one column per disease
monthly_wide <- long |> 
  select(state, date, disease, cases) |> 
  pivot_wider(names_from = disease, values_from = cases)
view(monthly_wide)
write.csv(monthly_wide, file = "Data/monthly_wide.csv", row.names = FALSE)


# c) long -> wide with two value columns
wide_cd <- long |> 
  select(state, year, month, disease, cases, deaths) |> 
  pivot_wider(names_from = disease, values_from = c(cases, deaths),
              names_sep = "_")
view(wide_cd)
write.csv(wide_cd, file = "Data/wide_cd.csv", row.names = FALSE)

# d) and back again
back_long <- monthly_wide |> 
  pivot_longer(Malaria:`Lassa Fever`, names_to = "disease", values_to = "cases")
view(back_long)
write.csv(back_long, file = "Data/back_long.csv", row.names = FALSE)

# 2. ggplot ----
# Bar chart: total cases per disease (log scale because malaria dominates)
long |> 
group_by(disease) |> 
summarise(total = sum(cases)) |> 
  ggplot(aes(reorder(disease, total), total, fill = disease)) +
  geom_col(show.legend = FALSE) + scale_y_log10(labels = scales::comma) +
  coord_flip() + labs(x = NULL, y = "Total cases (log10)", title = "Cases by disease")

# Boxplot: incidence by zone, faceted by disease
ggplot(long, aes(geopolitical_zone, incidence_per_100k, fill = geopolitical_zone)) +
  geom_boxplot(show.legend = FALSE) + facet_wrap(~disease, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heatmap: month x year for cholera
long |> 
  filter(disease == "Cholera") |> 
  group_by(year, month) |> 
  summarise(cases = sum(cases), .groups = "drop") |> 
  ggplot(aes(factor(month), factor(year), fill = cases)) +
  geom_tile() + scale_fill_viridis_c(option = "magma") +
  labs(x = "Month", y = "Year", fill = "Cases")

# Epi curve per disease (national)
long |> 
group_by(date, disease) |> 
  summarise(cases = sum(cases), .groups = "drop") |> 
  ggplot(aes(date, cases, colour = disease)) + geom_line(linewidth = 0.8) +
  facet_wrap(~disease, scales = "free_y", ncol = 1) + theme_minimal()

# TWO NUMERICAL VARIABLES WITH geom_line ----
# Rainfall vs malaria: dual-line on a standardised scale
lagos <- long |> 
  filter(state == "Lagos", disease == "Malaria")
ggplot(lagos, aes(date)) +
  geom_line(aes(y = cases / 1000, colour = "Malaria cases (000s)")) +
  geom_line(aes(y = rainfall_mm / 5, colour = "Rainfall (mm / 5)")) +
  labs(y = NULL, colour = NULL, title = "Lagos: malaria vs rainfall")

# Average seasonal profile: rainfall vs incidence by month
long |> 
  filter(disease == "Malaria") |> 
  group_by(month) |> 
  summarise(rain = mean(rainfall_mm, na.rm = TRUE),
            inc  = mean(incidence_per_100k)) |> 
  ggplot(aes(rain, inc)) + geom_path() + geom_point(size = 3) +
  geom_text(aes(label = month), vjust = -1) +
  labs(x = "Mean rainfall (mm)", y = "Incidence /100k",
       title = "Seasonal loop: rainfall vs malaria")        # note the lag!

# Measles incidence vs vaccination coverage over time (line per state)
long |>  
  filter(disease == "Measles") |> 
  group_by(state, year) |> 
  summarise(vacc = mean(vaccination_coverage_pct, na.rm = TRUE),
            inc  = sum(cases) / mean(population) * 1e5, .groups = "drop") |> 
  ggplot(aes(vacc, inc, group = state, colour = state)) +
  geom_line() + geom_point()

# 4. FURTHER ANALYSIS ----
# a) Case fatality rate by disease
long |> 
  group_by(disease) |> 
  summarise(CFR_pct = sum(deaths) / sum(cases) * 100)

# b) Lagged correlation: rainfall vs malaria cases (0-3 month lag)
mal <- long |>
  filter(disease == "Malaria") |>
  arrange(state, date) |>
  group_by(state) |>
  mutate(rain_lag1 = lag(rainfall_mm, 1), rain_lag2 = lag(rainfall_mm, 2)) |>
  ungroup()

# c) Poisson / negative-binomial regression with population offset
m <- glm(cases ~ vaccination_coverage_pct + avg_temp_c + factor(month) +
           offset(log(population)),
         family = poisson, data = filter(long, disease == "Measles"))
summary(m); exp(coef(m))   # rate ratios

# d) Detect the outbreaks (z-score vs state/disease history)
long |> 
  group_by(state, disease) |> 
  mutate(z = (cases - mean(cases)) / sd(cases)) |> 
  filter(z > 3) |> 
  arrange(desc(z))

# e) COVID-19 reporting dip in 2020 (Apr-Jun)
long |> 
  filter(disease == "Malaria") |> 
  group_by(date) |> 
  summarise(cases = sum(cases)) |> 
  ggplot(aes(date, cases)) + geom_line() +
  annotate("rect", xmin = ymd("2020-04-01"), xmax = ymd("2020-06-30"),
           ymin = -Inf, ymax = Inf, alpha = .2, fill = "red")

# f) Line list: age pyramid & age distribution by outcome
ll |> 
mutate(age_grp = cut(age, c(-1,4,14,29,49,64,100),
                            labels = c("0-4","5-14","15-29","30-49","50-64","65+"))) %>%
  count(age_grp, sex) |> 
  ggplot(aes(age_grp, ifelse(sex == "Male", -n, n), fill = sex)) +
  geom_col() + coord_flip() + labs(y = "Cases", x = "Age group")

ll |> 
  group_by(disease) |> 
  summarise(n = n(), median_age = median(age),
            pct_died = mean(outcome == "Died") * 100)

# g) Join with metadata and compare Lassa-endemicity groups
long |> 
left_join(meta, by = "state") |> 
  filter(disease == "Lassa Fever") |> 
  group_by(lassa_endemic, month) |> 
  summarise(cases = sum(cases), .groups = "drop") |> 
  ggplot(aes(month, cases, colour = lassa_endemic)) + geom_line() +
  scale_x_continuous(breaks = 1:12)
