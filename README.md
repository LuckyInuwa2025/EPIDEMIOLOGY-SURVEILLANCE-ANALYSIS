# Epidemiological Surveillance Analysis (R)

Analysis of monthly disease surveillance data for 12 Nigerian states
(2019–2024) covering malaria, cholera, measles and Lassa fever.
**Note: the data are synthetic and for practice only.**

## Objectives
1. Reshape data between long and wide formats
2. Visualise trends with ggplot2
3. Explore relationships between numeric variables using geom_line
4. Carry out further epidemiological analysis

## Data
| File | Description |
|---|---|
| epi_surveillance_long | Monthly cases/deaths by state and disease |
| epi_annual_cases_wide | Annual cases, one column per year |
| epi_linelist | 1,500 individual case records |
| state_metadata | Zone, climate belt, Lassa endemicity |

## Methods
- Reshaping: `pivot_longer()`, `pivot_wider()`
- Visualisation: bar, box, heatmap, epi curve, line and path plots
- Analysis: case fatality rate, lagged correlation, Poisson regression
  with population offset, z-score outbreak detection, age pyramid, joins

## Key findings
- Malaria peaks about a month after peak rainfall (seasonal loop plot)
- Measles incidence falls as vaccination coverage rises
- Lassa fever peaks Dec–Mar, concentrated in high-endemicity states
- Cholera outbreaks cluster in July–October
- Reporting dipped in April–June 2020

## Selected outputs
<img width="381" height="246" alt="04_epi_curves" src="https://github.com/user-attachments/assets/3d696079-e08e-4ee2-9750-a72468f117c6" />


<img width="381" height="246" alt="06_seasonal_loop_rainfall_malaria" src="https://github.com/user-attachments/assets/d4a6b69b-fa84-4ef2-aacb-d894753fbd0d" />


## How to reproduce
1. Clone: `git clone https://github.com/YOUR-USERNAME/epi-surveillance-analysis.git`
2. Open the `.Rproj` file in RStudio
3. Install packages: `install.packages(c("tidyverse","lubridate","readxl","here"))`
4. Run `Scripts/Script_for_Analysis.R`

## Tools
R, RStudio, tidyverse (dplyr, tidyr, ggplot2), lubridate, here

## Author
Lucky Yawat Inuwa · inuwalucky10@gmail.com · linkedin.com/in/lucky-yawat-642a00290

## License
MIT
