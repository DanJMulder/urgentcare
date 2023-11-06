############################################################
# KHSC Pediatric Urgent Care NACRS Data Analysis Script
# Scripts by Daniel Mulder Aug-Nov 2023 and contributed/reviewed by co-author team
# Data pull sent by decision support May 16 2023
############################################################

library(tidyverse)  # for basic data organization
library(magrittr) # piping
library(DT) # for datatables
library(glue)  # for gluing together text
library(lubridate)  # for creating/parsing date objects
library(officer)  # for saving notes to office formats
library(readxl)  # for working with xlsx files
library(skimr) # for getting detailed and aesthetically pleasing overview of each of the variables in your dataset
library(janitor) # tabyl produces the unique values, counts, and column-wise “percents” (actually proportions)
library(ggrepel) # for ggplot repelled labels
library(reshape2) # to melt for ggplot data
library(scales) # scaling functions for plots
library(e1071) # for skewness
library(tidymodels) # machine learning framework
library(GGaly) # for combining ggplot2 objects in simple ways
library(vip) # for variable importance plots


# Load in and prep data#########################################################

nacrs_copc_small_2 <- read_csv("nacrs_copc_clean.csv")
source("helper_objects.R")

nacrs_copc <- nacrs_copc_small_2 |>
  select(-encounter) |>
  mutate(gender = as.factor(gender)) |>
  mutate(primary_care_access = fct_recode(primary_care_access,
                                          has_pcp_access = "has_fam_physician",
                                          has_pcp_access = "fam_team_or_walkin",
                                          no_or_unknown_pcp_access = "no_pcp_access",
                                          no_or_unknown_pcp_access = "pcp_access_unknown",
                                          no_or_unknown_pcp_access = "NULL")) |>
  mutate(region = fct_collapse(region,
                               kingston_area = kingston_area,
                               belleville_area = belleville_area,
                               brockville_area = brockville_area,
                               nonregional = c(north_area, distant_area))) |>
  mutate(region = as.factor(region)) |>
  mutate(ctas = ifelse(ctas == "9", NA, ctas)) |>
  mutate(ctas = as.factor(ctas)) |>
  mutate(Year = lubridate::year(reg_datetime)) |>
  mutate(Year = as.factor(Year)) |>
  mutate(reg_date_only = as.Date(reg_datetime)) |>
  mutate(days_since_reference = as.numeric(difftime(reg_date_only, as.Date("2006-04-03"), units = "days"))) |>
  mutate(registration_time_of_day = format(reg_datetime, format = "%H:%M:%S")) |>
  mutate(registration_time_of_day = hms(registration_time_of_day)) |>
  mutate(reg_minutes_since_0900 = (hour(registration_time_of_day) * 60 + minute(registration_time_of_day) + second(registration_time_of_day) / 60) - (9 * 60))  |>
  mutate(left_date_only = as.Date(left_datetime)) |>
  mutate(left_time_of_day = format(left_datetime, format = "%H:%M:%S")) |>
  mutate(left_time_of_day = hms(left_time_of_day)) |>
  mutate(left_minutes_since_0900 = (hour(left_time_of_day) * 60 + minute(left_time_of_day) + second(left_time_of_day) / 60) - (9 * 60))  |>
  mutate(day_of_week = as.factor(day_of_week)) |>
  mutate(referred_from = as.factor(referred_from)) |>
  mutate(referred_from = fct_collapse(referred_from,
                                      self = c("Self/Family", "Unknown"),
                                      facility = facility)) |>
  mutate(chief_complaint = as.factor(chief_complaint)) |>
  mutate(chief_complaint = fct_collapse(chief_complaint,
                                        cc_cardiac = cc_cardiac,
                                        cc_ent = cc_ent,
                                        cc_environmental = cc_environmental,
                                        cc_gi = cc_gi,
                                        cc_gu = cc_gu,
                                        cc_mental_health = cc_mental_health,
                                        cc_neurologic = cc_neurologic,
                                        cc_ob_gyn = cc_ob_gyn,
                                        cc_ophtho = cc_ophtho,
                                        cc_ortho = cc_ortho,
                                        cc_resp = cc_resp,
                                        cc_derm = cc_derm,
                                        cc_substance_misuse = cc_substance_misuse,
                                        cc_trauma = cc_trauma,
                                        cc_general_minor = cc_general_minor)) |>
  mutate(cacs_dx = as.factor(cacs_dx)) |>
  mutate(cacs_dx = fct_collapse(cacs_dx,
                                cardiovasc = cardiovasc,
                                derm = derm,
                                endocrine_renal_fluid = endocrine_renal_fluid,
                                ent_dental_ophtho = ent_dental_ophtho,
                                gi_liver = gi_liver,
                                gyne_uro = gyne_uro,
                                heme_onc = heme_onc,
                                infectious = infectious,
                                lwobs = lwobs,
                                mental_health = mental_health,
                                msk = msk,
                                neuro = neuro,
                                other_cacs_dx = other_cacs_dx,
                                resp = resp)) |>
  mutate(intervention = ifelse(intervention == "NULL", "none", intervention)) |>
  mutate(intervention = ifelse(intervention != "none", "yes", intervention)) |>
  mutate(intervention = as.factor(intervention)) |>
  select(-discharge_diagnosis) |>
  select(-disposition) |>
  mutate(disposition = as.factor(disposition_2)) |>
  select(-disposition_2) |>
  mutate(disposition = fct_collapse(disposition,
                                    home = home,
                                    admit_or_transfer_to_facility = admit_or_transfer_to_facility,
                                    lwobs_ama = lwobs_ama)) |>
  mutate(disposition_service = case_when(disposition_service == "NULL" ~ "home",
                                         TRUE ~ disposition_service)) |>
  mutate(disposition_service = as.factor(disposition_service)) |>
  mutate(disposition_service = fct_collapse(disposition_service,
                                            outpatient_no_cas = outpatient_no_cas,
                                            outpatient_cas = outpatient_cas,
                                            inpatient = inpatient)) |>
  filter(Year != "2023") |> # removes 1932 visits from early 2023 that were included in the datapull
  filter(reg_minutes_since_0900 > -60) |> # removes 28 visits where the patient left more than an hour before triage opened
  filter(left_minutes_since_0900 > 0) |> # removes 13 visits where the patient left before triage opened (or triaged early)
  filter(left_minutes_since_0900 < 898) |> # removes 124 visits where left >15 h after 0900h (aka midnight)
  filter(length_of_stay_min < 1000) |> # removes 13651 visits (7.57% of data) where the length of stay is >16h40min (likely forgot to discharge, some admits, usually discharge time is between 0800h and 0900h next morning when likely next day team realized hadn't been discharged in system)
  filter(!is.na(cacs_dx)) |> # removes 27 values with no diagnosis
  filter(!is.na(ctas)) |> # removes same 27 values as line above, but just for consistency including this line - these were likely incompletely registered patients based on how incomplete their entry is
  # note that the 8 filter steps above were run separate from each other (1 filter step at a time) to get the number of rows removed at each step (ie each "removes X values" number is calculated by 180408-[filter step Y result])
  select(age_years, gender, region, primary_care_access, ctas, reg_datetime, reg_date_only, reg_minutes_since_0900, day_of_week, days_since_reference, left_datetime, length_of_stay_min, left_date_only, left_time_of_day, left_minutes_since_0900, referred_from, chief_complaint, cacs_dx, intervention, disposition_service, disposition, Year) |>
  mutate_at(.vars = vars(gender, region, primary_care_access, region, day_of_week, referred_from, chief_complaint, cacs_dx, intervention, disposition, disposition_service),
            .funs = list(~ if_else(. == "NULL", NA, .))) |>
  droplevels() |>
  write_csv(paste0("Raw Data/nacrs_copc_clean_", today(), ".csv"))

rm(list=setdiff(ls(), c("nacrs_copc", "nacrs_copc_small_2")))

set.seed(123)



# Figure 1A. visits per day (using registrations)###############################

visits_per_day_all_plot <- nacrs_copc |>
  mutate(reg_date_only = as.Date(reg_datetime)) |>
  group_by(reg_date_only) |>
  summarize(visits = n()) |>
  ggplot(aes(x = reg_date_only, y = visits)) + 
  geom_line(colour = "light blue") +
  geom_smooth(method = "loess", span = 0.3, colour = "purple") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        plot.margin = margin(1, 1, 0.5, 1, "cm")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(expand = c(0, 0)) +
  labs(x = "Year", y = "Patients per Day")
visits_per_day_all_plot
ggsave(paste0("nacrs_visits_per_day_", today(), ".png"),
       visits_per_day_all_plot,
       width = 14, height = 8, dpi = 600, units = "in", device='png')


# Figure 1B. length of stay#####################################################

avg_los_plot <- nacrs_copc |>
  mutate(reg_date_only = as.Date(reg_datetime)) |>
  group_by(reg_date_only) |>
  summarize(avg_los = mean(length_of_stay_min)) |>
  ggplot(aes(x = reg_date_only, y = avg_los)) + 
  geom_line(colour = "mediumpurple1") +
  geom_smooth(method = "loess", span = 0.3, colour = "#541388") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        plot.margin = margin(1, 1, 0.5, 1, "cm")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(expand = c(0, 0), limits = c(as.Date("2007-06-01"), as.Date("2022-12-31"))) +
  labs(x = "Year", y = "Daily Avg Length of Stay (Minutes)")
avg_los_plot
ggsave(paste0("avg_los_plot_", today(), ".png"),
       avg_los_plot,
       width = 14, height = 8, dpi = 600, units = "in", device='png')


 # inset for Figure 1B. just 2019 to 2022

avg_los_plot_small <- nacrs_copc |>
  mutate(reg_date_only = as.Date(reg_datetime)) |>
  group_by(reg_date_only) |>
  summarize(avg_los = mean(length_of_stay_min)) |>
  ggplot(aes(x = reg_date_only, y = avg_los)) + 
  geom_line(colour = "mediumpurple1") +
  geom_smooth(method = "loess", span = 0.3, colour = "#541388") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        plot.margin = margin(1, 1, 0.5, 1, "cm")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(expand = c(0, 0), limits = c(as.Date("2019-01-01"), as.Date("2022-10-31"))) +
  labs(x = "Year", y = "Daily Avg Length of Stay (Minutes)")
avg_los_plot_small
ggsave(paste0("avg_los_plot_small_", today(), ".png"),
       avg_los_plot_small,
       width = 14, height = 8, dpi = 600, units = "in", device='png')


# Figure 1C. hours of care per day ###########################

hours_of_care_per_day_plot <- nacrs_copc |>
  mutate(reg_date_only = as.Date(reg_datetime)) |>
  group_by(reg_date_only) |>
  summarise(min_of_care_per_day = sum(length_of_stay_min)) |>
  mutate(hours_of_care_per_day = min_of_care_per_day / 60) |>
  ggplot(aes(x = reg_date_only, y = hours_of_care_per_day)) + 
  geom_line(colour = "lightblue") +
  geom_smooth(method = "loess", span = 0.3, colour = "blue") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        plot.margin = margin(1, 1, 0.5, 1, "cm")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(expand = c(0, 0), limits = c(as.Date("2007-06-01"), as.Date("2022-12-31"))) +
  labs(x = "Year", y = "Hours of Care Per Day")
hours_of_care_per_day_plot
ggsave(paste0("hours_of_care_per_day_plot_", today(), ".png"),
       hours_of_care_per_day_plot,
       width = 14, height = 8, dpi = 600, units = "in", device='png')


# Figure 1D. hours of care per day ANNUAL TOTALS ###############################

hours_of_care_by_year <- function(start_year, end_year){
  hours_of_care_per_day_plot <- nacrs_copc |>
    na.omit(length_of_stay_min) |>
    mutate(reg_date_only = as.Date(reg_datetime)) |>
    group_by(reg_date_only) |>
    summarise(min_of_care_per_day = sum(length_of_stay_min)) |>
    mutate(hours_of_care_per_day = min_of_care_per_day / 60) |>
    mutate(Year = lubridate::year(reg_date_only)) |>
    filter(Year >= start_year & Year <= end_year) |>
    group_by(Year) |>
    summarise(total_hours_of_care = sum(hours_of_care_per_day))
}

hours_of_care_by_year_2007_to_2022 <- hours_of_care_by_year(2007, 2022)

hours_of_care_by_year_2007_to_2022_plot <- ggplot(hours_of_care_by_year_2007_to_2022,
                                                  aes(x = Year, y = total_hours_of_care)) +
  geom_bar(stat = "identity", fill = "royal blue") +
  labs(x = "Year", y = "Total Hours of Care") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        plot.margin = margin(1, 1, 0.5, 1, "cm")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 39000)) +
  scale_x_continuous(breaks = seq(2007, 2022, by = 3), labels = as.character(seq(2007, 2022, by = 3)))
hours_of_care_by_year_2007_to_2022_plot
ggsave(paste0("hours_of_care_by_year_2007_to_2022_plot_big_", today(),".png"), hours_of_care_by_year_2007_to_2022_plot,
       width = 14, height = 8, dpi = 600, units = "in", device='png')


# Figure 1E. histograms comparing 2014 and 2022 length of stay ###############################

# Filter the data for the years 2014 and 2022
sample_years_for_histo <- nacrs_copc |>
  filter(Year %in% c(2014, 2022))

# Create separate histograms for each year
sample_years_los_histo <- ggplot(sample_years_for_histo, aes(x = length_of_stay_min)) +
  geom_histogram(data = filter(sample_years_for_histo, Year == 2022), binwidth = 3, fill = "purple", alpha = 0.7) +
  geom_histogram(data = filter(sample_years_for_histo, Year == 2014), binwidth = 3, fill = "blue", alpha = 0.5) +
  labs(x = "Length of Stay (minutes)",
       y = "Count") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        plot.margin = margin(1, 1, 0.5, 1, "cm")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 390)) +
  scale_x_continuous(expand = c(0, 0))
sample_years_los_histo
ggsave(paste0("length_of_stay_histo_overlay_", today(), ".png"), sample_years_los_histo)


# Figure 1F. box-and-whiskers plots comparing 2013, 2016, 2019 and 2022 length of stay faceted by top 4 diagnoses ###############################

sample_years_for_bwp <- nacrs_copc |>
  filter(Year %in% c(2013, 2016, 2019, 2022)) |>
  mutate(cacs_dx = fct_recode(cacs_dx,
                              "Infectious" = "infectious",
                              "Respiratory" = "resp",
                              "MSK/Injury" = "msk",
                              "Other (med refills, etc)" = "other_cacs_dx"))

nacrs_just_top_4 <- sample_years_for_bwp |>
  filter(cacs_dx %in% c("Other (med refills, etc)", "Infectious", "MSK/Injury", "Respiratory"))

nacrs_bwp_just_top_4_facet <- ggplot(nacrs_just_top_4, aes(x = as.factor(Year), y = length_of_stay_min, fill = Year)) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        plot.margin = margin(1, 1, 0.5, 1, "cm"),
        strip.text = element_text(size = 22),
        legend.position = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 410)) +
  labs(x = "Year", y = "Length of Stay (minutes)") +
  facet_wrap(~factor(cacs_dx, levels = c("Infectious", "Respiratory", "MSK/Injury", "Other (med refills, etc)")), scales = "free_x") +
  scale_fill_manual(values = c("2013" = "deeppink", "2016" = "cyan3", "2019" = "blue3", "2022" = "purple"))
nacrs_bwp_just_top_4_facet
ggsave(paste0("facets_selected_", today(), ".png"), nacrs_bwp_just_top_4_facet)


# Figure 2A. paired bar plot of case count vs year for head/neck and infectious diagnoses ###############################

infectious_cases <- nacrs_copc |>
  filter(cacs_dx == "infectious") |>
  group_by(Year) |>
  summarize(Number_of_Cases = n())

# Create a new data frame containing both sets of proportions
combined_count_data <- infectious_cases |>
  left_join(ent_cases, by = "Year") |>
  rename(Infectious_Cases = Number_of_Cases.x, ENT_Cases = Number_of_Cases.y) |>
  pivot_longer(cols = c(Infectious_Cases, ENT_Cases),
               names_to = "Diagnosis",
               values_to = "Absolute Count")

# Create a paired bar chart
paired_bar_chart_count <- ggplot(combined_count_data, aes(fill = Diagnosis, x = Year, y = `Absolute Count`)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Year", y = "Case Count") +
  scale_fill_manual(values = c("lightslateblue", "brown1"), name = "Diagnosis", labels = c("Head and Neck", "Infectious")) +
  ggtitle("Number of Cases by Year") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        plot.margin = margin(1, 1, 0.5, 1, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom", legend.title = element_text(size = 20), legend.text = element_text(size = 20)) +
  scale_y_continuous(limits = c(0, 4800), expand = expansion(mult = 0)) +
  guides(fill = guide_legend(title = "Diagnosis")) +
  scale_x_discrete(breaks = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"), labels = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"))
paired_bar_chart_count
ggsave(paste0("paired_bar_count_ent_infect", today(), ".png"), paired_bar_chart_count)


# Figure 2B. paired bar plot of proportion of cases vs year for head/neck and infectious diagnoses ###############################

infect_proportion <- nacrs_copc |>
  group_by(Year) |>
  summarize(Proportion_Infectious = sum(cacs_dx == "infectious") / n())

# Create a new data frame containing both sets of proportions
combined_proportions_data <- infect_proportion |>
  left_join(ent_proportion, by = "Year") |>
  pivot_longer(cols = c(Proportion_Infectious, Proportion_ENT),
               names_to = "Diagnosis",
               values_to = "Proportion")

# Create a paired bar chart
paired_bar_chart <- ggplot(combined_proportions_data, aes(fill = Diagnosis, x = Year, y = Proportion)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Year", y = "Proportion") +
  scale_fill_manual(values = c("lightslateblue", "brown1"), name = "Diagnosis", labels = c("Head and Neck", "Infectious")) +
  ggtitle("Proportion of Total Cases") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        plot.margin = margin(1, 1, 0.5, 1, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom", legend.title = element_text(size = 20), legend.text = element_text(size = 20)) +
  scale_y_continuous(limits = c(0, 0.35), expand = expansion(mult = 0)) +
  # coord_flip() +
  guides(fill = guide_legend(title = "Diagnosis")) +
  scale_x_discrete(breaks = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"), labels = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"))
paired_bar_chart
ggsave(paste0("paired_bar_chart_ent_infect", today(), ".png"), paired_bar_chart)


# Figure 2C. bar plot primary care access vs year - absolute case counts ###############################

nacrs_copc_factors <- nacrs_copc |>
  select(gender, primary_care_access, region, ctas, day_of_week, referred_from, cacs_dx, intervention, disposition, disposition_service, Year) |>
  filter(!is.na(ctas)) |>
  filter(!is.na(cacs_dx)) |>
  droplevels()

pcp_vs_year_plot_data <- nacrs_copc_factors[, c("primary_care_access", "Year"), drop = FALSE]

pcp_vs_year_plot_data_labeled <- pcp_vs_year_plot_data |>
  filter(!is.na(primary_care_access)) |>
  filter(!Year %in% c("2006", "2007", "2008", "2009")) |>
  droplevels() |>
  rename_with(~ "primary_care_access", 1) |>
  rename_with(~ "Year", 2) |>
  mutate(primary_care_access = fct_recode(primary_care_access,
                                          "No Primary Care" = "no_or_unknown_pcp_access",
                                          "Has Primary Care" = "has_pcp_access"))

pcp_vs_year_plot_data_labeled$primary_care_access <- fct_rev(pcp_vs_year_plot_data_labeled$primary_care_access)

filtered_data <- pcp_vs_year_plot_data_labeled |>
  filter(primary_care_access == "No Primary Care")

y_upper_limit <- 1900

pcp_vs_year_bar_plot <- ggplot(filtered_data) +
  geom_bar(aes(x = Year), fill = "royalblue1", position = "identity") +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 25),
        legend.position = "none") +
  labs(x = "Year", y = "Number of Visits without Primary Care") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  coord_cartesian(ylim = c(0, y_upper_limit))
pcp_vs_year_bar_plot
ggsave(paste0("pcp_vs_year_bar_plot_", today(), ".png"), pcp_vs_year_bar_plot, width = 8, height = 8, dpi = 600)


# Figure 2D. bar plot primary care access vs year - percent ###############################

prop_w_no_pcp <- pcp_vs_year_plot_data_labeled |>
  group_by(Year) |>
  summarise(Proportion = (sum(primary_care_access == "No Primary Care") / n())*100)

# Create the bar plot
prop_w_no_pcp_bar_plot <- ggplot(prop_w_no_pcp, aes(x = as.factor(Year), y = Proportion, fill = "No Primary Care")) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = "purple") +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 25),
        legend.position = "none") +
  labs(x = "Year", y = "Percent of Visits without Primary Care") +
  scale_y_continuous(expand = expansion(mult = 0)) +
  coord_cartesian(ylim = c(0, 14))
prop_w_no_pcp_bar_plot
ggsave(paste0("prop_w_no_pcp_bar_plot_", today(), ".png"), prop_w_no_pcp_bar_plot, width = 8, height = 8, dpi = 600)


# Figure 3A. bar plot CTAS3 cases vs year - absolute case counts ###############################

ctas3_cases <- nacrs_copc |>
  filter(ctas == "3") |>
  group_by(Year) |>
  summarize(Number_of_Cases = n())

ctas3_cases_plot <- ggplot(ctas3_cases, aes(y = Number_of_Cases, x = Year)) +
  geom_bar(stat = "identity", fill = "royalblue1") +
  labs(y = "Number of CTAS 3 Cases", x = "Year") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"), labels = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022")) +
  scale_y_continuous(limits = c(0, 3500), expand = expansion(mult = 0))
ctas3_cases_plot
ggsave(paste0("ctas3_cases", today(), ".png"), ctas3_cases_plot)


# Figure 3B. bar plot CTAS3 cases vs year - proportion ###############################

ctas3_proportion <- nacrs_copc |>
  group_by(Year) |>
  summarize(Proportion_CTAS3 = sum(ctas == "3") / n())

ctas3_proportion_plot <- ggplot(ctas3_proportion, aes(y = Proportion_CTAS3, x = Year)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(y = "Proportion of CTAS 3 Cases", x = "Year") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"), labels = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022")) +
  scale_y_continuous(limits = c(0, 0.27), expand = expansion(mult = 0))
ctas3_proportion_plot
ggsave(paste0("ctas3_proportion", today(), ".png"), ctas3_proportion_plot)


# Figure 3C. bar plot proportion without primary care by CTAS level ###############################

proportions_ctas_by_pcp_data <- nacrs_copc |>
  group_by(ctas) |>
  summarize(Proportion = sum(primary_care_access == "no_or_unknown_pcp_access") / n(),
            Patients = n())

proportions_ctas_by_pcp_plot <- ggplot(proportions_ctas_by_pcp_data, aes(x = as.factor(ctas), y = Proportion)) +
  geom_bar(stat = "identity", position = "identity", fill = "royalblue1") +
  labs(x = "CTAS Level", y = "Proportion without Primary Care") +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5")) +  # Set labels for CTAS levels
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24)) +
  scale_y_continuous(limits = c(0, 0.32), expand = expansion(mult = 0))
proportions_ctas_by_pcp_plot
ggsave(paste0("proportions_ctas_by_pcp_plot", today(), ".png"), proportions_ctas_by_pcp_plot)


# Figure 3D. bar plot proportion without primary care by diagnosis category ###############################

proportions_dx_by_pcp <- nacrs_copc |>
  group_by(cacs_dx) |>
  summarize(
    Proportion = sum(primary_care_access == "no_or_unknown_pcp_access") / n()) |>
  arrange(desc(Proportion))

proportions_dx_by_pcp$cacs_dx <- fct_reorder(proportions_dx_by_pcp$cacs_dx, proportions_dx_by_pcp$Proportion, .desc = TRUE)

custom_labels <- c("Fluid/Renal", "Other/Nonurgent", "Mental Health", "Heme/Onc", "Head and Neck", "Derm", "Uro/gyne", "GI/Liver", "MSK", "Resp", "Neuro", "Infectious", "LWOBS", "Cardiovascular")

proportions_dx_by_pcp_plot <- ggplot(proportions_dx_by_pcp, aes(x = as.factor(cacs_dx), y = Proportion)) +
  geom_bar(stat = "identity", position = "identity", fill = "purple") +
  labs(x = "Diagnosis", y = "Proportion without Primary Care") +
  scale_x_discrete(labels = custom_labels) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 0.32), expand = expansion(mult = 0))
proportions_dx_by_pcp_plot
ggsave(paste0("proportions_dx_by_pcp_plot", today(), ".png"), proportions_dx_by_pcp_plot)


# Machine Learning ####

tidymodels_prefer()
set.seed(123)


# Suppl Figure 1A and B - Length of Stay Estimation and Logged Length of Stay####

los_histogram <- ggplot(nacrs_copc, aes(x = length_of_stay_min)) + 
  geom_histogram(bins = 50, col= "white") +
  theme_bw()
los_histogram
ggsave(paste0("los_histogram", today(), ".png"), los_histogram)
log_logged_histogram <- ggplot(nacrs_copc, aes(x = length_of_stay_min)) + 
  geom_histogram(bins = 50, col= "white") +
  scale_x_log10() +
  theme_bw()
log_logged_histogram
ggsave(paste0("log_logged_histogram", today(), ".png"), log_logged_histogram)

# thus, for ML with LOS as outcome, should be log transformed (recommended to be done outside of pipeline for tidymodels)

# at the same time we clean Data for LoS ML ####
nacrs_copc_for_ml <- nacrs_copc |>
  mutate(los_logged = log10(length_of_stay_min)) |>
  select(age_years, gender, region, primary_care_access, ctas, reg_minutes_since_0900, left_minutes_since_0900, cacs_dx, disposition, Year, los_logged) #chief_complaint is slightly more granular than cacx_dx (15 levels over 14 levels) but cc is missing 60k values so will use cacx_dx instead

copc_split <- initial_split(nacrs_copc_for_ml, prop = 0.80)
copc_split

copc_train <- training(copc_split)
copc_test  <-  testing(copc_split)


# LINEAR REGRESSION MODEL - LoS #####

# Pre-processing recipe
lm_recipe <- 
  recipe(los_logged ~ ., data = copc_train) |>
  step_dummy(all_nominal_predictors())

# Define a model specification
lm_model_spec <- 
  linear_reg() |>
  set_engine("lm")

# pipe in the model and set the formula
lm_workflow <- 
  workflow() |>
  add_model(lm_model_spec) |>
  add_recipe(lm_recipe)
lm_workflow

#fit the model to the training data
lm_fit <- fit(lm_workflow, copc_train)
lm_fit

# make predictions with the test data
copc_lm_result <- predict(lm_fit, copc_test)
copc_lm_result

# fit the final model based on the most recent workflow
final_lm_results <- last_fit(lm_workflow, copc_split)

# collect the evaluation metrics and predictions from the model
collect_metrics(final_lm_results, copc_split)
collect_predictions(final_lm_results)
lm_preds_df <- collect_predictions(final_lm_results)

# Supplementary Figure 2A - Histogram of Length of Stay LM Prediction ####

# unlogging the predictions and actual LOS values
lm_preds_df$predicted_linear <- 10^lm_preds_df$.pred
lm_preds_df$actual_linear <- 10^lm_preds_df$los_logged

lm_preds_histo <- ggplot(lm_preds_df, aes(x = actual_linear)) +
  geom_histogram(binwidth = 3, fill = "purple", alpha = 0.7) +
  geom_histogram(data = lm_preds_df, aes(x = predicted_linear), binwidth = 3, fill = "blue", alpha = 0.7) +
  labs(x = "Length of Stay (minutes)",
       y = "Count") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        plot.margin = margin(1, 1, 0.5, 1, "cm")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))
lm_preds_histo
ggsave(paste0("lm_preds_histo_", today(), ".png"), lm_preds_histo)

# Supplementary Figure 2B - Scatterplot of Length of Stay LM Prediction ####

lm_res_plot <- ggplot(lm_preds_df, aes(x = actual_linear, y = predicted_linear)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Length of Stay (min)", x = "Actual Length of Stay (min)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred() +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        plot.margin = margin(1, 1, 0.5, 1, "cm")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))
lm_res_plot
ggsave(paste0("lm_res_plot", today(), ".png"), lm_res_plot)


# RANDOM FOREST MODEL - LoS ####

rf_model <- 
  rand_forest(mode = "regression") |>
  set_engine("ranger", importance = "impurity")

nacrs_copc_rf_recipe <-
  recipe(los_logged ~ .,
         data = copc_train) |>
  step_dummy(all_nominal_predictors())

rf_workflow <- 
  workflow() |>
  add_model(rf_model) |>
  add_recipe(nacrs_copc_rf_recipe)
rf_workflow

# Running the fit and collecting the metrics without cross validation
rf_fit <- fit(rf_workflow, copc_train)
rf_fit

# make predictions with the test data
copc_rf_result <- predict(rf_fit, copc_test)
copc_rf_result

final_rf_results <- last_fit(rf_workflow, copc_split)

# collect the evaluation metrics and predictions from the model
collect_metrics(final_rf_results, copc_split)
rf_predictions <- collect_predictions(final_rf_results)

# now unlogging the predictions and actual LOS values
rf_predictions$predicted_linear <- 10^rf_predictions$.pred
rf_predictions$actual_linear <- 10^rf_predictions$los_logged


# Supplementary Figure 2C - Histogram of Length of Stay RF Prediction ####

rf_preds_histo <- ggplot(rf_predictions, aes(x = actual_linear)) +
  geom_histogram(binwidth = 3, fill = "purple", alpha = 0.7) +
  geom_histogram(data = rf_predictions, aes(x = predicted_linear), binwidth = 3, fill = "blue", alpha = 0.7) +
  labs(x = "Length of Stay (minutes)",
       y = "Count") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        plot.margin = margin(1, 1, 0.5, 1, "cm")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))
rf_preds_histo
ggsave(paste0("rf_preds_histo_", today(), ".png"), rf_preds_histo)


# Supplementary Figure 2D - Scatterplot of Length of Stay RF Prediction ####

rf_res_plot <- ggplot(rf_predictions, aes(x = actual_linear, y = predicted_linear)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Length of Stay (min)", x = "Actual Length of Stay (min)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred() +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        plot.margin = margin(1, 1, 0.5, 1, "cm")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))
rf_res_plot
ggsave(paste0("rf_res_plot_", today(), ".png"), rf_res_plot)


# Supplementary Figure 2E - Relative importance plot ####
importance_plot <- rf_fit |>
  extract_fit_parsnip() |>
  vip(num_features = 7)
importance_plot

custom_y_labels <- rev(c("Time of Day Discharged", "Time of Day Registered", "2022", "Age", "Discharged Home", "Head and Neck Presentation", "2021"))

importance_plot_clean <- importance_plot +
  geom_bar(stat = "identity", fill = "blue") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.margin = margin(1, 1, 0.5, 1, "cm")) +
  labs(x = "Variable", y = "Relative Importance") +
  scale_x_discrete(labels = custom_y_labels)
importance_plot_clean
ggsave(paste0("importance_plot_clean_", today(), ".png"), importance_plot_clean)



# 2022 Estimation ####################################################

# add binary classifier for Year
nacrs_copc_for_ml <- nacrs_copc |>
  mutate(year_binary = ifelse(Year == "2022", 1, 0)) |>
  mutate(year_binary = as.factor(year_binary)) |>
  select(age_years, gender, region, primary_care_access, ctas, reg_minutes_since_0900, length_of_stay_min, left_minutes_since_0900, cacs_dx, disposition, year_binary) #chief_complaint is slightly more granular than cacx_dx (15 levels over 14 levels) but cc is missing 60k values so will use cacx_dx instead

copc_split <- initial_split(nacrs_copc_for_ml, prop = 0.80)
copc_split

copc_train <- training(copc_split)
copc_test  <-  testing(copc_split)

# PENALIZED LOGISTIC REGRESSION MODEL - predicting 2022 ####

lr_model <- 
  logistic_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet")

lr_recipe <- 
  recipe(year_binary ~ ., data = nacrs_copc_for_ml) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_predictors())

lr_workflow <- 
  workflow() |>
  add_model(lr_model) |>
  add_recipe(lr_recipe)

# V folds cross validation ####
copc_vfolds <- vfold_cv(copc_train, v = 5, repeats = 5)
copc_vfolds

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

# create tuning grid
lr_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))
lr_grid |>
  top_n(-5) # lowest penalty values
lr_grid |>
  top_n(5)  # highest penalty values

lr_results <- 
  lr_workflow |>
  tune_grid(copc_vfolds,
            grid = lr_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))
lr_results

lr_plot <- 
  lr_results |>
  collect_metrics() |>
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())
lr_plot

top_models <-
  lr_results |>
  show_best("roc_auc", n = 15) |>
  arrange(penalty) 
top_models

lr_best <- 
  lr_results |> 
  collect_metrics() |>
  arrange(penalty) |>
  slice(6)
lr_best

lr_auc <- 
  lr_results |>
  collect_predictions(parameters = lr_best) |>
  roc_curve(year_binary, .pred_0) |>
  mutate(model = "Logistic Regression")
autoplot(lr_auc)


# RANDOM FOREST MODEL - predicting 2022 ####

rf_model <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |>
  set_engine("ranger", num.threads = cores) |>
  set_mode(mode = "classification")

rf_recipe <- 
  recipe(year_binary ~ ., data = nacrs_copc_for_ml) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_predictors())

rf_workflow <- 
  workflow() |>
  add_model(rf_model) |>
  add_recipe(rf_recipe)
rf_workflow

# V folds cross validation ####
copc_vfolds <- vfold_cv(copc_train, v = 5, repeats = 5)
copc_vfolds

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

rf_results <- 
  rf_workflow |>
  tune_grid(copc_vfolds,
            grid = 3,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))
rf_results

rf_results |>
  show_best(metric = "roc_auc") # will show top 5

rf_best <- 
  rf_results |> 
  select_best(metric = "roc_auc") # selects highest auc as final model
rf_best

assess_rf_results <- collect_predictions(rf_results)
assess_rf_results

# collect hyperparameters that went into the best model
rf_auc <- 
  rf_results |> 
  collect_predictions(parameters = rf_best) |> 
  roc_curve(children, .pred_children) |> 
  mutate(model = "Random Forest")

# recipe for subsequent model creation
copc_rec <-
  recipe(year_binary ~ ., data = nacrs_copc_for_ml) |>
  step_naomit(everything(), skip = TRUE) |>
  step_novel(all_nominal(), -all_outcomes()) |>
  step_normalize(all_numeric(), -all_outcomes()) |> 
  step_dummy(all_nominal(), -all_outcomes())
summary(copc_rec)

# XGBoost
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), copc_train),
  learn_rate(),
  size = 30
)

xgb_spec <- 
  boost_tree(trees = 1000,
             tree_depth = tune(),
             min_n = tune(),
             loss_reduction = tune(),
             sample_size = tune(),
             mtry = tune(),
             learn_rate = tune()) |> 
  set_engine("xgboost") |> 
  set_mode("classification")

xgb_wflow <-
  workflow() |>
  add_recipe(copc_rec) |> 
  add_model(xgb_spec)

xgb_res <- tune_grid(
  xgb_wflow,
  resamples = cv_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_res |> collect_metrics(summarize = TRUE)

xgb_res %>%
  collect_metrics() |>
  filter(.metric == "roc_auc") |>
  select(mean, mtry:sample_size) |>
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

show_best(xgb_res, "roc_auc")

best_auc <- select_best(xgb_res, "roc_auc")
best_auc

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)

final_xgb

final_res <- last_fit(final_xgb, copc_split)

collect_metrics(final_res)


# knn

knn_spec <- 
  nearest_neighbor(neighbors = tune(),
                   dist_power = tune(),
                   weight_func = "optimal") |>
  set_engine("kknn") |> 
  set_mode("classification") 

knn_wflow <-
  workflow() |>
  add_recipe(copc_rec) |> 
  add_model(knn_spec)

knn_res <- tune_grid(
  knn_wflow,
  resamples = cv_folds, 
  grid = knn_grid,
  control = control_resamples(save_pred = TRUE)) 

knn_res |> collect_metrics(summarize = TRUE)

knn_res |>
  collect_metrics() |>
  filter(.metric == "roc_auc") |>
  select(mean, neighbors:sample_size) |>
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

show_best(knn_res, "roc_auc")

best_auc <- select_best(knn_res, "roc_auc")
best_auc

final_knn <- finalize_workflow(
  knn_wf,
  best_auc
)

final_knn

final_res <- last_fit(final_knn, copc_split)

collect_metrics(final_res)

# nn
install_keras()
install_tensorflow()
tf_config()

#tuned using iterative 

nnet_spec <-
  mlp(hidden_units = tune()) |>
  set_mode("classification") |> 
  set_engine("keras", verbose = 0)

nnet_param <- 
  nnet_spec %>% 
  extract_parameter_set_dials() %>% 
  update(hidden_units = hidden_units(c(1, 27)))

nnet_wflow <-
  workflow() |>
  add_recipe(copc_rec) |> 
  add_model(nnet_spec)

nnet_res <- tune_grid(
  nnet_wflow, 
  resamples = cv_folds,
  grid = nnet_param,
  control = control_resamples(save_pred = TRUE)) 

nnet_res |> collect_metrics(summarize = TRUE)

nnet_res |>
  collect_metrics() |>
  filter(.metric == "roc_auc") |>
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

show_best(nnet_res, "roc_auc")

best_auc <- select_best(nnet_res, "roc_auc")
best_auc

final_nnet <- finalize_workflow(
  nnet_wflow,
  best_auc
)

final_nnet

final_res <- last_fit(final_nnet, copc_split)

collect_metrics(final_res)

nnet_res |> collect_metrics(summarize = TRUE)


# Compare Metrics

log_metrics <- 
  log_res |> 
  collect_metrics(summarise = TRUE) |>
  mutate(model = "Logistic Regression") # add the name of the model to every row

rf_metrics <- 
  rf_res |> 
  collect_metrics(summarise = TRUE) |>
  mutate(model = "Random Forest")

xgb_metrics <- 
  xgb_res |> 
  collect_metrics(summarise = TRUE) |>
  mutate(model = "XGBoost")

knn_metrics <- 
  knn_res |> 
  collect_metrics(summarise = TRUE) |>
  mutate(model = "Knn")

nnet_metrics <-
  nnet_res |>
  collect_metrics(summarise = TRUE) |>
  mutate(model = "Neural Net")

# create dataframe with all models
model_compare <- bind_rows(
  log_metrics,
  rf_metrics,
  xgb_metrics,
  knn_metrics,
  nnet_metrics
) 

# change data structure for plotting
model_comp <- 
  model_compare |> 
  select(model, .metric, mean, std_err) |> 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

# Figure 4A - mean AUC for each model ########

# show mean area under the curve (auc) per model
model_comp_auc <- model_comp |> 
  arrange(mean_roc_auc) |> 
  mutate(model = fct_reorder(model, mean_roc_auc)) |>
  ggplot(aes(model, mean_roc_auc, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") + 
  geom_text(
    size = 3,
    aes(label = round(mean_roc_auc, 2), y = mean_roc_auc + 0.08),
    vjust = 1
  )
model_comp_auc
ggsave(paste0("five_algorithm_auc_", today(), ".png"), model_comp_auc)

# Figure 4B - F1 score for each model ########

# show F1-Score for every model
model_comp_f1 <- model_comp |> 
  arrange(mean_f_meas) |> 
  mutate(model = fct_reorder(model, mean_f_meas)) |> # order results
  ggplot(aes(model, mean_f_meas, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    size = 3,
    aes(label = round(mean_f_meas, 2), y = mean_f_meas + 0.08),
    vjust = 1
  )
model_comp_f1
ggsave(paste0("five_algorithm_F1_", today(), ".png"), model_comp_f1)


# Figure 4C - relative importance plot from RF model ########
custom_y_labels <- rev(c("Length of Stay", "Time of Day Discharged", "Time of Day Registered", "Diagnosis", "Age"))

rf_importance <- last_fit_rf |> 
  pluck(".workflow", 1) |>   
  extract_fit_parsnip() |> 
  vip(num_features = 5) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.margin = margin(1, 1, 0.5, 1, "cm")) +
  theme(text = element_text(size = 26)) +
  scale_x_discrete(labels = custom_y_labels)
rf_importance
ggsave(paste0("rf_relative_import_", today(), ".png"), rf_importance,  width = 8.5, height = 7.2, units = "in", dpi = 300)

