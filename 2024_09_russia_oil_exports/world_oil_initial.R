library(tidyverse)
library(scales)
library(countrycode)

source("~/R/newsletter/initial_setup.R")

Sys.setlocale("LC_TIME", "hy_AM.UTF-8")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#######################################

normalize_trade_data <- function(tbl) {
  # Convert all column names to lowercase
  names(tbl) <- tolower(names(tbl))
  
  # Function to safely get column data and convert type
  safe_get_column <- function(pattern, convert_to_character = FALSE) {
    col_name <- grep(pattern, names(tbl), ignore.case = TRUE, value = TRUE)
    if (length(col_name) == 0) {
      warning(paste("Column matching", pattern, "not found"))
      return(NA)
    }
    # Handle encoding issues and convert to character if needed
    col_data <- tbl[[col_name]]
    if (is.character(col_data)) {
      col_data <- iconv(col_data, "latin1", "UTF-8", sub = "")
    }
    if (convert_to_character) {
      col_data <- as.character(col_data)
    }
    return(col_data)
  }
  
  tbl <- tbl |> 
    transmute(
      year = safe_get_column("^refyear$"),
      month = safe_get_column("^refmonth$"),
      period = ymd(safe_get_column("^refperiodid$")),
      period_desc = safe_get_column("^period$"),
      aggregate_level = safe_get_column("^aggrlevel$"),
      is_leaf_code = safe_get_column("^isleaf$", convert_to_character = TRUE),
      trade_code = safe_get_column("^flowcode$"),
      trade_flow = safe_get_column("^flowdesc$"),
      reporter_iso = safe_get_column("^reporteriso$"),
      reporter = safe_get_column("^reporterdesc$"),
      partner_iso = safe_get_column("^partneriso$"),
      partner = safe_get_column("^partnerdesc$"),
      commodity_code = as.numeric(safe_get_column("^cmdcode$")),
      commodity = safe_get_column("^cmddesc$"),
      mot_code = safe_get_column("^motcode$"),
      mot_desc = safe_get_column("^motdesc$"),
      QtyUnitCode = safe_get_column("^qtyunitcode$"),
      QtyUnitAbbr = safe_get_column("^qtyunitabbr$"),
      Qty = safe_get_column("^qty$"),
      IsQtyEstimated = safe_get_column("^isqtyestimated$", convert_to_character = TRUE),
      IsNetWgtEstimated = safe_get_column("^isnetwgtestimated$", convert_to_character = TRUE),
      netweight_kg = safe_get_column("^netwgt$"),
      trade_value_us = safe_get_column("^primaryvalue$"),
      cifvalue = safe_get_column("^cifvalue$"),
      fobvalue = safe_get_column("^fobvalue$"),
      IsReported = safe_get_column("^isreported$", convert_to_character = TRUE),
      IsAggregate = safe_get_column("^isaggregate$", convert_to_character = TRUE)
    )
  
  # Filter and mutate operations
  tbl <- tbl |> 
    filter(
      !is.na(commodity_code),
      tolower(partner) != "world",
      tolower(reporter) != "world"
    ) |> 
    mutate(
      reporter = countrycode(reporter_iso, origin = "iso3c", destination = "country.name", warn = FALSE),
      partner = countrycode(partner_iso, origin = "iso3c", destination = "country.name", warn = FALSE),
      continent = countrycode(reporter_iso, origin = "iso3c", destination = "continent", warn = FALSE)
    )
  
  return(tbl)
}

directory <- "~/R/Gcapatker/2024_05_13_world_gold_exports"

# world_oil <- read_csv(file.path(directory, "7108_oil_data_annual_2024_05.csv")) |> normalize_trade_data()
# world_petrol <- read_csv(file.path(directory,"2709_petrol_data_annual_2024_05.csv")) |> normalize_trade_data()

# world_oil <- world_petrol

world_oil_monthly <-
  tibble(
    files = list.files("~/R/Gcapatker/2024_05_13_world_gold_exports", full.names = TRUE)
  ) |> 
  # filter(grepl("monthly", files) & grepl("7108", files)) |> 
  
  filter(grepl("monthly", files) & grepl("2709", files)) |> 
  mutate(
    data = map(files, read_csv),
    data = map(data, normalize_trade_data)
  ) |> 
  select(-files) |> 
  unnest(data)

world_oil_monthly |> write_csv("7108_world_oil_data_monthly.csv")

world_oil_monthly |> 
  filter(reporter_iso == "CHE", partner_iso == "RUS") |> 
  view()

world_oil_monthly |> 
  filter(reporter_iso == "RUS") |> 
  count(period) |> 
  view()

library(countrycode)

# tibble(list = countrycode::codelist |> colnames()) |> filter(grepl("hy", list))



EU = c(
  "Austria", "Italy", "Belgium", "Latvia", "Bulgaria", "Lithuania", "Croatia",
  "Luxembourg", "Cyprus", "Malta", "Czech Rep.", "Netherlands", "Denmark",
  "Poland","Estonia", "Portugal", "Finland", "Romania", "France", "Slovakia",
  "Germany", "Slovenia", "Greece", "Spain", "Hungary", "Sweden", "Ireland"
)

EAEU = c("Russian Federation", "Kazakhstan", "Belarus", "Armenia", "Kyrgyzstan")

Middle_East = c(
  "Bahrain", "Egypt", "Iran", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon",
  "Oman", "Qatar", "Saudi Arabia", "Syria", "Turkey", "United Arab Emirates", "Yemen"
)

China = c("China", "Hong Kong SAR")

russia_oil <- 
  bind_rows(
    world_oil_monthly |> 
      filter(reporter_iso == "RUS", trade_code == "X", period < ymd("2022-01-01")) |> 
      select(period, netweight_kg, partner, partner_iso) |> 
      mutate(estimate = FALSE),
    world_oil_monthly |> 
      filter(partner_iso == "RUS", trade_code == "M", period >= ymd("2022-01-01")) |> 
      transmute(period, netweight_kg, partner = reporter, partner_iso = reporter_iso, estimate = TRUE)
  ) |> 
  left_join(
    countrycode::codelist |> transmute(partner_iso = iso3c, partner_arm = cldr.name.hy),
    by = join_by(partner_iso)
  ) |> 
  mutate(
    period = period + days(15),
    partner = case_when(
      partner %in% EU ~ "European Union",
      # partner %in% Middle_East ~ "Middle_East",
      partner %in% China ~ "Greater China",
      partner == "India" ~ "India",
      TRUE ~ "Other countries"
    ),
    # partner = fct_lump_n(partner, 8, other_level = "Other countries"),
    # partner = as.character(partner),
    # partner = ifelse(partner_iso == "IND", "India", partner),
    # partner = fct_reorder(partner, netweight_kg, .na_rm = TRUE, .desc = TRUE),
    # partner = fct_relevel(partner, "Other countries", after = Inf),
    # partner_arm = fct_lump_n(partner_arm, 8, other_level =  "Այլ պետություններ"),
    # partner_arm = as.character(partner_arm),
    # partner_arm = ifelse(partner_iso == "IND", "Հնդկաստան", partner),
    # partner_arm = fct_reorder(partner_arm, netweight_kg, .na_rm = TRUE, .desc = TRUE),
    # partner_arm = fct_relevel(partner_arm, "Այլ պետություններ", after = Inf)
  ) |> 
  filter(period >= ymd("2019-01-01"))
  

russia_oil |>  
  filter(period <= ymd("2023-12-31")) |>
  ggplot(aes(period, netweight_kg / 1e9, fill = partner)) +
  geom_col() +
  geom_vline(xintercept = ymd("2022-01-01"), color = new_palette_colors[1], linetype = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_manual(values = new_palette_colors[c(2,4,6,8)]) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Russian Oil Exports",
    subtitle = "Monthly Volume in Million Tons (HS Code: 2709 - Crude Petroleum Oils and Oils from Bituminous Minerals)",
    caption = "Aghasi Tavdyan | Data Source: UN Comtrade | Estimates for 2022 and beyond are provided by the author."
  )

ggsave("plots/russia_oil_exports_1.png", ggplot2::last_plot(), height = 7, width = 10)
ggsave("plots/russia_oil_exports_1.svg", ggplot2::last_plot(), height = 7, width = 10)


year_position_f <- function(year, start_year = 2019) {
  ifelse(
    !grepl("Q", year),
    as.numeric(year) - start_year + 1,
    as.numeric(sub("^(\\d+).*", "\\1", year)) - start_year + 2.5
  )
}


russia_oil |> 
  mutate(
    year = year(period),
    quarter = quarter(period),
    # year = ifelse(year == 2024 & quarter == 1, "2024 Q1", as.character(year)),
  ) %>%
  filter(year <= 2023) |> 
  # bind_rows(
  #   filter(., year == "2023", quarter == 1) |> 
  #     mutate(year = "2023 Q1")
  # ) |> 
  arrange(period, year) |> 
  group_by(year, partner) |> 
  summarise(netweight_kg = sum(netweight_kg, na.rm = TRUE), .groups = "drop") |> 
  group_by(year) |> 
  mutate(
    netweight_tn = netweight_kg / 1e9,
    pct = netweight_tn / sum(netweight_tn),
    netweight_tn_text = number(netweight_tn, accuracy = 0.1),
    pct_text = paste0(netweight_tn_text, " | ", scales::percent(pct, accuracy = 0.1)),
    pct_text = ifelse(netweight_tn >= 6, pct_text, NA),
    year_position = year_position_f(year)
  ) |> 
  ggplot(aes(x = year_position, y = netweight_tn, fill = partner, label = pct_text)) +
  geom_col(width = 0.9) +
  geom_text(position = position_stack(vjust = 0.5)) +
  geom_vline(xintercept = 3.5, color = new_palette_colors[1], linetype = 2) +
  scale_fill_manual(values = new_palette_colors[c(2,4,6,8)]) +
  scale_x_continuous(
    breaks = year_position_f(c("2019", "2020", "2021", "2022", "2023")),
    labels = c("2019", "2020", "2021", "2022", "2023"),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Russian Oil Exports",
    subtitle = "Annual Volume in Million Tons (HS Code: 2709 - Crude Petroleum Oils and Oils from Bituminous Minerals)",
    caption = "Aghasi Tavdyan | Data Source: UN Comtrade | Estimates for 2022 and beyond are provided by the author."
  ) +
  theme(
    panel.grid.major.x = element_blank()
  )


ggsave("plots/russia_oil_exports_2.png", ggplot2::last_plot(), height = 7, width = 10)
ggsave("plots/russia_oil_exports_2.svg", ggplot2::last_plot(), height = 7, width = 10)


###################################################

















# Step 1: Determine the last available period for each country
last_periods <- world_oil_monthly %>% 
  group_by(reporter_iso) %>% 
  summarize(last_period = max(period)) %>%
  ungroup()






estimated_oil_data <- 
  bind_rows(
    
    world_oil_monthly |> 
      inner_join(last_periods, by = join_by(reporter_iso)) |> 
      filter(trade_code %in% c("X", "M"), period < last_period) |> 
      select(
        period, reporter, reporter_iso,  partner, partner_iso, 
        trade_code, trade_value_us, netweight_kg, 
      ) |> 
      mutate(estimate = FALSE),
    
    world_oil_monthly |> 
      inner_join(last_periods, by = c("partner_iso" = "reporter_iso")) |> 
      filter(trade_code %in% c("X", "M"), period >= last_period) |> 
      rename(
        reporter = partner,
        reporter_iso = partner_iso,
        partner = reporter,
        partner_iso = reporter_iso
      ) |> 
      transmute(
        period,
        reporter,
        reporter_iso,
        partner,
        partner_iso,
        trade_code = ifelse(trade_code == "M", "X", "M"),
        trade_value_us,
        netweight_kg,
        estimate = TRUE
      )
  )



# 
# estimated_oil_data |> 
#   filter(reporter_iso == "RUS", trade_code == "X", !is.na(netweight_kg), !is.na(partner), period >= ymd("2019-01-01")) |> 
#   
#   left_join(
#     countrycode::codelist |> transmute(partner_iso = iso3c, partner_arm = cldr.name.hy),
#     by = join_by(partner_iso)
#   ) |> 
#   mutate(
#     period = period + days(15),
#     partner = fct_lump_n(partner, 8, other_level = "Other countries"),
#     # partner = fct_reorder(partner, netweight_kg, .na_rm = TRUE, .desc = TRUE),
#     # partner = fct_relevel(partner, "Other countries", after = Inf),
#     partner_arm = fct_lump_n(partner_arm, 8, other_level =  "Այլ պետություններ"),
#     # partner_arm = fct_reorder(partner_arm, netweight_kg, .na_rm = TRUE, .desc = TRUE),
#     # partner_arm = fct_relevel(partner_arm, "Այլ պետություններ", after = Inf)
#   ) |> 
#   ggplot(aes(period, netweight_kg / 1000, fill = partner_arm)) +
#   geom_col() +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   scale_fill_manual(values = colfunc3(9)) +
#   labs(
#     x = NULL,
#     y = NULL,
#     fill = NULL,
#     title = "Հայաստանից ոսկու արտահանումը",
#     subtitle = "տոննա, ամսական կտրվածքով, ԱՏԳ ԱԱ 7108. Ոսկի անմշակ կամ կիսամշակ, փոշի",
#     caption = caption_f(source = "UN comtrade")
#   )

india_oil <- 
  estimated_oil_data |> 
  filter(
    reporter_iso == "IND",
    # trade_code == "X", 
    # !is.na(netweight_kg), 
    # !is.na(partner), 
    period >= ymd("2019-01-01"),
    period <= ymd("2024-01-01")
  )

india_oil |> filter(trade_code == "M") |> 
  mutate(
    year = year(period),
    partner = fct_lump_n(partner, 4, other_level = "Other countries"),
    partner = as.character(partner),
    partner = ifelse(partner_iso == "RUS", "Russia", partner),
  ) |> 
  group_by(period, partner) |> 
  summarise(netweight_kg = sum(netweight_kg) / 1e9) |> 
  ggplot(aes(period, netweight_kg, fill = partner)) +
  geom_col() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_manual(values = new_palette_colors[c(1,3,5,6,7,8)]) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Oil Exports to India",
    subtitle = "Monthly Volume in Million Tons (HS Code: 2709 - Crude Petroleum Oils and Oils from Bituminous Minerals)",
    caption = "Aghasi Tavdyan | Data Source: UN Comtrade."
  )

ggsave("plots/india_oil_imports_1.png", ggplot2::last_plot(), height = 7, width = 10)
ggsave("plots/india_oil_imports_1.svg", ggplot2::last_plot(), height = 7, width = 10)





india_oil |> 
  mutate(
    year = year(period),
    quarter = quarter(period),
    # year = ifelse(year == 2024 & quarter == 1, "2024 Q1", as.character(year)),
  ) %>%
  filter(year <= 2023) |> 
  # bind_rows(
  #   filter(., year == "2023", quarter == 1) |> 
  #     mutate(year = "2023 Q1")
  # ) |> 
  mutate(
    partner = fct_lump_n(partner, 4, other_level = "Other countries"),
    partner = as.character(partner),
    partner = ifelse(partner_iso == "RUS", "Russia", partner),
  ) |> 
  arrange(period, year) |> 
  group_by(year, partner) |> 
  summarise(netweight_kg = sum(netweight_kg, na.rm = TRUE), .groups = "drop") |> 
  group_by(year) |> 
  mutate(
    netweight_tn = netweight_kg / 1e9,
    pct = netweight_tn / sum(netweight_tn),
    netweight_tn_text = number(netweight_tn, accuracy = 0.1),
    pct_text = paste0(netweight_tn_text, " | ", scales::percent(pct, accuracy = 0.1)),
    pct_text = ifelse(netweight_tn >= 6, pct_text, NA),
    year_position = year_position_f(year)
  ) |> 
  ggplot(aes(x = year_position, y = netweight_tn, fill = partner, label = pct_text)) +
  geom_col(width = 0.9) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = new_palette_colors[c(1,3,5,6,7,8)]) +
  scale_x_continuous(
    breaks = year_position_f(c("2019", "2020", "2021", "2022", "2023")),
    labels = c("2019", "2020", "2021", "2022", "2023"),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Oil Exports to India",
    subtitle = "Annual Volume in Million Tons (HS Code: 2709 - Crude Petroleum Oils and Oils from Bituminous Minerals)",
    caption = "Aghasi Tavdyan | Data Source: UN Comtrade."
  ) +
  theme(
    panel.grid.major.x = element_blank()
  )




ggsave("plots/india_oil_imports_2.png", ggplot2::last_plot(), height = 7, width = 10)
ggsave("plots/india_oil_imports_2.svg", ggplot2::last_plot(), height = 7, width = 10)



  