russia_gold

library(countrycode)
library(treemapify)
library(ggfittext)


iso_to_unicode_flag <- function(iso2c) {
  sapply(iso2c, function(code) {
    if (is.na(code)) return(NA)
    paste0(
      intToUtf8(127462L + which(LETTERS == substr(code, 1, 1)) - 1L),
      intToUtf8(127462L + which(LETTERS == substr(code, 2, 2)) - 1L)
    )
  })
}

year_select = 2023

transfers_cleant_reemap_data <-
  russia_gold |>
  mutate(year = year(period)) |> 
  filter(year == year_select) |> 
  group_by(partner_iso, estimate) |> 
  summarise(netweight_kg = sum(netweight_kg)) |> 
  mutate(
    country = countrycode(partner_iso, origin = 'iso3c', destination = 'country.name')
  ) |> 
  ungroup() |> 
  mutate(
    pct = netweight_kg / sum(netweight_kg, na.rm = TRUE),
    pct = percent(pct, accuracy = 0.1),
    iso2c = countrycode(country, origin = "country.name", destination = "iso2c"),
    flag_unicode = iso_to_unicode_flag(iso2c),
    flag_unicode = ifelse(is.na(flag_unicode), "🇺🇳", flag_unicode),
    continent = countrycode(country, origin = "country.name", destination = "continent"),
    continent = ifelse(!continent %in% c("Americas", "Asia", "Europe"), "Asia", continent),
    # country = countrycode(country, origin = "country.name", destination = "cldr.short.hy"),
    # country = ifelse(is.na(country), "Այլ պետություններ", country),
    country = ifelse(!continent %in% c("Americas", "Asia", "Europe"), "Other countries", country),
    # country = ifelse(grepl("Էմիրություններ", country), "Արաբական Միացյալ\nԷմիրություններ", country)
  ) |> 
  arrange(desc(netweight_kg)) |> 
  mutate(country = factor(country, levels = c(country[country != "Other countries"], "Other countries")))


total <- 
  transfers_cleant_reemap_data |> 
  summarise(netweight_kg = sum(netweight_kg / 1e9, na.rm = TRUE)) |> 
  pull(netweight_kg) |> 
  number(accuracy = 0.1)

# transfers_cleant_reemap_data |> view()
# countrycode::codelist |> select(contains("hy"))

transfers_cleant_reemap_data |> 
  ggplot(aes(
    area = netweight_kg, fill = continent, 
    label = paste0(flag_unicode, "\n", country, "\n", pct, " (", number(netweight_kg/1e9, accuracy = 0.01), ")")
  )) +
  geom_treemap(layout = "squarified", start = "topleft") +
  geom_treemap_text(
    place = "centre", 
    start = "topleft",
    size = 14, 
    color = "white",
    reflow = TRUE,
  ) +
  theme_tvyal() +
  scale_fill_manual(
    values = new_palette_colors[c(8,2,6)],
    labels = c("Americas", "Asia", "Europe")
  ) +
  labs(
    # fill = "Սշխարհամաս`",
    title = paste("Russia crude oil exports in", year_select),
    subtitle = paste0("Total: ", total, " million tons", ifelse(year_select >=2022, ", data is estimated", "")),
    caption = "Aghasi Tavdyan | Data Source: UN Comtrade."
  )

ggsave(paste0("plots/russia_oil_exports_", year_select,".png"), ggplot2::last_plot(), height = 7, width = 10)
ggsave(paste0("plots/russia_oil_exports_", year_select,".svg"), ggplot2::last_plot(), height = 7, width = 10)
