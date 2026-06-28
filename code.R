# Packages ------------------------------
load_packages <- function(packages) {
  
  missing_packages <- packages[
    !packages %in% rownames(installed.packages())
  ]
  
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, dependencies = TRUE)
  }
  
  invisible(
    lapply(
      packages,
      library,
      character.only = TRUE
    )
  )
}

packages <- c(
  "readxl",
  "tidyverse",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "patchwork",
  "concaveman",
  "car",
  "brms",
  "tidybayes",
  "ggdist",
  "viridis",
  "lwgeom",
  "ggspatial",
  "nlme",
  "iNEXT",
  "purrr",
  "ggrepel"
)

load_packages(packages)

# Data ------------------------------
species = read_excel("species_info.xlsx")
occ = read_excel("coord_all.xlsx", sheet = "coord")

# Occurrence counts ---------------------------
head(occ$lat)
head(occ$lat)

occ %>%
  summarise(
    n_total = n(),
    n_lat_NA = sum(is.na(lat)),
    n_long_NA = sum(is.na(long)),
    n_any_coord_NA = sum(is.na(lat) | is.na(long))
  )

summary_occ = occ %>%
  distinct(Species, lat, long) %>%
  count(Species, name = "n_occurrences") %>%
  summarise(
    n_species = n(),
    min_occurrences = min(n_occurrences),
    median_occurrences = median(n_occurrences),
    mean_occurrences = mean(n_occurrences),
    max_occurrences = max(n_occurrences)
  ) 

occ_sf <- occ %>%
  distinct(Species, lat, long) %>%
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326,
    remove = FALSE
  )

n_distinct(occ_sf$Species) 

# Figure 2A -------------------------------------
species$status = as.factor(species$status)
levels(species$status)
head(species$year_description)

summary(species$year_description) # 1812 - 2025

# 545 species, 494 valid, 51 synonyms
species %>%
  count(status) %>%
  bind_rows(
    summarise(., status = "total", n = sum(n))
  )

valid_rates <- species %>%
  filter(
    status == "valid",
    !is.na(year_description)
  ) %>%
  mutate(
    year_description = as.integer(year_description),
    decade = floor(year_description / 10) * 10
  )

overall_rate <- valid_rates %>%
  summarise(
    first_year = min(year_description),
    last_year = max(year_description),
    n_species = n(),
    mean_species_per_year =
      n_species / (last_year - first_year + 1)
  )

overall_rate

decadal_rates <- valid_rates %>%
  count(decade, name = "n_species") %>%
  mutate(
    species_per_year = n_species / 10
  )

decadal_rates


accum_status <- species %>%
  filter(
    !is.na(year_description),
    status %in% c("valid", "synonym")
  ) %>%
  mutate(
    year_description = as.integer(year_description),
    status = factor(status, levels = c("valid", "synonym"))
  ) %>%
  count(year_description, status, name = "n_described") %>%
  complete(
    year_description = full_seq(year_description, 1),
    status,
    fill = list(n_described = 0)
  ) %>%
  arrange(status, year_description) %>%
  group_by(status) %>%
  mutate(
    cumulative_n = cumsum(n_described)
  ) %>%
  ungroup()

last_points <- accum_status %>%
  group_by(status) %>%
  slice_max(year_description, n = 1) %>%
  ungroup() %>%
  mutate(
    label = case_when(
      status == "valid" ~ "Valid",
      status == "synonym" ~ "Synonyms"
    )
  )

fig2_A <- ggplot(
  accum_status,
  aes(
    x = year_description,
    y = cumulative_n,
    color = status,
    linetype = status
  )
) +
  geom_line(linewidth = 1.3) +
  geom_text_repel(
    data = last_points,
    aes(label = label),
    nudge_x = 1.5,
    nudge_y = -11,
    hjust = 0,
    direction = "y",
    segment.color = NA,
    show.legend = FALSE,
    size = 4.5,
    fontface = "bold"
  )+
  scale_color_manual(
    values = c(
      valid = "black",
      synonym = "grey55"
    )
  ) +
  scale_linetype_manual(
    values = c(
      valid = "solid",
      synonym = "22"
    )
  ) +
  scale_x_continuous(
    limits = c(1820, 2035),
    breaks = seq(1825, 2025, by = 50)
  ) +
  labs(
    x = "Year of description",
    y = "Cumulative number of species"
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black")
  )

fig2_A

# Figure 2B -----------------------------------------
fig2_B_data <- accum_status %>%
  select(year_description, status, cumulative_n) %>%
  pivot_wider(
    names_from = status,
    values_from = cumulative_n
  ) %>%
  mutate(
    synonym_percent_over_valid = (synonym / valid) * 100
  )

last_point_B <- fig2_B_data %>%
  slice_max(year_description, n = 1)

fig2_B <- ggplot(
  fig2_B_data,
  aes(
    x = year_description,
    y = synonym_percent_over_valid
  )
) +
  geom_area(
    fill = "grey85",
    alpha = 0.75
  ) +
  geom_line(
    color = "black",
    linewidth = 1.25
  ) +
  geom_point(
    data = last_point_B,
    size = 3,
    color = "black"
  ) +
  scale_x_continuous(
    limits = c(1820, 2025),
    breaks = seq(1825, 2025, by = 50)
  ) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Year of description",
    y = "Synonyms relative to valid species (%)"
  ) +
  theme_classic(base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black")
  )

fig2_B


# Figure 2 Complete -------------------------------------
fig2 <- fig2_A + fig2_B +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "a")

fig2

fig2 <- fig2 +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")


ggsave(
  "Figure_2.tiff",
  fig2,
  width = 13,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# Stot -------------------------------------------
## Preparing -------------------------------------
valid_data <- species %>%
  filter(status == "valid") %>%
  transmute(
    Species = species_without_authors,
    ano_descricao = as.numeric(coalesce(year_description, year_valid)),
    author_names
  ) %>%
  filter(!is.na(ano_descricao))

nrow(valid_data) # 494

### Authors per 5-year interval -------------------------------

authors_by_species <- valid_data %>%
  mutate(
    interval_start = floor(ano_descricao / 5) * 5,
    author_clean = author_names %>%
      str_replace_all("\\s*&\\s*", "; ") %>%
      str_replace_all("\\s+and\\s+", "; ") %>%
      str_replace_all("\\s*;\\s*", "; ") %>%
      str_replace_all("\\[|\\]", "") %>%
      str_replace_all("(?<=[A-Z]\\.)\\s*,\\s*(?=[A-Z][a-z]+)", "; ") %>%
      str_replace_all(",\\s+(?=[A-Z][a-z]+\\s+[A-Z]\\.)", "; ") %>%
      str_replace_all("(?<=[A-Z]\\.)\\s+(?=[A-Z][a-z]+\\s+[A-Z]\\.)", "; ")
  ) %>%
  separate_rows(author_clean, sep = ";") %>%
  mutate(
    author_clean = str_squish(author_clean),
    author_id = author_clean %>%
      str_replace_all(",", "") %>%
      str_squish()
  ) %>%
  filter(
    !is.na(author_id),
    author_id != "",
    !str_detect(author_id, "^Jr\\.?$")
  )

authors_by_interval <- authors_by_species %>%
  group_by(interval_start) %>%
  summarise(
    Tt = n_distinct(author_id),
    .groups = "drop"
  )


### Discovery data by 5-year intervals ------------------------

desc_raw <- valid_data %>%
  mutate(
    interval_start = floor(ano_descricao / 5) * 5,
    interval_end = interval_start + 5
  ) %>%
  group_by(interval_start, interval_end) %>%
  summarise(
    Ano_medio = mean(ano_descricao, na.rm = TRUE),
    Delta_St = n(),
    .groups = "drop"
  )

ultimo_ano <- max(valid_data$ano_descricao)

all_intervals <- tibble(
  interval_start = seq(
    floor(min(valid_data$ano_descricao) / 5) * 5,
    floor(ultimo_ano / 5) * 5,
    by = 5
  )
) %>%
  mutate(
    interval_end = interval_start + 5
  )

desc_intervalo <- all_intervals %>%
  left_join(desc_raw, by = c("interval_start", "interval_end")) %>%
  left_join(authors_by_interval, by = "interval_start") %>%
  mutate(
    Delta_St = replace_na(Delta_St, 0L),
    Tt = replace_na(Tt, 0L),
    Ano_medio = if_else(is.na(Ano_medio), interval_start + 2.5, Ano_medio)
  ) %>%
  arrange(interval_start) %>%
  mutate(
    St = lag(cumsum(Delta_St), default = 0),
    Acumulado_Total = cumsum(Delta_St),
    Tempo = Ano_medio - min(Ano_medio)
  )

max(desc_intervalo$Acumulado_Total) 

## Model fitting ---------------------------------------------

n_obs <- max(desc_intervalo$Acumulado_Total)
Stot_init <- n_obs * 1.2

fit_gnls_safe <- function(formula, data, start, model_name) {
  
  fit1 <- tryCatch(
    gnls(
      formula,
      data = data,
      start = start,
      weights = varPower(),
      control = gnlsControl(
        returnObject = TRUE,
        minScale = 1e-500,
        tolerance = 0.001,
        nlsMaxIter = 3,
        maxIter = 200
      )
    ),
    error = function(e) NULL
  )
  
  if (!is.null(fit1)) {
    message(model_name, ": fitted with varPower")
    return(fit1)
  }
  
  fit2 <- tryCatch(
    gnls(
      formula,
      data = data,
      start = start,
      control = gnlsControl(
        returnObject = TRUE,
        minScale = 1e-500,
        tolerance = 0.001,
        nlsMaxIter = 100,
        maxIter = 500
      )
    ),
    error = function(e) {
      message(model_name, " failed: ", e$message)
      NULL
    }
  )
  
  if (!is.null(fit2)) {
    message(model_name, ": fitted without varPower")
  }
  
  fit2
}

modelo_luhe <- fit_gnls_safe(
  Delta_St ~ Tt * (a + b * Delta_St) * (Stot - St),
  data = desc_intervalo,
  start = list(Stot = Stot_init, a = 0.01, b = 0.001),
  model_name = "Lu & He"
)

modelo_joppa <- fit_gnls_safe(
  Delta_St ~ Tt * (a + b * Tempo) * (Stot - St),
  data = desc_intervalo,
  start = list(Stot = Stot_init, a = 0.01, b = 0.001),
  model_name = "Joppa"
)

modelo_logistico <- fit_gnls_safe(
  Delta_St ~ (a + b * St) * (Stot - St),
  data = desc_intervalo,
  start = list(Stot = Stot_init, a = 0.01, b = 0.001),
  model_name = "Logistic"
)

modelos_lista <- list(
  "Lu & He" = modelo_luhe,
  "Joppa" = modelo_joppa,
  "Logistic" = modelo_logistico
) %>%
  purrr::discard(is.null)

names(modelos_lista)

## Model comparison ------------------------------------------

get_k <- function(m) length(coef(m))

AICc_local <- function(m, n) {
  k <- get_k(m)
  AIC(m) + (2 * k * (k + 1)) / (n - k - 1)
}

tabela_comparacao <- tibble(
  Model = names(modelos_lista),
  AICc = map_dbl(modelos_lista, ~ AICc_local(.x, nrow(desc_intervalo))),
  k = map_int(modelos_lista, get_k),
  Stot = map_dbl(modelos_lista, ~ coef(.x)[["Stot"]]),
  a = map_dbl(modelos_lista, ~ coef(.x)[["a"]]),
  b = map_dbl(modelos_lista, ~ coef(.x)[["b"]])
) %>%
  arrange(AICc) %>%
  mutate(
    Delta_AICc = AICc - min(AICc),
    wAICc = exp(-0.5 * Delta_AICc) / sum(exp(-0.5 * Delta_AICc))
  )

## Bootstrap -----------------------------

make_desc_intervalo <- function(valid_data_boot) {
  
  authors_by_species_boot <- valid_data_boot %>%
    mutate(
      interval_start = floor(ano_descricao / 5) * 5,
      author_clean = author_names %>%
        str_replace_all("\\s*&\\s*", "; ") %>%
        str_replace_all("\\s+and\\s+", "; ") %>%
        str_replace_all("\\s*;\\s*", "; ") %>%
        str_replace_all("\\[|\\]", "") %>%
        str_replace_all("(?<=[A-Z]\\.)\\s*,\\s*(?=[A-Z][a-z]+)", "; ") %>%
        str_replace_all(",\\s+(?=[A-Z][a-z]+\\s+[A-Z]\\.)", "; ") %>%
        str_replace_all("(?<=[A-Z]\\.)\\s+(?=[A-Z][a-z]+\\s+[A-Z]\\.)", "; ")
    ) %>%
    separate_rows(author_clean, sep = ";") %>%
    mutate(
      author_clean = str_squish(author_clean),
      author_id = author_clean %>%
        str_replace_all(",", "") %>%
        str_squish()
    ) %>%
    filter(
      !is.na(author_id),
      author_id != "",
      !str_detect(author_id, "^Jr\\.?$")
    )
  
  authors_by_interval_boot <- authors_by_species_boot %>%
    group_by(interval_start) %>%
    summarise(
      Tt = n_distinct(author_id),
      .groups = "drop"
    )
  
  desc_raw_boot <- valid_data_boot %>%
    mutate(
      interval_start = floor(ano_descricao / 5) * 5,
      interval_end = interval_start + 5
    ) %>%
    group_by(interval_start, interval_end) %>%
    summarise(
      Ano_medio = mean(ano_descricao, na.rm = TRUE),
      Delta_St = n(),
      .groups = "drop"
    )
  
  all_intervals %>%
    left_join(desc_raw_boot, by = c("interval_start", "interval_end")) %>%
    left_join(authors_by_interval_boot, by = "interval_start") %>%
    mutate(
      Delta_St = replace_na(Delta_St, 0L),
      Tt = replace_na(Tt, 0L),
      Ano_medio = if_else(
        is.na(Ano_medio),
        interval_start + 2.5,
        Ano_medio
      )
    ) %>%
    arrange(interval_start) %>%
    mutate(
      St = lag(cumsum(Delta_St), default = 0),
      Acumulado_Total = cumsum(Delta_St),
      Tempo = Ano_medio - min(Ano_medio)
    )
}

fit_three_models <- function(data_boot) {
  
  n_obs_boot <- max(data_boot$Acumulado_Total)
  Stot_init_boot <- n_obs_boot * 1.2
  
  list(
    "Lu & He" = fit_gnls_safe(
      Delta_St ~ Tt * (a + b * Delta_St) * (Stot - St),
      data = data_boot,
      start = list(Stot = Stot_init_boot, a = 0.01, b = 0.001),
      model_name = "Lu & He bootstrap"
    ),
    
    "Joppa" = fit_gnls_safe(
      Delta_St ~ Tt * (a + b * Tempo) * (Stot - St),
      data = data_boot,
      start = list(Stot = Stot_init_boot, a = 0.01, b = 0.001),
      model_name = "Joppa bootstrap"
    ),
    
    "Logistic" = fit_gnls_safe(
      Delta_St ~ (a + b * St) * (Stot - St),
      data = data_boot,
      start = list(Stot = Stot_init_boot, a = 0.01, b = 0.001),
      model_name = "Logistic bootstrap"
    )
  ) %>%
    purrr::discard(is.null)
}


set.seed(2026)

nboot <- 100

boot_df <- purrr::map_dfr(seq_len(nboot), function(i) {
  
  valid_boot <- valid_data %>%
    slice_sample(
      n = nrow(valid_data),
      replace = TRUE
    )
  
  data_boot <- make_desc_intervalo(valid_boot)
  
  fits_boot <- fit_three_models(data_boot)
  
  purrr::imap_dfr(
    fits_boot,
    ~ tibble(
      boot_id = i,
      Model = .y,
      Stot_boot = coef(.x)[["Stot"]]
    )
  )
}) %>%
  filter(
    is.finite(Stot_boot),
    !is.na(Stot_boot),
    Stot_boot > n_obs
  )

boot_df %>%
  count(Model)


ci_stot_df <- boot_df %>%
  group_by(Model) %>%
  summarise(
    Lower_95_CI = quantile(Stot_boot, 0.025, na.rm = TRUE),
    Upper_95_CI = quantile(Stot_boot, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

## Table 1 ---------------------------------------------
tabela_final <- tabela_comparacao %>%
  left_join(ci_stot_df, by = "Model") %>%
  select(
    Model,
    Stot,
    Lower_95_CI,
    Upper_95_CI,
    a,
    b,
    Delta_AICc,
    wAICc
  ) %>%
  mutate(
    Stot = round(Stot, 0),
    Lower_95_CI = round(Lower_95_CI, 0),
    Upper_95_CI = round(Upper_95_CI, 0),
    Delta_AICc = round(Delta_AICc, 1),
    wAICc = round(wAICc, 4)
  )

tabela_final

# Range, latitude, population, order, elevation -------------------------------

## Latitude  --------------------------------
species_lat <- occ %>%
  filter(
    !is.na(Species),
    !is.na(lat)
  ) %>%
  group_by(Species) %>%
  summarise(
    Latitude = mean(lat, na.rm = TRUE),
    .groups = "drop"
  )

species <- species %>%
  left_join(
    species_lat,
    by = c(
      "species_without_authors" = "Species"
    )
  )

## Range, order, population and road density  ---------------------
new_variables = read_excel("coord_all.xlsx", sheet = "variables")

species <- species %>%
  left_join(
    new_variables %>%
      transmute(
        species_without_authors = species,
        Range_Size = Area,
        River_order = order,
        Population = population,
        Road_density = road
      ),
    by = "species_without_authors"
  )

## Elevation ------------------------------------------
species_altitude <- occ %>%
  mutate(
    Altitude = as.numeric(`Altitude (m)`)
  ) %>%
  filter(
    !is.na(Species),
    !is.na(Altitude)
  ) %>%
  group_by(Species) %>%
  summarise(
    Altitude = mean(Altitude, na.rm = TRUE),
    .groups = "drop"
  )

species <- species %>%
  left_join(
    species_altitude,
    by = c("species_without_authors" = "Species")
  )

species %>%
  summarise(
    n_species = n(),
    n_with_altitude = sum(!is.na(Altitude)),
    n_without_altitude = sum(is.na(Altitude)),
    min_altitude = min(Altitude, na.rm = TRUE),
    median_altitude = median(Altitude, na.rm = TRUE),
    max_altitude = max(Altitude, na.rm = TRUE)
  )

# Year as response ----------------------------------------
## Preparation -----------------------------------
fig3_data <- species %>%
  filter(status == "valid") %>%
  transmute(
    Species = species_without_authors,
    genus = `valid genus`,
    year_description,
    size_TL_mm,
    life_cycle,
    size_TL_mm,
    Latitude, 
    Range_Size,
    River_order,
    Population,
    Road_density,
    Altitude
  ) 

fig3_data$size_TL_mm = as.numeric(fig3_data$size_TL_mm)

fig3_data <- fig3_data %>%
  left_join(
    species %>%
      filter(status == "valid") %>%
      transmute(
        Species = species_without_authors,
        IUCN_category,
        
        Threatened = if_else(
          IUCN_category %in% c("CR", "EN", "VU"),
          1L,
          0L
        ),
        
        Data_deficient = if_else(
          IUCN_category == "DD",
          1L,
          0L
        )
      ),
    by = "Species"
  )


fig3_data %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "n_NA"
  ) %>%
  arrange(desc(n_NA))

fig3_model <- fig3_data %>%
  select(
    Species,
    year_description,
    genus,
    Data_deficient,
    Threatened,
    Latitude,
    Range_Size,
    life_cycle,
    size_TL_mm,
    Population,
    Road_density,
    River_order, 
    Altitude
  ) %>%
  mutate(
    Range_Size = log10(Range_Size + 1),
    Population = log10(Population + 1),
    Road_density = log10(Road_density + 1),
    River_order = log10(River_order + 1),
    Altitude = log10(Altitude + 1),
    size_TL_mm = log10(size_TL_mm + 1)
  ) %>%
  mutate(
    across(
      c(
        Latitude,
        Range_Size,
        Population,
        Road_density,
        River_order,
        Altitude, 
        size_TL_mm
      ),
      ~ as.numeric(scale(.))
    )
  ) %>%
  drop_na()

nrow(fig3_model) # 453

n_distinct(fig3_model$Species)

## VIF ---------------------------------
mod_vif <- lm(
  year_description ~
    Latitude +
    Range_Size +
    life_cycle +
    size_TL_mm + 
    Population +
    Road_density +
    River_order +
    Altitude,
  
  data = fig3_model
)

car::vif(mod_vif)

## Model: year as response ----------------------------------
mod_bayes <- brm(
  year_description ~
    Latitude +
    Range_Size +
    life_cycle +
    size_TL_mm + 
    Population +
    Road_density +
    River_order +
    Altitude +
    (1 | genus),
  
  data = fig3_model,
  family = Gamma(link = "log"),
  chains = 4,
  cores = 4,
  iter = 4000,
  seed = 2026
)

summary(mod_bayes)

fixef(mod_bayes) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Parameter") %>%
  mutate(
    Posterior_mean = round(Estimate, 4),
    Lower_95CI = round(Q2.5, 4),
    Upper_95CI = round(Q97.5, 4)
  ) %>%
  filter(Q2.5 * Q97.5 > 0) %>%
  select(
    Parameter,
    Posterior_mean,
    Lower_95CI,
    Upper_95CI
  )

bayes_R2(mod_bayes)

rhat_vals <- rhat(mod_bayes)
max(rhat_vals, na.rm = TRUE)

## Figure 3 -------------------------------------------------
posterior <- spread_draws(
  mod_bayes,
  b_Latitude,
  b_Range_Size,
  b_life_cycleNonMannual,
  b_size_TL_mm,
  b_Population,
  b_Road_density,
  b_River_order,
  b_Altitude
)

posterior_long <- posterior %>%
  pivot_longer(
    cols = starts_with("b_"),
    names_to = "term",
    values_to = "estimate"
  ) %>%
  mutate(
    estimate = if_else(
      term == "b_life_cycleNonMannual",
      -estimate,
      estimate
    )
  ) %>%
  mutate(
    term = dplyr::recode(
      term,
      "b_life_cycleNonMannual" = "Annual",
      "b_Range_Size" = "Range size",
      "b_Latitude" = "Latitude",
      "b_Population" = "Population",
      "b_Road_density" = "Road density",
      "b_River_order" = "River order",
      "b_Altitude" = "Elevation",
      "b_size_TL_mm" = "Body size"
    )
  )

desired_order <- c(
  "Annual",
  "Body size",
  "Range size",
  "Latitude",
  "Elevation",
  "River order",
  "Population",
  "Road density"
)

posterior_summary <- posterior_long %>%
  group_by(term) %>%
  summarise(
    mean = mean(estimate),
    lower = quantile(estimate, 0.025),
    upper = quantile(estimate, 0.975),
    .groups = "drop"
  ) %>%
  mutate(
    term = factor(term, levels = rev(desired_order))
  )

posterior_long <- posterior_long %>%
  mutate(
    term = factor(term, levels = rev(desired_order))
  )

fig3 <- ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray40"
  ) +
  stat_halfeye(
    data = posterior_long,
    aes(x = estimate, y = term),
    fill = "gray30",
    alpha = 0.2,
    color = "gray30",
    slab_color = NA,
    point_interval = "mean_qi",
    .width = 0.95
  ) +
  geom_segment(
    data = posterior_summary,
    aes(
      x = lower,
      xend = upper,
      y = term,
      yend = term
    ),
    linewidth = 1.2,
    color = "black"
  ) +
  geom_point(
    data = posterior_summary,
    aes(x = mean, y = term),
    shape = 21,
    size = 4,
    fill = "gray50",
    color = "black"
  ) +
  labs(
    x = "Posterior estimate",
    y = NULL
  ) +
  theme_classic(base_size = 15) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold")
  )+
  scale_x_continuous(limits = c(-0.016,  0.016),
                     breaks = seq(-0.015, 0.015, by = 0.005))

fig3

ggsave(
  "Figure_3.tiff",
  plot = fig3,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

# Figure 4A -------------------------------
## Preparation ----------------------------
hybas9_atlas <- st_read("hydroatlas_9/BasinATLAS_v10_lev09.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid()

sf_use_s2(FALSE)

neotropics_bbox <- st_bbox(
  c(
    xmin = -120,
    xmax = -30,
    ymin = -58,
    ymax = 35
  ),
  crs = st_crs(4326)
)

hybas9_atlas_clean <- hybas9_atlas %>%
  select(HYBAS_ID, geometry) %>%
  st_transform(4326) %>%
  sf::st_make_valid() %>%
  st_buffer(0)

hybas9 <- suppressWarnings(
  st_crop(
    hybas9_atlas_clean,
    neotropics_bbox
  )
) %>%
  sf::st_make_valid() %>%
  st_buffer(0)

occ_sf <- occ %>%
  filter(!is.na(Species), !is.na(lat), !is.na(long)) %>%
  distinct(Species, lat, long) %>%
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326,
    remove = FALSE
  )

species_year <- species %>%
  filter(status == "valid") %>%
  transmute(
    Species = species_without_authors,
    year_description = as.numeric(coalesce(year_description, year_valid))
  ) %>%
  filter(!is.na(year_description))

occ_hybas <- st_join(
  occ_sf,
  hybas9 %>% select(HYBAS_ID),
  join = st_within,
  left = FALSE
)

basin_species <- occ_hybas %>%
  st_drop_geometry() %>%
  distinct(HYBAS_ID, Species)

basin_mean_year_tbl <- basin_species %>%
  inner_join(
    species_year,
    by = "Species"
  ) %>%
  group_by(HYBAS_ID) %>%
  summarise(
    basin_mean_year = mean(year_description, na.rm = TRUE),
    basin_richness = n_distinct(Species),
    .groups = "drop"
  )

hybas9_year <- hybas9 %>%
  left_join(
    basin_mean_year_tbl,
    by = "HYBAS_ID"
  ) %>%
  filter(!is.na(basin_mean_year))

neotropics_poly <- st_as_sfc(neotropics_bbox)

grid_1deg <- st_make_grid(
  neotropics_poly,
  cellsize = 1,
  square = TRUE
) %>%
  st_as_sf() %>%
  mutate(grid_id = row_number()) %>%
  st_set_crs(4326) %>%
  st_make_valid()

grid_basin_intersections <- st_intersects(
  grid_1deg,
  hybas9_year
)

grid_mean_year_tbl <- tibble(
  grid_id = grid_1deg$grid_id,
  basin_index = grid_basin_intersections
) %>%
  unnest(basin_index) %>%
  mutate(
    HYBAS_ID = hybas9_year$HYBAS_ID[basin_index],
    basin_mean_year = hybas9_year$basin_mean_year[basin_index],
    basin_richness = hybas9_year$basin_richness[basin_index]
  ) %>%
  group_by(grid_id) %>%
  summarise(
    mean_basin_year_description = mean(basin_mean_year, na.rm = TRUE),
    mean_basin_richness = mean(basin_richness, na.rm = TRUE),
    n_basins = n_distinct(HYBAS_ID),
    .groups = "drop"
  )

grid_mean_year <- grid_1deg %>%
  left_join(
    grid_mean_year_tbl,
    by = "grid_id"
  ) %>%
  filter(!is.na(mean_basin_year_description))

world_neotropics <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  st_transform(4326) %>%
  st_crop(neotropics_bbox)

land_neotropics <- world_neotropics %>%
  st_union() %>%
  st_make_valid()

grid_mean_year_land <- st_intersection(
  st_make_valid(grid_mean_year),
  land_neotropics
)

## Figure 4A ----------------------------------------------
fig4_A <- ggplot() +
  geom_sf(
    data = world_neotropics,
    fill = "gray92",
    color = "gray65",
    linewidth = 0.2
  ) +
  geom_sf(
    data = grid_mean_year_land,
    aes(fill = mean_basin_year_description),
    color = "black",
    alpha = 0.9
  ) +
  geom_sf(
    data = world_neotropics,
    fill = NA,
    color = "gray35",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    option = "magma",
    limits = c(1800, 2020),
    breaks = c(1800, 1850, 1900, 1950, 2000),
    oob = scales::squish,
    name = "Mean year of\ndescription"
  )+
  coord_sf(
    xlim = c(-120, -30),
    ylim = c(-58, 35),
    expand = FALSE
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.title = element_blank(),
    legend.position = "right"
  )

fig4_A <- fig4_A +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(2.2, "cm"),
      barheight = unit(0.35, "cm"),
      direction = "horizontal"
    )
  ) +
  theme(
    axis.title = element_blank(),
    legend.position = c(0.19, 0.15),
    legend.direction = "horizontal",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.55),
      color = NA
    ),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8,
                               angle = 45))


fig4_A

# Wallacean shortfall  ------------------------
## Preparation ----------------------------------
sf_use_s2(FALSE)

coverage_threshold <- 0.70
min_records_well_sampled <- 10
neotropics_bbox <- st_bbox(
  c(
    xmin = -120,
    xmax = -30,
    ymin = -58,
    ymax = 35
  ),
  crs = st_crs(4326)
)

neotropics_poly <- st_as_sfc(neotropics_bbox)

world_neotropics <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  st_transform(4326) %>%
  st_crop(neotropics_bbox)

land_neotropics <- world_neotropics %>%
  st_union() %>%
  st_make_valid()


grid_1deg <- st_make_grid(
  neotropics_poly,
  cellsize = 1,
  square = TRUE
) %>%
  st_as_sf() %>%
  mutate(grid_id = row_number()) %>%
  st_set_crs(4326) %>%
  st_make_valid()

grid_land <- st_intersection(
  st_make_valid(grid_1deg),
  land_neotropics
)

valid_species <- species %>%
  filter(status == "valid") %>%
  pull(species_without_authors)

occ_clean <- occ %>%
  filter(
    Species %in% valid_species,
    !is.na(Species),
    !is.na(lat),
    !is.na(long)
  ) %>%
  distinct(Species, lat, long)

occ_sf <- occ_clean %>%
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326,
    remove = FALSE
  )

occ_grid <- st_join(
  occ_sf,
  grid_1deg %>% select(grid_id),
  join = st_within,
  left = FALSE
)

grid_records_tbl <- occ_grid %>%
  st_drop_geometry() %>%
  count(grid_id, name = "n_records")

grid_species_abund <- occ_grid %>%
  st_drop_geometry() %>%
  count(grid_id, Species, name = "abundance")


get_coverage <- function(x) {
  
  x <- x[x > 0]
  
  if (length(x) == 0) return(NA_real_)
  
  out <- tryCatch(
    iNEXT::DataInfo(x, datatype = "abundance"),
    error = function(e) NULL
  )
  
  if (is.null(out)) return(NA_real_)
  
  as.numeric(out$SC[1])
}


grid_coverage_tbl <- grid_species_abund %>%
  group_by(grid_id) %>%
  summarise(
    n_species = n_distinct(Species),
    sample_coverage = get_coverage(abundance),
    .groups = "drop"
  )

grid_wallace_tbl <- grid_land %>%
  st_drop_geometry() %>%
  distinct(grid_id) %>%
  left_join(grid_records_tbl, by = "grid_id") %>%
  left_join(grid_coverage_tbl, by = "grid_id") %>%
  mutate(
    n_records = replace_na(n_records, 0L),
    n_species = replace_na(n_species, 0L),
    sample_coverage_pct = sample_coverage * 100,
    has_records = n_records > 0,
    well_sampled = sample_coverage >= coverage_threshold &
      n_records >= min_records_well_sampled
  )

grid_wallace <- grid_land %>%
  left_join(grid_wallace_tbl, by = "grid_id")

## Summary ---------------------------------
wallace_summary <- grid_wallace_tbl %>%
  summarise(
    n_cells_total = n(),
    n_cells_without_records = sum(n_records == 0),
    percent_cells_without_records = n_cells_without_records / n_cells_total * 100,
    n_cells_with_records = sum(n_records > 0),
    min_records = min(n_records[n_records > 0], na.rm = TRUE),
    mean_records = mean(n_records[n_records > 0], na.rm = TRUE),
    median_records = median(n_records[n_records > 0], na.rm = TRUE),
    max_records = max(n_records[n_records > 0], na.rm = TRUE),
    n_well_sampled = sum(well_sampled, na.rm = TRUE),
    percent_well_sampled = n_well_sampled / n_cells_total * 100
  )

wallace_summary

grid_wallace <- grid_wallace %>%
  mutate(
    well_sampled_5 = sample_coverage >= 0.70 &
      n_records >= 5,
    
    well_sampled_10 = sample_coverage >= 0.70 &
      n_records >= 10,
    
    well_sampled_20 = sample_coverage >= 0.70 &
      n_records >= 20
  )

grid_wallace %>%
  st_drop_geometry() %>%
  summarise(
    cells_5 = sum(well_sampled_5, na.rm = TRUE),
    cells_10 = sum(well_sampled_10, na.rm = TRUE),
    cells_20 = sum(well_sampled_20, na.rm = TRUE)
  )

grid_wallace_tbl %>%
  filter(n_records > 0) %>%
  summarise(
    min = min(n_species),
    mean = mean(n_species),
    median = median(n_species),
    max = max(n_species)
  )

## Within Rivulidae grid envelope -------------------

riv_cells <- grid_wallace %>%
  filter(n_records > 0)

riv_grid_envelope <- riv_cells %>%
  st_union() %>%
  st_convex_hull() %>%
  st_make_valid()

idx_envelope <- lengths(
  st_intersects(
    grid_wallace,
    riv_grid_envelope
  )
) > 0

grid_within_riv_envelope <- grid_wallace[idx_envelope, ] %>%
  mutate(
    record_status = if_else(
      n_records > 0,
      "With Rivulidae records",
      "Without Rivulidae records"
    )
  )

wallace_envelope_summary <- grid_within_riv_envelope %>%
  st_drop_geometry() %>%
  summarise(
    n_cells_total_envelope = n(),
    n_cells_with_records = sum(n_records > 0),
    n_cells_without_records = sum(n_records == 0),
    percent_with_records = n_cells_with_records / n_cells_total_envelope * 100,
    percent_without_records = n_cells_without_records / n_cells_total_envelope * 100
  )

wallace_envelope_summary

riv_cells <- grid_wallace %>%
  filter(n_records > 0)

riv_grid_envelope <- riv_cells %>%
  st_union() %>%
  st_convex_hull() %>%
  st_make_valid()

idx_envelope <- lengths(
  st_intersects(
    grid_wallace,
    riv_grid_envelope
  )
) > 0

grid_within_riv_envelope <- grid_wallace[idx_envelope, ] %>%
  mutate(
    record_status = if_else(
      n_records > 0,
      "With Rivulidae records",
      "Without Rivulidae records"
    )
  )

wallace_envelope_summary <- grid_within_riv_envelope %>%
  st_drop_geometry() %>%
  summarise(
    n_cells_total_envelope = n(),
    n_cells_with_records = sum(n_records > 0),
    n_cells_without_records = sum(n_records == 0),
    percent_with_records = n_cells_with_records / n_cells_total_envelope * 100,
    percent_without_records = n_cells_without_records / n_cells_total_envelope * 100
  )

wallace_envelope_summary

## Figure 4B ----------------------------
fig4_B <- ggplot() +
  geom_sf(
    data = world_neotropics,
    fill = "gray92",
    color = "gray65",
    linewidth = 0.2
  ) +
  geom_sf(
    data = grid_wallace %>% filter(n_records > 0),
    aes(fill = n_records),
    color = "black",
    alpha = 0.9
  ) +
  geom_sf(
    data = world_neotropics,
    fill = NA,
    color = "gray35",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    option = "magma",
    trans = "log10",
    name = "Number of\nrecords"
  ) +
  coord_sf(
    xlim = c(-120, -30),
    ylim = c(-58, 35),
    expand = FALSE
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.title = element_blank(),
    legend.position = "right"
  )

fig4_B

fig4_B <- fig4_B +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(2.2, "cm"),
      barheight = unit(0.35, "cm"),
      direction = "horizontal"
    )
  ) +
  theme(
    axis.title = element_blank(),
    legend.position = c(0.19, 0.15),
    legend.direction = "horizontal",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.55),
      color = NA
    ),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8, angle = 45)
  )

fig4_B

## Figure 4C (10 records) ------------------------------------
fig4_C <- ggplot() +
  geom_sf(
    data = world_neotropics,
    fill = "gray92",
    color = "gray65",
    linewidth = 0.2
  ) +
  geom_sf(
    data = grid_wallace %>% filter(well_sampled),
    aes(fill = sample_coverage_pct),
    color = "black",
    alpha = 0.9
  ) +
  geom_sf(
    data = world_neotropics,
    fill = NA,
    color = "gray35",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    option = "magma",
    limits = c(70, 100),
    oob = scales::squish,
    name = "Completeness (%)"
  ) +
  coord_sf(
    xlim = c(-120, -30),
    ylim = c(-58, 35),
    expand = FALSE
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.title = element_blank(),
    legend.position = "right"
  )

fig4_C

fig4_C <- fig4_C +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(2.2, "cm"),
      barheight = unit(0.35, "cm"),
      direction = "horizontal"
    )
  ) +
  theme(
    axis.title = element_blank(),
    legend.position = c(0.21, 0.13),
    legend.direction = "horizontal",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.55),
      color = NA
    ),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8,
                               angle = 45)
  )

fig4_C


# Figure 4D: distance to nearest well-sampled cell ------------
sf_use_s2(TRUE)

well_cells <- grid_wallace %>%
  filter(well_sampled_10)

record_cells <- grid_wallace %>%
  filter(n_records > 0)

nrow(well_cells)
nrow(record_cells)

record_points <- record_cells %>%
  st_point_on_surface()

well_points <- well_cells %>%
  st_point_on_surface()

nearest_id <- st_nearest_feature(
  record_points,
  well_points
)

dist_to_well_km <- st_distance(
  record_points,
  well_points[nearest_id, ],
  by_element = TRUE
) %>%
  as.numeric() / 1000

grid_distance <- record_cells %>%
  mutate(
    dist_nearest_well_km = dist_to_well_km
  )


distance_summary <- grid_distance %>%
  st_drop_geometry() %>%
  summarise(
    n_cells_with_records = n(),
    n_well_sampled_cells = sum(well_sampled_10, na.rm = TRUE),
    min_km = min(dist_nearest_well_km, na.rm = TRUE),
    mean_km = mean(dist_nearest_well_km, na.rm = TRUE),
    median_km = median(dist_nearest_well_km, na.rm = TRUE),
    max_km = max(dist_nearest_well_km, na.rm = TRUE)
  )

distance_summary


fig4_D <- ggplot() +
  geom_sf(
    data = world_neotropics,
    fill = "gray92",
    color = "gray65",
    linewidth = 0.2
  ) +
  geom_sf(
    data = grid_distance,
    aes(fill = dist_nearest_well_km),
    color = "black",
    linewidth = 0.2,
    alpha = 0.9
  ) +
  geom_sf(
    data = grid_distance %>% filter(well_sampled_10),
    fill = NA,
    color = "black",
    linewidth = 0.45
  ) +
  geom_sf(
    data = world_neotropics,
    fill = NA,
    color = "gray35",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    option = "magma",
    name = "Distance to nearest\nwell-sampled cell (km)"
  ) +
  coord_sf(
    xlim = c(-120, -30),
    ylim = c(-58, 35),
    expand = FALSE
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.title = element_blank(),
    legend.position = "right"
  )

fig4_D

fig4_D <- fig4_D +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(2.2, "cm"),
      barheight = unit(0.35, "cm"),
      direction = "horizontal"
    )
  ) +
  theme(
    axis.title = element_blank(),
    legend.position = c(0.24, 0.15),
    legend.direction = "horizontal",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.55),
      color = NA
    ),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8,
                               angle = 45)
  )

fig4_D

## Figure 4 Complete ---------------------------------
fig4 <- (fig4_A + fig4_B) /
  (fig4_C + fig4_D) +
  plot_annotation(
    tag_levels = "A"
  )

fig4 <- fig4 +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

fig4

ggsave(
  "Figure_4.tiff",
  plot = fig4,
  width = 14,
  height = 10,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# Figure S1 ---------------------------------------------------
## Figure S1A (5 records) --------------------------------
fig_S1_A <- ggplot() +
  geom_sf(
    data = world_neotropics,
    fill = "gray92",
    color = "gray65",
    linewidth = 0.2
  ) +
  geom_sf(
    data = grid_wallace %>% filter(well_sampled_5),
    aes(fill = sample_coverage_pct),
    color = "black"
  ) +
  geom_sf(
    data = world_neotropics,
    fill = NA,
    color = "gray35",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    option = "magma",
    limits = c(70, 100),
    name = "Completeness (%)"
  ) +
  coord_sf(
    xlim = c(-120, -30),
    ylim = c(-58, 35),
    expand = FALSE
  ) +
  theme_classic(base_size = 15)

fig_S1_A <- fig_S1_A +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(2.2, "cm"),
      barheight = unit(0.35, "cm"),
      direction = "horizontal"
    )
  ) +
  theme(
    axis.title = element_blank(),
    legend.position = c(0.19, 0.11),
    legend.direction = "horizontal",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.55),
      color = NA
    ),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8,
                               angle = 45)
  )

fig_S1_A


## Figure S1B (20 records) -------------------------------------------
fig_S1_B <- ggplot() +
  geom_sf(
    data = world_neotropics,
    fill = "gray92",
    color = "gray65",
    linewidth = 0.2
  ) +
  geom_sf(
    data = grid_wallace %>% filter(well_sampled_20),
    aes(fill = sample_coverage_pct),
    color = "black",
    alpha = 0.9
  ) +
  geom_sf(
    data = world_neotropics,
    fill = NA,
    color = "gray35",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    option = "magma",
    limits = c(70, 100),
    oob = scales::squish,
    name = "Completeness (%)"
  ) +
  coord_sf(
    xlim = c(-120, -30),
    ylim = c(-58, 35),
    expand = FALSE
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.title = element_blank(),
    legend.position = "right"
  )

fig_S1_B

fig_S1_B <- fig_S1_B +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(2.2, "cm"),
      barheight = unit(0.35, "cm"),
      direction = "horizontal"
    )
  ) +
  theme(
    axis.title = element_blank(),
    legend.position = c(0.19, 0.11),
    legend.direction = "horizontal",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.55),
      color = NA
    ),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8,
                               angle = 45)
  )

fig_S1_B

## Figure S1 Complete -------------------------------
fig_S1 <- (fig_S1_A + fig_S1_B) +
  plot_annotation(
    tag_levels = "A"
  )

fig_S1 <- fig_S1 +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

fig_S1

ggsave(
  "Figure_S1.tiff",
  plot = fig_S1,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)
