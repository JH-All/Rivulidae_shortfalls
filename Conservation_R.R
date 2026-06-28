
# 2.X  Conservation Shortfall

library(readxl)
Rivulidae_shortfall_FINAL <- read_excel("C:/Users/gusta/OneDrive/Área de Trabalho/João Costa/Linnean shortfall/analises/Rivulidae_shortfall_FINAL.xlsx")
View(Rivulidae_shortfall_FINAL)

###################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)

#FIgure5

# Ordem das categorias
ordem_categorias <- c("NE", "DD", "LC", "NT", "VU", "EN", "CR")

# Dados a partir da coluna current_category_IUCN_SALVE
dados <- Rivulidae_shortfall_FINAL %>%
  mutate(
    categoria = as.character(current_category_IUCN_SALVE),
    categoria = trimws(categoria),
    categoria = if_else(is.na(categoria) | categoria == "", "NE", categoria),
    categoria = factor(categoria, levels = ordem_categorias)
  ) %>%
  filter(!is.na(categoria)) %>%
  count(categoria, name = "n", .drop = FALSE)

# Gráfico
p <- ggplot(dados, aes(x = categoria, y = n, fill = categoria)) +
  geom_col(width = 0.75, color = "black", linewidth = 0.3) +
  geom_text(
    aes(label = n),
    vjust = -0.4,
    size = 5
  ) +
  scale_fill_manual(
    values = c(
      "NE" = "grey80",
      "DD" = "grey55",
      "LC" = "#4CAF50",
      "NT" = "#C7D84B",
      "VU" = "#FFD54F",
      "EN" = "#F28E2B",
      "CR" = "#D7191C"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 165),
    breaks = seq(0, 160, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Category",
    y = "Species Number"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

p


ggsave(
  filename = "grafico_categorias_iucn.tiff",
  plot = p,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

#########################

#Figure6

# ============================================================
# Gráfico temporal: espécies válidas avaliadas vs não avaliadas
# Usando a planilha Rivulidae_shortfall_FINAL
#
# Colunas usadas:
# - Year_description
# - first_assessment_year_IUCN
# - current_category_IUCN_SALVE
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)

# ----------------------------
# 1. Preparar dados
# ----------------------------

dados <- Rivulidae_shortfall_FINAL %>%
  mutate(
    ano_descricao = as.numeric(Year_description),
    first_assessment_year = as.numeric(first_assessment_year_IUCN),
    current_category = str_squish(as.character(current_category_IUCN_SALVE)),
    current_category = if_else(
      is.na(current_category) | current_category == "",
      "NE",
      current_category
    )
  )

# ----------------------------
# 2. Definir anos que entrarão no gráfico
# ----------------------------

anos <- tibble(
  ano = c(1821, 1900, 1964:2026)
)

# ----------------------------
# 3. Calcular séries temporais
# ----------------------------

dados_temporal <- anos %>%
  rowwise() %>%
  mutate(
    
    especies_validas = sum(
      !is.na(dados$ano_descricao) &
        dados$ano_descricao <= ano,
      na.rm = TRUE
    ),
    
    especies_avaliadas = sum(
      !is.na(dados$first_assessment_year) &
        dados$current_category != "NE" &
        dados$first_assessment_year <= ano,
      na.rm = TRUE
    ),
    
    especies_validas_nao_avaliadas =
      especies_validas - especies_avaliadas
  ) %>%
  ungroup()

# ----------------------------
# 4. Criar eixo X artificial
# ----------------------------

dados_temporal <- dados_temporal %>%
  mutate(
    x_plot = case_when(
      ano == 1821 ~ 0,
      ano == 1900 ~ 1.2,
      ano >= 1964 ~ 2.4 + ((ano - 1964) / (2026 - 1964)) * 7.6
    )
  )

# ----------------------------
# 5. Preparar dados em formato longo
# ----------------------------

dados_longo <- dados_temporal %>%
  select(
    ano,
    x_plot,
    especies_validas_nao_avaliadas,
    especies_avaliadas
  ) %>%
  pivot_longer(
    cols = c(
      especies_validas_nao_avaliadas,
      especies_avaliadas
    ),
    names_to = "serie",
    values_to = "n_species"
  ) %>%
  mutate(
    serie = recode(
      serie,
      especies_validas_nao_avaliadas = "Valid species not yet assessed",
      especies_avaliadas = "Assessed species"
    )
  )

# ----------------------------
# 6. Definir rótulos do eixo X
# ----------------------------

axis_breaks <- tibble(
  ano = c(1821, 1900, 1964, 1980, 2000, 2020, 2026),
  x_plot = case_when(
    ano == 1821 ~ 0,
    ano == 1900 ~ 1.2,
    ano >= 1964 ~ 2.4 + ((ano - 1964) / (2026 - 1964)) * 7.6
  )
)

# ----------------------------
# 7. Resumo do déficit de avaliação
# ----------------------------

resumo_deficit <- dados_temporal %>%
  summarise(
    max_deficit = max(especies_validas_nao_avaliadas, na.rm = TRUE),
    ano_max_deficit = ano[which.max(especies_validas_nao_avaliadas)],
    deficit_2026 = especies_validas_nao_avaliadas[ano == 2026],
    especies_avaliadas_2026 = especies_avaliadas[ano == 2026],
    especies_validas_2026 = especies_validas[ano == 2026]
  )

resumo_deficit

# ----------------------------
# 8. Gráfico final
# ----------------------------

grafico_duas_linhas <- ggplot(
  dados_longo,
  aes(
    x = x_plot,
    y = n_species,
    color = serie
  )
) +
  
  geom_line(linewidth = 1.3) +
  
  geom_vline(
    xintercept = axis_breaks$x_plot[axis_breaks$ano == 1964],
    linetype = "dashed",
    linewidth = 0.8,
    color = "black"
  ) +
  
  annotate(
    "text",
    x = axis_breaks$x_plot[axis_breaks$ano == 1964] + 0.15,
    y = max(dados_longo$n_species, na.rm = TRUE) * 0.95,
    label = "IUCN Red List",
    hjust = 0,
    size = 4
  ) +
  
  annotate(
    "text",
    x = 0.6,
    y = -25,
    label = "//",
    size = 6
  ) +
  
  annotate(
    "text",
    x = 1.8,
    y = -25,
    label = "//",
    size = 6
  ) +
  
  scale_color_manual(
    values = c(
      "Assessed species" = "red3",
      "Valid species not yet assessed" = "black"
    )
  ) +
  
  scale_x_continuous(
    breaks = axis_breaks$x_plot,
    labels = axis_breaks$ano,
    limits = c(-0.2, 10.2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  
  scale_y_continuous(
    breaks = pretty_breaks(n = 8),
    expand = expansion(mult = c(0.08, 0.05))
  ) +
  
  labs(
    x = "Year",
    y = "Number of species",
    color = NULL
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

grafico_duas_linhas

# ----------------------------
# 9. Salvar gráfico
# ----------------------------

ggsave(
  filename = "grafico_deficit_avaliacao_iucn_eixo_customizado.tiff",
  plot = grafico_duas_linhas,
  width = 10,
  height = 5.5,
  dpi = 600
)

##############################################################################



