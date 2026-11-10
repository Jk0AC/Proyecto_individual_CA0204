

#Graficos: ----



## Graficos elección empresas: ----


heatmap.muestra <- muestra %>%
  summarise(
    Empresas = paste(Ticker, collapse = ", "),
    n = n(),
    .by = Grupo
  ) %>%
  tidyr::separate(Grupo,
                  into = c("Volumen", "Volatilidad"),
                  sep = " \\| ") %>%
  mutate(
    Volatilidad = factor(Volatilidad, levels = c("Baja σ", "Media σ", "Alta σ")),
    
    Volumen = factor(
      Volumen,
      levels = c("Alto volumen", "Medio volumen", "Bajo volumen")
    )
  )


grafico.muestra <- ggplot(
  heatmap.muestra,
  aes(
    x = Volatilidad,
    y = Volumen,
    fill = Volatilidad
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  
  geom_text(
    aes(label = Empresas),
    size = 4,
    fontface = "bold",
    color = "white"
  ) +
  
  scale_fill_manual(
    name = "Volatilidad:",
    values = c(
      "Baja σ"  = "#2ECC71",
      "Media σ" = "#F4D03F",
      "Alta σ"  = "#E74C3C"
    ),
    breaks = c(
      "Baja σ",
      "Media σ",
      "Alta σ"
    )
  ) +
  
  labs(
    title = "Selección de empresas por volatilidad y volumen",
    y = "Clasificación de volumen",
    x = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    legend.position = "bottom",
    
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

##Graficos serie histórica precio: ----

#Ordenados por volatilidad:


#1era parte:
empresas.1 <- muestra$Ticker[1:9]

# Datos para el gráfico
datos.plot <- stock.price.dep %>%
  filter(Ticker %in% empresas.1) %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    Base100 = (`Adj Close` / first(`Adj Close`)) * 100
  ) %>%
  ungroup()

# Última observación de cada empresa
etiquetas <- datos.plot %>%
  group_by(Ticker) %>%
  slice_max(Date, n = 1) %>%
  ungroup() %>%
  arrange(desc(Base100))

fecha.corte <- as.Date("2026-08-31")


grafico.serie.historica.1 <- ggplot(
  datos.plot,
  aes(
    x = Date,
    y =`Adj Close`,
    color = Ticker
  )
) +
  
  geom_line(
    linewidth = 0.8
  ) +
  
  geom_vline(
    xintercept = fecha.corte,
    color = "red",
    linewidth = 0.7,
    linetype = "dashed",
    alpha = 0.3
    
  ) +
  
  annotate(
    "text",
    x = fecha.corte,
    y = 700,
    size = 3,
    label = "31 agosto 2026",
    color = "red",
    vjust = 1.0,
    hjust = -0.1,
    alpha = 0.7
  ) +
  
  geom_text_repel(
    data = etiquetas,
    aes(
      label = Ticker
    ),
    direction = "y",
    hjust = 0,
    nudge_x = 300,
    segment.color = "grey50",
    show.legend = FALSE
  ) +
  
  scale_x_date(
    breaks = seq(
      as.Date("2016-01-01"),
      as.Date("2026-01-01"),
      by = "2 years"
    ),
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.08)),
  ) +
  
  labs(
    title = "Evolución histórica de precios ajustados acciones",
    subtitle = "(2016 - 2026)",
    x = "Año",
    y = "Precio acción USD"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.7
    )
  )

# 
# stock.price.dep %>% 
#   filter(Ticker %in% muestra$Ticker[1:9]) %>% 
#   summarise(Maximo = max(`Adj Close`, na.rm = T),.by = Ticker) %>% 
#   arrange(desc(Maximo))
# 
# 
# stock.price.dep %>% 
#   filter(Ticker %in% muestra.volt$Ticker[1:9]) %>% 
#   summarise(Maximo = max(`Adj Close`, na.rm = T),.by = Ticker) %>% 
#   arrange(desc(Maximo))
#

#Sda parte empresas:
empresas.2 <- muestra$Ticker[10:18]

# Datos para el gráfico
datos.plot.2 <- stock.price.dep %>%
  filter(Ticker %in% empresas.2) %>%
  arrange(Ticker, Date)

# Última observación de cada empresa
etiquetas.2 <- datos.plot.2 %>%
  group_by(Ticker) %>%
  slice_max(Date, n = 1) %>%
  ungroup()

grafico.serie.historica.2 <- ggplot(
  datos.plot.2,
  aes(
    x = Date,
    y =`Adj Close`,
    color = Ticker
  )
) +
  
  geom_line(
    linewidth = 0.8
  ) +
  
  geom_vline(
    xintercept = fecha.corte,
    color = "red",
    linewidth = 0.7,
    linetype = "dashed",
    alpha = 0.3
    
  ) +
  
  annotate(
    "text",
    x = fecha.corte,
    y = 700,
    size = 3,
    label = "31 agosto 2026",
    color = "red",
    vjust = 1.0,
    hjust = -0.1,
    alpha = 0.7
  ) +
  
  geom_text_repel(
    data = etiquetas.2,
    aes(
      label = Ticker
    ),
    direction = "y",
    hjust = 0,
    nudge_x = 300,
    segment.color = "grey50",
    show.legend = FALSE
  ) +
  
  scale_x_date(
    breaks = seq(
      as.Date("2016-01-01"),
      as.Date("2026-01-01"),
      by = "2 years"
    ),
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.08)),
  ) +
  
  labs(
    title = "Evolución histórica de precios ajustados acciones",
    subtitle = "(2016 - 2026)",
    x = "Año",
    y = "Precio acción USD"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.7
    )
  )




##Grafico de calor con correlaciones empresas: ----

diag(matriz.cor) <- 1

grafico.corr.empresas <- function(){
  
  corrplot(
    matriz.cor,
    method = "color",
    order = "hclust",
    hclust.method = "complete",
    addrect = 3,
    title = "",
    tl.col = "#05235c",
    tl.cex = 0.9,
    rect.col = "#05235c",
    mar = c(0,0,2,0)
  )
  
}


## Grafico resumen estadístico resultados de carteras: ----




grafico.perceptiles <- ggplot(
  resumen.carteras %>%
    mutate(
      Distancia = `P95 (95%)` - `VaR (5%)`
    ) %>%
    arrange(desc(Distancia)),
  aes(
    y = reorder(Cartera, Distancia)
  )
) +
  
  geom_segment(
    aes(
      x = `VaR (5%)`,
      xend = `P95 (95%)`,
      yend = Cartera
    ),
    linewidth = 1.3,
    color = "grey70"
  ) +
  
  geom_point(
    aes(
      x = `VaR (5%)`,
      color = "VaR (5%)"
    ),
    size = 4
  ) +
  
  geom_point(
    aes(
      x = `Mediana (50%)`,
      color = "Mediana (50%)"
    ),
    size = 4
  ) +
  
  geom_point(
    aes(
      x = `Valor esperado`,
      color = "Valor esperado"
    ),
    size = 4
  ) +
  
  geom_point(
    aes(
      x = `P95 (95%)`,
      color = "P95 (95%)"
    ),
    size = 4
  ) +
  
  # Etiquetas VaR
  geom_text(
    aes(
      x = `VaR (5%)`,
      label = scales::comma(round(`VaR (5%)`,0))
    ),
    hjust = 1.15,
    size = 4,
    colour = "#B22222"
  )  +
  
  # Etiquetas P95
  geom_text(
    aes(
      x = `P95 (95%)`,
      label = scales::comma(round(`P95 (95%)`,0))
    ),
    hjust = -0.15,
    size = 4,
    colour = "#2E8B57"
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c(
      "VaR (5%)" = "#B22222",
      "Mediana (50%)" = "#E69F00",
      "Valor esperado" = "#1F4E79",
      "P95 (95%)" = "#2E8B57"
    )
  ) +
  
  scale_x_continuous(
    labels = scales::comma,
    breaks = c(
      100000,
      120000,
      140000,
      160000,
      180000,
      200000
    ),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  coord_cartesian(
    xlim = c(
      85000,
      190000
    )
  ) +
guides(
  color = guide_legend(
    override.aes = list(size = 5)
  )
)+
  
  labs(
    title = "",
    subtitle = "",
    x = "Valor final de la cartera (USD)",
    y = NULL
  ) +
  
  theme_minimal(base_size = 10) +
  
  theme(
    axis.text.y  = element_text(size = 12, face = "bold"),
    axis.text.x  = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 12),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 14,
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    
    legend.position = "bottom",
    
    panel.border = element_rect(
      colour = "black",
      fill = NA
    ),
    
    panel.grid.minor = element_blank()
  )
