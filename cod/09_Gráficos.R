

#Graficos: ----
# ggplot(stock.price.dep[1:252, ], aes(t, Log_Retornos)) +
#   geom_line(color = "steelblue") +
#   geom_hline(yintercept = 0,
#              linetype = "dashed",
#              color = "red") +
#   labs(
#     title = "Serie temporal de log-retornos",
#     x = "Observación",
#     y = "Log-retorno"
#   ) +
#   theme_minimal()


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
    xintercept = fecha_corte,
    color = "red",
    linewidth = 0.7,
    linetype = "dashed",
    alpha = 0.3
    
  ) +
  
  annotate(
    "text",
    x = fecha_corte,
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
    xintercept = fecha_corte,
    color = "red",
    linewidth = 0.7,
    linetype = "dashed",
    alpha = 0.3
    
  ) +
  
  annotate(
    "text",
    x = fecha_corte,
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




muestra.volt$Ticker




##Grafico de calor con correlaciones empresas: ----

diag(matriz.cor) <- 1

grafico.corr.empresas <- corrplot(
  matriz.cor,
  method = "color",
  order = "hclust",
  hclust.method = "complete",
  addrect = 3,
  title = "Correlacion entre los rendimientos de las empresas", 
  tl.col = "#05235c",
  tl.cex = 0.9,
  rect.col = "#05235c",
  mar = c(0,0,2,0)
)

