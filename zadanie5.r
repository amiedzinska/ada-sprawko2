library(dplyr)
library(knitr)

ankieta <- read.csv("ankieta2.csv", sep=";")
colnames(ankieta) <- c("DZIAŁ", "STAŻ", "CZY_KIER", "PYT_1", "PYT_2", "PYT_3", "PŁEĆ", "WIEK")
anyNA(ankieta) # nie ma braków w danych
unique(ankieta$DZIAŁ)
unique(ankieta$STAŻ)
unique(ankieta$CZY_KIER)
unique(ankieta$PYT_1)
unique(ankieta$PYT_2)
unique(ankieta$PŁEĆ)
unique(ankieta$WIEK)

ankieta <- ankieta |> mutate("WIEK_KAT" = cut(
  WIEK,
  breaks = c(0, 35, 45, 55, Inf),
  labels = c("do 35 lat", "między 36 a 45 lat", "między 46 a 55 lat", "powyżej 55 lat")
))

ankieta <- ankieta |>
  mutate(CZY_ZADOW = case_when(
    PYT_2 %in% c(-2, -1) ~ "niezadowolony",
    PYT_2 %in% c(1, 2) ~ "zadowolony",
    TRUE ~ NA_character_
  ))


wykonaj_test_fishera <- function(dane, var1, var2) {

  dane_clean <- dane |>
    filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))

  tabela <- table(dane_clean[[var1]], dane_clean[[var2]])

  test <- fisher.test(tabela, simulate.p.value = TRUE, B = 10000)

  p_val <- test$p.value

  decyzja <- ifelse(p_val < 0.05,
                    "Odrzucamy H0 (Zależne)",
                    "Nie odrzucamy H0 (Niezależne)")

  data.frame(
    Badana_para = paste(var1, "vs", var2),
    p_value = round(p_val, 4),
    Wniosek = decyzja
  )
}

pary_podstawowe <- list(
  c("CZY_KIER", "WIEK_KAT"),
  c("CZY_KIER", "STAŻ"),
  c("PYT_2", "CZY_KIER"),
  c("PYT_2", "STAŻ"),
  c("PYT_2", "PŁEĆ"),
  c("PYT_2", "WIEK_KAT")
)

pary_zadowolenie <- list(
  c("CZY_ZADOW", "CZY_KIER"),
  c("CZY_ZADOW", "STAŻ"),
  c("CZY_ZADOW", "PŁEĆ"),
  c("CZY_ZADOW", "WIEK_KAT")
)

wyniki_podstawowe <- lapply(pary_podstawowe, function(x) wykonaj_test_fishera(ankieta, x[1], x[2])) |> bind_rows()
wyniki_zadowolenie <- lapply(pary_zadowolenie, function(x) wykonaj_test_fishera(ankieta, x[1], x[2])) |> bind_rows()

tabela_koncowa <- bind_rows(
  wyniki_podstawowe |> mutate(Wariant = "Oryginalne PYT_2"),
  wyniki_zadowolenie |> mutate(Wariant = "Przekształcone CZY_ZADOW")
) |>
  select(Wariant, Badana_para, p_value, Wniosek) # zamiana kolejności kolumn by lepiej to wyglądało

kable(tabela_koncowa, align = c('l', 'c', 'c', 'c'), caption = "Wyniki testu Fishera (Freemana-Haltona)")


