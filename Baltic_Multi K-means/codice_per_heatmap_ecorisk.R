# Leggi il file CSV
df <- read.csv("all_centroid_classification_assignment_31_2020_gadus_morhua.csv", stringsAsFactors = FALSE)

# ---- 1. Creazione colonna hotspot05 ----
df$hotspot05 <- ifelse(df$distance_class_interpretation == "high attention",
                        0.5,
                        0)

# ---- 2. Creazione colonna hotspot_eco_risk ----
#qui viene distinto  rischio ecosistemmico dal rischio NON ecosistemico (con 0.5 e 0 rispettivamente)
target_values <- c(2, 21)      # qui inserisci i cluster che consideri più a rischio ECOSISTEMICO
df$hotspot_eco_risk <- ifelse(df$distance_class %in% target_values,
                              0.5,
                              0)

# ---- 3A. Creazione colonna weight_eco_risk ----
# qui viene classificato ulteriormente il rischio ecosistemico (con 0.5 e 1 in base alla combinazione di stress valutati)
df$weight_eco_risk <- ifelse(df$distance_class %in% c(21), 1,     # qui inserisci i cluster a maggior rishio ECOSISTEMICO
                             ifelse(df$distance_class %in% c(2), 0.5,    #  qui quelli a medio rischio ECOSISTEMICO 
                                    0))      # il resto rimane a 0 sia eco che non eco

# ---- 3B. Creazione colonna weight_eco_risk ----
# se quelli ecosistemici risultano tutti a super rischio e serve solo l'opzione del valore "1"
#      target_values <- c(12, 21, 22, 30)
#   df$weight_eco_risk <- ifelse(df$distance_class %in% target_values,
    #                         1,
    #                         0)


# salva il nuovo file
write.csv(df, "all_centroid_classification_assignment_31_2020_gadus_morhua_heatmap.csv", row.names = FALSE)







