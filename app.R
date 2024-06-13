# Charger les bibliothèques nécessaires
library(shiny)
library(shinydashboard)
library(httr)
library(rvest)
library(dplyr)
library(ggplot2)

# Définir l'interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Taux de Change HTG/USD"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Taux de Change par Banque", tabName = "bank_rates", icon = icon("university")),
      menuItem("Taux du Secteur Informel", tabName = "informal_rates", icon = icon("exchange-alt")),
      menuItem("Taux de Référence", tabName = "reference_rate", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "bank_rates",
              fluidRow(
                box(title = "Taux de Change par Banque", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("bankRatePlot", height = 400))
              ),
              fluidRow(
                box(title = "Banque avec le Taux d'Achat le Plus Élevé", status = "warning", solidHeader = TRUE, width = 6,
                    htmlOutput("highestBuyRateBank")),
                box(title = "Banque avec le Taux de Vente le Plus Élevé", status = "warning", solidHeader = TRUE, width = 6,
                    htmlOutput("highestSellRateBank"))
              ),
              fluidRow(
                box(title = "Banque avec le Taux d'Achat le Plus Bas", status = "info", solidHeader = TRUE, width = 6,
                    htmlOutput("lowestBuyRateBank")),
                box(title = "Banque avec le Taux de Vente le Plus Bas", status = "info", solidHeader = TRUE, width = 6,
                    htmlOutput("lowestSellRateBank"))
              )),
      tabItem(tabName = "informal_rates",
              fluidRow(
                box(title = "Taux du Secteur Informel", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("informalRatePlot", height = 400))
              )),
      tabItem(tabName = "reference_rate",
              fluidRow(
                box(title = "Taux de Référence", status = "primary", solidHeader = TRUE, width = 12,
                    htmlOutput("referenceRate")),
                box(title = "Graphique de Variation du Taux de Référence", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("referenceRatePlot", height = 400))
              ))
    )
  )
)

# Définir la logique serveur
server <- function(input, output, session) {
  
  # Fonction pour récupérer les données du site BRH
  fetch_data <- function() {
    # URL de la page à scraper
    url <- "https://www.brh.ht/taux-affiches-par-les-banques-et-les-agents-de-change-2/"
    
    # Lire le contenu de la page
    page <- read_html(url)
    
    # Extraire le tableau contenant les taux de change
    tables <- page %>% html_nodes("table")
    
    # Extraire le premier tableau
    exchange_rates_table <- html_table(tables[1], fill = TRUE)
    
    # Convertir en data frame
    exchange_rates_df <- as.data.frame(exchange_rates_table)
    
    # Renommer les colonnes (selon les besoins, à adapter)
    colnames(exchange_rates_df) <- c("Banque", "Drop", "Achat", "Vente", "Marge")
    
    # Ajouter les URLs des logos des banques (ajoutez vos URLs ici)
    exchange_rates_df$Logo <- c(
      "https://i0.wp.com/www.brh.ht/brh/wp-content/themes/brh/img/logo_bnc.png?ssl=1",
      "https://play-lh.googleusercontent.com/g6SkmV9eK1pdyVQpJS8x7VtPZnJlQ77GpDtI_U4JRj3fdeDlD7gHZxl8mtzxTL1pZkKD=w240-h480-rw",
      "https://is1-ssl.mzstatic.com/image/thumb/Purple118/v4/0d/11/7e/0d117e9f-7b1b-157f-8932-5a7ddb5346f6/AppIcon-0-1x_U007emarketing-0-0-85-220-6.png/230x0w.webp",
      "https://housinginnovation.co/wp-content/uploads/2020/01/citi.png",
      "https://play-lh.googleusercontent.com/PmB2rC1Unl3JZ8dbl_Vy0oMGK4btov92DFJoo-_709IWl9Da8ZsVigTcC6wudALIXQ",
      "https://www.sogebank.com/wp-content/uploads/2017/12/sogebel.png",
      "https://play-lh.googleusercontent.com/lJYChu_qFF7NqVoWYluKhazd615vzdrDmZp4NtovS4gl3rJhDBxri0xshlC62xNVZUg",
      # Use a default logo for banks without specific logos
      rep("https://via.placeholder.com/150", nrow(exchange_rates_df) - 7)
    )
    
    # Sélectionner les lignes de 1 à 7
    selected_df <- exchange_rates_df %>%
      slice(1:7)
    
    # Supprimer la colonne "Drop"
    selected_df <- selected_df %>%
      select(-Drop)
    
    # Convertir les colonnes de taux en numériques
    selected_df <- selected_df %>%
      mutate(Achat = as.numeric(gsub(",", ".", Achat)),
             Vente = as.numeric(gsub(",", ".", Vente)),
             Marge = as.numeric(gsub(",",".", Marge)))
    
    return(selected_df)
  }
  
  # Fonction pour récupérer les données du site BRH Taux du jour
  get_exchange_rates <- function() {
    # URL de la page à scraper
    url <- "https://www.brh.ht/taux-du-jour/"
    
    # Lire le contenu de la page
    page <- read_html(url)
    
    # Extraire les tableaux contenant les taux et références
    tables <- page %>% html_nodes("table")
    
    # Extraire le premier tableau
    exchange_informel_table <- html_table(tables[1], fill = TRUE)
    
    # Convertir en data frame
    exchange_informel_df <- as.data.frame(exchange_informel_table)
    
    # Renommer les colonnes (à adapter selon les besoins)
    colnames(exchange_informel_df) <- c("Marché", "Achat", "Vente", "Marge")
    
    # Sélectionner les lignes 1 et 4
    selected_exchange_informel_df <- exchange_informel_df %>%
      slice(c(1, 4))
    
    # Convertir les colonnes de taux en numériques
    selected_exchange_informel_df <- selected_exchange_informel_df %>%
      mutate(Achat = as.numeric(gsub(",", ".", Achat)),
             Vente = as.numeric(gsub(",", ".", Vente)),
             Marge = as.numeric(gsub(",", ".", Marge)))
    
    return(selected_exchange_informel_df)
  }
  
  
  # Fonction pour récupérer les données du site BRH Taux du jour
  get_exchange_reference <- function() {
    # URL de la page à scraper
    url <- "https://www.brh.ht/taux-du-jour/"
    
    # Lire le contenu de la page
    page <- read_html(url)
    
    # Extraire les tableaux contenant les taux et références
    tables <- page %>% html_nodes("table")
    
    # Extraire le premier tableau
    exchange_informel_table <- html_table(tables[1], fill = TRUE)
    
    # Convertir en data frame
    exchange_informel_df <- as.data.frame(exchange_informel_table)
    
    # Renommer les colonnes (à adapter selon les besoins)
    colnames(exchange_informel_df) <- c("Marché", "Achat", "Vente", "Marge")
    
    # Convertir les colonnes de taux en numériques
    exchange_informel_df <- exchange_informel_df %>%
      mutate(Achat = as.numeric(gsub(",", ".", Achat)),
             Vente = as.numeric(gsub(",", ".", Vente)),
             Marge = as.numeric(gsub(",", ".", Marge)))
    
    # Sélectionner la ligne 8
    exchange_informel_df <- exchange_informel_df %>% slice(8)
    
    # Supprimer les colonnes Vente et Marge
    exchange_informel_df <- exchange_informel_df %>% select(Marché, Achat)
    
  }
  
  
  # Fonction pour récupérer les données du site BRH variation
  get_exchange_variation <- function() {
    # URL de la page à scraper
    url <- "https://www.brh.ht/taux-du-jour/"
    
    # Lire le contenu de la page
    page <- read_html(url)
    
    # Extraire les tableaux contenant les taux et références
    tables <- page %>% html_nodes("table")
    
    # Extraire le premier tableau
    exchange_informel_table <- html_table(tables[1], fill = TRUE)
    
    # Convertir en data frame
    exchange_informel_df <- as.data.frame(exchange_informel_table, stringsAsFactors = FALSE)
    
    # Renommer les colonnes (à adapter selon les besoins)
    colnames(exchange_informel_df) <- c("Marché", "Achat", "Vente", "Marge")
    
    # Supprimer les colonnes "Vente" et "Marge"
    exchange_informel_df <- exchange_informel_df[, -c(3, 4)]
    
    # Supprimer les lignes avec des valeurs manquantes
    exchange_informel_df <- exchange_informel_df[complete.cases(exchange_informel_df), ]
    
    # Convertir les valeurs avec pourcentage en numériques
    exchange_informel_df$Achat <- as.numeric(gsub(",", ".", gsub("%", "", exchange_informel_df$Achat)))
    
    # Renvoyer seulement les lignes 9 à 11
    return(exchange_informel_df[9:11, ])
  }
  
  # Appel de la fonction pour récupérer seulement les lignes 9 à 11 de la table des taux du jour
  result <- get_exchange_rates()
  result
  
  
  # Récupérer les données 
  data <- fetch_data()
  
  # Graphique des taux de change par banque
  output$bankRatePlot <- renderPlot({
    ggplot(data, aes(x = Banque, y = Vente, fill = Banque)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Vente), vjust = -0.5, color = "black", size = 3) +
      geom_hline(aes(yintercept = mean(Vente, na.rm = TRUE), color = "Moyenne"), linetype = "dashed") +
      scale_color_manual(name = "Légende", values = c("Moyenne" = "red")) +
      theme_minimal() +
      labs(title = "Taux de Change par Banque", x = "Banque", y = "Taux de Change (HTG/USD)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Trouver la banque avec le taux d'achat le plus élevé et le plus bas
  output$highestBuyRateBank <- renderUI({
    highest_buy_rate <- data %>%
      filter(Achat == max(Achat, na.rm = TRUE))
    HTML(paste0("<img src='", highest_buy_rate$Logo, "' height='30'> ", 
                highest_buy_rate$Banque, ": ", highest_buy_rate$Achat, " HTG/USD"))
  })
  
  output$lowestBuyRateBank <- renderUI({
    lowest_buy_rate <- data %>%
      filter(Achat == min(Achat, na.rm = TRUE))
    HTML(paste0("<img src='", lowest_buy_rate$Logo, "' height='30'> ", 
                lowest_buy_rate$Banque, ": ", lowest_buy_rate$Achat, " HTG/USD"))
  })
  
  # Trouver la banque avec le taux de vente le plus élevé et le plus bas
  output$highestSellRateBank <- renderUI({
    highest_sell_rate <- data %>%
      filter(Vente == max(Vente, na.rm = TRUE))
    HTML(paste0("<img src='", highest_sell_rate$Logo, "' height='30'> ", 
                highest_sell_rate$Banque, ": ", highest_sell_rate$Vente, " HTG/USD"))
  })
  
  output$lowestSellRateBank <- renderUI({
    lowest_sell_rate <- data %>%
      filter(Vente == min(Vente, na.rm = TRUE))
    HTML(paste0("<img src='", lowest_sell_rate$Logo, "' height='30'> ", 
                lowest_sell_rate$Banque, ": ", lowest_sell_rate$Vente, " HTG/USD"))
  })
  
  placeholder_data <- get_exchange_rates()
  
  # Graphique des taux du secteur informel
  output$informalRatePlot <- renderPlot({
    ggplot(placeholder_data, aes(x = Marché)) +
      geom_line(aes(y = Achat, color = "Achat"), size = 1) +
      geom_point(aes(y = Achat, color = "Achat"), size = 2) +
      geom_line(aes(y = Vente, color = "Vente"), size = 1) +
      geom_point(aes(y = Vente, color = "Vente"), size = 2) +
      scale_color_manual(name = "Légende", values = c("Achat" = "orange", "Vente" = "blue")) +
      theme_minimal() +
      labs(title = "Comparaison des taux d'achat et de vente",
           x = "Marché",
           y = "Taux de Change (HTG/USD)") +
      guides(color = guide_legend(title = "Taux"))
  })
  
  
  # Afficher le taux de référence
  output$referenceRate <- renderUI({
    reference_rate <- get_exchange_reference()
    HTML(paste0( reference_rate, " HTG/USD"))
  })
  
  # Graphique des variations de taux de référence
  output$referenceRatePlot <- renderPlot({
    data_variation <- get_exchange_variation()
    ggplot(data_variation, aes(x = Marché, y = Achat, label = paste0(Achat, "%"), color = Marché)) +
      geom_line() +
      geom_point(size = 3) +
      geom_text(hjust = -0.2, vjust = -0.5, color = "black", size = 3) + # Ajouter des étiquettes aux points
      theme_minimal() +
      labs(title = "Variation", x = Sys.Date(), y = "Taux de Change (HTG/USD)") +
      scale_color_discrete(name = "Marché") + # Ajouter une légende pour les couleurs
      scale_y_continuous(labels = function(x) paste0(x, "%")) # Ajouter le symbole % aux valeurs de l'axe des y
  })
  
  
  
  
  
}

# Lancer l'application
shinyApp(ui = ui, server = server)
