library(shiny)
library(shinydashboard)
library(ggplot2)
library(viridisLite)
library(readr)
library(dplyr)
library(lattice)
library(tidyr)
library(plotly)
library(ggsci)


dane <- read.csv("homeschool.csv", header = TRUE)

#przygotowanie danych
#attach(dane)

#usuwanie duplikatów
dane <- distinct(dane)

# usuwanie zbędnych kolumn
#dane<- dane %>% select(-"idTerytWojewodztwo", -"idTypPodmiotu",-"idKategoriaUczniow", -"idPublicznosc", -"idTypDanaDziedzinowa", -"idKlasa",                       -"idSpecyfikaSzkoly")

dane <- distinct(dane) %>% 
  select(-idTerytWojewodztwo, -idTypPodmiotu, -idKategoriaUczniow, 
         -idPublicznosc, -idTypDanaDziedzinowa, -idKlasa, -idSpecyfikaSzkoly) # %>% 
#  mutate(Rok = as.character(Rok), Klasa = as.integer(Klasa), 
#         LiczbaUczniów = as.integer(LiczbaUczniów))

# Wyodrębnienie danych dla kolejnych lat
dane_2019 <- dane %>% filter(Rok == 2019)
dane_2020 <- dane %>% filter(Rok == 2020)
dane_2021 <- dane %>% filter(Rok == 2021)

# Sumowanie liczby uczniów w szkołach publicznych i niepublicznych w kolejnych latach
dane_sum_2019 <- dane_2019 %>% group_by(Publiczność) %>% summarise(LiczbaUczniów = sum(LiczbaUczniów))
dane_sum_2020 <- dane_2020 %>% group_by(Publiczność) %>% summarise(LiczbaUczniów = sum(LiczbaUczniów))
dane_sum_2021 <- dane_2021 %>% group_by(Publiczność) %>% summarise(LiczbaUczniów = sum(LiczbaUczniów))

# Sumowanie liczby uczniów w szkołach specjalnych i zwykłych w kolejnych latach
dane_sum2_2019 <- dane_2019 %>% group_by(SpecyfikaSzkoły) %>% summarise(LiczbaUczniów = sum(LiczbaUczniów))
dane_sum2_2020 <- dane_2020 %>% group_by(SpecyfikaSzkoły) %>% summarise(LiczbaUczniów = sum(LiczbaUczniów))
dane_sum2_2021 <- dane_2021 %>% group_by(SpecyfikaSzkoły) %>% summarise(LiczbaUczniów = sum(LiczbaUczniów))

dane$Rok <- factor(dane$Rok)

# Define UI for application that draws a histogram
ui <- navbarPage("Analiza ilości uczniów korzystających z nauczania domowego w latach 2019-2021 - przygotowanie Patrycja Baczewska",

                 tabPanel("Stosunek liczby uczniów szkół prywatnych do publicznych",
                 sidebarLayout(
                   sidebarPanel(
                     radioButtons("rok", "Wybierz rok", choices = unique(dane$Rok), selected = "2021")
                   ),
                   mainPanel(
                     plotOutput("wykres1")
                   )
                 )
                 ),
                 
                 tabPanel("Liczebność klas w zależności od województwa i roku",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("WYBOR1", "Wybierz województwo:", 
                                          sort(unique(dane$Wojewodztwo)), 
                                          selected = "MAZOWIECKIE"),
                              radioButtons("WYBOR2", "Wybierz rok:",choices = list(
                                "2019",
                                "2020",
                                "2021"
                                ),
                                selected = "2019")
                            ),
                            
                            mainPanel(
                              plotOutput("PLOT1")
                            )
                          )
                 ),
                 
                 tabPanel("Liczebność uczniów w kolejnych latach według klas",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("SLIDER2", "Zaznacz klasy:", min = -1, max = 8, value = c(4, 6), step = 1),
                              br(), tags$label("Legenda:"), br(),
                              tags$label(" -1 -  wychowanie przedszkolne"), br(),
                              tags$label("  0 -  zerówka"), br(),
                              tags$label("1-8 - kolejne klasy szkoły podstawowej")
                            ),
                            
                            mainPanel(
                              plotOutput("PLOT2")
                            )
                          )
                 ),
                 
                 tabPanel("Liczebność uczniów w zależności od rodzaju szkoły i roku",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("WYBOR3", "Wybierz rodzaj szkoły:", choices = c(unique(dane$TypPodmiotu)))
                            ),
                            
                            mainPanel(
                              plotOutput("PLOT3")
                            )
                          )
                 ),
                 
                 tabPanel("Stosunek liczby uczniów szkół specjalnych do zwykłych",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("rok2", "Wybierz rok", choices = unique(dane$Rok), selected = "2021")
                            ),
                            mainPanel(
                              plotOutput("wykres2")
                            )
                          )
                 ),
                 tabPanel("Zestawienie ilości uczniów w poszczególnych klasach według lat",
                            mainPanel(
                              plotlyOutput("wykres3", width = "150%", height = "700px")
                            )
                 )
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$wykres1 <- renderPlot({
    
    if(input$rok == 2019){
      ggplot(data = dane_sum_2019, aes(x="", y=LiczbaUczniów, fill=Publiczność)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        scale_fill_manual(values = c("#FFB6C1", "#32CD32")) + 
        labs(fill="Typ szkoły") +
        ggtitle("Stosunek uczniów szkół publicznych do niepublicznych w 2019 roku")
    }
    else if(input$rok == 2020) {
      ggplot(data = dane_sum_2020, aes(x="", y=LiczbaUczniów, fill=Publiczność)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        scale_fill_manual(values = c("#FFB6C1", "#32CD32")) + 
        labs(fill="Typ szkoły") +
        ggtitle("Stosunek uczniów szkół publicznych do niepublicznych w 2020 roku")
    }
    else if(input$rok == 2021) {
      ggplot(data = dane_sum_2021, aes(x="", y=LiczbaUczniów, fill=Publiczność)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        scale_fill_manual(values = c("#FFB6C1", "#32CD32")) + 
        labs(fill="Typ szkoły") +
        ggtitle("Stosunek uczniów szkół publicznych do niepublicznych w 2021 roku")
    }
    
  })
  
  # funkcja generująca wykres
  output$PLOT1 <- renderPlot({
    # dane do wykresu
    dane_do_wykresu <- dane[dane$Wojewodztwo == input$WYBOR1 & dane$Rok == input$WYBOR2, ]
    dane_do_wykresu$Klasa <- factor(dane_do_wykresu$Klasa, levels = sort(unique(dane_do_wykresu$Klasa)))
    # wykres
    ggplot(dane_do_wykresu, aes(x = Klasa, y = LiczbaUczniów, fill = Klasa)) + 
      geom_col() +
      scale_fill_manual(values = brewer.pal(length(unique(dane_do_wykresu$Klasa)), "Set1")) +
      labs(title = paste0("Liczba uczniów w klasach dla ", input$WYBOR1, " w roku ", input$WYBOR2),
           x = "Klasa",
           y = "Liczba uczniów")
  })
  
  output$PLOT2 <- renderPlot({
  # wybór wierszy, których klasa mieści się w przedziale od min do max
  min <- input$SLIDER2[1]
  max <- input$SLIDER2[2]
  dane_wybrane <- dane %>%
    filter(Klasa >= min & Klasa <= max)
  
  # zsumowanie liczby uczniów dla każdego roku i klasy
  dane_wybrane <- dane_wybrane %>%
    group_by(Rok, Klasa) %>%
    summarize(LiczbaUczniów = sum(LiczbaUczniów))
  
  # utworzenie wykresu słupkowego
  ggplot(dane_wybrane, aes(x = Rok, y = LiczbaUczniów, fill = factor(Klasa))) +
    geom_col(position = "dodge") +
    labs(x = "Rok", y = "Liczba uczniów", fill = "Klasa")
  }
  )

  
  output$PLOT3 <- renderPlot({
    dane_do_wykresu <- dane %>%
      filter(TypPodmiotu %in% input$WYBOR3) %>%
      group_by(Rok, TypPodmiotu) %>%
      summarise(LiczbaUczniów = sum(LiczbaUczniów))
    
    dane_do_wykresu$Rok <- factor(dane_do_wykresu$Rok)
    
    ggplot(dane_do_wykresu, aes(x = Rok, y = LiczbaUczniów, color = TypPodmiotu, group = TypPodmiotu)) + 
      geom_point(size = 4) +
      geom_line(size = 1) +
      scale_x_discrete(labels = as.character(dane_do_wykresu$Rok)) +
      labs(title = "Liczebność uczniów w zależności od rodzaju szkoły i roku",
           x = "Rok",
           y = "Liczba uczniów",
           color = "Typ szkoły") +
      theme_bw()
  })
  
  output$wykres2 <- renderPlot({
    
    if(input$rok2 == 2019){
      ggplot(data = dane_sum2_2019, aes(x="", y=LiczbaUczniów, fill=SpecyfikaSzkoły)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        labs(fill="Typ szkoły") +
        ggtitle("Stosunek uczniów szkół specjalnych do zwykłych w 2019 roku")
    }
    else if(input$rok2 == 2020) {
      ggplot(data = dane_sum2_2020, aes(x="", y=LiczbaUczniów, fill=SpecyfikaSzkoły)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        labs(fill="Typ szkoły") +
        ggtitle("Stosunek uczniów szkół specjalnych do zwykłych w 2020 roku")
    }
    else if(input$rok2 == 2021) {
      ggplot(data = dane_sum2_2021, aes(x="", y=LiczbaUczniów, fill=SpecyfikaSzkoły)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        labs(fill="Typ szkoły") +
        ggtitle("Stosunek uczniów szkół specjalnych do zwykłych w 2021 roku")
    }
  })
  
  output$wykres_3d <- renderPlotly({
    dane_do_wykresu <- dane %>%
      group_by(Rok, Klasa) %>%
      summarise(LiczbaUczniów = sum(LiczbaUczniów)) %>%
      ungroup()
    
    dane_do_wykresu$Rok <- factor(dane_do_wykresu$Rok)
    
    fig <- plot_ly(dane_do_wykresu, x = ~Rok, y = ~Klasa, z = ~LiczbaUczniów, 
                   type = "surface", colors = c("#FF69B4", "#32CD32"))
    
    fig %>% layout(scene = list(xaxis = list(title = "Rok"), 
                                yaxis = list(title = "Klasa"), 
                                zaxis = list(title = "Liczba uczniów")))
  })
  
  output$wykres3 <- renderPlotly({
    wykres <- plot_ly(dane, x = ~Rok, y = ~Klasa, z = ~LiczbaUczniów, type = "scatter3d",
                      mode = "markers",
                      marker = list(size = 5, color = ~LiczbaUczniów, colorscale = 'YlOrRd'),
                      hoverinfo = "text",
                      text = ~paste("Rok: ", Rok, "<br>",
                                    "Klasa: ", Klasa, "<br>",
                                    "Liczba uczniów: ", LiczbaUczniów, "<br>")) %>%
      layout(scene = list(xaxis = list(title = "Rok"),
                          yaxis = list(title = "Klasa"),
                          zaxis = list(title = "Liczba uczniów")),
             margin = list(l = 0, r = 0, b = 0, t = 0),
             title = "Liczba uczniów w zależności od roku i klasy",
             showlegend = FALSE)
    
    # Wyświetlamy wykres
    wykres
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
