library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(arrow)
library(here)
library(shiny)

dat <- get_data()

dat <- dat[dat$age_months <= 50, ]

dat$lang_1 <- ifelse(is.na(dat$lang_1), "none", dat$lang_1)
dat$lang_2 <- ifelse(is.na(dat$lang_2), "none", dat$lang_2)
dat$lang_3 <- ifelse(is.na(dat$lang_3), "none", dat$lang_3)

dat$doe_1 <- ifelse(is.na(dat$doe_1), 0, dat$doe_1)
dat$doe_2 <- ifelse(is.na(dat$doe_2), 0, dat$doe_2)
dat$doe_3 <- ifelse(is.na(dat$doe_3), 0, dat$doe_3)

all_langs <- unique(unlist(dat[, c("lang_1", "lang_2", "lang_3")]))
all_langs <- ifelse(all_langs=="spaa", "spa", all_langs)
all_langs <- ifelse(all_langs=="cata", "cat", all_langs)
all_langs <- ifelse(all_langs=="enga", "eng", all_langs)
all_langs <- sort(all_langs)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Babylab Database"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("birthday", "Birth date", min = min(dat$birthday, na.rm = TRUE), 
                           max = today(), start = min(dat$birthday, na.rm = TRUE), end = today()),
            sliderInput("age_months", "Age (months)", value = c(0, 32), min = 0, max = ceiling(max(dat$age_months, na.rm = TRUE))),
            selectInput("sex", "Sex", choices = unique(dat$sex), multiple = TRUE, selected =  unique(dat$sex)),
            sliderInput("gestation_weeks", "Min. gestation weeks", value = 36, min = 0, max = max(dat$gestation_weeks, na.rm = TRUE)),
            sliderInput("weight", "Birth weight (g)", value = 2, min = 0, max = max(dat$weight, na.rm = TRUE)),
            selectInput("lang_1", "L1", choices = all_langs, multiple = TRUE, selected = c("cat", "eng", "spa")),
            selectInput("lang_2", "L2", choices = all_langs, multiple = TRUE, selected = c("cat", "eng", "spa", "none")),
            selectInput("lang_3", "L3", choices = all_langs, multiple = TRUE, selected = c("cat", "eng", "spa", "none")),
            sliderInput("doe_1", "L1 exposure (%)", value = c(0, 1), min = 0, max = 1),
            sliderInput("doe_2", "L2 exposure (%)", value = c(0, 1), min = 0, max = 1),
            sliderInput("doe_3", "L3 exposure (%)", value = c(0, 1), min = 0, max = 1),
            numericInput("bilingualism_threshold", "Bilingualism threshold (%)", min = 0.01, max = 0.49, value = 0.2),
            width = 2
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(tableOutput("summary_table")),
            br(),
            fluidRow(dataTableOutput("main_table"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dat_main <- reactive({
        dat_main <- dat[
            dat$birthday >= input$birthday[1] & 
                dat$birthday <= input$birthday[2] &
                dat$age_months >= input$age_months[1] &
                dat$age_months <= input$age_months[2] &
                dat$sex %in% input$sex &
                dat$gestation_weeks >= input$gestation_weeks &
                dat$lang_1 %in% input$lang_1 &
                dat$lang_2 %in% input$lang_2 &
                dat$lang_3 %in% input$lang_3 &
                dat$doe_1 >= input$doe_1[1] & dat$doe_1 <= input$doe_1[2] &
                dat$doe_2 >= input$doe_2[1] & dat$doe_2 <= input$doe_2[2] &
                dat$doe_3 >= input$doe_3[1] & dat$doe_3 <= input$doe_3[2]
            ,] 
        
        dat_main$lp <- ifelse(dat_main$doe_1 > (1-input$bilingualism_threshold), "Monolingual", "Bilingual")
        dat_main$birthday <- as.character(dat_main$birthday)
        dat_main$age_months <- round(dat_main$age_months, 2)
        
        dat_main$doe_1 <- percent(dat_main$doe_1, accuracy = 1)
        dat_main$doe_2 <- percent(dat_main$doe_2, accuracy = 1)
        dat_main$doe_3 <- percent(dat_main$doe_3, accuracy = 1)
        
        dat_main <- dat_main[, c("id_baby", "birthday", "sex", "age_months", "lp", "lang_1", "lang_2", "lang_3", "doe_1", "doe_2", "doe_3")]
        
        return(dat_main)
    })
    
    
    output$main_table <- renderDataTable({ dat_main() })
    
    dat_summary <- reactive({
        dat <- dat_main()
        dat$age_bin <- as.integer(floor(dat$age_months))
        dat_summary <- count(dat, age_bin, lp)
        dat_summary <- pivot_wider(dat_summary, names_from = age_bin, values_from = n, values_fill = 0)
        return(dat_summary)
    })
    
    output$summary_table <- renderTable({ dat_summary() }, striped = TRUE)
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)
