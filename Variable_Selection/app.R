
library(shiny)
library(tidyverse)
library(ctsem)
fed = read.csv("https://www.dropbox.com/s/xtt4emmz6txtbun/fed_agency_capacity_autonomy.csv?dl=1")
df = fed %>%
    filter(yr != 1973) %>%
    filter(yr >= 1974) %>%
    filter(med_sal_ > 0) %>% 
    filter(n > 5) %>%
    drop_na(med_sal_) %>%
    filter(AGYSUB != "TOTL")%>% ### Removes Total Columns 
    filter(!grepl("NET", agy_full)) %>% ### Removes any total agency counts, ie only individual agencies are left
    mutate(logMA_pct = log(ma_pct + 1)) %>%
    mutate(logLST_pct = log(LST_pct + 1)) %>%
    mutate(logAIN_pct = log(AIN_pct + 1)) %>%
    mutate(logPHDpct = log(doc_pct + 1)) %>%
    mutate(logn = log(n)) 

env = df %>%
    filter(agy_typ == "Natural Resources and Environment") %>%
    select(AGYSUB,
           agy_full,
           yr,
           n,
           n_roll,
           b18_roll,
           b18_d_roll,
           ma_ed,
           doc_ed,
           ma_pct,
           doc_pct,
           LOSavg,
           med_sal_,
           gini)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Make Dynamic Models"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            sliderInput("years",
                        "Year Range:",
                        min = 1974,
                        max = 2019,
                        value = c(1980, 1990)),
            
            checkboxGroupInput("regressors",
                               "Choose Regressors",
                               c("N Employees" = "n",
                                 "N Roll" = "n_roll",
                                 "Budget Roll" = "b18_roll",
                                 "Discr Budget Roll" = "b18_d_roll",
                                 "N Masters" = "ma_ed",
                                 "N PhD" = "doc_ed",
                                 "MA %" = "ma_pct",
                                 "PhD %" = "doc_pct",
                                 "Length of Service Avg" = "LOSavg",
                                 "Median Salary" = "med_sal_",
                                 "Gini" = "gini"),
                               selected = c("n",
                                            "ma_pct",
                                            "med_sal_"))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = 4 + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
