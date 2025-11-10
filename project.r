# 1 - Libraries
library(shiny) #dashboard
library(readr)#data import for csv
library(dplyr)#data manipulation 
library(scales)#number format
library(janitor)#data "hygien"
library(ggplot2)#graphics
library(plotly)# interactive plots

# 2 - Download the Scorecard ZIP
zip_url  <- "https://ed-public-download.scorecard.network/downloads/Most-Recent-Cohorts-Institution_05192025.zip"
zip_path <- tempfile(fileext = ".zip")
download.file(zip_url, destfile = zip_path, mode = "wb")

# 3 - Check contents of ZIP
files_in_zip <- unzip(zip_path, list = TRUE)
print(files_in_zip)

# 4 - Pick the CSV file that matches the naming pattern (most recent cohort)
inst_file <- files_in_zip$Name[grepl("Most-Recent-Cohorts-Institution", files_in_zip$Name)]

# 5 - Unzip  that file
unzip(zip_path, files = inst_file, exdir = tempdir())
csv_path <- file.path(tempdir(), inst_file)

# 6 - Read the CSV into R
scorecard <- read_csv(csv_path)

# 7 - Quick look at the data
View(scorecard)

# 8 - Select Columns 
scorecard <- scorecard %>%
  select(
    INSTNM, #school name
    STABBR, #state
    PREDDEG, #dominant degree
    CONTROL,#school type
    MN_EARN_WNE_P10, # median earnings
    GRAD_DEBT_MDN,# debt at graduation
    HIGHDEG, # highest degree awareded
    REGION#region
  )

# 9 -  Rename Columns 
scorecard <- scorecard %>%
  rename(
    SCHOOL_NAME = INSTNM,
    STATE = STABBR,
    PRE_DOMINANT_DEGREE_AWARDED = PREDDEG,
    SCHOOL_TYPE = CONTROL,
    MEDIAN_EARNINGS_10_YEARS = MN_EARN_WNE_P10,
    MEDIAN_DEBT_FOR_GRAD = GRAD_DEBT_MDN,
    HIGHEST_DEGREE_AWARDED = HIGHDEG
  )

#10 - Bucket SCHOOL_TYPE 
scorecard <- scorecard %>%
  mutate(
    SCHOOL_TYPE = recode(
      as.character(SCHOOL_TYPE),
      `1` = "PUBLIC",
      `2` = "PRIVATE NON-PROFIT",
      `3` = "PRIVATE FOR-PROFIT"
    )
  )

#11 - Bucket PREDOMINANT DEGREE 
scorecard <- scorecard %>%
  mutate(
    PRE_DOMINANT_DEGREE_AWARDED = recode(
      as.character(PRE_DOMINANT_DEGREE_AWARDED),
      `0` = "NOT CLASSIFIED",
      `1` = "CERTIFICATE",
      `2` = "ASSOCIATE",
      `3` = "BACHELOR",
      `4` = "GRADUATE DEGREE"
    )
  )

# 12 - Bucket REGION 
scorecard <- scorecard %>%
  mutate(
    REGION = recode(
      as.character(REGION),
      `0` = "U.S. Service Schools",
      `1` = "New England (CT, ME, MA, NH, RI, VT)",
      `2` = "Mid East (DE, DC, MD, NJ, NY, PA)",
      `3` = "Great Lakes (IL, IN, MI, OH, WI)",
      `4` = "Plains (IA, KS, MN, MO, NE, ND, SD)",
      `5` = "Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV)",
      `6` = "Southwest (AZ, NM, OK, TX)",
      `7` = "Rocky Mountains (CO, ID, MT, UT, WY)",
      `8` = "Far West (AK, CA, HI, NV, OR, WA)",
      `9` = "Outlying Areas (AS, FM, GU, MH, MP, PR, PW, VI)"
    )
  )

# 13 - Bucket HIGHEST DEGREE 
scorecard <- scorecard %>%
  mutate(
    HIGHEST_DEGREE_AWARDED = recode(
      as.character(HIGHEST_DEGREE_AWARDED),
      `0` = "Non-degree granting",
      `1` = "Certificate",
      `2` = "Associate",
      `3` = "Bachelor",
      `4` = "Graduate or higher"
    )
  )

# 14 format Money Columns & Compute ROI 
scorecard <- scorecard %>%
  mutate(
    MEDIAN_EARNINGS_10_YEARS = as.numeric(gsub("[^0-9.-]", "", MEDIAN_EARNINGS_10_YEARS)),
    MEDIAN_DEBT_FOR_GRAD     = as.numeric(gsub("[^0-9.-]", "", MEDIAN_DEBT_FOR_GRAD)),
    ROI = round(MEDIAN_EARNINGS_10_YEARS / MEDIAN_DEBT_FOR_GRAD, 2)
  )

#15 cleans blank roi
scorecard <- scorecard %>% filter(!is.na(ROI), is.finite(ROI))

#view scorecard
View(scorecard)

#######start of dashboard#############
#1- start ui (what I see)
ui <- fluidPage(
  titlePanel("College ROI Dashboard"),
  
  #filters on the left
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectInput("region", "Region:", c("All", sort(unique(scorecard$REGION))), "All"),
      selectInput("state", "State:", c("All", sort(unique(scorecard$STATE))), "All"),
      checkboxGroupInput("school_type", "School Type:",
                         sort(unique(scorecard$SCHOOL_TYPE)),
                         sort(unique(scorecard$SCHOOL_TYPE))),
      checkboxGroupInput("degree", "Predominant Degree Awarded:",
                         sort(unique(scorecard$PRE_DOMINANT_DEGREE_AWARDED)),
                         sort(unique(scorecard$PRE_DOMINANT_DEGREE_AWARDED))),
      
      #uncheck box" / reset button 
      actionButton("clear_checks", "Uncheck all school/degree types"),
      
      selectInput("school", "School (optional):",
                  c("All", sort(unique(scorecard$SCHOOL_NAME))), "All"),
      helpText("Filters apply to all tabs.")
    ),
    
    mainPanel(
      #Global summary strip 
      fluidRow(
        column(3,
               h5("Average ROI"),
               textOutput("avg_roi")
        ),
        column(3,
               h5("Median Earnings (10y)"),
               textOutput("median_earnings")
        ),
        column(3,
               h5("Median Debt"),
               textOutput("median_debt")
        ),
        column(3,
               h5("Number of Schools"),
               textOutput("school_count")
        )
      ),
      br(),
      
      tabsetPanel(
        tabPanel("Q1: Earnings by School Type",
                 br(), p("How do median 10-year earnings compare across school types?"),
                 plotOutput("plot_q1")),
        tabPanel("Q2: ROI by Region",
                 br(), p("Which regions have the highest and lowest median ROI?"),
                 plotOutput("plot_q2")),
        tabPanel("Q3: ROI by Degree & Type",
                 br(), p("Within each school type, how does ROI vary by predominant degree awarded?"),
                 plotOutput("plot_q3")),
        tabPanel("Q4: Debt vs Earnings (Grad Schools)",
                 br(), p("How are debt and earnings related?"),
                 plotlyOutput("plot_q4")),
        tabPanel("Q5: Top & Bottom ROI Schools",
                 br(), h4("Top 10 by ROI"), tableOutput("top10"),
                 br(), h4("Bottom 10 by ROI"), tableOutput("bottom10"))
      )
    )
  )
)

#server (what goes on behind the scenes)
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- scorecard
    if (input$region != "All") df <- df %>% filter(REGION == input$region)
    if (input$state != "All") df <- df %>% filter(STATE == input$state)
    if (!is.null(input$school_type)) df <- df %>% filter(SCHOOL_TYPE %in% input$school_type)
    if (!is.null(input$degree)) df <- df %>% filter(PRE_DOMINANT_DEGREE_AWARDED %in% input$degree)
    if (input$school != "All") df <- df %>% filter(SCHOOL_NAME == input$school)
    df
  })
  
  #uncheck box resets everything
  observeEvent(input$clear_checks, {
    # reset dropdowns
    updateSelectInput(session, "region", selected = "All")
    updateSelectInput(session, "state",  selected = "All")
    updateSelectInput(session, "school", selected = "All")
    
    # reset checkbox groups to "everything selected"
    updateCheckboxGroupInput(
      session, "school_type",
      selected = sort(unique(scorecard$SCHOOL_TYPE))
    )
    
    updateCheckboxGroupInput(
      session, "degree",
      selected = sort(unique(scorecard$PRE_DOMINANT_DEGREE_AWARDED))
    )
  })
  
  #kpi math
  output$avg_roi <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("No data")
    dollar(round(mean(df$ROI, na.rm = TRUE), 2), accuracy = 0.01)
  })
  
  output$median_earnings <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("No data")
    dollar(round(median(df$MEDIAN_EARNINGS_10_YEARS, na.rm = TRUE), 0))
  })
  
  output$median_debt <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("No data")
    dollar(round(median(df$MEDIAN_DEBT_FOR_GRAD, na.rm = TRUE), 0))
  })
  
  output$school_count <- renderText({
    df <- filtered_data()
    formatC(nrow(df), format = "d", big.mark = ",")
  })
  
  #starts viz's
  # Q1: Earnings by School Type
  output$plot_q1 <- renderPlot({
    df <- filtered_data() %>%
      group_by(SCHOOL_TYPE) %>%
      summarise(MEDIAN_EARNINGS = median(MEDIAN_EARNINGS_10_YEARS, na.rm = TRUE), .groups = "drop")
    
    ggplot(df, aes(x = reorder(SCHOOL_TYPE, MEDIAN_EARNINGS), y = MEDIAN_EARNINGS)) +
      geom_col(fill = "#2E86C1") +
      geom_text(aes(label = dollar(MEDIAN_EARNINGS)), hjust = -0.1, size = 4) +
      coord_flip() +
      labs(
        title = "",
        x = "School Type",
        y = "Median 10-Year Earnings"
      ) +
      scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0, 0.1))) +
      theme_minimal()
  })
  
  # Q2: ROI by Region
  output$plot_q2 <- renderPlot({
    df <- filtered_data() %>%
      group_by(REGION) %>%
      summarise(MEAN_ROI = mean(ROI, na.rm = TRUE), .groups = "drop")
    
    ggplot(df, aes(x = reorder(REGION, MEAN_ROI), y = MEAN_ROI)) +
      geom_col(fill = "#27AE60") +
      geom_text(aes(label = dollar(round(MEAN_ROI, 2))), hjust = -0.1, size = 3.5) +
      coord_flip() +
      labs(
        title = "",
        x = "Region",
        y = "Average ROI (Earnings ÷ Debt)"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      theme_minimal()
  })
  
  
  # Q3: ROI by Degree & Type 
  output$plot_q3 <- renderPlot({
    df <- filtered_data() %>%
      group_by(SCHOOL_TYPE, PRE_DOMINANT_DEGREE_AWARDED) %>%
      summarise(MEDIAN_ROI = median(ROI, na.rm = TRUE), .groups = "drop")
    
    degree_order <- c("NOT CLASSIFIED", "CERTIFICATE", "ASSOCIATE", "BACHELOR", "GRADUATE DEGREE")
    df$PRE_DOMINANT_DEGREE_AWARDED <- factor(df$PRE_DOMINANT_DEGREE_AWARDED, levels = degree_order)
    
    ggplot(df, aes(x = PRE_DOMINANT_DEGREE_AWARDED, y = MEDIAN_ROI, fill = SCHOOL_TYPE)) +
      geom_col(position = position_dodge(width = 0.8)) +
      geom_text(
        aes(label = dollar(round(MEDIAN_ROI, 2))),
        position = position_dodge(width = 0.8),
        vjust = -0.3,
        size = 3
      ) +
      labs(
        title = "",
        x = "Predominant Degree Awarded",
        y = "Median ROI (Earnings ÷ Debt)",
        fill = "School Type"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        plot.margin = margin(10, 20, 10, 20)
      )
  }, width = 900, height = 500)
  
  
  # Q4: Debt vs Earnings (Grad Schools) – interactive + summary box
  output$plot_q4 <- renderPlotly({
    df <- filtered_data() %>%
      filter(HIGHEST_DEGREE_AWARDED == "Graduate or higher")
    
    if (nrow(df) == 0) return(NULL)
    
    # highest ROI school
    top_roi <- df %>%
      filter(ROI == max(ROI, na.rm = TRUE)) %>%
      slice(1)
    
    # lowest ROI school
    bottom_roi <- df %>%
      filter(ROI == min(ROI, na.rm = TRUE)) %>%
      slice(1)
    
    summary_label <- paste0(
      "Highest ROI: ", top_roi$SCHOOL_NAME,
      "\n  ROI: ", round(top_roi$ROI, 2),
      "\n  Earnings: ", dollar(top_roi$MEDIAN_EARNINGS_10_YEARS),
      "\n  Debt: ", dollar(top_roi$MEDIAN_DEBT_FOR_GRAD),
      "\n\nLowest ROI: ", bottom_roi$SCHOOL_NAME,
      "\n  ROI: ", round(bottom_roi$ROI, 2),
      "\n  Earnings: ", dollar(bottom_roi$MEDIAN_EARNINGS_10_YEARS),
      "\n  Debt: ", dollar(bottom_roi$MEDIAN_DEBT_FOR_GRAD)
    )
    
    p <- ggplot(
      df,
      aes(
        x = MEDIAN_DEBT_FOR_GRAD,
        y = MEDIAN_EARNINGS_10_YEARS,
        text = paste0(
          "School: ", SCHOOL_NAME,
          "<br>Median Earnings (10y): ", dollar(MEDIAN_EARNINGS_10_YEARS),
          "<br>Median Debt: ", dollar(MEDIAN_DEBT_FOR_GRAD),
          "<br>ROI: ", round(ROI, 2)
        )
      )
    ) +
      geom_point(alpha = 0.5, color = "#8E44AD") +
      geom_smooth(method = "lm", se = FALSE, color = "gray40") +
      labs(
        title = "",
        x = "Median Debt at Graduation",
        y = "Median 10-Year Earnings"
      ) +
      scale_x_continuous(labels = dollar_format()) +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal()
    
    #adds summary box
    ggplotly(p, tooltip = "text") %>%
      layout(
        annotations = list(
          list(
            x = 1, y = 0.98,
            xref = "paper", yref = "paper",
            xanchor = "right",
            yanchor = "top",
            text = gsub("\n", "<br>", summary_label),
            showarrow = FALSE,
            align = "left",
            bordercolor = "black",
            borderwidth = 1,
            bgcolor = "rgba(255,255,255,0.8)",
            font = list(size = 12)
          )
        ),
        margin = list(r = 180)
      )
  })
  
  
  # Q5: Top & Bottom 10 ROI
  output$top10 <- renderTable({
    filtered_data() %>%
      arrange(desc(ROI)) %>%
      mutate(
        MEDIAN_EARNINGS_10_YEARS = dollar(MEDIAN_EARNINGS_10_YEARS),
        MEDIAN_DEBT_FOR_GRAD     = dollar(MEDIAN_DEBT_FOR_GRAD),
        ROI                      = dollar(round(ROI, 2))
      ) %>%
      select(SCHOOL_NAME, STATE, SCHOOL_TYPE, PRE_DOMINANT_DEGREE_AWARDED,
             MEDIAN_EARNINGS_10_YEARS, MEDIAN_DEBT_FOR_GRAD, ROI) %>%
      head(10)
  })
  
  output$bottom10 <- renderTable({
    filtered_data() %>%
      arrange(ROI) %>%
      mutate(
        MEDIAN_EARNINGS_10_YEARS = dollar(MEDIAN_EARNINGS_10_YEARS),
        MEDIAN_DEBT_FOR_GRAD     = dollar(MEDIAN_DEBT_FOR_GRAD),
        ROI                      = dollar(round(ROI, 2))
      ) %>%
      select(SCHOOL_NAME, STATE, SCHOOL_TYPE, PRE_DOMINANT_DEGREE_AWARDED,
             MEDIAN_EARNINGS_10_YEARS, MEDIAN_DEBT_FOR_GRAD, ROI) %>%
      head(10)
  })
}


# RUN APP
shinyApp(ui = ui, server = server)

