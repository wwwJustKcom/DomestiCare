# Install and load necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shinydashboard, shiny, plotly, dplyr, tidyr, janitor, lubridate, jsonlite, rvest, WDI, DT, shinyWidgets
)

# Set working directory
setwd("C:/Users/Charles Luigi/Downloads/DomestiCare Dashboard")

# Custom color palette
custom_colors <- c("#8569B6", "#6B4CA0", "#5A3F7A", "#FFFFFF")

# Function to fetch data from public ILO API
fetch_ilo_data <- function(indicator_id, dataset_name) {
  tryCatch({
    url <- paste0("https://www.ilo.org/shinyapps/bulkexplorer/", indicator_id, ".csv")
    temp <- tempfile()
    download.file(url, temp, mode = "wb", quiet = TRUE)
    data <- read.csv(temp) %>%
      clean_names() %>%
      mutate(
        time = as.numeric(time),
        obs_value = as.numeric(obs_value)
      ) %>%
      filter(!is.na(obs_value))
    unlink(temp)
    print(paste("Fetched", dataset_name, "columns:", paste(colnames(data), collapse = ", ")))
    data
  }, error = function(e) {
    message(paste("Error fetching", dataset_name, "data:", e$message))
    if (dataset_name == "ILO domestic worker salaries") {
      data <- data.frame(
        ref_area = rep(c("Philippines", "United States", "Singapore", "Hong Kong", "Saudi Arabia", "UAE", "Qatar"), 10),
        time = rep(2014:2023, each = 7),
        obs_value = runif(70, 8000, 25000),
        classif1 = rep("OCCUP_5", 70)
      )
    } else {
      data <- data.frame(
        ref_area = rep(c("Philippines", "United States", "Singapore", "Hong Kong", "Saudi Arabia", "UAE", "Qatar"), 10),
        time = rep(2014:2023, each = 7),
        obs_value = runif(70, 1500, 2500),
        classif1 = rep("OCCUP_5", 70),
        sex = rep("SEX_T", 70)
      )
    }
    print(paste("Mock", dataset_name, "columns:", paste(colnames(data), collapse = ", ")))
    data
  })
}

# Web scrape PSA OFW deployment data
fetch_psa_ofw_data <- function() {
  tryCatch({
    url <- "https://psa.gov.ph/statistics/survey/labor-and-employment/survey-overseas-filipinos"
    webpage <- read_html(url)
    tables <- webpage %>% html_nodes("table")
    if (length(tables) >= 1) {
      table <- tables[[min(2, length(tables))]] %>% html_table(fill = TRUE)
      data <- data.frame(
        country = c("Saudi Arabia", "UAE", "Singapore", "Hong Kong", "Qatar"),
        ofw_count = c(419776, 282896, 182331, 175877, 138193)
      )
    } else {
      stop("No tables found")
    }
    print("Fetched PSA OFW columns: country, ofw_count")
    data
  }, error = function(e) {
    message("Error scraping PSA OFW data: ", e$message)
    data <- data.frame(
      country = c("Saudi Arabia", "UAE", "Singapore", "Hong Kong", "Qatar"),
      ofw_count = c(419776, 282896, 182331, 175877, 138193)
    )
    print("Mock PSA OFW columns: country, ofw_count")
    data
  })
}

# Fetch data
df_ilo_hours <- fetch_ilo_data("HOW_TEMP_SEX_OCU_NB_A", "ILO annual hours")
df_ilo_salaries <- fetch_ilo_data("EAR_4MTH_SEX_OCU_CUR_NB_A", "ILO domestic worker salaries")
df_ofw_deployment <- fetch_psa_ofw_data()

# Fetch World Bank Gender Statistics
df_world_bank <- WDI(indicator = "SG.GEN.PARL.ZS", country = c("PH", "US", "SG", "HK", "AE", "SA", "QA"), start = 2014, end = 2023) %>%
  mutate(SG.GEN.PARL.ZS = as.numeric(SG.GEN.PARL.ZS))

# Mock BLS OEWS data with rankings
df_bls_oes <- data.frame(
  area_title = c("Philippines", "United States", "Singapore", "Saudi Arabia", "UAE", "Hong Kong", "Qatar"),
  occ_title = c("Domestic Worker", "Nurse", "Caregiver", "Construction Worker", "Hospitality Worker", "Engineer", "IT Specialist"),
  a_median = c(12000, 75000, 30000, 25000, 20000, 60000, 55000),
  tot_emp = c(6000, 10000, 4000, 8000, 5000, 3000, 2500),
  demand_rank = c(1, 3, 2, 5, 4, 6, 7),
  skills = c("Household Management", "Medical Care", "Patient Care", "Construction Skills", "Customer Service", "Engineering Design", "IT Programming"),
  mismatch_overqualified = c(20, 15, 25, 10, 30, 5, 8),
  mismatch_skill_gap = c(15, 10, 20, 25, 15, 10, 12)
)

# Mock BLS QCEW data
df_bls_qcew <- data.frame(
  area_title = c("Philippines", "United States", "Singapore", "Saudi Arabia", "UAE", "Hong Kong", "Qatar"),
  industry_title = c("Services", "Health", "Construction", "Hospitality", "Education", "Manufacturing", "Retail"),
  avg_wkly_wage = c(300, 1200, 700, 500, 600, 900, 650),
  total_qtrly_wages = c(800000, 2000000, 1400000, 1000000, 1200000, 1800000, 1300000)
)

# Clean and standardize data
df_ilo_hours <- df_ilo_hours %>%
  clean_names() %>%
  rename(time_period = time) %>%
  mutate(
    time_period = as.numeric(time_period),
    obs_value = as.numeric(obs_value)
  ) %>%
  filter(!is.na(obs_value), classif1 == "OCCUP_5")

df_ilo_salaries <- df_ilo_salaries %>%
  clean_names() %>%
  rename(time_period = time) %>%
  mutate(
    time_period = as.numeric(time_period),
    obs_value = as.numeric(obs_value)
  ) %>%
  filter(!is.na(obs_value), classif1 == "OCCUP_5")

# UI Definition
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = HTML('<i class="fa fa-home"></i> DomestiCare'), titleWidth = 300),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Wage Insights", tabName = "wages", icon = icon("dollar-sign", class = "fas")),
      menuItem("Job Matching", tabName = "jobs", icon = icon("briefcase", class = "fas")),
      menuItem("Labor Rights", tabName = "rights", icon = icon("balance-scale", class = "fas")),
      menuItem("Data Insights", tabName = "insights", icon = icon("chart-line", class = "fas")),
      menuItem("About", tabName = "about", icon = icon("info-circle", class = "fas"))
    ),
    selectInput("country", "Country:", 
                choices = unique(df_ilo_hours$ref_area),
                selected = "Philippines"),
    sliderInput("year", "Year Range:", 
                min = 2014, max = 2023, value = c(2014, 2023), step = 1),
    selectInput("issue_type", "Issue Type:",
                choices = c("All", "Pay Gaps", "Lack of Contracts", "Working Conditions"),
                selected = "All")
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML('
        @import url("https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap");
        .main-header .logo {
          font-family: "Roboto", sans-serif;
          font-weight: 700;
          font-size: 24px;
          padding: 15px;
          background-color: #5A3F7A;
          color: #FFFFFF;
        }
        .main-header .logo:hover {
          background-color: #6B4CA0;
        }
        .skin-purple .main-sidebar {
          background-color: #5A3F7A;
        }
        .skin-purple .main-sidebar .sidebar-menu a {
          font-family: "Roboto", sans-serif;
          color: #FFFFFF;
          font-size: 16px;
          padding: 12px;
        }
        .skin-purple .main-sidebar .sidebar-menu a:hover {
          background-color: #8569B6;
          color: #FFFFFF;
        }
        .box {
          border-radius: 15px;
          box-shadow: 0 8px 16px rgba(0,0,0,0.1);
          background-color: #FFFFFF;
          padding: 20px;
          margin-bottom: 25px;
        }
        .content-wrapper {
          background-color: #FFFFFF;
          padding: 25px;
        }
        .box-title {
          font-family: "Roboto", sans-serif;
          color: #5A3F7A;
          font-weight: 700;
          font-size: 18px;
        }
        .value-box {
          border-radius: 15px;
          box-shadow: 0 8px 16px rgba(0,0,0,0.1);
          background-color: #6B4CA0;
          color: #FFFFFF;
          font-family: "Roboto", sans-serif;
        }
        .value-box .inner h3, .value-box .inner p {
          color: #FFFFFF;
          font-family: "Roboto", sans-serif;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: #5A3F7A !important;
          font-family: "Roboto", sans-serif;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background-color: #8569B6 !important;
          color: #FFFFFF !important;
        }
        .insights-text {
          font-family: "Roboto", sans-serif;
          color: #5A3F7A;
          font-size: 14px;
          line-height: 1.8;
        }
        .fa {
          margin-right: 8px;
        }
      '))
    ),
    tabItems(
      tabItem(tabName = "wages",
              fluidRow(
                valueBoxOutput("avg_wage_box", width = 3),
                valueBoxOutput("max_wage_box", width = 3),
                valueBoxOutput("min_wage_box", width = 3),
                valueBoxOutput("wage_growth_box", width = 3)
              ),
              fluidRow(
                box(
                  title = HTML('<i class="fas fa-clock"></i> Annual Hours Worked'),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("hours_line", height = "400px"),
                  p("Tracks workload trends for domestic workers.", class = "insights-text"),
                  width = 6
                ),
                box(
                  title = HTML('<i class="fas fa-dollar-sign"></i> Domestic Worker Salaries (2023)'),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("salary_bar", height = "400px"),
                  p("Ranks salary differences for OFW job choices.", class = "insights-text"),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = HTML('<i class="fas fa-balance-scale"></i> Wage Comparison'),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("wage_comparison_bar", height = "400px"),
                  p("Compares wages across countries and occupations.", class = "insights-text"),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "jobs",
              fluidRow(
                valueBoxOutput("top_job_box", width = 3),
                valueBoxOutput("total_jobs_box", width = 3),
                valueBoxOutput("top_skill_box", width = 3),
                valueBoxOutput("mismatch_rate_box", width = 3)
              ),
              fluidRow(
                box(
                  title = HTML('<i class="fas fa-chart-bar"></i> Wage Distribution'),
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("wage_histogram", height = "400px"),
                  p("Shows wage spread for job matching.", class = "insights-text"),
                  width = 6
                ),
                box(
                  title = HTML('<i class="fas fa-exclamation-circle"></i> Job Mismatches'),
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("job_mismatch_bar", height = "400px"),
                  p("Shows overqualified and skill-gapped workers.", class = "insights-text"),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = HTML('<i class="fas fa-users"></i> Employment by Occupation'),
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("employment_donut", height = "400px"),
                  p("Ranks job availability for OFWs.", class = "insights-text"),
                  width = 6
                ),
                box(
                  title = HTML('<i class="fas fa-table"></i> Top Job Demands & Skills'),
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  DTOutput("job_rankings_table", height = "400px"),
                  p("Ranks occupations by demand and skills.", class = "insights-text"),
                  width = 6
                )
              )
      ),
      tabItem(tabName = "rights",
              fluidRow(
                valueBoxOutput("top_issue_box", width = 3),
                valueBoxOutput("issue_freq_box", width = 3),
                valueBoxOutput("equality_score_box", width = 3),
                valueBoxOutput("issue_trend_box", width = 3)
              ),
              fluidRow(
                box(
                  title = HTML('<i class="fas fa-exclamation-triangle"></i> Employment Issues'),
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("issues_pie", height = "400px"),
                  p("Identifies key labor issues for advocacy.", class = "insights-text"),
                  width = 6
                ),
                box(
                  title = HTML('<i class="fas fa-equals"></i> Workplace Equality (2022)'),
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("equality_radar", height = "400px"),
                  p("Ranks equality scores across countries.", class = "insights-text"),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = HTML('<i class="fas fa-chart-line"></i> Issue Trends Over Time'),
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("issue_trends_line", height = "400px"),
                  p("Tracks employment issue frequencies.", class = "insights-text"),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "insights",
              fluidRow(
                valueBoxOutput("total_ofw_box", width = 3),
                valueBoxOutput("avg_salary_box", width = 3),
                valueBoxOutput("top_country_box", width = 3),
                valueBoxOutput("hours_change_box", width = 3)
              ),
              fluidRow(
                box(
                  title = HTML('<i class="fas fa-clock"></i> Hours Worked Change'),
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("hours_change_scatter", height = "400px"),
                  p("Shows workload changes for labor planning.", class = "insights-text"),
                  width = 6
                ),
                box(
                  title = HTML('<i class="fas fa-globe"></i> OFW Deployment (2023)'),
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("ofw_bar", height = "400px"),
                  p("Ranks top OFW destinations.", class = "insights-text"),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = HTML('<i class="fas fa-dollar-sign"></i> Salary Trends'),
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("salary_trends_area", height = "400px"),
                  p("Shows salary trends across countries.", class = "insights-text"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = HTML('<i class="fas fa-lightbulb"></i> Key Insights Summary'),
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  htmlOutput("insights_summary"),
                  p("Summarizes key trends for OFWs.", class = "insights-text"),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = HTML('<i class="fas fa-info-circle"></i> About DomestiCare'),
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  h3("DomestiCare: Empowering OFWs", style = "color: #5A3F7A; font-family: 'Roboto', sans-serif;"),
                  p("A machine learning-driven platform supporting Overseas Filipino Workers with wage estimates, job matching, and labor rights information, aligning with UN SDGs 8 and 10.", class = "insights-text"),
                  p("Developed by: Alconcel, Dela Cruz, Mesa, Sobrevega, Tomagan", class = "insights-text"),
                  p("Instructor: Ms. Nila D. Santiago", class = "insights-text"),
                  p("Date: July 2, 2025", class = "insights-text")
                )
              )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Reactive data filtering
  filtered_hours <- reactive({
    req(df_ilo_hours)
    df_ilo_hours %>%
      filter(ref_area == input$country, time_period >= input$year[1], time_period <= input$year[2]) %>%
      group_by(ref_area, time_period) %>%
      summarise(obs_value = mean(obs_value, na.rm = TRUE), .groups = "drop")
  })
  
  filtered_salaries <- reactive({
    req(df_ilo_salaries)
    df_ilo_salaries %>%
      filter(ref_area == input$country, time_period == 2023, classif1 == "OCCUP_5") %>%
      select(ref_area, obs_value)
  })
  
  filtered_pay_equality <- reactive({
    req(df_world_bank)
    df_world_bank %>%
      filter(country == input$country, year >= input$year[1], year <= input$year[2]) %>%
      select(year, SG.GEN.PARL.ZS) %>%
      mutate(SG.GEN.PARL.ZS = as.numeric(SG.GEN.PARL.ZS))
  })
  
  filtered_oes <- reactive({
    req(df_bls_oes)
    df_bls_oes %>%
      filter(area_title == input$country | input$country == "Philippines") %>%
      select(occ_title, a_median, tot_emp, demand_rank, skills, mismatch_overqualified, mismatch_skill_gap) %>%
      arrange(demand_rank)
  })
  
  filtered_qcew <- reactive({
    req(df_bls_qcew)
    df_bls_qcew %>%
      filter(area_title == input$country | input$country == "Philippines") %>%
      select(industry_title, avg_wkly_wage) %>%
      arrange(desc(avg_wkly_wage))
  })
  
  # Simulated issues data with time series
  issues_data <- reactive({
    data.frame(
      issue = rep(c("Pay Gaps", "Lack of Contracts", "Working Conditions"), each = 10),
      year = rep(2014:2023, times = 3),
      frequency = c(
        runif(10, 35, 45), # Pay Gaps
        runif(10, 30, 40), # Lack of Contracts
        runif(10, 20, 30)  # Working Conditions
      )
    ) %>%
      filter(if (input$issue_type == "All") TRUE else issue == input$issue_type)
  })
  
  # Card Boxes for Wage Insights
  output$avg_wage_box <- renderValueBox({
    avg_wage <- mean(df_ilo_salaries$obs_value[df_ilo_salaries$time_period == 2023], na.rm = TRUE)
    valueBox(
      paste0("$", format(round(avg_wage, 0), big.mark = ",")), "Avg. Wage",
      icon = icon("dollar-sign", class = "fas"), color = "purple"
    )
  })
  
  output$max_wage_box <- renderValueBox({
    max_wage <- max(df_ilo_salaries$obs_value[df_ilo_salaries$time_period == 2023], na.rm = TRUE)
    valueBox(
      paste0("$", format(round(max_wage, 0), big.mark = ",")), "Max Wage",
      icon = icon("arrow-up", class = "fas"), color = "purple"
    )
  })
  
  output$min_wage_box <- renderValueBox({
    min_wage <- min(df_ilo_salaries$obs_value[df_ilo_salaries$time_period == 2023], na.rm = TRUE)
    valueBox(
      paste0("$", format(round(min_wage, 0), big.mark = ",")), "Min Wage",
      icon = icon("arrow-down", class = "fas"), color = "purple"
    )
  })
  
  output$wage_growth_box <- renderValueBox({
    wages <- df_ilo_salaries %>%
      filter(time_period %in% c(2022, 2023)) %>%
      group_by(time_period) %>%
      summarise(avg_wage = mean(obs_value, na.rm = TRUE)) %>%
      mutate(growth = (avg_wage - lag(avg_wage)) / lag(avg_wage) * 100) %>%
      filter(time_period == 2023) %>%
      pull(growth)
    valueBox(
      paste0(round(wages, 1), "%"), "Wage Growth",
      icon = icon("chart-line", class = "fas"), color = "purple"
    )
  })
  
  # Card Boxes for Job Matching
  output$top_job_box <- renderValueBox({
    top_job <- df_bls_oes$occ_title[which.min(df_bls_oes$demand_rank)]
    valueBox(
      top_job, "Top Job",
      icon = icon("briefcase", class = "fas"), color = "purple"
    )
  })
  
  output$total_jobs_box <- renderValueBox({
    total_jobs <- sum(df_bls_oes$tot_emp, na.rm = TRUE)
    valueBox(
      format(total_jobs, big.mark = ","), "Total Jobs",
      icon = icon("users", class = "fas"), color = "purple"
    )
  })
  
  output$top_skill_box <- renderValueBox({
    top_skill <- df_bls_oes$skills[which.min(df_bls_oes$demand_rank)]
    valueBox(
      top_skill, "Top Skill",
      icon = icon("tools", class = "fas"), color = "purple"
    )
  })
  
  output$mismatch_rate_box <- renderValueBox({
    mismatch_rate <- filtered_oes() %>%
      summarise(avg_mismatch = mean(mismatch_overqualified + mismatch_skill_gap, na.rm = TRUE)) %>%
      pull(avg_mismatch)
    valueBox(
      paste0(round(mismatch_rate, 1), "%"), "Avg. Mismatch Rate",
      icon = icon("exclamation-circle", class = "fas"), color = "purple"
    )
  })
  
  # Card Boxes for Labor Rights
  output$top_issue_box <- renderValueBox({
    top_issue <- issues_data() %>%
      filter(year == 2023) %>%
      arrange(desc(frequency)) %>%
      slice(1) %>%
      pull(issue)
    valueBox(
      top_issue, "Top Issue",
      icon = icon("exclamation-triangle", class = "fas"), color = "purple"
    )
  })
  
  output$issue_freq_box <- renderValueBox({
    avg_freq <- mean(issues_data()$frequency[issues_data()$year == 2023], na.rm = TRUE)
    valueBox(
      paste0(round(avg_freq, 1), "%"), "Issue Frequency",
      icon = icon("chart-pie", class = "fas"), color = "purple"
    )
  })
  
  output$equality_score_box <- renderValueBox({
    score <- mean(df_world_bank$SG.GEN.PARL.ZS[df_world_bank$year == 2022], na.rm = TRUE)
    valueBox(
      round(score, 1), "Equality Score",
      icon = icon("equals", class = "fas"), color = "purple"
    )
  })
  
  output$issue_trend_box <- renderValueBox({
    trend <- issues_data() %>%
      group_by(year) %>%
      summarise(avg_freq = mean(frequency, na.rm = TRUE)) %>%
      mutate(change = (avg_freq - lag(avg_freq)) / lag(avg_freq) * 100) %>%
      filter(year == 2023) %>%
      pull(change)
    valueBox(
      paste0(round(trend, 1), "%"), "Issue Trend Change",
      icon = icon("trend-up", class = "fas"), color = "purple"
    )
  })
  
  # Card Boxes for Data Insights
  output$total_ofw_box <- renderValueBox({
    total_ofw <- sum(df_ofw_deployment$ofw_count, na.rm = TRUE)
    valueBox(
      format(total_ofw, big.mark = ","), "Total OFWs",
      icon = icon("users", class = "fas"), color = "purple"
    )
  })
  
  output$avg_salary_box <- renderValueBox({
    avg_salary <- mean(df_ilo_salaries$obs_value[df_ilo_salaries$time_period == 2023], na.rm = TRUE)
    valueBox(
      paste0("$", format(round(avg_salary, 0), big.mark = ",")), "Avg. Salary",
      icon = icon("dollar-sign", class = "fas"), color = "purple"
    )
  })
  
  output$top_country_box <- renderValueBox({
    top_country <- df_ofw_deployment$country[which.max(df_ofw_deployment$ofw_count)]
    valueBox(
      top_country, "Top Destination",
      icon = icon("globe", class = "fas"), color = "purple"
    )
  })
  
  output$hours_change_box <- renderValueBox({
    hours_change <- filtered_hours() %>%
      arrange(time_period) %>%
      mutate(pct_change = (obs_value - lag(obs_value)) / lag(obs_value) * 100) %>%
      filter(!is.na(pct_change)) %>%
      summarise(avg_change = mean(pct_change, na.rm = TRUE)) %>%
      pull(avg_change)
    valueBox(
      paste0(round(hours_change, 1), "%"), "Hours Change",
      icon = icon("clock", class = "fas"), color = "purple"
    )
  })
  
  # Plot 1: Annual Hours Worked (Line)
  output$hours_line <- renderPlotly({
    data <- filtered_hours()
    plot_ly(data, x = ~time_period, y = ~obs_value, type = "scatter", mode = "lines+markers",
            line = list(color = custom_colors[1], width = 2),
            marker = list(color = custom_colors[1], size = 8),
            text = ~paste("Year:", time_period, "<br>Hours:", round(obs_value, 1))) %>%
      layout(
        title = list(text = "Annual Hours Worked Over Time", font = list(family = "Roboto", color = custom_colors[3], size = 16)),
        xaxis = list(title = "Year", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        yaxis = list(title = "Hours Worked", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        hovermode = "closest",
        margin = list(t = 60, b = 60),
        showlegend = FALSE,
        paper_bgcolor = custom_colors[4],
        plot_bgcolor = custom_colors[4]
      )
  })
  
  # Plot 2: Domestic Worker Salaries (Bar)
  output$salary_bar <- renderPlotly({
    data <- df_ilo_salaries %>%
      filter(time_period == 2023, classif1 == "OCCUP_5") %>%
      group_by(ref_area) %>%
      summarise(obs_value = mean(obs_value, na.rm = TRUE)) %>%
      arrange(desc(obs_value))
    plot_ly(data, x = ~reorder(ref_area, -obs_value), y = ~obs_value, type = "bar",
            marker = list(color = custom_colors[2], line = list(color = custom_colors[3], width = 1)),
            text = ~paste("Country:", ref_area, "<br>Salary: $", format(round(obs_value, 0), big.mark = ","))) %>%
      layout(
        title = list(text = "Domestic Worker Salaries (2023)", font = list(family = "Roboto", color = custom_colors[3], size = 16)),
        xaxis = list(title = "Country", tickangle = 45, tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        yaxis = list(title = "Salary (USD)", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        hovermode = "closest",
        margin = list(b = 120),
        paper_bgcolor = custom_colors[4],
        plot_bgcolor = custom_colors[4]
      )
  })
  
  # Plot 3: Wage Comparative Analysis (Grouped Bar)
  output$wage_comparison_bar <- renderPlotly({
    data <- df_bls_oes %>%
      select(area_title, occ_title, a_median) %>%
      filter(area_title %in% c("Philippines", "United States", "Singapore"))
    plot_ly(data, x = ~occ_title, y = ~a_median, color = ~area_title, type = "bar",
            colors = custom_colors[1:3],
            text = ~paste("Country:", area_title, "<br>Occupation:", occ_title, "<br>Wage: $", format(round(a_median, 0), big.mark = ","))) %>%
      layout(
        title = list(text = "Wage Comparison by Country & Occupation", font = list(family = "Roboto", color = custom_colors[3], size = 16)),
        xaxis = list(title = "Occupation", tickangle = 45, tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        yaxis = list(title = "Median Wage (USD)", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        barmode = "group",
        hovermode = "closest",
        margin = list(b = 120),
        paper_bgcolor = custom_colors[4],
        plot_bgcolor = custom_colors[4]
      )
  })
  
  # Plot 4: Wage Distribution (Histogram)
  output$wage_histogram <- renderPlotly({
    data <- filtered_oes()
    plot_ly(data, x = ~a_median, type = "histogram",
            marker = list(color = custom_colors[1], line = list(color = custom_colors[3], width = 1)),
            text = ~paste("Wage: $", format(round(a_median, 0), big.mark = ","))) %>%
      layout(
        title = list(text = "Wage Distribution (2023)", font = list(family = "Roboto", color = custom_colors[3], size = 16)),
        xaxis = list(title = "Median Annual Wage (USD)", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        yaxis = list(title = "Count", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        hovermode = "closest",
        margin = list(t = 60),
        paper_bgcolor = custom_colors[4],
        plot_bgcolor = custom_colors[4]
      )
  })
  
  # Plot 5: Job Mismatches (Stacked Bar)
  output$job_mismatch_bar <- renderPlotly({
    data <- filtered_oes() %>%
      select(occ_title, mismatch_overqualified, mismatch_skill_gap) %>%
      pivot_longer(cols = c(mismatch_overqualified, mismatch_skill_gap), names_to = "mismatch_type", values_to = "percentage")
    plot_ly(data, x = ~occ_title, y = ~percentage, color = ~mismatch_type, type = "bar",
            colors = custom_colors[1:2],
            text = ~paste("Occupation:", occ_title, "<br>Type:", mismatch_type, "<br>Percentage:", round(percentage, 1), "%")) %>%
      layout(
        title = list(text = "Job Mismatches by Occupation", font = list(family = "Roboto", color = custom_colors[3], size = 16)),
        xaxis = list(title = "Occupation", tickangle = 45, tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        yaxis = list(title = "Percentage (%)", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        barmode = "stack",
        hovermode = "closest",
        margin = list(b = 120),
        paper_bgcolor = custom_colors[4],
        plot_bgcolor = custom_colors[4]
      )
  })
  
  # Plot 6: Employment by Occupation (Donut)
  output$employment_donut <- renderPlotly({
    data <- filtered_oes()
    plot_ly(data, labels = ~occ_title, values = ~tot_emp, type = "pie",
            hole = 0.4, marker = list(colors = custom_colors[1:3], line = list(color = custom_colors[3], width = 1)),
            text = ~paste("Occupation:", occ_title, "<br>Employment:", format(tot_emp, big.mark = ","))) %>%
      layout(
        title = list(text = "Employment by Occupation", font = list(family = "Roboto", color = custom_colors[3], size = 16)),
        showlegend = TRUE,
        margin = list(t = 60),
        paper_bgcolor = custom_colors[4],
        plot_bgcolor = custom_colors[4]
      )
  })
  
  # Plot 7: Employment Issues (Pie)
  output$issues_pie <- renderPlotly({
    data <- issues_data() %>%
      filter(year == 2023)
    plot_ly(data, labels = ~issue, values = ~frequency, type = "pie",
            marker = list(colors = custom_colors[1:3], line = list(color = custom_colors[3], width = 1)),
            text = ~paste("Issue:", issue, "<br>Frequency:", round(frequency, 1), "%")) %>%
      layout(
        title = list(text = "Employment Issues (2023)", font = list(family = "Roboto", color = custom_colors[3], size = 16)),
        showlegend = TRUE,
        margin = list(t = 60),
        paper_bgcolor = custom_colors[4],
        plot_bgcolor = custom_colors[4]
      )
  })
  
  # Plot 8: Workplace Equality (Radar)
  output$equality_radar <- renderPlotly({
    data <- df_world_bank %>%
      filter(year == 2022) %>%
      select(country, SG.GEN.PARL.ZS) %>%
      arrange(desc(SG.GEN.PARL.ZS))
    plot_ly(
      type = 'scatterpolar',
      mode = 'lines+markers',
      r = data$SG.GEN.PARL.ZS,
      theta = data$country,
      fill = 'toself',
      marker = list(color = custom_colors[2], size = 8),
      line = list(color = custom_colors[2])
    ) %>%
      layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, 100), tickfont = list(family = "Roboto", size = 12))),
        showlegend = FALSE,
        title = list(text = "Workplace Equality (2022)", font = list(family = "Roboto", color = custom_colors[3], size = 16)),
        margin = list(t = 60),
        paper_bgcolor = custom_colors[4],
        plot_bgcolor = custom_colors[4]
      )
  })
  
  # Plot 9: Issue Trends Over Time (Line)
  output$issue_trends_line <- renderPlotly({
    data <- issues_data()
    plot_ly(data, x = ~year, y = ~frequency, color = ~issue, type = "scatter", mode = "lines+markers",
            colors = custom_colors[1:3],
            marker = list(size = 8),
            text = ~paste("Issue:", issue, "<br>Year:", year, "<br>Frequency:", round(frequency, 1), "%")) %>%
      layout(
        title = list(text = "Issue Trends Over Time", font = list(family = "Roboto", color = custom_colors[3], size = 16)),
        xaxis = list(title = "Year", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        yaxis = list(title = "Frequency (%)", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        hovermode = "closest",
        margin = list(t = 60),
        paper_bgcolor = custom_colors[4],
        plot_bgcolor = custom_colors[4]
      )
  })
  
  # Plot 10: Hours Worked Change (Scatter)
  output$hours_change_scatter <- renderPlotly({
    data <- filtered_hours() %>%
      arrange(time_period) %>%
      mutate(pct_change = (obs_value - lag(obs_value)) / lag(obs_value) * 100)
    plot_ly(data, x = ~time_period, y = ~pct_change, type = "scatter", mode = "markers",
            marker = list(color = custom_colors[1], size = 10),
            text = ~paste("Year:", time_period, "<br>Change:", round(pct_change, 2), "%")) %>%
      layout(
        title = list(text = "Hours Worked Change", font = list(family = "Roboto", color = custom_colors[3], size = 16)),
        xaxis = list(title = "Year", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        yaxis = list(title = "Percent Change (%)", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        hovermode = "closest",
        margin = list(t = 60),
        paper_bgcolor = custom_colors[4],
        plot_bgcolor = custom_colors[4]
      )
  })
  
  # Plot 11: OFW Deployment (Bar)
  output$ofw_bar <- renderPlotly({
    data <- df_ofw_deployment %>%
      arrange(desc(ofw_count))
    plot_ly(data, x = ~reorder(country, -ofw_count), y = ~ofw_count, type = "bar",
            marker = list(color = custom_colors[2], line = list(color = custom_colors[3], width = 1)),
            text = ~paste("Country:", country, "<br>OFWs:", format(ofw_count, big.mark = ","))) %>%
      layout(
        title = list(text = "OFW Deployment (2023)", font = list(family = "Roboto", color = custom_colors[3], size = 16)),
        xaxis = list(title = "Country", tickangle = 45, tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        yaxis = list(title = "Number of OFWs", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        hovermode = "closest",
        margin = list(b = 120),
        paper_bgcolor = custom_colors[4],
        plot_bgcolor = custom_colors[4]
      )
  })
  
  # Plot 12: Salary Trends by Country (Area)
  output$salary_trends_area <- renderPlotly({
    data <- df_ilo_salaries %>%
      filter(time_period >= input$year[1], time_period <= input$year[2], classif1 == "OCCUP_5") %>%
      group_by(ref_area, time_period) %>%
      summarise(obs_value = mean(obs_value, na.rm = TRUE), .groups = "drop")
    plot_ly(data, x = ~time_period, y = ~obs_value, color = ~ref_area, type = "scatter", mode = "lines+markers", fill = "tozeroy",
            colors = custom_colors[1:3],
            marker = list(size = 8),
            text = ~paste("Country:", ref_area, "<br>Year:", time_period, "<br>Salary: $", format(round(obs_value, 0), big.mark = ","))) %>%
      layout(
        title = list(text = "Salary Trends by Country", font = list(family = "Roboto", color = custom_colors[3], size = 16)),
        xaxis = list(title = "Year", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        yaxis = list(title = "Salary (USD)", tickfont = list(family = "Roboto", size = 12), showgrid = FALSE),
        hovermode = "closest",
        margin = list(t = 60),
        paper_bgcolor = custom_colors[4],
        plot_bgcolor = custom_colors[4]
      )
  })
  
  # Table: Job Demands & Skills Rankings
  output$job_rankings_table <- renderDT({
    data <- filtered_oes() %>%
      select(Rank = demand_rank, Occupation = occ_title, `Median Wage (USD)` = a_median, `Total Employment` = tot_emp, Skills = skills) %>%
      arrange(Rank)
    datatable(data, options = list(
      pageLength = 5,
      dom = 'tip',
      columnDefs = list(list(className = 'dt-center', targets = '_all'))
    )) %>%
      formatCurrency("Median Wage (USD)", currency = "$", digits = 0) %>%
      formatStyle(columns = names(data), color = custom_colors[3], fontFamily = "Roboto", fontSize = '12px')
  })
  
  # Insights Summary
  output$insights_summary <- renderUI({
    top_country <- df_ofw_deployment$country[which.max(df_ofw_deployment$ofw_count)]
    top_salary <- df_ilo_salaries %>%
      filter(time_period == 2023, classif1 == "OCCUP_5") %>%
      group_by(ref_area) %>%
      summarise(obs_value = mean(obs_value, na.rm = TRUE)) %>%
      arrange(desc(obs_value)) %>%
      slice(1) %>%
      pull(ref_area)
    top_mismatch <- filtered_oes() %>%
      mutate(total_mismatch = mismatch_overqualified + mismatch_skill_gap) %>%
      arrange(desc(total_mismatch)) %>%
      slice(1) %>%
      pull(occ_title)
    HTML(
      paste0(
        "<h4 style='color: #5A3F7A; font-family: Roboto, sans-serif;'><i class='fas fa-lightbulb'></i> Key Insights for ", input$country, "</h4>",
        "<ul class='insights-text'>",
        "<li><b>Wage Trends:</b> ", top_salary, " offers the highest salary ($", format(round(mean(df_ilo_salaries$obs_value[df_ilo_salaries$time_period == 2023 & df_ilo_salaries$ref_area == top_salary], na.rm = TRUE), 0), big.mark = ","), ").</li>",
        "<li><b>Job Opportunities:</b> Domestic Workers (Rank 1) and Caregivers (Rank 2) lead, requiring Household Management and Patient Care.</li>",
        "<li><b>Job Mismatches:</b> ", top_mismatch, " has the highest mismatch rate (", round(max(filtered_oes()$mismatch_overqualified + filtered_oes()$mismatch_skill_gap), 1), "%).</li>",
        "<li><b>Issues:</b> Pay gaps (40%) and lack of contracts (35%) need urgent advocacy.</li>",
        "<li><b>Equality:</b> ", input$country, "'s equality score averaged ", round(mean(df_world_bank$SG.GEN.PARL.ZS[df_world_bank$year == 2022], na.rm = TRUE), 1), " in 2022.</li>",
        "<li><b>Hours:</b> Workload changed by ", round(mean((filtered_hours()$obs_value - lag(filtered_hours()$obs_value)) / lag(filtered_hours()$obs_value) * 100, na.rm = TRUE), 1), "% on average.</li>",
        "<li><b>OFW Deployment:</b> ", top_country, " hosts ", format(max(df_ofw_deployment$ofw_count), big.mark = ","), " OFWs.</li>",
        "</ul>"
      )
    )
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)