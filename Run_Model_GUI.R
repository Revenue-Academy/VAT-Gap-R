library(shinydashboard)
library(DT)
library(readxl)
library(shinyjs)
library(plotly)
library(data.table)
library(fontawesome)
library(shiny)
library(ggplot2)
library(tidyverse)
library(kableExtra)
library(stringr)
library(reshape2)





# I.Define the UI ---------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "VAT-GAP MODEL"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input", icon = icon("database")),
      menuItem("Simulation parameters", tabName = "simulationParameters", icon = icon("edit")),
      menuItem("Run simulation", tabName = "simulation", icon = icon("calculator")),
      menuItem("Results", tabName = "Result", icon = icon("gauge")),
      menuItem("Charts", tabName = "charts", icon = icon("chart-line"), #startExpanded = TRUE,
              # menuSubItem("Historic", tabName = "Historic"), 
               menuSubItem("SUT", tabName = "SUT"),
               menuSubItem("SimulatedResults", tabName = "SimulatedResults"),
               menuSubItem("TaxExpenditures", tabName = "TaxExpenditures")
      ),
      # Add a new tabItem for the Summary tab
      menuItem("Summary", tabName = "summary", icon = icon("info"))
    )
  ),
   dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "input",
        fluidRow(
          column(6,
                 selectInput("inputType", "Data Source",
                             choices = c("Excel File"),
                             selected = "Excel File"),
                 conditionalPanel(
                   condition = "input.inputType == 'Excel File'",
                   fileInput("fileInput", "Upload Excel File", accept = c(".xlsx")),
                   checkboxInput("hasHeader", "Header", TRUE)
                 ),
                 actionButton("resetInput", "Reset")
                )
              )
            ),
      tabItem(
        tabName = "simulation",
        fluidRow(
          column(6,
                 sliderInput("simulationSlider", "VAT benchmark rate",
                             min = 0, max = 1, step = 0.01, value = 0.18),
                 actionButton("runSimulation", "Run Simulation")
          ),
          column(6,
                 # Table to display the slider values
                 tableOutput("sliderValueTable")
          )
        )
      ),
      tabItem(
        tabName = "simulationParameters",
        fluidRow(
          column(
            12,
            DTOutput("excelDataTable"),
            br(),
            actionButton("updateGlobalData", "Update"),
            actionButton("resetGlobalData", "Reset Imported Data")
          )
        )
      ),
# Results
tabItem(
  tabName = "Result",
  fluidRow(
    column(12,
           # Add your table here using DTOutput or any other appropriate UI element
           DTOutput("TableOutputId")  # Replace "yourTableOutputId" with a unique identifier
           #uiOutput("TableOutputId")  # Replace "yourTableOutputId" with a unique identifier
    )
  )
),
# Charts tab
      tabItem(
        tabName = "Historic",
        fluidRow(
          column(6,
                 selectInput("chartSelectHistoric", "Select Chart",
                             choices = c(
                               "Nominal_GDP", "VAT_Share_GDP","Structure_VAT_Revenues", "VAT_revenue_pct" ,"VAT_Rev_Pct_GDP_Africa" #,"VAT_C_Efficiency_Africa"
                             ),
                             selected = "Nominal_GDP")
          )
        ),
        fluidRow(
          column(12,
                 plotlyOutput("chartOutputHistoric", height = "700px")
          )
        )
      ),
      tabItem(
        tabName = "SUT",
        fluidRow(
          column(6,
                 selectInput("chartSelectSUT", "Select Chart",
                             choices = c(
                               "Supply_Output", "Supply_Import_CIF","Supply_basic_prices","Supply_purchasers_prices",
                               "Use_total_intermediate_consumption","Use_final_consumtion_househodls","Use_Final_consumption_NPISH",
                               "Use_Final_consumption_government","Use_Final_consumption_total","VAT_revenues_industries", "VAT_revenues_NPISH","VAT_revenues_Government","VAT_revenues_Households","VAT_revenues_Total"
                             ),
                             selected = "TotalOutput_SUT")
          )
        ),
        fluidRow(
          column(12,
                 plotlyOutput("chartOutputSUT", height = "700px")
          )
        )
      ),
      tabItem(
        tabName = "SimulatedResults",
        fluidRow(
          column(12,
                 valueBoxOutput("SimulatedResults",width = 30)
          )
        ),
        fluidRow(
          column(6,
                 selectInput("chartSelectResults", "Select Chart",
                             choices = c("ComparisonOfRevenues","DifferenceVAT_revenues"),
                             selected = "ComparisonOfRevenues")
          )
        ),
        fluidRow(
          column(12,
                 plotlyOutput("chartOutputResults", height = "700px")
          )
        )
      ),
      tabItem(
        tabName = "TaxExpenditures",
        fluidRow(
          column(12,
                 valueBoxOutput("TaxExpenditures",width = 30)#,
          )
        ),
        fluidRow(
          column(6,
                 selectInput("chartTaxExpenditures", "Select Chart",
                             choices = c("TaxExpenditure_plt"),
                             selected = "TaxExpenditure_plt")
          )
        ),
        fluidRow(
          column(12,
                 plotlyOutput("chartOutputTaxExpenditures", height = "700px")
          )
        )
      ),      
      # Summary tab
tabItem(
  tabName = "summary",
  fluidRow(
    uiOutput("infoBoxUI"),
    column(12,
           plotlyOutput("treemap_final_plt", height = "700px")
              )
            )
          )

        ),
)
)


# II. Define the server logic -------------------------------------

server <- function(input, output, session) {
  
  # Create a reactive data frame to store the editable Excel data
  originalExcelData <- reactiveVal(NULL)
  editedExcelData <- reactiveVal(NULL)
  
  observe({
    if (!is.null(input$fileInput)) {
      excel_data <- read_excel(input$fileInput$datapath)
      originalExcelData(excel_data)
      editedExcelData(excel_data)
    }
  })
  
  # Define a DataTable using the reactive data frame
  output$excelDataTable <- renderDT({
    data <- editedExcelData()
    if (!is.null(data)) {
      datatable(
        data,
        caption = "Taxable proportions and VAT rates by CPA",
        editable = TRUE,
        options = list(
          pageLength = 15,
          columnDefs = list(
            list(
              targets = c(0:4),
              className = "not-editable"
            )
          )
        )
      )
    }
  })
  
  # Observer to update the reactive data frame when the DataTable is edited
  observeEvent(input$excelDataTable_cell_edit, {
    info <- input$excelDataTable_cell_edit
    
    if (!is.null(editedExcelData())) {
      modifiedData <- editedExcelData()
      modifiedData[info$row, info$col] <- info$value
      
      # Update the reactive data frame
      editedExcelData(modifiedData)
    }
  })
  
  # Button click event to update the Global Environment
  observeEvent(input$updateGlobalData, {
    global_data <- editedExcelData()
    
    # Store the updated data in the global environment
    assign("TAXABLE_PROPORTION_IMPORT", global_data, envir = .GlobalEnv)
  })
  
  # Button click event to reset the imported data
  observeEvent(input$resetGlobalData, {
    editedExcelData(originalExcelData())
  })
  

  # Simulation
  observeEvent(input$runSimulation, {

    setwd(path1)
    getwd()
    
    # Assign rentInput value to VAT_Input in the global environment
    VAT_benchmark_tax_rate <- input$simulationSlider
    assign("VAT_benchmark_tax_rate", VAT_benchmark_tax_rate, envir = .GlobalEnv)
  
    
    # Execute the scripts
    source("./Scripts/Simulation-Parameters-Module.R")
    source("./Scripts/VAT estimation-Module.R")
    source("./Scripts/TaxCalc-Module.R")
    source("./Scripts/Export-Module.R")
    source("./Scripts/ChartsParametars-Module.R")
    
 
    # Import simulated data -------------------------------------------------------------
    ## Historic data import
    setwd(path)
    getwd()
    
    HistoricRevenue <- read_excel("EgyptData_SUT_v1.2.xlsx", sheet = "VAT_REVENUES", col_names = T)
    VAT_Rev_Pct_GDP_Africa <- read_excel("EgyptData_SUT_v1.2.xlsx", sheet="VAT_Africa")
    #VAT_C_Efficiency_Africa <- read_excel("EgyptData_SUT_v1.2.xlsx", sheet="VAT_C_Africa")
    
    
    Supply_agg <- read_excel("export_data.xlsx",sheet="Supply_export")
    Use_Purchaser_agg <- read_excel("export_data.xlsx",sheet="Use_Purchaser_export")
    
    Revenue_vat_total_bu <- read_excel("export_data.xlsx",sheet="revenue_vat_total_bu")
    
    
    Est_Rev1 <- read_excel("export_data.xlsx", sheet = "Est_Rev1")
    Est_Rev_BU <- read_excel("export_data.xlsx", sheet = "Est_Rev_BU")
    Revenue_VAT_TOTAL <- read_excel("export_data.xlsx", sheet = "Revenue_VAT_TOTAL")
    Main_Results <- read_excel("export_data.xlsx",sheet="Main_Results")
    SimulatedResult<- read_excel("export_data.xlsx",sheet="Results_1")
    TaxExpenditures<- read_excel("export_data.xlsx",sheet="Est_Rev_BU_TE")
    
 
    # Tables

    output$TableOutputId <-renderDT({
      datatable(Main_Results,
                caption = tags$caption("Main results from simulation", class = "table-caption-bold"),
                extensions='Buttons',
                options = list(
                  pageLength = 15,
                  dom = 'Blfrtip',
                  buttons=c('copy','csv','excel','print','pdf'),
                  lengthMenu = list(c(10,25,50,-1),c(10,25,50,"All"))))
     })
    

    
    # Charts ------------------------------------------------------------------
    
     # 1.Historic data -----------------------------------------------------------
        # 1.1 GDP -------------------------------------------------------------
    suppressMessages({     
        Nominal_GDP_tbl<-HistoricRevenue%>%
          dplyr::select(c(Year,Nominal_GDP))
        
        
        Nominal_GDP <- plot_ly(Nominal_GDP_tbl, x = ~Year, y = ~Nominal_GDP, name = "Actual", type = 'scatter', mode = 'lines', line = list(dash = "solid", width = 4)) %>%
          layout(title = 'Nominal GDP',font =t_11,
                 xaxis = list(title = ''),
                 yaxis = list(title = ' '),
                 annotations = list(
                   x = 0, y = -0.056,
                   text = "Source: UNU Wider Government Revenue Dataset",
                   showarrow = FALSE,
                   xref = 'paper',
                   yref = 'paper',
                   align = 'left'
                 ))
        
        Nominal_GDP <- layout(Nominal_GDP, xaxis = list(title = ''), yaxis = list(title = 'In million LCU'))
        Nominal_GDP <- layout(Nominal_GDP, legend = list(orientation = 'h'))
    
        # 1.2 Share of VAT revenues in GDP -----------------------------------------
    
        VAT_Share_GDP_tbl<-HistoricRevenue%>%
          dplyr::select(c(Year,Nominal_GDP,Total_VAT))
        
        VAT_Share_GDP <- plot_ly(VAT_Share_GDP_tbl)
        VAT_Share_GDP <- VAT_Share_GDP %>% add_trace(x = ~Year, y = ~Nominal_GDP, type = 'bar', name = 'GDP')
        VAT_Share_GDP <- VAT_Share_GDP %>% add_trace(
          x = ~Year, y = ~Total_VAT, type = 'scatter', mode = 'lines+markers', name = 'Share of revenue of GDP ',
          yaxis = 'y2', line = list(dash = 'dot', color = "#FFA500", width = 4)  # Set width to 4 for a bold dotted line
        )
        
        VAT_Share_GDP <- VAT_Share_GDP %>% layout(
          title = 'Nominal GDP and share of VAT in GDP',font =t_11,
          xaxis = list(title = ""),
          yaxis = list(side = 'left', title = 'In million LCU', showgrid = FALSE, zeroline = FALSE),
          yaxis2 = list(side = 'right', overlaying = "y", title = 'Percentage', showgrid = FALSE, zeroline = FALSE, font = list(size = 11)), 
          annotations = list(
            x = 0, y = -0.056,
            text = "Source: National authorities and UNU Wider Government Revenue Dataset ",
            showarrow = FALSE,
            xref = 'paper',
            yref = 'paper',
            align = 'left'
          )
        )
        
        # 1.3 Structure of VAT revenues (nominal) -------------------------------------------------
    
        HistoricRevenue_tbl<-HistoricRevenue%>%
          dplyr::select(-c(Total_VAT,Nominal_GDP,ShareOfGDP))
        
        HistoricRevenue_tbl$Year<-as.factor(HistoricRevenue_tbl$Year)
        HistoricRevenue_tbl<-melt(HistoricRevenue_tbl)
        
        HistoricRevenue_tbl$color <- factor(HistoricRevenue_tbl$variable, labels =c("forestgreen", "brown", "orange", "red", "cyan","royalblue","green", "orange")) 
        Structure_VAT_Revenues <- plot_ly(HistoricRevenue_tbl, x = ~Year, y = ~value,
                       type = 'bar',
                       marker = list(color = ~color), name = ~variable) %>%
          layout(title = 'Structure of revenues',font =t_11,
                 xaxis = list(title = ''), 
                 yaxis = list(title = 'In million LCU'),
                 annotations =
                   list(x = 0,y = -0.056, 
                        text = "Source:National authorities",
                        showarrow = F,
                        xref='paper',
                        yref='paper',align='left'),barmode = 'stack')
        
    
        # 1.4 Structure of VAT revenues (in pct) -----------------------------------------------
        HistoricRevenue_pct_tbl<-HistoricRevenue%>%
          dplyr::select(-c(Total_VAT,Nominal_GDP,ShareOfGDP))
        
        HistoricRevenue_pct_tbl$Year<-as.factor(HistoricRevenue_pct_tbl$Year)
        HistoricRevenue_pct_tbl<-melt(HistoricRevenue_pct_tbl)
        
        HistoricRevenue_pct_tbl$color <- factor(HistoricRevenue_pct_tbl$variable, labels =c("forestgreen", "brown", "orange", "red", "cyan","royalblue","green", "orange")) 
        
        VAT_revenue_pct_tbl<-
          dplyr::group_by(HistoricRevenue_pct_tbl, Year) %>% dplyr::mutate(Pct = value/sum(value))
        
        
        VAT_revenue_pct <- plot_ly(VAT_revenue_pct_tbl, x = ~Year, y = ~Pct*100,
                       type = 'bar',
                       marker = list(color = ~color), name = ~variable) %>%
          layout(title = 'Structure of revenues in percentage',font =t_11,
                 xaxis = list(title = ''), 
                 yaxis = list(title = 'In percentage '),
                 annotations =
                   list(x = 0,y = -0.056, 
                        text = "Source:National authorities",
                        showarrow = F,
                        xref='paper',
                        yref='paper',align='left'),barmode = 'stack')
        
    
        
        
        # 1.5 VAT Revenue as Pct  of GDP Africa -----------------------------------------------------------

        VAT_Rev_Pct_GDP_Africa$Abbreviation <-reorder(VAT_Rev_Pct_GDP_Africa$Abbreviation, -VAT_Rev_Pct_GDP_Africa$value)

        VAT_Rev_Pct_GDP_Africa <- plot_ly(VAT_Rev_Pct_GDP_Africa, x = ~Abbreviation, y = ~value,
                             type = 'bar',
                             #orientation = 'h',
                             marker = list(
                               color = c('#1f77b4', '#1f77b4', '#1f77b4', '#1f77b4', '#1f77b4',  '#1f77b4','#d62728'),
                               line = list(color = 'black'),
                               text = ~EconomicGroup,
                               textposition = 'inside' 
                             ),
                             hoverinfo  = "y+text", # Show y values and text on hover
                             text = ~EconomicGroup) %>%
          layout(
                          title = "VAT as a % of GDP by Regional Economic Communities in Africa",font =t_11,
                          barmode = 'stack',
                          xaxis = list(title = ''),
                          yaxis = list(title = 'In percentage (%)'),#,tickmode = "linear", dtick = 1),
                          annotations = list(
                            x = 0,
                            y = -0.04,
                            title = "",
                            text = "Source: Calculations by WB staff based on the UNU Wider Government Revenue Dataset, data in this chart represents various years, with calculations based on the most recent available data ",
                            showarrow = FALSE,
                            xref = 'paper',
                            yref = 'paper',
                            align = 'left',
                            font = t_8
            )
          )
        
        
        # 1.6 VAT C-efficiencies ---------------------------------------------------------------

        # VAT_C_Efficiency_Africa$Abbreviation <-reorder(VAT_C_Efficiency_Africa$Abbreviation, -VAT_C_Efficiency_Africa$value)
        # 
        # # Create the  bar chart with hover text
        # VAT_C_Efficiency_Africa <- plot_ly(VAT_C_Efficiency_Africa, x = ~Abbreviation, y = ~value,
        #                           type = 'bar',
        #                           marker = list(
        #                             color = c('#1f77b4', '#1f77b4', '#1f77b4', '#1f77b4', '#1f77b4','#1f77b4','#d62728'),
        #                             line = list(color = 'black'),
        #                             text = ~EconomicGroup
        #                           ),
        #                           hoverinfo  = "y+text", # Show y values and text on hover
        #                           text = ~EconomicGroup ) %>%
        #                      layout(
        #                           title = " C-efficiencies in Africa",font =t_11,
        #                           barmode = 'stack',
        #                           xaxis = list(title = ''),
        #                           yaxis = list(title = 'In percentage'),
        #                           annotations = list(
        #                             x = 0,
        #                             y = -0.056,
        #                             title = "",
        #                             text = "Source: Calculations by WB staff based on the US-AID Dataset, data in this chart represents various years, with calculations based on the most recent available data ",font =t_11,
        #                             showarrow = FALSE,
        #                             xref = 'paper',
        #                             yref = 'paper',
        #                             align = 'left',
        #                             font = t_8
        #                               )
        #                             )

        
    # 2.SUTs ------------------------------------------------------------------
      # 2.1 Supply ------------------------------------------------------------------
        # 2.1.1 Supply-Total output --------------------------------------------------
          Supply_agg_tbl<-Supply_agg%>%
                dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Total_output)
          
          Supply_agg_tbl <- Supply_agg_tbl %>%
                dplyr::mutate_if(is.numeric, funs(. / 1e06))
          
          Supply_agg_tbl<-melt(Supply_agg_tbl)
          
          Supply_agg_tbl<-Supply_agg_tbl%>%
                dplyr::mutate(value=round(value),1)%>%
                dplyr::arrange(desc(value))
              
          Supply_agg_tbl$SUT_division<-"Product(CPC 1.1)"
          
          Supply_Output<-plot_ly(data = Supply_agg_tbl, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                           marker = list(colors = setNames(colr, unique(Supply_agg_tbl$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent"
                           )  %>%
            layout(title="Total output (in LCU billions)",font =t_11)    
  
          
      
        # 2.1.2 Imports CIF  ----------------------------------------------------
  
          Import_CIF_tbl<-Supply_agg%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Imports_CIF)
          
          Import_CIF_tbl <- Import_CIF_tbl %>%
            dplyr::mutate_if(is.numeric, funs(. / 1e06))
          
          Import_CIF_tbl<-melt(Import_CIF_tbl)
          
          Import_CIF_tbl<-Import_CIF_tbl%>%
            dplyr::mutate(value=round(value),4)%>%
            dplyr::arrange(desc(value))
          
          Import_CIF_tbl$SUT_division<-"Product(CPC 1.1)"
          
          Supply_Import_CIF<-plot_ly(data = Import_CIF_tbl, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                           marker = list(colors = setNames(colr, unique(Import_CIF_tbl$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent")  %>%
            layout(title="Imports CIF (in LCU billions)",font =t_11)   
          
          
        # 2.1.3 Total supply basic prices ---------------------------------------
  
          Supply_basic_price<-Supply_agg%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Total_supply_at_basic_prices)
          
          Supply_basic_price <- Supply_basic_price %>%
            mutate_if(is.numeric, funs(. / 1e06))
          
          Supply_basic_price<-melt(Supply_basic_price)
          
          Supply_basic_price<-Supply_basic_price%>%
            dplyr::mutate(value=round(value),8)%>%
            dplyr::arrange(desc(value))
          
          Supply_basic_price$SUT_division<-"Product(CPC 1.1)"
          
          Supply_basic_prices<-plot_ly(data = Supply_basic_price, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                           marker = list(colors = setNames(colr, unique(Supply_basic_price$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent")  %>%
            layout(title="Total supply basic prices (in LCU billions)",font =t_11)    
          
        # 2.1.4 Total supply purchasers prices -----------------------------------------
  
          Supply_purchasers_prices<-Supply_agg%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Total_supply_at_purchasers_prices)
          
          Supply_purchasers_prices <- Supply_purchasers_prices %>%
            dplyr::mutate_if(is.numeric, funs(. / 1e06))
          
          Supply_purchasers_prices<-melt(Supply_purchasers_prices)
          
          Supply_purchasers_prices<-Supply_purchasers_prices%>%
            dplyr::mutate(value=round(value),0)%>%
            dplyr::arrange(desc(value))
          
          Supply_purchasers_prices$SUT_division<-"Product(CPC 1.1)"
          
          Supply_purchasers_prices<-plot_ly(data = Supply_purchasers_prices, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                           marker = list(colors = setNames(colr, unique(Supply_purchasers_prices$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent")  %>%
            layout(title="Total supply purchasers prices (in LCU billions)",font =t_11)            
          
          
          
          
          
          
      # 2.2 Use Purchaser -----------------------------------------------------------
        # 2.2.1 Total intermediate consumption ------------------------------------------------------
          
          Use_total_intermediate_consumption<-Use_Purchaser_agg%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Total_intermediate_consumption_at_purchasers_prices)
          
          Use_total_intermediate_consumption <- Use_total_intermediate_consumption %>%
            mutate_if(is.numeric, funs(. / 1e06))
          
          Use_total_intermediate_consumption<-melt(Use_total_intermediate_consumption)
          
          Use_total_intermediate_consumption<-Use_total_intermediate_consumption%>%
            dplyr::mutate(value=round(value),1)%>%
            dplyr::arrange(desc(value))
          
          Use_total_intermediate_consumption$SUT_division<-"Product(CPC 1.1)"
          
          
          Use_total_intermediate_consumption<-plot_ly(data = Use_total_intermediate_consumption, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                           marker = list(colors = setNames(colr, unique(Use_total_intermediate_consumption$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent")  %>%
            layout(title="Total intermediate consumption (in LCU billions)",font =t_11)  

        
        
        
        
        # 2.2.2 Final consumption households  ----------------------------------
          
          Use_final_consumtion_househodls<-Use_Purchaser_agg%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_consumption_expenditure_by_households)
          
          Use_final_consumtion_househodls <- Use_final_consumtion_househodls %>%
            mutate_if(is.numeric, funs(. / 1e06))
          
          Use_final_consumtion_househodls<-melt(Use_final_consumtion_househodls)
          
          Use_final_consumtion_househodls<-Use_final_consumtion_househodls%>%
            dplyr::mutate(value=round(value),4)%>%
            dplyr::arrange(desc(value))
          
          Use_final_consumtion_househodls$SUT_division<-"Product(CPC 1.1)"
          
          
          Use_final_consumtion_househodls<-plot_ly(data = Use_final_consumtion_househodls, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                           marker = list(colors = setNames(colr, unique(Use_final_consumtion_househodls$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent")  %>%
            layout(title="Final consumption households (in LCU billions)",font =t_11)    
          
          
          
          
        # 2.2.3 Final consumption NPISH ----------------------------------------
          
          Use_Final_consumption_NPISH<-Use_Purchaser_agg%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_consumption_expenditure_NPISH)
          
          Use_Final_consumption_NPISH <- Use_Final_consumption_NPISH %>%
            mutate_if(is.numeric, funs(. / 1e06))
          
          Use_Final_consumption_NPISH<-melt(Use_Final_consumption_NPISH)
          
          Use_Final_consumption_NPISH<-Use_Final_consumption_NPISH%>%
            dplyr::mutate(value=round(value),8)%>%
            dplyr::arrange(desc(value))
          
          Use_Final_consumption_NPISH$SUT_division<-"Product(CPC 1.1)"
          
          Use_Final_consumption_NPISH<-plot_ly(data = Use_Final_consumption_NPISH, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                           marker = list(colors = setNames(colr, unique(Use_Final_consumption_NPISH$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent")  %>%
            layout(title="Final consumption NPISH (in LCU billions)",font =t_11)  
          
          
          
          
        # 2.2.4 Final consumption government -------------------------------------------------
          
          Use_Final_consumption_government<-Use_Purchaser_agg%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_consumption_expenditure_by_government)
          Use_Final_consumption_government <- Use_Final_consumption_government %>%
            mutate_if(is.numeric, funs(. / 1e06))
          
          Use_Final_consumption_government<-melt(Use_Final_consumption_government)
          
          Use_Final_consumption_government<-Use_Final_consumption_government%>%
            dplyr::mutate(value=round(value),0)%>%
            dplyr::arrange(desc(value))
          
          Use_Final_consumption_government$SUT_division<-"Product(CPC 1.1)"
          
          Use_Final_consumption_government<-plot_ly(data = Use_Final_consumption_government, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                           marker = list(colors = setNames(colr, unique(Use_Final_consumption_government$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent")  %>%
            layout(title="Final consumption government (in LCU billions)",font =t_11)    
          
          
          
        # 2.2.5 Total use -------------------------------------------

          Use_Final_consumption_total<-Use_Purchaser_agg%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Total_use_at_purchasers_prices)
          
          Use_Final_consumption_total <- Use_Final_consumption_total %>%
            mutate_if(is.numeric, funs(. / 1e06))
          
          Use_Final_consumption_total<-melt(Use_Final_consumption_total)
          
          Use_Final_consumption_total<-Use_Final_consumption_total%>%
            dplyr::mutate(value=round(value),0)%>%
            dplyr::arrange(desc(value))
          
          Use_Final_consumption_total$SUT_division<-"Product(CPC 1.1)"
          
          Use_Final_consumption_total<-plot_ly(data = Use_Final_consumption_total, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                           marker = list(colors = setNames(colr, unique(Use_Final_consumption_total$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent")  %>%
            layout(title="Total use (in LCU billions)",font =t_11)    
          
          
     # 2.3 VAT ---------------------------------------------------------------------
       # 2.3.1 VAT revenues from Industries  -----------------------

          VAT_revenues_industries<-Revenue_vat_total_bu%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Total_Revenues_from_Intermediate_Inputs)

                    VAT_revenues_industries<-melt(VAT_revenues_industries)
          
          VAT_revenues_industries<-VAT_revenues_industries%>%
            dplyr::mutate(value=round(value),1)%>%
            dplyr::arrange(desc(value))
          
          VAT_revenues_industries$SUT_division<-"Product(CPC 1.1)"
          
          
          VAT_revenues_industries<-plot_ly(data = VAT_revenues_industries, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                            marker = list(colors = setNames(colr, unique(VAT_revenues_industries$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent")  %>%
            layout(title="VAT revenues from Industries (value by Thousand LCU)",font =t_11)    
  

       # 2.3.2 VAT revenues from NPISH -----------------------------------

          VAT_revenues_NPISH<-Revenue_vat_total_bu%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_NPISH)

          VAT_revenues_NPISH<-melt(VAT_revenues_NPISH)
          
          VAT_revenues_NPISH<-VAT_revenues_NPISH%>%
            dplyr::mutate(value=round(value),8)%>%
            dplyr::arrange(desc(value))
          
          VAT_revenues_NPISH$SUT_division<-"Product(CPC 1.1)"
          
          VAT_revenues_NPISH<-plot_ly(data = VAT_revenues_NPISH, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                           marker = list(colors = setNames(colr, unique(VAT_revenues_NPISH$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent")  %>%
            layout(title="VAT revenues from NPISH (value by Thousand LCU)",font =t_11)   
          
       # 2.3.3 VAT revenues from Government ----------------------------------------------------

          VAT_revenues_Government<-Revenue_vat_total_bu%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_Government)
          

          VAT_revenues_Government<-melt(VAT_revenues_Government)
          
          VAT_revenues_Government<-VAT_revenues_Government%>%
            dplyr::mutate(value=round(value),4)%>%
            dplyr::arrange(desc(value))
          
          VAT_revenues_Government$SUT_division<-"Product(CPC 1.1)"
          
          
          VAT_revenues_Government<-plot_ly(data = VAT_revenues_Government, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                           marker = list(colors = setNames(colr, unique(VAT_revenues_Government$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent")  %>%
            layout(title="VAT revenues from Government (value by Thousand LCU)",font =t_11) 

       # 2.3.4 VAT revenues from Households -----------------------------------------

          VAT_revenues_Households<-Revenue_vat_total_bu%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_HH)

          VAT_revenues_Households<-melt(VAT_revenues_Households)
          
          VAT_revenues_Households<-VAT_revenues_Households%>%
            dplyr::mutate(value=round(value),0)%>%
            dplyr::arrange(desc(value))
          
          VAT_revenues_Households$SUT_division<-"Product(CPC 1.1)"
          
          VAT_revenues_Households<-plot_ly(data = VAT_revenues_Households, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                           parents = ~SUT_division, name = " ",
                           text = ~PRODUCT_INDUSTRY_NAME,
                           marker = list(colors = setNames(colr, unique(VAT_revenues_Households$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                           textinfo="label+value+percent parent")  %>%
            layout(title="VAT revenues from Households (value by Thousand LCU)",font =t_11)    
          
          
       # 2.3.5 VAT revenue total ------------------------------------------------

          VAT_revenues_Total<-Revenue_vat_total_bu%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_Total)
          
          VAT_revenues_Total<-melt(VAT_revenues_Total)
          
          VAT_revenues_Total<-VAT_revenues_Total%>%
            dplyr::mutate(value=round(value),0)%>%
            dplyr::arrange(desc(value))
          
          VAT_revenues_Total$SUT_division<-"Product(CPC 1.1)"
          
          VAT_revenues_Total<-plot_ly(data = VAT_revenues_Total, type = "treemap", values = ~value, labels = ~PRODUCT_INDUSTRY_CODE,
                                           parents = ~SUT_division, name = " ",
                                           text = ~PRODUCT_INDUSTRY_NAME,
                                           marker = list(colors = setNames(colr, unique(VAT_revenues_Total$PRODUCT_INDUSTRY_CODE))), # <- mod here!
                                           textinfo="label+value+percent parent")  %>%
            layout(title="VAT revenues Total (value by Thousand LCU)",font =t_11)   
          
    # 3. Simulated results -----------------------------------------------------------------
        # 3.1 Comparison of VAT revenues (before and after simulation) ------------
          dat_r <- Est_Rev1 %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, Final_Demand_Total) %>%
            data.table()
          
          dat_ba <- Est_Rev_BU %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, Final_Demand_Total) %>%
            data.table()
          
          dat <- left_join(dat_r, dat_ba, by = c("PRODUCT_INDUSTRY_CODE"))
          
          colnames(dat) <- c('NACE', 'series1', 'series2')
          
          ComparisonOfRevenues <- plot_ly(dat) %>% 
            add_trace(x = ~NACE, y = ~series2, type = 'bar', name = 'Before reform') %>% 
            add_trace(x = ~NACE, y = ~series1, type = 'bar', name = 'After reform') %>% 
            layout(
              xaxis = list(title = ''), 
              yaxis = list(title = ''),
              legend = list(x = 0.9, y = 0.99),
              barmode = 'group'
            )%>%
            layout(title="Comparison of VAT revenues (before and after simulation)",font =t_11)   
    
        # 3.2  DifferenceVAT_revenues---------------------------------------------

          dat_r<-Revenue_VAT_TOTAL%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,Final_Demand_Total)%>%
            data.table()
          
          dat_ba<-Revenue_vat_total_bu%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,Final_Demand_Total)%>%
            data.table()
          
          dat<-left_join(dat_r,dat_ba,by = c("PRODUCT_INDUSTRY_CODE"))
          
          
          colnames(dat) <- c('NACE', 'series1', 'series2')
          
          dat$diff<-dat$series1-dat$series2
          
          dat$diff<-round(dat$diff,1)
          
          DifferenceVAT_revenues <- plot_ly(dat) %>% 
            add_trace(x = ~NACE, y = ~diff, type = 'bar', name = 'Before reform') %>% 
            layout(
              xaxis = list(title = '',font = t_8), 
              yaxis = list(title = ''),
              legend = list(x = 0.9, y = 0.99),
              barmode = 'group'
              )  %>%
             layout(title="Comparison of VAT revenues (before and after simulation)",font =t_11)          
          
          


        # 4.Tax expenditures ------------------------------------------------------

          dat_r<-Est_Rev_BU%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Final_Demand_Total)%>%
            data.table()

          dat_te<-TaxExpenditures%>%
            dplyr::select(PRODUCT_INDUSTRY_CODE,Final_Demand_Total)%>%
            data.table()
          
          
          TaxExpenditure_tbl<-left_join(dat_r,dat_te,by = c("PRODUCT_INDUSTRY_CODE"))%>%
            select(PRODUCT_INDUSTRY_NAME,PRODUCT_INDUSTRY_CODE,Final_Demand_Total.y,Final_Demand_Total.x)
          
   
          colnames(TaxExpenditure_tbl) <- c('Description','NACE', 'vat_revenue_before','vat_revenue_after')
          
          
          TaxExpenditure_plt <- plot_ly(TaxExpenditure_tbl) %>% 
            add_trace(x = ~NACE, y = ~vat_revenue_before, type = 'bar', name = 'With standard VAT rates', text = ~Description) %>% 
            add_trace(x = ~NACE, y = ~vat_revenue_after, type = 'bar', name = 'With current VAT rates', text = ~Description) %>% 
            layout(
              xaxis = list(title = '', font = t_8), 
              yaxis = list(title = ''),
              legend = list(x = 0.9, y = 0.99),
              barmode = 'group')%>%
             layout(title="Comparison of tax expenditures",font =t_11)      
          
          
       # 5. Summary tree map ------------------------------------------------------

          output$treemap_final_plt <- renderPlotly({
          treemap_final<-Main_Results%>%
            dplyr::filter(variable %in% c("Policy_Gap (in LCU Billions)","Compliance_Gap (in LCU Billions)"))
          # #%>%
          # dplyr::rename(
          #   #"BencmarkVAT"="Benchmark_VAT_M_of_LCU",
          #   "VATCollection"="Calibrated_VAT_Est.M_of_LCU",
          #   "PolicyGap"="Policy_Gap.M_of_LCU",
          #   #"TotalVATGAP"="Total_VAT_Gap.M_of_LCU",
          #   "PolicyGap"="Policy_Gap.M_of_LCU",
          #   "ComplianceGap"="Compliance_Gap.M_of_LCU"
          # )

          # treemap_final<-melt(treemap_final)
          # 
          # treemap_final <- treemap_final %>%
          #   dplyr::mutate_if(is.numeric, funs(. / 1e06))
          # 
          # 
          # treemap_final<-treemap_final%>%
          #   dplyr::mutate(value=round(value),1)%>%
          #   dplyr::arrange(desc(value))
          
          treemap_final$SUT_division<-"Components of VAT GAP "
          
          treemap_final_plt<-plot_ly(data = treemap_final, type = "treemap", values = ~Simulation, labels = ~variable,
                                     parents = ~SUT_division, name = " ",
                                     text = ~variable   ,
                                     textinfo="label+value+percent parent")%>%
                                      layout(title="Decomposition of VAT GAP",font =t_11)  
          })
  
    })        
# 4.Drop down menu ---------------------------------------------------
      # 4.1 Historic ---------------------------------------
          output$chartOutputHistoric <- renderPlotly({
            switch(input$chartSelectHistoric,
                   "Nominal_GDP" = Nominal_GDP,
                   "VAT_Share_GDP" = VAT_Share_GDP,
                   "Structure_VAT_Revenues" = Structure_VAT_Revenues,
                   "VAT_revenue_pct" = VAT_revenue_pct,
                   "VAT_Rev_Pct_GDP_Africa"=VAT_Rev_Pct_GDP_Africa
                   #"VAT_C_Efficiency_Africa"=VAT_C_Efficiency_Africa
                   
            )
          })
      
      # 4.2  SUT --------------------------------------------------------
      
          output$chartOutputSUT <- renderPlotly({
            switch(input$chartSelectSUT,
                   "Supply_Output" = Supply_Output,
                   "Supply_Import_CIF" = Supply_Import_CIF,
                   "Supply_basic_prices" = Supply_basic_prices,
                   "Supply_purchasers_prices" = Supply_purchasers_prices,
                   "Use_total_intermediate_consumption" = Use_total_intermediate_consumption,
                   "Use_final_consumtion_househodls" = Use_final_consumtion_househodls,
                   "Use_Final_consumption_NPISH" = Use_Final_consumption_NPISH,
                   "Use_Final_consumption_government" = Use_Final_consumption_government,
                   "Use_Final_consumption_total" = Use_Final_consumption_total,
                   "VAT_revenues_industries" = VAT_revenues_industries,
                   "VAT_revenues_NPISH" = VAT_revenues_NPISH,
                   "VAT_revenues_Government" = VAT_revenues_Government,
                   "VAT_revenues_Households" = VAT_revenues_Households,
                   "VAT_revenues_Total" = VAT_revenues_Total
            )
          })
          
      
      # 4.3 Simulated results ------------------------------------------------------------------
      
          output$chartOutputResults <- renderPlotly({
            switch(input$chartSelectResults,
                   "ComparisonOfRevenues" = ComparisonOfRevenues,
                   "DifferenceVAT_revenues" = DifferenceVAT_revenues
            )
          })


          
          
          
      # 4.4 Tax expenditures -----------------------------------------------------
          
          output$chartOutputTaxExpenditures <- renderPlotly({
            switch(input$chartTaxExpenditures,
                   "TaxExpenditure_plt" = TaxExpenditure_plt
                   
            )
          })
          
          
          
          
          
# 5. Info Boxes -------------------------------------------------------------

    # Calculations of values for Info boxes
   # value1 <-round(Main_Results%>%filter(variable=="Benchmark_VAT")%>%select(Simulation),0)
    value1 <-Main_Results %>%filter(variable=="Benchmark_VAT (in LCU Billions)")%>%select(Simulation)
    value2 <- Main_Results %>%filter(variable=="Uncalibrated_VAT (in LCU Billions)")%>%select(Simulation)
    value3 <- Main_Results %>%filter(variable=="Calibrated_VAT (in LCU Billions)")%>%select(Simulation)
    value4 <- Main_Results %>%filter(variable=="Total_VAT_Gap (in LCU Billions)")%>%select(Simulation)
    value5 <- Main_Results %>%filter(variable=="Policy_Gap (in LCU Billions)")%>%select(Simulation)
    value6 <- Main_Results %>%filter(variable=="Compliance_Gap (in LCU Billions)")%>%select(Simulation)
    #value7 <- round((sum(Est_Rev1$Final_Demand_Total / 1e06) - sum(Est_Rev_BU$Final_Demand_Total / 1e06)), 0)
    value7 <- Main_Results$`Difference(Simulation-Actual)`[2]
    #value8 <- round(sum(Est_Rev1$Final_Demand_Total) / sum(Est_Rev_BU$Final_Demand_Total * 100), 2)
    value8 <- Main_Results$`Pct_Change(Simulation/Actual)`[2]
    value9 <- round((Main_Results$Simulation[5]/Main_Results$Simulation[3])*100,2)
    
    
   # Update the content of the infoboxes Simulated results
    output$SimulatedResults <- renderUI({
      info_boxes_SimulatedResults <- list(
        column(
          width = 6,  
          infoBox(
            "Fiscal impact of simulation (in billion LCU)",
            value7,
            width = 17,
            icon = icon("chart-line"),
            color = "blue",
            fill = TRUE
          )
        ),
        column(
          width = 6,  
          infoBox(
            "Percentage change in VAT revenues",
            value8,
            width = 17,
            icon = icon("fas fa-chart-pie"),
            color = "orange",
            fill = TRUE
          )
        )
      )
      
      do.call(tagList, info_boxes_SimulatedResults)
    })
    
    
    
    
    
    # Update the content of the infoboxes Tax expenditures
    output$TaxExpenditures <- renderUI({
      info_boxes_TaxExpenditures <- list(
        column(
          width = 6,  
          infoBox(
            "Total tax expenditures (in billion LCU)",
            value5,
            width = 17,
            icon = icon("hand-holding-dollar"),
            color = "light-blue",
            fill = TRUE
          )
        ),
        column(
          width = 6,  
          infoBox(
            "Percentage change in VAT revenues",
            value9,
            width = 17,
            icon = icon("fas fa-chart-pie"),
            color = "green",
            fill = TRUE
          )
        )
      )

      do.call(tagList, info_boxes_TaxExpenditures)
    })

   
    # Update the content of the infoboxes
    output$infoBoxUI <- renderUI({
      info_boxes <- list(
        infoBox("Benchmark VAT (in billion LCU )", value1, icon = icon("coins"), width = 4, color = "blue"),
        infoBox("Uncalibrated VAT (in billion LCU )", value2, icon = icon("business-time"), width = 4,color = "red"),
        infoBox("Calibrated VAT (in billion LCU)", value3,  icon = icon("chart-column"), width = 4,color = "green"),
        infoBox("Total VAT Gap (in billion LCU)", value4,  icon = icon("gauge"), width = 4,color = "orange"),
        infoBox("Policy Gap (in billion LCU)", value5,  icon = icon("coins"), width = 4,color = "red"),
        infoBox("Compliance Gap (in billion LCU)", value6,  icon = icon("chart-pie"), width = 4,color = "aqua")
      )
      #  Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
      do.call(tagList, info_boxes)
    })
    


    
    
# 6. Show notification -------------------------------------------------------


    # Show a centered modal notification when the simulation is done
    showModal(
      modalDialog(
        title = "Simulation Complete",
        footer = NULL,
        easyClose = TRUE,
        size = "s",
        draggable = TRUE
      )
    )
  })

}


# III. Create a Shiny app
shinyApp(ui, server)
