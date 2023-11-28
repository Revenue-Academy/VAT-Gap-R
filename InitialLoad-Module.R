
'Set path and install packages 
                                          '

# path<-"C:/Users/wb591157/OneDrive - WBG/Documents/WB/Countries/Egypt/VAT-GAP-EGYPT-APP/DATA/INPUT" #<----Set your path here for data 
# path1<-"C:/Users/wb591157/OneDrive - WBG/Documents/WB/Countries/Egypt/VAT-GAP-EGYPT-APP" #<----Set your path here for the model

path<-"C:/Users/j_sim/Documents/WB/Countries/Egypt/VAT-GAP-EGYPT-APP/DATA/INPUT" #<----Set your path here for data 
path1<-"C:/Users/j_sim/Documents/WB/Countries/Egypt/VAT-GAP-EGYPT-APP" #<----Set your path here for the model



'1. Before starting the application, the following command installs the specified libraries. '

          # Installing online version  
          #packageurl <- "https://cran.r-project.org/src/contrib/Archive/rccmisc/rccmisc_0.3.7.tar.gz"
          #install.packages(packageurl, repos=NULL, type="source")
          
          # Offline version
          install.packages (paste(path, "rccmisc", sep = "/"), repos=NULL, type="source")
          
          
          list.of.packages <- c("DataEditR", "data.table","tidyr","readxl","dplyr","reshape2","openxlsx",
                                "flexdashboard","ggplot2","plotly","tidyverse","knitr","DT","rpivotTable",
                                "forecast","openintro","highcharter","ggvis","DataCombine","viridisLite","orca","sass","vctrs","markdown",
                                "sm","ks","rlang","shades","tidyverse","stringr","fontawesome","shinydashboard","shinyjs","reshape2")
          
          new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
          if(length(new.packages)) install.packages(new.packages)



'2. Importing raw data  from Excel into R enviroment '

          
          library(tidyverse)
          library(readxl)
          library(reshape2)
          library(rccmisc) 
          
        
         excel_file<- "EgyptData_SUT_v1.2.xlsx"  #' <-- Set name of the file with data which include SUTs
          
          GDP_2018<- 4437.4 # Data in billion of LCU

          
          setwd(path)
          getwd()
  

# 1. DEFINE FUNCTIONS ----

      #  The function creates an ntile group vector:
      qgroup = function(numvec, n, na.rm=TRUE){
        qtile = quantile(numvec, probs = seq(0, 1, 1/n), na.rm)  
        out = sapply(numvec, function(x) sum(x >= qtile[-(n+1)]))
        return(out)
      }
      
      #  to extract only English names from SUTs
      trim <- function (x) gsub("^\\s+|\\s+$", "", x) 
      input_output_matrix_to_long_data <- function(matrix){
        
        matrix <- matrix %>%
          dplyr::filter(...2 != "NA")
        
        
        colnames(matrix) <- matrix[1,]
        
        data <- matrix[c(-1,-2),c(-1,-2)] %>% as.matrix() %>% melt()
        
        product_industry_name <- matrix[[2]][c(-1,-2)]
        product_industry_code <- matrix[[1]][c(-1,-2)]
        industry_code <-  matrix[2,c(-1,-2)] %>% as.character()
        
        data$Var1 <- rep(product_industry_name, time = length(industry_code))
        
        data <- data %>% 
          dplyr::rename(PRODUCT_INDUSTRY_NAME = Var1,
                        INDUSTRY_NAME = Var2)
        
        
        data$PRODUCT_INDUSTRY_CODE <- rep(product_industry_code, time = length(industry_code))
        data$INDUSTRY_CODE <- rep(industry_code, each = length(product_industry_code))
        
        data <- data %>% 
          dplyr::select(PRODUCT_INDUSTRY_NAME, PRODUCT_INDUSTRY_CODE, INDUSTRY_NAME, INDUSTRY_CODE, value)
        
        
        # Leave the names only in English
        data$PRODUCT_INDUSTRY_NAME<-gsub("^.*\\/", "",  data$PRODUCT_INDUSTRY_NAME) %>% trim()
        data$INDUSTRY_NAME<-gsub("^.*\\/", "",  data$INDUSTRY_NAME) %>% trim()
        
        data$value <- as.numeric(as.character(data$value))
        
        return(data)
        
      }


# 2. RAW DATA IMPORT AND PREPROCESS  ----- 
# 3. IMPORT RAW DATA (SUTS) ------------------------------------

      TAXABLE_PROPORTION_BU<-read_excel(excel_file, sheet = "TaxableProportion")
      

        SUPPLY <- read_excel(excel_file, sheet = "Supply_EG", col_names = F)[c(-1,-2,-3,-4),] %>%
                input_output_matrix_to_long_data()            
      
                   SUPPLY$INDUSTRY_CODE<-as.numeric(SUPPLY$INDUSTRY_CODE) 
              

             USE_PURCHASER <- read_excel(excel_file, sheet = "Use_Purchaser_EG", col_names = F)[c(-1,-2,-3,-4),] %>%
               input_output_matrix_to_long_data()
             
             USE_PURCHASER$INDUSTRY_CODE<-as.numeric(USE_PURCHASER$INDUSTRY_CODE) #New
             
            USE_VAT <- read_excel(excel_file, sheet = "VAT_EG", col_names = F)[c(-1,-2,-3,-4),] %>%
              input_output_matrix_to_long_data()
            
            USE_VAT$INDUSTRY_CODE<-as.numeric(USE_VAT$INDUSTRY_CODE) #New
            
            USE_BASIC <- read_excel(excel_file, sheet = "Use_Purchaser_Basic_EG", col_names = F)[c(-1,-2,-3,-4),] %>%
              input_output_matrix_to_long_data()
            
            
            USE_BASIC$INDUSTRY_CODE<-as.numeric(USE_BASIC$INDUSTRY_CODE) 

# 4. AGGREGATE DATA IN LISTS -----

            ' For calculation of rows'
          CPA_PRODUCTS <- as.list(c(1:4))
          names(CPA_PRODUCTS) = c("Supply", "Use_Purchaser", "Use_VAT", "Use_Basic")
          
          ' For calculation of columns'
          NACE_INDUSTRIES <- as.list(c(1:4))
          names(NACE_INDUSTRIES) = c("Supply", "Use_Purchaser", "Use_VAT", "Use_Basic")
          

# 4.1 Supply matrix -----
      
          CPA_PRODUCTS$Supply <- SUPPLY %>% 
            dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
            dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
            dplyr::summarise(Total_output = sum(value, na.rm = T))

          CPA_PRODUCTS$Supply <- SUPPLY %>%
            dplyr::filter(INDUSTRY_NAME == "Imports CIF") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Supply, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Imports_CIF = value)

          CPA_PRODUCTS$Supply <- SUPPLY %>%
            dplyr::filter(INDUSTRY_NAME == "Trade and transport margins") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Supply, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Trade_and_transport_margins = value)
 
          
          CPA_PRODUCTS$Supply <- SUPPLY %>%
            dplyr::filter(INDUSTRY_NAME == "Taxes less subsidies on products") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Supply, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Taxes_less_subsidies_on_products = value)
          


          CPA_PRODUCTS$Supply <- CPA_PRODUCTS$Supply %>%
            dplyr::mutate(Total_supply_at_basic_prices = psum(Total_output,Imports_CIF, na.rm=TRUE),
                          Total_supply_at_purchasers_prices = psum(Total_supply_at_basic_prices,
                                                                   Trade_and_transport_margins,
                                                                   Taxes_less_subsidies_on_products, na.rm=TRUE))
          
         
          NACE_INDUSTRIES$Supply <- SUPPLY %>% 
            dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
            dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
            dplyr::summarise(Total_output_by_industries_at_basic_prices = sum(value, na.rm = T))
          
# 4.2 Use Purchaser matrix ----

          CPA_PRODUCTS$Use_Purchaser <- USE_PURCHASER %>% 
            dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
            dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
            dplyr::summarise(Total_intermediate_consumption_at_purchasers_prices = sum(value, na.rm = T))

    
          CPA_PRODUCTS$Use_Purchaser <-  USE_PURCHASER %>%
            dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by households") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Final_consumption_expenditure_by_households = value)

        
          CPA_PRODUCTS$Use_Purchaser <- USE_PURCHASER %>%
            dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Final_consumption_expenditure_NPISH = value)

      
          CPA_PRODUCTS$Use_Purchaser <- USE_PURCHASER %>%
            dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by government") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Final_consumption_expenditure_by_government = value)

    
          CPA_PRODUCTS$Use_Purchaser <- USE_PURCHASER %>%
            dplyr::filter(INDUSTRY_NAME == "Gross fixed capital formation") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Gross_fixed_capital_formation = value)

       
          CPA_PRODUCTS$Use_Purchaser <- USE_PURCHASER %>%
            dplyr::filter(INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Changes_in_inventories_and_acquisition_less_disposals_of_valuables = value)

      
          CPA_PRODUCTS$Use_Purchaser <- USE_PURCHASER %>%
            dplyr::filter(INDUSTRY_NAME == "Exports FOB") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Use_Purchaser, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Exports_FOB = value)
          
          
          CPA_PRODUCTS$Use_Purchaser <- CPA_PRODUCTS$Use_Purchaser %>%
            dplyr::mutate(Total_final_consumption_expenditure_at_purchasers_prices = psum(Final_consumption_expenditure_by_households,
                                                                                          Final_consumption_expenditure_NPISH,
                                                                                          Final_consumption_expenditure_by_government, na.rm=TRUE),
                          Gross_capital_formation = psum(Gross_fixed_capital_formation,
                                                         Changes_in_inventories_and_acquisition_less_disposals_of_valuables, na.rm=TRUE),
                          Total_final_uses_at_purchasers_prices = psum(Total_final_consumption_expenditure_at_purchasers_prices,
                                                                       Gross_capital_formation,
                                                                       Exports_FOB, na.rm=TRUE),
                          Total_use_at_purchasers_prices = psum(Total_intermediate_consumption_at_purchasers_prices,
                                                                Total_final_uses_at_purchasers_prices, na.rm=TRUE))
         
          NACE_INDUSTRIES$Use_Purchaser <- USE_PURCHASER %>% 
            dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
            dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
            dplyr::summarise(Total_intermediate_consumption_by_industries_at_purchasers_prices = sum(value, na.rm = T))


# 4.3 Use Basic matrix ----
          CPA_PRODUCTS$Use_Basic <- USE_BASIC %>% 
            dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
            dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
            dplyr::summarise(Total_intermediate_consumption_at_basic_prices = sum(value, na.rm = T))
          
          CPA_PRODUCTS$Use_Basic <- USE_BASIC %>%
            dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by households") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Final_consumption_expenditure_by_households = value)
          
          CPA_PRODUCTS$Use_Basic <- USE_BASIC %>%
            dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Final_consumption_expenditure_NPISH = value)
          
          CPA_PRODUCTS$Use_Basic <- USE_BASIC %>%
            dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by government") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Final_consumption_expenditure_by_government = value)
          
          CPA_PRODUCTS$Use_Basic <- USE_BASIC %>%
            dplyr::filter(INDUSTRY_NAME == "Gross fixed capital formation") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Gross_fixed_capital_formation = value)
          
          CPA_PRODUCTS$Use_Basic <- USE_BASIC %>%
            dplyr::filter(INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Changes_in_inventories_and_acquisition_less_disposals_of_valuables = value)
          
          CPA_PRODUCTS$Use_Basic <- USE_BASIC %>%
            dplyr::filter(INDUSTRY_NAME == "Exports FOB") %>%
            dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
            merge.data.frame(CPA_PRODUCTS$Use_Basic, key = "PRODUCT_INDUSTRY_CODE") %>%
            dplyr::rename(Exports_FOB = value)
          
          CPA_PRODUCTS$Use_Basic$Exports_FOB[is.na(CPA_PRODUCTS$Use_Basic$Exports_FOB)] <- 0
          
          
          CPA_PRODUCTS$Use_Basic <- CPA_PRODUCTS$Use_Basic %>%
            dplyr::mutate(Total_final_consumption_expenditure_at_basic_prices = psum(
              Final_consumption_expenditure_by_households,
              Final_consumption_expenditure_NPISH,
              Final_consumption_expenditure_by_government,
              na.rm=TRUE),
              Gross_capital_formation = psum(
                Gross_fixed_capital_formation,
                Changes_in_inventories_and_acquisition_less_disposals_of_valuables,
                na.rm=TRUE),
              Total_final_uses_at_basic_prices = psum(Total_final_consumption_expenditure_at_basic_prices,
                                                      Gross_capital_formation,
                                                      Exports_FOB,
                                                      na.rm=TRUE),
              Total_use_at_basic_prices = psum(Total_intermediate_consumption_at_basic_prices,
                                               Total_final_uses_at_basic_prices,
                                               na.rm=TRUE))
          
          NACE_INDUSTRIES$Use_Basic <- USE_BASIC %>% 
            dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
            dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
            dplyr::summarise(Total_intermediate_consumption_by_industries_at_basic_prices = sum(value, na.rm = T))


# 4.4 Use VAT matrix ----
          
        CPA_PRODUCTS$Use_VAT <- USE_VAT %>% 
          dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
          dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
          dplyr::summarise(Total_intermediate_consumption_at_basic_prices = sum(value, na.rm = T))
        
        CPA_PRODUCTS$Use_VAT <- USE_VAT %>%
          dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by households") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Final_consumption_expenditure_by_households = value)
        
        
        CPA_PRODUCTS$Use_VAT <- USE_VAT %>%
          dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by non-profit organisations serving households (NPISH)") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Final_consumption_expenditure_NPISH = value)
        
        
        CPA_PRODUCTS$Use_VAT <- USE_VAT %>%
          dplyr::filter(INDUSTRY_NAME == "Final consumption expenditure by government") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Final_consumption_expenditure_by_government = value)
        
        
        CPA_PRODUCTS$Use_VAT <- USE_VAT %>%
          dplyr::filter(INDUSTRY_NAME == "Gross fixed capital formation") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Gross_fixed_capital_formation = value)
        
        
        CPA_PRODUCTS$Use_VAT <- USE_VAT %>%
          dplyr::filter(INDUSTRY_NAME == "Changes in inventories and acquisition less disposals of valuables") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Changes_in_inventories_and_acquisition_less_disposals_of_valuables = value)
        
        
        CPA_PRODUCTS$Use_VAT <- USE_VAT %>%
          dplyr::filter(INDUSTRY_NAME == "Exports FOB") %>%
          dplyr::select(PRODUCT_INDUSTRY_CODE, value) %>%
          merge.data.frame(CPA_PRODUCTS$Use_VAT, key = "PRODUCT_INDUSTRY_CODE") %>%
          dplyr::rename(Exports_FOB = value)
        
        
        CPA_PRODUCTS$Use_VAT <- CPA_PRODUCTS$Use_VAT %>%
          dplyr::mutate(Total_final_consumption_expenditure_at_basic_prices = psum(Final_consumption_expenditure_by_households,
                                                                                   Final_consumption_expenditure_NPISH,
                                                                                   Final_consumption_expenditure_by_government,
                                                                                   na.rm=TRUE),
                        Gross_capital_formation = psum(Gross_fixed_capital_formation,
                                                       Changes_in_inventories_and_acquisition_less_disposals_of_valuables,
                                                       na.rm=TRUE),
                        Total_final_uses_at_basic_prices = psum(Total_final_consumption_expenditure_at_basic_prices,
                                                                Gross_capital_formation,
                                                                Exports_FOB,
                                                                na.rm=TRUE),
                        Total_use_at_basic_prices = psum(Total_intermediate_consumption_at_basic_prices,
                                                         Total_final_uses_at_basic_prices,
                                                         na.rm=TRUE))
        
        NACE_INDUSTRIES$Use_VAT <- USE_VAT %>% 
          dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
          dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
          dplyr::summarise(Total_VAT = sum(value, na.rm = T))


# 5. Save data in R environment (RDS file) --------------------------------------------------------
        setwd(path1)
        getwd()
        
        rm(list = ls()[!ls() %in% c("path", "path1", "SUPPLY", "USE_BASIC", "USE_PURCHASER", "USE_VAT", "TAXABLE_PROPORTION_BU", "NACE_INDUSTRIES", "CPA_PRODUCTS","GDP_2018"
                                    )])
        
        save.image(file=".RData") 

        