library(readxl)
library(xlsx)
library(dplyr)
library(data.table)
library(ggplot2)
library(countrycode)
library(tidyr)
library(plotly)
library(reshape2)


# I. Define countries from Africa -----------------------------------------

# 1. Southern African Development Community (SADC)

sadc_iso3 <- data.frame(iso3c=c(
                                  "AGO",   # Angola
                                  "BWA",   # Botswana
                                  "COM",   # Comoros
                                  "COD",   # Democratic Republic of the Congo
                                  "SWZ",   # Eswatini
                                  "LSO",   # Lesotho
                                  "MDG",   # Madagascar
                                  "MWI",   # Malawi
                                  "MUS",   # Mauritius
                                  "MOZ",   # Mozambique
                                  "NAM",   # Namibia
                                  "SYC",   # Seychelles
                                  "ZAF",   # South Africa
                                  "TZA",   # Tanzania
                                  "ZMB",   # Zambia
                                  "ZWE"    # Zimbabwe
                                ))


# 2. East African Community (EAC)

eac_iso3 <- data.frame(iso3c=c(
                                "BDI",   # Burundi
                                "KEN",   # Kenya
                                "RWA",   # Rwanda
                                "SSD",   # South Sudan
                                "TZA",   # Tanzania
                                "UGA"    # Uganda
                              ))


# 3. Economic and Monetary Community of Central Africa (CEMAC)

cemac_iso3 <- data.frame(iso3c=c(
                                "CMR",   # Cameroon
                                "CAF",   # Central African Republic
                                "TCD",   # Chad
                                "COG",   # Republic of the Congo
                                "GNQ",   # Equatorial Guinea
                                "GAB"    # Gabon
                              ))

# 4. Economic Community of West African States (ECOWAS)

ecowas_iso3 <- data.frame(iso3c=c(
                                  "BEN",   # Benin
                                  "BFA",   # Burkina Faso
                                  "CPV",   # Cape Verde
                                  "CIV",   # Côte d'Ivoire (Ivory Coast)
                                  "GMB",   # Gambia
                                  "GHA",   # Ghana
                                  "GIN",   # Guinea
                                  "GNB",   # Guinea-Bissau
                                  "LBR",   # Liberia
                                  "MLI",   # Mali
                                  "NER",   # Niger
                                  "NGA",   # Nigeria
                                  "SEN",   # Senegal
                                  "SLE",   # Sierra Leone
                                  "TGO"    # Togo
                                ))

# 5. Arab Maghreb Union (AMU)

amu_iso3 <-  data.frame(iso3c=c(
                                  "DZA",   # Algeria
                                  "LBY",   # Libya
                                  "MAR",   # Morocco
                                  "MRT",   # Mauritania (Mauritania was a founding member but withdrew in 2001)
                                  "TUN"    # Tunisia
                                ))

# 6. Intergovernmental Authority on Development (IGAD)

igad_iso3 <- data.frame(iso3c=c(
                                "DJI",   # Djibouti
                                "ERI",   # Eritrea
                                "ETH",   # Ethiopia
                                "KEN",   # Kenya
                                "SOM",   # Somalia
                                "SSD",   # South Sudan
                                "SUD",   # Sudan
                                "UGA"    # Uganda
                              ))
# 7. Full names of organizations
africa_full_names_organizations<- data.frame(
  Abbreviation = c("SADC", "EAC", "CEMAC", "ECOWAS", "AMU", "IGAD","EGY"),
  FullName = c(
    "Southern African Development Community",
    "East African Community",
    "Economic and Monetary Community of Central Africa",
    "Economic Community of West African States",
    "Arab Maghreb Union",
    "Intergovernmental Authority on Development",
    "Egypt"
  )
)




# II.UNU-WIDER data ------------------------------------------------------------
        # 1. Choropleth map (UNU-WIDER) -------------------------------------------------------
        
        VAT_pct_GDP_raw <- read_excel("DATA/INPUT/EgyptData_SUT_v1.2.xlsx", 
                                          sheet = "UNU-WIDER")
          
        VAT_pct_GDP <- VAT_pct_GDP_raw %>%
          na.omit() %>%
          group_by(iso) %>%
          arrange(desc(VAT_GDP)) %>%  # Replace "your_column_name" with the name of your column
          filter(VAT_GDP != 0) %>%    # Replace "your_column_name" again
          slice_head(n = 1) %>%
          ungroup()
          
        
        Geo <- read_excel("DATA/INPUT/EgyptData_SUT_v1.2.xlsx", 
                                  sheet = "Geo")
        
        df_tbl<-left_join(Geo,VAT_pct_GDP,by=c("iso3c"="iso"))%>%
          dplyr::select(iso3c,income_group,countries,region,VAT_GDP,continent)
        
        df_tbl<-df_tbl%>%
          rename("region_classification"="region")
        
        
        # Adding ISO 3 codes to names of countries from ggplot2
        
        mapdata <- map_data("world") 
        iso3c <- data.frame(iso3=countrycode(mapdata$region, "country.name", "iso3c"))
        mapdata_iso3c<-cbind(mapdata,iso3c)
        #mapdata2 <- left_join(mapdata_iso3c,df_tbl,by = c("region"="countries"))
        mapdata2 <- left_join(mapdata_iso3c,df_tbl,by = c("iso3"="iso3c"))
        
        
        
        # Removing Antarctica and Nan values
        mapdata3<-mapdata2[!(mapdata2$region=="Antarctica"),]

        
        mapdata3<-mapdata3%>%
          #dplyr::filter(region_classification=="Middle East & North Africa")%>%
          dplyr::filter(continent=="Africa")%>%
          
          
          dplyr::select(-c(iso3,subregion,income_group,continent,order))%>%
          dplyr:: group_by(VAT_GDP,long,lat,group,countries)

        
        # Plotting Choropleth Map
        # Example of map # https://stackoverflow.com/questions/27338512/color-countries-on-world-map-based-on-iso3-codes-in-r-using-ggplot
        
        map1 <- ggplot(mapdata3, aes(x= long , y=lat,group=group)) +
          geom_polygon(aes(fill=VAT_GDP), color="black")
        map2 <- map1 + scale_fill_gradient(name= "VAT revenues (% of GDP) ", low="yellow" , high="red", na.value="grey90") +
          #map2 <- map1 + scale_fill_gradient(name= "Total Revenue ") +
         # ggtitle("               A comparison of the average CIT to GDP  ratio in the Western Balkan countries, for the period 2019-2021 ") +
          theme_minimal()+
          theme(plot.title = element_text(face = "bold"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                legend.position="bottom")
        
        map2
        
        
        
        # 2. Southern African Development Community (SADC) ------------------------------
        
        VAT_SADC<-left_join(sadc_iso3,df_tbl,by=c("iso3c"="iso3c"))%>%
          dplyr::select(iso3c,VAT_GDP)%>%
          dplyr::summarise(VAT_SADC=mean(VAT_GDP,na.rm=TRUE))
          
        
        # 3. East African Community (EAC)-------------------------------
        
        VAT_EAC<-left_join(eac_iso3,df_tbl,by=c("iso3c"="iso3c"))%>%
          dplyr::select(iso3c,VAT_GDP)%>%
          dplyr::summarise(VAT_EAC=mean(VAT_GDP,na.rm=TRUE))
        
        # 4. Economic and Monetary Community of Central Africa (CEMAC)---------------------
        
        VAT_CEMAC<-left_join(cemac_iso3,df_tbl,by=c("iso3c"="iso3c"))%>%
          dplyr::select(iso3c,VAT_GDP)%>%
          dplyr::summarise(VAT_CEMAC=mean(VAT_GDP,na.rm=TRUE))
        
        # 5. Economic Community of West African States (ECOWAS)--------------------
        
        VAT_ECOWAS<-left_join(ecowas_iso3,df_tbl,by=c("iso3c"="iso3c"))%>%
          dplyr::select(iso3c,VAT_GDP)%>%
          dplyr::summarise(VAT_ECOWAS=mean(VAT_GDP,na.rm=TRUE))
        
        # 6. Arab Maghreb Union (AMU)----------------------------------
        
        VAT_AMU<-left_join(amu_iso3,df_tbl,by=c("iso3c"="iso3c"))%>%
          dplyr::select(iso3c,VAT_GDP)%>%
          dplyr::summarise(VAT_AMU=mean(VAT_GDP,na.rm=TRUE))
        
        
        # 7. Intergovernmental Authority on Development (IGAD) --------------------------
        VAT_IGAD<-left_join(igad_iso3,df_tbl,by=c("iso3c"="iso3c"))%>%
          dplyr::select(iso3c,VAT_GDP)%>%
          dplyr::summarise(VAT_IGAD=mean(VAT_GDP,na.rm=TRUE))
        
        
        # 8. Only Egypt -----------------------------------------------------
        
        VAT_EGY<-df_tbl%>%
          dplyr::filter(iso3c=="EGY")%>%
          dplyr::select(VAT_GDP)
        
        
        # 9. Merged data with Egypt ------------------------------------------------
        
        VAT_AFRICA<-data.frame(
                                VAT_SADC=c(VAT_SADC),
                                VAT_EAC=c(VAT_EAC),
                                VAT_CEMAC=c(VAT_CEMAC),
                                VAT_ECOWAS=c(VAT_ECOWAS),
                                VAT_AMU=c(VAT_AMU),
                                VAT_IGAD=c(VAT_IGAD),
                                VAT_EGY=c(VAT_EGY[1,1])
                              )
        

        
# II.US-AID data set -------------------------------------------------------

        
        # Procedure for data here is different because here US-AID have data about on countries istead iso-3 code
        #data(codelist)
        #country_set <- codelist
        country_set <- data.table(codelist)
        country_set<- country_set %>% 
          dplyr::select(country.name.en , iso2c, iso3c,continent, region,region23,eu28)
        
        # Add Kosovo with iso2c code "XK"
        country_set <- rbind(country_set, data.table(country.name.en = "Kosovo", iso2c = "XK", iso3c = "XKX", continent = "Europe", region = "Europe & Central Asia", region23 = "Southern Europe", eu28 = NA))
        # Remove GB from EU28
        country_set[iso2c == "GB", eu28 := NA]
        
        country_set<-country_set%>%
          dplyr::filter(!is.na(iso2c))%>%
          # Rename EU28 to EU
          dplyr::rename("eu27"="eu28")%>%
          data.table()
        
        
        # Input US-AID data
        VAT_pct_GDP_raw <- read_excel("DATA/INPUT/EgyptData_SUT_v1.2.xlsx", 
                                      sheet = "USAID")
        
        
        
        VAT_pct_GDP_raw<-VAT_pct_GDP_raw%>%
          dplyr::select(country_name,"Value-added tax (VAT) C-efficiency ratio [ef_c_vat]")%>%
          dplyr::rename("c_efficiency"="Value-added tax (VAT) C-efficiency ratio [ef_c_vat]")
        
        VAT_pct_GDP_raw$c_efficiency<-as.numeric(VAT_pct_GDP_raw$c_efficiency)
        
        View(VAT_pct_GDP_raw)
        
        
        
        VAT_pct_GDP <- VAT_pct_GDP_raw %>%
          na.omit() %>%
          group_by(country_name) %>%
          arrange(desc(c_efficiency)) %>%  # Replace "your_column_name" with the name of your column
          filter(c_efficiency != 0) %>%    # Replace "your_column_name" again
          slice_head(n = 1) %>%
          ungroup()
        
        
        Geo <- read_excel("DATA/INPUT/EgyptData_SUT_v1.2.xlsx",
                          sheet = "Geo")
        
        df_tbl<-left_join(Geo,VAT_pct_GDP,by=c("countries"="country_name"))%>%
          dplyr::select(iso3c,income_group,countries,region,c_efficiency,continent)
        
        df_tbl<-df_tbl%>%
          rename("region_classification"="region")
        
        
        
        # Adding ISO 3 codes to names of countries from ggplot2
        
        mapdata <- map_data("world") 
        iso3c <- data.frame(iso3=countrycode(mapdata$region, "country.name", "iso3c"))
        mapdata_iso3c<-cbind(mapdata,iso3c)
        mapdata2 <- left_join(mapdata_iso3c,df_tbl,by = c("iso3"="iso3c"))
        
        
        # Removing Antarctica and Nan values
        mapdata3<-mapdata2[!(mapdata2$region=="Antarctica"),]
        
        
        # Filter only Africa
        mapdata3<-mapdata3%>%
          #dplyr::filter(region_classification=="Middle East & North Africa")%>%
          dplyr::filter(continent=="Africa")%>%
          dplyr::select(-c(iso3,subregion,income_group,continent,order))%>%
          dplyr:: group_by(c_efficiency,long,lat,group,countries)
        
        
        # Plotting Choropleth Map
        # Example of map # https://stackoverflow.com/questions/27338512/color-countries-on-world-map-based-on-iso3-codes-in-r-using-ggplot
        
        map1 <- ggplot(mapdata3, aes(x= long , y=lat,group=group)) +
          geom_polygon(aes(fill=c_efficiency), color="black")
        map2 <- map1 + scale_fill_gradient(name= "VAT revenues (% of GDP) ", low="yellow" , high="red", na.value="grey90") +
          #map2 <- map1 + scale_fill_gradient(name= "Total Revenue ") +
          # ggtitle("               A comparison of the average CIT to GDP  ratio in the Western Balkan countries, for the period 2019-2021 ") +
          theme_minimal()+
          theme(plot.title = element_text(face = "bold"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                legend.position="bottom")
        
        map2
        

# 2. Southern African Development Community (SADC)------------------------

        VAT_C_SADC<-left_join(sadc_iso3,df_tbl,by=c("iso3c"="iso3c"))%>%
          dplyr::select(iso3c,c_efficiency)%>%
          dplyr::summarise(SADC=mean(c_efficiency,na.rm=TRUE))
        

# 3.East African Community (EAC) ------------------------------------------

        VAT_C_EAC<-left_join(eac_iso3,df_tbl,by=c("iso3c"="iso3c"))%>%
          dplyr::select(iso3c,c_efficiency)%>%
          dplyr::summarise(EAC=mean(c_efficiency,na.rm=TRUE))
        
        
# 4. Economic and Monetary Community of Central Africa (CEMAC)-------------------

        VAT_C_CEMAC<-left_join(cemac_iso3,df_tbl,by=c("iso3c"="iso3c"))%>%
          dplyr::select(iso3c,c_efficiency)%>%
          dplyr::summarise(CEMAC=mean(c_efficiency,na.rm=TRUE))
        

# 5. Economic Community of West African States (ECOWAS)----------------

          VAT_C_ECOWAS<-left_join(ecowas_iso3,df_tbl,by=c("iso3c"="iso3c"))%>%
          dplyr::select(iso3c,c_efficiency)%>%
          dplyr::summarise(ECOWAS=mean(c_efficiency,na.rm=TRUE))


# 6.Arab Maghreb Union (AMU) -------------------------------------------

          VAT_C_AMU<-left_join(amu_iso3,df_tbl,by=c("iso3c"="iso3c"))%>%
          dplyr::select(iso3c,c_efficiency)%>%
          dplyr::summarise(AMU=mean(c_efficiency,na.rm=TRUE))      
        

# 7. Intergovernmental Authority on Development (IGAD)--------------------
          VAT_C_IGAD<-left_join(igad_iso3,df_tbl,by=c("iso3c"="iso3c"))%>%
          dplyr::select(iso3c,c_efficiency)%>%
          dplyr::summarise(IGAD=mean(c_efficiency,na.rm=TRUE)) 
        

# 8. Only Egypt--------------------------------------------

          VAT_C_EGY<-df_tbl%>%
          dplyr::filter(iso3c=="EGY")%>%
          dplyr::select(c_efficiency)%>%  
          rename("EGY"="c_efficiency")
        
        
                
        
# 9. Merged data with Egypt-----------------------------------------

          VAT_C_AFRICA<-data.frame(
                                  VAT_C_SADC=c(VAT_C_SADC),
                                  VAT_C_EAC=c(VAT_C_EAC),
                                  VAT_C_CEMAC=c(VAT_C_CEMAC),
                                  VAT_C_ECOWAS=c(VAT_C_ECOWAS),
                                  VAT_C_AMU=c(VAT_C_AMU),
                                  VAT_C_IGAD=c(VAT_C_IGAD),
                                  EGY=c(VAT_C_EGY[1,1])
                                )        

       
       
        
        
# 10. Chart -------------------------------------------------------------------
        
        
        VAT_C_AFRICA<-melt(VAT_C_AFRICA)
        
       
        VAT_C_AFRICA<-left_join(VAT_C_AFRICA,africa_full_names_organizations,by=c("variable"="Abbreviation"))
        
        
        
        
        
        # Reorder the levels of the 'variable' factor based on 'value'
        VAT_C_AFRICA$variable <-reorder(VAT_C_AFRICA$variable, -VAT_C_AFRICA$value)
        
    
        
        
        
        # Create the  bar chart with hover text
        VAT_AFRICA_plt <- plot_ly(VAT_C_AFRICA, x = ~variable, y = ~value,
                                  type = 'bar',
                                  marker = list(
                                   color = c('#1f77b4', '#1f77b4', '#1f77b4', '#1f77b4', '#1f77b4','#1f77b4','#d62728'),
                                   line = list(color = 'black'),
                                   text = ~FullName
                                  ),
                                  hoverinfo  = "y+text", # Show y values and text on hover
                                  text = ~FullName
        ) %>%
                       layout(
                                  title = "Africa: C-efficiencies",
                                  barmode = 'stack',
                                  xaxis = list(title = ''),
                                  yaxis = list(title = 'In percentage'),
                                  annotations = list(
                                    x = 0,
                                    y = -0.056,
                                    title = "",
                                    text = "Source: Calculation of WB staff based on UNU Wider Government Revenue Dataset ",
                                    showarrow = FALSE,
                                    xref = 'paper',
                                    yref = 'paper',
                                    align = 'left'
            )
          )
        

        VAT_AFRICA_plt
        
     ### test
        # Calculate the average value
        average_value <- mean(VAT_C_AFRICA$value)
        
        # Create the bar chart with hover text
        VAT_AFRICA_plt <- plot_ly(VAT_C_AFRICA, x = ~variable, y = ~value,
                                  type = 'bar',
                                  marker = list(
                                    color = c('#1f77b4', '#1f77b4', '#1f77b4', '#d62728', '#1f77b4', '#1f77b4','#1f77b4'),
                                    line = list(color = 'black'),
                                    text = ~variable
                                  ),
                                  hoverinfo  = "y+text" # Show y values and text on hover
        ) %>%
          layout(
            title = "VAT as % of GDP, in AFRICA",
            barmode = 'stack',
            xaxis = list(title = ''),
            yaxis = list(title = 'In percentage'),
            annotations = list(
              x = 0,
              y = -0.056,
              title = "",
              text = "Source: Calculation of WB staff based on UNU Wider Government Revenue Dataset ",
              showarrow = FALSE,
              xref = 'paper',
              yref = 'paper',
              align = 'left'
            )
          )
        
        # Add a dotted average line spanning the entire chart
        VAT_AFRICA_plt <- VAT_AFRICA_plt %>%
          add_trace(x = c("SADC","EAC","CEMAC","ECOWAS","EGY","AMU","IGAD"),
                    y = rep(average_value, 7),
                    type = 'scatter',
                    mode = 'lines',
                    line = list(color = 'red', dash = 'dot'),
                    showlegend = FALSE)
        
        VAT_AFRICA_plt
        
        
        
        
        