vars1 <- c(
  "All Conservation Plans" = "All",
  "Water Quality Related Plans" = "Water",
  "Habitat Related Plans" = "Habitat",
  "Resources/Species Related Plans" = "Species",
  "Community Resilience Related Plans" ="Community",
  "Ecosystem Resilience Related Plans" ="Ecosystem",
  "Gulf Economy Related Plans" ="Economy"
)

vars2 <- c(
  "All Plans" = "2017",
  "Plans within 5 years" = "F2012",
  "Plans within 10 years" = "F2007",
  "Plans longer than 10 years" = "F2002"
)

vars3 <- c(
  "All scales" = "All",
  "State Level" = "State",
  "County & Parish Level" = "County",
  "Coastal Area & Watershed" = "CA",
  "Regional Level" = "Regional"
)

navbarPage("SCA Project Inventory of Plans (IOP)", id="nav",
          # tabPanel(
          #   "Home",
          #   tabsetPanel(id="tabsAbout",
          #               tabPanel("Introduction",
          #                        column(4,
          #                               h5("    Welcome to the Conservation Projects & Plans category Visualization Tool.  
          #                                  This visualization tool is intended for users to explore the plans inventory that 
          #                                  has been collected during the development of SCA Tool."),
          #                               h5("All the plans are categorized based on their relevance to achieving 
          #                                  the goals of the RESTORE Council (Table 1).Under each RESTORE goal exists several priorities that were 
          #                                  identified by stakeholders and are reflected as GIS data layers.")
          #                               ),
          #                       column(6,
          #                               imageOutput("goalsTable")
                                         #,
                                         
                                         #tags$style("#logo {vertical-align:top;margin-top:150px;}"),
                                         #imageOutput("logo")
          #                        ))
          #               
          #                               ),
          #   column(2,
          #          tags$br(),
          #          tags$br(),
          #          actionButton("gototool","Explore the tool!")
          #   )
             
           #),
           
           tabPanel("Interactive map",
                    div(class="outer",
                        tags$head(
                          includeCSS("styles.css"),
                          includeScript("gomap.js"),
                          includeScript("google-analytics.js"),
                          useShinyjs()
                        ),
                        leafletOutput("map", width="100%", height="100%"),
                        absolutePanel(id = "Sidebar", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 800, height = "auto",
                                      h3("Plans recorded in this area"),
                                      hr(),
                                      DT::dataTableOutput("docs"),
                                      style = "opacity: 0.65; z-index: 1000;"
                        ),
                        absolutePanel(id='controls',fixed= TRUE,
                                      draggable= FALSE, top= 60, left=50, right = "auto",bottom = "auto",
                                      width = 400,height = "auto",
                                      h3("Land Conservation Cataloging"),
                                      hr(),
                                      selectInput("scale", "Scale", vars3,selected = "All"),
                                      hr(),
                                      selectInput("timeframe", "Timeframe", vars2,selected = "2017"),
                                      hr(),
                                      selectInput("priority", "Priorities", vars1,selected = "All"),
                                      hr(),
                                      bsButton("showpanel","Show/Hide Table", type = "toggle", value= TRUE),
                                      style = "opacity: 0.65; z-index: 1000;"
                        )
                    )),
           tabPanel("Data Tables",
                    tabsetPanel(id="nav1",
                      tabPanel("Regional",
                               hr(),
                               actionButton(inputId='form1', label="Add New Plans", 
                                                  icon = icon("th"), 
                                                  onclick ="window.open('http://bit.ly/sca_addplan_test', '_blank')"),
                               actionButton(inputId='form2', label="Newly Added Plans", 
                                            icon = icon("th")),
                               hr(),
                               DT::dataTableOutput("Regional")
                      ),
                      tabPanel("Alabama",
                               hr(),
                               actionButton(inputId='form1', label="Add New Plans", 
                                            icon = icon("th"), 
                                            onclick ="window.open('http://bit.ly/sca_addplan_test', '_blank')"),
                               actionButton(inputId='form2', label="Newly Added Plans", 
                                            icon = icon("th")),
                               hr(),
                               DT::dataTableOutput("Alabama")
                      ),
                      tabPanel("Louisiana",
                               hr(),
                               actionButton(inputId='form1', label="Add New Plans", 
                                            icon = icon("th"), 
                                            onclick ="window.open('http://bit.ly/sca_addplan_test', '_blank')"),
                               actionButton(inputId='form2', label="Newly Added Plans", 
                                            icon = icon("th")),
                               hr(),
                               DT::dataTableOutput("Louisiana")
                      ),
                      tabPanel("Mississippi",
                               hr(),
                               actionButton(inputId='form1', label="Add New Plans", 
                                            icon = icon("th"), 
                                            onclick ="window.open('http://bit.ly/sca_addplan_test', '_blank')"),
                               actionButton(inputId='form2', label="Newly Added Plans", 
                                            icon = icon("th")),
                               hr(),
                               DT::dataTableOutput("Mississippi")
                      ),
                      tabPanel("Texas",
                               hr(),
                               actionButton(inputId='form1', label="Add New Plans", 
                                            icon = icon("th")),
                               actionButton(inputId='form2', label="Newly Added Plans", 
                                            icon = icon("th")),
                               hr(),
                               DT::dataTableOutput("Texas")
                      ),
                      tabPanel("Florida",
                               hr(),
                               actionButton(inputId='form1', label="Add New Plans", 
                                            icon = icon("th"), 
                                            onclick ="window.open('http://bit.ly/sca_addplan_test', '_blank')"),
                               actionButton(inputId='form2', label="Newly Added Plans", 
                                            icon = icon("th")),
                               hr(),
                               DT::dataTableOutput("Florida")
                      )
                    )
           ),
           tabPanel("Recently Added Plans", 
                    DT::dataTableOutput("Newly")),
          
          tabPanel("Help",
                   tabsetPanel(id="tabsAbout",
                               tabPanel("Contact Us",
                                        column(8,
                                               h5(strong("For more information please contact our Conservation Applications Specialist: ")),
                                               h6(tags$b("Matthew Heinemann")),
                                               h6("D.J. Case and Associates"),
                                               HTML(paste0("Email: ",'<a href="mailto:Matt.Heinemann@djcase.com"> Matt.Heinemann@djcase.com</a>')),
                                               hr(),
                                               h5(strong("Or one of our project's Principal Investigators: ")),
                                               h6(tags$b("Kristine Evans")),
                                               h6("Co-Director of the Quantitative Ecology and Spatial Technologies Lab (QuEST) Lab"),
                                               h6("Mississippi State University"),
                                               h6("Department of Wildlife Fisheries and Aquaculture"),
                                               HTML(paste0("Email: ",'<a href="mailto:kristine.evans@msstate.edu"> kristine.evans@msstate.edu</a>')),
                                               hr()
                                               
                                        )
                                        #,
                                        #column(6,
                                        #                tags$style("#logo3 {vertical-align:top;margin-top:550px;}"),
                                        #                imageOutput("logo3"))
                               )
                   )
          )
)