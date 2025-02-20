################################################################################
################################ Set Up ########################################
################################################################################

library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)

library(shinyjs)
library(shinyscreenshot)

# install.packages("remotes")
#remotes::install_github("deepanshu88/shinyDarkmode")
library(shinyDarkmode)



################################################################################
############################### Load Data ######################################
################################################################################

#data <- readxl::read_excel("Shiny_Data.xlsx")
data <- readRDS("wwc-shiny-13Sep2023.RDS")



################################################################################
############################### Style Sheet ####################################
################################################################################

jscode <- "shinyjs.refresh_page = function() { history.go(0); }" 

shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme("lumen"),
    #use_darkmode(),
    
    tags$head(HTML("<title>Sensitivity Analysis Benchmarks: What Works Clearinghouse</title><link rel='icon' href='KonFoundit!-mark.png' type='image/gif/png'>"),
              tags$script(src="script.js"),
              tags$style(HTML("
              
              /* import Google fonts */
              @import url('https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,100;0,300;0,400;0,500;0,700;0,900;1,100;1,300;1,400;1,500;1,700;1,900&family=Victor+Mono&display=swap');
              @import url('https://fonts.googleapis.com/css2?family=Raleway:ital,wght@0,100;0,200;0,300;0,400;1,100;1,200;1,300;1,400');
              @import url('https://fonts.googleapis.com/css2?family=Cardo:wght@400;700&family=Cormorant:wght@400;700&family=Nanum+Myeongjo:wght@400;700;800&family=Playfair+Display+SC:wght@400;700;900&family=Roboto+Slab:wght@100;400;700;800;900&display=swap');

  
              
              /* change page background as well as header and footer text */
              body {
                    background-color: #fff;
                    font-family: 'Roboto', sans-serif;
                    font-size: 18px;
                    font-weight: 300;
                    color: #000;
              }
  
  
              
              /* change page title */
              h2 {
                    font-family: 'Raleway', serif;
                    font-size: 76px;
                    font-weight: 400;
                    color: #639dad; /* theme blue color */
              }
              
              /* change page first-level headings (e.g., Specification, Results) */
              h3 {
                    font-family: 'Roboto', sans-serif;
                    font-size: 32px;
                    font-weight: 700;
                    color: #7f9f3d;  /* theme green color */
              }

              /* change page second-level headings */
              h4 {
                    font-family: 'Roboto', sans-serif;
                    font-size: 24px;
                    font-weight: 700;
                    color: #000;
              }
              
              /* change page third-level headings */
              h5 {
                    font-family: 'Roboto', sans-serif;
                    font-size: 18px;
                    color: #000;
              }
              
              /* change hyperlink text */
              a {
                    color: #0645ad;  /* standard blue hyperlink color */
                    font-weight: 400;
              }
              
              /* change hyperlink hover text */
              a:hover {
                    color: #e04300;
                    text-decoration: underline;
              }
              
              /* change multiple choice text */
              .shiny-input-container {
                    color: #000;
                    font-size: 16px;
              }
              
              
              
              /* change navigation bar */
              .navbar-default {
                    background-color: #639dad;
                    border-color: #999;
                    font-size: 18px;
                    font-weight: 300;
              }
              
              .navbar-nav > .active {
                    background-color: #7f9f3d;
              }
              
              /* change text in navigation bar: active tab */
              .navbar-nav > .active > a {
                    color: #fff !important;
              }
              
              /* change text in navigation bar: inactive tab */
              .navbar-nav > li > a {
                    color: #eee !important;
              }
              

              
              /* change background of area for user input */
              .well { 
                    background-color: #fff !important;
              }
              
              /* change rows in area for user input */
              .row { 
                    background-color: #fff !important;
              }
              
              
              
              /* change color of most buttons */
              #startover_button, #results_pg_l, 
              #results_pg_pse, #results_pg_cop, 
              #results_pg_di, #results_pg_2x2 {
                    background-color: #7f9f3d !important;
                    color: #fff !important;
                    font-family: 'Roboto', sans-serif;
                    font-size: 22px;
              }
              
              /* change hover color of most buttons */
              #startover_button:hover, #results_pg_l:hover, 
              #results_pg_pse:hover, #results_pg_cop:hover, 
              #results_pg_di:hover, #results_pg_2x2:hover {
                    background-color: #465723 !important;
                    color: #fff !important;
                    font-family: 'Roboto', sans-serif;
              }
              
              
              
              /* change radio buttons */
              label > input[type='radio'] {
                    opacity: 1;
                    position: absolute;
              }
              
              label > input[type='radio'] + *::before {
                    content: '';
                    margin: 4px 0 0;
                    width: 13px;
                    height: 13px;
                    position: absolute;
                    margin-left: -20px;
                    border-radius: 50%;
                    border-style: solid;
                    border-width: 0.1rem;
                    border-color: #639dad;
              }
                    
              label > input[type='radio']:checked + *::before {
                    background: radial-gradient(#7f9f3d 0%, #7f9f3d 50%, transparent 50%) !important;
                    border-color: #639dad !important;
              }
              
              
                    
              label > input[type='checkbox'] {
                    opacity: 1;
                    position: absolute;
              }
                    
              label > input[type='checkbox'] + *::before {
                    content: '';
                    position: absolute;
                    margin: 4px 0 0;
                    margin-left: -20px;
                    align: center;
                    width: 13px;
                    height: 13px;
                    margin-right: 1rem;
                    border-radius: 0%;
                    border-style: solid;
                    border-width: 0.1rem;
                    border-color: #639dad !important;
              }
                    
              label > input[type='checkbox']:checked + *::before {
                    content: '';
                    width: 13px;
                    height: 13px;
                    border-color: #639dad !important;
                    background: #7f9f3d !important;
              }



              /* change text on info button */
              [id*=info] {
                    background-color: #639dad !important;
                    color: #fff !important;
                    font-family: 'Roboto', sans-serif;
                    text-shadow: -.0px 0 #fff, 0 .0px #fff, .0px 0 #fff, 0 -.0px #fff;
              }
              
              
              
              /* change color of visit website button */
              #visit_website_button {
                    background-color: #7f9f3d !important;
                    color: #fff !important;
                    font-family: 'Roboto', sans-serif;
                    font-size: 16px;
              }
              
              /* change hover color of visit website button */
              #visit_website_button:hover {
                    background-color: #465723 !important;
                    color: #fff !important;
                    font-family: 'Roboto', sans-serif;
              }
              
              
              
              /* change color of screenshot button */
              [id*=screenshot] {
                    background-color: #7f9f3d !important;
                    color: #fff !important;
                    font-family: 'Roboto', sans-serif;
                    font-size: 22px;
              }
              
              /* change hover color of screenshot button */
              [id*=screenshot]:hover {
                    background-color: #465723 !important;
                    color: #fff !important;
                    font-family: 'Roboto', sans-serif;
              }

              
    "
                              
              )
              )
    ),



                      
    
################################################################################
############################### Header #########################################
################################################################################    
    
    titlePanel(title = div(img(style = "height:0.75em; vertical-align:center; margin-bottom: 18px;",
                               src = "KonFoundit!-mark.png",
                               alt = "Konfound R package logo"), 
                           "Sensitivity Analysis Benchmarks")),

    h3("Robustness of Findings in What Works Clearinghouse"),
    tags$p("This page contains sensitivity analysis measures calculated for findings the ", 
           tags$a(href="https://ies.ed.gov/ncee/wwc/", "What Works Clearinghouse"),
           " has rated as meeting its standards for a strong and well-executed research design. 
           These values can be used to create tailored reference distributions that help locate
           the robustness of your finding in a distribution of other similar and well-designed educational studies."),
    tags$p("For more information on using these benchmarks, please see  ", 
           tags$a(href="", "Practice Guide.")),
    tags$p("For details on the calculations of the benchmark values, please see ", 
           tags$a(href="create-wwc-dbs-13Sep2023.html", "here.")),
    tags$p(actionButton("visit_website_button",
                  icon = icon("globe", lib = "font-awesome"),
                  label = "KonFound-It website",
                  onclick = "window.open('https://konfound-it.org/', '_blank')")
           ),
   # tags$p(tags$i(paste("Powered by version", packageVersion('konfound'), "of the konfound R package."))),
  

                      
  ### For refresh button: 
  useShinyjs(),
  extendShinyjs(text = jscode, functions = "refresh_page"),

  
  navbarPage("",
             tabPanel(div(icon("house", lib = "font-awesome"), " Home"),
                      sidebarLayout(
                        sidebarPanel(
                          verticalLayout(
                            fluidRow(
                              align = "center",
                              h3("Specification")
                            ),
                            
                            selectInput("selectedStudyDesign", "Choose a Study Design:",
                                        #choices = c("All", setdiff(unique(data$s_Study_Design), c(NA, "?", "Uncertain"))),
                                        choices = c("All", 
                                                    "Randomized Controlled Trial" = "RCT",
                                                    "Quasi-Experiment" = "QE"),
                                        selected = "All"
                            ),
                            selectInput("selectedDomain", "Choose an Outcome Domain Group:",
                                        #choices = c("All", sort(setdiff(unique(data$`Outcome Domain Group Expanded`), c(NA, "?", "Uncertain")))),
                                        # choices = list(
                                        #    "Primary Domains" = c("All", sort(unique(data$`Outcome Domain Group`))),
                                        #    "Sub-Domains of Academic Readiness" = c(unique(data$`Outcome Domain Group Expanded`)),
                                        #  ),
                                        #  selected = "All"),
                                        choices = list(
                                          #"Main Domains" = c("A", "B", "C"),
                                          "Main Domains" = c("All", sort(unique(data$`Outcome Domain Group`))),
                                          #"Sub-Domains of A" = c("A1", "A2", "A3")
                                          #"Sub-Domains of A" = unique(data$`Outcome Domain Group Expanded`)
                                          "Sub-Domains of Academic Readiness" = c("Literacy" = "ARKS - Literacy",
                                                                                  "Mathematics" = "ARKS - Mathematics",
                                                                                  "Science" = "ARKS - Science",
                                                                                  "Social Studies" = "ARKS - Social Studies")
                                        )
                            ),
                            
                            selectInput("selectedDichCont", "Choose Dichotomous or Continuous Outcome Measures:",
                                        choices = c("All", setdiff(unique(data$outcome_type), c(NA, "?", "Uncertain"))),
                                        selected = "All"
                            ),
                            selectInput("selectedFindingRating", "Choose WWC Finding Rating:",
                                        choices = c("All", setdiff(unique(data$f_Finding_Rating), c(NA, "?", "Uncertain"))),
                                        selected = "All"
                            ),
                            selectInput("selectedVariable", "Choose a Sensitivity Measure:",
                                        choices = c("RIR as a percentage of Sample Size" = "RIR_percent",
                                                    "Robustness of Inferences to Replacement (RIR)" = "RIR_primary", 
                                                    "Fragility (dichotomous only)" = "fragility_primary.lo",
                                                    "Unselected"),
                                        selected = "RIR_percent"
                            )
                          ),
                          

################################################################################ 

               #           tags$p("Note: These RIR are based on Hedges GC. ", 
              #                   tags$a(href="", "Read more here.")  
              #            )
                       ),

################################################################################ 

                        mainPanel(
                          verticalLayout(
                            fluidRow(
                              br(.noWS = "before"),
                              align = "center",
                              h3("Output")),
                            
                            wellPanel(
                              h4("Distribution"),
                              plotOutput("histPlot", height = "400px")
                            ),
                            
                            wellPanel(
                              h4("Place Your Value in the Distribution"),
                              numericInput("userValue", "Enter your value:", value = NULL),
                              textOutput("percentileResult")
                            ),
                          
                            wellPanel(
                              h4("Descriptive Statistics"),
                              div(
                                style = "overflow-x: auto;",
                                tableOutput("descriptiveStatsTable")
                              )
                            ),
                            
                            
                            column(12,
                                   screenshotButton(inputId = "screenshot_button",
                                                    label = " Take Screenshot"),
                                   actionButton(inputId = "startover_button", 
                                                label = div(icon("rotate-right", lib = "font-awesome"), 
                                                            " Start Over")),
                                   align = "right"
                            )
                        
                          )
                        )
                      )
             ),


################################################################################
###########################  SECOND TAB  #######################################
################################################################################


tabPanel(div(icon("screwdriver-wrench", lib = "font-awesome"), " Resources"),
         
         tags$h4("Overall"),
         tags$ul(
           tags$li(icon("globe", lib = "font-awesome", style = "color: #639dad"), 
                   "Learn more on the",
                   tags$a(href="https://konfound-it.org/", "KonFound-It website.")),
           tags$li(icon("paper-plane", lib = "font-awesome", style = "color: #639dad"), 
                   "Questions? Issues? Suggestions? Reach out through the",
                   tags$a(href="https://groups.google.com/g/konfound-it", "KounFound-It! Google Group.")
           )
         ),
         hr(),
         
         
         tags$h4("Tools"),
         tags$ul(
           tags$li(icon("r-project", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://CRAN.R-project.org/package=konfound", 
                          "R package (CRAN version)")),
           tags$li(icon("r-project", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://github.com/konfound-project/konfound", 
                          "R package (development version)")),
           tags$li(icon("calculator", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://konfound-project.shinyapps.io/konfound-it/", 
                          "R Shiny app")),
           tags$li(icon("stripe-s", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://doi.org/10.1177/1536867X19874223", 
                          "Stata package")),
           tags$li(icon("warehouse", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://konfound-project.shinyapps.io/wwc-sensitivity-benchmark/", 
                          "Benchmarks: What Works Clearinghouse"))
         ),
         hr(),
         
         
         tags$h4("Explanatory Resources"),
         tags$ul(
           tags$li(icon("globe", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://konfound-it.org/page/faq/", "FAQ")), 
           tags$li(icon("images", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://konfound-it.org/page/resources/overview-of-techniques.pptx", "Overview of KonFound techniques")),
           tags$li(icon("file-pdf", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://www.dropbox.com/s/33zkk861g04hocf/Overview%20of%20Konfound%20commands%20with%20inputs%20and%20outputs.docx?dl=0", "Overview of KonFound commands")),
           tags$li(icon("file-pdf", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://drive.google.com/file/d/1qbRx2penqzb7kEJkxJD3ARf26CjXMhzg/view", "Quick examples")), 
           tags$li(icon("images", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://www.dropbox.com/s/o67e7w2sm8uww18/quantifying%20the%20robustness%20of%20causal%20inferences%20combined%20frameworks%20for%20stat%20horizons%20distribute.pptx?dl=0", "Slides quantifying the robustness of causal inferences combined frameworks")),
           tags$li(icon("images", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://www.dropbox.com/s/8t6x00mokkljksh/quantifying%20the%20robustness%20of%20causal%20inferences%20%20comparison%20of%20frameworks.pptx?dl=0", "Slides for comparison of frameworks"))
         ),
         hr(),
         
         
         tags$h4("Resources for Publication"),
         tags$ul(
           tags$li(icon("file-pdf", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://www.dropbox.com/s/bc4ert79kgbsac4/Examples%20of%20applications%20of%20indices%20for%20quantifying%20the%20robustness%20of%20causal%20inferences.docx?dl=0", "Published empirical examples")),
           tags$li(icon("table", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://www.dropbox.com/s/accoz5xu82vy27v/KonFound-it%21%20enhanced.xlsx?dl=0", "Spreadsheet for calculating indices (KonFound-it!)"))
         ),
         hr(),
         
         
         tags$h4("Publications: Impact Threshold for a Confounding Variable"),
         tags$ul(  
           tags$li("Frank, K. (2000). Impact of a confounding variable on the inference of a regression coefficient.",
                   tags$i("Sociological Methods and Research, 29"),
                   "(2), 147-194. | ",
                   tags$a(href="https://drive.google.com/file/d/1F7oGYZ8SS8hnZxSI3Dch_w65Qz6KIRdI/view", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.1177/0049124100029002001", icon("globe", lib = "font-awesome"), "Web version")
           ),
           tags$li("Frank, K. A., Sykes, G., Anagnostopoulos, D., Cannata, M., Chard, L., Krause, A., & McCrory, R. (2008). Does NBPTS certification affect the number of colleagues a teacher helps with instructional matters?.",
                   tags$i("Educational Evaluation and Policy Analysis, 30"),
                   "(1), 3-30. | ",
                   tags$a(href="https://drive.google.com/file/d/1aOvAXEVnQCe9-dbWkgTqtq56Y3Z1tpkg/view", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.3102/0162373707313781", icon("globe", lib = "font-awesome"), "Web version")
           )
         ),
         hr(),
         
         
         tags$h4("Publications: Robustness of Inference to Replacement"),
         tags$ul(  
           tags$li("Frank, K. A., Lin, Q., Maroulis, S., Mueller, A. S., Xu, R., Rosenberg, J. M., Hayter, C. S., Mahmoud, R. A., Kolak, M., Dietz, T., & Zhang, L. (2021). Hypothetical case replacement can be used to quantify the robustness of trial results.",
                   tags$i("Journal of Clinical Epidemiology, 134"),
                   ", 150-159. (authors listed alphabetically.)  | ",
                   tags$a(href="https://www.dropbox.com/s/2dzkvalwmgr5v5z/Hypothetical%20case%20replacement%20can%20be%20used%20to%20quantify%20the%20robustness%20of%20trial%20results%20submit.docx?dl=0", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.1016/j.jclinepi.2021.01.025", icon("globe", lib = "font-awesome"), "Web version")
           ),
           tags$li("Frank, K. A., Maroulis, S. J., Duong, M. Q., & Kelcey, B. M. (2013). What would it take to change an inference? Using Rubin’s causal model to interpret the robustness of causal inferences.",
                   tags$i("Educational Evaluation and Policy Analysis, 35"),
                   "(4), 437-460. | ",
                   tags$a(href="https://drive.google.com/file/d/1aGhxGjvMvEPVAgOA8rrxvA97uUO5TTMe/view", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.3102/0162373713493129", icon("globe", lib = "font-awesome"), "Web version")
           ),
           tags$li("Frank, K. A., & Min, K. (2007). Indices of robustness for sample representation.",
                   tags$i("Sociological Methodology. 37"),
                   "(1). 349-392. (equal first authors.) | ",
                   tags$a(href="https://www.dropbox.com/s/o0rmduhe8pj3khd/INDICES%20OF%20ROBUSTNESS%20FOR%20SAMPLE%20REPRESENTATION.pdf?dl=0", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.1111/j.1467-9531.2007.00186.x", icon("globe", lib = "font-awesome"), "Web version")
           )
         ),                       
         hr(),
         
         
         tags$h4("Publications for Both Frameworks"),
         tags$ul(
           tags$li("Frank, K.A., Lin, Q., Xu, R., Maroulis, S.J., Mueller, A. (2023). Quantifying the robustness of causal inferences: Sensitivity analysis for pragmatic social science.",
                   tags$i("Social Science Research, 110"),
                   ", 102815. | ",
                   tags$a(href="https://www.dropbox.com/s/rn8a4jbxtiynefh/Quantifying%20the%20Robustness%20of%20Causal%20Inferences%20Frank%20SSR%20final.pdf?dl=0", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.1016/j.ssresearch.2022.102815", icon("globe", lib = "font-awesome"), "Web version")
           ),
           tags$li("Narvaiz, S., Lin, Q., Rosenberg, J. M., Frank, K. A., Maroulis, S. J., Wang, W., & Xu, R. (2024). konfound: An R sensitivity analysis package to quantify the robustness of causal inferences.",
                   tags$i("Journal of Open Source Software, 9"),
                   "(95), 5779. | ",
                   tags$a(href="https://doi.org/10.21105/joss.05779", icon("globe", lib = "font-awesome"), "Web version")
           ),
           tags$li("Xu, R., Frank, K. A., Maroulis, S. J., & Rosenberg, J. M. (2019). konfound: Command to quantify robustness of causal inferences.",
                   tags$i("The Stata Journal, 19"),
                   "(3), 523–550. | ",
                   tags$a(href="https://www.researchgate.net/profile/Ran-Xu-6/publication/335956720_konfound_Command_to_quantify_robustness_of_causal_inferences/links/5e49a3d2a6fdccd965ac3564/konfound-Command-to-quantify-robustness-of-causal-inferences.pdf", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.1177/1536867X19874223", icon("globe", lib = "font-awesome"), "Web version")
           )
         ),
         hr(),
         
         tags$h4("Connect"),
         tags$ul(
           tags$li("Project Overview and Details: Peruse the", 
                   tags$a(href="https://konfound-it.org/", "KonFound-It! Website")),
           tags$li("Frequently Asked Questions: Check the", 
                   tags$a(href="https://konfound-it.org/page/faq/", "FAQ page"),
                   "|",
                   tags$a(href="https://www.dropbox.com/s/9eymdekym5g50o7/frequently%20asked%20questions%20for%20application%20of%20konfound-it.docx?dl=0", "FAQ dev version")),
           tags$li("Specific Questions: Ask in the", tags$a(href="https://groups.google.com/g/konfound-it", "KounFound-It! Google Group")),
           tags$li("Issues with the konfound R Package: Post to", tags$a(href="https://github.com/konfound-project/konfound/issues", "konfound GitHub Issues")),
           tags$li("Overall KonFound-It! Project Inquiries: Contact", tags$a(href="https://msu.edu/~kenfrank/", "Ken Frank")),
           tags$li("Sensitivity Analysis Benchmarks - What Works Clearinghouse: Contact", tags$a(href="http://www.public.asu.edu/~smarouli/Spiro_Maroulis/Home.html", "Spiro Maroulis")),
           tags$li("R Package: Contact", tags$a(href="https://www.linkedin.com/in/qinyun-lin-b72763112/", "Qinyun Lin")),
           tags$li("R Shiny App: Contact", tags$a(href="https://joshuamrosenberg.com/", "Joshua Rosenberg")),
           tags$li("Stata Package: Contact", tags$a(href="https://sites.google.com/site/ranxupersonalweb/", "Ran Xu")),
           tags$li("User Guide: Contact", tags$a(href="https://www.cgu.edu/people/guan-saw/", "Guan Saw")),
           tags$li("Website: Contact", tags$a(href="https://bretsw.com", "Bret Staudt Willet"))
         )
         
),


################################################################################

hr(),

################################################################################
############################### Citation #######################################
################################################################################

#tags$p(tags$b("To cite this application:")),
#tags$p(
#  "Maroulis, S., Frank, K., Overstreet, D., & Staudt Willet, K. B. (2024).",
#  tags$i("Sensitivity analysis benchmarks: Data from What Works Clearinghouse"), 
#  "[R Shiny app powered by konfound R package version 1.0.2]."
#),

hr(),

tags$image(style = "height:3.5em; vertical-align:center;", src = "ies-logo.jpg", alt = "Konfound-It! logo"),
tags$p("KonFound-It! is supported by IES Grant",
       tags$a(href="https://ies.ed.gov/funding/grantsearch/details.asp?ID=5723", "#R305D220022"),
       "— 'Quantifying the Robustness of Causal Inferences: Extensions and Application to Existing Databases' "
),

hr(),

tags$p(tags$b(paste("\u00A9", format(Sys.Date(), "%Y"))), "by KonFound-It!")

  )
  )
)
