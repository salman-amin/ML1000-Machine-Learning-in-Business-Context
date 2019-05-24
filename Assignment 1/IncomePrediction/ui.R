

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Income Prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("Age","Age", value = "21", width = NULL, placeholder = NULL),
      selectInput("workclass", "Work Class:",
                  c("Private" = " Private",
                    "Self Employed Not inc" = " Self-emp-not-inc",
                    "Local Government" = " Local-gov",
                    "State Government" = " State-gov",
                    "Self Employed inc" = " Self-emp-inc",
                    "Federal Government" = " Federal-gov",
                    "Never-worked" = "  Never-worked",
                    "Without Pay" = " Without-pay")),
      selectInput("education", "Education:",
                  c("Highscool Graduate" = " HS-grad",
                    "College" = " Some-college",
                    "Bachelors" = " Bachelors",
                    "Masters" = " Masters",
                    "Assoc-voc" = " Assoc-voc",
                    "Doctorate" = " Doctorate",
                    "Masters" = " Masters")),
      selectInput("marStat", " Marital Status:",
                  c("Divorced" = " Divorced",
                    "Married" = " Married-civ-spouse",
                    "Married Absent Spouse" = " Married-spouse-absent",
                    "Never Married" = " Never-married",
                    "Separated" = " Separated",
                    "Widowed" = " Widowed")),
      selectInput("occupation", " Craft-repair",
                  c("Proffesional Specialty" = " Prof-specialty",
                    "Manegerial Exec" = " Exec-managerial",
                    "Adm-clerical" = " Adm-clerical",
                    "Sales" = " Sales",
                    "Armed-Forces" = " Armed-Forces",
                    "Farming-fishing" = " Farming-fishing",
                    "Handlers-cleaners" = " Handlers-cleaners",
                    "Protective services" = " Protective-serv",
                    "Sales" = " Sales",
                    "Tech-support" = " Tech-support",
                    "Other Services" = " Other-service")),
      selectInput("relationship", " Relationship",
                  c("Husband" = " Husband",
                    "No family" = " Not-in-family",
                    "Other relative" = " Other-relative",
                    "Own child" = " Own-child",
                    "Unmarried" = " Unmarried",
                    "Wife" = " Wife")),
      selectInput("Race", "Race",
                  c("White" = " White",
                    "African-American" = " Black",
                    "Other" = " Other")),
      selectInput("Sex", "Sex",
                  c("Female" = " Female",
                    "Male" = " Male")),
      textInput("hrs-per-week","Hours of Work Per Week", value = "40", width = NULL, placeholder = NULL)
      
    ),
  
    
    mainPanel(
      textOutput("incomeOutput")
      
    )
  )
  
  
))
