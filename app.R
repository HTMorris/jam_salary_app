#load packages
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(bslib)
library(thematic)
library(gt)
library(DT)
# Build Net Pay Funciton

net_pay_func <- function(basic_salary=0,
                         additional_taxed_income=0,
                         additional_non_taxed_income=0,
                         
                         # Insurance
                         health_insurance = 0,
                         life_insurance =0,
                         house_insurance =0,
                         car_insurance =0,
                         other_insurance = 0,
                         
                         #Loans
                         car_loan = 0,
                         morgtage = 0,
                         other_loan = 0,
                         
                         # Savings
                         savings = 0,
                         pension_rate=0.05, 
                         
                         # Frequency
                         per_year = 1
                         
){
  
  # Indicators
  income_tax_threshold = 1500096/per_year
  
  salary_taxed = basic_salary + additional_taxed_income
  
  total_insurance = health_insurance + life_insurance + 
    house_insurance + car_insurance + 
    other_insurance
  
  total_loans = car_loan + morgtage + other_loan
  
  if(salary_taxed>6000000/per_year){
    nis = (5000000/per_year) *0.03
    pension = salary_taxed*pension_rate
    statuatory_income =  salary_taxed - nis - pension
    edu_tax = statuatory_income * 0.0225
    nht = salary_taxed*0.02
    
    paye <- max((6000000/per_year-income_tax_threshold)*0.25 + (statuatory_income-6000000/per_year)*0.3, 0)
    
  }
  
  else if(salary_taxed<=6000000/per_year & salary_taxed>=5000000/per_year){
    
    nis = (5000000/per_year)*0.03
    pension = salary_taxed*pension_rate
    statuatory_income =  salary_taxed - nis - pension
    edu_tax = statuatory_income * 0.0225
    nht = salary_taxed*0.02
    
    paye <- max((statuatory_income-income_tax_threshold)*0.25, 0)
    
  }
  
  else if(salary_taxed<5000000/per_year & salary_taxed>income_tax_threshold){
    
    nis <- salary_taxed*0.03
    pension <- salary_taxed*pension_rate
    statuatory_income <-  salary_taxed - nis - pension
    edu_tax <- statuatory_income * 0.0225
    nht <- salary_taxed*0.02
    
    sub_total_deduction = nis + nht + pension + edu_tax
    
    
    paye <- max((statuatory_income-income_tax_threshold)*0.25, 0)
    
    
  }  
  
  
  else if(salary_taxed<=income_tax_threshold){
    
    nis <- salary_taxed*0.03
    pension <- salary_taxed*pension_rate
    statuatory_income <-  salary_taxed - nis - pension
    edu_tax <- statuatory_income * 0.0225
    nht <- salary_taxed*0.02
    sub_total_deduction = nis + nht + pension + edu_tax
    #income_tax_threshold <- 1500096.0
    
    paye <- max((statuatory_income-income_tax_threshold)*0,0)
    
  }
  
  total_statutory_deductions = nis + edu_tax + nht + paye
  total_statutory_pension_deductions = total_statutory_deductions + pension
  
  Net_Pay = salary_taxed + additional_non_taxed_income - total_statutory_pension_deductions - total_insurance - total_loans - savings
  
  table <- tibble(
    
    `Basic Salary` = basic_salary,
    `Additional Income(Taxed)` = additional_taxed_income,
    `Additonal Income (Non-Taxed)` = additional_non_taxed_income,
    `Total Income` = `Basic Salary` + `Additional Income(Taxed)` + `Additonal Income (Non-Taxed)`,
    PENSION = pension,
    NIS=nis,
    STATUATORY_INCOME = statuatory_income,
    EDUCATION_TAX = edu_tax,
    NHT = nht,
    INCOME_TAX_THRESHOLD = income_tax_threshold,
    PAYE = paye,
    TOTAL_STATUTORY_DEDUCTION = total_statutory_deductions,
    TOTAL_NON_STATUTURY_DEDUCTIONS =  total_insurance + total_loans + savings + pension,
    NET_PAY = Net_Pay
  ) %>% 
    pivot_longer(cols = everything(), names_to = "Indicators", values_to = "Value") %>% 
    mutate(Value = formattable::currency(Value),
           `ANNUAL VALUES` = Value * per_year,
           `MONTHLY VALUES` = `ANNUAL VALUES`/12,
           `FORTNIGHTLY VALUES` = `ANNUAL VALUES`/26,
           `WEEKLY VALUES` = `ANNUAL VALUES`/52,
           Indicators = str_replace_all(Indicators, "_", " ")
    ) %>% 
    select(-2)
  return(table)
  
  
}


net_pay_func(basic_salary = 200000, per_year = 12)

### APP


# Define UI
ui <- navbarPage(
  
  theme = bs_theme(
    bg = "#00707a",
    fg = "#FFFFFF",
    primary = "#000000",
    base_font = font_google("Prompt"),
    code_font = font_google("JetBrains Mono")
  ),
  # Page title
  titlePanel("NET SALARY CALCULATOR"),
  
  # Input form
  sidebarLayout(
    sidebarPanel(
      selectInput("freq", "Select Payment frequency:", 
                  choices = c("Annually", "Monthly", "Forenightly", "Weekly"), selected = "Monthly"),
      numericInput("basic_salary", "Basic Salary:", 0, min = 0),
      numericInput("additional_taxed_income", "Additional Income (TAXED):", 0, min = 0),
      numericInput("additional_non_taxed_income", "Additional Income (NON-TAXED):", 0, min = 0),
      #Insurance
      numericInput("health_insurance", "Health Insurance:", 0, min = 0),
      # numericInput("life_insurance", "Life Insurance:", 0, min = 0),
      # numericInput("house_insurance", "House Insurance:", 0, min = 0),
      # numericInput("car_insurance", "Car Insurance:", 0, min = 0),
      numericInput("other_insurance", "Other Insurance:", 0, min = 0),
      # Loans
      numericInput("car_loan", "Car Loan:", 0, min = 0),
      numericInput("morgtage", "Morgtage:", 0, min = 0),
      numericInput("other_loan", "Other Loan:", 0, min = 0),
      #
      numericInput("savings", "Savings:", 0, min = 0),
      numericInput("pension_rate", "Pension Rate:", 0, min = 0, step = 0.005, max=0.2)
      
      #  actionButton("reset", "Reset Salary")
    ),
    
    # Output table and graph
    mainPanel(
      tabsetPanel(
        tabPanel("Montly Table", tableOutput("monthly_sal_table")),
        tabPanel("Forenightly Table", tableOutput("forenightly_sal_table")),
        tabPanel("Weekly Table", tableOutput("weekly_sal_table")),
        tabPanel("Annual Table", tableOutput("annual_sal_table"))
        
        # tabPanel("Expenses Graph", plotOutput("expensesPlot"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  per_year <- reactive({
    if(input$freq=="Annually"){
      1
    }
    else if(input$freq=="Monthly"){
      12
    }
    else if(input$freq=="Forenightly"){
      26
    }
    else if(input$freq=="Weekly"){
      52
    }
    
  } )
  
  # Calculate net salary
  
  net_pay <- reactive({
    net_pay_func(basic_salary =input$basic_salary,
                 additional_taxed_income=input$additional_taxed_income,
                 additional_non_taxed_income=input$additional_non_taxed_income,
                 # Insurance
                 health_insurance = input$health_insurance,
                 # life_insurance =input$life_insurance,
                 # house_insurance =input$house_insurance,
                 # car_insurance =input$car_insurance,
                 other_insurance = input$other_insurance,
                 
                 #Loans
                 car_loan = input$car_loan,
                 morgtage = input$morgtage,
                 other_loan = input$other_loan,
                 
                 # Savings
                 savings = input$savings,
                 pension_rate=input$pension_rate, 
                 
                 
                 
                 
                 
                 
                 
                 
                 per_year = per_year())
    
  })
  
  
  
  
  output$annual_sal_table <- renderTable({
    
    net_pay()%>% 
      select(Indicators,  `ANNUAL VALUES`) %>% 
      gt() %>% 
      fmt_currency( `ANNUAL VALUES`, decimals = 2)
    
    
  })
  
  output$monthly_sal_table <- renderTable({
    net_pay()%>% 
      select(Indicators,  `MONTHLY VALUES`) %>% 
      gt() %>% 
      fmt_currency( `MONTHLY VALUES`, decimals = 2)
    #mutate(across(where(is.numeric), ~formattable::currency(.,digits=2)))
    
    
  })
  
  
  output$forenightly_sal_table <- renderTable({
    net_pay()%>% 
      select(Indicators,  `FORTNIGHTLY VALUES`) %>% 
      gt() %>% 
      fmt_currency( `FORTNIGHTLY VALUES`, decimals = 2)
  })
  
  output$weekly_sal_table <- renderTable({
    net_pay()%>% 
      select(Indicators,  `WEEKLY VALUES`) %>% 
      gt() %>% 
      fmt_currency( `WEEKLY VALUES`, decimals = 2)
    
  })
  
  
}

shinyApp(ui, server)






