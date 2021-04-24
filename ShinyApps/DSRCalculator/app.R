library(shiny);
library(shinyjs);
library(shinythemes);
library(jsonlite);

tax.json <- fromJSON("./tax.json")["bracket"];
tax.display <- as.data.frame(tax.json);
colnames(tax.display) <- c("Annual Income", "Deductible in RM", "% to Charge on Excess");
bracket_incomes <- tax.json$bracket$income;

ui <- fluidPage(
    shinyjs::useShinyjs(),
    theme= shinytheme("slate"),
    titlePanel("Malaysia Debt Service Ratio (DSR) Calculator Version FY2021",
               windowTitle="Msia DSR 2021"),
    sidebarLayout(
        sidebarPanel(
            width=4,
            # Loan application
            numericInput(
                inputId = "loanAmount",
                label = "Amount of Loan to Apply (RM)",
                value = 0,
                min = 0
            ),
            numericInput(
                inputId = "loanRate",
                label = "Rate of Loan in Percentage (%)",
                value = 0,
                min = 0,
                max = 100
            ),
            numericInput(
                inputId = "loanDuration",
                label = "Duration of Loan (Years)",
                value = 0,
                min = 0,
                max = 100
            ),
            # Income input section
            tags$p("Input your latest 3 months income below."),
            div(style="margin-top:-10px"),
            tags$p("Note: Order does not matter."),
            numericInput(
               inputId = "income1",
               label = "Income 1 (RM)",
               value = 0,
               min = 0
            ),
            div(style="margin-top:-10px"),
            numericInput(
               inputId = "income2",
               label = "Income 2 (RM)",
               value = 0,
               min = 0
            ),
            div(style="margin-top:-10px"),
            numericInput(
               inputId = "income3",
               label = "Income 3 (RM)",
               value = 0,
               min = 0
            ),
            # Commitment input section
            numericInput(
                inputId = "commitment",
                label = "Commitment/Month",
                value = 0,
                min = 0
            ),
            # EPF Calculation Section
            numericInput(
                inputId = "epf",
                label = "EPF Contribution in Percentage (%)",
                value = 0,
                min = 0,
                max = 100
            )
        ),
        mainPanel(
            width=8,
            tags$h2("Forecast Summary"),
            splitLayout(cellWidths=c("auto", "auto"),
                disabled(numericInput("outputNet", "Forecasted Annual Net Income (RM)", 0)),
                disabled(numericInput("outputGross", "Forecasted Annual Gross Income (RM)", 0))
            ),
            splitLayout(cellWidths=c("auto", "auto"),
                disabled(numericInput("outputTax","Maximum Tax Payable (RM)", 0)),
                disabled(numericInput("outputCommitment", "Forecasted Annual Commitment (RM)", 0))
            ),
            splitLayout(cellWidths=c("auto", "auto"),
                disabled(numericInput("outputEPF","Forecasted Annual EPF (RM)", 0)),
                disabled(numericInput("outputSocso", "Forecasted Annual SOCSO (RM)", 0))
            ),
            splitLayout(cellWidths=c("auto", "auto"),
                disabled(textInput("outputPreDSR","DSR Before Loan in Percentage (%)", "Pending Input")),
                disabled(numericInput("outputRepayment", "Loan Monthly Repayment (RM)", 0))
            ),
            splitLayout(cellWidths=c("auto", "auto"),
                disabled(textInput("outputPostDSR", "DSR After Loan in Percentage (%)", "Pending Input")),
                disabled(textInput("score", "Suggested Application Result (Below 40%)", "Pending Input"))
            )
        )
    ),
    tags$hr(),
    # Tax bracket information for FY 2021
    tags$h3("Malaysia Tax Bracket Information for FY 2021"),
    tableOutput("taxTBL")
)

server <- function(input, output, session) {
    eventListener <- reactive({
        list(input$loanAmount, input$loanRate, input$loanDuration, input$income1, input$income2, input$income3, input$commitment, input$epf);
    });
    observeEvent(eventListener(),{
        # Update annual gross
        gross <- round((input$income1+input$income2+input$income3)*4, 2);
        updateNumericInput(session, "outputGross", value=gross);
        # Update tax payable
        tax <- round(deductible(gross), 2);
        updateNumericInput(session, "outputTax", value=tax);
        # Update Commitment
        commitment <- round(input$commitment*12, 2);
        updateNumericInput(session, "outputCommitment", value=commitment);
        # Update EPF
        epf <- round(epf_contribution(gross, input$epf), 2); 
        updateNumericInput(session, "outputEPF", value=epf);
        # Update socso
        socso <- round(socso_contribution(gross), 2);
        updateNumericInput(session, "outputSocso", value=socso);
        # Update annual net
        net <- gross-tax-commitment-epf-socso;
        updateNumericInput(session, "outputNet", value=net);
        # Update loan repayment
        repayment <- repayment_month(input$loanAmount, input$loanRate, input$loanDuration);
        updateNumericInput(session, "outputRepayment", value=repayment);
        # Update DSR before loan
        preDSR <- dsr_before(commitment, net);
        updateTextInput(session, "outputPreDSR", value=preDSR);
        # # Update DSR after loan
        postDSR <- dsr_after(commitment, repayment, net);
        updateTextInput(session, "outputPostDSR", value=postDSR);
        # Update Scoring
        updateTextInput(session, "score", value=dsr_score(postDSR));
        }
    );
    # Worker to calculate tax payable --- incomplete need to load json
    deductible <- function(gross) {
        # error capture
        if (is.na(gross)){
            gross=0;
        };
        # Capture index
        tax_index <- 0;
        for (bracket_income in bracket_incomes) {
            if (gross >= bracket_income) {
                tax_index = match(bracket_income, bracket_incomes);
            } else {
                break;
            };
        };
        # Get the variables
        income <- tax.json$bracket$income[tax_index];
        pcb <- tax.json$bracket$pcb[tax_index];
        rate <- tax.json$bracket$excess[tax_index];
        # excess <- (gross - deducted) * tax.json(1)
        return(pcb+((gross-income)*rate));
    };
    # Worker to calculate epf
    epf_contribution <- function(gross, percentage) {
        return(gross/100*percentage);
    };
    # Worker to calculate socso
    socso_contribution <- function(gross) {
        rate <- 0.5; # employee fixed at 0.5%
        return(gross/100*rate);
    };
    # Worker on DSR before
    dsr_before <- function(commitment, net) {
        if (!(commitment/net) || is.na(commitment/net)) {
            return(0);
        } else if (commitment/net > 0 & commitment/net < Inf) {
            return(round(commitment/net*100, 2))
        } else {
            return("Invalid Application");
        };
    };
    # Worker to calculate monthly repayment
    repayment_month <- function(amount, rate, years) {
        if ((!amount || is.na(amount)) || (!rate || is.na(rate))|| (!years || is.na(years))) {
            return(0);
        } else {
            r <- rate/100/12;
            n <- years*12;
            return(round(amount*r*(1+r)**n/((1+r)**n-1), 2));
        };
    };
    # Worker on DSR after
    dsr_after <- function(commitment, repayment, net) {
        if (!((commitment+repayment)/net) || is.na((commitment+repayment)/net)) {
            return(0);
        } else if ((commitment+repayment)/net > 0 & (commitment+repayment)/net < Inf ) {
            return(round((commitment+repayment)/net*100, 2));
        } else {
            return("Invalid Application");
        };
    };
    # Worker to score if application is good or bad
    dsr_score <- function(DSR) {
        recommended <- 40;
        if (!is.numeric(DSR)) {
            return("Pending Input");
        } else if (DSR <= recommended) {
            return("Good Application");
        } else {
            return("Bad Application");
        };
    };
    # Tax Table
    output$taxTBL <- renderTable(tax.display, width="30%");
}

# Run the application 
shinyApp(ui = ui, server = server)
