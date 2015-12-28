library(shiny)

shinyUI(
#   titlePanel("Tecan PicoGreen GUI"),
  navbarPage("Tecan PicoGreen GUI",
             tabPanel('Measurements',
                      wellPanel(fluidRow(column(12,
                                                column(2, textInput(inputId = "measurements", label = "Measurements (96-well plate)", value = "2740  1255	677	306	174	103	66	46	44	43	45	43 2730	1414	622	306	168	97	56	45	43	45	45	42 438	752	1064	454	609	929	360	538	566	35	40	37 575	714	968	483	618	957	406	611	606	38	37	37 192	40	222	1047	25	24	25	24	23	24	22	23 383	39	242	1039	22	23	24	24	25	23	23	23 24	21	23	23	23	22	26	23	23	22	23	22 22	22	24	23	22	22	23	24	23	22	22	24")),
                                                column(2, textInput(inputId = "background.position", label = "Background wells", value = "A12")),
                                                column(2, textInput(inputId = "dilution.factor", label = "Dilution factor value", value = "10"))))),
                      wellPanel(fluidRow(column(12, 
                                                column(2, textInput(inputId = "std.curve.position", label = "Standard curve wells", value = "B1 B2 B3 B4 B5 B6 B7 B8")),
                                                column(2, textInput(inputId = "starting.conc", label = "Starting concentration", value = "1")),
                                                column(2, textInput(inputId = "serial.dilution.factor", label = "Serial dilution factor", value = "2"))
                      ))),
                      wellPanel(fluidRow(column(12, 
                                                column(2, textInput(inputId = "group1", label = "Group 1 name", value = "1 DBS 12/17"), textInput(inputId = "group1.pos", label = "Group 1 wells", value = "C1, C4, C7 D1, D4, D7")),
                                                column(2, textInput(inputId = "group2", label = "Group 2 name", value = "2 DBS 12/17"), textInput(inputId = "group2.pos", label = "Group 2 wells", value = "C2, C5, C8 D2, D5, D8")), 
                                                column(2, textInput(inputId = "group3", label = "Group 3 name", value = "3 DBS 12/17"), textInput(inputId = "group3.pos", label = "Group 3 wells", value = "C3, C6, C9 D3, D6, D9")), 
                                                column(2, textInput(inputId = "group4", label = "Group 4 name", value = "Blank 12/17"), textInput(inputId = "group4.pos", label = "Group 4 wells", value = "C10, C11, C12 D10, D11, D12")),
                                                column(2, textInput(inputId = "group5", label = "Group 5 name", value = "DBS 12/15"), textInput(inputId = "group5.pos", label = "Group 5 wells", value = "E1, E3, F1, F3")),
                                                column(2, textInput(inputId = "group6", label = "Group 6 name", value = "Saliva 12/15"), textInput(inputId = "group6.pos", label = "Group 6 wells", value = "E4, F4"))
                      ))), 
                      
                      submitButton("Submit"),
                      
                      hr(),
                      
                      fluidRow(column(12, h5("96-well plate"), verbatimTextOutput("plate"))),
                      fluidRow(column(4, h5("Standard curve"), plotOutput("std_curve")), column(8,  h5("Summary stats"), plotOutput("sample_plot")))
             ),
             tabPanel('Concentration',
                      fluidRow(column(6, tableOutput("table")))
             ),
             tabPanel('Summary Stats',
                      fluidRow(column(6, tableOutput("summary_stats")))
             )
  )
)# End Shiny