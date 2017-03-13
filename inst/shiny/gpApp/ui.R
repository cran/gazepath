library(shiny)
tags$script("$(function() {$.fn.dataTableExt.errMode = 'none';});")
shinyUI(navbarPage('Gazepath',
                   tabPanel('Load Data',
                     titlePanel("Load Data"),
                     sidebarLayout(
                       sidebarPanel(
                         fileInput('file1', 'Choose File',
                                   multiple = TRUE,
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         tags$hr(),
                         uiOutput('ppVariables'),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(None='',
                                        Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"'),
                         radioButtons('na', 'NA strings',
                                      c(None='',
                                        Point='.',
                                        Comma=",",
                                        na='NA'),
                                      '.')
                       ),
                       mainPanel(
                         textOutput('textR'),
                         tableOutput('contenth'),
                         tableOutput('contentt') 
                       )
                     )
                   ), 
                   tabPanel('Analyze Data',
                     titlePanel("Analyze data"),
                     sidebarLayout(
                       sidebarPanel(
                         conditionalPanel('input.go',
                           uiOutput('ptVariables')
                         ),
                         uiOutput('nameXVariables'),
                         uiOutput('nameYVariables'),
                         uiOutput('nameDVariables'),
                         uiOutput('nameX2Variables'),
                         uiOutput('nameY2Variables'),
                         uiOutput('nameD2Variables'),
                         uiOutput('nameTVariables'),
                         selectInput('method', 'Choose the method', c('gazepath', 
                                                                      'Mould',
                                                                      'MouldDur',
                                                                      'Tobii',
                                                                      'EyeLink')),
                         numericInput('samplerate', 'Samplerate', 500),
                         numericInput('res_y', 'Screen Resolution Height (px)', 1024),
                         numericInput('res_x', 'Screen Resolution Width (px)', 1280),
                         numericInput('height_px', 'Stimuli Height (px)', 1024),
                         numericInput('width_px', 'Stimuli Width (px)', 1280),
                         numericInput('height_mm', 'Stimuli Height (mm)', 270),
                         numericInput('width_mm', 'Stimuli Width (mm)', 340),
                         textInput('extra_var', 'Variable(s) to keep from the original data. Comma separated, without spaces, e.g., condition,stimuli,etc.', NULL),
                         
                         actionButton('go', 'Go!')
                       ),
                       mainPanel(
                         tags$style(type="text/css",
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }"),
                         textOutput('textF'),
                         tableOutput('data')
                       )
                     )
                   ),
                   navbarMenu('Visualize',
                              tabPanel('Visualize parsing', 
                                       titlePanel('Visualize parsing'),
                                       sidebarLayout(
                                         sidebarPanel(
                                           uiOutput('pppVariables'),
                                           uiOutput('trialVariables'),
                                           sliderInput('plot_h', 'Height of plot in pixel', min = 600, max = 3000, value = 600, step = 100),
                                           sliderInput('plot_w', 'Width of plot in pixel', min = 600, max = 3000, value = 800, step = 100),
                                           actionButton('plotPar', 'Plot')
                                         ),
                                         mainPanel(
                                           conditionalPanel(
                                             'input.plotPar',
                                             plotOutput('plot')
                                           )
                                         )
                                       )),
                              tabPanel('Visualize threshold estimation', 
                                       titlePanel('Visualize threshold estimation'),
                                       sidebarLayout(
                                         sidebarPanel(
                                           uiOutput('ptiVariables'),
                                           uiOutput('treshVariables'),
                                           actionButton('plotThres', 'Plot')
                                         ),
                                         mainPanel(
                                           conditionalPanel(
                                             'input.plotThres',
                                             plotOutput('plotMould')
                                           )
                                         )
                                       )),
                              tabPanel('Visualize stimuli', 
                                       titlePanel('Visualize stimuli'),
                                       sidebarLayout(
                                         sidebarPanel(
                                           fileInput('image', 'Select the stimulus (only jpg format supported at this moment)',
                                                     multiple = FALSE,
                                                     accept='image/jpg'),
                                           uiOutput('plotVariables'),
                                           uiOutput('ppiVariables'),
                                           sliderInput('p_h', 'Height of plot in pixel', min = 600, max = 3000, value = 600, step = 100),
                                           sliderInput('p_w', 'Width of plot in pixel', min = 600, max = 3000, value = 800, step = 100),
                                           actionButton('imageplot', 'Plot fixations')
                                         ),
                                         mainPanel(
                                           conditionalPanel(
                                             condition = 'input.imageplot',
                                             plotOutput('image')
                                           )
                                         )
                                       )),
                              tabPanel('Visualize robustness and precision', 
                                       titlePanel('Visualize robustness and precision'),
                                       sidebarLayout(
                                         sidebarPanel(
                                           actionButton('rp', 'Plot')
                                         ),
                                         mainPanel(
                                           conditionalPanel(
                                             condition = 'input.rp',
                                             plotOutput('rob'),
                                             plotOutput('pre')
                                           )
                                         )
                                       ))
                              ),
                   tabPanel('Get Results',
                     titlePanel('Download Results'),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput('out', 'Choose the output', c('All fixations and saccades',
                                                                   'Only complete fixations and saccades',
                                                                   'Fixations only',
                                                                   'Only complete fixations')),
                         downloadButton('downloadData', 'Download')
                       ),
                       mainPanel(
                         tableOutput('datasum')
                       )
                     )
                   ), position = 'fixed-top', header = tags$style(type="text/css", "body {padding-top: 50px;}"))
        )

