#
## ================= THE ENTIRE UI DEFINITION FOR THE `SHINYAPP01` =======================
#
library(shiny)
library(shinythemes)
library(DT)
#
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  #
  ## visual style:
  shiny::tags$h2(
    shiny::tags$img(src = "logo_new.png",height = 64,width = 64),
    "Continuous Wave (CW) Isotropic EPR Spectrum Dashboard",
    style = "color: #E2E2E2;
            background-color: #00205b;
            border-radius: 6px;
            padding: 6px;"
  ),
  shiny::tags$div(
    shiny::icon("computer"),
    "Data",
    style = "color: #346DB6;font-size = 16pt"
  ),
  fluidRow(
    column(width = 4,
           selectInput(
             inputId = "origin",
             label = "Acquisition software origin",
             choices = c("Xenon","WinEpr","Magnettech"),
             selected = "Xenon"
           )),
    column(width = 4,
           fileInput(
             inputId = "ASCIIfile",
             NULL,
             buttonLabel = shiny::div(
               shiny::icon("file-waveform"),
               "Upload spectrum data"
             ),
             accept = c(".asc",".txt",".csv")
           )),
    column(width = 4,
           fileInput(
             inputId = "ParamsFile",
             NULL,
             buttonLabel = shiny::div(
               shiny::icon("file-invoice"),
               "Upload params. file"
             ),
             accept = c(".dsc",".par")
           ))),
  tabsetPanel(
    tabPanel(
      title = "Spectrum Preview",
      #
      #  --------------------- SIDEBAR --------------------------
      #
      sidebarPanel(
        ## Inputs UI
        checkboxInput(
          inputId = "instPars",
          label = "Show instrumental parameters (params.)",
          FALSE
        ),
        shiny::tags$h4("Spectrum Display Settings"),
        selectInput(
          inputId = "specColor",
          label = "Spectrum color",
          choices = color.select,
          selected = "darkviolet"
        ),
        selectInput(
          inputId = "Bunit",
          label = shiny::HTML("Magnetic flux density (<i>B</i>) unit"),
          choices = c("G","mT"),
          selected = "G"
        ),
        checkboxInput(
          inputId = "gval",
          label = "Show g-Values",
          FALSE
        ),
        conditionalPanel(
          condition = "input.gval == true", # lower case "true" !!!
          numericInput(
            inputId = "MWnuGHz",
            label = "Microwave frequency (GHz)",
            value = 9.8,
            min = 1,
            max = 300
          )
        ),
        numericInput(
          inputId = "qValue",
          label = shiny::HTML("<i>Q</i> Value (sensitivity factor)"),
          value = 1,
          min = 800,
          max = 4200
        ),
        checkboxInput(
          inputId = "normVec",
          label = "Additional normalization",
          FALSE
        ),
        conditionalPanel(
          condition = "input.normVec == true",
          textInput(
            inputId = "vecNorm",
            label = "Enter number or vector (comma delimited)",
            value = "1,1,1"
          )
        ),
        shiny::tags$h4("Sample Characterization"),
        checkboxInput(
          inputId = "structCond",
          label = "Draw paramagnetic compound (radical) structure",
          FALSE
        ),
        conditionalPanel(
          condition = "input.structCond == true",
          textAreaInput(
            inputId = "structure",
            label = "Enter SMILES code",
            rows = 2
          ),
          plotOutput("struct",NULL)
        ),
        textAreaInput(
          inputId = "notes",
          label = "Comment & Notes",
          rows = 4
        )
      ),
      # ------------------- MAIN ------------------------
      mainPanel(
        # plot
        shiny::tags$h3(
          shiny::icon("chart-line"),
          "Spectrum",
          style = h3.style.string
        ),
        plotly::plotlyOutput("plot"),
        # DT table => params:
        conditionalPanel(
          condition = "input.instPars == true",
          shiny::tags$h3(
            shiny::icon("circle-info"),
            "Info",
            style = h3.style.string
          ),
          DT::DTOutput("tab2"),
          shiny::tags$h3(
            shiny::icon("table"),
            "Parameters",
            style = h3.style.string
          ),
          DT::DTOutput("tab1")
        )
      ),
      shiny::tags$footer(
        shiny::icon("r-project"),
        "{eprscope} 📦",
        align = "right"
      )
    ),
    tabPanel(
      title = "Simulation",
      #
      ##  --------------------- SIDEBAR --------------------------
      #
      sidebarPanel(
        ## INPUTS UI
        shiny::tags$h4("Spectrum Display Settings and Table"),
        checkboxInput(
          inputId = "simOverlay",
          label = "Overlay experimental & simulated spectrum",
          TRUE
        ),
        checkboxInput(
          inputId = "exprSimTable",
          label = "Show interactive table (data frame)",
          FALSE
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = "exprColor",
              label = "Expr. spectrum color",
              choices = color.select,
              selected = "red"
            )
          ),
          column(
            width = 6,
            selectInput(
              inputId = "simColor",
              label = "Sim. spectrum color",
              choices = color.select,
              selected = "blue"
            )
          )
        ),
        uiOutput("Bslider"),
        shiny::tags$h4("Parameter Estimation"),
        numericInput(
          inputId = "giso",
          label = shiny::HTML("Enter <i>g</i>(iso)"),
          value = 2.002319,
          min = 1,
          max = 10
        ),
        checkboxInput(
          inputId = "splitCond",
          label = "Hyperfine splitting/coupling",
          FALSE
        ),
        conditionalPanel(
          condition = "input.splitCond == true",
          shiny::tags$h5("System of interacting nuclei"),
          textAreaInput(
            inputId = "nuclearSys",
            label = shiny::HTML("Enter interacting groups of equivalent nuclei, </br>
                              like: `14N,1,45` &equiv; nucleus,number and A(MHz), </br>
                              each group on separate line"),
            rows = 6
          ),
          numericInput(
            inputId = "aAconv",
            label = shiny::HTML("<i>a</i> (mT) &rArr; <i>A</i> (MHz) conversion,
                                enter <i>a</i> in mT"),
            NULL
          ),
          textOutput("aAconverted")
        ),
        shiny::tags$h5("EPR Spectrum Line Form"),
        sliderInput(
          inputId = "Gcontent",
          label = shiny::HTML("Gaussian (Gau) line <i>form</i> content (<i>x</i>) </br>
                              in pseudo-Voigt <i>form</i> = [ <i>x</i> Gau + (1 - <i>x</i>) Lor ], </br>
                              where Lor = Lorentz <i>form</i>"),
          value = 0.5,
          min = 0,
          max = 1,
          step = 0.01
        ),
        ## the Delta Bpp to be updated within the server (assign NULL if 0):
        sliderInput(
          inputId = "DeltaBppG",
          label = shiny::HTML("&Delta;<i>B</i><sub>pp</sub> (Gau)
                              in corresponding </br>
                              <i>B</i> units (see `Spectrum Preview`)"),
          value = 0.5,
          min = 0,
          max = 10,
          step = 0.01
        ),
        ## the Delta Bpp to be updated within the server (assign NULL if 0):
        sliderInput(
          inputId = "DeltaBppL",
          label = shiny::HTML("&Delta;<i>B</i><sub>pp</sub> (Lor)
                              in corresponding </br>
                              <i>B</i> units (see `Spectrum Preview`)"),
          value = 0.5,
          min = 0,
          max = 10,
          step = 0.01
        )
      ),
      #
      ## ----------------------- MAIN -----------------------------
      #
      mainPanel(
        # plot
        shiny::tags$h3(
          shiny::icon("chart-line"),
          "Experimental (Expr.) & Simulated (Sim.) Spectrum",
          style = h3.style.string
        ),
        plotOutput("simPlot"),
        fluidRow(
          column(width = 4,
                 selectInput(
                   inputId = "figformat",
                   label = "Plot/Spectrum save format",
                   choices = c("png","pdf","jpeg"),
                   selected = "png"
                 )),
          column(width = 4,
                 textInput(
                   inputId = "figfilename",
                   label = "File name to save plot/spectrum",
                   NULL
                 )),
          column(width = 4,
                 shiny::br(), ## blank line
                 downloadButton(
                   outputId = "simPlotsave",
                   label = "Save plot/spectrum",
                   icon = shiny::icon("floppy-disk")
                 ))
        ),
        conditionalPanel(
          condition = "input.exprSimTable == true",
          shiny::tags$h3(
            shiny::icon("table"),
            "Table (Data Frame) of Expr. & Sim. Spectrum",
            style = h3.style.string
          ),
          DT::DTOutput("simTab")
        ),
        shiny::tags$footer(
          shiny::icon("r-project"),
          "{eprscope} 📦",
          align = "right"
        )
      )
    )
    #
  )
)
#
