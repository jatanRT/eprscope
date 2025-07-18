#
## ============= SERVER SETTINGS FOR THE `SHINYAPP01` ==============
#
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(magrittr)
library(ggplot2)
library(openxlsx)
#
server <- function(input, output,session) {
  #
  ## redefinition of `norm.vec.add`
  norm_vec <- reactiveValues(val = NULL)
  observe({
    if (isTRUE(input$normVec)){
      norm_vec$val <- as.numeric(unlist(strsplit(input$vecNorm,",")))
    } else {
      norm_vec$val <- NULL
    }
  })
  #
  ## Load experimental spectrum data frame
  expr_data <- reactive({
    #
    shiny::req(input$ASCIIfile)
    #
    spectr.data <-
      readEPR_Exp_Specs(
        path_to_ASC = input$ASCIIfile$datapath,
        col.names = switch(
          3-origin.cond(orig = input$origin),
          c("index","B_G", "dIepr_over_dB"),
          c("B_G", "dIepr_over_dB"),
          c("B_mT","dIepr_over_dB")
        ),
        x.unit = switch(
          3-origin.cond(orig = input$origin),
          "G",
          "G",
          "mT"
        ),
        qValue = input$qValue,
        origin = input$origin,
        norm.vec.add = norm_vec$val
      )
    spectr.data
  })
  #
  ## B-range/zoom for the EPR spectrum
  output$Bslider <- renderUI({
    df <- expr_data()
    sliderInput(
      inputId = "Brange",
      label = shiny::HTML("Magnetic flux density (<i>B</i>) range"),
      min = round(min(df[[paste0("B_",input$Bunit)]]),digits = 2),
      max = round(max(df[[paste0("B_",input$Bunit)]]),digits = 2),
      value = c(
        round(min(df[[paste0("B_",input$Bunit)]]),digits = 2),
        round(max(df[[paste0("B_",input$Bunit)]]),digits = 2)
      )
    )
  })
  #
  # -------------------- INTERACTIVE SPECTRUM ---------------------
  #
  output$plot <- plotly::renderPlotly({
    #
    ## add g-Value condition:
    if (isTRUE(input$gval)) {
      expr_data_g <- expr_data() %>%
        dplyr::mutate(g_Value =
                        eval_gFactor(
                          nu.val = input$MWnuGHz,
                          B.val = .data$B_mT
                        ))
    }
    #
    ## update numeric input frequency
    ## in order to automatically take the value from
    ## params. file
    if (isTRUE(input$gval) & isTRUE(input$instPars)) {
      updateNumericInput(
        session,
        "MWnuGHz",
        value = readEPR_param_slct(
          path_to_dsc_par = input$ParamsFile$datapath,
          string = switch(
            3-origin.cond(orig = input$origin),
            "MWFQ",
            "MF",
            "MWFQ"
          ),
          origin = input$origin
        ) * switch(3-origin.cond(orig = input$origin),1e-9,1,1e-9)
      )
    }
    #
    ## interactive plot (based on g-Value condition):
    if (isFALSE(input$gval)) {
      plot_EPR_Specs2D_interact(
        expr_data(),
        x = paste0("B_",input$Bunit),
        x.unit = input$Bunit,
        line.colors = input$specColor
      )
    } else {
      plot_EPR_Specs2D_interact(
        expr_data_g,
        x = "g_Value",
        x.unit = "Unitless",
        line.colors = input$specColor
      )
    }
    #
  })
  #
  # -------------------- TABLES (PARAMS.) -------------------
  #
  ## tables:
  output$tab1 <- DT::renderDT({
    ## requirements:
    shiny::req(input$ParamsFile)
    #
    readEPR_params_tabs(
      path_to_dsc_par = input$ParamsFile$datapath,
      origin = input$origin
    )$params
  },
  extensions = c("Buttons","Scroller"),
  options = list(
    dom = "Bfrtip",
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    deferRender = TRUE,
    scrollY = 320,
    scroller = TRUE
  )
  )
  output$tab2 <- DT::renderDT({
    ## requirements:
    req(input$ParamsFile)
    #
    readEPR_params_tabs(
      path_to_dsc_par = input$ParamsFile$datapath,
      origin = input$origin
    )$info
  },
  extensions = "Buttons",
  options = list(
    dom = "Bfrtip",
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  )
  )
  #
  # ----------------- COMPOUND STRUCTURE ------------------
  #
  ## compound structure
  output$struct <- renderPlot({
    #
    draw_molecule_by_rcdk(
      molecule = input$structure,
      style = "nob"
    )
  })
  #
  # ----------------- a to A CONVERSION -------------------
  #
  ## convert:
  output$aAconverted <- renderText({
    A.conv <- convert_a_mT_2A(a.mT = input$aAconv,g.val = input$giso)
    paste0("A = ",A.conv," MHz")
  })
  #
  # ------------------ SIMULATION ------------------------
  #
  #
  ## updating DeltaBpp and content based on Gaussian (Gau) content:
  ## Linewidth
  observeEvent(req(input$Gcontent == 0),{
    updateNumericInput(
      session,
      "DeltaBppG",
      value = 0
    )
  })
  #
  observeEvent(req(input$Gcontent == 1),{
    updateNumericInput(
      session,
      "DeltaBppL",
      value = 0
    )
  })
  #
  ## Gaussian Content
  observeEvent(req(input$DeltaBppG == 0),{
    updateNumericInput(
      session,
      "Gcontent",
      value = 0
    )
  })
  #
  observeEvent(req(input$DeltaBppL == 0),{
    updateNumericInput(
      session,
      "Gcontent",
      value = 1
    )
  })
  #
  DeltaBGpp <- reactiveValues(val = NULL)
  observe({
    if (input$DeltaBppG == 0) {
      DeltaBGpp$val <- NULL
    } else {
      DeltaBGpp$val <- input$DeltaBppG
    }
  })
  #
  DeltaBLpp <- reactiveValues(val = NULL)
  observe({
    if (input$DeltaBppL == 0) {
      DeltaBLpp$val <-  NULL
    } else {
      DeltaBLpp$val <- input$DeltaBppL
    }
  })
  #
  ## variables and conditions for data frame of the simulated spectrum,
  ## interacting nuclei list:
  nucs_list <- reactive({
    if (isFALSE(input$splitCond)) {
      return(NULL)
    } else {
      ## nuclear system definition and conversion into list
      ## required for `eval_sim_EPR_iso`
      ## into vector:
      nucs.string.vec <- unlist(strsplit(input$nuclearSys,split = "\n"))
      ## into list:
      nucs.string.list.01 <- lapply(nucs.string.vec, function(n) n)
      ## separate inner string:
      nucs.string.list.02 <-
        lapply(nucs.string.list.01, function(str) unlist(strsplit(str,",")))
      ## final list:
      nucs.list.final <- list()
      for (i in 1:length(nucs.string.list.02)) {
        nucs.list.final[[i]] <-
          lapply(nucs.string.list.02[[i]],convert_str2list)
      }
      return(nucs.list.final)
    }
  })
  #
  ## required 'nuclear' list interaction (Entire Characteristics)
  nucleiCharacter <- reactive({ ## `reactiveValues()` does NOT WORK !!
    #
    interact.nuclei <- list()
    #
    if (isTRUE(input$splitCond)) {
      nested_list <- any(sapply(nucs_list(), is.list))
      if (isFALSE(nested_list)){
        nucs_list_gr <- list(nucs_list())
      } else {
        nucs_list_gr <- nucs_list()
      }
      ## extract list components and convert them into vectors
      interact.nuclei$nucle_us_i <- sapply(
        1:length(nucs_list_gr),
        function(e) nucs_list_gr[[e]][[1]]
      )
      interact.nuclei$N_nuclei <- sapply(
        1:length(nucs_list_gr),
        function(e) nucs_list_gr[[e]][[2]]
      )
      interact.nuclei$A_iso_MHz <- sapply(
        1:length(nucs_list_gr),
        function(e) nucs_list_gr[[e]][[3]]
      )
    }
    return(interact.nuclei)
  })
  #
  ## simulated spectrum data frame:
  sim_data <- reactive({
    sim.spec.df <-
      eval_sim_EPR_iso(
        g.iso = input$giso,
        instrum.params = c(
          Bcf = stats::median(expr_data()[[paste0("B_",input$Bunit)]]),
          Bsw = max(expr_data()[[paste0("B_",input$Bunit)]]) -
            min(expr_data()[[paste0("B_",input$Bunit)]]),
          Npoints = nrow(expr_data()),
          mwGHz = input$MWnuGHz
        ),
        path_to_dsc_par = NULL,
        origin = input$origin,
        B.unit = input$Bunit,
        nuclear.system = nucs_list(),
        lineG.content = input$Gcontent,
        lineGL.DeltaB = list(DeltaBGpp$val,DeltaBLpp$val)
      )$df
    sim.spec.df
  })
  #
  ## function for experimental and simulated spectrum plot:
  expr_sim_plot <- reactive({
    ## ----- title and caption for the plot ------
    #
    char.title <-
      switch(2-isFALSE(input$splitCond),
             "Non-Interacting Paramagnetic Center/Radical",
             mapply(function(x,y,z)
               paste0("A(",x," x ",y,") = ",z," MHz"),
               nucleiCharacter()[["N_nuclei"]],
               nucleiCharacter()[["nucle_us_i"]],
               nucleiCharacter()[["A_iso_MHz"]]))
    #
    if (isTRUE(input$splitCond)){
      ## separate description into several lines
      if (length(char.title) <= 3){
        char.title <- paste(unname(char.title), collapse = ", ")
        char.title.title <-
          paste("EPR Spectrum Simulation with ",char.title,sep = "\n")
      }
      if (length(char.title) > 3){
        ## first line variable =>
        char.title1L <- paste(unname(char.title[1:3]), collapse = ", ") ## 1st Line
      }
      if (length(char.title) > 3 & length(char.title) <= 6){
        char.title2L <-
          paste(unname(char.title[4:length(char.title)]), collapse = ", ") ## 2nd Line
        char.title.title <- paste(
          "EPR Spectrum Simulation with ",
          char.title1L,
          char.title2L,
          sep = "\n"
        )
      }
      if (length(char.title) > 6){
        ## second line variable =>
        char.title2L <-
          paste(unname(char.title[4:6]), collapse = ", ") ## 2nd Line
      }
      if (length(char.title) > 6 & length(char.title) <= 9){
        char.title3L <-
          paste(unname(char.title[7:length(char.title)]), collapse = ", ") ## 3rd Line
        char.title.title <- paste(
          "EPR Spectrum Simulation with ",
          char.title1L,
          char.title2L,
          char.title3L,
          sep = "\n"
        )
      }
      if (length(char.title) > 9 & length(char.title) <= 12){
        char.title3L <-
          paste(unname(char.title[7:9]), collapse = ", ") ## 3rd Line
        char.title4L <-
          paste(unname(char.title[10:length(char.title)]), collapse = ", ") ## 4th Line
        char.title.title <- paste(
          "EPR Spectrum Simulation with ",
          char.title1L,
          char.title2L,
          char.title3L,
          char.title4L,
          sep = "\n"
        )
      }
      if (length(char.title) > 12){
        char.title.title <- paste("EPR Spectrum Simulation with ",
                                  "> 12 Groups of Equivalent Nuclei.",
                                  sep = "\n")
      }
    } else {
      char.title.title <-
        paste("EPR Spectrum Simulation of ",char.title,sep = "\n")
    }
    #
    ## caption
    if (is.null(DeltaBGpp$val)){
      char.caption <-
        bquote(
          italic(g)(iso) == .(input$giso)~~~Delta*italic(B)(Gau) ==
            0~~.(input$Bunit)~~
            ~Delta*italic(B)(Lor) == .(DeltaBLpp$val)~~.(input$Bunit)
        )
    }
    if (is.null(DeltaBLpp$val)){
      char.caption <- bquote(
        italic(g)(iso) == .(input$giso)~~~Delta*italic(B)(Gau) ==
          .(DeltaBGpp$val)~~.(input$Bunit)~~
          ~Delta*italic(B)(Lor) == 0~~.(input$Bunit)
      )
    }
    if (!is.null(DeltaBGpp$val) & !is.null(DeltaBLpp$val)){
      char.caption <- bquote(
        italic(g)(iso) == .(input$giso)~~~Delta*italic(B)(Gau) ==
          .(DeltaBGpp$val)~~.(input$Bunit)~~
          ~Delta*italic(B)(Lor) == .(DeltaBLpp$val)~~.(input$Bunit)
      )
    }
    #
    ## spectrum overlay conditions
    overlay <- NULL
    if (isTRUE(input$simOverlay)){
      overlay <- overlay
    } else {
      overlay <- 1.1
    }
    #
    ##  ---- final spectrum comparison ------
    plot.expr.sim <- present_EPR_Sim_Spec(
      data.spectr.expr = expr_data(),
      data.spectr.sim = sim_data(),
      Intensity.shift.ratio = overlay,
      line.color.expr = input$exprColor,
      line.color.sim = input$simColor,
      Blim = input$Brange,
      B.unit = input$Bunit,
      output.df = TRUE
    )$plot +
      ggplot2::labs(
        title = char.title.title,
        caption = char.caption
      ) +
      plot_theme_NoY_ticks(
        legend.text = element_text(size = 13)
      ) +
      theme(
        plot.title = element_text(hjust = 0.5,size = 14)
      )
    return(plot.expr.sim)
  })
  #
  ## experimental and simulated spectrum output
  output$simPlot <- renderPlot({
    expr_sim_plot()
  })
  #
  ## experimental and simulated data frame output
  output$simTab <- DT::renderDT({
    present_EPR_Sim_Spec(
      data.spectr.expr = expr_data(),
      data.spectr.sim = sim_data(),
      Blim = input$Brange,
      B.unit = input$Bunit,
      output.df = TRUE
    )$df
  },
  extensions = "Scroller",
  options = list(
    deferRender = TRUE,
    scrollY = 420,
    scroller = TRUE
    )
  )
  #
  ## -------------- save plot (Sim) -----------------
  output$simPlotsave <- downloadHandler(
    filename = function() {
      paste(input$figfilename,input$figformat,sep = ".")
    },
    content = function(file) {
      if (input$figformat == "png") {
        grDevices::png(
          file,
          width = 7,
          height = 5,
          units = "in",
          res = 300
        )
      } else if (input$figformat == "pdf") {
        grDevices::pdf(file,width = 7,height = 5)
      } else if (input$figformat == "jpeg") {
        grDevices::jpeg(
          file,
          width = 7,
          height = 5,
          units = "in",
          res = 300
        )
      }
      #
      graphics::plot(expr_sim_plot())
      dev.off()
    }
  )
  #
  ## --------- save code for additional fit ----------
  text_sim_code <- reactive({
    #
    ## system of nuclei - text:
    nucs.system.text.vec <-
      mapply(
        function(m,n)
          paste0("list('",n,"',",m,")"),
        nucleiCharacter()[["N_nuclei"]],
        nucleiCharacter()[["nucle_us_i"]]
      )
    #
    ## write list text:
    list.nucs.text <- paste0(
      "list(",paste(nucs.system.text.vec,collapse = ","),")"
    )
    #
    ## write own text:
    R.text.code.for.sim <-
      paste0(
        "# \n",
        "epr.spectrum.data <- \n",
        "  readEPR_Exp_Specs( \n",
        "    '<path to file>', # define the path to EPR spectrum data \n",
        "    col.names = ",
        switch(
          3 - origin.cond(orig = input$origin),
          "c('index','B_G', 'dIepr_over_dB')",
          "c('B_G', 'dIepr_over_dB')",
          "c('B_mT','dIepr_over_dB')"
        ),", \n",
        "    x.unit = ",
        switch(
          3 - origin.cond(orig = input$origin),
          "'G'",
          "'G'",
          "'mT'"
        ),", \n",
        "    qValue = ",input$qValue,", \n",
        "    norm.vec.add = c(",paste(norm_vec$val,collapse = ","),"), \n",
        "    origin = '",input$origin,"' \n",
        "  ) \n",
        "# \n",
        "# arguments of the `eval_sim_EPR_isoFit()` may be varied (refer to documentation) \n",
        "epr.spectrum.sim.fit <- \n",
        "  eval_sim_EPR_isoFit( \n",
        "    data.spectr.expr = epr.spectrum.data, \n",
        "    nu.GHz = ",input$MWnuGHz,", \n",
        "    B.unit = '",input$Bunit,"', \n",
        "    Blim = c(",paste(input$Brange,collapse = ","),"), \n",
        "    lineG.content = ",input$Gcontent,", \n",
        "    optim.method = c('pswarm','levenmarq'), \n",
        "    nuclear.system.noA = ",
        switch(2 - isFALSE(input$splitCond),"NULL, \n",
               paste0(list.nucs.text,", \n")),
        "    baseline.correct = 'constant', \n",
        "    Nmax.evals = 1024, \n",
        "    optim.params.init = c(",input$giso,",",DeltaBGpp$val,
        ",",DeltaBLpp$val,",","0",",",
        round(
          (max(expr_data()[["dIepr_over_dB"]]) /
             max(sim_data()[["dIeprSim_over_dB"]])) * 0.54,
          digits = 7
        ),
        switch(
          2 - isFALSE(input$splitCond),
          "), \n",
          paste0(",",paste(nucleiCharacter()[["A_iso_MHz"]],collapse = ","),"), \n")
        ),
        "    msg.optim.progress = TRUE, \n",
        "    eval.optim.progress = TRUE \n",
        "  ) \n",
        "# \n",
        "# \n",
        "# \n",
        "# \n"
      )
    return(R.text.code.for.sim)
  })
  #
  output$codesave <- downloadHandler(
    filename = function() {
      paste(input$codefilename,input$codeformat,sep = ".")
    },
    content = function(file) {
      if (input$codeformat == "R" || input$codeformat == "txt") {
        writeLines(
          text_sim_code(),
          con = file,
          sep = "" #,
          # useBytes = TRUE
        )
      }
    }
  )
  #
  ## ------------- save table (Sim) -----------------
  output$simTablesave <- downloadHandler(
    filename = function() {
      paste(input$tabfilename,input$tabformat,sep = ".")
    },
    content = function(file) {
      if (input$tabformat == "csv") {
        utils::write.csv(
          present_EPR_Sim_Spec(
            data.spectr.expr = expr_data(),
            data.spectr.sim = sim_data(),
            Blim = input$Brange,
            B.unit = input$Bunit,
            output.df = TRUE
          )$df,
          file = file,
          row.names = FALSE
        )
      } else if (input$tabformat == "xlsx") {
        openxlsx::write.xlsx(
          present_EPR_Sim_Spec(
            data.spectr.expr = expr_data(),
            data.spectr.sim = sim_data(),
            Blim = input$Brange,
            B.unit = input$Bunit,
            output.df = TRUE
          )$df,
          file = file
        )
      }
    }
  )
  #
}
#
