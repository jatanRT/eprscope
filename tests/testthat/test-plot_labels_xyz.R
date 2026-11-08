#
## ======================= Testing labels for plots ==========================
#
## ---------------------- 'Xenon' integrals --------------------------
## ----------------- Triarylamine radical cation ----------------------
#
test_that("X,Y labels for EPR integral plot do match. ",{
  #
  triaryl_radCat_path <-
    load_data_example(file = "Triarylamine_radCat_decay_a.txt")
  ## corresponding data (double integrated EPR
  ## spectrum = `Area` vs `time`)
  triaryl_radCat_data <-
    readEPR_Exp_Specs(
      triaryl_radCat_path,
      header = TRUE,
      fill = TRUE,
      select = c(3,7),
      col.names = c("time_s","Area"),
      x.unit = "s",
      x.id = 1,
      Intensity.id = 2,
      qValue = 1700,
      data.structure = "others"
    ) %>%
    na.omit()
  #
  ## plot integrals (use `plot_labels_xyz()`)
  labels.plot <-
    ggplot2::ggplot(data = triaryl_radCat_data) +
    ggplot2::geom_point(
      ggplot2::aes(x = time_s,y = Area)
    ) +
    ggplot2::labs(
      title = "Radical Kinetics",
      x = plot_labels_xyz(Time,s),
      y = plot_labels_xyz(Double~~Integral,p.d.u.)
    ) +
    plot_theme_NoY_ticks()
  #
  x.lab <- bquote(italic(~Time) ~ ~"(" ~ ~s ~ ")") %>% as.character()
  y.lab <- bquote(italic(~(Double ~ ~Integral)) ~ ~"(" ~ ~p.d.u. ~ ")") %>%
    as.character()
  #
  expect_equal(as.character(labels.plot@labels$x),x.lab)
  expect_equal(as.character(labels.plot@labels$y),y.lab)
  expect_equal(
    plot_labels_xyz(Double ~ ~ Integral, p.d.u.) %>% as.character(),
    bquote(italic(~(Double ~ ~Integral)) ~ ~"(" ~ ~p.d.u. ~ ")") %>%
      as.character()
  )
  #
})
#
## --------------- Testing labels for Voltammogram -----------------
#
test_that("X,Y labels for voltammogram do match .",{
  #
  triarylamine.path.cv <-
    load_data_example(file = "Triarylamine_ECh_CV_ivium.txt")
  #
  ## load voltammogram data
  triarylamine.data.cv <-
    data.table::fread(
      file = triarylamine.path.cv,
      skip = 2,
      col.names = c(
        "time_s",
        "E_V_des", ## desired potential
        "I_A",
        "E_V_app" ## applied potential
      )
    )
  #
  ## reference electrode
  reference <- "Ag-quasiref."
  #
  ## plot voltammogram
  plot.volta <-
    plot_ECh_VoC_amperogram(
      data.vat = triarylamine.data.cv,
      x = "E_V_app",
      Current = "I_A",
      Current.unit = "A",
      ref.electrode = reference
    )
  #
  x.lab <- bquote(italic(E) ~ ~"(" ~ "V" ~ ")" ~ ~~italic(vs) ~ ~~.(reference)) %>%
    as.character()
  y.lab <- bquote(italic(Current) ~ ~"(" ~ A ~ ")") %>%
    as.character()
  #
  expect_equal(as.character(plot.volta@labels$x),x.lab)
  expect_equal(as.character(plot.volta@labels$y),y.lab)
})
