context("check_geom_Timeline")


test_that("plot geomTimeline", {




  #datafile <- system.file("tests","testthat","GeomDF.rda",package = "capStoneQuake")
  load(file = "GeomDF.rda")
  expect_is(df,"data.frame")

  apl<- df %>% dplyr::arrange(DEATHS )%>% tail() %>%
    ggplot() +
    aes(
      x = date,
      size = EQ_PRIMARY,
      colour = DEATHS
    ) +
    geom_timeline(alpha=0.1)

  expect_is(apl,"ggplot")

})
