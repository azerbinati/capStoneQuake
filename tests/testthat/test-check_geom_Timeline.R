context("check_geom_Timeline")


test_that("multiplication works", {
  load("~/capStoneQuake/tests/testthat/sysdata.rda")
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
