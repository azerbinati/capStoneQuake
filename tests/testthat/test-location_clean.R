context("location_clean")

test_that("location_clean works", {
  LOCATION_NAME<- c("ITALY: SICILY","ITALY:  VENICE")
  COUNTRY <- c("ITALY","ITALY")
  df = data.frame(COUNTRY,LOCATION_NAME,stringsAsFactors=FALSE)

  df<-eq_location_clean(df)

  expect_equal(df[1,]$LOCATION_NAME,"Sicily")
  expect_equal(df[2,]$LOCATION_NAME,"Venice")
})
