context("clean_data")

test_that("clean data works", {

  LOCATION_NAME<- c("ITALY: SICILY","ITALY:  VENICE")
  COUNTRY <- c("ITALY","ITALY")
  DAY <- as.integer(c(3,6))
  MONTH <-as.integer(c(1,11))
  YEAR <- as.integer(c(2005,2012))
  LATITUDE <- c("43.5","45.8")
  LONGITUDE <- c("11.33","11.05")



  df = data.frame(DAY,MONTH,YEAR,COUNTRY,LOCATION_NAME,LATITUDE,LONGITUDE,stringsAsFactors=FALSE)

  df<-eq_clean_data(df)

  expect_is(df[1,]$date,"Date")
  expect_is(df[1,]$LATITUDE,"numeric")
  expect_is(df[1,]$LONGITUDE,"numeric")
  expect_equal(format(df[1,]$date,"%d %m %Y"),"03 01 2005")
  expect_equal(format(df[2,]$date,"%d %m %Y"),"06 11 2012")


})
