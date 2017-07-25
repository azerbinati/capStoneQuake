context("titolize")

test_that("single titolization works", {
  myword <- "SUPERCALY other than THIS! 33."
  target <- "Supercaly Other Than This! 33."
  titolized <- titolize(myword)

  expect_equal(titolized,target)
})

test_that("vectorized titolization works", {
  myword <- c("SUPERCALY other than THIS! 33.","OTHER","than","tHiSS!!","WILL WOrk")
  target <- c("Supercaly Other Than This! 33.","Other","Than","Thiss!!","Will Work")
  titolized <- titolize(myword)

  expect_equal(titolized,target)
})
