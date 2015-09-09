#test

library(stringr)

test_that("Correct output for simple input",
        {expect_equal(euclidian(100,10),10)
                expect_equal(euclidian(100,-10),10)}
)
test_that("Function will return error if needed",
          {expect_error(euclidian("a",10))
                  expect_error(euclidian(10,"a"))
                  expect_error(euclidian(c(1,2),10))
                  expect_error(euclidian(1.1,10))
                  }
          )

