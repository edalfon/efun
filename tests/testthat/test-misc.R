test_that("timestamp_it works", {

  expect_error(regexp = NA, object = {
    timestamp_it("asdf")
    timestamp_it("asdf/")
    timestamp_it("C:/sdfkjj/asdf/")
    timestamp_it("asdf.txt")
    timestamp_it("DESCRIPTION")
    timestamp_it("DESCRIPTION/")
    timestamp_it("DESCRIPTION/", FALSE)
    timestamp_it(mtcars, FALSE)
  })

  expect_error(object = {
    timestamp_it(mtcars, TRUE)
  })

})
