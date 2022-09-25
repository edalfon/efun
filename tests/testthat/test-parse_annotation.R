test_that("parse_annotation works", {

  expect_snapshot_value(
    parse_single_annotation(
      "#1 ##1.1 ##1.2 ##1.3 ###1.3.1 ###1.3.2 ##1.4 ##1.5 #2 ##2.1 ###2.1.1",
      pattern = "#+"
    ),
    style = "serialize"
  )

  # testing the forgivingness with the first separator
  expect_equal(
    parse_single_annotation("a ##b ###c", "#+"),
    parse_single_annotation("#a ##b ###c", "#+")
  )

  # testing the forgivingness with extra-separators
  expect_equal(
    parse_single_annotation("#a ###b ######c ##x ###y ####z", "#+"),
    parse_single_annotation("#a ##b ###c #x ##y ###z")
  )

  expect_snapshot_value(

    tibble::tibble(mynotes = c("a ##b ###c", "#+", "as", "#1 ##11 ###33#5")) |>
      mutate(x = parse_annotations(mynotes)) |>
      tidyr::unnest(x)
    ,
    style = "serialize"
  )

})
