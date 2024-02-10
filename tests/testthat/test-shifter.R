test_that("shifter returns valid inputs",  {
    # equivalence
    expect_equal(shifter(c(1:1:13), -1),  c(13, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    # positive non-integers
    expect_error(shifter(c:1:1:3, 1.1))
    expect_error(shifter(c:1:1:5, 0.1))
    # negative non-integers
    expect_error(shifter(c:1:1:4, -1.1))
    expect_error(shifter(c:1:1:5, -0.1))
    # not a number
    expect_error(shifter(c:1:1:5, "string"))
    # special case: vector of length 1
    expect_equal(shifter(1, 0), 1)
    # special case: vector of length 1 - invalid inputs
    # missing arg
    expect_error(shifter())
    expect_error(shifter(c:1:1:5))
    expect_error(shifter(, "string"))
})
