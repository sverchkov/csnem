test_that("R matrix replication works", {

  small_matrix <- matrix(1:12, 4, 3)
  colnames(small_matrix) = letters[1:3]

  big_matrix <- matrix(rep(1:12,5), 4, 15)
  colnames(big_matrix) <- paste(letters[1:3], rep(1:5, each=3), sep="_")

  expect_equal(makeBigRMatrix(small_matrix, 5), big_matrix)
})
