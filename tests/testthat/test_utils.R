context("Testing utils")
library(bsibayes)

test_that("recode_id of factor is correct", {
  expect_equal(recode_id(factor(c('a','a','b','c'))), c('a'=1,'a'=1,'b'=2,'c'=3))
})

test_that("recode_id of character vector is correct", {
  expect_equal(recode_id(c('a', 'b', 'c', 'a')), c('a'=1, 'b'=2, 'c'=3, 'a'=1))
})

test_that("recode_id of integer vector is correct", {
  expect_equal(recode_id(c(2,5,2,6,6)), c('2'=1, '5'=2, '2'=1, '6'=3, '6'=3))
})


tab_counts <- tibble::tibble(labcode = c(1,1,2,2),
                  year = c(1,2,1,2),
                  total_bsi = c(4,5,6,7),
                  sp1 = c(4,5,6,7),
                  sp2 = c(10,11,12,13))

exp_count <- tibble::tibble(ix = c(1:4, 1:4),
                        species = c(rep('sp1', 4), rep('sp2', 4)),
                        counts = c(4,5,6,7,10,11,12,13))

test_that("gather_counts is working", {
  expect_equal(gather_counts(tab_counts), exp_count)
})
