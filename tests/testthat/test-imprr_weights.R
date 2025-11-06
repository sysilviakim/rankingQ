test_that("imprr_weights returns expected structure", {
  # This test uses the identity dataset
  identity <- rankingQ::identity
  
  result <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    seed = 123
  )
  
  # Should return a list
  expect_type(result, "list")
  
  # Should have expected components
  expect_true("rankings" %in% names(result))
})

test_that("imprr_weights infers J from data when not specified", {
  identity <- rankingQ::identity
  
  result <- imprr_weights(
    identity,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    seed = 123
  )
  
  expect_type(result, "list")
})

test_that("imprr_weights accepts custom seed", {
  identity <- rankingQ::identity
  
  result1 <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    seed = 123
  )
  
  result2 <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    seed = 123
  )
  
  # Results should be identical with same seed
  expect_equal(result1$rankings, result2$rankings)
})

test_that("imprr_weights with different population settings", {
  identity <- rankingQ::identity
  
  result_non_random <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    population = "non-random",
    seed = 123
  )
  
  result_all <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    population = "all",
    assumption = "uniform",
    seed = 123
  )
  
  # Both should return valid results
  expect_type(result_non_random, "list")
  expect_type(result_all, "list")
})

test_that("imprr_weights with custom ranking column name", {
  identity <- rankingQ::identity
  
  result <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    ranking = "custom_ranking",
    seed = 123
  )
  
  expect_type(result, "list")
})

test_that("imprr_weights handles survey weights", {
  identity <- rankingQ::identity
  
  result <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    weight = identity$s_weight,
    seed = 123
  )
  
  expect_type(result, "list")
  expect_true("rankings" %in% names(result))
})

test_that("imprr_weights produces rankings data frame", {
  identity <- rankingQ::identity
  
  result <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    seed = 123
  )
  
  # rankings should be a data frame
  expect_s3_class(result$rankings, "data.frame")
  
  # Should have ranking column
  expect_true("ranking" %in% names(result$rankings))
})

test_that("imprr_weights with contaminated assumption", {
  identity <- rankingQ::identity
  
  result <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    population = "all",
    assumption = "contaminated",
    seed = 123
  )
  
  expect_type(result, "list")
})

test_that("imprr_weights produces weights in output", {
  identity <- rankingQ::identity
  
  result <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    seed = 123
  )
  
  # rankings data frame should have weights column
  expect_true("weights" %in% names(result$rankings))
})
