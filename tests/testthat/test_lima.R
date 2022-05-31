test_that("Test cp function", {
    q2_ <- 60
    q3_ <- 100
    cPRA1_ <- 50
    cPRA2_ <- 85

    candids <- data.frame(
        ID = c(1, 1, 1, 1, 1, 1),
        bg = c('A', 'A', 'A', 'A', 'A', 'A'),
        A1 = c(1, 1, 1, 1, 1, 1),
        A2 = c(1, 1, 1, 1, 1, 1),
        B1 = c(1, 1, 1, 1, 1, 1),
        B2 = c(1, 1, 1, 1, 1, 1),
        DR1 = c(1, 1, 1, 1, 1, 1),
        DR2 = c(1, 1, 1, 1, 1, 1),
        age = c(1, 1, 1, 1, 1, 1),
        dialysis = c(1, 1, 101, 1, 61, 1),
        cPRA = c(1, 86, 1, 51, 1, 1),
        urgent = c(1, 0, 0, 0, 0, 0)
    )

    results <- factor(
        list(1, 2, 2, 3, 3, 4), 
        levels = 1:4,
        labels = c('Red', 'Orange', 'Yellow', 'Green')
    )

    expect_equal(
        cp(
            data = candids,
            q2 = q2_,
            q3 = q3_,
            cPRA1 = cPRA1_,
            cPRA2 = cPRA2_
        )$cp,
        results
    )
})
