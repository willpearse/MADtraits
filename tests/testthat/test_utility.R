context("utility.r")

test_that(".df.melt", {
    data <- data.frame(weights=1:10, colours=rep(letters[1:5],2), species=rep(letters[1:5],each=2))
    expected <- list(
        numeric=data.frame(
            species = rep(letters[1:5],each=2),
            metadata = NA, variable = "weights", value = 1:10,
            units = as.character(NA), stringsAsFactors=FALSE),
        character=data.frame(
            species = rep(letters[1:5],each=2),
            metadata = NA, variable = "colours", value = rep(letters[1:5],2)
          , units = as.character(NA), stringsAsFactors=FALSE)
    )
    class(expected) <- "natdb"
    expect_equal(natdb:::.df.melt(data, "species"), expected)

    expected$numeric$units <- "a"
    expect_equal(natdb:::.df.melt(data, "species", c("a",NA)), expected)

    expected$character$metadata <- c("a:a;b:stuff", "a:b;b:stuff",
                                     "a:c;b:stuff", "a:d;b:stuff",
                                     "a:e;b:stuff", "a:f;b:stuff",
                                     "a:g;b:stuff", "a:h;b:stuff",
                                     "a:i;b:stuff", "a:j;b:stuff")
    expected$numeric$metadata <- expected$character$metadata
    expect_equal(natdb:::.df.melt(data, "species", c("a",NA), data.frame(a=letters[1:10], b="stuff")), expected)
})

