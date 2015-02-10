
expect_equal(
    ngram_tokenizer(1)(""),
    character(0)
)

expect_equal(
    ngram_tokenizer(1)("I'm an aspiring data scientist"),
    c( "I'm", "an", "aspiring", "data", "scientist")
)

expect_equal(
    ngram_tokenizer(2)("I'm an aspiring data scientist"),
    c("I'm an", "an aspiring", "aspiring data", "data scientist")
)

expect_equal(
    ngram_tokenizer(3)("I'm an aspiring data scientist"),
    c("I'm an aspiring", "an aspiring data", "aspiring data scientist")
)

expect_equal(
    ngram_tokenizer(5)("I'm an aspiring data scientist"),
    c("I'm an aspiring data scientist")
)

expect_equal(
    ngram_tokenizer(6)("I'm an aspiring data scientist"),
    character(0)
)
