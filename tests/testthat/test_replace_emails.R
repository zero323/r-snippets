
testthat::expect_equal(
    replace_emails("This is an email from root@localhost"),
    "This is an email from __EMAIL__"
)