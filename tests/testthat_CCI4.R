if (identical(Sys.getenv("CIRCLECI"), "true"))
{
    Sys.setenv("LC_ALL" = "")
    Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
    Sys.setenv(LANG = Sys.getlocale("LC_CTYPE"))
    test.files <- list.files("tests/testthat", pattern = "\\.R$")
    test.files <- gsub("test-|\\.R$", "", test.files)
    test.filter <- grep("^[asrcd]", test.files,
                        invert = TRUE, value = TRUE)
    test.filter <- paste0("^", test.filter, "$")
    if (!dir.exists("reports"))
        dir.create("reports")
    out.file <- paste0("reports/test_results", Sys.getenv("CIRCLE_NODE_INDEX"), ".xml")
    file.filter <- paste0(test.filter, collapse = "|")
    exit.code <- flipDevTools::RunTestsOnCircleCI(path = ".", filter = file.filter, output_file = out.file)
    q(status = exit.code, save = "no")
}
