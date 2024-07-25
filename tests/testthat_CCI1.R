if (identical(Sys.getenv("CIRCLECI"), "true") && require(devtools, quietly = TRUE))
{
    load_all(".")
    Sys.setenv("LC_ALL" = "")
    Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
    Sys.setenv(LANG = Sys.getlocale("LC_CTYPE"))
    if (!dir.exists("reports"))
        dir.create("reports")
    out.file <- paste0("reports/test_results", Sys.getenv("CIRCLE_NODE_INDEX"), ".xml")
    exit.code <- flipDevTools::RunTestsOnCircleCI(filter = "^[as]",
                                                  load_package = "none", output_file = out.file)
    q(status = exit.code, save = "no")
}
