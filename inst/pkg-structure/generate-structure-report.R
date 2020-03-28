

library(pkgnet)

## Declare paths explicitly as currently required by pkgnet
pkg_path <- system.file(package = "EpiSoon")
report_path <- file.path("inst/pkg-structure", "EpiSoon_report.html")

## Generate pkg report
report <- CreatePackageReport("EpiSoon",
                              report_path = report_path)
