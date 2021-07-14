## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)

mkdir("report")
mkdir("report/figs")


# Rmarkdown reports ------------------------------------------------

library(rmarkdown)

rm.list <- ls(all.names=TRUE)
rm(list=rm.list)

rmarkdown::render(
  input="report.Rmd",
  output_file = "report/report.docx"
)


# rmarkdown::render(
#   input="report_01_advice.Rmd",
#   output_file = "report/report_01_advice.docx"
# )

# rm.list <- ls(all.names=TRUE)
# rm(list=rm.list)
# 
# rmarkdown::render(
#   input="utilities_advice_NON-FIDES.Rmd",
#   output_file = "report/advice_NON-FIDES.docx"
# )

# rm.list <- ls(all.names=TRUE)
# rm(list=rm.list)
# 
# # Extra figs for report
# rmarkdown::render(
#   input="report_02_report.Rmd",
#   output_file = "report/report_02_report.docx"
# )

