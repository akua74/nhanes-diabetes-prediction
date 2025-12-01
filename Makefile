# ============================================================
# Makefile for NHANES Diabetes Prediction Project
# Build the analysis report from R Markdown
#
# Usage (inside the Docker container, in project root):
#   make report.pdf
#   make report        # same as above
#   make clean
# ============================================================

RMD         = report/report.Rmd
REPORT_PDF  = report/report.pdf
ANALYSIS_R  = code/analysis.R

# Default target: build the PDF report
all: $(REPORT_PDF)

# Build the PDF report from the Rmd + analysis code
$(REPORT_PDF): $(RMD) $(ANALYSIS_R)
	Rscript -e "rmarkdown::render('$(RMD)', output_file = 'report.pdf', output_dir = 'report')"

# Convenience alias: `make report` also builds the PDF
report: $(REPORT_PDF)

# Remove built artifacts
.PHONY: clean
clean:
	rm -f report/*.pdf report/*.html report/*.log report/*.aux report/*.out report/*.toc report/*.tex
