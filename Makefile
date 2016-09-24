all: clean install

PKG_VERS = $(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME = $(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

.PHONY: all install build check clean vignettes

install: $(PKG_NAME)_$(PKG_VERS).tar.gz
	R CMD INSTALL --no-build-vignettes $<

build: $(PKG_NAME)_$(PKG_VERS).tar.gz

test: $(PKG_NAME)_$(PKG_VERS).tar.gz
	R CMD check --as-cran $<

clean:
	-rm $(PKG_NAME)_*.tar.gz
	-rm -rf $(PKG_NAME).Rcheck

$(PKG_NAME)_$(PKG_VERS).tar.gz: DESCRIPTION
	# Rscript -e "Rcpp::compileAttributes()"
	R CMD build --no-build-vignettes ../$(PKG_NAME)

vignettes: inst/doc/stan_usage.pdf

inst/doc/stan_usage.pdf: vignettes/stan_usage.Rnw
	Rscript -e "knitr::knit(\"vignettes/stan_usage.Rnw\", output = \"inst/doc/stan_usage.tex\")"
	texi2pdf inst/doc/stan_usage.tex
	mv stan_usage.pdf $@
	- rm inst/doc/*.log
	- rm inst/doc/*.tex
	- rm inst/doc/*.aux}
