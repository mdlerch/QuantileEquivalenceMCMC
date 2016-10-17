all: clean install

PKG_VERS = $(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME = $(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

.PHONY: all install build check clean vignettes

install: $(PKG_NAME)_$(PKG_VERS).tar.gz
	R CMD INSTALL $<

build: $(PKG_NAME)_$(PKG_VERS).tar.gz

test: $(PKG_NAME)_$(PKG_VERS).tar.gz
	R CMD check --as-cran $<

clean:
	-rm $(PKG_NAME)_*.tar.gz
	-rm -rf $(PKG_NAME).Rcheck

$(PKG_NAME)_$(PKG_VERS).tar.gz: DESCRIPTION
	# Rscript -e "Rcpp::compileAttributes()"
	R CMD build ../$(PKG_NAME)

vignettes: inst/doc/stan_usage.pdf inst/doc/jags_usage.pdf inst/doc/coda_usage.pdf

inst/doc/stan_usage.pdf: vignettes/stan_usage.Rnw
	Rscript -e "knitr::knit(\"vignettes/stan_usage.Rnw\", output = \"inst/doc/stan_usage.tex\")"
	texi2pdf inst/doc/stan_usage.tex
	mv stan_usage.pdf $@
	- rm inst/doc/*.log
	- rm inst/doc/*.tex
	- rm inst/doc/*.aux}

inst/doc/jags_usage.pdf: vignettes/jags_usage.Rnw
	Rscript -e "knitr::knit(\"vignettes/jags_usage.Rnw\", output = \"inst/doc/jags_usage.tex\")"
	texi2pdf inst/doc/jags_usage.tex
	mv jags_usage.pdf $@
	- rm inst/doc/*.log
	- rm inst/doc/*.tex

inst/doc/coda_usage.pdf: vignettes/coda_usage.Rnw
	Rscript -e "knitr::knit(\"vignettes/coda_usage.Rnw\", output = \"inst/doc/coda_usage.tex\")"
	texi2pdf inst/doc/coda_usage.tex
	mv coda_usage.pdf $@
	- rm inst/doc/*.log
	- rm inst/doc/*.tex
	- rm inst/doc/*.aux}
	- rm inst/doc/*.aux}
