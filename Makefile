# h/t to @jimhester and @yihui for this parse block:
# https://github.com/yihui/knitr/blob/dc5ead7bcfc0ebd2789fe99c527c7d91afb3de4a/Makefile#L1-L4
# Note the portability change as suggested in the manual:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-portable-packages
PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGTARBALL = $(PKGNAME)_$(PKGVERS).tar.gz
DATETIME = `date --rfc-3339=seconds`

all: check

build:
	R CMD build .

check: build
	R CMD check --no-manual $(PKGNAME)_$(PKGVERS).tar.gz

install_deps:
	Rscript \
	-e 'if (!requireNamespace("remotes")) install.packages("remotes")' \
	-e 'remotes::install_deps(dependencies = TRUE)'

install: install_deps build
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

# this happens inside docker
.ONESHELL:
drat_insert:
	PKGREPO=$PWD
	cd /drat
	git config user.name "Sykdomspulsen"
	git config user.email "sykdomspulsen@fhi.no"
	git config push.default simple
	git checkout gh-pages
	git pull

	Rscript -e "drat::insertPackage('$PKGREPO/PKGTARBALL', repodir = '.')"
	sed -i "/## News/a $DATETIME Inserted $PKGNAME $PKGVERS" README.md
	git add -A
	git commit -am "Jenkins $PKGNAME $PKGVERS" #Committing the changes

	cd $PKGREPO

# this happens outside of docker
drat_push:
	git -C /mnt/n/sykdomspulsen_config/drat push -f origin gh-pages #pushes to master branch

.ONESHELL:
drat_prune_history:
	cd /tmp
	git clone "git@github.com:folkehelseinstituttet/drat.git"
	cd drat
	git config user.name "Sykdomspulsen"
	git config user.email "sykdomspulsen@fhi.no"
	git config push.default simple
	git checkout gh-pages

	git checkout --orphan latest_branch
	git add -A
	git commit -am "Cleaning history" #Committing the changes
	git branch -D gh-pages #Deleting master branch
	git branch -m gh-pages #renaming branch as master
	git -C /tmp/drat push -f origin gh-pages #pushes to master branch

clean:
	@rm -rf $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck
