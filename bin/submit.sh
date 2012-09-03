# provide package.tar.gz as argument
echo ""
echo "file to be submitted: $1"
echo ""
echo "ftp  -u ftp://cran.r-project.org/incoming/$1 $1"
ftp  -u ftp://cran.r-project.org/incoming/$1 $1

echo ""
echo "A submission should be accompanied by an email to CRAN@R-project.org,"
echo "if possible sent from the maintainer address listed in the package,"
echo "using a subject line of the form"
echo ""
echo "    `CRAN submission PACKAGE VERSION'."
echo ""

