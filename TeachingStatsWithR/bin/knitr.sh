#!/bin/bash
###------------------------------------------------------------------------
### What: Run Sweave and post process with LaTeX and friends directly from
###       command line
### Time-stamp: <2008-11-26 11:11:55 ggorjan>
### $Id: Sweave.sh 318 2008-04-27 10:50:27Z ggorjan $
###------------------------------------------------------------------------

### {{{ TODO
###------------------------------------------------------------------------
###
### o use R CMD Sweave/Stangle
###   - not until options can be passed to that script!
###
### o possibility to specify R options, such as --vanilla
###   --> already possible via --optionsr
###
### o use the trick by Andrew Robinson for latex rerun ...
###
### }}}
### {{{ SCRIPT DOCS
###------------------------------------------------------------------------
###
### In short this script runs Sweave on a given Rnw file(s) and post-processes
### the produced tex file(s) with LaTeX and friends directly from the command
### line. It has a rich set of command line options to ease the whole process.
###
### This scripts depends on:
###   - R <http://www.r-project.com>
###
### It can be enhanced with:
###  - standard LaTeX tools
###  - texi2dvi <http://www.gnu.org/software/texinfo/texinfo.html>
###  - LyX <http://www.lyx.org>
###
### Initial idea for the script was taken from:
### <http://www.ci.tuwien.ac.at/~leisch/Sweave/FAQ.html#x1-5000A.3>
###
### There was quite some talk on the R-help mailing list about the issue of
### automagically processing the Sweave source files - follow the link bellow).
### There are various ways to achive this task. I wrote this script in a way 
### that covers most of my needs. Actually, I tried to cover "all possible" or 
### at least most common LaTeX compiling paths. Do not hesitate to contact me or
### provide a patch, if you find that there is a need for the improvement.
###
### <https://stat.ethz.ch/pipermail/r-help/2005-February/065047.html>
###
### }}}
### {{{ USE WITH CYGWIN
###------------------------------------------------------------------------
###
### Use under windows MS (via Cygwin):
### This tool can be used on UNIX alike OSes as well as on Windows under
### Cygwin. Cygwin can lack some TeX stuff and if you have a Windows
### installation of TeX you can tweak the system to use both of them. Try
### first to compile and then decide if you need to merge TeX paths of Cygwin
### and 'Windows'. On the other hand, you can of course always add TeX
### packages to Cygwin's TeX. Here is the list how to set the whole thing up
### and merge both TeX paths:
###
### - install R
###
### - install "TeX" distribution, such as MikTeX and follow instructions at
###   http://www.murdoch-sutherland.com/Rtools/ for fusing MikTeX and R
###
### - install Cygwin with tetex package
###
### - launch Cygwin terminal i.e. one bash shell session and do
###   vi /usr/share/texmf/web2c/texmf.cnf
###
### - find the lines bellow, where * means some directory place
###   TEXMFLOCAL = *
###   TEXMFHOME = *
###
### - change those * to something like (where your Windows TeX installation is)
###   TEXMFLOCAL = /cygdrive/c/Programs/tex/texmf
###   TEXMFHOME = /cygdrive/c/Programs/tex/localtexmf
###
### - find the lines where TEXMFLOCAL and TEXMFHOME appear and put them
###   behind TEXMFMAIN, so that under Cywgin first Cygwin TeX stuff will be
###   used, but in case anything is missing it will take a look in Windows
###   TeX installation i.e. MikTeX in this case.
###   TEXMF = {!!$TEXMFCONFIG,!!$TEXMFVAR!!$TEXMFSYSCONFIG,!!$TEXMFSYSVAR,!!$TEXMFMAIN,!!$TEXMFLOCAL,!!$TEXMFHOME,!!$TEXMFDIST}
###   SYSTEXMF = $TEXMFMAIN;$TEXMFLOCAL;$TEXMFHOME;$TEXMFDIST
###
### I am not really familiar with TEXMF variable syntax so this might be done
### in some other better/nicer way. You can definitely show me how ;)
###
### }}}
### {{{ COPYRIGHT
###------------------------------------------------------------------------
###
### Sweave.sh
### Copyright (C) 2005-2009 Gregor Gorjanc
###
### This code is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by the
### Free Software Foundation; either version 2 of the License, or (at your
### option) any later version.
###
### This code is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License along
### with this code; if not, write to the Free Software Foundation, Inc., 51
### Franklin St, Fifth Floor, Boston, MA 02110-1301 USA. You can also
### download it from http://www.gnu.org/copyleft/gpl.html in HTML and from
### http://www.gnu.org/copyleft/gpl.txt in plain text format.
###
### }}}
### {{{ SCRIPT
###------------------------------------------------------------------------

### {{{ --- Defaults ---

CLEAN=yes
RMTEXFILE=yes
FORCE=no
MODE=no

# Defaults for R
R=R
OPTR='--no-save --no-restore'
WEAVE=yes
TANGLE=no
CACHE=no
CACHEDIR=''

# Defaults for LaTeX and friends
OPTTEXI2DVI='--clean --quiet'
OPTDVIPS=''
#OPTPS2PDF='-dCompatibility=1.3'
PS2PDFAPP=ps2pdf13
OPTLATEX=''
OPTPDFLATEX=''
OPTBIBTEX=''
OPTMAKEINDEX=''

# Defaults for viewers
PDFAPPIN=acroread
PSAPPIN=gv
DVIAPPIN=xdvi

### }}}

# Nothing to configure below!
#--------------------------------------------------------------------------

### {{{ --- Usage ---

NAME=$(basename $0)

usage () {
    cat <<EOF
NAME
    $NAME - Run Sweave and postprocess with LaTeX directly from the command line

SYNOPSIS
    $NAME [OPTION] ...

DESCRIPTION
    Run Sweave directly from the command line and optionally postprocess with
    LaTeX and other related tools. Following options can be used:
    
    -c, --cache
    -c=, --cache=
      Use caching as implemented in the cacheSweave R package. You need to 
      install the cacheSweave packge in R before this option can be used. See
      http://cran.r-project.org/package=cacheSweave for the details of the 
      caching process. The other form of the option, i.e., --cache= can be used
      to seet the caching directory via setCacheDir() function in R. Note that 
      this can be overriden by a call to setCacheDir() within the Sweave file!

    -f, --force
      Force the continuation of the script when an error is issued. Default is 
      to stop when an error occurs. Errors are checked by the means of $? 
      environmental variable, i.e., when the value of this variable differs from
      zero. Warnings are always printed. Tests for errors are performed after the
      run of R and after the runs of texi2dvi, latex, dvips and ps2pdf. Exit 
      status of the bibtex and makeindex in a hardcoded set is not tested.

    -h, --help
      Print this output.

    -m, --mode
      By default all input files are first processed with Sweave and then
      with LaTeX. With this option each file is first processed with Sweave
      and directly after that with LaTeX. There is no difference for only one 
      input file.

    -nc, --noclean
      Do not remove intermediate files. These are PDF and EPS pictures and
      LaTeX file from R run of Sweave and DVI file and other LaTeX temporary
      files from LaTeX run. It is default to remove intermediate files for
      all post-processing options, i.e., the -t*, -l*, -ot* and -ol*, but not 
      if only Sweave mode is used.

    -nw, --noweave
      Do not weave, i.e, do not run Sweave function on a given file(s). Note
      that with this option old 'LaTeX' file might be processed if 'LaTeX'
      options are used. This might be usefull with the -t option.

    -nq, --noquiet
      Work quietly as possible, which is the default. Works only with options
      -t* and -ot* due to the availability of the --quiet option in 'texi2dvi'.

    -R=, --R-bin=
      Use given R binary, e.g., R-devel.

    -t, --tangle
      Tangle, i.e., extract R code from a given file(s). If this option is set,
      produced R file is not removed in the cleaning part, otherwise it is.

    There are the following ways of processing the LaTeX file:

      Command and path | Script option          | Used tools
       - texi2dvi
          - PS          -tp,  --texi2dvi2ps      texi2dvi and dvips
          - PS to PDF   -tld, --texi2dvi2ps2pdf  texi2dvi, dvips and ps2pdf
          - PDF         -td,  --texi2dvi2pdf     texi2dvi with pdf option

       - latex
          - PS          -lp,  --latex2dvi2ps     hardcoded set of 'latex and friends' and dvips
          - PS to PDF   -lld, --latex2dvi2ps2pdf hardcoded set of 'latex and friends', dvips and ps2pdf

       - pdflatex
          - PDF         -ld,  --latex2pdf        hardcoded set of 'pdflatex and friends'

    Script can open the produced file(s) (PDF or Postscript) via the use of 
    options bellow. These options are "one step further" of the options above and 
    always imply them:

    -otp=gv,        --opentexi2dvi2ps=gv
    -otld=acroread, --opentexi2dvi2ps2pdf=acroread
    -otd=acroread,  --opentexi2dvi2pdf=acroread
    -olp=gv,        --openlatex2dvi2ps=gv
    -olld=acroread, --openlatex2dvi2ps2pdf=acroread
    -old=acroread,  --openlatex2pdf=acroread
      Files are opened with a defined application. Defaults are values taken
      from environmental variables PDFAPP and PSAPP. If these variables are not
      defined, the default is 'acroread' for PDF and 'gv' for Postscript. Quotes
      are necessary in the case of "bad" filenames.

    -optr=, --optionsr=
    -optt=, --optionstexi2dvi=
    -optp=, --optionsdvips=
    -optf=, --optionsps2pdf=
    -optl=, --optionslatex=
    -optd=, --optionspdflatex=
    -optb=, --optionsbibtex=
    -optm=, --optionsmakeindex=
      Any additional options that should be passed to R or LaTeX and other tools.
      Multiple options for one tool must be given within the quotes (single or 
      double), e.g., -optp="-Ppdf -GO".

DETAILS
    R is by default started with the following options:
     - do not save the workspace, i.e., --no-save
     - do not restore anything, i.e., --no-restore

    Hardcoded set of 'latex/pdflatex and friends' for the options -lp and -lld
    means that a hardoced set of LaTeX and other tools are used. There may 
    appear some errors on screen if there is no need for bibtex and/or makeindex, 
    since these two are also on the list.

       The list/order of hardcoded commands is:
        latex
        bibtex
        makeindex
        latex
        latex

    Options can not be combined like -cotld, but should be given separatelly 
    like -c -otld. Look in the examples.

    This script has also the capability to compile LyX files. Specified LyX
    files are exported to LaTeX via the LyX export function, then moved to a 
    file with the Rnw extension and compiled as usual. The LyX file type is 
    determined via the file extension .lyx. Note that this approach assumes 
    that the LyX file has S code chunks inserted within the TeX insets. See 
    the following article, if you are interested in the direct use of R (Sweave) 
    with LyX:

    Gorjanc, G. (2008) Using Sweave with LyX: How to lower the LaTeX\Sweave
    learning curve. Rnews, 8(1):2-9.
    http://www.r-project.org/doc/Rnews/Rnews_2008-1.pdf
      
    Additionally, if you only have a LaTeX file, you can compile it with this 
    script. In that case the source LaTeX file is of course not removed!

    Only files with the following extensions (and their uppercase equivalents)
    are accepted, otherwise script exits: .Rnw, .Snw, .nw, .lyx, and .tex.

EXAMPLE
    Default usage - run Sweave on the file
    $NAME Sweave-test-1.Rnw

    Use  another version of R
    $NAME Sweave-test-1.Rnw -R=R-devel

    Create postscript via the texi2dvi tool and do not clean temporary files
    $NAME -tp -nc Sweave-test-1.Rnw

    Create PDF via the texi2dvi (latex) tool and open a produced file
    $NAME -otld Sweave_prosper_slides.Rnw

    Create PDF via the texi2dvi (latex) tool and specify some special options
    $NAME -tld -optp="-Ppdf -GO" -optf=-dPDFsettings=/prepress \\
          Sweave_prosper_slides.Rnw

    Create PDF via the texi2dvi (latex) tool and open a produced file with a "non-standard" viewer
    $NAME -otld=acrobat Sweave_prosper_slides.Rnw

    Create PDF via the texi2dvi (pdflatex) tool and open a produced file
    $NAME -otd Sweave_beamer_slides.Rnw

    Cache intermediate calculations in .cache directory
    $NAME -c=.cache Sweave_long_computation.Rnw
    
    Create PDF via the pdflatex tool from a LyX file
    $NAME -ld Sweave-test-1.lyx

    Create PDF from LaTeX file via the pdflatex tool - no weaving
    $NAME -nw -ld Sweave-test-1.tex

AUTHOR
    Gregor GORJANC <gregor.gorjanc at bfro.uni-lj.si>

EOF
}

### }}}
### {{{ --- Options ---

OPTIONS=$@
for OPTION in "$@"; do
    case "$OPTION" in
        -c* | --cache*)
            CACHE=yes
            CACHEDIRTEST=$(echo $OPTION | sed -e "s/--cache=//"\
                                              -e "s/--cache//"\
                                              -e "s/-c=//"\
                                              -e "s/-c//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - cache mode via cacheSweave R package\n"
            if [ -n "$CACHEDIRTEST" ]; then
                CACHEDIR=$CACHEDIRTEST
                OPTIONS_ECHO="$OPTIONS_ECHO - caching folder $CACHEDIR\n"
            fi

            ;;
        -f | --force )
            FORCE=yes
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - force mode\n"
            ;;
        -h | --help )
            usage
            exit 0
            ;;
        -m | --mode )
            MODE=yes
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - direct mode\n"
            ;;
        -nc | --noclean )
            CLEAN=no
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - noclean mode\n"
            ;;
        -nq | --noquiet )
            QUIET=no
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - noquiet mode\n"
            ;;
        -nw | --noweave )
            WEAVE=no
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - do not weave\n"
            ;;
        -R=* | --R-bin=*)
            R=$(echo $OPTION | sed -e "s/--R-bin=//"\
                                   -e "s/-R=//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - use R binary: $R\n"
            ;;
        -t | --tangle )
            TANGLE=yes
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - tangle\n"
            ;;
        -tp | --texi2dvi2ps)
            TEX=yes
            TEXI2DVI=yes
            DVIPS=yes
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - create PS via texi2dvi\n"
            ;;
        -otp* | --opentexi2dvi2ps*)
            TEX=yes
            TEXI2DVI=yes
            DVIPS=yes
            PSOPEN=yes
            PSAPP1=$(echo $OPTION | sed -e "s/--opentexi2dvips=//"\
                                        -e "s/--opentexi2dvips//"\
                                        -e "s/-otp=//"\
                                        -e "s/-otp//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            if [ ! -n "$PSAPP1" -a ! -n "$PSAPP" ]; then
                PSAPP1=$PSAPPIN
            elif [ -n "$PSAPP" ]; then
                PSAPP1=$PSAPP
            fi
            OPTIONS_ECHO="$OPTIONS_ECHO - create PS via texi2dvi and open it with $PSAPP1\n"
            ;;
        -tld | --texi2dvi2ps2pdf)
            TEX=yes
            TEXI2DVI=yes
            DVIPS=yes
            PS2PDF=yes
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - create PS and PDF via texi2dvi\n"
            ;;
        -otld* | --opentexi2dvi2ps2pdf*)
            TEX=yes
            TEXI2DVI=yes
            DVIPS=yes
            PS2PDF=yes
            PDFOPEN=yes
            PDFAPP1=$(echo $OPTION | sed -e "s/--opentexi2dvi2ps2pdf=//"\
                                         -e "s/--opentexi2dvi2ps2pdf//"\
                                         -e "s/-otld=//"\
                                         -e "s/-otld//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            if [ ! -n "$PDFAPP1" -a ! -n "$PDFAPP" ]; then
                PDFAPP1=$PDFAPPIN
            elif [ -n "$PDFAPP" ]; then
                PDFAPP1=$PDFAPP
            fi
            OPTIONS_ECHO="$OPTIONS_ECHO - create PS and PDF via texi2dvi and open PDF with $PDFAPP1\n"
            ;;
        -td | --texi2dvi2pdf)
            TEX=yes
            TEXI2DVI=yes
            PDFLATEX=yes
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - create PDF via pdflatex\n"
            ;;
        -otd* | --opentexi2dvi2pdf*)
            TEX=yes
            TEXI2DVI=yes
            PDFLATEX=yes
            PDFOPEN=yes
            PDFAPP1=$(echo $OPTION | sed -e "s/--opentexi2dvi2pdf=//"\
                                         -e "s/--opentexi2dvi2pdf//"\
                                         -e "s/-otd=//"\
                                         -e "s/-otd//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            if [ ! -n "$PDFAPP1" -a ! -n "$PDFAPP" ]; then
                PDFAPP1=$PDFAPPIN
            elif [ -n "$PDFAPP" ]; then
                PDFAPP1=$PDFAPP
            fi
            OPTIONS_ECHO="$OPTIONS_ECHO - create PDF via texi2dvi-pdflatex and open it with $PDFAPP1\n"
            ;;
        -lp | --latex2dvi2ps)
            TEX=yes
            LATEX=yes
            DVIPS=yes
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - create PS via latex\n"
            ;;
        -olp* | --openlatex2dvi2ps*)
            TEX=yes
            LATEX=yes
            DVIPS=yes
            PSOPEN=yes
            PSAPP1=$(echo $OPTION | sed -e "s/--openlatex2dvi2ps=//"\
                                        -e "s/--opentexi2dvi2pdf//"\
                                        -e "s/-olp=//"\
                                        -e "s/-olp//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            if [ ! -n "$PSAPP1" -a ! -n "$PSAPP" ]; then
                PSAPP1=$PSAPPIN
            elif [ -n "$PSAPP" ]; then
                PSAPP1=$PSAPP
            fi
            OPTIONS_ECHO="$OPTIONS_ECHO - create PS via latex and open it with $PSAPP1\n"
            ;;
        -lld | --latex2dvi2ps2pdf)
            TEX=yes
            LATEX=yes
            DVIPS=yes
            PS2PDF=yes
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - create PS and PDF via latex\n"
            ;;
        -olld* | --openlatex2dvi2ps2pdf*)
            TEX=yes
            LATEX=yes
            DVIPS=yes
            PS2PDF=yes
            PDFOPEN=yes
            PDFAPP1=$(echo $OPTION | sed -e "s/--openlatex2dvi2ps2pdf=//"\
                                         -e "s/--openlatex2dvi2ps2pdf//"\
                                         -e "s/-olld=//"\
                                         -e "s/-olld//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            if [ ! -n "$PDFAPP1" -a ! -n "$PDFAPP" ]; then
                PDFAPP1=$PDFAPPIN
            elif [ -n "$PDFAPP" ]; then
                PDFAPP1=$PDFAPP
            fi
            OPTIONS_ECHO="$OPTIONS_ECHO - create PS and PDF via latex and open PDF with $PDFAPP1\n"
            ;;
        -ld | --latex2pdf)
            TEX=yes
            PDFLATEX=yes
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTIONS_ECHO="$OPTIONS_ECHO - create PDF via pdflatex\n"
            ;;
        -old* | --openlatex2pdf*)
            TEX=yes
            PDFLATEX=yes
            PDFOPEN=yes
            PDFAPP1=$(echo $OPTION | sed -e "s/--openlatex2pdf=//"\
                                         -e "s/--openlatex2pdf//"\
                                         -e "s/-old=//"\
                                         -e "s/-old//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            if [ ! -n "$PDFAPP1" -a ! -n "$PDFAPP" ]; then
                PDFAPP1=$PDFAPPIN
            elif [ -n "$PDFAPP" ]; then
                PDFAPP1=$PDFAPP
            fi
            OPTIONS_ECHO="$OPTIONS_ECHO - create PDF via pdflatex and open it with $PDFAPP1\n"
            ;;
        -optr=* | --optionsr=*)
            NEW=$(echo $OPTION | sed -e "s/--optionsr=//"\
                                     -e "s/-optr=//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTR="$OPTR $NEW"
            OPTIONS_ECHO="$OPTIONS_ECHO - use $OPTR in R\n"
            ;;
        -optt=* | --optionstexi2dvi=*)
            NEW=$(echo $OPTION | sed -e "s/--optionstexi2dvi=//"\
                                     -e "s/-optt=//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTTEXI2DVI="$OPTTEXI2DVI $NEW"
            OPTIONS_ECHO="$OPTIONS_ECHO - use $OPTTEXI2DVI in texi2dvi\n"
            ;;
        -optp=* | --optionsdvips=*)
            NEW=$(echo $OPTION | sed -e "s/--optionsdvips=//"\
                                     -e "s/-optp=//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTDVIPS="$OPTDVIPS $NEW"
            OPTIONS_ECHO="$OPTIONS_ECHO - use $OPTDVIPS in dvips\n"
            ;;
        -optf=* | --optionsps2pdf=*)
            NEW=$(echo $OPTION | sed -e "s/--optionsps2pdf=//"\
                                     -e "s/-optf=//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTPS2PDF="$OPTPS2PDF $NEW"
            OPTIONS_ECHO="$OPTIONS_ECHO - use $OPTPS2PDF in ps2pdf\n"
            ;;
        -optl=* | --optionslatex=*)                        
            NEW=$(echo $OPTION | sed -e "s/--optionslatex=//"\
                                     -e "s/-optl=//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION|")
            OPTLATEX="$OPTLATEX $NEW"
            OPTIONS_ECHO="$OPTIONS_ECHO - use $OPTLATEX in latex\n"
            ;;
        -optd=* | --optionspdflatex=*)
            NEW=$(echo $OPTION | sed -e "s/--optionspdflatex=//"\
                                     -e "s/-optd=//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTPDFLATEX="$OPTPDFLATEX $NEW"
            OPTIONS_ECHO="$OPTIONS_ECHO - use $OPTPDFLATEX in pdflatex\n"
            ;;
        -optb=* | --optionsbibtex=*)
            NEW=$(echo $OPTION | sed -e "s/--optionsbibtex=//"\
                                     -e "s/-optb=//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTBIBTEX="$OPTBIBTEX $NEW"
            OPTIONS_ECHO="$OPTIONS_ECHO - use $OPTBIBTEX in bibtex\n"
            ;;
        -optm=* | --optionsmakeindex=*)
            NEW=$(echo $OPTION | sed -e "s/--optionsmakeindex=//"\
                                     -e "s/-optm=//")
            OPTIONS=$(echo $OPTIONS | sed -e "s|$OPTION||")
            OPTMAKEINDEX="$OPTMAKEINDEX $NEW"
            OPTIONS_ECHO="$OPTIONS_ECHO - use $OPTMAKEINDEX in makeindex\n"
            ;;
        * ) # no options, just run Sweave with the default settings
            ;;
    esac
done

### }}}
### {{{ --- Processing options ---

FILES=$OPTIONS

# Options effect
if [ ! -n "$TEX" ]; then
    CLEAN=no
fi
if [ "$CLEAN" = "no" ]; then
    OPTTEXI2DVI=$(echo $OPTTEXI2DVI | sed -e "s/--clean//" -e "s/-c//")
fi
if [ "$QUIET" = "no" ]; then
    OPTTEXI2DVI=$(echo $OPTTEXI2DVI | sed -e "s/--quiet//" -e "s/-q//")
fi

# Print usage if there is no input files
if [ ! -n "$FILES" ]; then
    echo -e "\nError: There is no input files! Usage is:\n"
    usage
    exit 1
fi

### }}}
### {{{ --- Functions ---

# Weave & Tangle
sweaveWeave ()
{
    # $1 - file
    # $2 - logical [yes|no], should we weave?
    # $3 - logical [yes|no], should we tangle?
    # $4 - logical [yes|no], caching
    # $5 - cachedir
    COMM=''
    # Get weave info
    if [ "$2" = "yes" ]; then # do weave
        if [ "$4" = "no" ]; then # do not cache
            COMM="${COMM} library(knitr);"
            COMM="${COMM} opts_knit\$set(progress = TRUE, verbose = TRUE);"
            COMM="${COMM} knit(input='$1');"
        else                     # do cache
            #COMM="${COMM} library(package='cacheSweave');"
            COMM="${COMM} library(package='knit');"
            COMM="${COMM} opts_knit\$set(progress = TRUE, verbose = TRUE);"
            if [ -n "$5" ]; then # do set cachedir
                if [ ! -d "$5" ]; then
                    echo -e "\nNo such directory: $5"
                    exit 1
                fi
                COMM="${COMM} setCacheDir(path='$5');"
            fi
            COMM="${COMM} knit(input='$1', driver=cacheSweaveDriver);"
        fi
    fi
    # Get tangle info
    if [ "$3" = "yes" ]; then # do tangle
        COMM="${COMM} knit('$1', tangle=TRUE);"
    fi
    # Run the command
    echo $COMM | $R $OPTR
}

# Clean
sweaveClean ()
{
    # $1 - file
    # $2 - logical [yes|no], did we tangle?
    # $3 - logical [yes|no], remove LaTeX script?
    # R plots
    rm -f ${1}-*.pdf ${1}-*.eps Rplots.ps ${1}.dvi
    # Remove LaTeX script
    if [ "$3" = "yes" ]; then
        rm -f ${1}.tex
    fi
    # Standard LaTeX intermediate files
    rm -f ${1}.log ${1}.aux ${1}.out ${1}.dvi
    # Other LaTeX
    rm -f ${1}.toc ${1}.lot ${1}.lof ${1}.loa ${1}.bbl ${1}.blg
    rm -f ${1}.bm ${1}.idx ${1}.ilg ${1}.ind ${1}.tex.dep
    if [ "$2" = "no" ]; then
        rm -f ${1}.R
    fi
}

# Check for error
sweaveError ()
{
    # $1 - character string, name of the "process"
    EXIT=$(echo $?)
    if [ "$EXIT" != "0" ]; then
        echo -e "\nAn error occured after the use of '$1'!"
        if [ "$FORCE" = "no" ]; then
            echo -e "Quiting."
            exit 1
        fi
    fi
}

sweaveExt ()
{
    # $1 - file
    # Strip of the .Rnw, .Snw or .nw from filename
    FILE=$(echo $1 | sed -e 's/\.[Rr][Nn][Ww]$//' \
                         -e 's/\.[Ss][Nn][Ww]$//' \
                         -e 's/\.[Nn][Ww]$//'     \
                         -e 's/\.[Tt][Ee][Xx]$//')
    echo $FILE
}

# The LaTeX machine
sweaveLatex ()
{
    # $1 - file
    # $2 - logical [yes|no], remove LaTeX script?
    # File extension
    FILE=$(sweaveExt $1)
    # texi2dvi
    if [ "$TEXI2DVI" = "yes" ]; then
        echo -e " - using 'texi2dvi'"
        if [ "$DVIPS" = "yes" ]; then
            echo -e "\nPostscript creation"
            texi2dvi $OPTTEXI2DVI ${FILE}.tex
            sweaveError "texi2dvi $OPTTEXI2DVI ${FILE}.tex"
            dvips $OPTDVIPS ${FILE}.dvi -o ${FILE}.ps
            sweaveError "dvips $OPTDVIPS ${FILE}.dvi -o ${FILE}.ps"
            if [ "$PSOPEN" = "yes" ]; then
                echo -e "\nPS opening"
                $PSAPP1 ${FILE}.ps &
                sweaveError "$PSAPP1 ${FILE}.ps &"
            fi
            if [ "$PS2PDF" = "yes" ]; then
                echo -e "\nPDF creation"
                $PS2PDFAPP $OPTPS2PDF ${FILE}.ps ${FILE}.pdf
                sweaveError "$PS2PDFAPP $OPTPS2PDF ${FILE}.ps ${FILE}.pdf"
                if [ "$PDFOPEN" = "yes" ]; then
                    echo -e "\nPDF opening"
                    $PDFAPP1 ${FILE}.pdf &
                    sweaveError "$PDFAPP1 ${FILE}.pdf &"
                fi
            fi
        elif [ "$PDFLATEX" = "yes" ]; then
            echo -e "\nPDF creation"
            texi2dvi $OPTTEXI2DVI --pdf ${FILE}.tex
			#latexmk -pdf ${FILE}.tex
            sweaveError "texi2dvi texi2dvi $OPTTEXI2DVI --pdf ${FILE}.tex"
            if [ -n "$PDFOPEN" ]; then
                echo -e "\nPDF opening"
                $PDFAPP1 ${FILE}.pdf &
                sweaveError "$PDFAPP1 ${FILE}.pdf &"
            fi
        fi
    # LaTeX and friends
    elif [ "$LATEX" = "yes" ]; then
        echo -e " - using hardcoded list of 'LaTeX and friends' commands"
        if [ "$DVIPS" = "yes" ]; then
            echo -e "\nPostscript creation"
            latex $OPTLATEX $FILE
            sweaveError "latex $OPTLATEX $FILE"
            bibtex $OPTBIBTEX $FILE
            makeindex $OPTMAKEINDEX $FILE
            latex $OPTLATEX $FILE
            sweaveError "latex $OPTLATEX $FILE"
            latex $OPTLATEX $FILE
            sweaveError "latex $OPTLATEX $FILE"
            dvips $OPTDVIPS ${FILE}.dvi -o ${FILE}.ps
            sweaveError "dvips $OPTDVIPS ${FILE}.dvi -o ${FILE}.ps"
            if [ "$PSOPEN" = "yes" ]; then
                echo -e "\nPS opening"
                $PSAPP1 ${FILE}.ps &
                sweaveError "$PSAPP1 ${FILE}.ps &"
            fi
            if [ "$PS2PDF" = "yes" ]; then
                echo -e "\nPDF creation"
                $PS2PDFAPP $OPTPS2PDF ${FILE}.ps ${FILE}.pdf
                sweaveError "$PS2PDFAPP $OPTPS2PDF ${FILE}.ps ${FILE}.pdf"
                if [ "$PDFOPEN" = "yes" ]; then
                    echo -e "\nPDF opening"
                    $PDFAPP1 ${FILE}.pdf &
                    sweaveError "$PDFAPP1 ${FILE}.pdf &"
                fi
            fi
        fi
    # pdfLaTeX and friends
    else
        echo -e " - using hardcoded list of 'pdfLaTeX and friends' commands"
        echo -e "\nPDF creation"
        pdflatex $OPTPDFLATEX $FILE
        sweaveError "pdflatex $OPTPDFLATEX $FILE"
        bibtex $OPTBIBTEX $FILE
        makeindex $OPTMAKEINDEX $FILE
        pdflatex $OPTPDFLATEX $FILE
        sweaveError "pdflatex $OPTPDFLATEX $FILE"
        pdflatex $OPTPDFLATEX $FILE
        sweaveError "pdflatex $OPTPDFLATEX $FILE"
        if [ -n "$PDFOPEN" ]; then
            echo -e "\nPDF opening"
            $PDFAPP1 ${FILE}.pdf &
            sweaveError "$PDFAPP1 ${FILE}.pdf &"
        fi
    fi

    # Remove PDF and EPS pictures, DVI files and other tmp LaTeX files
    if [ "$CLEAN" = "yes" ]; then
        sweaveClean $FILE $TANGLE $2
    fi
}

### }}}
### {{{ --- Core ---

# -- Title ---
echo -e "\nRun Sweave and postprocess with LaTeX directly from the command line"

# --- Report options ---
echo -e "$OPTIONS_ECHO"

# --- Main program ---
# Check file types
for FILE in $FILES; do
    LYX=$(echo $FILE | grep '.[Ll][Yy][Xx]$')
    RNW=$(echo $FILE | grep '.[Rr][Nn][Ww]$')
    SNW=$(echo $FILE | grep '.[Ss][Nn][Ww]$')
    NW=$(echo $FILE | grep '.[Nn][Ww]$')
    TEXFILE=$(echo $FILE | grep '.[Tt][Ee][Xx]$')
    ## Export and rename LyX file
    if [ -n "$LYX" ]; then
        NAME=$(echo $FILE | sed -e "s/\.[Ll][Yy][Xx]$//")
        echo -e " - exporting $FILE to ${NAME}.Rnw"
        lyx $FILE -e latex
        mv -f ${NAME}.tex ${NAME}.Rnw
        FILE=$(echo $FILE | sed -e "s/\.[Ll][Yy][Xx]$/\.Rnw/")
    fi
    ## Keep LaTeX script if this was the input file!
    if [ -n "$TEXFILE" ]; then
        RMTEXFILE=no
    fi
    if [ ! -n "$RNW" -a ! -n "$SNW" -a ! -n "$NW" -a ! -n "$TEXFILE" ]; then
        echo "$FILE is not a supported file type!"
        echo "It should be one of: .lyx, .Rnw, .Snw., .nw or .tex"
        exit 1
    fi
    NEWFILES="$NEWFILES $FILE"
    unset LYX RNW SNW NW TEXFILE
done
FILES=$NEWFILES

# Clean old products
if [ "$CLEAN" = "yes" ]; then
    rm -f ${FILE}.pdf ${FILE}.ps
    sweaveClean $FILE $TANGLE $RMTEXFILE
fi

# Sweave and LaTeX in direct mode
for FILE in $FILES; do
    if [ "$WEAVE" = "yes" -o "$TANGLE" = "yes" ]; then
        sweaveWeave $FILE $WEAVE $TANGLE $CACHE $CACHEDIR
        sweaveError "R"
    fi
    if [ "$TEX" = "yes" -a "$MODE" = "yes" ]; then
        echo -e "\nLaTeX on produced TeX file i.e direct mode"
        sweaveLatex $FILE $RMTEXFILE
    fi
done

# LaTeX in late mode
if [ "$TEX" = "yes" -a "$MODE" = "no" ]; then
    echo -e "\nLaTeX on produced TeX files"
    for FILE in $FILES; do
        sweaveLatex $FILE $RMTEXFILE
    done
fi

# --- Exit ---
exit 0

### }}}
### }}}
### {{{ Dear Emacs
### Local variables:
### folded-file: t
### end:
### }}}

###------------------------------------------------------------------------
### Sweave.sh ends here
