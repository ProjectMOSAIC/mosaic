export BIBINPUTS := ./inputs//:${BIBINPUTS}
export TEXINPUTS := ./inputs//:${TEXINPUTS}

SWEAVEOPTS = -c=cache 
SWEAVEOPTS = ""

RFILES = \
	  Chap-GettingStarted.R \
	  Chap-Manipulate.R \
	  Chap-SimulationBased.R \
	  Chap-Core.R \
	  Chap-CalculusForStats.R \
	  Chap-RIntro.R \
	  Chap-MoreR.R \
	  Chap-Internet.R \
	  Chap-Multivariate.R \


TEXFILES = \
	  Master-TeachingStatsWithR.tex \
	  Chap-FrontMatter.tex \
	  Chap-GettingStarted.tex \
	  Chap-Manipulate.tex \
	  Chap-SimulationBased.tex \
	  Chap-ByHand.tex \
	  Chap-MoreExamples.tex \
	  Chap-Core.tex \
	  Chap-CalculusForStats.tex \
	  Chap-Misc.tex \
	  Chap-Handouts.tex \
	  Chap-RIntro.tex \
	  Chap-MoreR.tex \
	  Chap-Multivariate.tex \
	  Chap-Internet.tex \

all: Master-TeachingStatsWithR.pdf

tex: $(TEXFILES)

R: $(RFILES)

clean:
	- \rm Chap-*.tex 
	- \rm Chap-*.aux 
	- \rm Master-*aux 
	- \rm Master-*tex 
	- \rm Master-*blg 
	- \rm Master-*ind 
	- \rm Master-TeachingStatsWithR*.pdf

%.tex: %.Rnw setup.R inputs/problems.sty inputs/authNote.sty \
	inputs/sfsect.sty inputs/USCOTS.bib
	bin/knitr.sh -nc $(SWEAVEOPTS) $< 

%.R: %.Rnw 
	bin/knitr.sh -t $(SWEAVEOPTS) $< 

Master-TeachingStatsWithR.pdf: $(TEXFILES) 
	./bin/latexmk -f -pdf  Master-TeachingStatsWithR.tex

Dierker.tex: Dierker.Rnw
	bin/knitr.sh -nc $(SWEAVEOPTS) Dierker.Rnw

Chap-GettingStartedshort.tex: Chap-GettingStartedshort.Rnw
	bin/knitr.sh -nc $(SWEAVEOPTS) Chap-GettingStartedshort.Rnw

Dierker.pdf: Dierker.tex Chap-GettingStartedshort.tex Chap-Core.tex
	./bin/latexmk -f -pdf  Dierker.tex

