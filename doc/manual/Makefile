xsltproc = xsltproc --nonet \
 --param section.autolabel 0 \
 --param section.label.includes.component.label 0 \
 --param chapter.autolabel 0 \
 --param chapter.label.includes.component.label 0 \
 --param appendix.autolabel 0 \
 --param appendix.label.includes.component.label 0 \
 --param generate.toc "'book toc,title chapter nop section nop sect1 nop sect2 nop sect3 nop sect4 nop sect5 nop'" \
 --param html.stylesheet \'style.css\' \
 --param xref.with.number.and.title 0 \
 --param toc.section.depth 3 \
 --param admon.style \'\' \
 --param callout.graphics.extension \'.gif\' \
 --param contrib.inline.enabled 0

docbookxsl = http://docbook.sourceforge.net/release/xsl/current

all: manual.html

manual.html: manual.xml introduction.xml options-composition.xml options-service.xml
	$(xsltproc) --xinclude --stringparam profile.condition manual \
	  $(docbookxsl)/profiling/profile.xsl manual.xml | \
	  $(xsltproc) --output manual.html $(docbookxsl)/xhtml/docbook.xsl -

# -e 's_<book lang="en">__' -e 's_</book>__'
%.xml: %.asciidoc
	asciidoctor --backend docbook45 --doctype article $<
	sed -e 's/<!DOCTYPE.*//' -e 's/<?asciidoc-toc?>//' -i $@
	cat $@

options-composition.xml options-service.xml:
	@if test -z '$(generatedDocBook)'; then echo "generatedDocBook env var is required. Are you running in 'nix-shell -A manual'?"; exit 1; fi
	cp $(generatedDocBook)/* .

install: all
	mkdir -p $(docdir)
	cp manual.html style.css $(docdir)
