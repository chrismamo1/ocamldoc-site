all: ocamldoc_site.native

ocamldoc_site.native: ocamldoc_site.ml
	corebuild -pkg opium,cow,cow.syntax,sqlite3,csv,netstring $@

test: ocamldoc_site.native
	cp packages.db ./env/ && cd env && ../ocamldoc_site.native -p 9009 -d

run: ocamldoc_site.native
	cd env && ../ocamldoc_site.native -p 80

clean:
	corebuild -clean
