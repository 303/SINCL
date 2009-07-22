RESULT = sincl
SOURCES = \
	util.ml \
	srcloc.ml \
	tree.ml \
	parsererr.ml \
	type.ml \
	idtable.ml \
	parser.mly \
	lexer.mll \
	error.ml \
	pptree.ml \
	main.ml
LIBS = unix str

YFLAGS = -v

include OCamlMakefile
