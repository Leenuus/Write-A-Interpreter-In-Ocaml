build_dir := _build/

all:
	dune exec interpreter

watch:
	dune build -w

test:
	dune test -w

clean: ${build_dir}
	rm -rf ${build_dir}