shocamlc: main.ml
	@  ocamlc -o shocamlc main.ml

test: shocamlc
	@  ./shocamlc > output.s
	@  gcc -o output output.s
	@- ./output # '-'を付けると、0以外の終了ステータスを許容する

clean:
	@- rm shocamlc
	@- rm output
	@- rm output.s
