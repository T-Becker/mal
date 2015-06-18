DLL = mal.dll
PATH = /Library/Frameworks/Mono.framework/Versions/4.0.1/lib/mono/4.5/fsc.exe
CC = mono $(PATH)

all: step0_repl step1_read_print

step0_repl: step0_repl.exe
step1_read_print: step1_read_print.exe

$(DLL): types.fs printer.fs atom.fs ast.fs reader.fs
	$(CC) -a -O $+ --out:$@

step0_repl.exe: step0_repl.fs
	$(CC) --out:$@ $+

%.exe: %.fs $(DLL)
	$(CC) --reference:$(DLL) $(@:%.exe=%.fs) --out:$@ 
