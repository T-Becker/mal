DLL = mal.dll
FSC = /Library/Frameworks/Mono.framework/Versions/4.0.1/lib/mono/4.5/fsc.exe
FSHARPC = mono $(FSC)

all: step0_repl step1_read_print

step0_repl: step0_repl.exe
step1_read_print: step1_read_print.exe

$(DLL): types.fs printer.fs atom.fs ast.fs reader.fs
	$(FSHARPC) -a -O $+ -o $@

step0_repl.exe: step0_repl.fs
	$(FSHARPC) -o $@ $+

%.exe: %.fs $(DLL)
	$(FSHARPC) -r $(DLL) $(@:%.exe=%.fs) -o $@ 

clean:
	rm -rf *.exe $(DLL) bin/ obj/

