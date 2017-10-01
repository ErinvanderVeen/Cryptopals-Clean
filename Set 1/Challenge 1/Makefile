INCLUDES:=-I $$CLEAN_HOME/lib/Platform -I $$CLEAN_HOME/lib/Generics -I $$CLEAN_HOME/lib/StdLib
FLAGS:=-l -no-pie
EXE:=hex_to_base64

all: *.icl *.dcl
	clm $(FLAGS) $(INCLUDES) $(CLIBS) $(EXE) -o $(EXE).out

clean:
	$(RM) -r "Clean System Files" $(EXE)
