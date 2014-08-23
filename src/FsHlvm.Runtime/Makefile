all:    libfshlvmruntime.so libfshlvmllvmwrapper.so

clean:
	rm -f libfshlvmruntime.so libfshlvmllvmwrapper.so

libfshlvmruntime.so: runtime.cpp
	g++ -pg -pthread -Wall -O3 -fPIC -shared runtime.cpp -o libfshlvmruntime.so

libfshlvmllvmwrapper.so: llvm.cpp
	g++ -I /usr/local/stow/llvm-3.2-hsail/include -L /usr/local/stow/llvm-3.2-hsail/lib -lLLVM-3.2 -pg -pthread -Wall -O3 -fPIC -shared llvm.cpp -o libfshlvmllvmwrapper.so

DESTDIR = ../../lib
install: libfshlvmruntime.so libfshlvmllvmwrapper.so
	install libfshlvmruntime.so $(DESTDIR)
	install libfshlvmllvmwrapper.so $(DESTDIR)

.PHONY:	clean all
