all:    libfshlvmruntime.so libfshlvmllvmwrapper.so

clean:
	rm -f libfshlvmruntime.so libfshlvmllvmwrapper.so

libfshlvmruntime.so: runtime.cpp
	g++ -pg -pthread -Wall -O3 -fPIC -shared runtime.cpp -o libfshlvmruntime.so

libfshlvmllvmwrapper.so: llvm.cpp
	g++ -I /usr/lib/llvm-3.4/include -L /usr/lib/llvm-3.4/lib -lLLVM-3.4 -pg -pthread -Wall -O3 -fPIC -shared llvm.cpp -o libfshlvmllvmwrapper.so

DESTDIR = ../../lib
install: libfshlvmruntime.so libfshlvmllvmwrapper.so
	install libfshlvmruntime.so $(DESTDIR)
	install libfshlvmllvmwrapper.so $(DESTDIR)

.PHONY:	clean all
