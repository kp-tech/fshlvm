all:    libfshlvmruntime.so libfshlvmllvmwrapper.so

clean:
	rm -f libfshlvmruntime.so libfshlvmllvmwrapper.so

libfshlvmruntime.so: runtime.cpp
	g++ -pg -pthread -Wall -O3 -fPIC -shared -std=c++11 runtime.cpp -o libfshlvmruntime.so

libfshlvmllvmwrapper.so: llvm.cpp
	g++ -I /usr/lib/llvm-3.8/include -L /usr/lib/llvm-3.8/lib -lLLVM-3.8 -pg -pthread -Wall -O3 -fPIC -shared -std=c++11 llvm.cpp -o libfshlvmllvmwrapper.so

DESTDIR = ../../lib
install: libfshlvmruntime.so libfshlvmllvmwrapper.so
	install libfshlvmruntime.so $(DESTDIR)
	install libfshlvmllvmwrapper.so $(DESTDIR)

.PHONY:	clean all
