SOURCES = \
    make.jl \
    src/binaryheaps.md \
    src/index.md \
    src/install.md \
    src/library.md \
    src/nodes.md \
    src/priorityqueues.md

default: build/index.html

clean:
	rm -f *~ */*~ */*/*~

build/index.html: $(SOURCES)
	julia --color=yes make.jl

.PHOONY: default clean
