.PHONY: all clean
SRCS = Popopo.elm SuperRollingBar.elm
DSTS = $(SRCS:%.elm=%.html)
all: $(DSTS)

clean:
	rm $(DSTS)

%.html : %.elm
	elm make $< --output $@

