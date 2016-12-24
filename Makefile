.PHONY: all clean push
SRCS = Popopo.elm SuperRollingBar.elm PokeDrill.elm
DSTS = $(SRCS:%.elm=%.html)
all: $(DSTS)

clean:
	rm $(DSTS)

push: $(DSTS)
	git add $(DSTS)
	git commit -m 'update'
	git push

%.html : %.elm
	elm make $< --output $@

