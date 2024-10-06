NAME = ft_turing
DUNE = opam exec -- dune
TARGET = _build/default/bin/main.exe
INIT = opam init -y && opam install dune -y && opam install yojson -y

$(TARGET):
	$(DUNE) build
	cp ./_build/default/bin/main.exe ./main.exe
	mv ./main.exe ./ft_turing

run: $(TARGET)
	$(DUNE) exec $(NAME) $(ARGS)
	

first:
	$(INIT)

clean:
	$(DUNE) clean
	rm -rf ./ft_turing

re: clean $(TARGET)

.PHONY: build run clean re
