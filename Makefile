.PHONY: help lint

help:
	@echo TODO: Implement help target

lint:
	@stack exec -- hlint --no-exit-code lib \
		|| echo Ensure that hlint is installed for the project with \
			\"stack build hlint\".

