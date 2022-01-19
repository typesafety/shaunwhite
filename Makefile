.PHONY: help lint run

help:
	@echo TODO: Implement help target

# Run hlint on the project. Assumes that hlint has been installed and can be
# run with `stack exec hlint`.
lint:
	@cabal exec -- hlint --no-exit-code src \
		|| echo Ensure that hlint is installed for the project with \
			\"stack build hlint\".

# Run the bot locally. Attempts to use a token file "token" in the same
# directory, and a configuration file from
# "$XDG_CONFIG_HOME/.config/shaunwhite/config.json".
run:
	cabal run shaunwhite -- --token ./token
