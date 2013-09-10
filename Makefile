RELNAME=hammerl
TABLE=blog

all:
	rebar get-deps; rebar compile; relx

run:
	_rel/bin/$(RELNAME) console

clean:
	rm -r _rel ebin/ deps

deps:
	rebar get-deps

docs:
	erl -noshell \
		-eval 'edoc:application($(RELNAME), ".", []), init:stop().'

database:
	mysql -u $(MYSQL_USER) -p $(TABLE) <blog.sql

dialyze:
	dialyzer --fullpath -Wno_undefined_callbacks -Wno_return \
	-r ebin/
