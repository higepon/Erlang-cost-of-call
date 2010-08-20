TARGET=test_server.beam test.beam

all: $(TARGET)

%.beam: %.erl
	erlc $(ERLC_FLAGS)  -o . $<

start: $(TARGET)
	erl +K true -name test1@127.0.0.1 -noshell -noinput -s test start &

check: $(TARGET)
	erl +K true -name test2@127.0.0.1 -noshell -noinput -s test test

stop:
	pkill -f "test1@127.0.0.1"
