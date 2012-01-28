VERSION=0.0.1
ERLCFLAGS=-o
ERLC=/usr/bin/env erlc
SRCDIR=src
BEAMDIR=./ebin
LIBDIR=/usr/lib/erlang/lib/rsg-$(VERSION)
INITDIR=/etc/rc.d

all:
	@mkdir -p $(BEAMDIR);
	@$(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl;

install:
	@mkdir -p $(LIBDIR)
	@cp $(SRCDIR)/rsg.init $(INITDIR)/rsg
	@chmod +x $(INITDIR)/rsg
	@cp -rf $(BEAMDIR) $(SRCDIR) $(LIBDIR)

clean:
	@rm -rf $(BEAMDIR);
	@rm -rf erl_crush.dump
