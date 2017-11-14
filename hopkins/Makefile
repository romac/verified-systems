
FILES := $(shell find src -name '*.scala' -type f)

ARGS :=
TIMEOUT := 5

STAINLESS := stainless
STAINLESS_OPTIONS := --timeout=$(TIMEOUT)

ifeq ($(WATCH),1)
STAINLESS_OPTIONS := --watch $(STAINLESS_OPTIONS)
endif

ifneq ($(NOCACHE),1)
STAINLESS_OPTIONS := --vc-cache $(STAINLESS_OPTIONS)
endif

ifeq ($(TERMINATION),1)
STAINLESS_OPTIONS := --termination --debug=termination $(STAINLESS_OPTIONS)
else
STAINLESS_OPTIONS := --verification --debug=verification $(STAINLESS_OPTIONS)
endif

STAINLESS_OPTIONS := $(STAINLESS_OPTIONS) $(ARGS)

%:
	$(STAINLESS) $(STAINLESS_OPTIONS) $(FILES) examples/$@.scala

