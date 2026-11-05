# Include after src/Makefile; build only, no production Makefile changes.
# make -C src -f Makefile -f pmolfese/tests/insync_m0.mk insync_m0_test_binaries
INSYNC_M0_TEST_DIR ?= /tmp/insync-m0-tests

.PHONY: insync_m0_test_binaries
insync_m0_test_binaries: $(INSYNC_M0_TEST_DIR)/test_insync $(INSYNC_M0_TEST_DIR)/test_xmat $(INSYNC_M0_TEST_DIR)/test_window_fixture

$(INSYNC_M0_TEST_DIR):
	mkdir -p "$@"

$(INSYNC_M0_TEST_DIR)/test_insync: pmolfese/tests/test_insync.c thd_insync_stats.c thd_insync.h | $(INSYNC_M0_TEST_DIR)
	$(CCOMP) $(INFLAGS) -o "$@" pmolfese/tests/test_insync.c thd_insync_stats.c -lm

$(INSYNC_M0_TEST_DIR)/test_xmat: pmolfese/tests/test_xmat.c thd_xmat.o thd_datatable.o | $(INSYNC_M0_TEST_DIR)
	$(CCOMP) $(INFLAGS) -o "$@" pmolfese/tests/test_xmat.c thd_xmat.o thd_datatable.o $(LFLAGS) $(LLIBS)

$(INSYNC_M0_TEST_DIR)/test_window_fixture: pmolfese/tests/test_insync_window_fixture.c thd_xmat.o thd_datatable.o | $(INSYNC_M0_TEST_DIR)
	$(CCOMP) $(INFLAGS) -o "$@" pmolfese/tests/test_insync_window_fixture.c thd_xmat.o thd_datatable.o $(LFLAGS) $(LLIBS)

$(INSYNC_M0_TEST_DIR)/test_selection_kernel: pmolfese/tests/test_insync_selection_kernel.c thd_insync_stats.c thd_insync.h | $(INSYNC_M0_TEST_DIR)
	$(CCOMP) $(INFLAGS) -o "$@" pmolfese/tests/test_insync_selection_kernel.c thd_insync_stats.c -lm
