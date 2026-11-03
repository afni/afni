#ifndef THD_PROGRESS_HEADER_
#define THD_PROGRESS_HEADER_

/* Shared CLI progress reporting.  All output goes to stderr.
 *
 * Configure once, optionally set options, then begin/update/finish each phase.
 * begin() closes an unfinished phase before starting another.  Counts within a
 * phase are monotonic: stale set() values are ignored; use begin() for restarts.
 * finish_status("converged") reports the actual count, never an invented 100%.
 *
 * add(), advance(), set(), and tick() serialize counting AND output under one
 * OpenMP critical section when compiled with USE_OMP.  Lifecycle/configuration
 * calls must be made with workers stopped (outside parallel regions).  Without
 * USE_OMP all calls must be serialized by the caller.  Independent displays and
 * unrelated diagnostics are not coordinated: use suspend()/resume() around
 * messages, and only one live in-place display on stderr at a time.
 *
 * Migration recipes (no command-line options or callers are changed here):
 *   3dMVPA: AUTO, SHOW_DEFAULT, advance() per completed location.
 *   3dRSA: same, with stage/nstage and a new begin() for each phase.
 *   3dInSync: SHOW_PERCENT|SHOW_RATE, advance() per voxel.
 *   3dSRM: set(iter, objective_text), begin() per random start, then
 *          finish_status("converged") on early convergence.
 *   SurfClustSim: LINE, options.every_units=10, percent_step=0;
 *          add(completed_blocks, simulation_count_text). TICKS for its legacy
 *          pacifier. A callback can call set(completed, NULL); its supplied
 *          maximum belongs in begin(), not in each callback invocation.
 *   3dShuffle: BAR, bar_width=40, percent_step=1, min_seconds=0.
 *   vstep_print: DIGITS (0123456789.); DOTS for simple dot pacifiers;
 *          INDEX for numbered volume ticks; COUNTER for 3dNLfim-style counts.
 *   Adaptive fits/streaming: begin_unknown(), add()/set(), tick() for heartbeat
 *          check-ins. Percentage and ETA are suppressed without a total.
 * Scientific diagnostics and GUI progress remain the caller's responsibility.
 */

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Preserve the four existing program mode values. AUTO resolves from stderr. */
#define THD_PROGRESS_AUTO     0
#define THD_PROGRESS_BAR      1
#define THD_PROGRESS_LINE     2
#define THD_PROGRESS_OFF      3
#define THD_PROGRESS_COUNTER  4  /* in-place count, without a bar */
#define THD_PROGRESS_DOTS     5  /* ..... */
#define THD_PROGRESS_DIGITS   6  /* 0123456789.0123456789. */
#define THD_PROGRESS_TICKS    7  /* 123456789.123456789. */
#define THD_PROGRESS_INDEX    8  /* 1.2.3. ... sampled completion counts */

#define THD_PROGRESS_SHOW_PERCENT  1
#define THD_PROGRESS_SHOW_RATE     2
#define THD_PROGRESS_SHOW_ELAPSED  4
#define THD_PROGRESS_SHOW_ETA      8
#define THD_PROGRESS_SHOW_DEFAULT 15

typedef int64_t THD_progress_count;

typedef struct {
   int bar_width;             /* 1..200, default 20 */
   int marks;                 /* 1..10000, default 50, capped at total */
   double min_seconds;        /* -1: default; otherwise >=0 */
   double percent_step;       /* -1: default; 0: off; otherwise (0,100] */
   THD_progress_count every_units; /* 0: off; otherwise positive */
   double heartbeat_seconds;  /* 0: off; otherwise >0, textual modes only */
} THD_progress_options;

/* Public for stack allocation; treat fields as private. Copy only inactive
 * instances. No heap allocation, callbacks, global counters, or timer threads. */
typedef struct {
   int enabled, mode, quiet, is_tty, flags, stage, nstage;
   int unknown, display, suspended, completed, line_open, printed_marks;
   THD_progress_count total, done, last_done;
   double start, last_report;
   THD_progress_options options;
   char program[32], label[64], unit[32], extra[192];
} THD_progress;

void THD_progress_options_default(THD_progress_options *options);
void THD_progress_config(THD_progress *p, const char *program, int mode, int quiet);
/* Call between phases. Returns 0 (without changing configuration) for invalid
 * options or an active phase; NULL options restores defaults. */
int THD_progress_set_options(THD_progress *p, const THD_progress_options *options);
/* Case-insensitive names matching all modes above. Invalid input leaves *mode. */
int THD_progress_parse_mode(const char *text, int *mode);

/* total<=0 disables this phase, as in the original API. Strings are copied and
 * bounded. quiet/OFF phases do no counting or output. */
void THD_progress_begin(THD_progress *p, int stage, int nstage,
                        const char *label, const char *unit,
                        THD_progress_count total, int flags);
/* Explicit unknown total: LINE stays LINE; all other visible modes use COUNTER.
 * No fraction, percentage, or ETA is fabricated. */
void THD_progress_begin_unknown(THD_progress *p, int stage, int nstage,
                                const char *label, const char *unit, int flags);
void THD_progress_advance(THD_progress *p);
/* Positive deltas saturate at total (or INT64_MAX if unknown). Negative deltas
 * are ignored. Zero delta checks cadence without advancing. */
void THD_progress_add(THD_progress *p, THD_progress_count delta, const char *extra);
void THD_progress_set(THD_progress *p, THD_progress_count done, const char *extra);
/* Check heartbeat/cadence with unchanged count. No background timer. NULL extra
 * retains the last text; "" clears it. This also applies to add() and set(). */
void THD_progress_tick(THD_progress *p, const char *extra);

/* Defaults: BAR/COUNTER refresh after 1s on a changed count; LINE every 10%.
 * Unknown LINE defaults to a 1s minimum and updates on changed counts.
 * Percentage and every_units thresholds are ORed, then min_seconds throttles
 * them. With neither enabled, changed counts are eligible. Heartbeat permits
 * unchanged counts, still subject to min_seconds. Initial/final reports bypass
 * throttling. Pacifiers instead catch up all crossed marks, subject only to
 * min_seconds; finish never fills in unearned marks. INDEX uses every_units
 * (default 1), emits the current count once per update, without catch-up spam.
 */

/* Suspend closes any in-place line; updates still count and retain extra text.
 * Resume redraws the latest state. Neither excludes pause time from elapsed. */
void THD_progress_suspend(THD_progress *p);
void THD_progress_resume(THD_progress *p);
/* Both are idempotent. finish() flushes unreported counts and ends the line;
 * early exits are labeled "stopped". finish_status() substitutes the supplied
 * status. After automatic 100% output, a status is a separate message, with no
 * duplicate final count. Finishing while suspended emits the final report. */
void THD_progress_finish(THD_progress *p);
void THD_progress_finish_status(THD_progress *p, const char *status);
void THD_progress_duration(double seconds, char *buf, size_t nbuf);

#ifdef __cplusplus
}
#endif
#endif
