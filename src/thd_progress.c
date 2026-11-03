/* Shared CLI displays; see thd_progress.h for contracts and migration recipes. */
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#include "thd_progress.h"
#include <stdio.h>
#include "debugtrace.h"  /* INFO_message, without the dataset/GUI headers */
#include <inttypes.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

static double progress_now(void)
{
#ifdef CLOCK_MONOTONIC
   struct timespec ts;
   if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0)
      return (double)ts.tv_sec + 1.e-9 * ts.tv_nsec;
#endif
   /* Fallback for platforms without a monotonic clock; never use the wrapping
      integer millisecond counter NI_clock_time(). Negative deltas are clamped. */
   {
      struct timeval tv;
      if (gettimeofday(&tv, NULL) != 0) return 0.0;
      return (double)tv.tv_sec + 1.e-6 * tv.tv_usec;
   }
}

static void progress_copy(char *dst, size_t size, const char *src)
{
   snprintf(dst, size, "%s", src ? src : "");
}

void THD_progress_duration(double seconds, char *buf, size_t size)
{
   long ss, hh, mm;
   if (!buf || !size) return;
   if (!(seconds > 0.0)) seconds = 0.0;
   if (seconds > 1.e9) seconds = 1.e9;
   ss = (long)(seconds + 0.5); hh = ss / 3600; mm = (ss % 3600) / 60; ss %= 60;
   if (hh) snprintf(buf, size, "%ldh%02ldm%02lds", hh, mm, ss);
   else if (mm) snprintf(buf, size, "%ldm%02lds", mm, ss);
   else snprintf(buf, size, "%lds", ss);
}

int THD_progress_parse_mode(const char *text, int *mode)
{
   static const char *names[] = {
      "auto", "bar", "line", "off", "counter", "dots", "digits", "ticks", "index"
   };
   int i;
   if (!text || !mode) return 0;
   for (i = 0; i <= THD_PROGRESS_INDEX; ++i)
      if (!strcasecmp(text, names[i])) { *mode = i; return 1; }
   return 0;
}

void THD_progress_options_default(THD_progress_options *o)
{
   if (!o) return;
   o->bar_width = 20; o->marks = 50;
   o->min_seconds = o->percent_step = -1.0;
   o->every_units = 0; o->heartbeat_seconds = 0.0;
}

void THD_progress_config(THD_progress *p, const char *program, int mode, int quiet)
{
   if (!p) return;
   memset(p, 0, sizeof(*p));
   progress_copy(p->program, sizeof(p->program), program);
   p->quiet = quiet != 0;
   p->is_tty = isatty(fileno(stderr));
   if (mode < THD_PROGRESS_AUTO || mode > THD_PROGRESS_INDEX) mode = THD_PROGRESS_AUTO;
   p->mode = mode == THD_PROGRESS_AUTO
           ? (p->is_tty ? THD_PROGRESS_BAR : THD_PROGRESS_LINE) : mode;
   THD_progress_options_default(&p->options);
}

int THD_progress_set_options(THD_progress *p, const THD_progress_options *options)
{
   THD_progress_options o;
   if (!p || p->enabled) return 0;
   if (options) o = *options;
   else THD_progress_options_default(&o);
   if (o.bar_width < 1 || o.bar_width > 200 || o.marks < 1 || o.marks > 10000 ||
       !isfinite(o.min_seconds) || (o.min_seconds < 0 && o.min_seconds != -1) ||
       !isfinite(o.percent_step) || (o.percent_step < 0 && o.percent_step != -1) ||
       o.percent_step > 100 || o.every_units < 0 ||
       !isfinite(o.heartbeat_seconds) || o.heartbeat_seconds < 0) return 0;
   p->options = o;
   return 1;
}

static int progress_pacifier(const THD_progress *p)
{
   return p->display >= THD_PROGRESS_DOTS;
}

static void progress_head(const THD_progress *p, char *buf, size_t size)
{
   char stage[32] = "";
   if (p->nstage > 0) snprintf(stage, sizeof(stage), "[%d/%d]", p->stage, p->nstage);
   snprintf(buf, size, "%s%s%s%s%s", p->program,
            p->program[0] && stage[0] ? " " : "", stage,
            (p->program[0] || stage[0]) && p->label[0] ? " " : "", p->label);
}

static void progress_close_line(THD_progress *p)
{
   if (p->line_open) {
      if (p->is_tty && !progress_pacifier(p)) fputs("\033[K", stderr);
      fputc('\n', stderr); fflush(stderr);
      p->line_open = 0;
   }
}

/* Exact ceil(total * mark / marks), without overflowing a 64-bit count. */
static THD_progress_count progress_threshold(THD_progress_count total, int mark, int marks)
{
   return (total / marks) * mark + ((total % marks) * mark + marks - 1) / marks;
}

static int progress_marks(const THD_progress *p)
{
   return p->total < p->options.marks ? (int)p->total : p->options.marks;
}

static int progress_due(const THD_progress *p, double now)
{
   double wait = fmax(0.0, now - p->last_report);
   double minimum = p->options.min_seconds;
   double percent = p->options.percent_step;
   int eligible = 0;
   if (minimum < 0) minimum = (!p->unknown && (p->display == THD_PROGRESS_LINE || progress_pacifier(p))) ? 0 : 1;
   if (wait < minimum) return 0;
   if (progress_pacifier(p) && p->display != THD_PROGRESS_INDEX) {
      int marks = progress_marks(p);
      return p->printed_marks < marks &&
             p->done >= progress_threshold(p->total, p->printed_marks + 1, marks);
   }
   if (percent < 0) percent = p->display == THD_PROGRESS_LINE && !p->unknown ? 10 : 0;
   if (p->unknown) percent = 0;
   if (percent > 0) {
      /* Compare floating buckets without narrowing potentially huge quotients. */
      if (percent <= 100.0L / p->total) eligible = p->done > p->last_done;
      else eligible = floorl(100.0L * p->done / p->total / percent) >
                      floorl(100.0L * p->last_done / p->total / percent);
   }
   if (p->options.every_units > 0)
      eligible |= p->done / p->options.every_units > p->last_done / p->options.every_units;
   if (percent == 0 && p->options.every_units == 0) eligible = p->done > p->last_done;
   if (!progress_pacifier(p) && p->options.heartbeat_seconds > 0)
      eligible |= wait >= p->options.heartbeat_seconds;
   return eligible;
}

/* A bounded tail assembled without interpreting caller text as a format. */
static void progress_append(char *buf, size_t size, const char *sep, const char *text)
{
   size_t used = strlen(buf);
   if (used < size) snprintf(buf + used, size - used, "%s%s", sep, text);
}

/* Caller serializes access. force: initial/resume; ending: finish/early exit. */
static void progress_render(THD_progress *p, int force, int ending, const char *status)
{
   double now, elapsed, rate;
   long double fraction;
   int final, inplace;
   char head[160], counts[128], tail[512] = "", text[128], duration[32];
   const char *sep;
   if (!p->enabled || p->suspended || p->completed) return;
   now = progress_now();
   final = ending || (!p->unknown && p->done == p->total);
   if (!force && !final && !progress_due(p, now)) return;
   progress_head(p, head, sizeof(head));
   fraction = p->unknown ? 0 : (long double)p->done / p->total;
   if (progress_pacifier(p)) {
      int marks = progress_marks(p);
      if (!p->line_open) { fprintf(stderr, "++ %s: ", head); p->line_open = 1; }
      if (p->display == THD_PROGRESS_INDEX) {
         if (p->done > p->last_done) fprintf(stderr, "%" PRId64 ".", p->done);
      } else while (p->printed_marks < marks &&
                    p->done >= progress_threshold(p->total, p->printed_marks + 1, marks)) {
         int mark = p->printed_marks++;
         if (p->display == THD_PROGRESS_DOTS) fputc('.', stderr);
         else if (p->display == THD_PROGRESS_DIGITS) {
            fputc('0' + mark % 10, stderr);
            if (mark % 10 == 9) fputc('.', stderr);
         } else fputc((mark + 1) % 10 ? '0' + (mark + 1) % 10 : '.', stderr);
      }
      if (final) {
         if (p->done == p->total) fputs(" done", stderr);
         else fprintf(stderr, " stopped at %" PRId64 "/%" PRId64 " %s", p->done, p->total, p->unit);
         if (p->extra[0]) fprintf(stderr, "; %s", p->extra);
         if (status && status[0]) fprintf(stderr, "; %s", status);
         progress_close_line(p);
      }
   } else {
      inplace = p->display != THD_PROGRESS_LINE;
      sep = inplace ? " | " : "; ";
      elapsed = fmax(0.0, now - p->start);
      rate = elapsed > 0 ? (double)p->done / elapsed : 0;
      if (!p->unknown && (p->flags & THD_PROGRESS_SHOW_PERCENT))
         snprintf(tail, sizeof(tail), " (%.1Lf%%)", 100 * fraction);
      if (p->flags & THD_PROGRESS_SHOW_RATE) {
         snprintf(text, sizeof(text), "%.2f %s/s", rate, p->unit);
         progress_append(tail, sizeof(tail), sep, text);
      }
      if (p->flags & THD_PROGRESS_SHOW_ELAPSED) {
         THD_progress_duration(elapsed, duration, sizeof(duration));
         snprintf(text, sizeof(text), "elapsed %s", duration);
         progress_append(tail, sizeof(tail), sep, text);
      }
      if (!p->unknown && !final && rate > 0 && (p->flags & THD_PROGRESS_SHOW_ETA)) {
         THD_progress_duration((double)(p->total - p->done) / rate, duration, sizeof(duration));
         snprintf(text, sizeof(text), "ETA ~%s", duration);
         progress_append(tail, sizeof(tail), sep, text);
      }
      if (p->extra[0]) progress_append(tail, sizeof(tail), sep, p->extra);
      if (status && status[0]) progress_append(tail, sizeof(tail), sep, status);
      if (p->unknown) snprintf(counts, sizeof(counts), "%" PRId64, p->done);
      else snprintf(counts, sizeof(counts), "%" PRId64 "/%" PRId64, p->done, p->total);
      if (inplace) {
         int i, fill = (int)(p->options.bar_width * fraction);
         fprintf(stderr, "%s++ %s ", p->line_open ? "\r" : "", head);
         if (p->display == THD_PROGRESS_BAR) {
            fputc('[', stderr);
            for (i = 0; i < p->options.bar_width; ++i) fputc(i < fill ? '#' : '-', stderr);
            fputs("] ", stderr);
         }
         fprintf(stderr, "%s %s%s", counts, p->unit, tail);
         p->line_open = 1;
         if (p->is_tty) fputs("\033[K", stderr);
         if (final) progress_close_line(p);
      } else INFO_message("%s: %s %s complete%s", head, counts, p->unit, tail);
   }
   fflush(stderr);
   p->last_done = p->done; p->last_report = now;
   if (final) p->completed = 1;
}

static void progress_begin(THD_progress *p, int stage, int nstage,
                           const char *label, const char *unit,
                           THD_progress_count total, int flags, int unknown)
{
   if (!p) return;
   THD_progress_finish(p);
   p->stage = stage; p->nstage = nstage > 0 ? nstage : 0;
   p->total = total; p->done = p->last_done = 0; p->flags = flags;
   p->unknown = unknown; p->display = p->mode;
   if (unknown && p->mode != THD_PROGRESS_LINE) p->display = THD_PROGRESS_COUNTER;
   p->suspended = p->completed = p->line_open = p->printed_marks = 0;
   p->extra[0] = '\0';
   progress_copy(p->label, sizeof(p->label), label);
   progress_copy(p->unit, sizeof(p->unit), unit);
   p->start = p->last_report = progress_now();
   p->enabled = !p->quiet && p->mode != THD_PROGRESS_OFF && (unknown || total > 0);
   progress_render(p, 1, 0, NULL);
}

void THD_progress_begin(THD_progress *p, int stage, int nstage,
                        const char *label, const char *unit, THD_progress_count total, int flags)
{
   progress_begin(p, stage, nstage, label, unit, total, flags, 0);
}

void THD_progress_begin_unknown(THD_progress *p, int stage, int nstage,
                                const char *label, const char *unit, int flags)
{
   progress_begin(p, stage, nstage, label, unit, 0, flags, 1);
}

static void progress_update(THD_progress *p, THD_progress_count value,
                            int absolute, const char *extra)
{
   if (!p) return;
#ifdef USE_OMP
#pragma omp critical(thd_progress_render)
#endif
   {
      if (p->enabled && !p->completed && value >= 0 && (!absolute || value >= p->done)) {
         THD_progress_count limit = p->unknown ? INT64_MAX : p->total;
         if (absolute) p->done = value > limit ? limit : value;
         else p->done += value > limit - p->done ? limit - p->done : value;
         if (extra) progress_copy(p->extra, sizeof(p->extra), extra);
         progress_render(p, 0, 0, NULL);
      }
   }
}

void THD_progress_add(THD_progress *p, THD_progress_count delta, const char *extra)
{
   progress_update(p, delta, 0, extra);
}

void THD_progress_advance(THD_progress *p) { THD_progress_add(p, 1, NULL); }
void THD_progress_tick(THD_progress *p, const char *extra) { THD_progress_add(p, 0, extra); }

void THD_progress_set(THD_progress *p, THD_progress_count done, const char *extra)
{
   progress_update(p, done, 1, extra);
}

void THD_progress_suspend(THD_progress *p)
{
   if (!p || !p->enabled || p->suspended) return;
   progress_close_line(p);
   p->suspended = 1;
}

void THD_progress_resume(THD_progress *p)
{
   if (!p || !p->enabled || !p->suspended) return;
   p->suspended = 0;
   progress_render(p, 1, 0, NULL);
}

void THD_progress_finish_status(THD_progress *p, const char *status)
{
   char head[160];
   if (!p || !p->enabled) return;
   p->suspended = 0;
   if (p->completed) {
      if (status && status[0]) {
         progress_head(p, head, sizeof(head));
         INFO_message("%s: %s", head, status);
      }
   } else {
      const char *end = status;
      if (!end || !end[0]) end = p->unknown ? "done" : p->done < p->total ? "stopped" : NULL;
      /* Pacifiers already spell out incomplete counts. */
      if (progress_pacifier(p) && (!status || !status[0])) end = NULL;
      progress_render(p, 1, 1, end);
   }
   progress_close_line(p);
   p->enabled = 0;
}

void THD_progress_finish(THD_progress *p) { THD_progress_finish_status(p, NULL); }
