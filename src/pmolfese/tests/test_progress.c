/* Include implementation to test cadence and integer thresholds deterministically,
 * without sleeps or test-only production hooks. */
#include "../../thd_progress.c"
#include <stdlib.h>

#define CHECK(x) do { if (!(x)) { \
   fprintf(stdout, "FAIL line %d: %s\n", __LINE__, #x); exit(1); \
} } while (0)

static FILE *capture;
static int saved_stderr;

static void capture_begin(void)
{
   fflush(stderr);
   saved_stderr = dup(fileno(stderr));
   capture = tmpfile();
   CHECK(saved_stderr >= 0 && capture);
   CHECK(dup2(fileno(capture), fileno(stderr)) >= 0);
}

static char *capture_end(void)
{
   long size;
   char *text;
   fflush(stderr);
   size = ftell(capture);
   CHECK(size >= 0);
   text = malloc((size_t)size + 1);
   CHECK(text);
   rewind(capture);
   CHECK(fread(text, 1, (size_t)size, capture) == (size_t)size);
   text[size] = '\0';
   CHECK(dup2(saved_stderr, fileno(stderr)) >= 0);
   close(saved_stderr); fclose(capture);
   return text;
}

static int occurrences(const char *s, const char *needle)
{
   int n = 0;
   while ((s = strstr(s, needle))) { ++n; s += strlen(needle); }
   return n;
}

static void begin(THD_progress *p, int mode, THD_progress_count total)
{
   THD_progress_config(p, "test", mode, 0);
   THD_progress_begin(p, 1, 2, "work", "items", total, THD_PROGRESS_SHOW_DEFAULT);
}

static void test_modes(void)
{
   THD_progress p;
   THD_progress_options o;
   char *s, duration[32];
   int i, mode = 99;
   CHECK(THD_progress_parse_mode("bAr", &mode) && mode == THD_PROGRESS_BAR);
   CHECK(!THD_progress_parse_mode("nonsense", &mode) && mode == THD_PROGRESS_BAR);
   CHECK(!THD_progress_parse_mode(NULL, &mode));
   CHECK(!THD_progress_parse_mode("off", NULL));
   THD_progress_duration(NAN, duration, sizeof(duration)); CHECK(!strcmp(duration, "0s"));
   THD_progress_duration(-1, duration, sizeof(duration)); CHECK(!strcmp(duration, "0s"));
   THD_progress_duration(3723, duration, sizeof(duration)); CHECK(!strcmp(duration, "1h02m03s"));
   THD_progress_duration(INFINITY, duration, sizeof(duration)); CHECK(duration[0]);
   THD_progress_duration(2, duration, 1); CHECK(!duration[0]);
   capture_begin();
   for (i = 0; i < 3; ++i) {
      THD_progress_config(&p, "test", i == 0 ? THD_PROGRESS_OFF : THD_PROGRESS_BAR, i == 1);
      THD_progress_begin(&p, 0, 0, "quiet", "items", i == 2 ? 0 : 100, 15);
      THD_progress_add(&p, 100, "ignored"); THD_progress_finish_status(&p, "done");
      CHECK(p.done == 0);
   }
   s = capture_end(); CHECK(!*s); free(s);
   THD_progress_config(&p, "test", THD_PROGRESS_LINE, 0);
   THD_progress_options_default(&o);
   o.bar_width = 0; CHECK(!THD_progress_set_options(&p, &o));
   o.bar_width = 40; o.percent_step = NAN; CHECK(!THD_progress_set_options(&p, &o));
   o.percent_step = 1; o.min_seconds = -2; CHECK(!THD_progress_set_options(&p, &o));
   o.min_seconds = 0; CHECK(THD_progress_set_options(&p, &o));
   CHECK(p.options.bar_width == 40);
   CHECK(THD_progress_set_options(&p, NULL) && p.options.bar_width == 20);
}

static void test_counts(void)
{
   THD_progress p;
   char *s;
   capture_begin();
   begin(&p, THD_PROGRESS_LINE, 100);
   CHECK(!THD_progress_set_options(&p, NULL));
   THD_progress_set(&p, 80, "objective 12.5");
   THD_progress_set(&p, 0, "stale");
   THD_progress_set(&p, -4, "negative");
   THD_progress_add(&p, -10, "negative");
   CHECK(p.done == 80);
   THD_progress_set(&p, 100, "final objective");
   THD_progress_set(&p, 100, NULL);
   THD_progress_advance(&p);
   THD_progress_finish(&p); THD_progress_finish(&p);
   s = capture_end();
   CHECK(occurrences(s, "100/100") == 1 && occurrences(s, "0/100 items complete (0.0%)") == 1);
   CHECK(!strstr(s, "stale") && !strstr(s, "negative"));
   CHECK(strstr(s, "[1/2] work") && strstr(s, "final objective")); free(s);

   capture_begin();
   begin(&p, THD_PROGRESS_LINE, INT64_MAX);
   THD_progress_add(&p, INT64_MAX - 1, NULL);
   THD_progress_add(&p, INT64_MAX, NULL);
   CHECK(p.done == INT64_MAX);
   THD_progress_finish(&p);
   s = capture_end(); CHECK(strstr(s, "9223372036854775807/9223372036854775807")); free(s);
}

static void test_lifecycle(void)
{
   THD_progress p;
   char *s;
   capture_begin();
   begin(&p, THD_PROGRESS_BAR, 100);
   THD_progress_set(&p, 3, "objective 42"); /* below ordinary cadence */
   THD_progress_finish_status(&p, "converged");
   THD_progress_finish_status(&p, "duplicate");
   THD_progress_begin(&p, 2, 2, "restart", "iterations", 100, 1);
   THD_progress_set(&p, 5, NULL);
   THD_progress_begin(&p, 0, 0, "third", "iterations", 10, 1);
   THD_progress_finish(&p);
   s = capture_end();
   CHECK(strstr(s, "3/100 items (3.0%)") && strstr(s, "objective 42 | converged\n"));
   CHECK(!strstr(s, "100.0%") && !strstr(s, "duplicate"));
   CHECK(strstr(s, "5/100 iterations (5.0%) | stopped\n++ test third"));
   CHECK(s[strlen(s)-1] == '\n'); free(s);

   capture_begin();
   begin(&p, THD_PROGRESS_BAR, 10);
   THD_progress_suspend(&p); THD_progress_suspend(&p);
   fprintf(stderr, "diagnostic\n");
   THD_progress_add(&p, 10, "latest"); CHECK(p.done == 10 && !p.completed);
   THD_progress_resume(&p); THD_progress_resume(&p);
   THD_progress_finish_status(&p, "success");
   s = capture_end();
   CHECK(strstr(s, "\ndiagnostic\n++ test"));
   CHECK(occurrences(s, "10/10") == 1 && strstr(s, "latest") && strstr(s, "success")); free(s);

   capture_begin();
   begin(&p, THD_PROGRESS_LINE, 100);
   THD_progress_suspend(&p); THD_progress_add(&p, 7, "pending");
   THD_progress_finish_status(&p, "cancelled");
   s = capture_end(); CHECK(strstr(s, "7/100") && strstr(s, "pending; cancelled")); free(s);
}

static void test_cadence(void)
{
   THD_progress p;
   char *s;
   capture_begin();
   begin(&p, THD_PROGRESS_LINE, 100);
   p.last_report = 10; p.last_done = 0;
   p.done = 9; CHECK(!progress_due(&p, 10));
   p.done = 10; CHECK(progress_due(&p, 10));
   p.options.percent_step = 0; p.options.every_units = 10;
   p.done = 19; p.last_done = 11; CHECK(!progress_due(&p, 10));
   p.done = 20; CHECK(progress_due(&p, 10));
   p.options.min_seconds = 2; CHECK(!progress_due(&p, 11)); CHECK(progress_due(&p, 12));
   p.last_done = p.done; p.options.heartbeat_seconds = 5;
   CHECK(!progress_due(&p, 14)); CHECK(progress_due(&p, 15));
   /* Exercise public tick after forcing a past heartbeat deadline. */
   p.last_report = progress_now() - 6;
   THD_progress_tick(&p, "still working");
   THD_progress_finish(&p);
   s = capture_end(); CHECK(strstr(s, "still working")); free(s);
}

static void test_unknown(void)
{
   THD_progress p;
   char *s;
   int mode;
   for (mode = THD_PROGRESS_BAR; mode <= THD_PROGRESS_INDEX; ++mode) {
      if (mode == THD_PROGRESS_OFF) continue;
      capture_begin();
      THD_progress_config(&p, "stream", mode, 0);
      THD_progress_begin_unknown(&p, 0, 0, "read", "bytes", 15);
      THD_progress_add(&p, INT64_MAX - 2, "received");
      THD_progress_add(&p, 10, NULL); CHECK(p.done == INT64_MAX && !p.completed);
      THD_progress_finish(&p);
      s = capture_end();
      CHECK(!strstr(s, "%") && !strstr(s, "ETA") && !strstr(s, "/0"));
      CHECK(strstr(s, "9223372036854775807 bytes") && strstr(s, "received") && strstr(s, "done"));
      free(s);
   }
}

static void test_styles(void)
{
   THD_progress p;
   THD_progress_options o;
   char *s;
   const char *expected[] = {".......... done", "0123456789. done", "123456789. done"};
   int mode, mark;
   for (mode = THD_PROGRESS_DOTS; mode <= THD_PROGRESS_TICKS; ++mode) {
      capture_begin(); begin(&p, mode, 10);
      THD_progress_add(&p, 4, NULL); THD_progress_add(&p, 6, NULL);
      THD_progress_finish(&p);
      s = capture_end(); CHECK(strstr(s, expected[mode - THD_PROGRESS_DOTS])); free(s);
   }
   capture_begin(); begin(&p, THD_PROGRESS_DIGITS, 100);
   THD_progress_add(&p, 5, NULL); THD_progress_finish(&p);
   s = capture_end(); CHECK(strstr(s, "01 stopped at 5/100")); CHECK(!strstr(s, "012")); free(s);
   capture_begin(); begin(&p, THD_PROGRESS_INDEX, 3);
   THD_progress_advance(&p); THD_progress_advance(&p); THD_progress_advance(&p);
   THD_progress_finish(&p);
   s = capture_end(); CHECK(strstr(s, "1.2.3. done")); free(s);
   capture_begin();
   THD_progress_config(&p, "shuffle", THD_PROGRESS_BAR, 0);
   THD_progress_options_default(&o); o.bar_width = 40; o.min_seconds = 0; o.percent_step = 1;
   CHECK(THD_progress_set_options(&p, &o));
   THD_progress_begin(&p, 0, 0, "permuting", "permutations", 100, 1);
   THD_progress_set(&p, 1, NULL); THD_progress_set(&p, 50, NULL);
   THD_progress_set(&p, 100, NULL); THD_progress_finish(&p);
   s = capture_end();
   CHECK(strstr(s, "1/100") && strstr(s, "[####################--------------------]")); free(s);
   for (mark = 1; mark <= 10000; ++mark) {
      THD_progress_count threshold = progress_threshold(INT64_MAX, mark, 10000);
      CHECK(threshold > 0 && threshold <= INT64_MAX);
      if (mark > 1) CHECK(threshold > progress_threshold(INT64_MAX, mark - 1, 10000));
   }
   CHECK(progress_threshold(INT64_MAX, 10000, 10000) == INT64_MAX);
}

static void test_parallel(void)
{
   THD_progress p;
   char *s, *cursor;
   int i, count, previous = -1;
   capture_begin(); begin(&p, THD_PROGRESS_LINE, 10000);
#ifdef USE_OMP
#pragma omp parallel for
#endif
   for (i = 0; i < 20000; ++i) THD_progress_advance(&p);
   CHECK(p.done == 10000 && p.completed);
   THD_progress_finish(&p);
   s = capture_end(); cursor = s;
   while ((cursor = strstr(cursor, "work: "))) {
      CHECK(sscanf(cursor, "work: %d/10000", &count) == 1);
      CHECK(count > previous); previous = count; ++cursor;
   }
   CHECK(previous == 10000 && occurrences(s, "10000/10000") == 1);
   free(s);
}

int main(int argc, char **argv)
{
   if (argc == 3 && !strcmp(argv[1], "--demo")) {
      THD_progress p; int mode;
      if (!THD_progress_parse_mode(argv[2], &mode)) return 2;
      begin(&p, mode, 10); THD_progress_add(&p, 10, NULL); THD_progress_finish(&p);
      return 0;
   }
   test_modes(); test_counts(); test_lifecycle(); test_cadence();
   test_unknown(); test_styles(); test_parallel();
   puts("thd_progress: all tests passed");
   return 0;
}
