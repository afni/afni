#include <check.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

START_TEST(test_buffer_read_never_exceeds_declared_length)
{
    // Invariant: Buffer reads never exceed the declared length
    const char *payloads[] = {
        "valid",                    // Valid input
        "A",                        // Boundary: 1 char
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", // 100 chars (overflow)
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB", // Another overflow pattern
        "-root",                    // Exact exploit case trigger
    };
    int num_payloads = sizeof(payloads) / sizeof(payloads[0]);

    for (int i = 0; i < num_payloads; i++) {
        pid_t pid = fork();
        if (pid == 0) {
            // Child process: execute the actual count.c program
            char *argv[] = {"./count", "-root", (char *)payloads[i], NULL};
            execvp("./count", argv);
            exit(EXIT_FAILURE); // execvp failed
        } else {
            // Parent process: wait and check for abnormal termination
            int status;
            waitpid(pid, &status, 0);
            ck_assert_msg(!WIFSIGNALED(status), 
                "Buffer overflow detected with payload '%s': program terminated by signal %d", 
                payloads[i], WTERMSIG(status));
        }
    }
}
END_TEST

Suite *security_suite(void)
{
    Suite *s;
    TCase *tc_core;

    s = suite_create("Security");
    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_buffer_read_never_exceeds_declared_length);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = security_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);

    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}