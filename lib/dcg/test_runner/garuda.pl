/** Garuda is the test runner written in python at rubrik.
  This module define dcg grammar extract useful information from test logs.

  log_level, module, line number, Thread
  run summary
  failure/skip details: module name, module setup, class name, class setup, methods, status, duration, teardown
  traceback, filename, linenumber, method name, exception name, error message
  aggregated stats
*/

:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

:- dynamic log_level/1,
        log_thread_name/1,
        logger/1,
        log_message/1,
        key_value_status_duration/4.

dcg_log_level("INFO") --> "INFO".
dcg_log_level("DEBUG") --> "DEBUG".
dcg_log_level("WARNING") --> "WARNING".
dcg_log_level("ERROR") --> "ERROR".

dcg_process_thread(ProcessId, ThreadName) -->
        dcg_log_process_id(ProcessId),
        dcg_thread_name(ThreadName).

dcg_log_process_id(ProcId) -->
        "[",
        number(ProcId),
        "]".

dcg_thread_name("MainThread") -->
        "MainThread".
dcg_thread_name(ThreadName) -->
        string(Codes),
        "-",
        number(ThreadNumber),
        { string_codes(ThreadPrefix, Codes),
          atomics_to_string([ThreadPrefix, '-', ThreadNumber],
                            ThreadName)
        }.

dcg_run_summary(Summary) -->
        string(_),
        dcg_run_summary_title,
        dcg_run_summary_details(Summary).

dcg_run_summary_title --> "Run Summary".

dcg_run_summary_details([]) -->
        eos.
dcg_run_summary_details([Detail|MoreDetails]) -->
        dcg_run_summary_next(Detail),
        dcg_run_summary_details(MoreDetails).

dcg_run_summary_next(Detail) -->
        string(Key),
        whites,
        ":",
        string(Value),
        whites,
        dcg_execution_status(Status),
        whites,
        dcg_execution_duration(Duration, Unit),
        blanks,
        {
         Detail = key_value_status_duration(Key,
                                            Value,
                                            Status,
                                            duration(Duration, Unit))
        }.
dcg_run_summary_next(Detail) -->
        string(_),
        "\n",
        dcg_run_summary_next(Detail).

dcg_execution_status("Sucess") -->
        "Success".
dcg_execution_status("NotRun") -->
        "NotRun".
dcg_execution_status("Error") -->
        "Error".
dcg_execution_status("Fail") -->
        "Fail".
dcg_execution_status("") --> [].

dcg_execution_duration(Duration, "secs") -->
        number(Duration),
        white,
        "secs".