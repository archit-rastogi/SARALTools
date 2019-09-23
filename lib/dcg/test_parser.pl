:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

:- dynamic log_line/2, log_body/1.

/**
  Usage scenarios:
  1. given log file, render into structured interactive view. export to jira, etc.
  2. mine stats
     - test results
     - time spent
     - link with code editor, etc.
     - triage bugs
     - audit history
  
  parser: parses the whole log buffer into lines, dictated by Options
  Options define pattern to identify one log line.
     |
    \ /
  
  High level predicates on parsed output.

  Options (implement later):
  1. turn on or off line parser.
     - this is to defer parsing line at later point in time.
  2. Filter lines
     - by arbitrary predicate
  3. Start boundary of log line
  **/

parse_log_file(File, Lines, Rem) :-
        open(File,
             'read',
             Stream,
             [buffer(line)]),
        stream_to_lazy_list(Stream, Codes),
        phrase(dcg_log_buffer(Lines, _), Codes, Rem),
        !,
        close(Stream).

dcg_log_buffer([], _) --> eos.
dcg_log_buffer([Line|MoreLines], Options) -->
        dcg_log_line(Line, Options),
        dcg_log_buffer(MoreLines, Options).

dcg_log_line(log_line(datetime(Date,Tz,Time), log_body(Body)), Options) -->
        dcg_log_line_start(Date, Tz, Time),
        white,
        dcg_log_line_body(Body).

dcg_log_line_start([Y, M, D], Tz, [H,Min,S]) -->
        dcg_date(Y, M, D),
        dcg_time_zone(Tz),
        dcg_time(H, Min, S).

dcg_log_line_body(Body) -->
        string(Body),
        dcg_log_line_end.

dcg_log_line_end -->
        "\n".
dcg_log_line_end -->
        eos.
     
dcg_date(Year, Month, Day) -->
        dcg_year(Year),
        "-",
        dcg_month(Month),
        "-",
        dcg_day(Day).

dcg_year(Year) -->
        number(Year), {Year >= 2018}.

dcg_month(Month) -->
        number(Month), {Month >=1, Month =<12}.

dcg_day(Day) -->
        number(Day), {Day >=1, Day =<31}.

dcg_time_zone(Tz) --> "T", {Tz='T'}.
      
dcg_time(Hour, Min, Second) -->
        dcg_hour(Hour),
        ":",
        dcg_minute(Min),
        ":",
        dcg_second(Second).

dcg_hour(Hour) -->
        number(Hour), {Hour >=0, Hour =<24}.

dcg_minute(Minute) -->
        number(Minute), {Minute >=0, Minute =<60}.

dcg_second(Second) -->
        number(Second), {Second >=0, Second =<60}.

