
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

  Other design requirements:
  1. rolling logs, when logs are rolled over to next file
  2. compressed logs in gz
  3. Logs are uploaded to archival
  **/

parse_log_file(File, Lines) :-
        setup_call_cleanup(open_stream(File, Stream),
                           parse_log_stream(Stream, Lines),
                           close(Stream)).

open_stream(File, Stream):-
        open(File, 'read', Stream, [ buffer(line)]).

parse_log_stream(Stream, Lines):-
        stream_to_lazy_list(Stream, Codes),
        once(phrase(dcg_log_buffer(Lines, _), Codes, _)).
                          