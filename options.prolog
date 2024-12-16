% M-x sweeprolog-view-messages

% listing(whatever/3).

% Show lists of codes as text (if 3 chars or longer)
:- portray_text(true).

:- set_prolog_flag(double_quotes, chars).

:- set_prolog_flag(answer_write_options,[max_depth(0)]).

:- set_prolog_flag(re_compile, true).

:- set_prolog_flag(occurs_check, error).
