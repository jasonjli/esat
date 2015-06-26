-module(cnf).

% Module for manipulating CNF in dimac form
% Jason Jingshi Li, commenced in June 2015.
% jason.li@gmx.ch

-export_type([literal/0, clause/0, cnf/0]).
-export([print_cnf_manager/1, parse_cnf_file/1]).

-type literal() :: integer().
-type clause() :: [literal()].
-type cnf() :: [clause()].

-type cnf_manager() :: {integer(), integer(), cnf()}.

-spec print_literal(literal()) -> string().
print_literal(L) -> integer_to_list(L).

-spec print_clause(clause()) -> string().
print_clause([]) -> "0";
print_clause([First|Rest]) -> print_literal(First) ++ " " ++ print_clause(Rest).

-spec print_cnf(cnf()) -> string().
print_cnf([]) -> "0";
print_cnf([First|Rest]) -> print_clause(First) ++ io:format("~n",[]) ++ print_cnf(Rest).

-spec print_cnf_manager(cnf_manager()) -> string().
print_cnf_manager({LiteralCount, ClauseCount, CNF}) 
    -> io:format("p cnf ~p ~p~n", [LiteralCount, ClauseCount]) ++ print_cnf(CNF).

-spec parse_cnf_file(string()) -> cnf_manager().
parse_cnf_file(FileName) ->
    {ok, File} = file:read_file(FileName),
    parse_each_line(File, init_cnf_manager()).

-spec parse_each_line(file:io_device(), cnf_manager()) -> cnf_manager().
parse_each_line(File, CNF_Manager) ->
    case io:get_line(File, "") of
        eof -> file:close(File), finalize_cnf(CNF_Manager);
        Line -> New_CNF_Manager = parse_line(Line, CNF_Manager),
                parse_each_line(File, New_CNF_Manager)
    end.

-spec init_cnf_manager() -> cnf_manager().
init_cnf_manager() -> {0,0,[]}.

-spec finalize_cnf(cnf_manager()) -> cnf_manager().
finalize_cnf({NBVAR, NBCLAUSE, CNF}) -> {NBVAR, NBCLAUSE, lists:reverse(CNF)}.

-spec parse_line(string(), cnf_manager()) -> cnf_manager().
% Ignore empty lines and comments
parse_line("", CNF_Manager) -> CNF_Manager;
parse_line(["c"|_], CNF_Manager) -> CNF_Manager;

% Read clauses
parse_line(Line, CNF_Manager) ->
    Literals = re:split(string:strip(Line, right, 10), "\s+"),
    Clause = lists:map(fun erlang:list_to_integer/1, lists:map(fun erlang:binary_to_list/1, Literals)),
    append_clause(CNF_Manager,Clause).

-spec append_clause(cnf_manager(), clause()) -> cnf_manager().
append_clause(CNF_Manager, []) -> CNF_Manager;
append_clause({NumVar, NumClause, CNF}, Clause) -> {NumVar, NumClause, [Clause|CNF]}.
