%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Martin Scholl.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(ecsv_parser).
-include_lib("eunit/include/eunit.hrl").

-export([parse/2]).
-on_load(init/0).

-define(APPNAME, ecsv).
-define(LIBNAME, ecsv).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).


%
% incremental CSV parser. 
%
%  parse($,, <<"a,b,c">>)          = [incomplete,5,<<"a">>,<<"b">>,<<"c">>]
%  parse($,, <<"a,b,c\n">>)        = [ok,6,<<"a">>,<<"b">>,<<"c">>]
%  parse($,, <<"a,b,c\nd,e,f\n">>) = [ok,6,<<"a">>,<<"b">>,<<"c">>]
%
% -spec parse(delimiter(), rawdata()) -> [result(), bytes_parsed() | fields()]
%     delimiter() :: int()
%     rawdata() :: binary()
%     bytes_parsed() :: non_neg_integer()
%     fields() :: [binary]
%     result :: ok | incomplete | open_quote
%     
parse(_Delimiter, _RawData) ->
    exit(nif_library_not_loaded).


-ifdef(TEST).

simple_test_() ->
    Delim = $;,
    Tests = [
	      {<<"a">>, [incomplete, 1, <<"a">>]}
	     ,{<<"a\n">>, [ok, 2, <<"a">>]}
	     ,{<<>>, [incomplete, 0, <<>>]}
	     ,{<<"a;b">>, [incomplete, 3, <<"a">>, <<"b">>]}
	     ,{<<"a;b\n">>, [ok, 4, <<"a">>, <<"b">>]}
	     ,{<<"a;b\na">>, [ok, 4, <<"a">>, <<"b">>]}
	     ,{<<"\n\n">>, [ok, 1, <<>>]}
	     ,{<<"\r\n\\n">>, [ok, 2, <<>>]}
	     ,{<<"a;b\r\n">>, [ok, 5, <<"a">>, <<"b">>]}
	    ],
    [
     { Csv, ?_assertEqual(Result, parse(Delim, Csv)) }
     || {Csv, Result} <- Tests
    ].

-endif.
