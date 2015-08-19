/* -------------------------------------------------------------------
 *
 * Copyright (c) 2015 Martin Scholl.  All Rights Reserved.
 *
 * This file is provided to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License.  You may obtain
 * a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 * -------------------------------------------------------------------
 */
#include "erl_nif.h"
#include <stdio.h>
#include <string.h>

static ERL_NIF_TERM atom_incomplete;
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_open_quote;

static const size_t INITIAL_SIZE = 64;

static ERL_NIF_TERM
make_result(ErlNifEnv* env,
	    ERL_NIF_TERM head,
	    const size_t bytes_parsed,
	    ERL_NIF_TERM* items, size_t nitems)
{
	items[0] = head;
	items[1] = enif_make_ulong(env, bytes_parsed);
	return enif_make_list_from_array(env, items, nitems);
}

static ERL_NIF_TERM
parser_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ERL_NIF_TERM result;
	unsigned int delimiter_;
	ErlNifBinary bin_raw;
	const unsigned char* iter;
	const unsigned char* prev_iter;
	const unsigned char* bin_end;
	size_t        bytes_parsed = 0;
	unsigned char delimiter;
	size_t        items_size = INITIAL_SIZE;
	size_t        nitems     = 2; // reserve room for result head, # bytes parsed 
	ERL_NIF_TERM  items_on_stack[INITIAL_SIZE];
	ERL_NIF_TERM *items      = items_on_stack; 
	
	if(argc != 2 || !enif_get_uint(env, argv[0], &delimiter_) ||
	   !enif_inspect_binary(env, argv[1], &bin_raw))
	{
		return enif_make_badarg(env);
	}

	iter      = prev_iter = bin_raw.data;
	bin_end   = iter + bin_raw.size;
	delimiter = (unsigned char)delimiter_;

	do {
		unsigned int eol = 0;
		unsigned int quotelevel = 0;
		
#define IS_EOL(i) (*(i) == '\n' && (quotelevel & 1) == 0)
		
		while (iter != bin_end) {
			if(*iter == delimiter) {
				if((quotelevel & 1) == 0)
					break;
			}
			else if(*iter == '"') {
				quotelevel += 1;
			}
			else if(IS_EOL(iter)) {
				eol = 1;
				break;
			}

			iter += 1;
		}

		bytes_parsed = iter - bin_raw.data + 1;
		if(bytes_parsed > bin_raw.size)
			bytes_parsed = bin_raw.size;

		if(eol && iter != bin_raw.data) {
			if(*(iter-1) == '\r')
				iter -= 1;
		}

		/* conditionally allocate a new items array */
		if(nitems + 1 >= items_size) {
			const size_t new_size = sizeof(items[0]) * items_size * 2;
			ERL_NIF_TERM* new_items;

			if(items == items_on_stack) {
				new_items = (ERL_NIF_TERM*)malloc(new_size);
				if(new_items != NULL)
					memcpy(new_items, items, new_size/2);
			} else {
				new_items = (ERL_NIF_TERM*)realloc(items, new_size);
			}

			if(new_items == NULL)
				goto out_of_mem;

			items = new_items;
			items_size *= 2;
		}

		items[nitems] = enif_make_sub_binary(
			env, argv[1],
			prev_iter - bin_raw.data, iter - prev_iter);
		nitems += 1;
		iter += 1;
		prev_iter = iter;
		if((quotelevel & 1))
			goto open_quote;
		if(eol)
			goto ok;
	} while(iter < bin_end);	

#define FREE_ITEMS() do { if(items != items_on_stack) free(items); } while(0)
	result = make_result(env, atom_incomplete, bytes_parsed, items, nitems);
	FREE_ITEMS();
	return result;

ok:
	result = make_result(env, atom_ok, bytes_parsed, items, nitems);
	FREE_ITEMS();
	return result;

open_quote:
	result = make_result(env, atom_open_quote, bytes_parsed, items, nitems);
	FREE_ITEMS();
	return result;
	
out_of_mem:
        FREE_ITEMS();
	return atom_error;
}

static ErlNifFunc nif_funcs[] = {
	{"parse", 2, parser_nif}
};

static int
upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
  return 0;
}

static int
reload(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  return 0;
}

static int
on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
#define MK_ATOM(name) enif_make_atom(env, name)
	atom_incomplete = MK_ATOM("incomplete");
	atom_ok         = MK_ATOM("ok");
	atom_error      = MK_ATOM("error");
	atom_open_quote = MK_ATOM("open_quote");
#undef MK_ATOM

	return 0;
}

ERL_NIF_INIT(ecsv_parser, nif_funcs, on_load, reload, upgrade, NULL)
