/**
 * Copyright 2011,  Filipe David Manana  <fdmanana@apache.org>
 * Web:  http://github.com/fdmanana/snappy-erlang-nif
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 **/

#include <iostream>
#include <cstring>
#include <sys/time.h>

#include "erl_nif_compat.h"

#ifdef OTP_R13B03
#error OTP R13B03 not supported. Upgrade to R13B04 or later.
#endif

#ifdef __cplusplus
#define BEGIN_C extern "C" {
#define END_C }
#else
#define BEGIN_C
#define END_C
#endif

#define SC_PTR(c) reinterpret_cast<char *>(c)


static inline ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom_compat(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}


static inline ERL_NIF_TERM
make_ok(ErlNifEnv* env, ERL_NIF_TERM mesg)
{
    ERL_NIF_TERM ok = make_atom(env, "ok");
    return enif_make_tuple2(env, ok, mesg);   
}


static inline ERL_NIF_TERM
make_error(ErlNifEnv* env, const char* mesg)
{
    ERL_NIF_TERM error = make_atom(env, "error");
    return enif_make_tuple2(env, error, make_atom(env, mesg));
}


BEGIN_C


ERL_NIF_TERM 
nif_now(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	struct timeval tv;
	int a,b,c;

	gettimeofday(&tv, NULL);
	
	a = tv.tv_sec / 1000000;
	b = tv.tv_sec % 1000000;
	c = tv.tv_usec;

    return enif_make_tuple(env, 
							3,
							enif_make_int(env, a),
							enif_make_int(env, b),
							enif_make_int(env, c)
							);
}

int
on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}


int
on_reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}


int
on_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return 0;
}


static ErlNifFunc nif_functions[] = {
    {"nif_now", 0, nif_now}
};


ERL_NIF_INIT(snappy, nif_functions, &on_load, &on_reload, &on_upgrade, NULL);


END_C
