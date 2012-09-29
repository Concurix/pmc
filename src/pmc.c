// Copyright (c) 2012 Concurix, Inc.
//
// A performance monitoring counter library for Erlang programs.

#include <sys/types.h>
#include <errno.h>
#include <pmc.h>

#include <stdio.h>

#include "erl_nif.h"


// Utility function to convert a Unix errno value into an atom.

static ERL_NIF_TERM errno_to_atom(ErlNifEnv *env, int error) {
    switch (error) {
    case EBUSY:
        return enif_make_atom(env, "pmc_error_busy");
    case EDOOFUS:
        return enif_make_atom(env, "pmc_error_missing_log_file");
    case EEXIST:
        return enif_make_atom(env, "pmc_error_exist");
    case EINVAL:
        return enif_make_atom(env, "pmc_error_invalid");
    case ENXIO:
        return enif_make_atom(env, "pmc_error_missing_resource");
    case EOPNOTSUPP:
        return enif_make_atom(env, "pmc_error_not_supported");
    case EPERM:
        return enif_make_atom(env, "pmc_error_not_permitted");
    case ESRCH:
        return enif_make_atom(env, "pmc_error_missing_process");
    default:
        return enif_make_atom(env, "pmc_error_unknown");
    }
}


// Handlers for loading, upgrading and unloading this module in an
// Erlang environment.


static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    return pmc_init();
}


static int on_upgrade(ErlNifEnv *env, void **priv_data,
                      void **old_priv_data, ERL_NIF_TERM load_info) {
    return 0;
}


static void on_unload(ErlNifEnv *env, void *priv_data) {}


// Erlang native implemented functions for allocating, releasing, attaching,
// starting, stopping, reading and writing performance monitoring counters.


// Allocate a PMC for an event specified as an Erlang atom.  Returns
// an integer PMC ID number on success and an atom on failure.
static ERL_NIF_TERM allocate_nif(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
    unsigned len;
    char *event_spec;
    pmc_id_t pmcid;
    int ret;

    if (enif_is_atom(env, argv[0])) {
        if (!enif_get_atom_length(env, argv[0], &len, ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        }

        event_spec = (char *)enif_alloc(len + 1);

        if (!enif_get_atom(env, argv[0], event_spec, len+1, ERL_NIF_LATIN1)) {
            return enif_make_badarg(env);
        }
    } else if (enif_is_list(env, argv[0])) {
        if (!enif_get_list_length(env, argv[0], &len)) {
            return enif_make_badarg(env);
        }

        event_spec = (char *)enif_alloc(len + 1);

        if (enif_get_string(env, argv[0], event_spec,
                            len + 1, ERL_NIF_LATIN1) != len + 1) {
            return enif_make_badarg(env);
        }
    } else {
        return enif_make_badarg(env);
    }
      

    ret = pmc_allocate(event_spec, PMC_MODE_TC, PMC_F_DESCENDANTS,
                       PMC_CPU_ANY, &pmcid);

    if (ret != 0) {
        return errno_to_atom(env, errno);
    }

    return enif_make_int(env, pmcid);
}


// Release an allocated PMC ID.  Takes a PMC ID number and returns
// an ok atom on success and an informative atom on failure.
static ERL_NIF_TERM release_nif(ErlNifEnv *env, int argc,
                                const ERL_NIF_TERM argv[]) {
    pmc_id_t pmcid;
    int ret;

    if (!enif_get_int(env, argv[0], &pmcid)) {
        return enif_make_badarg(env);
    }

    ret = pmc_release(pmcid);

    if (ret != 0) {
        return errno_to_atom(env, errno);
    }

    return enif_make_atom(env, "ok");
}


// Attach an allocated PMC ID to the current process.  Takes
// a PMC ID number and returns an ok atom on success and an
// informative atom on failure.
static ERL_NIF_TERM attach_nif(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
    pmc_id_t pmcid;
    int ret;

    if (!enif_get_int(env, argv[0], &pmcid)) {
        return enif_make_badarg(env);
    }

    /* Attach to the current process */
    ret = pmc_attach(pmcid, 0);

    if (ret != 0) {
        return errno_to_atom(env, errno);
    }

    return enif_make_atom(env, "ok");
}


// Do not use pmc_detach! It can cause severe problems
// such as preventing the OS from forking another process.


// Start counting events for a PMC ID.  Takes a PMC ID number
// and returns an ok atom on success and an informative atom
// on failure.
static ERL_NIF_TERM start_nif(ErlNifEnv *env, int argc,
                              const ERL_NIF_TERM argv[]) {
    pmc_id_t pmcid;
    int ret;

    if (!enif_get_int(env, argv[0], &pmcid)) {
        return enif_make_badarg(env);
    }

    ret = pmc_start(pmcid);

    if (ret != 0) {
        return errno_to_atom(env, errno);
    }

    return enif_make_atom(env, "ok");
}


// Stop counting events for a PMC ID.  Takes a PMC ID number
// and returns an ok atom on success and an informative atom
// on failure.
static ERL_NIF_TERM stop_nif(ErlNifEnv *env, int argc,
                             const ERL_NIF_TERM argv[]) {
    pmc_id_t pmcid;
    int ret;

    if (!enif_get_int(env, argv[0], &pmcid)) {
        return enif_make_badarg(env);
    }

    ret = pmc_stop(pmcid);

    if (ret != 0) {
        return errno_to_atom(env, errno);
    }

    return enif_make_atom(env, "ok");
}


// Read the count of events for a PMC ID.  Takes a PMC ID number
// and returns an integer on success and an informative atom
// on failure.
static ERL_NIF_TERM read_nif(ErlNifEnv *env, int argc,
                             const ERL_NIF_TERM argv[]) {
    pmc_id_t pmcid;
    pmc_value_t value;
    int ret;

    if (!enif_get_int(env, argv[0], &pmcid)) {
        return enif_make_badarg(env);
    }

    ret = pmc_read(pmcid, &value);

    if (ret != 0) {
        return errno_to_atom(env, errno);
    }

    return enif_make_uint64(env, value);
}


// Write the counter for a PMC ID.  Takes a PMC ID number and
// an integer and returns an ok atom on success and an informative
// atom on failure.
static ERL_NIF_TERM write_nif(ErlNifEnv *env, int argc,
                              const ERL_NIF_TERM argv[]) {
    pmc_id_t pmcid;
    ErlNifUInt64 value;
    int ret;

    if (!enif_get_int(env, argv[0], &pmcid)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint64(env, argv[1], &value)) {
        return enif_make_badarg(env);
    }

    ret = pmc_write(pmcid, value);

    if (ret != 0) {
        return errno_to_atom(env, errno);
    }

    return enif_make_atom(env, "ok");
}


static ErlNifFunc nif_funcs[] = {
    {"allocate", 1, allocate_nif},
    {"release", 1, release_nif},
    {"attach", 1, attach_nif},
    {"start", 1, start_nif},
    {"stop", 1, stop_nif},
    {"read", 1, read_nif},
    {"write", 2, write_nif}
};


ERL_NIF_INIT(pmc, nif_funcs, on_load, NULL, on_upgrade, on_unload)

