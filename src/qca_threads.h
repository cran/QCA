/*
Copyright (c) 2016 - 2026, Adrian Dusa
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, in whole or in part, are permitted provided that the
following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * The names of its contributors may NOT be used to endorse or promote
      products derived from this software without specific prior written
      permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef QCA_THREADS_H
#define QCA_THREADS_H

#if defined(HAVE_PTHREAD)
    #include <pthread.h>
#endif

typedef void (*qca_range_worker)(
    unsigned long long start,
    unsigned long long end,
    int worker_id,
    void *data
);

int qca_default_thread_count(void);

int qca_parallel_for(
    unsigned long long count,
    int requested_threads,
    qca_range_worker worker,
    void *data
);

typedef struct {
#if defined(HAVE_PTHREAD)
    pthread_mutex_t mutex;
#else
    int unused;
#endif
} qca_mutex;

int qca_mutex_init(qca_mutex *mutex);
void qca_mutex_lock(qca_mutex *mutex);
void qca_mutex_unlock(qca_mutex *mutex);
void qca_mutex_destroy(qca_mutex *mutex);

#endif
