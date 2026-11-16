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

#include "qca_threads.h"

#include <stdlib.h>

#if defined(HAVE_PTHREAD)
    #include <pthread.h>
    #if defined(__EMSCRIPTEN_PTHREADS__)
        #include <emscripten/threading.h>
    #elif defined(_WIN32)
        #include <windows.h>
    #else
        #include <unistd.h>
    #endif
#endif

#if defined(__EMSCRIPTEN_PTHREADS__)
    #define QCA_THREAD_LIMIT 4
#else
    #define QCA_THREAD_LIMIT 64
#endif

typedef struct {
    qca_range_worker worker;
    void *data;
    unsigned long long start;
    unsigned long long end;
    int worker_id;
} QCARangeTask;

#if defined(HAVE_PTHREAD)
static void *qca_range_thread_main(void *arg) {
    QCARangeTask *task = (QCARangeTask *) arg;
    task->worker(task->start, task->end, task->worker_id, task->data);
    return NULL;
}
#endif

int qca_default_thread_count(void) {
    const char *requested = getenv("QCA_NUM_THREADS");
    if (requested != NULL && requested[0] != '\0') {
        char *end = NULL;
        long parsed = strtol(requested, &end, 10);
        if (end != requested && *end == '\0' && parsed > 0) {
            return parsed > QCA_THREAD_LIMIT ? QCA_THREAD_LIMIT : (int)parsed;
        }
    }
#if defined(HAVE_PTHREAD)
    #if defined(__EMSCRIPTEN_PTHREADS__)
    if (!emscripten_has_threading_support()) {
        return 1;
    }
    int nprocs = emscripten_num_logical_cores();
    if (nprocs > 1) {
        return nprocs > QCA_THREAD_LIMIT ? QCA_THREAD_LIMIT : nprocs;
    }
    #elif defined(_WIN32)
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    if (info.dwNumberOfProcessors > 1) {
        return info.dwNumberOfProcessors > QCA_THREAD_LIMIT ?
            QCA_THREAD_LIMIT : (int) info.dwNumberOfProcessors;
    }
    #else
    long nprocs = sysconf(_SC_NPROCESSORS_ONLN);
    if (nprocs > 1) {
        return nprocs > QCA_THREAD_LIMIT ? QCA_THREAD_LIMIT : (int) nprocs;
    }
    #endif
#endif
    return 1;
}

int qca_parallel_for(
    unsigned long long count,
    int requested_threads,
    qca_range_worker worker,
    void *data
) {
    int nthreads = requested_threads;

    if (count == 0) {
        return 1;
    }

    if (nthreads <= 0) {
        nthreads = qca_default_thread_count();
    }
    if (nthreads < 1) {
        nthreads = 1;
    }
    if (nthreads > QCA_THREAD_LIMIT) {
        nthreads = QCA_THREAD_LIMIT;
    }
    if ((unsigned long long) nthreads > count) {
        nthreads = (int) count;
    }

#if defined(HAVE_PTHREAD)
    if (nthreads > 1) {
        int helper_count = nthreads - 1;
        pthread_t *threads = (pthread_t *) calloc((size_t) helper_count, sizeof(pthread_t));
        QCARangeTask *tasks = (QCARangeTask *) calloc((size_t) nthreads, sizeof(QCARangeTask));
        unsigned long long base = count / (unsigned long long) nthreads;
        unsigned long long rem = count % (unsigned long long) nthreads;
        unsigned long long start = 0;
        int started = 0;

        if (threads == NULL || tasks == NULL) {
            free(threads);
            free(tasks);
            worker(0, count, 0, data);
            return 1;
        }

        for (int i = 0; i < nthreads; i++) {
            unsigned long long width = base + ((unsigned long long) i < rem ? 1 : 0);
            tasks[i].worker = worker;
            tasks[i].data = data;
            tasks[i].start = start;
            tasks[i].end = start + width;
            tasks[i].worker_id = i;
            start += width;
        }

        for (int i = 1; i < nthreads; i++) {
            if (pthread_create(&threads[started], NULL, qca_range_thread_main, &tasks[i]) != 0) {
                break;
            }
            started++;
        }

        tasks[0].worker(tasks[0].start, tasks[0].end, 0, data);

        for (int i = started + 1; i < nthreads; i++) {
            tasks[i].worker(tasks[i].start, tasks[i].end, i, data);
        }

        for (int i = 0; i < started; i++) {
            pthread_join(threads[i], NULL);
        }

        free(threads);
        free(tasks);
        return 1;
    }
#endif

    worker(0, count, 0, data);
    return 1;
}

int qca_mutex_init(qca_mutex *mutex) {
#if defined(HAVE_PTHREAD)
    return pthread_mutex_init(&mutex->mutex, NULL) == 0;
#else
    (void) mutex;
    return 1;
#endif
}

void qca_mutex_lock(qca_mutex *mutex) {
#if defined(HAVE_PTHREAD)
    pthread_mutex_lock(&mutex->mutex);
#else
    (void) mutex;
#endif
}

void qca_mutex_unlock(qca_mutex *mutex) {
#if defined(HAVE_PTHREAD)
    pthread_mutex_unlock(&mutex->mutex);
#else
    (void) mutex;
#endif
}

void qca_mutex_destroy(qca_mutex *mutex) {
#if defined(HAVE_PTHREAD)
    pthread_mutex_destroy(&mutex->mutex);
#else
    (void) mutex;
#endif
}
