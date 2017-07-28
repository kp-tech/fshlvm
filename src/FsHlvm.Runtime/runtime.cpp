// ---------------------------------------------------------------------------
// Copyright (c) 2014-2017, Zoltan Podlovics, KP-Tech Kft. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0. See LICENSE.md in the 
// project root for license information.
// ---------------------------------------------------------------------------
// This file incorporates work covered by the following copyright and
// permission notice:
// ---------------------------------------------------------------------------
// Portions of Copyright (c) 2009, Jon Harrop, Flying Frog Consultancy Ltd.
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimer in the
//   documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
// NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// ---------------------------------------------------------------------------

#include <cstdio>
#include <cstdlib>
#include <sys/time.h>
#include <pthread.h>
#include <unistd.h>
#include <map>

using namespace std;

pthread_mutex_t mutex;
map<int, int> sizes;

void add_size(void *key, int val) {
  pthread_mutex_lock(&mutex);
  sizes[long(key)] = val;
  pthread_mutex_unlock(&mutex);
}

int get_size(void *key) {
  pthread_mutex_lock(&mutex);
  int size = sizes[long(key)];
  pthread_mutex_unlock(&mutex);
  return size;
}

extern "C" {
  const bool debug = false;

  void *fshlvm_alloc(int n, int m) {
    if (debug)
      printf("fshlvm_alloc(%d, %d)\n", n, m);
    if (n*m == 0) {
      if (debug)
	printf("fshlvm_alloc(%d, %d) -> %p\n", n, m, (void *)NULL);
      return 0;
    }
    //void *data = calloc(n, m);
    void *data = malloc(n*m);
    if (data == 0) {
      printf("Out of memory\n");
      exit(1);
    }
    if (debug)
      printf("fshlvm_alloc(%d, %d) -> %p\n", n, m, data);
    if (debug)
      add_size(data, n*m);
    return data;
  }

  void fshlvm_free(void *data) {
    if (data != 0) {
      int size = 0;
      if (debug)
        size = get_size(data);
      if (debug)
        sizes.erase(long(data));
      free(data);
    }
  }

  pthread_attr_t attr;
  pthread_key_t key;
  __thread void *local;

  void fshlvm_init() {
    if (debug) {
      printf("fshlvm_init()\n");
      printf("Initializing pthread attribute...\n");
    }
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
    if (debug)
      printf("Creating pthread key...\n");
    key = pthread_key_create(&key, NULL);
    pthread_mutex_init(&mutex, NULL);
    if (debug)
      printf("fshlvm_init() ends\n");
  }

  void *fshlvm_create_thread(void *(*f)(void *), void *x) {
    if (debug)
      printf("fshlvm_create_thread(%p, %p)\n", f, x);
    pthread_t *thread = (pthread_t *)fshlvm_alloc(1, sizeof(pthread_t));
    pthread_create(thread, &attr, f, x);
    if (debug)
      printf("fshlvm_create_thread(%p, %p) -> %p\n", f, x, thread);
    return (void *)thread;
  }

  void fshlvm_join_thread(pthread_t *thread) {
    if (debug)
      printf("fshlvm_join_thread(%p)\n", thread);
    pthread_join(*thread, NULL);
    fshlvm_free(thread);
    if (debug)
      printf("fshlvm_join_thread(%p) done\n", thread);
  }

  void *fshlvm_create_mutex() {
    if (debug)
      printf("fshlvm_create_mutex()\n");
    pthread_mutex_t *mutex =
      (pthread_mutex_t *)fshlvm_alloc(1, sizeof(pthread_mutex_t));
    pthread_mutex_init(mutex, NULL);
    if (debug)
      printf("fshlvm_create_mutex() -> %p\n", mutex);
    return (void *)mutex;
  }

  void fshlvm_lock_mutex(pthread_mutex_t *mutex) {
    if(debug)
      printf("fshlvm_lock_mutex()\n");
    pthread_mutex_lock(mutex);
  }

  void fshlvm_unlock_mutex(pthread_mutex_t *mutex) {
    if(debug)
      printf("fshlvm_unlock_mutex()\n");
    pthread_mutex_unlock(mutex);
  }

  void *fshlvm_get_thread_local() {
    void *tl = pthread_getspecific(key);
    if(debug)
      printf("fshlvm_get_thread_local() -> %p\n", tl);
    return tl;
  }

  void fshlvm_set_thread_local(void *ptr) {
    if (debug)
      printf("fshlvm_set_thread_local(%p)\n", ptr);
    pthread_setspecific(key, ptr);
    if (debug)
      printf("fshlvm_set_thread_local(%p) done\n", ptr);
  }

  // If *ptr is oldval then replace with newval, returning the old *ptr
  int fshlvm_cas(int *ptr, int oldval, int newval) {
    return __sync_val_compare_and_swap(ptr, oldval, newval);
  }

  double fshlvm_time() {
    struct timeval t;
    gettimeofday(&t, NULL);
    return (double)t.tv_sec + 1e-6 * (double)t.tv_usec;
  }
}
