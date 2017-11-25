#include <atomic>
#include <iostream>
#include <random>
#include <thread>
#include <unistd.h>

std::atomic<bool> terminate;

////////////////////////////////////////////////////////////////////////////////

#include "deque.h"

const int N = 4; // number of processors

DQueue job_queue[N];

int steal_work(int current) {
  __transaction_relaxed {
    for (int i = 0; i < N; i++) {
      if (i != current) {
        int job = job_queue[i].PopRight();
        if (job == 0)
          job_queue[i].PushRight(job);
        else if (job != -1) {
          std::cout << "Processor " << current << ": "
                    << "stealing job (duration:  " << job
                    << "s) from processor: " << i << ".\n";

          return job;
        }
      }
    }
  }
  return 0;
}

void processor(int n) {
  while (!terminate) {
    // TODO
    int job = 0;
    __transaction_relaxed { job = job_queue[n].PopLeft(); }
    if (job == -1) {
      job = steal_work(n);
    } else {
      std::cout << "Processor " << n << ": "
                << "executing job (duration:  " << job << "s)\n";
    }
    sleep(job);
  }
}

////////////////////////////////////////////////////////////////////////////////

void user()
{
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<int> processor(0, N-1);
  std::uniform_int_distribution<int> duration(0, 2*N);

  unsigned int time = 0;

  // generates a new job about once per second
  while (!terminate)
    {
      int p = processor(gen);
      int d = duration(gen);

      __transaction_relaxed
        {
          std::cout << time << ": scheduling a job on processor " << p << " (duration: " << d << " s)." << std::endl;
          job_queue[p].PushRight(d);
        }

      sleep(1);
      ++time;
    }
}

////////////////////////////////////////////////////////////////////////////////

int main()
{
  std::thread user_thread(user);

  std::thread processors[N];
  for (int i=0; i<N; ++i)
    {
      processors[i] = std::thread(processor, i);
    }

  sleep(60);
  terminate = true;

  user_thread.join();
  for (int i=0; i<N; ++i)
    {
      processors[i].join();
    }

  return 0;
}
