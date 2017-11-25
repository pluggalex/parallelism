#include <iostream>
#include <random>
#include <thread>
#include <unistd.h>

const int N = 8; // size of vector -- change this for different measurements

// sequential algorithm
void finite_differences(double vector[N])
{
  double diff[N];

  for (int i=1; i<N-1; ++i)
    {
      diff[i] = (vector[i-1] + 2.0 * vector[i] + vector[i+1]) / 4.0;
    }

  for (int i=1; i<N-1; ++i)
    {
      vector[i] = diff[i];
    }
}

////////////////////////////////////////////////////////////////////////////////

bool terminate = false; // we are using transactions to prevent data
                        // races (declaring this as std::atomic<bool>
                        // might have been easier)

unsigned int updates = 0;

void iterate(double vector[N])
{
  bool t = false;
  while (!t)
    {
      finite_differences(vector);
      ++updates;
      __transaction_atomic
        {
          t = terminate;
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

int main()
{
  // initialize vector with random values
  double vector[N];

  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_real_distribution<double> dis(0,1);
  for (int i=0; i<N; ++i)
    {
      vector[i] = dis(gen);
    }

  // compute finite differences (in a worker thread)
  std::thread worker(iterate, vector);

  sleep(1);
  __transaction_atomic
    {
      terminate = true;
    }

  worker.join();

  // output result
  std::cout << "Updates/s: " << updates << std::endl;
  for (int i=0; i<N; ++i)
    {
      std::cout << vector[i] << " ";
    }
  std::cout << std::endl;

  return 0;
}
