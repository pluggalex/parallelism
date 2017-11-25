#include "deque.h"
#include <iostream>
#include <unistd.h>
#include <thread>

bool running = true;

void pushing(int val, DQueue *queue){
  while(running) queue->PushLeft(val); 
}

void pushing_and_poppin(int val, DQueue *queue){
  while(running){
    queue->PushLeft(val); 
    queue->PopLeft(); 
  }
}

void pushing_and_poppin_again(int val, DQueue *queue){
  while(running){
    queue->PushLeft(val); 
    queue->PopRight(); 
  }
}



int main(int argv, char* argc[]){
  DQueue queue;
  int seconds = 5;
  int thread_amount = atoi(argc[1]);
  std::thread threads[thread_amount];


  for(int i = 0; i < thread_amount; i++){
    threads[i] = std::thread(pushing_and_poppin, i, &queue);  
  }

  sleep(seconds);
  running = false;

  for(int i = 0; i < thread_amount; i++){
    threads[i].join();
  }

  std::cout << "Threads: " << thread_amount << " OP/Sec: " << queue.get_counter()/seconds << "\n";
  return 0;
}
