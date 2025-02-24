import threading
import time
import random


class Philosopher(threading.Thread):
    def __init__(self, name, left_fork, right_fork, stop_event):
        threading.Thread.__init__(self)
        self.name = name
        self.left_fork = left_fork
        self.right_fork = right_fork
        self.stop_event = stop_event

    def run(self):
        while not self.stop_event.is_set():
            print(f"{self.name} is thinking.")
            time.sleep(random.uniform(1, 3))

            print(f"{self.name} is hungry and wants to eat.")

            self.left_fork.acquire()
            print(f"{self.name} has picked up the left fork.")
            time.sleep(random.uniform(0.1, 0.5))

            if self.right_fork.acquire(timeout=1):
                print(f"{self.name} has picked up the right fork and starts eating.")
                time.sleep(random.uniform(2, 5))
                print(f"{self.name} has finished eating and puts down the forks.")
                self.right_fork.release()
            else:
                print(f"{self.name} couldn't pick up the right fork and puts down the left fork.")

            self.left_fork.release()


if __name__ == "__main__":
    forks = [threading.Lock() for _ in range(5)]
    stop_event = threading.Event()

    philosophers = [
        Philosopher(f"Philosopher {i}", forks[i], forks[(i + 1) % 5], stop_event)
        for i in range(5)
    ]

    for philosopher in philosophers:
        philosopher.start()

    time.sleep(20)
    stop_event.set()

    for philosopher in philosophers:
        philosopher.join()

    print("Simulation finished.")
