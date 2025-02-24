import threading
import time
import random
import numpy as np


# --- Task 1: Evolution with Barrier Synchronization ---

class Evolution:
    def __init__(self, size, num_threads, steps):
        self.size = size
        self.num_threads = num_threads
        self.steps = steps
        self.grid = np.random.randint(2, size=self.size)
        self.new_grid = np.zeros_like(self.grid)
        self.barrier = threading.Barrier(self.num_threads)
        self.strip_size = self.size[0] // self.num_threads
        self.locks = [threading.Lock() for _ in range(self.num_threads)]

    def get_neighbors(self, row, col):
        neighbors = []
        for i in range(-1, 2):
            for j in range(-1, 2):
                if i == 0 and j == 0:
                    continue
                neighbors.append(self.grid[(row + i) % self.size[0], (col + j) % self.size[1]])
        return neighbors

    def update_cell(self, row, col):
        live_neighbors = sum(self.get_neighbors(row, col))
        if self.grid[row, col] == 1:
            if live_neighbors < 2 or live_neighbors > 3:
                self.new_grid[row, col] = 0
            else:
                self.new_grid[row, col] = 1
        else:
            if live_neighbors == 3:
                self.new_grid[row, col] = 1
            else:
                self.new_grid[row, col] = 0

    def worker(self, thread_id):
        start_row = thread_id * self.strip_size
        end_row = (thread_id + 1) * self.strip_size

        for step in range(self.steps):
            for row in range(start_row, end_row):
                for col in range(self.size[1]):
                    self.update_cell(row, col)

            self.barrier.wait()  # Synchronization point

            if thread_id == 0:
                self.grid[:] = self.new_grid[:]

            self.barrier.wait()  # Ensure all threads see updated grid

    def run_threaded(self):
        threads = []
        for i in range(self.num_threads):
            thread = threading.Thread(target=self.worker, args=(i,))
            threads.append(thread)
            thread.start()

        for thread in threads:
            thread.join()

    def run_sequential(self):
        for step in range(self.steps):
            for row in range(self.size[0]):
                for col in range(self.size[1]):
                    self.update_cell(row, col)
            self.grid[:] = self.new_grid[:]


size = (200, 200)
num_threads = 4
steps = 50

evolution_threaded = Evolution(size, num_threads, steps)
start_time = time.time()
evolution_threaded.run_threaded()
end_time = time.time()
threaded_time = end_time - start_time

evolution_sequential = Evolution(size, 1, steps)  # Single thread for sequential
start_time = time.time()
evolution_sequential.run_sequential()
end_time = time.time()
sequential_time = end_time - start_time

print(f"Treads: {num_threads}")
print(f"Threaded time: {threaded_time:.4f} seconds")
print(f"Sequential time: {sequential_time:.4f} seconds")


# --- Task 2: Linked List with Read/Write Locks ---


class Node:
    def __init__(self, data):
        self.data = data
        self.next = None


class LinkedList:
    def __init__(self):
        self.head = None
        self.read_lock = threading.Lock()
        self.write_lock = threading.Lock()

    def contains(self, value):
        with self.read_lock:
            current = self.head
            while current:
                if current.data == value:
                    return True
                current = current.next
            return False

    def append(self, value):
        with self.write_lock:
            if self.contains(value):
                return
            new_node = Node(value)
            if not self.head:
                self.head = new_node
            else:
                current = self.head
                while current.next:
                    current = current.next
                current.next = new_node


def worker_linked_list(linked_list, num_values):
    for _ in range(num_values):
        value = random.randint(0, 1000)
        linked_list.append(value)


num_threads_ll = 4
num_values_per_thread = 1000

linked_list = LinkedList()
threads_ll = []
for _ in range(num_threads_ll):
    thread = threading.Thread(target=worker_linked_list, args=(linked_list, num_values_per_thread))
    threads_ll.append(thread)
    thread.start()

for thread in threads_ll:
    thread.join()

values = []
current = linked_list.head
while current:
    values.append(current.data)
    current = current.next

print(f"Linked list contains duplicates: {len(values) != len(set(values))}")
