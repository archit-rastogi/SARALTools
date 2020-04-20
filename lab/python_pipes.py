"""Using python pipes.

TODO:
1. Two processes connected via one way pipe.
2. Graceful termination by firing appropriate event.
3. Process management:
   - when one of two process dies.
4. Abstract producer-consumer pattern. Required traits
   - composable
   - graceful termination
   - instrumentation
     - logging lifecycle
     - profiling
5. Process algebra

"""

from multiprocessing import Pool


def workload():
    pass


if __name__ == '__main__':
    '''Steps:

    1. create a pipe.
    2. Create callable objects with pipes.
    3. Create two process with callable object.
    4. Start the processes.
    '''
    pass
