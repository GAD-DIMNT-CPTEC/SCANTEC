import time


def time_statistics(func):
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()

        if not hasattr(wrapper, 'times'):
            wrapper.times = []

        wrapper.times.append(end - start)
        return result

    return wrapper

