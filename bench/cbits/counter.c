#include <stdint.h>

#ifdef __linux__

#include <linux/perf_event.h>
#include <string.h>
#include <sys/syscall.h>
#include <unistd.h>

/* Open a counter of the instructions the calling thread retires in user
   space, giving -1 where the system does not let a process count them. */
int tilia_bench_counter_open(void)
{
  struct perf_event_attr attr;
  memset(&attr, 0, sizeof attr);
  attr.type = PERF_TYPE_HARDWARE;
  attr.size = sizeof attr;
  attr.config = PERF_COUNT_HW_INSTRUCTIONS;
  attr.exclude_kernel = 1;
  attr.exclude_hv = 1;
  return syscall(SYS_perf_event_open, &attr, 0, -1, -1, 0);
}

/* Read a counter. */
uint64_t tilia_bench_counter_read(int fd)
{
  uint64_t count = 0;
  if (read(fd, &count, sizeof count) != sizeof count)
    return 0;
  return count;
}

#else

int tilia_bench_counter_open(void)
{
  return -1;
}

uint64_t tilia_bench_counter_read(int fd)
{
  (void)fd;
  return 0;
}

#endif
