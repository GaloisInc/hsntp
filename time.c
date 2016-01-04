#include <sys/time.h>
#include <time.h>
#include <string.h>
#include <errno.h>


int sntp_set_time_large(unsigned sec, unsigned msec) {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  tv.tv_sec += sec;
  tv.tv_usec+= msec;
  return settimeofday(&tv, NULL);
}

int sntp_set_time_small(unsigned sec, unsigned msec) {
  struct timeval tv;
  tv.tv_sec = sec;
  tv.tv_usec= msec;
  return adjtime(&tv, NULL);
}

char* sntp_strerr(void) {
  return strerror(errno);
}
