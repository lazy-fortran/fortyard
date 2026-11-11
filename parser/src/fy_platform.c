/* Linux-only system calls behind one portable name each. Elsewhere (the
 * macOS runner) they fail with ENOSYS, and the callers fall back: the remote
 * runner polls instead of watching events, a check started without a death
 * signal is ended by its watcher, and a check's output file keeps the bytes
 * already sent. */
#define _GNU_SOURCE
#include <errno.h>

#ifdef __linux__
#include <fcntl.h>
#include <sys/inotify.h>
#include <sys/prctl.h>

int fy_inotify_init1(int flags) { return inotify_init1(flags); }

int fy_inotify_add_watch(int fd, const char *path, int mask)
{
    return inotify_add_watch(fd, path, (unsigned int)mask);
}

int fy_set_pdeathsig(int sig) { return prctl(PR_SET_PDEATHSIG, (unsigned long)sig, 0UL, 0UL, 0UL); }

int fy_punch_hole(int fd, long offset, long length)
{
    return fallocate(fd, FALLOC_FL_PUNCH_HOLE | FALLOC_FL_KEEP_SIZE, (off_t)offset, (off_t)length);
}

#else

int fy_inotify_init1(int flags)
{
    (void)flags;
    errno = ENOSYS;
    return -1;
}

int fy_inotify_add_watch(int fd, const char *path, int mask)
{
    (void)fd;
    (void)path;
    (void)mask;
    errno = ENOSYS;
    return -1;
}

int fy_set_pdeathsig(int sig)
{
    (void)sig;
    errno = ENOSYS;
    return -1;
}

int fy_punch_hole(int fd, long offset, long length)
{
    (void)fd;
    (void)offset;
    (void)length;
    errno = ENOSYS;
    return -1;
}

#endif
