/* Linux-only system calls behind one portable name each. Elsewhere (the
 * macOS runner) they fail with ENOSYS, and the callers fall back: the remote
 * runner polls instead of watching events, and only the server, which runs
 * on Linux, asks for a death signal. */
#include <errno.h>

#ifdef __linux__
#include <sys/inotify.h>
#include <sys/prctl.h>

int fy_inotify_init1(int flags) { return inotify_init1(flags); }

int fy_inotify_add_watch(int fd, const char *path, int mask)
{
    return inotify_add_watch(fd, path, (unsigned int)mask);
}

int fy_set_pdeathsig(int sig) { return prctl(PR_SET_PDEATHSIG, (unsigned long)sig, 0UL, 0UL, 0UL); }

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

#endif
