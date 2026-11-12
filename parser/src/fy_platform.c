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

/* Opens `path` for reading only when it is a regular file itself, not a
 * symbolic link, FIFO or device: another container that can write the
 * directory cannot make the reader open one of the reader's own files. */
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

int fy_open_regular(const char *path)
{
    struct stat st;
    int fd = open(path, O_RDONLY | O_NOFOLLOW | O_NONBLOCK | O_CLOEXEC);
    if (fd < 0)
        return -1;
    if (fstat(fd, &st) != 0 || !S_ISREG(st.st_mode)) {
        close(fd);
        return -1;
    }
    return fd;
}

/* The hand-over directory another container can write, used through a
 * descriptor: whatever that container renames or links there later, the
 * reader's creates, unlinks and opens stay inside the directory it opened
 * and never follow a link to one of its own. */
#include <stdio.h>
#include <string.h>

int fy_dir_open(const char *path)
{
    int fd;
    (void)mkdir(path, 0700);
    fd = open(path, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (fd >= 0)
        return fd;
    /* A link or file in its place: removed, the directory made anew. */
    if (unlink(path) != 0 || mkdir(path, 0700) != 0)
        return -1;
    return open(path, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
}

int fy_open_regular_at(int dfd, const char *name)
{
    struct stat st;
    int fd = openat(dfd, name, O_RDONLY | O_NOFOLLOW | O_NONBLOCK | O_CLOEXEC);
    if (fd < 0)
        return -1;
    if (fstat(fd, &st) != 0 || !S_ISREG(st.st_mode)) {
        close(fd);
        return -1;
    }
    return fd;
}

int fy_exists_at(int dfd, const char *name)
{
    struct stat st;
    return fstatat(dfd, name, &st, AT_SYMLINK_NOFOLLOW) == 0;
}

int fy_unlink_at(int dfd, const char *name) { return unlinkat(dfd, name, 0); }

/* `name` whole or not at all: written to name.new, created anew, then
 * renamed into place. */
int fy_put_at(int dfd, const char *name, const char *data, long n)
{
    char aside[256];
    long done = 0;
    int fd;
    if (strlen(name) + 5 > sizeof aside)
        return -1;
    strcpy(aside, name);
    strcat(aside, ".new");
    (void)unlinkat(dfd, aside, 0);
    fd = openat(dfd, aside, O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW | O_CLOEXEC, 0600);
    if (fd < 0)
        return -1;
    while (done < n) {
        ssize_t k = write(fd, data + done, (size_t)(n - done));
        if (k < 0) {
            if (errno == EINTR)
                continue;
            close(fd);
            return -1;
        }
        done += k;
    }
    if (close(fd) != 0)
        return -1;
    return renameat(dfd, aside, dfd, name);
}

/* At most `n` bytes from the start of the open file `fd` into `buf`. */
long fy_read_upto(int fd, char *buf, long n)
{
    long done = 0;
    while (done < n) {
        ssize_t k = pread(fd, buf + done, (size_t)(n - done), (off_t)done);
        if (k < 0) {
            if (errno == EINTR)
                continue;
            return -1;
        }
        if (k == 0)
            break;
        done += k;
    }
    return done;
}
