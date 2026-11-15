module fy_mail
    !! Mail to the people responsible for the instance when main changes state:
    !! a verification fails, or passes again. One mail per change of state,
    !! never one per run. The mail goes through an SMTP server (FY_SMTP_URL,
    !! e.g. smtp://faepmail.tugraz.at:25) with STARTTLS unless
    !! FY_SMTP_STARTTLS=0, from FY_MAIL_FROM to the comma-separated FY_MAIL_TO;
    !! FY_MAIL_PROXY names an HTTP proxy the connection is tunnelled through.
    !! Without FY_SMTP_URL, FY_MAIL_FROM or FY_MAIL_TO nothing is sent.
    use, intrinsic :: iso_fortran_env, only: &
        error_unit
    use fy_proc, only: &
        fd_close, &
        fd_read, &
        fd_write_all, &
        proc_exit_status, &
        proc_find, &
        proc_spawn
    implicit none
    private

    public :: &
        mail_issue

    character(len=*), parameter :: NL = achar(10), CRLF = achar(13)//achar(10)

contains

    subroutine mail_issue(repo, number, what, text)
        !! Tells of issue `number` of `repo`: `what` is the subject after the
        !! repository's name, `text` the body; the issue's address follows.
        character(len=*), intent(in) :: repo, what, text
        integer, intent(in) :: number
        character(len=:), allocatable :: base, body
        character(len=16) :: n

        write (n, '(i0)') number
        base = env_value('FY_PUBLIC_URL')
        if (len(base) == 0) base = env_value('FY_URL')
        body = text
        if (len(base) > 0) body = body//NL//NL//base//'/'//repo//'/-/issues/'//trim(n)//NL
        call mail_send('[fortyard] '//repo//': '//what, body)
    end subroutine mail_issue

    subroutine mail_send(subject, body)
        character(len=*), intent(in) :: subject, body
        character(len=:), allocatable :: url, from, to, conf, message, file, rest, curl
        character(len=1024) :: argv(3)
        character(len=1024) :: env(1)
        character(len=4096) :: buf
        integer :: pid, to_child, from_child, stat, k, n, unit

        url = env_value('FY_SMTP_URL')
        from = env_value('FY_MAIL_FROM')
        to = env_value('FY_MAIL_TO')
        curl = proc_find('curl')
        if (len(url) == 0 .or. len(from) == 0 .or. len(to) == 0 .or. len(curl) == 0) return
        conf = 'url = "'//quoted(url)//'"'//NL//'mail-from = "'//quoted(from)//'"'//NL// &
            'max-time = 60'//NL//'silent'//NL//'show-error'//NL//'proto = "=smtp,smtps"'//NL
        if (env_value('FY_SMTP_STARTTLS') /= '0') conf = conf//'ssl-reqd'//NL
        if (len(env_value('FY_MAIL_PROXY')) > 0) conf = conf//'proxy = "'// &
            quoted(env_value('FY_MAIL_PROXY'))//'"'//NL//'proxytunnel'//NL
        rest = to//','
        do while (len(rest) > 1)
            k = index(rest, ',')
            if (k > 1) conf = conf//'mail-rcpt = "'//quoted(trim(adjustl(rest(1:k - 1))))//'"'//NL
            rest = rest(k + 1:)
        end do
        message = 'From: '//from//CRLF//'To: '//to//CRLF//'Subject: '//one_line(subject)//CRLF// &
            'Content-Type: text/plain; charset=utf-8'//CRLF//CRLF//crlf_lines(body)
        ! The message goes in a file: curl reads its configuration on standard input.
        file = tmp_dir()//'/fy-mail-'//itoa_pid()//'.txt'
        open (newunit=unit, file=file, access='stream', form='unformatted', status='replace', &
            iostat=stat)
        if (stat /= 0) return
        write (unit, iostat=stat) message
        close (unit)
        conf = conf//'upload-file = "'//quoted(file)//'"'//NL
        argv(1) = 'curl'
        argv(2) = '-K'
        argv(3) = '-'
        env(1) = 'PATH=/usr/local/bin:/usr/bin:/bin'
        call proc_spawn(curl, argv, env, pid, to_child, from_child)
        if (pid >= 0) then
            call fd_write_all(to_child, conf, stat)
            call fd_close(to_child)
            do
                n = fd_read(from_child, buf)
                if (n <= 0) exit
            end do
            call fd_close(from_child)
            if (proc_exit_status(pid) /= 0) then
                write (error_unit, '(a)') 'fy: mail "'//one_line(subject)//'" was not sent'
                flush (error_unit)
            end if
        end if
        open (newunit=unit, file=file, status='old', iostat=stat)
        if (stat == 0) close (unit, status='delete')
    end subroutine mail_send

    function crlf_lines(text) result(out)
        !! SMTP lines end in CRLF, and a line that starts with a dot gets a
        !! second one (dot-stuffing; curl does it too, but only for its input).
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: i, start

        out = ''
        start = 1
        do i = 1, len(text) + 1
            if (i > len(text)) then
                if (i > start) out = out//text(start:)//CRLF
                exit
            end if
            if (text(i:i) == NL) then
                out = out//text(start:i - 1)//CRLF
                start = i + 1
            end if
        end do
    end function crlf_lines

    pure function one_line(s) result(r)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: r
        integer :: i

        r = s
        do i = 1, len(r)
            if (r(i:i) == NL .or. r(i:i) == achar(13)) r(i:i) = ' '
        end do
    end function one_line

    function tmp_dir() result(d)
        character(len=:), allocatable :: d

        d = env_value('TMPDIR')
        if (len(d) == 0) d = '/tmp'
    end function tmp_dir

    function itoa_pid() result(s)
        character(len=:), allocatable :: s
        character(len=32) :: buf
        integer :: count

        call system_clock(count)
        write (buf, '(i0)') count
        s = trim(buf)
    end function itoa_pid

    function env_value(name) result(value)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: value
        integer :: n, stat

        call get_environment_variable(name, length=n, status=stat)
        if (stat /= 0) n = 0
        allocate (character(len=n) :: value)
        if (n > 0) call get_environment_variable(name, value)
    end function env_value

    pure function quoted(s) result(q)
        !! Text inside a double-quoted curl configuration value.
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: q
        integer :: i

        q = ''
        do i = 1, len(s)
            if (s(i:i) == '"' .or. s(i:i) == '\') then
                q = q//'\'//s(i:i)
            else
                q = q//s(i:i)
            end if
        end do
    end function quoted

end module fy_mail
