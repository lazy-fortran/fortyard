program test_proc_run
    use fy_proc, only: proc_run
    implicit none

    character(len=:), allocatable :: out
    integer :: status
    character(len=64) :: argv3(3)

    argv3 = [character(len=64) :: 'printf', '%s|', 'a b;$(rm -rf x)''c']
    call proc_run(argv3, out, status, 1000)
    call check(status == 0 .and. out == 'a b;$(rm -rf x)''c|', &
        'arguments reach the program literally, without a shell')

    call proc_run([character(len=8) :: 'false'], out, status, 1000)
    call check(status == 1, 'exit code reported')

    call proc_run([character(len=32) :: 'fy-no-such-program'], out, status, 1000)
    call check(status == 127, 'a missing program exits with 127')

    call proc_run([character(len=8) :: 'seq', '1', '100000'], out, status, 10)
    call check(status == 0 .and. len(out) == 10 .and. out(1:4) == '1'//new_line('a')//'2'// &
        new_line('a'), 'output cut at the limit while the child still finishes')

    call proc_run([character(len=8) :: 'pwd'], out, status, 1000, dir='/usr')
    call check(status == 0 .and. out == '/usr'//new_line('a'), 'runs in the given directory')

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_proc_run
