program test_repo
    use fy_repo, only: &
        repo_blob, &
        repo_clone, &
        repo_commit_json, &
        repo_fetch, &
        repo_log_json, &
        repo_name_ok, &
        repo_path_ok, &
        repo_ref_ok, &
        repo_refs_json, &
        repo_tree_json
    implicit none

    character(len=*), parameter :: up = 'build/test_repo_up', mirror = 'build/test_repo.git'
    character(len=*), parameter :: g = 'git -c user.name=T -c user.email=t@example.org -C '// &
        up//' '
    character(len=:), allocatable :: json, content
    integer :: status

    call sh('rm -rf '//up//' '//mirror//' && mkdir -p '//up//'/src && git init -q -b main '//up// &
        ' && printf "hello\n" > '//up//'/README && printf "x = 1\n" > '//up//'/src/a.f90 && '// &
        g//'add -A && '//g//'commit -q -m "First commit" && printf "x = 2\n" > '//up// &
        '/src/a.f90 && '//g//'commit -q -am "Change \"x\""')

    call repo_clone(up, mirror, status)
    call check(status == 0, 'mirror cloned')

    call repo_refs_json(mirror, json, status)
    call check(status == 0 .and. index(json, '"default":"main"') > 0 .and. &
        index(json, '"name":"main"') > 0, 'branches and default branch')

    call repo_log_json(mirror, 'main', 50, json, status)
    call check(status == 0 .and. index(json, 'Change \"x\"') > 0 .and. &
        index(json, 'Change') < index(json, 'First commit'), 'history newest first, escaped')
    call check(count_of(json, '"id":"') == 2 .and. index(json, new_line('a')) == 0, &
        'every commit is a well-formed record')

    call repo_tree_json(mirror, 'main', '', json, status)
    call check(status == 0 .and. index(json, '"name":"src","type":"tree"') > 0 .and. &
        index(json, '"name":"src"') < index(json, '"name":"README"') .and. &
        index(json, '"name":"README","type":"blob","size":6') > 0, 'top directory, directories first')

    call repo_tree_json(mirror, 'main', 'src', json, status)
    call check(status == 0 .and. index(json, '"name":"a.f90"') > 0, 'subdirectory')

    call repo_blob(mirror, 'main', 'src/a.f90', content, status)
    call check(status == 0 .and. content == 'x = 2'//new_line('a'), 'file contents')

    call repo_commit_json(mirror, 'main', json, status)
    call check(status == 0 .and. index(json, '-x = 1') > 0 .and. index(json, '+x = 2') > 0 &
        .and. index(json, '"truncated":false') > 0, 'commit with its patch')

    call sh('printf "y\n" > '//up//'/new.txt && '//g//'add new.txt && '//g//'commit -q -m Third')
    call repo_fetch(mirror, status)
    call repo_log_json(mirror, 'main', 1, json, status)
    call check(status == 0 .and. index(json, '"subject":"Third"') > 0, 'fetch brings new commits')

    call check(repo_name_ok('fortyard') .and. .not. repo_name_ok('../x') .and. &
        .not. repo_name_ok('-x') .and. .not. repo_name_ok('A'), 'repository names')
    call check(repo_ref_ok('main') .and. repo_ref_ok('feature/a-1') .and. &
        .not. repo_ref_ok('--upload-pack=x') .and. .not. repo_ref_ok('a..b') .and. &
        .not. repo_ref_ok('a b'), 'refs cannot be options or ranges')
    call check(repo_path_ok('') .and. repo_path_ok('src/a.f90') .and. &
        .not. repo_path_ok('../x') .and. .not. repo_path_ok('a/../b') .and. &
        .not. repo_path_ok('/etc') .and. .not. repo_path_ok('a/..') .and. &
        .not. repo_path_ok('a'//achar(10)//'b'), 'paths stay inside the tree')

contains

    integer function count_of(s, what)
        character(len=*), intent(in) :: s, what
        integer :: pos, k
        count_of = 0
        pos = 1
        do
            k = index(s(pos:), what)
            if (k == 0) exit
            count_of = count_of + 1
            pos = pos + k
        end do
    end function count_of

    subroutine sh(cmd)
        character(len=*), intent(in) :: cmd
        integer :: st
        call execute_command_line(cmd, exitstat=st)
        call check(st == 0, 'setup: '//cmd(1:min(60, len(cmd))))
    end subroutine sh

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_repo
