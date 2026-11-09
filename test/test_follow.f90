program test_follow
    !! fy serve --follow LINK re-executes the release LINK points to when it
    !! changes: same process, same port, no request refused during the switch.
    !! Needs the built application (fo build), as the landing check provides.
    implicit none
    character(len=*), parameter :: d = 'build/test_follow'
    integer :: stat

    call execute_command_line('sh -c '''// &
        'set -e; d='//d//'; rm -rf $d; mkdir -p $d/r1 $d/r2; '// &
        'fy=$(ls -t build/fo/bin/fy build/fo/app/fy 2>/dev/null | head -1); '// &
        'cp $fy $d/r1/fy; cp $fy $d/r2/fy; ln -s $PWD/$d/r1 $d/current; '// &
        '$d/current/fy serve --init --data $d/data >/dev/null; '// &
        'port=$((20000 + $$ % 20000)); '// &
        '$d/current/fy serve --data $d/data --listen 127.0.0.1:$port --workers 1 --threads 2 '// &
        '--follow $PWD/$d/current 2>$d/log & pid=$!; '// &
        'i=0; until curl -fs -o /dev/null http://127.0.0.1:$port/healthz; do '// &
        'i=$((i+1)); [ $i -lt 100 ] || exit 2; sleep 0.1; done; '// &
        'case $(readlink /proc/$pid/exe) in */r1/fy) ;; *) exit 3;; esac; '// &
        '( n=0; while [ $n -lt 60 ]; do curl -fs -o /dev/null -m 5 http://127.0.0.1:$port/healthz '// &
        '|| echo refused >> $d/failures; n=$((n+1)); sleep 0.05; done ) & load=$!; '// &
        'sleep 0.5; ln -sfn $PWD/$d/r2 $d/current.new; mv -T $d/current.new $d/current; '// &
        'i=0; until case $(readlink /proc/$pid/exe) in */r2/fy) true;; *) false;; esac; do '// &
        'i=$((i+1)); [ $i -lt 100 ] || exit 4; sleep 0.1; done; '// &
        'wait $load; kill $pid; [ ! -e $d/failures ] || exit 5''', exitstat=stat)
    if (stat /= 0) then
        print '(a,i0)', 'FAIL: following a release switch, step ', stat
        error stop 1
    end if
    print '(a)', 'follow: the server became the new release without refusing requests'
end program test_follow
