program main
  implicit none
  integer :: ios, s, x = 1, c = 0, total = 0
  character(4) :: cmd
  character(40) :: line

  open(10, file='input.txt', form='formatted', status='old')
  do
    read(10, '(A,1x,i3)', iostat=ios) cmd, s
    if (ios /= 0) exit

    c = c + 1
    call add_signal

    if (cmd == 'noop') cycle
    if (cmd == 'addx') then
      c = c + 1
      call add_signal
      x = x + s
    end if
  end do
  print *, "Part 1 ", total
contains
  subroutine add_signal
    integer :: a, b
    if (c == 20 .or. mod(c, 40) == 20) then
      total = total + (c * x)
    end if

    a = mod(c, 40)
    if (a == 0) a = 40
    b = x - a
    if (any([-2, -1, 0] == b)) then
      line(a:a) = '#'
    else
      line(a:a) = '.'
    end if
    if (a == 40) print *, line
  end subroutine
end program main
