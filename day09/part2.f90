program main
  implicit none
  character :: direction
  integer :: ios, steps, x, y, i
  complex :: snake(10) = (0, 0), diff = (0, 0)
  complex, allocatable, dimension(:) :: visited

  visited = [cmplx(0, 0)]
  open(10, file='input.txt', form='formatted', status='old')
  do
    read(10, '(A,1x,i2)', iostat=ios) direction, steps
    if (ios /= 0) exit
    x = 0; y = 0
    if (direction == 'R') x = 1
    if (direction == 'L') x = -1
    if (direction == 'U') y = 1
    if (direction == 'D') y = -1

    do steps=steps,1,-1
      snake(1) = snake(1) + cmplx(x, y)
      do i=2,10
        diff = snake(i-1) - snake(i)
        if (real(diff) > 1) then
          snake(i) = snake(i) + cmplx(1, 0)
          if (imag(diff) > 0) snake(i) = snake(i) + cmplx(0, 1)
          if (imag(diff) < 0) snake(i) = snake(i) - cmplx(0, 1)
        else if (real(diff) < -1) then
          snake(i) = snake(i) - cmplx(1, 0)
          if (imag(diff) > 0) snake(i) = snake(i) + cmplx(0, 1)
          if (imag(diff) < 0) snake(i) = snake(i) - cmplx(0, 1)
        else if (imag(diff) > 1) then
          snake(i) = snake(i) + cmplx(0, 1)
          if (real(diff) > 0) snake(i) = snake(i) + cmplx(1, 0)
          if (real(diff) < 0) snake(i) = snake(i) - cmplx(1, 0)
        else if (imag(diff) < -1) then
          snake(i) = snake(i) - cmplx(0, 1)
          if (real(diff) > 0) snake(i) = snake(i) + cmplx(1, 0)
          if (real(diff) < 0) snake(i) = snake(i) - cmplx(1, 0)
        end if
        if (.not. any(visited == snake(10))) visited = [visited, snake(10)]
      end do
    end do
  end do
  print *, "Part 2 ", size(visited)
  close(10)
end program main
